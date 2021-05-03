#'@title Transform ggplot2 objects into 3D
#'
#'@description Plots a ggplot2 object in 3D by mapping the color or fill aesthetic to elevation.
#'
#'Currently, this function does not transform lines mapped to color into 3D.
#'
#'If there are multiple legends/guides due to multiple aesthetics being mapped (e.g. color and shape),
#'the package author recommends that the user pass the order of the guides manually using the ggplot2 function "guides()`. 
#'Otherwise, the order may change when processing the ggplot2 object and result in a mismatch between the 3D mapping
#'and the underlying plot.
#'
#'Using the shape aesthetic with more than three groups is not recommended, unless the user passes in 
#'custom, solid shapes. By default in ggplot2, only the first three shapes are solid, which is a requirement to be projected
#'into 3D.
#'
#'@param ggobj ggplot object to projected into 3D. 
#'@param width Default `3`. Width of ggplot, in `units`.
#'@param height Default `3`. Height of ggplot, in `units`.
#'@param height_aes Default `NULL`. Whether the `fill` or `color` aesthetic should be used for height values, 
#'which the user can specify by passing either `fill` or `color` to this argument.
#'Automatically detected. If both `fill` and `color` aesthetics are present, then `fill` is default.
#'@param invert Default `FALSE`. If `TRUE`, the height mapping is inverted.
#'@param shadow_intensity Default `0.5`. The intensity of the calculated shadows.
#'@param units Default `in`. One of c("in", "cm", "mm").
#'@param scale Default `150`. Multiplier for vertical scaling: a higher number increases the height
#'of the 3D transformation.
#'@param pointcontract Default `0.7`. This multiplies the size of the points and shrinks
#'them around their center in the 3D surface mapping. Decrease this to reduce color bleed on edges, and set to
#'`1` to turn off entirely. Note: If `size` is passed as an aesthetic to the same geom
#'that is being mapped to elevation, this scaling will not be applied. If `alpha` varies on the variable 
#'being mapped, you may want to set this to `1`, since the points now have a non-zero width stroke outline (however,
#'mapping `alpha` in the same variable you are projecting to height is probably not a good choice. as the `alpha`
#'variable is ignored when performing the 3D projection).
#'@param offset_edges Default `FALSE`. If `TRUE`, inserts a small amount of space between polygons for "geom_sf", "geom_tile", "geom_hex", and "geom_polygon" layers.
#'If you pass in a number, the space between polygons will be a line of that width. Note: this feature may end up removing thin polygons 
#'from the plot entirely--use with care.
#'@param preview Default `FALSE`. If `TRUE`, the raytraced 2D ggplot will be displayed on the current device.
#'@param raytrace Default `FALSE`. Whether to add a raytraced layer.
#'@param sunangle Default `315` (NW). If raytracing, the angle (in degrees) around the matrix from which the light originates. 
#'@param anglebreaks Default `seq(30,40,0.1)`. The azimuth angle(s), in degrees, as measured from the horizon from which the light originates.
#'@param lambert Default `TRUE`. If raytracing, changes the intensity of the light at each point based proportional to the
#'dot product of the ray direction and the surface normal at that point. Zeros out all values directed away from
#'the ray.
#'@param triangulate Default `FALSE`. Reduce the size of the 3D model by triangulating the height map.
#'Set this to `TRUE` if generating the model is slow, or moving it is choppy. Will also reduce the size
#'of 3D models saved to disk.
#'@param max_error Default `0.001`. Maximum allowable error when triangulating the height map,
#'when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with `render_highquality()`, or need 
#'to save a smaller 3D OBJ file to disk with `save_obj()`,
#'@param max_tri Default `0`, which turns this setting off and uses `max_error`. 
#'Maximum number of triangles allowed with triangulating the
#'height map, when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with `render_highquality()`, or need 
#'to save a smaller 3D OBJ file to disk with `save_obj()`,
#'@param verbose Default `TRUE`, if `interactive()`. Prints information about the mesh triangulation
#'if `triangulate = TRUE`.
#'@param reduce_size Default `NULL`. A number between `0` and `1` that specifies how much to reduce the resolution of the plot, for faster plotting. By
#'default, this just decreases the size of height map, not the image. If you wish the image to be reduced in resolution as well, pass a numeric vector of size 2.
#'@param multicore Default `FALSE`. If raytracing and `TRUE`, multiple cores will be used to compute the shadow matrix. By default, this uses all cores available, unless the user has
#'set `options("cores")` in which the multicore option will only use that many cores.
#'@param save_height_matrix Default `FALSE`. If `TRUE`, the function will return the height matrix used for the ggplot.
#'@param save_shadow_matrix Default `FALSE`. If `TRUE`, the function will return the shadow matrix for use in future updates via the `shadow_cache` argument passed to `ray_shade`.
#'@param saved_shadow_matrix Default `NULL`. A cached shadow matrix (saved by the a previous invocation of `plot_gg(..., save_shadow_matrix=TRUE)` to use instead of raytracing a shadow map each time.
#'@param ... Additional arguments to be passed to `plot_3d()`.
#'@return Opens a 3D plot in rgl.
#'@export
#'@examples
#'if(interactive()) {
#'library(ggplot2)
#'library(viridis)
#'\dontshow{
#'options("cores"=2)
#'}
#'
#'ggdiamonds = ggplot(diamonds, aes(x, depth)) +
#'  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", n = 200, bins = 50,contour = TRUE) +
#'  facet_wrap(clarity~.) +
#'  scale_fill_viridis_c(option = "A")
#'\dontrun{
#'plot_gg(ggdiamonds,multicore = TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
#'        zoom = 0.55, phi = 30)
#'render_snapshot()
#'}
#'#Change the camera angle and take a snapshot:
#'\dontrun{
#'render_camera(zoom=0.5,theta=-30,phi=30)
#'render_snapshot(clear = TRUE)
#'}
#'
#'#Contours and other lines will automatically be ignored. Here is the volcano dataset:
#'
#'ggvolcano = volcano %>% 
#'  reshape2::melt() %>%
#'  ggplot() +
#'  geom_tile(aes(x=Var1,y=Var2,fill=value)) +
#'  geom_contour(aes(x=Var1,y=Var2,z=value),color="black") +
#'  scale_x_continuous("X",expand = c(0,0)) +
#'  scale_y_continuous("Y",expand = c(0,0)) +
#'  scale_fill_gradientn("Z",colours = terrain.colors(10)) +
#'  coord_fixed()
#'ggvolcano
#'
#'\dontrun{
#'plot_gg(ggvolcano, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
#'        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
#'render_snapshot(clear = TRUE)
#'}
#'#Here, we will create a 3D plot of the mtcars dataset. This automatically detects 
#'#that the user used the `color` aesthetic instead of the `fill`.
#'mtplot = ggplot(mtcars) + 
#'  geom_point(aes(x=mpg,y=disp,color=cyl)) + 
#'  scale_color_continuous(limits=c(0,8)) 
#'
#'#Preview how the plot will look by setting `preview = TRUE`: We also adjust the angle of the light.
#'\dontrun{
#'plot_gg(mtplot, width=3.5, sunangle=225, preview = TRUE)
#'}
#'\dontrun{
#'plot_gg(mtplot, width=3.5, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
#'        zoom = 0.60, phi = 30, theta = 45)
#'render_snapshot(clear = TRUE)
#'}
#'#Now let's plot a density plot in 3D.
#'mtplot_density = ggplot(mtcars) + 
#'  stat_density_2d(aes(x=mpg,y=disp, fill=..density..), geom = "raster", contour = FALSE) +
#'  scale_x_continuous(expand=c(0,0)) +
#'  scale_y_continuous(expand=c(0,0)) +
#'  scale_fill_gradient(low="pink", high="red")
#'mtplot_density
#'\dontrun{
#'plot_gg(mtplot_density, width = 4,zoom = 0.60, theta = -45, phi = 30, 
#'        windowsize = c(1400,866))
#'render_snapshot(clear = TRUE)
#'}
#'#This also works facetted.
#'mtplot_density_facet = mtplot_density + facet_wrap(~cyl) 
#'
#'#Preview this plot in 2D:
#'\dontrun{
#'plot_gg(mtplot_density_facet, preview = TRUE)
#'}
#'\dontrun{
#'plot_gg(mtplot_density_facet, windowsize=c(1400,866),
#'        zoom = 0.55, theta = -10, phi = 25)
#'render_snapshot(clear = TRUE)
#'}
#'#That is a little cramped. Specifying a larger width will improve the readability of this plot.
#'\dontrun{
#'plot_gg(mtplot_density_facet, width = 6, preview = TRUE)
#'}
#'
#'#That's better. Let's plot it in 3D, and increase the scale.
#'\dontrun{
#'plot_gg(mtplot_density_facet, width = 6, windowsize=c(1400,866),
#'        zoom = 0.55, theta = -10, phi = 25, scale=300)
#'render_snapshot(clear = TRUE)
#'}
#'}
plot_gg = function(ggobj, width = 3, height = 3, 
                   height_aes = NULL, invert = FALSE, shadow_intensity = 0.5,
                   units = c("in", "cm", "mm"), scale=150, pointcontract = 0.7, offset_edges = FALSE,
                   preview = FALSE, raytrace = TRUE, sunangle = 315, anglebreaks = seq(30,40,0.1), 
                   multicore = FALSE, lambert=TRUE, triangulate = TRUE,
                   max_error = 0.001, max_tri = 0, verbose= FALSE,
                   reduce_size = NULL, save_height_matrix = FALSE, 
                   save_shadow_matrix = FALSE, saved_shadow_matrix=NULL, ...) {
  if(!"ggplot2" %in% rownames(utils::installed.packages())) {
    stop("Must have ggplot2 installed to use plot_gg()")
  }
  heightmaptemp = tempfile()
  colormaptemp = tempfile()
  if(methods::is(ggobj,"list") && length(ggobj) == 2) {
    ggplotobj2 = unserialize(serialize(ggobj[[2]], NULL))
    ggplot2::ggsave(paste0(colormaptemp,".png"),ggobj[[1]],width = width,height = height)
  } else {
    ggplotobj2 = unserialize(serialize(ggobj, NULL))
    ggplot2::ggsave(paste0(colormaptemp,".png"),ggplotobj2,width = width,height = height)
  }
  isfill = FALSE
  iscolor = FALSE
  if(is.null(height_aes)) {
    for(i in seq_len(length(ggplotobj2$layers))) {
      if("fill" %in% names(ggplotobj2$layers[[i]]$mapping)) {
        isfill = TRUE
      }
      if(any(c("color","colour") %in% names(ggplotobj2$layers[[i]]$mapping))) {
        iscolor = TRUE
      }
    }
    if(!iscolor && !isfill) {
      if("fill" %in% names(ggplotobj2$mapping)) {
        isfill = TRUE
      }
      if(any(c("color","colour") %in% names(ggplotobj2$mapping))) {
        iscolor = TRUE
      }
    }
    if(isfill && !iscolor) {
      height_aes = "fill"
    } else if (!isfill && iscolor) {
      height_aes = "colour"
    } else if (isfill && iscolor) {
      height_aes = "fill"
    } else {
      height_aes = "fill"
    }
  }
  if(height_aes == "color") {
    height_aes = "colour"
  }
  if(is.numeric(offset_edges)) {
    polygon_offset_value = offset_edges
    offset_edges = TRUE
  } else {
    polygon_offset_value = 0.5
  }
  polygon_offset_geoms = c("GeomPolygon","GeomSf", "GeomHex", "GeomTile")
  other_height_type = ifelse(height_aes == "colour", "fill", "colour")
  colortheme = c("line","rect","text","axis.title", "axis.title.x",
    "axis.title.x.top","axis.title.y","axis.title.y.right","axis.text",
    "axis.text.x" ,"axis.text.x.top","axis.text.y","axis.text.y.right",
    "axis.ticks" ,"axis.ticks.length","axis.line"  ,"axis.line.x",
    "axis.line.y","legend.background","legend.margin","legend.spacing",
    "legend.spacing.x","legend.spacing.y","legend.key" ,"legend.key.size",
    "legend.key.height","legend.key.width","legend.text","legend.text.align",
    "legend.title","legend.title.align","legend.position","legend.direction",
    "legend.justification" ,"legend.box","legend.box.margin","legend.box.background",
    "legend.box.spacing","panel.background","panel.border","panel.spacing",
    "panel.spacing.x","panel.spacing.y","panel.grid" ,"panel.grid.minor",
    "panel.ontop","plot.background","plot.title" ,"plot.subtitle",
    "plot.caption","plot.tag","plot.tag.position","plot.margin",
    "strip.background","strip.placement","strip.text" ,"strip.text.x",
    "strip.text.y","strip.switch.pad.grid","strip.switch.pad.wrap","panel.grid.major",
    "title","axis.ticks.length.x","axis.ticks.length.y","axis.ticks.length.x.top",
    "axis.ticks.length.x.bottom","axis.ticks.length.y.left","axis.ticks.length.y.right","axis.title.x.bottom",
    "axis.text.x.bottom","axis.text.y.left","axis.title.y.left", "aspect.ratio")
  
  key_theme_elements = c("text", "line", "axis.line", "axis.title", 
                         "axis.title.x",
                         "axis.title.y",
                         "axis.text", 
                         "axis.text.x", "axis.text.y", "axis.text.x.top", "axis.text.x.bottom", 
                         "axis.text.y.left", "axis.text.y.right",
                         "axis.ticks", "strip.background", "strip.text", "legend.text", "strip.text.x","strip.text.y",
                         "legend.title","legend.background", "legend.title", "panel.background")
  theme_bool = rep(TRUE,length(key_theme_elements))
  names(theme_bool) = key_theme_elements

  typetheme = c("line","rect","text","text","text", 
    "text","text","text","text",
    "text","text","text","text",
    "line","unit","line","line",
    "line","rect","margin","unit",
    "unit","unit","rect","unit", 
    "unit","unit","text","none",
    "text","none","none","none", 
    "none","rect","margin","rect",
    "unit","rect","rect","unit",
    "unit","unit","line","line", 
    "none","rect","text","text",
    "text","text","none","margin",
    "rect","none","text","text",
    "text","unit","unit","line",
    "text","line","line","line",
    "line","line","line","text",
    "text","text","text","none")
  black_white_pal = function(x) {
    grDevices::colorRampPalette(c("white", "black"))(255)[x * 254 + 1]
  }
  white_white_pal = function(x) {
    grDevices::colorRampPalette(c("white", "white"))(255)[x * 254 + 1]
  }
  ifelsefxn = function(entry) {
    if(!is.null(entry)) {
      return(entry)
    }
  }
  
  #aes_with_guides = c("size","shape","colour","fill","alpha","linetype")
  #Shift all continuous palettes of height_aes to black/white, and set all discrete key colors to white.
  if(ggplotobj2$scales$n() != 0) {
    anyfound = FALSE
    #Check to see if same guide being used for both color and fill aesthetics
    if(ggplotobj2$scales$has_scale("colour") && ggplotobj2$scales$has_scale("fill")) {
      fillscale = ggplotobj2$scales$get_scales("fill")
      colorscale = ggplotobj2$scales$get_scales("colour")
      same_limits = FALSE
      same_breaks = FALSE
      same_labels = FALSE
      same_calls = FALSE
      if((!is.null(fillscale$limits) && !is.null(colorscale$limits))) {
        if(fillscale$limits == colorscale$limits) {
          same_limits = TRUE
        }
      } else if (is.null(fillscale$limits) && is.null(colorscale$limits)) {
        same_limits = TRUE
      }
      if((!is.null(fillscale$breaks) && !is.null(colorscale$breaks))) {
        if(all(fillscale$breaks == colorscale$breaks)) {
          same_breaks = TRUE
        }
      } else if (is.null(fillscale$breaks) && is.null(colorscale$breaks)) {
        same_breaks = TRUE
      }
      if((class(fillscale$labels) != "waiver" && class(colorscale$labels) != "waiver")) {
        if(all(fillscale$labels == colorscale$labels)) {
          same_labels = TRUE
        }
      } else if ((class(fillscale$labels) == "waiver" && class(colorscale$labels) == "waiver")) {
        same_labels = TRUE
      }
      if(fillscale$call == colorscale$call) {
        same_calls = TRUE
      }
      if(same_limits && same_breaks && same_labels && same_calls) {
        if(height_aes == "fill") {
          ggplotobj2 = ggplotobj2 + ggplot2::guides(color = "none")
        } else {
          ggplotobj2 = ggplotobj2 + ggplot2::guides(fill = "none")
        }
      }
    }
    #Now check for scales and change to the b/w palette, but preserve guide traits.
    for(i in seq_len(ggplotobj2$scales$n())) {
      if(height_aes %in% ggplotobj2$scales$scales[[i]]$aesthetics) {
        ggplotobj2$scales$scales[[i]]$palette = black_white_pal
        ggplotobj2$scales$scales[[i]]$na.value = "white"
        has_guide = !any("guide" %in% class(ggplotobj2$scales$scales[[i]]$guide))
        if(any(c("logical" %in% class(ggplotobj2$scales$scales[[i]]$guide)))) {
          has_guide = ggplotobj2$scales$scales[[i]]$guide
        }
        if(has_guide) {
          if(height_aes == "fill") {
            if(is.null(ggplotobj2$guides$fill)) {
              ggplotobj2 = ggplotobj2 + ggplot2::guides(fill = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000,order=i))
            } else {
              if(any(ggplotobj2$guides$fill != "none")) {
                copyguide = ggplotobj2$guides$fill
                copyguide$frame.linewidth = 0
                copyguide$ticks = FALSE
                copyguide$nbin = 1000
                ggplotobj2 = ggplotobj2 + 
                  ggplot2::guides(fill = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000))
                ggplotobj2$guides$fill = copyguide
              }
            }
            for(j in seq_len(length(ggplotobj2$layers))) {
              if("colour" %in% names(ggplotobj2$layers[[j]]$mapping)) {
                ggplotobj2$layers[[j]]$geom$draw_key = drawkeyfunction_points
              }
            }
          } else {
            if(is.null(ggplotobj2$guides$colour)) {
              ggplotobj2 = ggplotobj2 + ggplot2::guides(colour = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000,order=i))
            } else {
              if(any(ggplotobj2$guides$colour != "none")) {
                copyguide = ggplotobj2$guides$colour
                copyguide$frame.linewidth = 0
                copyguide$ticks = FALSE
                copyguide$nbin = 1000
                ggplotobj2 = ggplotobj2 + 
                  ggplot2::guides(colour = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000))
                ggplotobj2$guides$colour = copyguide
              }
            }
          }
        }
        anyfound = TRUE
      } else if(other_height_type %in% ggplotobj2$scales$scales[[i]]$aesthetics) {
        #change guides for other height_aes to be the all white palette
        ggplotobj2$scales$scales[[i]]$palette = white_white_pal
        ggplotobj2$scales$scales[[i]]$na.value = "white"
      } 
    }
    #If no scales found, just add one to the ggplot object.
    if(!anyfound) {
      if(height_aes == "colour") {
        ggplotobj2 = ggplotobj2 + 
          ggplot2::scale_color_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          ggplot2::guides(colour = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000))
      }
      if(height_aes == "fill") {
        ggplotobj2 = ggplotobj2 + 
          ggplot2::scale_fill_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          ggplot2::guides(fill = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000))
      }
    }
  } else {
    #If no scales found, just add one to the ggplot object.
    if(ggplotobj2$scales$n() == 0) {
      if(height_aes == "fill") {
        ggplotobj2 = ggplotobj2 + 
          ggplot2::scale_fill_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          ggplot2::guides(fill = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000))
      } else {
        ggplotobj2 = ggplotobj2 + 
          ggplot2::scale_color_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          ggplot2::guides(colour = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000))
      }
    } else {
      if(height_aes == "fill") {
        ggplotobj2 = ggplotobj2 + ggplot2::scale_fill_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          ggplot2::guides(fill = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000))
      } else {
        ggplotobj2 = ggplotobj2 + ggplot2::scale_color_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          ggplot2::guides(colour = ggplot2::guide_colourbar(ticks = FALSE,nbin = 1000))
      }
    }
  }
  #Set all elements to white if custom theme passed, and color aesthetic geoms to size = 0 if height_aes == "fill"
  if(length(ggplotobj2$theme) > 0) {
    if(height_aes == "fill") {
      for(layer in seq_along(1:length(ggplotobj2$layers))) {
        if("colour" %in% names(ggplotobj2$layers[[layer]]$mapping) || 
           0 == length(names(ggplotobj2$layers[[layer]]$mapping))) {
          ggplotobj2$layers[[layer]]$aes_params$colour = "white"
        }
        if("fill" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$aes_params$size = NA
          if(any(polygon_offset_geoms %in% class(ggplotobj2$layers[[layer]]$geom)) && offset_edges) {
            ggplotobj2$layers[[layer]]$aes_params$size = polygon_offset_value
            ggplotobj2$layers[[layer]]$aes_params$colour = "white"
          }
        }
        if("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          shapedata = ggplot2::layer_data(ggplotobj2)
          numbershapes = length(unique(shapedata$shape))
          if(numbershapes > 3) {
            warning("Non-solid shapes will not be projected to 3D.")
          }
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
          for(j in seq_len(length(ggplotobj2$layers))) {
            if("stroke" %in% names(ggplotobj2$layers[[j]]$geom$default_aes)) {
              ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
            }
          }
          ggplotobj2 = suppressMessages({ggplotobj2 + ggplot2::scale_alpha_continuous(range=c(1,1))})
        }
        if("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
        }
      }
    } else {
      for(layer in seq_along(1:length(ggplotobj2$layers))) {
        if("fill" %in% names(ggplotobj2$layers[[layer]]$mapping) || 
           0 == length(names(ggplotobj2$layers[[layer]]$mapping))) {
          ggplotobj2$layers[[layer]]$aes_params$fill = "white"
        }
        if("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          shapedata = ggplot2::layer_data(ggplotobj2)
          numbershapes = length(unique(shapedata$shape))
          if(numbershapes > 3) {
            warning("Non-solid shapes will not be projected to 3D.")
          }
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
          for(j in seq_len(length(ggplotobj2$layers))) {
            if("stroke" %in% names(ggplotobj2$layers[[j]]$geom$default_aes)) {
              ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
            }
          }
          ggplotobj2 = suppressMessages({ggplotobj2 + ggplot2::scale_alpha_continuous(range=c(1,1))})
        }
        if("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
        }
      }
    }
    #switch all elements to white
    for(i in 1:length(ggplotobj2$theme)) {
      tempname = names(ggplotobj2$theme[i])
      if(tempname %in% key_theme_elements) {
        theme_bool[tempname] = FALSE
      } else if ("element_blank" %in% class(ggplotobj2$theme[[i]])) {
        theme_bool[tempname] = FALSE
      }
      whichtype = typetheme[which(tempname == colortheme)]
      if(length(whichtype) > 0) {
        if(whichtype %in% c("text","line")) {
          if(!is.null(ggplotobj2$theme[[i]])) {
            ggplotobj2$theme[[i]]$colour = "white"
          }
        } else if(whichtype == "rect") {
          if(!(tempname %in% c("panel.border","rect"))) {
            if(!is.null(ggplotobj2$theme[[i]])) {
              ggplotobj2$theme[[i]]$colour = "white"
              ggplotobj2$theme[[i]]$fill = "white"
            }
          } else {
            ggplotobj2$theme[[i]]$colour = "white"
            ggplotobj2$theme[[i]]$fill = NA
          }
        } 
      }
    }
    if(ggplotobj2$scales$n() > 0) {
      for(i in 1:ggplotobj2$scales$n()) {
        if(length(ggplotobj2$scales$scales[[i]]$guide) > 1) {
          ggplotobj2$scales$scales[[i]]$guide$frame.colour = "white"
          ggplotobj2$scales$scales[[i]]$guide$ticks = FALSE
          ggplotobj2$scales$scales[[i]]$guide$nbin = 256
          ggplotobj2$scales$scales[[i]]$guide$draw.llim = FALSE
          ggplotobj2$scales$scales[[i]]$na.value = "white"
        }
      }
    }
    if(theme_bool["text"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(text = ggplot2::element_text(color="white"))
    if(theme_bool["line"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(line = ggplot2::element_line(color="white"))
    if(theme_bool["axis.line"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(axis.line = ggplot2::element_line(color="white"))
    if(theme_bool["axis.title"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(axis.title = ggplot2::element_text(color="white"))
    if(theme_bool["axis.title.x"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(axis.title.x = ggplot2::element_text(color="white"))
    if(theme_bool["axis.title.y"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(axis.title.y = ggplot2::element_text(color="white"))
    if(theme_bool["axis.text"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(axis.text = ggplot2::element_text(color="white"))
    if(theme_bool["axis.text.x"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(axis.text.x = ggplot2::element_text(color="white"))
    if(theme_bool["axis.text.y"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(axis.text.y = ggplot2::element_text(color="white"))
    if(theme_bool["strip.text.x"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(strip.text.x = ggplot2::element_text(color="white"))
    if(theme_bool["strip.text.y"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(strip.text.y = ggplot2::element_text(color="white"))
    if(theme_bool["axis.ticks"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(axis.ticks = ggplot2::element_line(color="white"))
    if(theme_bool["strip.background"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white", color = "white"))
    if(theme_bool["strip.text"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(strip.text = ggplot2::element_text(color="white"))
    if(theme_bool["legend.text"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(legend.text = ggplot2::element_text(color="white"))
    if(theme_bool["legend.title"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(legend.title = ggplot2::element_text(color="white"))
    if(theme_bool["legend.background"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(legend.background = ggplot2::element_rect(fill = "white", color = "white"))
    if(theme_bool["legend.title"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(legend.title = ggplot2::element_text(color="white"))
    if(theme_bool["panel.background"]) ggplotobj2 = ggplotobj2 + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "white"))
  } else {
    if(height_aes == "fill") {
      for(layer in seq_along(1:length(ggplotobj2$layers))) {
        if("colour" %in% names(ggplotobj2$layers[[layer]]$mapping) || 
           0 == length(names(ggplotobj2$layers[[layer]]$mapping))) {
          ggplotobj2$layers[[layer]]$aes_params$colour = "white"
        }
        if("fill" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$aes_params$size = NA
          if(any(polygon_offset_geoms %in% class(ggplotobj2$layers[[layer]]$geom)) && offset_edges) {
            ggplotobj2$layers[[layer]]$aes_params$size = polygon_offset_value
            ggplotobj2$layers[[layer]]$aes_params$colour = "white"
          }
        }
        if("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          shapedata = ggplot2::layer_data(ggplotobj2)
          numbershapes = length(unique(shapedata$shape))
          if(numbershapes > 3) {
            warning("Non-solid shapes will not be projected to 3D.")
          }
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
          for(j in seq_len(length(ggplotobj2$layers))) {
            if("stroke" %in% names(ggplotobj2$layers[[j]]$geom$default_aes)) {
              ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
            }
          }
          ggplotobj2 = suppressMessages({ggplotobj2 + ggplot2::scale_alpha_continuous(range=c(1,1))})
        }
        if("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
        }
      }
    } else {
      for(layer in seq_len(length(ggplotobj2$layers))) {
        if("fill" %in% names(ggplotobj2$layers[[layer]]$mapping) || 
           0 == length(names(ggplotobj2$layers[[layer]]$mapping))) {
          ggplotobj2$layers[[layer]]$aes_params$fill = "white"
        }
        if("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          shapedata = ggplot2::layer_data(ggplotobj2)
          numbershapes = length(unique(shapedata$shape))
          if(numbershapes > 3) {
            warning("Non-solid shapes will not be projected to 3D.")
          }
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
          for(j in seq_len(length(ggplotobj2$layers))) {
            if("stroke" %in% names(ggplotobj2$layers[[j]]$geom$default_aes)) {
              ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
            }
          }
          ggplotobj2 = suppressMessages({ggplotobj2 + ggplot2::scale_alpha_continuous(range=c(1,1))})
        }
        if("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
        }
      }
    }
    #No custom theme passed, just create one.
    ggplotobj2 = ggplotobj2 +
      ggplot2::theme(text = ggplot2::element_text(color="white"),
            line = ggplot2::element_line(color="white"),
            axis.line = ggplot2::element_line(color="white"),
            axis.title = ggplot2::element_text(color="white"),
            axis.text = ggplot2::element_text(color="white"),
            axis.ticks = ggplot2::element_line(color="white"),
            strip.background = ggplot2::element_rect(fill = "white", color = "white"),
            strip.text = ggplot2::element_text(color="white"),
            legend.key = ggplot2::element_rect(fill = "white", color = "white"),
            legend.text = ggplot2::element_text(color="white"),
            legend.background = ggplot2::element_rect(fill = "white", color = "white"),
            legend.title = ggplot2::element_text(color="white"),
            panel.background = ggplot2::element_rect(fill = "white", color = "white"))
  }
  if(height_aes == "fill") {
    if(length(ggplotobj2$layers) > 0) {
      for(i in seq_along(1:length(ggplotobj2$layers))) {
        ggplotobj2$layers[[i]]$aes_params$size = NA
        if(any(polygon_offset_geoms %in% class(ggplotobj2$layers[[layer]]$geom)) && offset_edges) {
          ggplotobj2$layers[[i]]$aes_params$size = polygon_offset_value
          ggplotobj2$layers[[i]]$aes_params$colour = "white"
        }
      }
    }
  } else {
    if(length(ggplotobj2$layers) > 0) {
      for(i in seq_along(1:length(ggplotobj2$layers))) {
        ggplotobj2$layers[[i]]$aes_params$fill = "white"
        if("GeomContour" %in% class(ggplotobj2$layers[[i]]$geom)) {
          ggplotobj2$layers[[i]]$aes_params$alpha = 0
        }
      }
      if(pointcontract != 1) {
        for(i in 1:length(ggplotobj2$layers)) {
          if(!is.null(ggplotobj2$layers[[i]]$aes_params$size)) {
            ggplotobj2$layers[[i]]$aes_params$size = ggplotobj2$layers[[i]]$aes_params$size * pointcontract
          } else {
            ggplotobj2$layers[[i]]$geom$default_aes$size = ggplotobj2$layers[[i]]$geom$default_aes$size * pointcontract
          }
        }
      }
    }
  }
  tryCatch({
    ggplot2::ggsave(paste0(heightmaptemp,".png"),ggplotobj2,width = width,height = height)
  }, error = function(e) {
    if(any(grepl("Error: Discrete value supplied to continuous scale", as.character(e),fixed = TRUE))) {
      stop(paste0("Error: Discrete variable cannot be mapped to 3D. Did you mean to choose `",ifelse(height_aes == "fill","color","fill"), "` as the `height_aes`?"),call.=FALSE)
    }
  })
  if(!is.null(reduce_size)) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("magick package required to use argument reduce_size")
    } else {
      if(length(reduce_size) == 1 && reduce_size < 1) {
        scale = scale * reduce_size
        image_info = magick::image_read(paste0(heightmaptemp,".png")) %>%
          magick::image_info() 
        magick::image_read(paste0(heightmaptemp,".png")) %>%
          magick::image_resize(paste0(image_info$width * reduce_size,"x",image_info$height * reduce_size)) %>%
          magick::image_write(paste0(heightmaptemp,".png"))
      } else if (length(reduce_size) == 2 && all(reduce_size < 1)) {
        scale = scale * reduce_size[1]
        image_info = magick::image_read(paste0(heightmaptemp,".png")) %>%
          magick::image_info() 
        magick::image_read(paste0(heightmaptemp,".png")) %>%
          magick::image_resize(paste0(image_info$width * reduce_size[1],"x",image_info$height * reduce_size[1])) %>%
          magick::image_write(paste0(heightmaptemp,".png"))
        magick::image_read(paste0(colormaptemp,".png")) %>%
          magick::image_resize(paste0(image_info$width * reduce_size[2],"x",image_info$height * reduce_size[2])) %>%
          magick::image_write(paste0(colormaptemp,".png"))
      }
    }
  }
  mapcolor = png::readPNG(paste0(colormaptemp,".png"))
  mapheight = png::readPNG(paste0(heightmaptemp,".png"))
  if(length(dim(mapheight)) == 3) {
    mapheight = mapheight[,,1]
  } 
  if(invert) {
    mapheight = 1 - mapheight
  }
  if(raytrace) {
    if(is.null(saved_shadow_matrix)) {
      raylayer = ray_shade(t(1-mapheight),maxsearch = 600,sunangle = sunangle,anglebreaks = anglebreaks,
                           zscale=1/scale,multicore = multicore,lambert = lambert, ...)
      if(!preview) {
        mapcolor %>%
          add_shadow(raylayer,shadow_intensity) %>%
          plot_3d((t(1-mapheight)),zscale=1/scale, triangulate = triangulate,
                  max_error = max_error, max_tri = max_tri, verbose = verbose, ... )
      } else {
        mapcolor %>%
          add_shadow(raylayer,shadow_intensity) %>%
          plot_map(keep_user_par = FALSE)
      }
    } else {
      raylayer = saved_shadow_matrix
      if(!preview) {
        mapcolor %>%
          add_shadow(raylayer,shadow_intensity) %>%
          plot_3d((t(1-mapheight)),zscale=1/scale, triangulate = triangulate,
                  max_error = max_error, max_tri = max_tri, verbose = verbose, ... )
      } else {
        mapcolor %>%
          add_shadow(raylayer,shadow_intensity) %>%
          plot_map(keep_user_par = FALSE)
      }
    }
  } else {
    if(!preview) {
      plot_3d(mapcolor, (t(1-mapheight)), zscale=1/scale, triangulate = triangulate,
              max_error = max_error, max_tri = max_tri, verbose = verbose, ...)
    } else {
      plot_map(mapcolor, keep_user_par = FALSE)
    }
  }
  if(save_shadow_matrix & !save_height_matrix) {
    return(raylayer)
  }
  if(!save_shadow_matrix & save_height_matrix) {
    return(1-t(mapheight))
  }
  if(save_shadow_matrix & save_height_matrix) {
    return(list(1-t(mapheight),raylayer))
  }
  
}