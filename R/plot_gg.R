get_plot_gg_grob_background = function(grob, default = "white") {
  background_index = which(grob$layout$name == "background")
  if (!length(background_index)) {
    return(default)
  }
  background_grob = grob$grobs[[background_index[[1]]]]
  fill = background_grob$gp$fill
  if (is.null(fill) || !length(fill)) {
    return(default)
  }
  fill = fill[[1]]
  if (is.na(fill) || !nzchar(fill) || tolower(fill) == "transparent") {
    return(default)
  }
  fill_rgba = tryCatch(
    grDevices::col2rgb(fill, alpha = TRUE) / 255,
    error = function(e) NULL
  )
  default_rgb = tryCatch(
    grDevices::col2rgb(default) / 255,
    error = function(e) grDevices::col2rgb("white") / 255
  )
  if (is.null(fill_rgba)) {
    return(default)
  }
  fill_alpha = fill_rgba[4, 1]
  grob_alpha = background_grob$gp$alpha
  if (!is.null(grob_alpha) && length(grob_alpha) && !is.na(grob_alpha[[1]])) {
    fill_alpha = fill_alpha * grob_alpha[[1]]
  }
  if (!is.finite(fill_alpha) || fill_alpha <= 0) {
    return(default)
  }
  fill_rgb = fill_rgba[1:3, 1] *
    fill_alpha +
    default_rgb[1:3, 1] * (1 - fill_alpha)
  grDevices::rgb(fill_rgb[[1]], fill_rgb[[2]], fill_rgb[[3]])
}

remove_plot_gg_grob_background_line = function(grob) {
  background_index = which(grob$layout$name == "background")
  if (!length(background_index)) {
    return(grob)
  }
  for (index in background_index) {
    if (is.null(grob$grobs[[index]]$gp)) {
      grob$grobs[[index]]$gp = grid::gpar()
    }
    grob$grobs[[index]]$gp$col = NA
    grob$grobs[[index]]$gp$lwd = 0
    class(grob$grobs[[index]]$gp) = "gpar"
  }
  grob
}

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
#'@param ggobj_height Default `NULL`. A ggplot object that can be used to specify the 3D extrusion separately from the
#'`ggobj`. If this plot includes a `tidyterra::geom_spatraster()` height layer,
#'rayshader preserves the mapped raster data scale when building the height map
#'and uses the final rendered scene spacing as the default `zscale` unless
#'`zscale` is supplied.
#'@param width Default `3`. Width of ggplot, in `units`.
#'@param height Default `3`. Height of ggplot, in `units`.
#'@param height_aes Default `NULL`. Whether the `fill` or `color` aesthetic should be used for height values,
#'which the user can specify by passing either `fill` or `color` to this argument.
#'Automatically detected. If both `fill` and `color` aesthetics are present, then `fill` is default.
#'@param invert Default `FALSE`. If `TRUE`, the height mapping is inverted.
#'@param shadow_intensity Default `0.5`. The intensity of the calculated shadows.
#'When `raytrace = "radiance"`, this uses the same scale as the default raytraced
#'shadowing: `1` means no additional shading effect, and lower values increase the
#'strength of the radiance overlay.
#'@param units Default `in`. One of c("in", "cm", "mm").
#'@param zscale Default `NULL`. The ratio between the x/y spacing and the z axis
#'for the height surface. If omitted and `ggobj_height` includes a spatial raster
#'height layer, rayshader derives `zscale` from the final rendered ggplot scene
#'extent and output matrix spacing, which accounts for ggplot rasterization and
#'reprojection. Otherwise, the base `zscale` defaults to `1`.
#'@param vertical_exaggeration Default `NULL`. One-off multiplier applied to the
#'effective relief after resolving `zscale`. When omitted, rayshader uses `1`
#'for spatial raster height sources and `150` for non-raster ggplot height
#'mappings, which preserves the legacy `plot_gg()` default appearance.
#'@param scale Deprecated. Use `vertical_exaggeration` instead.
#'@param pointcontract Default `0.7`. This multiplies the size of the points and shrinks
#'them around their center in the 3D surface mapping. Decrease this to reduce color bleed on edges, and set to
#'`1` to turn off entirely. Note: If `size` is passed as an aesthetic to the same geom
#'that is being mapped to elevation, this scaling will not be applied. If `alpha` varies on the variable
#'being mapped, you may want to set this to `1`, since the points now have a non-zero width stroke outline (however,
#'mapping `alpha` in the same variable you are projecting to height is probably not a good choice. as the `alpha`
#'variable is ignored when performing the 3D projection).
#'@param offset_edges Default `FALSE`. If `TRUE`, inserts a small amount of space between polygons for "geom_sf", "geom_tile", "geom_hex", and "geom_polygon" layers.
#'If you pass in a number, the space between polygons will be a line of that width. You can also specify a number to control the thickness of the offset.
#'Note: this feature may end up removing thin polygons from the plot entirely--use with care.
#'@param flat_substrate Default `FALSE`. If `TRUE`, render the captured ggplot texture on flat solid plot panels instead of using the panel aesthetics as the surface height. Guides and legends retain their rendered height mapping, and the resulting scene can be used as a base for later `render_*()` calls.
#'@param flat_plot_render Default `FALSE`. Whether to render a flat version of the ggplot above (or alongside) the 3D version.
#'@param flat_distance Default `"auto"`. Distance to render the flat version of the plot from the 3D version.
#'@param flat_transparent_bg Default `FALSE`. Whether to set the background of the flat version of the ggplot to transparent.
#'@param flat_direction Default `"-z"`. Direction to render the flat copy of the plot, if `flat_plot_render = TRUE`.
#'Other options `c("z", "x", "-x", "y", "-y")`.
#'@param shadow Default `TRUE`. If `FALSE`, no shadow is rendered.
#'@param shadowdepth Default `auto`, which sets it to `soliddepth - soliddepth/10`. Depth of the shadow layer.
#'@param shadow_darkness Default `0.5`. Darkness of the shadow, if `shadowcolor = "auto"`.
#'@param shadowcolor Default `auto`. Color of the shadow, automatically computed as `shadow_darkness`
#'the luminance of the `background` color in the CIELab colorspace if not specified.
#'@param background Default `"white"`. Background color for the 3D scene.
#'This is independent of the ggplot `plot.background` fill used for the
#'captured texture.
#'@param preview Default `FALSE`. If `TRUE`, the raytraced 2D ggplot will be displayed on the current device.
#'@param raytrace Default `TRUE`. Controls the additional shading applied to the
#'ggplot texture. Use `TRUE`/`"raytrace"` for [ray_shade()], `FALSE`/`"none"`
#'for no extra shading, or `"radiance"` for [radiance_shade()].
#'@param radiance_args Default `list()`. Additional arguments passed to
#'[radiance_shade()] when `raytrace = "radiance"`. By default,
#'`lightdirection` is derived from `sunangle` and `lightaltitude` from the
#'midpoint of `anglebreaks`.
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
#'to decrease render time with [render_highquality()], or need
#'to save a smaller 3D OBJ file to disk with [save_obj()],
#'@param max_tri Default `0`, which turns this setting off and uses `max_error`.
#'Maximum number of triangles allowed with triangulating the
#'height map, when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with [render_highquality()], or need
#'to save a smaller 3D OBJ file to disk with [save_obj()],
#'@param verbose Default `FALSE`. Prints the computed `zscale` and horizontal
#'units. Also prints information about the mesh triangulation if
#'`triangulate = TRUE`.
#'@param emboss_text Default `0`, max `1`. Amount to emboss the text, where `1` is the tallest feature in the scene.
#'@param emboss_grid Default `0`, max `1`. Amount to emboss the grid lines, where `1` is the tallest feature in the scene.
#'By default, the minor grid lines will be half the size of the major lines. Pass a length-2 vector to specify them seperately (second value
#'is the minor grid height).
#'@param guide_bar_bleed_px Default `3L`. Number of rendered pixels to widen a continuous guide colorbar orthogonally without changing the guide layout. This ensures
#' either the sidewalls of the colorbar are either solid or represent the color ramp.
#'@param guide_bar_bleed_target Default `"texture"`. Which rendered grob should receive the colorbar bleed. Use `"height"` to keep white side walls,
#'`"texture"` to color the side walls with the gradient, or `"none"` to disable it (rayshader's old default).
#'@param reduce_size Default `NULL`. A number between `0` and `1` that specifies how much to reduce the resolution of the plot, for faster plotting. By
#'default, this just decreases the size of height map, not the image. If you wish the image to be reduced in resolution as well, pass a numeric vector of size 2.
#'@param multicore Default `FALSE`. If raytracing and `TRUE`, multiple cores will be used to compute the shadow matrix. By default, this uses all cores available, unless the user has
#'set `options("cores")` in which the multicore option will only use that many cores.
#'@param save_height_matrix Default `FALSE`. If `TRUE`, the function will return the height matrix used for the ggplot.
#'@param save_shadow_matrix Default `FALSE`. If `TRUE`, the function will return
#'the computed shading layer. For `raytrace = TRUE`, this is the shadow matrix
#'from [ray_shade()]. For `raytrace = "radiance"`, this is the RGBA radiance
#'overlay from [radiance_shade()].
#'@param saved_shadow_matrix Default `NULL`. A cached shading layer saved by a
#'previous invocation of [plot_gg()] with `save_shadow_matrix = TRUE`. For
#'`raytrace = TRUE`, pass a shadow matrix from [ray_shade()]. For
#'`raytrace = "radiance"`, pass an RGBA radiance overlay.
#'@param monitor_gamma Default `1.8`. Undo the gamma correction applied by the png device. Ignored if `ragg` is installed or the `cairo` PNG device is available.
#'@param plot Default `TRUE`. Whether to plot the image when `preview = TRUE`, or just return the RGBA rayimg.
#'@param ... Additional arguments to be passed to [plot_3d()].
#'@return Opens a 3D plot in rgl.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'library(ggplot2)
#'library(viridis)
#'\dontshow{
#'options("cores"=2)
#'}
#'
#'ggdiamonds = ggplot(diamonds, aes(x, depth)) +
#'  stat_density_2d(aes(fill = after_stat(nlevel), color = after_stat(nlevel)),
#'                  geom = "polygon",
#'                  n = 200, bins = 50,contour = TRUE) +
#'  facet_wrap(clarity~.) +
#'  scale_fill_viridis_c(option = "A") +
#'  scale_color_viridis_c(option = "A")
#'plot_gg(ggdiamonds,multicore = TRUE,width=5,height=5,
#'        vertical_exaggeration=250,windowsize=c(1400,866),
#'        zoom = 0.55, phi = 30)
#'render_snapshot()
#'#Change the camera angle and take a snapshot:
#'render_camera(zoom=0.5,theta=-30,phi=30)
#'render_snapshot()
#'
#'#Contours and other lines will automatically be ignored. Here is the volcano dataset:
#'ggvolcano = volcano |>
#'  reshape2::melt() |>
#'  ggplot() +
#'  geom_tile(aes(x=Var1,y=Var2,fill=value)) +
#'  geom_contour(aes(x=Var1,y=Var2,z=value),color="black") +
#'  scale_x_continuous("X",expand = c(0,0)) +
#'  scale_y_continuous("Y",expand = c(0,0)) +
#'  scale_fill_gradientn("Z",colours = terrain.colors(10)) +
#'  coord_fixed() +
#'  theme(legend.position = "none")
#'ggvolcano
#'
#'plot_gg(ggvolcano, multicore = TRUE, raytrace = TRUE, width = 7, height = 4,
#'        vertical_exaggeration = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
#'render_snapshot()
#'
#'#You can specify the color and height separately using the `ggobj_height()` argument.
#'ggvolcano_surface = volcano |>
#'reshape2::melt() |>
#'  ggplot() +
#'  geom_contour(aes(x=Var1,y=Var2,z=value),color="black") +
#'  geom_contour_filled(aes(x=Var1,y=Var2,z=value))+
#'  scale_x_continuous("X",expand = c(0,0)) +
#'  scale_y_continuous("Y",expand = c(0,0)) +
#'  coord_fixed() +
#'  theme(legend.position = "none")
#'
#'plot_gg(ggvolcano_surface, ggobj_height = ggvolcano,
#'       multicore = TRUE, raytrace = TRUE, width = 7, height = 4,
#'       vertical_exaggeration = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
#'render_snapshot()
#'#Here, we will create a 3D plot of the mtcars dataset. This automatically detects
#'#that the user used the `color` aesthetic instead of the `fill`.
#'mtplot = ggplot(mtcars) +
#'  geom_point(aes(x=mpg,y=disp,color=cyl)) +
#'  scale_color_continuous(limits=c(0,8))
#'
#'#Preview how the plot will look by setting `preview = TRUE`: We also adjust the angle of the light.
#'plot_gg(mtplot, width=3.5, sunangle=225, preview = TRUE)
#'plot_gg(mtplot, width=3.5, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
#'        zoom = 0.60, phi = 30, theta = 45)
#'render_snapshot()
#'
#'#Colorbar side-wall bleed control with a continuous guide:
#'bleedplot = ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'  geom_raster(interpolate = TRUE) +
#'  scale_fill_viridis_c() +
#'  theme_minimal()
#'plot_gg(bleedplot, width = 5, height = 4, vertical_exaggeration = 220,
#'        windowsize = c(1400,866), theta = -20, phi = 35, zoom = 0.60,
#'        guide_bar_bleed_target = "height")
#'render_snapshot()
#'plot_gg(bleedplot, width = 5, height = 4, vertical_exaggeration = 220,
#'        windowsize = c(1400,866), theta = -20, phi = 35, zoom = 0.60,
#'        guide_bar_bleed_target = "texture")
#'render_snapshot()
#'plot_gg(mtplot, width=3.5, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
#'        zoom = 0.60, phi = 30, theta = 45)
#'render_zaxis(zaxis_location = "panel_bottomleft")
#'render_snapshot()
#'#Now let's plot a density plot in 3D.
#'mtplot_density = ggplot(mtcars) +
#'  stat_density_2d(aes(x=mpg,y=disp, fill=after_stat(!!str2lang("density"))),
#'                  geom = "raster", contour = FALSE) +
#'  scale_x_continuous(expand=c(0,0)) +
#'  scale_y_continuous(expand=c(0,0)) +
#'  scale_fill_gradient(low="pink", high="red")
#'mtplot_density
#'
#'plot_gg(mtplot_density, width = 4,zoom = 0.60, theta = -45, phi = 30,
#'        windowsize = c(1400,866))
#'render_snapshot()
#'#This also works facetted.
#'mtplot_density_facet = mtplot_density + facet_wrap(~cyl)
#'
#'#Preview this plot in 2D:
#'plot_gg(mtplot_density_facet, preview = TRUE)
#'plot_gg(mtplot_density_facet, windowsize=c(1400,866),
#'        zoom = 0.55, theta = -10, phi = 25)
#'render_snapshot()
#'#That is a little cramped. Specifying a larger width will improve the readability of this plot.
#'plot_gg(mtplot_density_facet, width = 6, preview = TRUE)
#'
#'#That's better. Let's plot it in 3D, and increase the vertical exaggeration.
#'plot_gg(mtplot_density_facet, width = 6, windowsize=c(1400,866),
#'        zoom = 0.55, theta = -10, phi = 25, vertical_exaggeration=300)
#'render_snapshot()
#'
#'#We can also render a flat version of the plot alongside (or above/below) the 3D version.
#'plot_gg(mtplot_density_facet, width = 6, windowsize=c(1400,866),
#'        zoom = 0.65, theta = -25, phi = 35, vertical_exaggeration=300, flat_plot_render=TRUE,
#'        flat_direction = "x")
#'render_snapshot()
plot_gg = function(
  ggobj,
  ggobj_height = NULL,
  width = 3,
  height = 3,
  height_aes = NULL,
  invert = FALSE,
  shadow_intensity = 0.5,
  units = c("in", "cm", "mm"),
  scale = NULL,
  zscale = NULL,
  vertical_exaggeration = NULL,
  pointcontract = 0.7,
  offset_edges = FALSE,
  flat_substrate = FALSE,
  flat_plot_render = FALSE,
  flat_distance = "auto",
  flat_transparent_bg = FALSE,
  flat_direction = "-z",
  shadow = TRUE,
  shadowdepth = "auto",
  shadowcolor = "auto",
  shadow_darkness = 0.5,
  background = "white",
  preview = FALSE,
  raytrace = TRUE,
  radiance_args = list(),
  sunangle = 315,
  anglebreaks = seq(30, 40, 0.1),
  multicore = FALSE,
  lambert = TRUE,
  triangulate = FALSE,
  max_error = 0.001,
  max_tri = 0,
  verbose = FALSE,
  emboss_text = 0,
  emboss_grid = 0,
  guide_bar_bleed_px = 3L,
  guide_bar_bleed_target = c("height", "texture", "none"),
  reduce_size = NULL,
  save_height_matrix = FALSE,
  save_shadow_matrix = FALSE,
  saved_shadow_matrix = NULL,
  monitor_gamma = 1.8,
  plot = TRUE,
  ...
) {
  if (!(length(find.package("ggplot2", quiet = TRUE)) > 0)) {
    stop("Must have ggplot2 installed to use plot_gg()")
  }
  resolve_plot_gg_shadowdepth = function(
    height_matrix,
    zscale,
    shadowdepth,
    dot_args = list()
  ) {
    solid = TRUE
    if ("solid" %in% names(dot_args) && !is.null(dot_args$solid)) {
      solid = isTRUE(dot_args$solid[[1]])
    }
    soliddepth = if ("soliddepth" %in% names(dot_args)) {
      dot_args$soliddepth
    } else {
      "auto"
    }
    min_height = min(height_matrix, na.rm = TRUE)
    max_height = max(height_matrix, na.rm = TRUE)

    if (identical(soliddepth, "auto")) {
      if (min_height != max_height) {
        soliddepth = min_height /
          zscale -
          (max_height / zscale - min_height / zscale) / 5
      } else {
        max_dim = max(dim(height_matrix))
        soliddepth = min_height / zscale - max_dim / 25
      }
    } else {
      if (soliddepth > min_height) {
        message(sprintf(
          "`soliddepth` (set to %f) must be less than or equal to heightmap minimum value (%f). Setting to min(heightmap)",
          soliddepth,
          min_height
        ))
        soliddepth = min_height / zscale
      } else {
        soliddepth = soliddepth / zscale
      }
    }
    if (solid) {
      min_height_shadow = min(c(min_height, soliddepth * zscale))
    } else {
      min_height_shadow = min_height
    }
    if (identical(shadowdepth, "auto")) {
      if (min_height_shadow != max_height) {
        if (solid) {
          shadowdepth = soliddepth -
            (max_height / zscale - min_height_shadow / zscale) / 5
        } else {
          shadowdepth = min_height_shadow /
            zscale -
            (max_height / zscale - min_height_shadow / zscale) / 5
        }
      } else {
        max_dim = max(dim(height_matrix))
        if (solid) {
          shadowdepth = soliddepth - max_dim / 25
        } else {
          shadowdepth = min_height - max_dim / 25
        }
      }
    } else {
      if (shadowdepth > min_height) {
        message(sprintf(
          "`shadowdepth` (set to %f) is greater to heightmap minimum value (%f). Shadow will appear to be intersecting 3D model.",
          shadowdepth,
          min_height
        ))
      } else {
        shadowdepth = shadowdepth / zscale
      }
    }
    shadowdepth
  }
  dot_args = list(...)
  if ("geographic_aspect" %in% names(dot_args)) {
    message(
      "plot_gg(): `geographic_aspect` is ignored; ggplot panel geometry is preserved."
    )
    dot_args$geographic_aspect = NULL
  }
  clone_plot_gg_object = function(x) {
    cloned = unserialize(serialize(x, NULL))
    if (inherits(x, "ggplot")) {
      # Preserve live external-pointer-backed inputs such as terra::SpatRaster
      # objects referenced from captured environments and layer data.
      cloned@plot_env = x@plot_env
      cloned@data = x@data
      cloned@mapping = x@mapping
      if (length(cloned@layers) == length(x@layers)) {
        for (i in seq_along(cloned@layers)) {
          cloned@layers[[i]]$data = x@layers[[i]]$data
          cloned@layers[[i]]$mapping = x@layers[[i]]$mapping
        }
      }
    }
    cloned
  }
  normalize_plot_gg_raytrace_mode = function(raytrace) {
    if (is.logical(raytrace) && length(raytrace) == 1 && !is.na(raytrace)) {
      return(if (isTRUE(raytrace)) "raytrace" else "none")
    }
    if (is.character(raytrace) && length(raytrace) == 1) {
      raytrace = tolower(raytrace[[1]])
      if (raytrace %in% c("raytrace", "ray", "ray_shade")) {
        return("raytrace")
      }
      if (raytrace %in% c("radiance", "radiance_shade")) {
        return("radiance")
      }
      if (raytrace %in% c("none", "off")) {
        return("none")
      }
    }
    stop(
      "`raytrace` must be TRUE/FALSE or one of c(\"raytrace\", \"radiance\", \"none\")."
    )
  }
  flatten_plot_gg_panel_data = function(height_matrix, panel_info) {
    if (is.null(panel_info) || !nrow(panel_info)) {
      return(height_matrix)
    }
    required_cols = c(
      "panel_xmin",
      "panel_xmax",
      "panel_ymin",
      "panel_ymax"
    )
    if (!all(required_cols %in% names(panel_info))) {
      return(height_matrix)
    }
    for (i in seq_len(nrow(panel_info))) {
      row_bounds = suppressWarnings(as.numeric(c(
        panel_info$panel_xmin[i],
        panel_info$panel_xmax[i]
      )))
      col_bounds = suppressWarnings(as.numeric(c(
        panel_info$panel_ymin[i],
        panel_info$panel_ymax[i]
      )))
      if (!all(is.finite(row_bounds)) || !all(is.finite(col_bounds))) {
        next
      }
      row_start = max(1L, floor(min(row_bounds)))
      row_end = min(nrow(height_matrix), ceiling(max(row_bounds)))
      col_start = max(1L, floor(min(col_bounds)))
      col_end = min(ncol(height_matrix), ceiling(max(col_bounds)))
      if (row_start > row_end || col_start > col_end) {
        next
      }
      rows = seq.int(row_start, row_end)
      cols = seq.int(col_start, col_end)
      height_matrix[rows, cols] = 0
    }
    height_matrix
  }
  if (is.null(radiance_args)) {
    radiance_args = list()
  }
  if (!is.list(radiance_args)) {
    stop("`radiance_args` must be a list.")
  }
  if (
    !is.logical(flat_substrate) ||
      length(flat_substrate) != 1 ||
      is.na(flat_substrate)
  ) {
    stop("`flat_substrate` must be TRUE or FALSE.", call. = FALSE)
  }
  raytrace_mode = normalize_plot_gg_raytrace_mode(raytrace)
  reset_scene_context(
    clear_scene_metadata = TRUE,
    clear_scene_cache = TRUE
  )
  heightmaptemp = tempfile(fileext = ".png")
  colormaptemp = tempfile(fileext = ".png")
  png_device = grDevices::png
  apply_manual_correction = FALSE
  guide_bar_bleed_target = match.arg(guide_bar_bleed_target)
  height_plot_source = if (is.null(ggobj_height)) {
    if (methods::is(ggobj, "list") && length(ggobj) == 2) {
      ggobj[[2]]
    } else {
      ggobj
    }
  } else {
    ggobj_height
  }
  if (requireNamespace("ragg", quietly = TRUE)) {
    png_device = function(...) ragg::agg_png(...)
  } else if (isTRUE(capabilities("cairo"))) {
    png_device = function(...) grDevices::png(..., type = "cairo")
  } else {
    apply_manual_correction = TRUE
  }
  if (is.null(ggobj_height)) {
    if (methods::is(ggobj, "list") && length(ggobj) == 2) {
      if (
        !inherits(ggobj[[1]], "ggplot") ||
          !inherits(ggobj[[2]], "ggplot")
      ) {
        stop(
          "When `ggobj` is a list, both elements must be ggplot objects.",
          call. = FALSE
        )
      }
      ggplotobj2 = clone_plot_gg_object(ggobj[[2]])
      color_gg = clone_plot_gg_object(ggobj[[1]])
    } else {
      if (!inherits(ggobj, "ggplot")) {
        stop(
          "`ggobj` must be a ggplot object or a length-2 list of ggplot objects.",
          call. = FALSE
        )
      }
      ggplotobj2 = clone_plot_gg_object(ggobj)
      color_gg = clone_plot_gg_object(ggobj)
    }
  } else {
    if (!inherits(ggobj, "ggplot")) {
      stop("`ggobj` must be a ggplot object.", call. = FALSE)
    }
    if (!inherits(ggobj_height, "ggplot")) {
      stop("`ggobj_height` must be a ggplot object.", call. = FALSE)
    }
    ggplotobj2 = clone_plot_gg_object(ggobj_height)
    color_gg = clone_plot_gg_object(ggobj)
  }
  color_gg_grob = ggplot2::ggplotGrob(color_gg)
  plot_background = get_plot_gg_grob_background(color_gg_grob)
  color_gg_grob = remove_plot_gg_grob_background_line(color_gg_grob)

  grob_name = function(grob) {
    if (!is.null(grob$name)) {
      return(grob$name)
    }
    return("")
  }
  set_grob_white = function(grob, alpha = 0, linewidth = 0) {
    if (is.null(grob)) {
      return(grob)
    }
    if (is.null(grob$gp)) {
      grob$gp = grid::gpar()
    }
    grob$gp$col = "white"
    grob$gp$alpha = alpha
    grob$gp$fill = "white"
    grob$gp$lwd = linewidth
    class(grob$gp) = "gpar"
    return(grob)
  }
  is_grob_container = function(grob) {
    if (is.null(grob)) {
      return(FALSE)
    }
    return(
      !is.null(grob[["grobs"]]) ||
        !is.null(grob[["children"]]) ||
        (length(grob) == 1 && inherits(grob[[1]], "gTree")) ||
        inherits(grob, c("gTree", "gtable"))
    )
  }
  recursive_whiten_grob = function(grob) {
    if (is.null(grob)) {
      return(grob)
    }
    if (!is.null(grob[["grobs"]])) {
      for (j in seq_len(length(grob$grobs))) {
        grob$grobs[[j]] = recursive_whiten_grob(grob$grobs[[j]])
      }
    }
    if (!is.null(grob[["children"]])) {
      for (j in seq_len(length(grob$children))) {
        grob$children[[j]] = recursive_whiten_grob(grob$children[[j]])
      }
    }
    if (length(grob) == 1 && inherits(grob[[1]], "gTree")) {
      grob[[1]] = recursive_whiten_grob(grob[[1]])
    }
    if (!is_grob_container(grob) && !inherits(grob, "zeroGrob")) {
      grob = set_grob_white(grob)
    }
    return(grob)
  }
  set_to_white_except_panel_data = function(grob) {
    if (is.null(grob)) {
      return(grob)
    }
    if (!is.null(grob[["grobs"]])) {
      for (j in seq_len(length(grob$grobs))) {
        grob$grobs[[j]] = set_to_white_except_panel_data(grob$grobs[[j]])
      }
    }
    if (!is.null(grob[["children"]])) {
      for (j in seq_len(length(grob$children))) {
        grob$children[[j]] = set_to_white_except_panel_data(grob$children[[j]])
      }
    }
    if (length(grob) == 1 && inherits(grob[[1]], "gTree")) {
      grob[[1]] = set_to_white_except_panel_data(grob[[1]])
    }
    if (is_grob_container(grob) || inherits(grob, "zeroGrob")) {
      return(grob)
    }
    name = grob_name(grob)
    if (
      !(length(grep("geom", x = name)) > 0) &&
        !(length(grep("pathgrob", x = name)) > 0)
    ) {
      grob = set_grob_white(grob)
    }
    return(grob)
  }
  label_to_string = function(x) {
    if (is.null(x) || inherits(x, "waiver")) {
      return(NA_character_)
    }
    return(paste(as.character(x), collapse = ""))
  }
  normalize_guide_text = function(x) {
    if (length(x) == 0 || is.na(x) || is.null(x)) {
      return(NA_character_)
    }
    return(gsub("[[:space:]]+", " ", trimws(as.character(x))))
  }
  collect_text_grobs = function(grob) {
    text_labels = character(0)
    text_names = character(0)
    recurse = function(x) {
      if (is.null(x)) {
        return(invisible(NULL))
      }
      if (
        all(inherits(x, c("text", "grob"), which = TRUE) > 0) &&
          !is.null(x$label)
      ) {
        text_labels <<- c(text_labels, label_to_string(x$label))
        text_names <<- c(text_names, grob_name(x))
      }
      if (!is.null(x[["grobs"]])) {
        for (k in seq_len(length(x$grobs))) {
          recurse(x$grobs[[k]])
        }
      }
      if (!is.null(x[["children"]])) {
        for (k in seq_len(length(x$children))) {
          recurse(x$children[[k]])
        }
      }
      if (length(x) == 1 && inherits(x[[1]], "gTree")) {
        recurse(x[[1]])
      }
      invisible(NULL)
    }
    recurse(grob)
    return(list(labels = text_labels, names = text_names))
  }
  extract_guide_title = function(grob) {
    text_info = collect_text_grobs(grob)
    if (!length(text_info$labels)) {
      return(NA_character_)
    }
    title_index = which(grepl("title", text_info$names, ignore.case = TRUE))
    if (length(title_index)) {
      return(normalize_guide_text(text_info$labels[[title_index[1]]]))
    }
    return(normalize_guide_text(text_info$labels[[1]]))
  }
  guide_layout_names = function(grob) {
    if (!inherits(grob, "gtable") || is.null(grob$layout$name)) {
      return(character(0))
    }
    return(tolower(grob$layout$name))
  }
  is_candidate_guide = function(grob) {
    layout_names = guide_layout_names(grob)
    if (!length(layout_names)) {
      return(FALSE)
    }
    return(
      any(grepl("(^|-)bar($|-)", layout_names)) ||
        any(grepl("^key", layout_names)) ||
        any(grepl("label", layout_names)) ||
        any(grepl("title", layout_names))
    )
  }
  get_guide_entries = function(guide_box) {
    if (!inherits(guide_box, "gtable") || is.null(guide_box$grobs)) {
      return(list(indices = integer(0), grobs = list()))
    }
    guide_indices = which(vapply(
      guide_box$grobs,
      is_candidate_guide,
      logical(1)
    ))
    return(list(
      indices = guide_indices,
      grobs = guide_box$grobs[guide_indices]
    ))
  }
  match_guide_entry = function(
    guides,
    title = NA_character_,
    prefer_bar = FALSE
  ) {
    if (!length(guides$indices)) {
      return(NA_integer_)
    }
    normalized_title = normalize_guide_text(title)
    guide_titles = vapply(guides$grobs, extract_guide_title, character(1))
    if (!is.na(normalized_title) && nzchar(normalized_title)) {
      matched = which(guide_titles == normalized_title)
      if (length(matched)) {
        return(matched[1])
      }
    }
    if (prefer_bar) {
      has_bar = vapply(
        guides$grobs,
        function(x) any(grepl("(^|-)bar($|-)", guide_layout_names(x))),
        logical(1)
      )
      if (any(has_bar)) {
        return(which(has_bar)[1])
      }
    }
    has_key = vapply(
      guides$grobs,
      function(x) any(grepl("^key", guide_layout_names(x))),
      logical(1)
    )
    if (any(has_key)) {
      return(which(has_key)[1])
    }
    return(1L)
  }
  translate_grob_copy = function(
    grob,
    x_off = grid::unit(0, "in"),
    y_off = grid::unit(0, "in")
  ) {
    grid::grobTree(
      grob,
      vp = grid::viewport(
        x = grid::unit(0.5, "npc") + x_off,
        y = grid::unit(0.5, "npc") + y_off,
        width = grid::unit(1, "npc"),
        height = grid::unit(1, "npc"),
        just = c("center", "center"),
        clip = "off"
      )
    )
  }
  is_whiteish_color = function(x, tol = 8L) {
    if (is.null(x) || !length(x)) {
      return(logical(0))
    }
    vapply(
      x,
      function(val) {
        if (is.na(val) || identical(val, "transparent")) {
          return(TRUE)
        }
        rgb = tryCatch(
          grDevices::col2rgb(val, alpha = TRUE),
          error = function(e) NULL
        )
        if (is.null(rgb)) {
          return(FALSE)
        }
        if (rgb[4, 1] == 0) {
          return(TRUE)
        }
        all(rgb[1:3, 1] >= (255 - tol))
      },
      logical(1)
    )
  }
  leaf_grob_has_colored_content = function(grob) {
    if (is.null(grob) || inherits(grob, "zeroGrob")) {
      return(FALSE)
    }
    if (inherits(grob, "rastergrob")) {
      return(TRUE)
    }
    gp = grob$gp
    if (is.null(gp)) {
      return(FALSE)
    }
    cols = c(gp$col, gp$fill)
    cols = cols[!is.na(cols)]
    if (!length(cols)) {
      return(FALSE)
    }
    return(any(!is_whiteish_color(cols)))
  }
  bleed_leaf_grob = function(
    grob,
    delta,
    orientation = c("vertical", "horizontal")
  ) {
    orientation = match.arg(orientation)
    if (orientation == "vertical") {
      return(grid::grobTree(
        grob,
        translate_grob_copy(grob, x_off = -delta),
        translate_grob_copy(grob, x_off = delta)
      ))
    }
    grid::grobTree(
      grob,
      translate_grob_copy(grob, y_off = -delta),
      translate_grob_copy(grob, y_off = delta)
    )
  }
  infer_guide_bar_orientation = function(grob) {
    recurse = function(x) {
      if (is.null(x)) {
        return(NULL)
      }
      if (inherits(x, "gtable") && !is.null(x$layout$name)) {
        idx = which(grepl("(^|-)bar($|-)", tolower(x$layout$name)))
        if (length(idx)) {
          idx = idx[1]
          row_span = x$layout$b[idx] - x$layout$t[idx] + 1
          col_span = x$layout$r[idx] - x$layout$l[idx] + 1
          if (row_span > col_span) {
            return("vertical")
          }
          if (col_span > row_span) {
            return("horizontal")
          }
        }
      }
      if (!is.null(x[["grobs"]])) {
        for (k in seq_len(length(x$grobs))) {
          ans = recurse(x$grobs[[k]])
          if (!is.null(ans)) {
            return(ans)
          }
        }
      }
      if (!is.null(x[["children"]])) {
        for (k in seq_len(length(x$children))) {
          ans = recurse(x$children[[k]])
          if (!is.null(ans)) {
            return(ans)
          }
        }
      }
      if (length(x) == 1 && inherits(x[[1]], "gTree")) {
        return(recurse(x[[1]]))
      }
      return(NULL)
    }
    orientation = recurse(grob)
    if (is.null(orientation)) {
      orientation = "vertical"
    }
    return(orientation)
  }
  widen_guide_bar_grob = function(
    grob,
    bleed_px = 3L,
    orientation = c("vertical", "horizontal")
  ) {
    orientation = match.arg(orientation)
    bleed_in = bleed_px / 300
    if (!is.finite(bleed_in) || bleed_in <= 0) {
      return(grob)
    }
    delta = grid::unit(bleed_in, "in")
    recurse = function(x) {
      if (is.null(x)) {
        return(x)
      }
      if (!is.null(x[["grobs"]])) {
        for (k in seq_len(length(x$grobs))) {
          x$grobs[[k]] = recurse(x$grobs[[k]])
        }
        return(x)
      }
      if (!is.null(x[["children"]])) {
        for (k in seq_len(length(x$children))) {
          x$children[[k]] = recurse(x$children[[k]])
        }
        return(x)
      }
      if (length(x) == 1 && inherits(x[[1]], "gTree")) {
        x[[1]] = recurse(x[[1]])
        return(x)
      }
      if (leaf_grob_has_colored_content(x)) {
        return(bleed_leaf_grob(x, delta = delta, orientation = orientation))
      }
      return(x)
    }
    recurse(grob)
  }
  prepare_target_guide = function(
    grob,
    bleed_px = 0L,
    render_mode = c("height", "texture")
  ) {
    render_mode = match.arg(render_mode)
    if (!inherits(grob, "gtable") || is.null(grob$layout$name)) {
      if (render_mode == "height") {
        return(set_to_white_except_panel_data(grob))
      }
      return(grob)
    }
    out = grob
    layout_names = tolower(out$layout$name)
    bar_orientation = infer_guide_bar_orientation(out)
    for (j in seq_len(length(out$grobs))) {
      child_name = layout_names[j]
      if (grepl("(^|-)bar($|-)", child_name)) {
        if (isTRUE(bleed_px > 0)) {
          out$grobs[[j]] = widen_guide_bar_grob(
            out$grobs[[j]],
            bleed_px = bleed_px,
            orientation = bar_orientation
          )
        }
        next
      }
      if (render_mode == "texture") {
        next
      }
      if (grepl("^key", child_name)) {
        out$grobs[[j]] = set_to_white_except_panel_data(out$grobs[[j]])
      } else {
        out$grobs[[j]] = recursive_whiten_grob(out$grobs[[j]])
      }
    }
    return(out)
  }
  compose_guide_box = function(
    target_box,
    source_box,
    guide_title = NA_character_,
    prefer_bar = FALSE,
    bleed_px = 0L
  ) {
    if (!inherits(target_box, "gtable")) {
      return(recursive_whiten_grob(target_box))
    }
    out = target_box
    for (j in seq_len(length(out$grobs))) {
      out$grobs[[j]] = recursive_whiten_grob(out$grobs[[j]])
    }
    if (!inherits(source_box, "gtable")) {
      return(out)
    }
    target_entries = get_guide_entries(target_box)
    source_entries = get_guide_entries(source_box)
    if (!length(target_entries$indices) || !length(source_entries$indices)) {
      return(out)
    }
    target_match = match_guide_entry(
      target_entries,
      title = guide_title,
      prefer_bar = prefer_bar
    )
    source_match = match_guide_entry(
      source_entries,
      title = guide_title,
      prefer_bar = prefer_bar
    )
    if (is.na(target_match) || is.na(source_match)) {
      return(out)
    }
    out$grobs[[target_entries$indices[[target_match]]]] = prepare_target_guide(
      source_entries$grobs[[source_match]],
      bleed_px = bleed_px,
      render_mode = "height"
    )
    return(out)
  }
  apply_guide_bar_bleed_box = function(
    target_box,
    guide_title = NA_character_,
    prefer_bar = FALSE,
    bleed_px = 0L
  ) {
    if (!inherits(target_box, "gtable") || !isTRUE(bleed_px > 0)) {
      return(target_box)
    }
    entries = get_guide_entries(target_box)
    if (!length(entries$indices)) {
      return(target_box)
    }
    target_match = match_guide_entry(
      entries,
      title = guide_title,
      prefer_bar = prefer_bar
    )
    if (is.na(target_match)) {
      return(target_box)
    }
    out = target_box
    out$grobs[[entries$indices[[target_match]]]] = prepare_target_guide(
      entries$grobs[[target_match]],
      bleed_px = bleed_px,
      render_mode = "texture"
    )
    return(out)
  }
  apply_texture_grob = function(
    color_grob,
    guide_title = NA_character_,
    prefer_bar = FALSE,
    bleed_px = 0L
  ) {
    out = color_grob
    if (
      !isTRUE(bleed_px > 0) ||
        !inherits(out, "gtable") ||
        is.null(out$layout$name)
    ) {
      return(out)
    }
    guide_box_indices = which(grepl("^guide-box", out$layout$name))
    if (!length(guide_box_indices)) {
      return(out)
    }
    for (j in guide_box_indices) {
      out$grobs[[j]] = apply_guide_bar_bleed_box(
        target_box = out$grobs[[j]],
        guide_title = guide_title,
        prefer_bar = prefer_bar,
        bleed_px = bleed_px
      )
    }
    return(out)
  }
  compose_height_grob = function(
    color_grob,
    height_grob,
    guide_title = NA_character_,
    prefer_bar = FALSE,
    bleed_px = 0L
  ) {
    out = color_grob
    layout_names = out$layout$name
    panel_indices = which(grepl("^panel($|-)", layout_names))
    height_panel_indices = which(grepl(
      "^panel($|-)",
      height_grob$layout$name
    ))
    n_panels = min(length(panel_indices), length(height_panel_indices))
    if (n_panels > 0) {
      for (j in seq_len(n_panels)) {
        out$grobs[[panel_indices[j]]] = set_to_white_except_panel_data(
          height_grob$grobs[[height_panel_indices[j]]]
        )
      }
    }
    guide_box_indices = which(grepl("^guide-box", layout_names))
    for (j in seq_len(length(out$grobs))) {
      if (j %in% panel_indices) {
        next
      }
      if (j %in% guide_box_indices) {
        source_match = which(height_grob$layout$name == layout_names[j])
        if (length(source_match)) {
          out$grobs[[j]] = compose_guide_box(
            target_box = out$grobs[[j]],
            source_box = height_grob$grobs[[source_match[1]]],
            guide_title = guide_title,
            prefer_bar = prefer_bar,
            bleed_px = bleed_px
          )
        } else {
          out$grobs[[j]] = recursive_whiten_grob(out$grobs[[j]])
        }
      } else {
        out$grobs[[j]] = recursive_whiten_grob(out$grobs[[j]])
      }
    }
    return(out)
  }
  emboss_gg_text = function(grob, emboss) {
    if (!is.null(grob[["grobs"]])) {
      for (j in seq_len(length(grob$grobs))) {
        grob$grobs[[j]] = emboss_gg_text(grob$grobs[[j]], emboss)
      }
    } else if (!is.null(grob[["children"]])) {
      for (j in seq_len(length(grob$children))) {
        grob$children[[j]] = emboss_gg_text(grob$children[[j]], emboss)
      }
    } else if (all(inherits(grob, c("text", "grob"), which = TRUE) > 0)) {
      emboss = ceiling(max(c(min(c(emboss, 1)), 0)) * 100)
      colval = ifelse(emboss != 100, sprintf("grey%d", emboss), "white")
      grob$gp$col = colval
      grob$gp$alpha = 1
      grob$gp$fill = colval
      class(grob$gp) = "gpar"
    }
    return(grob)
  }
  emboss_gg_grid = function(grob, emboss) {
    if (!is.null(grob[["grobs"]])) {
      for (j in seq_len(length(grob$grobs))) {
        grob$grobs[[j]] = emboss_gg_grid(grob$grobs[[j]], emboss)
      }
    } else if (!is.null(grob[["children"]])) {
      for (j in seq_len(length(grob$children))) {
        grob$children[[j]] = emboss_gg_grid(grob$children[[j]], emboss)
      }
    } else if (
      (all(inherits(grob, c("polyline", "grob"), which = TRUE) > 0) &&
        length(grep("panel.grid", grob$name)) > 0) ||
        (all(inherits(grob, c("lines", "grob"), which = TRUE) > 0) &&
          (length(grep("GRID.lines", grob$name)) > 0))
    ) {
      if (length(grep("GRID.lines", grob$name)) > 0) {
        emboss = emboss[1]
      }
      if (length(grep("panel.grid.major", grob$name)) > 0) {
        emboss = emboss[1]
      }
      if (length(grep("panel.grid.minor", grob$name)) > 0) {
        emboss = emboss[2]
      }
      emboss = ceiling(max(c(min(c(emboss, 1)), 0)) * 100)
      colval = ifelse(emboss != 100, sprintf("grey%d", emboss), "white")
      grob$gp$col = colval
      grob$gp$alpha = 1
      grob$gp$fill = colval
      grob$gp$lwd = 1
      class(grob$gp) = "gpar"
    }
    return(grob)
  }
  #Determine if auto fill or color aes to be mapped to 3D
  height_aes_explicit = !is.null(height_aes)
  isfill = FALSE
  iscolor = FALSE
  if (is.null(height_aes)) {
    for (i in seq_len(length(ggplotobj2$layers))) {
      if ("fill" %in% names(ggplotobj2$layers[[i]]$mapping)) {
        isfill = TRUE
      }
      if (
        any(c("color", "colour") %in% names(ggplotobj2$layers[[i]]$mapping))
      ) {
        iscolor = TRUE
      }
    }
    if (!iscolor && !isfill) {
      if ("fill" %in% names(ggplotobj2$mapping)) {
        isfill = TRUE
      }
      if (any(c("color", "colour") %in% names(ggplotobj2$mapping))) {
        iscolor = TRUE
      }
    }
    if (isfill && !iscolor) {
      height_aes = "fill"
    } else if (!isfill && iscolor) {
      height_aes = "colour"
    } else if (isfill && iscolor) {
      height_aes = "fill"
    } else {
      height_aes = "fill"
    }
  }
  if (height_aes == "color") {
    height_aes = "colour"
  }
  height_color_build_obj = ggplot2::ggplot_build(
    clone_plot_gg_object(height_plot_source)
  )
  height_color_scale = height_color_build_obj$plot$scales$get_scales(
    height_aes
  )
  if (is.numeric(offset_edges)) {
    polygon_offset_value = offset_edges
    offset_edges = TRUE
  } else {
    polygon_offset_value = 0.5
  }
  polygon_offset_geoms = c("GeomPolygon", "GeomSf", "GeomHex", "GeomTile")
  other_height_type = ifelse(height_aes == "colour", "fill", "colour")

  black_white_pal = function(x) {
    grDevices::colorRampPalette(c("white", "black"))(255)[x * 254 + 1]
  }
  white_white_pal = function(x) {
    grDevices::colorRampPalette(c("white", "white"))(255)[x * 254 + 1]
  }
  ifelsefxn = function(entry) {
    if (!is.null(entry)) {
      return(entry)
    }
  }

  #Remove legend.ticks, if they exist
  ggplotobj2 = ggplotobj2 +
    ggplot2::theme(legend.ticks = ggplot2::element_blank())
  #Shift all continuous palettes of height_aes to black/white, and set all discrete key colors to white.
  if (ggplotobj2$scales$n() != 0) {
    anyfound = FALSE
    #Check to see if same guide being used for both color and fill aesthetics
    if (
      ggplotobj2$scales$has_scale("colour") &&
        ggplotobj2$scales$has_scale("fill")
    ) {
      fillscale = ggplotobj2$scales$get_scales("fill")
      colorscale = ggplotobj2$scales$get_scales("colour")
      same_limits = FALSE
      same_breaks = FALSE
      same_labels = FALSE
      same_calls = FALSE
      if ((!is.null(fillscale$limits) && !is.null(colorscale$limits))) {
        if (fillscale$limits == colorscale$limits) {
          same_limits = TRUE
        }
      } else if (is.null(fillscale$limits) && is.null(colorscale$limits)) {
        same_limits = TRUE
      }
      if ((!is.null(fillscale$breaks) && !is.null(colorscale$breaks))) {
        if (all(fillscale$breaks == colorscale$breaks)) {
          same_breaks = TRUE
        }
      } else if (is.null(fillscale$breaks) && is.null(colorscale$breaks)) {
        same_breaks = TRUE
      }
      if (
        !inherits(fillscale$labels, "waiver") &&
          !inherits(colorscale$labels, "waiver")
      ) {
        if (all(fillscale$labels == colorscale$labels)) {
          same_labels = TRUE
        }
      } else if (
        inherits(fillscale$labels, "waiver") &&
          inherits(colorscale$labels, "waiver")
      ) {
        same_labels = TRUE
      }
      if (fillscale$call == colorscale$call) {
        same_calls = TRUE
      }
      if (same_limits && same_breaks && same_labels && same_calls) {
        if (height_aes == "fill") {
          ggplotobj2 = ggplotobj2 + ggplot2::guides(color = "none")
        } else {
          ggplotobj2 = ggplotobj2 + ggplot2::guides(fill = "none")
        }
      }
    }
    #Now check for scales and change to the b/w palette, but preserve guide traits.
    for (i in seq_len(ggplotobj2$scales$n())) {
      if (height_aes %in% ggplotobj2$scales$scales[[i]]$aesthetics) {
        ggplotobj2$scales$scales[[i]]$palette = black_white_pal
        ggplotobj2$scales$scales[[i]]$na.value = "white"
        has_guide = !any(inherits(ggplotobj2$scales$scales[[i]]$guide, "guide"))
        if (any(inherits(ggplotobj2$scales$scales[[i]]$guide, "logical"))) {
          has_guide = ggplotobj2$scales$scales[[i]]$guide
        }
        if (has_guide) {
          if (height_aes == "fill") {
            if (is.null(ggplotobj2$guides$fill)) {
              ggplotobj2 = ggplotobj2 +
                ggplot2::guides(
                  fill = ggplot2::guide_colourbar(
                    # legend.ticks = ggplot2::element_blank(),
                    nbin = 1000,
                    order = i
                  )
                )
            } else {
              if (any(ggplotobj2$guides$fill != "none")) {
                copyguide = ggplotobj2$guides$fill
                copyguide$frame.linewidth = 0
                # copyguide$legend.ticks = ggplot2::element_blank()
                copyguide$nbin = 1000
                ggplotobj2 = ggplotobj2 +
                  ggplot2::guides(
                    fill = ggplot2::guide_colourbar(
                      # legend.ticks = ggplot2::element_blank(),
                      nbin = 1000
                    )
                  )
                ggplotobj2$guides$fill = copyguide
              }
            }
            for (j in seq_len(length(ggplotobj2$layers))) {
              if ("colour" %in% names(ggplotobj2$layers[[j]]$mapping)) {
                ggplotobj2$layers[[j]]$geom$draw_key = drawkeyfunction_points
              }
            }
          } else {
            if (is.null(ggplotobj2$guides$colour)) {
              ggplotobj2 = ggplotobj2 +
                ggplot2::guides(
                  colour = ggplot2::guide_colourbar(
                    # legend.ticks = ggplot2::element_blank(),
                    nbin = 1000,
                    order = i
                  )
                )
            } else {
              if (any(ggplotobj2$guides$colour != "none")) {
                copyguide = ggplotobj2$guides$colour
                copyguide$frame.linewidth = 0
                # copyguide$legend.ticks = ggplot2::element_blank()
                copyguide$nbin = 1000
                ggplotobj2 = ggplotobj2 +
                  ggplot2::guides(
                    colour = ggplot2::guide_colourbar(
                      # legend.ticks = ggplot2::element_blank(),
                      nbin = 1000
                    )
                  )
                ggplotobj2$guides$colour = copyguide
              }
            }
          }
        }
        anyfound = TRUE
      } else if (
        other_height_type %in% ggplotobj2$scales$scales[[i]]$aesthetics
      ) {
        #change guides for other height_aes to be the all white palette
        ggplotobj2$scales$scales[[i]]$palette = white_white_pal
        ggplotobj2$scales$scales[[i]]$na.value = "white"
      }
    }
    #If no scales found, just add one to the ggplot object.
    if (!anyfound) {
      if (height_aes == "colour") {
        ggplotobj2 = ggplotobj2 +
          ggplot2::scale_color_gradientn(
            colours = grDevices::colorRampPalette(c("white", "black"))(256),
            na.value = "white"
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(
              # legend.ticks = ggplot2::element_blank(),
              nbin = 1000
            )
          )
      }
      if (height_aes == "fill") {
        ggplotobj2 = ggplotobj2 +
          ggplot2::scale_fill_gradientn(
            colours = grDevices::colorRampPalette(c("white", "black"))(256),
            na.value = "white"
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_colourbar(
              # legend.ticks = ggplot2::element_blank(),
              nbin = 1000
            )
          )
      }
    }
  } else {
    #If no scales found, just add one to the ggplot object.
    if (ggplotobj2$scales$n() == 0) {
      if (height_aes == "fill") {
        ggplotobj2 = ggplotobj2 +
          ggplot2::scale_fill_gradientn(
            colours = grDevices::colorRampPalette(c("white", "black"))(256),
            na.value = "white"
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_colourbar(
              # legend.ticks = ggplot2::element_blank(),
              nbin = 1000
            )
          )
      } else {
        ggplotobj2 = ggplotobj2 +
          ggplot2::scale_color_gradientn(
            colours = grDevices::colorRampPalette(c("white", "black"))(256),
            na.value = "white"
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(
              # legend.ticks = ggplot2::element_blank(),
              nbin = 1000
            )
          )
      }
    } else {
      if (height_aes == "fill") {
        ggplotobj2 = ggplotobj2 +
          ggplot2::scale_fill_gradientn(
            colours = grDevices::colorRampPalette(c("white", "black"))(256),
            na.value = "white"
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_colourbar(
              # legend.ticks = ggplot2::element_blank(),
              nbin = 1000
            )
          )
      } else {
        ggplotobj2 = ggplotobj2 +
          ggplot2::scale_color_gradientn(
            colours = grDevices::colorRampPalette(c("white", "black"))(256),
            na.value = "white"
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_colourbar(
              # legend.ticks = ggplot2::element_blank(),
              nbin = 1000
            )
          )
      }
    }
  }
  if (height_aes == "fill") {
    for (layer in seq_along(ggplotobj2$layers)) {
      if (
        "colour" %in%
          names(ggplotobj2$layers[[layer]]$mapping) ||
          0 == length(names(ggplotobj2$layers[[layer]]$mapping))
      ) {
        ggplotobj2$layers[[layer]]$aes_params$colour = "white"
      }
      if ("fill" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        ggplotobj2$layers[[layer]]$aes_params$size = NA
        if (
          any(as.logical(inherits(
            ggplotobj2$layers[[layer]]$geom,
            polygon_offset_geoms
          ))) &&
            offset_edges
        ) {
          ggplotobj2$layers[[layer]]$aes_params$size = polygon_offset_value
          ggplotobj2$layers[[layer]]$aes_params$colour = "white"
        }
      }
      if ("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        shapedata = ggplot2::layer_data(ggplotobj2)
        numbershapes = length(unique(shapedata$shape))
        if (numbershapes > 3) {
          warning("Non-solid shapes will not be projected to 3D.")
        }
        ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
      }
      if ("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
      }
      if ("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        for (j in seq_len(length(ggplotobj2$layers))) {
          geom_defaults = ggplot2::get_geom_defaults(ggplotobj2$layers[[j]])
          if ("stroke" %in% names(geom_defaults)) {
            ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
          }
        }
        ggplotobj2 = suppressMessages({
          ggplotobj2 + ggplot2::scale_alpha_continuous(range = c(1, 1))
        })
      }
      if ("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
      }
    }
  } else {
    for (layer in seq_len(length(ggplotobj2$layers))) {
      if (
        "fill" %in%
          names(ggplotobj2$layers[[layer]]$mapping) ||
          0 == length(names(ggplotobj2$layers[[layer]]$mapping))
      ) {
        ggplotobj2$layers[[layer]]$aes_params$fill = "white"
      }
      if ("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        shapedata = ggplot2::layer_data(ggplotobj2)
        numbershapes = length(unique(shapedata$shape))
        if (numbershapes > 3) {
          warning("Non-solid shapes will not be projected to 3D.")
        }
        ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
      }
      if ("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
      }
      if ("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        for (j in seq_len(length(ggplotobj2$layers))) {
          geom_defaults = ggplot2::get_geom_defaults(ggplotobj2$layers[[j]])
          if ("stroke" %in% names(geom_defaults)) {
            ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
          }
        }
        ggplotobj2 = suppressMessages({
          ggplotobj2 + ggplot2::scale_alpha_continuous(range = c(1, 1))
        })
      }
      if ("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
        ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
      }
    }
  }
  #Offset edges for polygons/Perform point contraction
  if (height_aes == "fill") {
    if (length(ggplotobj2$layers) > 0) {
      for (i in seq_along(ggplotobj2$layers)) {
        ggplotobj2$layers[[i]]$aes_params$size = NA
        if (
          any(as.logical(inherits(
            ggplotobj2$layers[[i]]$geom,
            polygon_offset_geoms
          ))) &&
            offset_edges
        ) {
          ggplotobj2$layers[[i]]$aes_params$size = polygon_offset_value
          ggplotobj2$layers[[i]]$aes_params$colour = "white"
        }
      }
    }
  } else {
    if (length(ggplotobj2$layers) > 0) {
      for (i in seq_along(ggplotobj2$layers)) {
        ggplotobj2$layers[[i]]$aes_params$fill = "white"
        if (inherits(ggplotobj2$layers[[i]]$geom, "GeomContour")) {
          ggplotobj2$layers[[i]]$aes_params$alpha = 0
        }
      }
      if (pointcontract != 1) {
        for (i in seq_along(ggplotobj2$layers)) {
          if (!is.null(ggplotobj2$layers[[i]]$aes_params$size)) {
            ggplotobj2$layers[[i]]$aes_params$size = ggplotobj2$layers[[
              i
            ]]$aes_params$size *
              pointcontract
          } else {
            geom_defaults = ggplot2::get_geom_defaults(ggplotobj2$layers[[i]])
            if (!is.null(geom_defaults$size)) {
              ggplotobj2$layers[[i]]$aes_params$size = geom_defaults$size *
                pointcontract
            }
          }
        }
      }
    }
  }

  ggplot_build_obj = ggplot2::ggplot_build(ggplotobj2)
  height_guide_title = c(
    label_to_string(color_gg$labels[[height_aes]]),
    label_to_string(color_gg$labels[[ifelse(
      height_aes == "colour",
      "color",
      height_aes
    )]]),
    label_to_string(ggplotobj2$labels[[height_aes]]),
    label_to_string(ggplotobj2$labels[[ifelse(
      height_aes == "colour",
      "color",
      height_aes
    )]]),
    label_to_string(ggplot_build_obj$plot$labels[[height_aes]]),
    label_to_string(ggplot_build_obj$plot$labels[[ifelse(
      height_aes == "colour",
      "color",
      height_aes
    )]]),
    label_to_string(ggplotobj2$scales$get_scales(height_aes)$name)
  )
  height_guide_title = normalize_guide_text(height_guide_title[
    !is.na(height_guide_title)
  ][1])
  height_scale = ggplotobj2$scales$get_scales(height_aes)
  height_guide_prefers_bar = FALSE
  if (!is.null(height_scale)) {
    height_guide_prefers_bar =
      methods::is(height_scale, "ScaleContinuous") ||
      methods::is(height_scale, "ScaleBinned")
  }
  if (
    guide_bar_bleed_target %in%
      c("texture") &&
      isTRUE(guide_bar_bleed_px > 0)
  ) {
    color_gg_grob = apply_texture_grob(
      color_grob = color_gg_grob,
      guide_title = height_guide_title,
      prefer_bar = height_guide_prefers_bar,
      bleed_px = guide_bar_bleed_px
    )
  }
  old_dev = grDevices::dev.cur()
  png_device(
    filename = colormaptemp,
    width = width,
    height = height,
    units = "in",
    res = 300,
    bg = plot_background
  )
  grid::grid.draw(color_gg_grob)
  grDevices::dev.off()
  if (old_dev > 1) {
    grDevices::dev.set(old_dev)
  }
  plot_gg_transform_info = build_plot_gg_transform_info(
    ggplot_build_obj,
    height_scale = ggplot_build_obj$plot$scales$get_scales(height_aes),
    height_color_scale = height_color_scale,
    height_aes = height_aes,
    height_label = height_guide_title,
    height_is_mapped = if (isTRUE(height_aes_explicit)) {
      height_aes %in% c("fill", "colour")
    } else {
      isfill || iscolor
    },
    height_inverted = invert,
    height_use_data_scale = plot_gg_has_spatraster_height_source(
      height_plot_source
    )
  )
  resolved_height_zscale = resolve_plot_gg_height_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    scale = scale,
    scale_missing = missing(scale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    height_plot_source = height_plot_source,
    caller = "plot_gg"
  )
  height_gg_grob = ggplot2::ggplotGrob(ggplotobj2)
  ggplotobj2 = compose_height_grob(
    color_grob = color_gg_grob,
    height_grob = height_gg_grob,
    guide_title = height_guide_title,
    prefer_bar = height_guide_prefers_bar,
    bleed_px = ifelse(
      guide_bar_bleed_target %in% c("height"),
      guide_bar_bleed_px,
      0L
    )
  )
  if (emboss_text > 0) {
    emboss_text = 1 - emboss_text
    ggplotobj2 = emboss_gg_text(ggplotobj2, emboss_text)
  }
  if (emboss_grid > 0) {
    if (length(emboss_grid) == 1) {
      emboss_grid = c(emboss_grid, emboss_grid / 2)
    }
    emboss_grid = 1 - emboss_grid
    ggplotobj2 = emboss_gg_grid(ggplotobj2, emboss_grid)
  }
  old_dev = grDevices::dev.cur()
  png_device(
    filename = heightmaptemp,
    width = width,
    height = height,
    units = "in",
    res = 300
  )
  grid::grid.draw(ggplotobj2)
  plot_gg_panel_info = capture_plot_gg_panel_info(
    ggplotobj2,
    ggplot_build_obj,
    original_width_px = width * 300,
    original_height_px = height * 300
  )
  grDevices::dev.off()
  if (old_dev > 1) {
    grDevices::dev.set(old_dev)
  }
  height_resize_factor = 1
  if (!is.null(reduce_size)) {
    if (!(length(find.package("magick", quiet = TRUE)) > 0)) {
      stop("magick package required to use argument reduce_size")
    } else {
      if (length(reduce_size) == 1 && reduce_size < 1) {
        height_resize_factor = reduce_size
        image_info = magick::image_read(heightmaptemp) |>
          magick::image_info()
        magick::image_read(heightmaptemp) |>
          magick::image_resize(paste0(
            image_info$width * reduce_size,
            "x",
            image_info$height * reduce_size
          )) |>
          magick::image_write(heightmaptemp)
      } else if (length(reduce_size) == 2 && all(reduce_size < 1)) {
        height_resize_factor = reduce_size[1]
        image_info = magick::image_read(heightmaptemp) |>
          magick::image_info()
        magick::image_read(heightmaptemp) |>
          magick::image_resize(paste0(
            image_info$width * reduce_size[1],
            "x",
            image_info$height * reduce_size[1]
          )) |>
          magick::image_write(heightmaptemp)
        magick::image_read(colormaptemp) |>
          magick::image_resize(paste0(
            image_info$width * reduce_size[2],
            "x",
            image_info$height * reduce_size[2]
          )) |>
          magick::image_write(colormaptemp)
      }
    }
  }
  mapcolor = rayimage::ray_read_image(colormaptemp)
  mapheight = rayimage::ray_read_image(
    heightmaptemp,
    source_linear = TRUE
  )[,, 1]
  plot_gg_panel_info = finalize_plot_gg_panel_info(
    plot_gg_panel_info,
    original_width_px = width * 300,
    original_height_px = height * 300,
    scene_width_px = ncol(mapheight),
    scene_height_px = nrow(mapheight)
  )
  cache_plot_gg_panel_info(plot_gg_panel_info)
  cache_plot_gg_transform_info(plot_gg_transform_info)

  if (apply_manual_correction) {
    gamma_ratio = monitor_gamma / 2.2
    mapheight = mapheight^gamma_ratio
  }
  if (invert) {
    mapheight = 1 - mapheight
  }
  height_matrix = 1 - t(mapheight)
  height_matrix = restore_plot_gg_height_matrix_data_scale(
    height_matrix,
    transform_info = plot_gg_transform_info
  )
  if (isTRUE(flat_substrate)) {
    height_matrix = flatten_plot_gg_panel_data(
      height_matrix = height_matrix,
      panel_info = plot_gg_panel_info
    )
  }
  if (identical(resolved_height_zscale$source, "auto")) {
    rendered_auto_zscale = resolve_plot_gg_rendered_zscale(
      panel_info = plot_gg_panel_info,
      height_matrix = height_matrix,
      default = NA_real_
    )
    if (is.finite(rendered_auto_zscale) && rendered_auto_zscale > 0) {
      resolved_height_zscale$base_zscale = rendered_auto_zscale
      resolved_height_zscale$auto_zscale = rendered_auto_zscale
    } else {
      resolved_height_zscale$base_zscale =
        resolved_height_zscale$base_zscale / height_resize_factor
    }
  } else {
    resolved_height_zscale$base_zscale =
      resolved_height_zscale$base_zscale / height_resize_factor
  }
  zscale = apply_vertical_exaggeration(
    zscale = resolved_height_zscale$base_zscale,
    vertical_exaggeration = resolved_height_zscale$vertical_exaggeration,
    caller = "plot_gg"
  )
  if (isTRUE(verbose)) {
    emit_plot_gg_zscale_message(
      resolved_height_zscale = resolved_height_zscale,
      zscale = zscale,
      transform_info = plot_gg_transform_info
    )
  }
  shadowdepth = resolve_plot_gg_shadowdepth(
    height_matrix = height_matrix,
    zscale = zscale,
    shadowdepth = shadowdepth,
    dot_args = dot_args
  )

  if (flat_distance == "auto") {
    if (
      flat_direction == "x" ||
        flat_direction == "-x" ||
        flat_direction == "y" ||
        flat_direction == "-y"
    ) {
      flat_distance = 0.5
    } else {
      if (flat_direction == "z") {
        flat_distance = 3
      } else {
        flat_distance = -3
      }
    }
  } else {
    if (flat_direction == "-z") {
      flat_distance = -flat_distance
    }
  }
  shadow_flat = flat_plot_render &&
    shadow &&
    flat_distance / zscale < shadowdepth
  shadowdepth = ifelse(
    shadow_flat,
    flat_distance / zscale + shadowdepth,
    shadowdepth
  )
  shadelayer = NULL
  map_with_shading = mapcolor
  if (raytrace_mode == "raytrace") {
    if (is.null(saved_shadow_matrix)) {
      shadelayer = do.call(
        ray_shade,
        c(
          list(
            heightmap = height_matrix,
            maxsearch = 600,
            sunangle = sunangle,
            anglebreaks = anglebreaks,
            zscale = zscale,
            multicore = multicore,
            lambert = lambert,
            geographic_aspect = FALSE
          ),
          dot_args
        )
      )
    } else {
      shadelayer = saved_shadow_matrix
    }
    map_with_shading = add_shadow(mapcolor, shadelayer, shadow_intensity)
  } else if (raytrace_mode == "radiance") {
    if (is.null(saved_shadow_matrix)) {
      finite_anglebreaks = suppressWarnings(as.numeric(anglebreaks))
      finite_anglebreaks = finite_anglebreaks[is.finite(finite_anglebreaks)]
      default_lightaltitude = if (length(finite_anglebreaks)) {
        mean(range(finite_anglebreaks))
      } else {
        45
      }
      radiance_call_args = utils::modifyList(
        list(
          lightdirection = sunangle,
          lightaltitude = default_lightaltitude
        ),
        radiance_args
      )
      radiance_call_args = utils::modifyList(
        radiance_call_args,
        list(
          heightmap = height_matrix,
          texture = mapcolor,
          zscale = zscale,
          geographic_aspect = FALSE,
          plot = FALSE
        )
      )
      shadelayer = do.call(radiance_shade, radiance_call_args)
    } else {
      shadelayer = saved_shadow_matrix
    }
    map_with_shading = add_overlay(
      mapcolor,
      shadelayer,
      alphalayer = 1 - shadow_intensity
    )
  }
  if (!preview) {
    do.call(
      plot_3d,
      c(
        list(
          hillshade = map_with_shading,
          heightmap = height_matrix,
          zscale = resolved_height_zscale$base_zscale,
          vertical_exaggeration = resolved_height_zscale$vertical_exaggeration,
          geographic_aspect = FALSE,
          triangulate = triangulate,
          max_error = max_error,
          max_tri = max_tri,
          verbose = verbose,
          shadow = shadow,
          shadowdepth = shadowdepth * zscale,
          background = background,
          shadowcolor = shadowcolor
        ),
        dot_args
      )
    )
  } else {
    if (plot) {
      plot_map(map_with_shading)
    }
    return(invisible(map_with_shading))
  }
  cache_plot_gg_panel_info(plot_gg_panel_info)
  cache_plot_gg_transform_info(plot_gg_transform_info)
  cache_scene_zscale(
    resolved_height_zscale$base_zscale,
    label = "plot_gg_zscale"
  )
  cache_scene_vertical_exaggeration(
    resolved_height_zscale$vertical_exaggeration,
    label = "plot_gg_vertical_exaggeration"
  )
  if (!preview && flat_plot_render) {
    if (flat_transparent_bg) {
      new_temp = tempfile(fileext = ".png")
      color_gg = color_gg +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = NA, color = NA)
        )
      ggplot2::ggsave(
        new_temp,
        color_gg,
        width = width,
        height = height,
        dpi = 300
      )
      colormaptemp = new_temp
    }
    mapcolor = png::readPNG(colormaptemp)
    horizontal_offset = c(0, 0)
    shadowwidth = max(floor(min(dim(height_matrix)) / 10), 5)
    if (flat_direction == "x" || flat_direction == "-x") {
      horizontal_offset = abs(
        c(width * 300, 0) *
          flat_distance +
          c(width * 150, 0) +
          c(shadowwidth * 2, 0)
      )
      if (flat_direction == "-x") {
        horizontal_offset = -horizontal_offset
      }
      flat_distance = 0
    } else if (flat_direction == "y" || flat_direction == "-y") {
      horizontal_offset = abs(
        c(0, height * 300) *
          flat_distance +
          c(0, height * 150) +
          c(0, shadowwidth * 2)
      )
      if (flat_direction == "y") {
        horizontal_offset = -horizontal_offset
      }
      flat_distance = 0
    }

    render_floating_overlay(
      mapcolor,
      altitude = flat_distance,
      heightmap = height_matrix,
      zscale = zscale,
      horizontal_offset = horizontal_offset
    )
    if (shadow && flat_direction %in% c("x", "-x", "y", "-y")) {
      if (shadowcolor == "auto") {
        shadowcolor = convert_color(
          darken_color(background, darken = shadow_darkness),
          as_hex = TRUE
        )
      }
      make_shadow(
        height_matrix,
        shadowdepth,
        shadowwidth,
        background,
        shadowcolor,
        offset = horizontal_offset
      )
    }
  }
  if (!is.null(plot_gg_transform_info)) {
    attr(height_matrix, "ggplot_transform_info") = plot_gg_transform_info
  }
  if (!is.null(plot_gg_panel_info)) {
    attr(height_matrix, "ggplot_panel_info") = plot_gg_panel_info
    if (nrow(plot_gg_panel_info) == 1) {
      attr(height_matrix, "extent") = c(
        xmin = plot_gg_panel_info$extent_xmin,
        xmax = plot_gg_panel_info$extent_xmax,
        ymin = plot_gg_panel_info$extent_ymin,
        ymax = plot_gg_panel_info$extent_ymax
      )
      cache_scene_extent(
        attr(height_matrix, "extent"),
        label = "ggplot_panel_extent"
      )
    } else {
      cache_scene_extent(NULL)
    }
  }
  if (save_shadow_matrix & !save_height_matrix) {
    return(shadelayer)
  }
  if (!save_shadow_matrix & save_height_matrix) {
    return(height_matrix)
  }
  if (save_shadow_matrix & save_height_matrix) {
    return(list(height_matrix, shadelayer))
  }
  invisible(NULL)
}
