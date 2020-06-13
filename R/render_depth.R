#'@title Render Depth of Field
#'
#'@description Adds depth of field to the current RGL scene by simulating a synthetic aperture. 
#'
#'The size of the circle of confusion is determined by the following formula (z_depth is from the image's depth map).
#'
#'\code{abs(z_depth-focus)*focal_length^2/(f_stop*z_depth*(focus - focal_length))}
#'
#'@param focus Defaults `0.5`. Depth in which to blur. Minimum 0, maximum 1.
#'@param focallength Default `1`. Focal length of the virtual camera.
#'@param fstop Default `1`. F-stop of the virtual camera.
#'@param filename The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param preview_focus Default `FALSE`. If `TRUE`, a red line will be drawn across the image
#'showing where the camera will be focused.
#'@param bokehshape Default `circle`. Also built-in: `hex`. The shape of the bokeh. 
#'@param bokehintensity Default `3`. Intensity of the bokeh when the pixel intensity is greater than `bokehlimit`.
#'@param bokehlimit Default `0.8`. Limit after which the bokeh intensity is increased by `bokehintensity`. 
#'@param rotation Default `0`. Number of degrees to rotate the hexagon bokeh shape.
#'@param aberration Default `0`. Adds chromatic aberration to the image. Maximum of `1`. 
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@param transparent_water Default `FALSE`. If `TRUE`, depth is determined without water layer. User will have to re-render the water
#'layer with `render_water()` if they want to recreate the water layer.
#'@param heightmap Default `NULL`. The height matrix for the scene. Passing this will allow `render_depth()` 
#'to automatically redraw the water layer if `transparent_water = TRUE`.
#'@param zscale Default `NULL`. The zscale value for the heightmap. Passing this will allow `render_depth()` 
#'to automatically redraw the water layer if `transparent_water = TRUE`.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using magick::image_annotate. 
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in 
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica", 
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_bar_color Default `NULL`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param title_position Default `northwest`. Position of the title.
#'@param image_overlay Default `NULL`. Either a string indicating the location of a png image to overlay
#'over the image (transparency included), or a 4-layer RGBA array. This image will be resized to the 
#'dimension of the image if it does not match exactly.
#'@param vignette Default `FALSE`. If `TRUE` or numeric, a camera vignetting effect will be added to the image.
#'`1` is the darkest vignetting, while `0` is no vignetting. If vignette is a length-2 vector, the second entry will
#'control the blurriness of the vignette effect.
#'@param progbar Default `TRUE` if in an interactive session. Displays a progress bar. 
#'@param instant_capture Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, a slight delay is added 
#'before taking the snapshot. This can help stop prevent rendering issues when running scripts.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param bring_to_front Default `FALSE`. Whether to bring the window to the front when rendering the snapshot.
#'@param ... Additional parameters to pass to magick::image_annotate. 
#'@return 4-layer RGBA array.
#'@export
#'@examples
#'\donttest{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50, water=TRUE, waterlinecolor="white",
#'          zoom=0.3,theta=-135,fov=70, phi=20) 
#'  
#'#Preview where the focal plane lies
#'render_depth(focus=0.75, preview_focus=TRUE)
#'
#'#Render the depth of field effect
#'render_depth(focus=0.75, focallength = 100)
#'
#'#Add a chromatic aberration effect
#'render_depth(focus=0.75, focallength = 100, aberration = 0.3)
#'
#'#Render the depth of field effect, ignoring water and re-drawing the waterlayer
#'render_depth(focus=0.9, preview_focus=TRUE, 
#'             heightmap = montereybay, zscale=50, transparent_water=TRUE)
#'render_depth(focus=0.9, heightmap = montereybay, zscale=50, transparent_water=TRUE)
#'rgl::rgl.close()
#'
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50, water=TRUE, waterlinecolor="white",
#'          zoom=0.7,phi=30,fov=60,theta=-90)
#'          
#'render_camera(theta=45,zoom=0.15,phi=20)
#'
#'#Change the bokeh shape and intensity
#'render_depth(focus=0.7, bokehshape = "circle",focallength=300,bokehintensity=30,
#'             title_text = "Circular Bokeh", title_size = 30, title_color = "white", 
#'             title_bar_color = "black")
#'render_depth(focus=0.7, bokehshape = "hex",focallength=300,bokehintensity=30,
#'             title_text = "Hexagonal Bokeh", title_size = 30, title_color = "white", 
#'             title_bar_color = "black")
#'
#'#Add a title and vignette effect.
#'render_camera(theta=0,zoom=0.7,phi=30)
#'render_depth(focus = 0.75,focallength = 100, title_text = "Monterey Bay, CA", 
#'             title_size = 20, title_color = "white", title_bar_color = "black", vignette = TRUE)
#'             
#'#
#'rgl::rgl.close()
#'}
render_depth = function(focus = 0.5, focallength = 100, fstop = 4, filename=NULL,
                     preview_focus = FALSE, bokehshape = "circle", bokehintensity = 1, bokehlimit=0.8, 
                     rotation = 0, gamma_correction = TRUE, aberration = 0,
                     transparent_water = FALSE, heightmap = NULL, zscale = NULL, 
                     title_text = NULL, title_offset = c(20,20),
                     title_color = "black", title_size = 30, title_font = "sans",
                     title_bar_color = NULL, title_bar_alpha = 0.5, title_position = "northwest",
                     image_overlay = NULL, vignette = FALSE,
                     progbar = interactive(), 
                     instant_capture = interactive(), clear = FALSE, bring_to_front = FALSE, ...) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(!instant_capture) {
    Sys.sleep(0.5)
  }
  if(focallength < 1) {
    stop("focal length must be greater than 1")
  }
  temp = paste0(tempfile(),".png")
  rgl::snapshot3d(filename=temp, top = bring_to_front)
  if(transparent_water) {
    idlist = get_ids_with_labels(typeval = c("water","waterlines"))
    waterid = idlist$id[idlist$raytype == "water"][1]
    waterdepthval = max(rgl::rgl.attrib(waterid, "vertices")[1:3,2],na.rm=TRUE)
    has_waterlines = FALSE
    water_color = idlist$water_color[idlist$raytype == "water"][1]
    water_alpha = idlist$water_alpha[idlist$raytype == "water"][1]
    if("waterlines" %in% idlist$raytype) {
      has_waterlines = TRUE
      water_line_color = idlist$waterline_color[idlist$raytype == "waterlines"][1]
      water_line_alpha = idlist$waterline_alpha[idlist$raytype == "waterlines"][1]
    }
    rgl::pop3d(id=idlist$id)
  }
  if(preview_focus) {
    arraydepth = png::readPNG(temp)
    depthmap = rgl::rgl.pixels(component = "depth")
    maxval = max(depthmap[depthmap != 1])
    depthmap[depthmap == 1] = maxval
    rayimage::render_bokeh(arraydepth, depthmap, focus = focus, preview_focus = TRUE, preview = TRUE)
    if(transparent_water && !is.null(heightmap)) {
      if(is.null(zscale)) {
        zscale = 1
      }
      if(has_waterlines) {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     waterlinealpha = water_line_alpha, waterlinecolor = water_line_color,
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      } else {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      }
    }
  } else {
    depthmap = rgl::rgl.pixels(component = "depth")
    if(transparent_water) {
      remove_ids = get_ids_with_labels(typeval = "waterlines")$id
      rgl::pop3d(id=remove_ids)
    }
    tempmap = png::readPNG(temp)
    if(transparent_water && !is.null(heightmap)) {
      if(is.null(zscale)) {
        zscale = 1
      }
      if(has_waterlines) {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     waterlinealpha = water_line_alpha, waterlinecolor = water_line_color,
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      } else {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      }
    }
    tempmap = rayimage::render_bokeh(tempmap, depthmap = depthmap, focus = focus, focallength = focallength,
                           fstop = fstop, bokehshape = bokehshape, bokehintensity = bokehintensity,
                           bokehlimit = bokehlimit, rotation = rotation, aberration = aberration,
                           gamma_correction = gamma_correction, progress = progbar, preview = FALSE)
    if(!is.null(title_text)) {
      has_title = TRUE
    } else {
      has_title = FALSE
    }
    if(length(title_offset) != 2) {
      stop("`title_offset` needs to be length-2 vector")
    }
    if(!is.null(image_overlay)) {
      if(inherits(image_overlay,"character")) {
        image_overlay_file = image_overlay
        has_overlay = TRUE
      } else if(inherits(image_overlay,"array")) {
        image_overlay_file = tempfile()
        png::writePNG(image_overlay, image_overlay_file)
        has_overlay = TRUE
      }
    } else {
      has_overlay = FALSE
    }
    dimensions = dim(tempmap)
    if(has_overlay) {
      tempmap = rayimage::add_image_overlay(tempmap, image_overlay = image_overlay_file)
    }
    if(vignette || is.numeric(vignette)) {
      tempmap = rayimage::add_vignette(tempmap, vignette = vignette)
    }
    if(has_title) {
      tempmap = rayimage::add_title(tempmap, title_text = title_text, 
                          title_bar_color = title_bar_color,title_bar_alpha = title_bar_alpha,
                          title_offset = title_offset, title_color = title_color,
                          title_position = title_position,
                          title_size = title_size, title_font = title_font)
    }
    if(is.null(filename)) {
      plot_map(tempmap)
    } else {
      save_png(tempmap,filename)
    }
    if(clear) {
      rgl::rgl.clear()
    }
  }
}
