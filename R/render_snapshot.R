#'@title Render Snapshot of 3D Visualization
#'
#'@description Either captures the current rgl view and displays, or saves the current view to disk. 
#'
#'@param filename Filename of snapshot. If missing, will display to current device.
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
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param instant_capture Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, a slight delay is added 
#'before taking the snapshot. This can help stop prevent rendering issues when running scripts.
#'@param bring_to_front Default `FALSE`. Whether to bring the window to the front when taking the snapshot.
#'@param keep_user_par Default `TRUE`. Whether to keep the user's `par()` settings. Set to `FALSE` if you 
#'want to set up a multi-pane plot (e.g. set `par(mfrow)`).
#'@param webshot Default `FALSE`. Set to `TRUE` to have rgl use the `webshot2` package to take images,
#'which can be used when `rgl.useNULL = TRUE`.
#'@param width Default `NULL`. Optional argument to pass to `rgl::snapshot3d()` to specify the
#'width when `software_render = TRUE`..
#'@param height Default `NULL`. Optional argument to pass to `rgl::snapshot3d()` to specify the
#'height when `software_render = TRUE`.
#'@param software_render Default `FALSE`. If `TRUE`, rayshader will use the rayvertex package to render the snapshot, which
#'is not constrained by the screen size or requires OpenGL. 
#'Consider settings a `cache_filename` so a new OBJ file doesn't have to be written with every snapshot.
#'@param cache_filename Default `NULL`. Name of temporary filename to store OBJ file, if the user does not want to rewrite the file each time.
#'@param background Default `"white"`. Background color when `software_render = TRUE`.
#'@param text_angle Default `NULL`, which forces the text always to face the camera. If a single angle (degrees),
#'will specify the absolute angle all the labels are facing. If three angles, this will specify all three orientations
#'(relative to the x,y, and z axes) of the text labels.
#'@param text_size Default `30`. Height of the text.
#'@param point_radius Default `0.5`. Radius of 3D points (rendered with `render_points()`.
#'@param camera_location Default `NULL`. Custom position of the camera. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window.
#'@param camera_lookat Default `NULL`. Custom point at which the camera is directed. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window.
#'@param text_offset Default `c(0,0,0)`. Offset to be applied to all text labels.
#'@param print_scene_info Default `FALSE`. If `TRUE`, it will print the position and lookat point of the camera.
#'@param ... Additional parameters to pass to magick::image_annotate. 
#'@return Displays snapshot of current rgl plot (or saves to disk).
#'@export
#'@examples
#'\donttest{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90,phi=30)
#'}
#'  
#'\donttest{
#'render_snapshot()
#'}
#'  
#'#Create a title, but also pass the `gravity` argument to magick::image_annotate using ...
#'#to center the text.
#'\donttest{
#'render_snapshot(title_text = "Monterey Bay, California", 
#'                title_color = "white", title_bar_color = "black",
#'                title_font = "Helvetica", gravity = "North")
#'                
#'#Add a vignette effect
#'render_camera(zoom=0.8)
#'render_snapshot(title_text = "Monterey Bay, California", 
#'                title_color = "white", title_bar_color = "darkgreen",
#'                vignette = TRUE, 
#'                title_font = "Helvetica", gravity = "North")
#'rgl::rgl.close() 
#'}
render_snapshot = function(filename, clear=FALSE, 
                           title_text = NULL, title_offset = c(20,20), 
                           title_color = "black", title_size = 30, title_font = "sans",
                           title_bar_color = NULL, title_bar_alpha = 0.5, 
                           title_position = "northwest",
                           image_overlay = NULL, vignette = FALSE,
                           instant_capture = interactive(), bring_to_front = FALSE, 
                           keep_user_par = FALSE, webshot = FALSE, 
                           width = NULL, height = NULL, 
                           software_render = FALSE, camera_location = NULL, camera_lookat = c(0,0,0),
                           background = "white",
                           text_angle = NULL, text_size = 30, text_offset = c(0,0,0),
                           point_radius = 0.5,
                           cache_filename  = NULL,  
                           print_scene_info = FALSE, ...) {
  if(rgl::rgl.useNULL()) {
    software_render = TRUE
  }
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(!instant_capture) {
    Sys.sleep(0.5)
  }
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
  temp = tempfile(fileext = ".png")
  if(!software_render) {
    if("webshot" %in% names(formals(rgl::snapshot3d))) {
      if(!rgl::rgl.useNULL()) {
        webshot = FALSE
      }
      if(webshot) {
        rgl::snapshot3d(filename = temp, webshot = webshot, width = width, height = height)
      } else {
        rgl::snapshot3d(filename = temp, top = bring_to_front, webshot = webshot)
      }
    } else {
      rgl::snapshot3d(filename = temp, top = bring_to_front)
    }
  } else {
    render_snapshot_software(filename = temp, cache_filename = cache_filename,
                             camera_location = camera_location, camera_lookat = camera_lookat,
                             background="white",
                             width = width, height = height, light_direction = c(0,1,0), fake_shadow = TRUE, 
                             text_angle = text_angle, text_size = text_size, text_offset = text_offset,
                             print_scene_info = print_scene_info, point_radius = point_radius, ...)
  }
  tempmap = png::readPNG(temp)
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
  if(missing(filename)) {
    plot_map(tempmap, keep_user_par = keep_user_par)
  } else {
    save_png(tempmap, filename)
  }
  if(clear) {
    rgl::rgl.clear()
  }
}
