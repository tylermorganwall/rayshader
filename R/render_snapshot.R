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
#'@param ... Additional parameters to pass to magick::image_annotate. 
#'@return Displays snapshot of current rgl plot (or saves to disk).
#'@export
#'@examples
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90,phi=30)
#'}
#'  
#'\dontrun{
#'render_snapshot()
#'}
#'
#'\dontrun{
#'}
#'  
#'#Create a title, but also pass the `gravity` argument to magick::image_annotate using ...
#'#to center the text.
#'\dontrun{
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
                           image_overlay = NULL, vignette = FALSE,
                           instant_capture = interactive(), bring_to_front = FALSE, ...) {
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
    if("character" %in% class(image_overlay)) {
      image_overlay_file = image_overlay
      has_overlay = TRUE
    } else if("array" %in% class(image_overlay)) {
      image_overlay_file = tempfile()
      png::writePNG(image_overlay_file)
      has_overlay = TRUE
    }
  } else {
    has_overlay = FALSE
  }
  temp = paste0(tempfile(),".png")
  rgl::snapshot3d(filename = temp, top = bring_to_front)
  tempmap = png::readPNG(temp)
  dimensions = dim(tempmap)
  if(has_overlay) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding overlay")
    }
    magick::image_read(temp) %>%
      magick::image_composite(
        magick::image_scale(magick::image_read(image_overlay_file),
                            paste0(dimensions[1],"x",dimensions[2]))
      ) %>%
      magick::image_write(path = temp, format = "png")
  }
  if(vignette || is.numeric(vignette)) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding overlay")
    }
    if(length(vignette) > 1) {
      if(vignette[2] < 0) {
        stop("vignette[2] must be greater than 0")
      }
      radiusval = min(c(dimensions[1],dimensions[2]))/2 * vignette[2]
    } else {
      radiusval = min(c(dimensions[1],dimensions[2]))/2
    }
    if(is.numeric(vignette)) {
      if(vignette[1] > 1 || vignette[1] < 0) {
        stop("vignette value (", vignette[1],") must be between 0 and 1.")
      }
    } else {
      vignette = 0.4
    }
    imagefile = make_vignette_overlay(dimensions[1],dimensions[2], vignette, radiusval)
    magick::image_read(temp) %>%
      magick::image_composite(magick::image_read(imagefile)) %>%
      magick::image_write(path = temp, format = "png")
  }
  if(has_title) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding title")
    }
    if(!is.null(title_bar_color)) {
      title_bar_color = col2rgb(title_bar_color)/255
      title_bar = array(0,c(dimensions[1],dimensions[2],4))
      title_bar_width = 2 * title_offset[1] + title_size
      title_bar[1:title_bar_width,,1] = title_bar_color[1]
      title_bar[1:title_bar_width,,2] = title_bar_color[2]
      title_bar[1:title_bar_width,,3] = title_bar_color[3]
      title_bar[1:title_bar_width,,4] = title_bar_alpha
      title_bar_temp = paste0(tempfile(),".png")
      png::writePNG(title_bar,title_bar_temp)
      magick::image_read(temp) %>%
        magick::image_composite(magick::image_read(title_bar_temp),
        ) %>%
        magick::image_write(path = temp, format = "png")
    }
    magick::image_read(temp) %>%
      magick::image_annotate(title_text, 
                             location = paste0("+", title_offset[1],"+",title_offset[2]),
                             size = title_size, color = title_color, 
                             font = title_font, ...) %>%
      magick::image_write(path = temp, format = "png")
  }
  tempmap = png::readPNG(temp)
  if(missing(filename)) {
    plot_map(tempmap, keep_user_par = FALSE)
  } else {
    save_png(tempmap, filename)
  }
  if(clear) {
    rgl::rgl.clear()
  }
}
