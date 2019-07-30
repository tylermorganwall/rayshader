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
#'@param image_overlay Default `NULL`. Either a string indicating the location of a png image to overlay
#'over the image (transparency included), or a 4-layer RGBA array. This image will be resized to the 
#'dimension of the image if it does not match exactly.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param ... Additional parameters to pass to magick::image_annotate. 
#'@return Displays snapshot of current rgl plot (or saves to disk).
#'@export
#'@examples
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90)
#'}
#'  
#'\dontrun{
#'render_snapshot(clear = TRUE)
#'}
#'
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90)
#'}
#'  
#'#Create a title, but also pass the `gravity` argument to magick::image_annotate using ...
#'#to center the text.
#'\dontrun{
#'render_snapshot(clear = TRUE, title_text = "Monterey Bay, California", title_color = "darkgreen",
#'                title_font = "Helvetica", gravity = "North", title_offset = c(0,0))
#'}
render_snapshot = function(filename, clear=FALSE, 
                           title_text = NULL, title_offset = c(20,20), 
                           title_color = "black", title_size = 30, title_font = "sans",
                           image_overlay = NULL, ...) {
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
  rgl::snapshot3d(filename = temp)
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
  if(has_title) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding title")
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
    plot_map(tempmap)
  } else {
    save_png(tempmap, filename)
  }
  if(clear) {
    rgl::rgl.clear()
  }
}
