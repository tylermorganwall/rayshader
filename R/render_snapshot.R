#'@title Render Snapshot of 3D Visualization
#'
#'@description Either captures the current rgl view and displays, or saves the current view to disk. 
#'
#'@param filename Filename of snapshot. If missing, will display to current device.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
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
render_snapshot = function(filename, clear=FALSE) {
  temp = paste0(tempfile(),".png")
  rgl::snapshot3d(filename = temp)
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
