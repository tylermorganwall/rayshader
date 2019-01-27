#'@title Render Snapshot of 3D Visualization
#'
#'@description Either captures the current rgl view and displays, or saves the current view to disk. 
#'
#'@param filename Filename of snapshot. If missing, will display to current device.
#'@return Displays snapshot of current rgl plot (or saves to disk).
#'@export
#'@examples
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90)
#'  
#'render_snapshot()
#'rgl::rgl.clear()
render_snapshot = function(filename) {
  if(!rgl.useNULL()) {
    temp = paste0(tempfile(),".png")
    rgl::snapshot3d(filename=temp)
  } else {
    temp = paste0(tempfile())
    system(paste0("xwd -out ",temp,".xwd"))
    system(paste0("convert ",temp,".xwd ", temp,".png"))
    temp = paste0(temp,".png")
  }
  tempmap = png::readPNG(temp)
  if(missing(filename)) {
    plot_map(tempmap)
  } else {
    save_png(tempmap,filename)
  }
}
