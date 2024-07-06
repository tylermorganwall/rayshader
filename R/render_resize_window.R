#'@title Resize the rgl Window
#'
#'@param width Default `NULL`, no change to the current value. New window width.
#'@param height Default `NULL`, no change to the current value. New window height
#'@return None
#'@export
#'@examples
#'#Resize the rgl window to various sizes
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90,phi=30)
#'render_resize_window(width = 800, height = 800)
#'render_snapshot()
#'}
#'  
#'if(run_documentation()) {
#'render_resize_window(width = 200, height = 200)
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'render_resize_window(width = 800, height = 400)
#'render_snapshot()
#'}
render_resize_window = function(width = NULL,
                                  height = NULL) {
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  current_rect = rgl::par3d("windowRect")
  if(is.null(width)) {
    width = current_rect[3] - current_rect[1]
  }
  if(is.null(height)) {
    height = current_rect[4] - current_rect[2]
  }
  rgl::par3d(windowRect = c(current_rect[1],current_rect[2],
                            width + current_rect[1],height+current_rect[2]))
}
