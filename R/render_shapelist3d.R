#'@title Render a generic mesh3d
#'
#'@description Places a mesh shapelists to the current scene, using latitude/longitude or coordinates in the reference
#'
#
#'@param shapelist The shapelists RGL meshes generated with shapelist3d function
#'@param color Default NULL.
#'
#'@return Adds shapelists to current scene. No return value.
#'@export
#'@examples
#'#Add a North arrow to the map, by default in the bottom right (SE)
#'if(interactive()) {
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,theta=-45, water=TRUE)
#' 
#'render_shapelist3d(shapelist3d(icosahedron3d(), 
#'                   x = rnorm(3, sd = shape.scale), y = rnorm(3, sd = shape.scale),
#'                   z = rnorm(3, mean = shape.scale, sd = shape.scale), 
#'                   alpha = 0.3, col = 1:5, size = 10, plot = FALSE))
#' 
#'render_snapshot()
#'
#'
#'#We can change the colors in the compass, and also set it a constant distance away with
#'#`position_circular = TRUE`:
#'
#'render_camera(theta=0,phi=45,zoom=0.75)
#' 
#'render_shapelist3d(shapelist3d(icosahedron3d(), 
#'                   x = rnorm(3, sd = shape.scale), y = rnorm(3, sd = shape.scale),
#'                   z = rnorm(3, mean = shape.scale, sd = shape.scale), 
#'                   alpha = 0.3, col = 1:5, size = 10, plot = FALSE))
#' 

#'render_snapshot(clear=TRUE)
#'}
#'}
#'
render_shapelist3d = function(shapelist = shapelist3d(icosahedron3d(), x = 0, y = 0,
                                                      z = 0, col = 1, size = 40, plot = FALSE),
                              color = NULL
                              ) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  change_color_shape = function(shapes, color, shape_index) {
    color = convert_color(color, as_hex = TRUE)
    shapes[[shape_index]]$material$color = color
    shapes
  }
  if (!is.null(color)){
    shapelist = change_color_shape(shapelist, color, 1)
  }
  #rgl::rgl.material(color = color, ambient = ambient, size = 1)
  rgl::shade3d(shapelist, skipRedraw = FALSE)
}
