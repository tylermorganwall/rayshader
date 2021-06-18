#'@title Render a generic mesh3d
#'
#'@description Places a mesh3d object on the map.
#'
#
#'@param mesh3d Default `0`. The direction the arrow should be facing.
#'@param color Default `blue`.
#'
#'@return Adds mesh3d to map. No return value.
#'@export
#'@examples
#'#Add a North arrow to the map, by default in the bottom right (SE)
#'if(interactive()) {
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,theta=-45, water=TRUE)
#'render_compass()
#'render_snapshot()
#'
#'
#'#We can change the colors in the compass, and also set it a constant distance away with
#'#`position_circular = TRUE`:
#'
#'render_camera(theta=0,phi=45,zoom=0.75)
#'render_mesh3d(mesh3d = cube3d(), color = "blue)
#'render_snapshot(clear=TRUE)
#'}
#'}
#'
render_shapelist3d = function(shapelist = shapelist3d(shapes = list(cube3d()), plot = FALSE), 
                              color = "blue",
                              ambient = "#000020") {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  change_color_shape = function(shapes, color, shape_index) {
    color = convert_color(color, as_hex = TRUE)
    shapes[[shape_index]]$material$color = color
    shapes
  }
  
  shapelist = change_color_shape(shapelist, color, 1)
  rgl::rgl.material(color = color, ambient = ambient, size = 1)
  rgl::shade3d(shapelist,  
          lit=FALSE, ambient = "#000010", skipRedraw = FALSE)
}
