#' Flag 3D Model
#' 
#' 3D obj model of a flag pole, to be used with `render_obj()`. Use `full_flag_obj()` to get the complete
#' pole, and `flag_banner_obj()` and `flag_pole_obj()` to style them separately. 
#' 
#' @return File location of the included flag OBJ file (saved with a .txt extension)
#' @keywords internal
tree_cone_center_obj = function() {
  system.file("extdata", "custom_tree_cone_canopy_center.txt", package="rayshader")
}

#' Flag 3D Model
#' 
#' 3D obj model of a flag pole, to be used with `render_obj()`. Use `full_flag_obj()` to get the complete
#' pole, and `flag_banner_obj()` and `flag_pole_obj()` to style them separately. 
#' 
#' @return File location of the included flag OBJ file (saved with a .txt extension)
#' @keywords internal
tree_basic_center_obj = function() {
  system.file("extdata", "custom_tree_basic_canopy_center.txt", package="rayshader")
}

#' Flag 3D Model
#' 
#' 3D obj model of a flag pole, to be used with `render_obj()`. Use `full_flag_obj()` to get the complete
#' pole, and `flag_banner_obj()` and `flag_pole_obj()` to style them separately. 
#' 
#' @return File location of the included flag OBJ file (saved with a .txt extension)
#' @keywords internal
tree_trunk_obj = function() {
  system.file("extdata", "custom_tree_trunk_center.txt", package="rayshader")
}