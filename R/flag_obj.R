#' Flag 3D Model
#' 
#' 3D obj model of a flag, to be used with `render_obj()`. Use `flag_full_obj()` to get the complete
#' pole, and `flag_banner_obj()` and `flag_pole_obj()` to style them separately. 
#' 
#' @return File location of the included flag OBJ file (saved with a .txt extension)
#' @export
#'
#' @examples
#' #Print the location of the flag file
#' flag_full_obj()
flag_full_obj = function() {
  system.file("extdata", "full_flag.txt", package="rayshader")
}

#' Flag Pole 3D Model
#' 
#' 3D obj model of a flag pole, to be used with `render_obj()`. Use `full_flag_obj()` to get the complete
#' pole, and `flag_banner_obj()` and `flag_pole_obj()` to style them separately. 
#' 
#' @return File location of the included flag OBJ file (saved with a .txt extension)
#' @export
#'
#' @examples
#' #Print the location of the flag file
#' flag_pole_obj()
flag_pole_obj = function() {
  system.file("extdata", "pole.txt", package="rayshader")
}

#' Flag Banner 3D Model
#' 
#' 3D obj model of a flag (sans pole), to be used with `render_obj()`. Use `flag_full_obj()` to get the complete
#' pole, and `flag_banner_obj()` and `flag_pole_obj()` to style them separately. 
#' 
#' @return File location of the included flag OBJ file (saved with a .txt extension)
#' @export
#'
#' @examples
#' #Print the location of the flag file
#' flag_banner_obj()
flag_banner_obj = function() {
  system.file("extdata", "flag.txt", package="rayshader")
}