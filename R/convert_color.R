#' Convert Color
#'
#' @param color The color to convert. Can be either a hexadecimal code, or a numeric rgb 
#' vector listing three intensities between `0` and `1`.
#'
#' @return Color vector
#' @keywords internal
#'
#' @examples
#' #none
convert_color = function(color, as_hex = FALSE) {
  if(inherits(color,"character")) {
    color = as.vector(col2rgb(color))/255
  } 
  if(!all(color <= 1)) {
    stop("invalid color")
  }
  if(!all(color >= 0)) {
    stop("invalid color")
  }
  if(as_hex) {
    paste0("#",paste0(format(as.hexmode(round(color*255,0)),width=2),collapse=""),collapse="")
  } else {
    color
  }
}