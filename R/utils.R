#' Load Image
#'
#' @param image Matrix
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
load_image = function(image, reorient) {
  reorient_fun = function(x) return(x)
  if(reorient) {
    reorient_fun = function(x) {
      flipud(x)
    }
  }
  if(is.array(image)) {
    return(reorient_fun(image))
  }
  if(is.character(image)) {
    if(!file.exists(image)) {
      stop("file ", image, " wasn't found")
    }
    ext = tolower(tools::file_ext(image))
    if(ext == "png") {
      return(reorient_fun(png::readPNG(image)))
    } else if (ext %in% c("jpg","jpeg")) {
      return(reorient_fun(jpeg::readJPEG(image)))
      
    } else {
      stop("filetype ", ext, " not supported for overlays") 
    }
  }
}