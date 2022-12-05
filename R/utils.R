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
      rayimage::render_reorient(x, flipx = FALSE, flipy = FALSE)
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

#' Generate Base Shape
#'
#' @param image Matrix
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
generate_base_shape = function(heightmap, baseshape, angle=0) {
  if(baseshape == "circle") {
    radius = ifelse(nrow(heightmap) <= ncol(heightmap),nrow(heightmap)/2-1,ncol(heightmap)/2-1)
    radmat = gen_circle_psf(radius+1)
    if(min(dim(heightmap)) != min(dim(radmat))) {
      radmat = radmat[2:nrow(radmat),2:ncol(radmat)]
    }
    if(max(dim(heightmap)) != max(dim(radmat))) {
      difference = max(dim(heightmap)) - max(dim(radmat))
      if(difference == 1) {
        difference = difference + 1
      }
      radtemp = matrix(0,nrow=nrow(heightmap),ncol=ncol(heightmap))
      if(ncol(heightmap) != ncol(radmat)) {
        radtemp[,(difference/2):(difference/2+ncol(radmat)-1)] = radmat
      } else {
        radtemp[(difference/2):(difference/2+nrow(radmat)-1),] = radmat
      }
      radmat = radtemp
    }
    heightmap[radmat == 0] = NA
  } else if(baseshape == "hex") {
    radius = ifelse(nrow(heightmap) <= ncol(heightmap),nrow(heightmap)/2-1,ncol(heightmap)/2-1)
    radmat = gen_hex_psf(radius+1,rotation = angle)
    if(min(dim(heightmap)) != min(dim(radmat))) {
      radmat = radmat[2:nrow(radmat),2:ncol(radmat)]
    }
    if(max(dim(heightmap)) != max(dim(radmat))) {
      difference = max(dim(heightmap)) - max(dim(radmat))
      radtemp = matrix(0,nrow=nrow(heightmap),ncol=ncol(heightmap))
      if(ncol(heightmap) != ncol(radmat)) {
        radtemp[,(difference/2):(difference/2+ncol(radmat)-1)] = radmat
      } else {
        radtemp[(difference/2):(difference/2+nrow(radmat)-1),] = radmat
      }
      radmat = radtemp
    }
    heightmap[radmat == 0] = NA
  }
  return(heightmap)
}

#' Run Documentation
#' 
#' @return bool
#'
#' @keywords internal
run_documentation = function() {
  return(identical(Sys.getenv("IN_PKGDOWN"), "true") && 
         length(find.package("magick", quiet = TRUE)) > 0 && 
         length(find.package("sf", quiet = TRUE)) > 0 && 
         length(find.package("rgdal", quiet = TRUE)) > 0)
}

