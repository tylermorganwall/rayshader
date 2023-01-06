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

#' Unit Vector
#' 
#' @return vec
#' @keywords internal
unit_vector = function(x) {
  x/sqrt(sum(x * x))
}

#' Cross Product 
#' 
#' @return vec
#' @keywords internal
cross = function(u, v) {
  return(c(u[2] * v[3] - u[3] * v[2],
           u[3] * v[1] - u[1] * v[3],
           u[1] * v[2] - u[2] * v[1]))
}

#' Build From W
#' 
#' @return mat
#' @keywords internal
build_from_w = function(dir) {
  basis = matrix(0,3,3)
  basis[3,] = unit_vector(dir)
  if(abs(basis[3,1]) > 0.9) {
    a = c(0,1,0)
  } else {
    a = c(1,0,0)
  }
  basis[2,] = unit_vector(cross(basis[3,],a))
  basis[1,] = -cross(basis[3,], basis[2,])
  basis
}
#' Local To World
#' 
#' @return mat
#' @keywords internal
local_to_world = function(a, mat) {
  return(a[1]*mat[1,1] + a[2]*mat[2,] + a[3] *mat[3,]);
}

#' Generate Rotation Matrix
#' 
#' @return mat
#' @keywords internal
generate_rot_matrix = function(angle, order_rotation = c(1,2,3)) {
  rots = list()
  rots[[1]] = matrix(c(1,0,0,0,cos(angle[1]),sin(angle[1]),0,-sin(angle[1]),cos(angle[1])),3,3)
  rots[[2]] = matrix(c(cos(angle[2]),0,-sin(angle[2]),0,1,0,sin(angle[2]),0,cos(angle[2])),3,3)
  rots[[3]] = matrix(c(cos(angle[3]),sin(angle[3]),0,-sin(angle[3]),cos(angle[3]),0,0,0,1),3,3)
  returnmat = matrix(c(1,0,0,0,1,0,0,0,1),3,3)
  for(i in 1:3) {
    returnmat = returnmat %*% rots[[order_rotation[i]]]
  }
  return(returnmat)
}

#' Generate LookAt Matrix
#' 
#' @return mat
#' @keywords internal
lookat = function(from, to, up = c(0,1,0)) { 
  forward = unit_vector(from - to)
  right = unit_vector(cross(up, forward))
  newup = cross(forward, right)
  
  m = matrix(0,4,4)
  m[1:3,1] = right 
  m[1:3,2] = newup 
  m[1:3,3] = forward 
  m[1:3,4] = from 
  m[4,4] = 1
  return(m)
} 
