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

#' Get Position from Lat/Long and heightmap/extent
#'
#' @param image Matrix
#'
#' @return x/y/z
#' @keywords internal
#'
#' @examples
#' #Fake example
transform_into_heightmap_coords = function(extent, heightmap, lat, long,
                                           altitude, offset, zscale) {
  if(is.null(heightmap)) {
    vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
    nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
    ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
  } else {
    ncol_map = ncol(heightmap)
    nrow_map = nrow(heightmap)
  }
  e = extent
  distances_x = (long-e@xmin)/(e@xmax - e@xmin) * nrow_map
  distances_y = ncol_map - (lat-e@ymin)/(e@ymax - e@ymin) * ncol_map
  
  if(is.null(altitude)) {
    if(is.null(heightmap)) {
      stop("No altitude data requires heightmap argument be passed")
    }
    distances_x_index = distances_x
    distances_y_index = distances_y
    distances_x_index[floor(distances_x_index) > nrow(heightmap)] = nrow(heightmap)
    distances_y_index[floor(distances_y_index) > ncol(heightmap)] = ncol(heightmap)
    distances_x_index[floor(distances_x_index) < 1] = 1
    distances_y_index[floor(distances_y_index) < 1] = 1
    if(!length(find.package("rayimage", quiet = TRUE)) > 0) {
      xy = matrix(c(floor(distances_x_index),floor(distances_y_index)),
                  nrow=length(distances_x_index),ncol=2)
      flipped_mat = flipud(t(heightmap))
      altitude = apply(xy,1,(function(x) flipped_mat[x[2],x[1]])) + offset
    } else {
      altitude = rayimage::interpolate_array((t(heightmap)), distances_x_index,distances_y_index) + offset
    }
  }
  return(matrix(c(distances_x-nrow_map/2, altitude/zscale, distances_y-ncol_map/2),ncol=3,nrow=length(altitude)))
}
