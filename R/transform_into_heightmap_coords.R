#' Get Position from Lat/Long and heightmap/extent
#'
#' @return x/y/z
#' @keywords internal
#'
#' @examples
#' #Fake example
transform_into_heightmap_coords = function(extent, heightmap, lat = NULL, long = NULL,
                                           altitude = NULL, offset = 0, zscale = 1,
                                           use_altitude = TRUE,
                                           filter_bounds = TRUE) {
  offset = offset/zscale
  e = get_extent(extent)
  if(is.null(lat)) {
    lat = (e["ymax"] + e["ymin"])/2 
  }
  if(is.null(long)) {
    long = (e["xmax"] + e["xmin"])/2
  }
  if(is.null(heightmap)) {
    vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
    nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
    ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
  } else {
    ncol_map = ncol(heightmap)
    nrow_map = nrow(heightmap)
  }
  nrow_map = nrow_map - 1
  ncol_map = ncol_map - 1
  distances_x = (long-e["xmin"])/(e["xmax"] - e["xmin"]) * nrow_map + 1
  distances_y = 1 + ncol_map - (lat-e["ymin"])/(e["ymax"] - e["ymin"]) * ncol_map
  
  if(filter_bounds) {
    filter_out = distances_y > ncol_map | 
      distances_x > nrow_map | 
      distances_x < 0 | 
      distances_y < 0
  } else {
    filter_out = rep(FALSE, length(lat))
  }
  
  if(is.null(altitude)) {
    if(is.null(heightmap)) {
      stop("No altitude data requires heightmap argument be passed")
    }
    distances_x_index = distances_x
    distances_y_index = distances_y
    distances_x_index[floor(distances_x_index) >= nrow(heightmap)] = nrow(heightmap)
    distances_y_index[floor(distances_y_index) >= ncol(heightmap)] = ncol(heightmap)
    distances_x_index[floor(distances_x_index) < 1] = 1
    distances_y_index[floor(distances_y_index) < 1] = 1
    if(!length(find.package("rayimage", quiet = TRUE)) > 0) {
      xy = matrix(c(floor(distances_x_index),floor(distances_y_index)),
                  nrow=length(distances_x_index),ncol=2)
      flipped_mat = flipud(t(heightmap))
      altitude = apply(xy,1,(function(x) flipped_mat[x[2],x[1]])) 
    } else {
      altitude = rayimage::interpolate_array((t(heightmap)), distances_x_index,distances_y_index)
    }
  } else {
    if(length(altitude) == 1) {
      altitude = rep(altitude, length(distances_x))
    }
  }
  altitude[filter_out] = NA
  if(use_altitude) {
    return(matrix(c(distances_x-nrow_map/2-1, altitude/zscale  + offset, distances_y-ncol_map/2-1),ncol=3,nrow=length(altitude)))
  } else {
    return(matrix(c(distances_x-nrow_map/2-1, offset, distances_y-ncol_map/2-1),ncol=3,nrow=length(altitude)))
  }
}