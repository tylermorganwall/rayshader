#'@title Transform Polygon into Raycoords
#'
#'@keywords internal
transform_polygon_into_raycoords = function(polygon, heightmap = NULL, e = NULL, 
                                            top = NULL, bottom = NULL) {
  if(inherits(polygon,"SpatialPolygonsDataFrame") || inherits(polygon,"SpatialPolygons")) {
    polygon = sf::st_as_sf(polygon)
  }
  if(is.null(heightmap)) {
    vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
    nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) -
      min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
    ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) -
      min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
  } else {
    ncol_map = ncol(heightmap)
    nrow_map = nrow(heightmap)
  }
  ncol_map = ncol_map - 1
  nrow_map = nrow_map - 1
  
  if(inherits(polygon,"sf")) {
    if(length(find.package("sf",quiet=TRUE)) == 0) {
      stop("sf package required when handling sf objects")
    }
    #Remove z dimension from multipolygon z geometry
    if(ncol(as.matrix(sf::st_geometry(polygon)[[1]])) == 3) {
      polygon = sf::st_as_sf(sf::as_Spatial(sf::st_zm(polygon)))
    } else {
      polygon = sf::st_as_sf(sf::as_Spatial(polygon))
    }
    new_polygon = sf::st_coordinates(polygon) 
  } else {
    xylist = grDevices::xy.coords(polygon)
    new_polygon = stats::setNames(matrix(c(xylist$x,xylist$y, rep(1,length(xylist$y)*2)),ncol=4), 
                           c("X", "Y", "L1", "L2"))
  }
  
  new_extent = c(nrow_map/2,-nrow_map/2,
                 ncol_map/2,-ncol_map/2)
  new_sf_list = list()
  for(i in seq_len(nrow(polygon))) {
    new_sf_list[[i]] =  transform_polygon_custom_crs(polygon[i,],
                                                     e, new_extent)
    
    new_sf_list[[i]]$top = top[i]
    new_sf_list[[i]]$bottom = bottom[i]
  }
  return(do.call("rbind", new_sf_list))
}

#'@title Transform Points into Raycoords
#'
#'@keywords internal
transform_points_into_raycoords = function(points, heightmap = NULL, e = NULL, 
                                           top = NULL, bottom = NULL) {
  if(is.null(heightmap)) {
    vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
    nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) -
      min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
    ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) -
      min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
  } else {
    ncol_map = ncol(heightmap)
    nrow_map = nrow(heightmap)
  }
  ncol_map = ncol_map - 1
  nrow_map = nrow_map - 1 
  if(inherits(points,"sf")) {
    new_points = sf::st_coordinates(points) 
  } else {
    xylist = grDevices::xy.coords(points)
    new_points = stats::setNames(matrix(c(xylist$x,xylist$y),ncol=2), 
                           c("X", "Y"))
  }
  new_extent = c(nrow_map/2, -nrow_map/2,
                 ncol_map/2, -ncol_map/2)
  return(transform_points_custom_crs(new_points,
                                      e, new_extent))
}


#'@title Transform Polygon into Raycoords
#'
#'@keywords internal
transform_polygon_custom_crs = function(sf_object, orig_extent, new_extent) {
  # Extract coordinates
  coords = as.data.frame(sf::st_coordinates(sf_object))
  coords = coords[,c("X", "Y", "L1", "L2")]
  # Compute scale factors
  
  scale_x = (new_extent[2] - new_extent[1]) / (orig_extent[2] - orig_extent[1])
  scale_y = (new_extent[4] - new_extent[3]) / (orig_extent[4] - orig_extent[3])
  
  # Apply transformation
  coords[,1] = (coords[,1] - orig_extent[1]) * scale_x + new_extent[1]
  coords[,2] = (coords[,2] - orig_extent[3]) * scale_y + new_extent[3]
  
  polygons = split(coords, coords$L2)
  sf_objects = list()
  for(i in seq_along(polygons)) {
    poly_holes = split(polygons[[i]],polygons[[i]]$L1) 
    poly_holes |> 
      lapply(\(x) as.matrix(x[,c("X","Y")])) |> 
      sf::st_polygon() |> 
      sf::st_sfc() |> 
      sf::st_sf() ->
    sf_objects[[i]] 
    
    colnames(sf_objects[[i]]) = "geometry"
    sf::st_geometry(sf_objects[[i]]) = "geometry"
  }

  return(do.call("rbind",sf_objects))
}

#'@title Transform Polygon into Raycoords
#'
#'@keywords internal
transform_points_custom_crs = function(sf_object, orig_extent, new_extent) {
  # Extract coordinates
  # coords = as.data.frame(sf::st_coordinates(sf_object))
  coords = as.data.frame(sf_object)

  coords = coords[,c("X", "Y")]
  # Compute scale factors
  scale_x <- (new_extent[2] - new_extent[1]) / (orig_extent[2] - orig_extent[1])
  scale_y <- (new_extent[4] - new_extent[3]) / (orig_extent[4] - orig_extent[3])
  
  # Apply transformation
  coords[,1] <- (coords[,1] - orig_extent[1]) * scale_x + new_extent[1]
  coords[,2] <- (coords[,2] - orig_extent[3]) * scale_y + new_extent[3]
  
  coords |> 
    as.matrix() |> 
    sf::st_multipoint() |> 
    sf::st_sfc() |> 
    sf::st_sf() ->
  sf_object
  colnames(sf_object) = "geometry"
  sf::st_geometry(sf_object) = "geometry"
  
  return(sf_object)
}
