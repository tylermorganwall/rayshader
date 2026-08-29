#'@title Transform Polygon into Raycoords
#'
#'@param crs Default `NULL`. CRS of the input numeric x/y coordinates, or CRS to assign to CRS-less spatial data before transforming it into the active scene CRS. If spatial data already carries a CRS, that CRS is used automatically.
#'@param transform_scene Default `TRUE`. If `FALSE`, assumes `polygon` has already been transformed into the active scene coordinate system.
#' @param geographic_aspect Default `get_scene_geographic_aspect()`. Cached
#' horizontal scene scaling applied to the polygon footprint.
#'@keywords internal
transform_polygon_into_raycoords = function(
  polygon,
  heightmap = NULL,
  e = NULL,
  top = NULL,
  bottom = NULL,
  panel = NULL,
  crs = NULL,
  caller = NULL,
  transform_scene = TRUE,
  geographic_aspect = get_scene_geographic_aspect()
) {
  if (
    inherits(polygon, "SpatialPolygonsDataFrame") ||
      inherits(polygon, "SpatialPolygons")
  ) {
    polygon = sf::st_as_sf(polygon)
  }
  if (inherits(polygon, "sfc")) {
    polygon = sf::st_sf(geometry = polygon)
  }
  if (inherits(polygon, "sfg")) {
    polygon = sf::st_sf(geometry = sf::st_sfc(polygon))
  }
  e = resolve_scene_render_extent(
    extent = e,
    heightmap = heightmap,
    caller = caller,
    panel = panel,
    error_if_missing = FALSE
  )
  if (inherits(polygon, "sf") && isTRUE(transform_scene)) {
    scene_polygon = auto_transform_scene_sf(
      sf_object = polygon,
      extent = e,
      heightmap = heightmap,
      panel = panel,
      crs = crs,
      caller = caller
    )
    polygon = scene_polygon$object
    if (!is.null(scene_polygon$extent)) {
      e = scene_polygon$extent
    }
  }
  vertex_info = get_ids_with_labels(typeval = "surface_tris")
  if (nrow(vertex_info) > 1) {
    warning(
      "Multiple surfaces detected: only using the first surface to transform coords"
    )
  }
  ncol_map = vertex_info$ncol[[1]]
  nrow_map = vertex_info$nrow[[1]]

  ncol_map = ncol_map - 1
  nrow_map = nrow_map - 1

  if (inherits(polygon, "sf")) {
    if (length(find.package("sf", quiet = TRUE)) == 0) {
      stop("sf package required when handling sf objects")
    }
    #Remove z dimension from multipolygon z geometry
    if (ncol(as.matrix(sf::st_geometry(polygon)[[1]])) == 3) {
      polygon = sf::st_as_sf(sf::as_Spatial(sf::st_zm(polygon)))
    } else {
      polygon = sf::st_as_sf(sf::as_Spatial(polygon))
    }
    new_polygon = sf::st_coordinates(polygon)
  } else {
    xylist = grDevices::xy.coords(polygon)
    new_polygon = stats::setNames(
      matrix(c(xylist$x, xylist$y, rep(1, length(xylist$y) * 2)), ncol = 4),
      c("X", "Y", "L1", "L2")
    )
  }

  new_extent = c(nrow_map / 2, -nrow_map / 2, ncol_map / 2, -ncol_map / 2)
  new_sf_list = list()
  for (i in seq_len(nrow(polygon))) {
    new_sf_list[[i]] = transform_polygon_custom_crs(
      polygon[i, ],
      e,
      new_extent,
      geographic_aspect = geographic_aspect
    )

    new_sf_list[[i]]$top = top[i]
    new_sf_list[[i]]$bottom = bottom[i]
  }
  return(do.call("rbind", new_sf_list))
}

#'@title Transform Points into Raycoords
#'
#'@keywords internal
transform_points_into_raycoords = function(
  points,
  heightmap = NULL,
  e = NULL,
  top = NULL,
  bottom = NULL
) {
  vertex_info = get_ids_with_labels(typeval = "surface_tris")
  if (nrow(vertex_info) > 1) {
    warning(
      "Multiple surfaces detected: only using the first surface to transform coords"
    )
  }
  ncol_map = vertex_info$ncol[1]
  nrow_map = vertex_info$nrow[1]
  # if(is.null(heightmap)) {
  #   vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
  #   nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) -
  #     min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
  #   ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) -
  #     min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
  # } else {
  #   ncol_map = ncol(heightmap)
  #   nrow_map = nrow(heightmap)
  # }
  ncol_map = ncol_map - 1
  nrow_map = nrow_map - 1
  if (inherits(points, "sf")) {
    new_points = sf::st_coordinates(points)
  } else {
    xylist = grDevices::xy.coords(points)
    new_points = stats::setNames(
      matrix(c(xylist$x, xylist$y), ncol = 2),
      c("X", "Y")
    )
  }
  new_extent = c(nrow_map / 2, -nrow_map / 2, ncol_map / 2, -ncol_map / 2)
  return(transform_points_custom_crs(new_points, e, new_extent))
}


#'@title Transform Polygon into Raycoords
#'
#' @param geographic_aspect Default `identity_geographic_aspect()`. Horizontal
#' scene scaling applied after mapping into ray coordinates.
#'@keywords internal
transform_polygon_custom_crs = function(
  sf_object,
  orig_extent,
  new_extent,
  geographic_aspect = identity_geographic_aspect()
) {
  # Extract coordinates
  coords = as.data.frame(sf::st_coordinates(sf_object))
  coords = coords[, c("X", "Y", "L1", "L2")]
  # Compute scale factors

  scale_x = (new_extent[2] - new_extent[1]) / (orig_extent[2] - orig_extent[1])
  scale_y = (new_extent[4] - new_extent[3]) / (orig_extent[4] - orig_extent[3])

  # Apply transformation
  coords[, 1] = (coords[, 1] - orig_extent[1]) * scale_x + new_extent[1]
  coords[, 2] = (coords[, 2] - orig_extent[3]) * scale_y + new_extent[3]
  geographic_aspect = normalize_geographic_aspect(geographic_aspect)
  coords[, 1] = coords[, 1] * geographic_aspect$scale[["x"]]
  coords[, 2] = coords[, 2] * geographic_aspect$scale[["z"]]

  polygons = split(coords, coords$L2)
  sf_objects = list()
  for (i in seq_along(polygons)) {
    poly_holes = split(polygons[[i]], polygons[[i]]$L1)
    poly_holes |>
      lapply(\(x) as.matrix(x[, c("X", "Y")])) |>
      sf::st_polygon() |>
      sf::st_sfc() |>
      sf::st_sf() -> sf_objects[[i]]

    colnames(sf_objects[[i]]) = "geometry"
    sf::st_geometry(sf_objects[[i]]) = "geometry"
  }

  return(do.call("rbind", sf_objects))
}

#'@title Transform Polygon into Raycoords
#'
#'@keywords internal
transform_points_custom_crs = function(sf_object, orig_extent, new_extent) {
  # Extract coordinates
  # coords = as.data.frame(sf::st_coordinates(sf_object))
  coords = as.data.frame(sf_object)

  coords = coords[, c("X", "Y")]
  # Compute scale factors
  scale_x = (new_extent[2] - new_extent[1]) / (orig_extent[2] - orig_extent[1])
  scale_y = (new_extent[4] - new_extent[3]) / (orig_extent[4] - orig_extent[3])

  # Apply transformation
  coords[, 1] = (coords[, 1] - orig_extent[1]) * scale_x + new_extent[1]
  coords[, 2] = (coords[, 2] - orig_extent[3]) * scale_y + new_extent[3]

  coords |>
    as.matrix() |>
    sf::st_multipoint() |>
    sf::st_sfc() |>
    sf::st_sf() -> sf_object
  colnames(sf_object) = "geometry"
  sf::st_geometry(sf_object) = "geometry"

  return(sf_object)
}
