#' Create identity geographic aspect metadata
#'
#' @return Geographic aspect metadata with unit scene scaling.
#' @keywords internal
identity_geographic_aspect = function() {
  list(
    enabled = FALSE,
    scale = c(x = 1, z = 1),
    cell_meters = c(x = NA_real_, z = NA_real_),
    mean_cell_meters = NA_real_,
    center_latitude = NA_real_
  )
}

#' Normalize geographic aspect metadata
#'
#' @param aspect Geographic aspect metadata.
#'
#' @return Validated geographic aspect metadata.
#' @keywords internal
normalize_geographic_aspect = function(aspect = NULL) {
  identity = identity_geographic_aspect()
  if (is.null(aspect) || !is.list(aspect)) {
    return(identity)
  }
  scale = suppressWarnings(as.numeric(aspect$scale)[1:2])
  if (length(scale) != 2L || any(!is.finite(scale)) || any(scale <= 0)) {
    scale = identity$scale
  }
  if (max(abs(scale - 1)) <= sqrt(.Machine$double.eps)) {
    scale = c(1, 1)
  }
  names(scale) = c("x", "z")
  cell_meters = suppressWarnings(as.numeric(aspect$cell_meters)[1:2])
  if (
    length(cell_meters) != 2L ||
      any(!is.finite(cell_meters)) ||
      any(cell_meters <= 0)
  ) {
    cell_meters = identity$cell_meters
  }
  names(cell_meters) = c("x", "z")
  mean_cell_meters = suppressWarnings(as.numeric(
    aspect$mean_cell_meters
  )[1])
  if (!is.finite(mean_cell_meters) || mean_cell_meters <= 0) {
    mean_cell_meters = NA_real_
  }
  center_latitude = suppressWarnings(as.numeric(aspect$center_latitude)[1])
  if (!is.finite(center_latitude)) {
    center_latitude = NA_real_
  }
  list(
    enabled = isTRUE(aspect$enabled) && !isTRUE(all.equal(scale, c(1, 1))),
    scale = scale,
    cell_meters = cell_meters,
    mean_cell_meters = mean_cell_meters,
    center_latitude = center_latitude
  )
}

#' Enable or disable geographic aspect metadata
#'
#' @param aspect Geographic aspect metadata.
#' @param enabled Whether to apply the cached scale.
#'
#' @return Geographic aspect metadata with the requested activation state.
#' @keywords internal
set_geographic_aspect_enabled = function(aspect, enabled = TRUE) {
  aspect = normalize_geographic_aspect(aspect)
  aspect$enabled = isTRUE(enabled)
  aspect$scale = if (
    isTRUE(enabled) &&
      is.finite(aspect$mean_cell_meters) &&
      all(is.finite(aspect$cell_meters))
  ) {
    aspect$cell_meters / aspect$mean_cell_meters
  } else {
    c(x = 1, z = 1)
  }
  normalize_geographic_aspect(aspect)
}

#' Resolve geographic aspect metadata for a cached heightmap
#'
#' @param source Cache source, either `"hillshade"` or `"scene"`.
#' @param geographic_aspect Default `TRUE`. Whether to apply correction.
#' @param fallback Default `NULL`. Metadata used when the cache has no metric
#' aspect information.
#'
#' @return Geographic aspect metadata.
#' @keywords internal
resolve_cached_geographic_aspect = function(
  source,
  geographic_aspect = TRUE,
  fallback = NULL
) {
  cached = if (identical(source, "scene")) {
    get_scene_geographic_aspect()
  } else {
    get_hillshade_geographic_aspect()
  }
  if (
    !is.finite(cached$mean_cell_meters) ||
      any(!is.finite(cached$cell_meters))
  ) {
    return(normalize_geographic_aspect(fallback))
  }
  set_geographic_aspect_enabled(cached, geographic_aspect)
}

#' Calculate metric geographic aspect metadata
#'
#' @param heightmap Heightmap matrix.
#' @param extent Spatial extent.
#' @param crs Coordinate reference system for the extent.
#' @param geographic_aspect Default `TRUE`. Whether to apply unequal metric cell
#' spacing to scene geometry.
#' @param extent_is_cell_bounds Default `FALSE`. Whether the extent describes
#' outer raster cell bounds rather than matrix sample coordinates.
#'
#' @return Geographic aspect metadata.
#' @keywords internal
calculate_geographic_aspect = function(
  heightmap,
  extent = NULL,
  crs = NULL,
  geographic_aspect = TRUE,
  extent_is_cell_bounds = FALSE
) {
  identity = identity_geographic_aspect()
  if (
    !is.matrix(heightmap) ||
      nrow(heightmap) < 2L ||
      ncol(heightmap) < 2L ||
      is.null(extent) ||
      is.null(crs) ||
      !requireNamespace("sf", quietly = TRUE)
  ) {
    return(identity)
  }
  extent = tryCatch(get_extent(extent), error = function(error) NULL)
  parsed_crs = try_parse_scene_crs(crs)
  if (is.null(extent) || is.null(parsed_crs)) {
    return(identity)
  }
  x_intervals = if (isTRUE(extent_is_cell_bounds)) {
    nrow(heightmap)
  } else {
    nrow(heightmap) - 1L
  }
  z_intervals = if (isTRUE(extent_is_cell_bounds)) {
    ncol(heightmap)
  } else {
    ncol(heightmap) - 1L
  }
  x_step = abs(extent[["xmax"]] - extent[["xmin"]]) / x_intervals
  z_step = abs(extent[["ymax"]] - extent[["ymin"]]) / z_intervals
  center = c(
    mean(extent[c("xmin", "xmax")]),
    mean(extent[c("ymin", "ymax")])
  )
  if (
    any(!is.finite(c(x_step, z_step, center))) || any(c(x_step, z_step) <= 0)
  ) {
    return(identity)
  }
  if (
    isTRUE(sf::st_is_longlat(parsed_crs)) &&
      (extent[["xmin"]] < -180 ||
        extent[["xmax"]] > 180 ||
        extent[["ymin"]] < -90 ||
        extent[["ymax"]] > 90)
  ) {
    return(identity)
  }
  metric_info = tryCatch(
    {
      axis_points = sf::st_sfc(
        sf::st_point(center),
        sf::st_point(center + c(x_step, 0)),
        sf::st_point(center + c(0, z_step)),
        crs = parsed_crs
      )
      axis_points_longlat = sf::st_transform(axis_points, 4326)
      cell_meters = c(
        as.numeric(sf::st_distance(
          axis_points_longlat[1],
          axis_points_longlat[2],
          by_element = TRUE
        )),
        as.numeric(sf::st_distance(
          axis_points_longlat[1],
          axis_points_longlat[3],
          by_element = TRUE
        ))
      )
      center_longlat = sf::st_coordinates(axis_points_longlat[1])
      list(
        cell_meters = cell_meters,
        center_latitude = center_longlat[1, 2]
      )
    },
    error = function(error) NULL
  )
  if (
    is.null(metric_info) ||
      any(!is.finite(metric_info$cell_meters)) ||
      any(metric_info$cell_meters <= 0)
  ) {
    return(identity)
  }
  mean_cell_meters = mean(metric_info$cell_meters)
  scale = if (isTRUE(geographic_aspect)) {
    metric_info$cell_meters / mean_cell_meters
  } else {
    c(1, 1)
  }
  normalize_geographic_aspect(list(
    enabled = isTRUE(geographic_aspect),
    scale = scale,
    cell_meters = metric_info$cell_meters,
    mean_cell_meters = mean_cell_meters,
    center_latitude = metric_info$center_latitude
  ))
}

#' Cache hillshade geographic aspect metadata
#'
#' @param aspect Geographic aspect metadata.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
cache_hillshade_geographic_aspect = function(aspect = NULL) {
  assign(
    "hillshade_geographic_aspect",
    normalize_geographic_aspect(aspect),
    envir = ray_cache_scene_envir
  )
  invisible(NULL)
}

#' Get cached hillshade geographic aspect metadata
#'
#' @return Geographic aspect metadata.
#' @keywords internal
get_hillshade_geographic_aspect = function() {
  normalize_geographic_aspect(get0(
    "hillshade_geographic_aspect",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  ))
}

#' Cache scene geographic aspect metadata
#'
#' @param aspect Geographic aspect metadata.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
cache_scene_geographic_aspect = function(aspect = NULL) {
  assign(
    "scene_geographic_aspect",
    normalize_geographic_aspect(aspect),
    envir = ray_cache_scene_envir
  )
  invisible(NULL)
}

#' Get cached scene geographic aspect metadata
#'
#' @return Geographic aspect metadata.
#' @keywords internal
get_scene_geographic_aspect = function() {
  normalize_geographic_aspect(get_scene_context_value(
    "scene_geographic_aspect",
    default = NULL
  ))
}

#' Scale scene vertices for geographic aspect
#'
#' @param vertices Matrix containing x/y/z vertices.
#' @param aspect Default `get_scene_geographic_aspect()`. Geographic aspect
#' metadata.
#'
#' @return Scaled vertex matrix.
#' @keywords internal
apply_geographic_aspect_to_vertices = function(
  vertices,
  aspect = get_scene_geographic_aspect()
) {
  aspect = normalize_geographic_aspect(aspect)
  if (!is.matrix(vertices) || ncol(vertices) < 3L) {
    return(vertices)
  }
  vertices[, 1] = vertices[, 1] * aspect$scale[["x"]]
  vertices[, 3] = vertices[, 3] * aspect$scale[["z"]]
  vertices
}

#' Correct normal vectors for geographic aspect
#'
#' @param normals Normal-vector list returned by [calculate_normal()].
#' @param aspect Geographic aspect metadata.
#'
#' @return Corrected normal-vector list.
#' @keywords internal
correct_normal_geographic_aspect = function(normals, aspect) {
  aspect = normalize_geographic_aspect(aspect)
  existing_aspect = attr(normals, "geographic_aspect", exact = TRUE)
  if (!is.null(existing_aspect)) {
    existing_aspect = normalize_geographic_aspect(existing_aspect)
    if (isTRUE(all.equal(existing_aspect$scale, aspect$scale))) {
      return(normals)
    }
    normals$x = normals$x * existing_aspect$scale[["z"]]
    normals$y = normals$y * existing_aspect$scale[["x"]]
  }
  normals$x = normals$x / aspect$scale[["z"]]
  normals$y = normals$y / aspect$scale[["x"]]
  magnitude = sqrt(normals$x^2 + normals$y^2 + normals$z^2)
  finite = is.finite(magnitude) & magnitude > 0
  normals$x[finite] = normals$x[finite] / magnitude[finite]
  normals$y[finite] = normals$y[finite] / magnitude[finite]
  normals$z[finite] = normals$z[finite] / magnitude[finite]
  attr(normals, "geographic_aspect") = aspect
  normals
}
