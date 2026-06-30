#' Render Stream Paths
#'
#' @description Adds stream paths to the scene, removing the previous stream layer if desired.
#'
#' @param streams Spatial line data used to draw stream paths. Supports `sf`,
#' `sfc`, `sfg`, `SpatialLines`, and `SpatialLinesDataFrame` line inputs.
#' @param heightmap Default `NULL`. Height matrix or spatial raster for the current
#' scene. If omitted, this is taken from the cached scene set by [plot_3d()] or
#' [plot_gg()]. Pass explicitly to override the cached value.
#' @param watercolor Default `"lightblue"`. Stream color.
#' @param zscale Default `1`. The ratio between the x and y spacing and the z axis.
#' If omitted and `heightmap` is a spatial raster, rayshader uses the raster cell
#' resolution.
#' @param vertical_exaggeration Default `1`. Multiplier applied to the effective
#' visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()]
#' or [plot_gg()] when available; pass explicitly to override for this call.
#' @param width Default `1`. Stream width in scene grid-cell units for
#' [render_highquality()]. The rgl preview uses the same value as line width.
#' @param densify Default `TRUE`. Whether to densify stream paths and resample
#' them along the terrain before [render_highquality()] meshing. Set to `FALSE`
#' to use the vertices returned by [render_path()] directly.
#' @param offset Default `NULL`. Vertical stream centerline offset in elevation
#' units. When `NULL`, the stream centerline is placed on the sampled surface so
#' the high-quality rectangular stream profile is rendered halfway through the
#' terrain.
#' @param merge Default `TRUE`. Whether to merge connected stream linework before
#' rendering. This reduces visible caps between adjacent line features in
#' [render_highquality()].
#' @param clear_previous Default `TRUE`. If `TRUE`, removes the existing stream
#' layer before drawing the new one.
#'
#' @return Invisibly returns the rendered stream coordinates.
#' @export
render_streams = function(
  streams,
  heightmap = NULL,
  watercolor = "lightblue",
  zscale = 1,
  vertical_exaggeration = 1,
  width = 1,
  densify = TRUE,
  offset = NULL,
  merge = TRUE,
  clear_previous = TRUE
) {
  heightmap = resolve_render_water_heightmap(
    heightmap,
    heightmap_missing = missing(heightmap),
    caller = "render_streams"
  )
  if (is.null(heightmap)) {
    stop(
      "No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
    )
  }
  zscale = resolve_render_water_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    heightmap = heightmap,
    caller = "render_streams"
  )
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if (isTRUE(clear_previous)) {
    rgl::pop3d(tag = "water_path")
  }
  render_water_paths(
    waterpaths = streams,
    heightmap = heightmap,
    extent = resolve_scene_render_extent(
      heightmap = heightmap,
      caller = "render_streams",
      error_if_missing = FALSE
    ),
    zscale = zscale,
    watercolor = watercolor,
    waterpath_width = width,
    waterpath_densify = densify,
    waterpath_offset = offset,
    waterpath_merge = merge
  )
}

#' Is water path input
#'
#' @param x Object to test.
#'
#' @return Logical value.
#' @keywords internal
is_waterpath_input = function(x) {
  inherits(
    x,
    c(
      "sf",
      "sfc",
      "sfg",
      "SpatialLines",
      "SpatialLinesDataFrame"
    )
  )
}

#' Render water stream paths
#'
#' @param waterpaths Spatial line input.
#' @param heightmap Heightmap matrix.
#' @param extent Scene extent.
#' @param zscale Effective zscale.
#' @param watercolor Water color.
#' @param waterpath_width Stream width.
#' @param waterpath_densify Whether to densify paths.
#' @param waterpath_offset Centerline offset in elevation units.
#' @param waterpath_merge Whether to merge connected linework.
#'
#' @return Invisibly returns the rendered stream coordinates.
#' @keywords internal
render_water_paths = function(
  waterpaths,
  heightmap,
  extent,
  zscale,
  watercolor,
  waterpath_width = 1,
  waterpath_densify = TRUE,
  waterpath_offset = NULL,
  waterpath_merge = TRUE
) {
  if (!is_waterpath_input(waterpaths)) {
    stop(
      "`streams` must be an sf, sfc, sfg, SpatialLines, or SpatialLinesDataFrame line object.",
      call. = FALSE
    )
  }
  waterpath_width = validate_waterpath_positive_number(
    waterpath_width,
    "width"
  )
  waterpath_densify = validate_waterpath_logical(
    waterpath_densify,
    "densify"
  )
  waterpath_merge = validate_waterpath_logical(
    waterpath_merge,
    "merge"
  )
  waterpath_offset = resolve_waterpath_offset(
    waterpath_offset,
    name = "offset"
  )
  waterpaths = prepare_render_water_path_geometry(
    waterpaths = waterpaths,
    waterpath_merge = waterpath_merge
  )
  coord_list = render_path(
    y = waterpaths,
    extent = extent,
    zscale = zscale,
    vertical_exaggeration = 1,
    heightmap = heightmap,
    offset = 0,
    linewidth = waterpath_width,
    color = watercolor,
    return_coords = TRUE,
    tag = "water_path"
  )
  if (!length(coord_list)) {
    return(invisible(coord_list))
  }
  if (isTRUE(waterpath_densify)) {
    coord_list = densify_water_path_coords(
      coord_list = coord_list,
      heightmap = heightmap,
      zscale = zscale,
      offset = waterpath_offset
    )
  } else if (!identical(waterpath_offset, 0)) {
    coord_list = offset_water_path_coords(
      coord_list = coord_list,
      offset = waterpath_offset / zscale
    )
  }
  for (coord in coord_list) {
    if (is.matrix(coord) && nrow(coord) >= 2) {
      rgl::lines3d(
        coord,
        color = watercolor,
        tag = "water_path",
        lwd = waterpath_width,
        line_antialias = FALSE
      )
    }
  }
  invisible(coord_list)
}

#' Validate water path positive number
#'
#' @param value Numeric value.
#' @param name Argument name.
#' @param allow_zero Default `FALSE`. Whether zero is allowed.
#'
#' @return Numeric value.
#' @keywords internal
validate_waterpath_positive_number = function(
  value,
  name,
  allow_zero = FALSE
) {
  value = suppressWarnings(as.numeric(value))
  valid = length(value) == 1 &&
    is.finite(value) &&
    if (allow_zero) value >= 0 else value > 0
  if (!valid) {
    stop(
      sprintf(
        "`%s` must be a single %s number.",
        name,
        if (allow_zero) "non-negative" else "positive"
      ),
      call. = FALSE
    )
  }
  value
}

#' Resolve water path offset
#'
#' @param value Default `NULL`. Requested offset in elevation units.
#' @param name Default `"offset"`. Argument name for error messages.
#'
#' @return Offset in elevation units.
#' @keywords internal
resolve_waterpath_offset = function(value = NULL, name = "offset") {
  if (!is.null(value)) {
    return(validate_waterpath_positive_number(
      value,
      name,
      allow_zero = TRUE
    ))
  }
  0
}

#' Water path profile height ratio
#'
#' @return Height-to-width ratio for high-quality rectangular stream meshes.
#' @keywords internal
waterpath_profile_height_ratio = function() {
  0.2
}

#' Validate water path logical value
#'
#' @param value Logical-like value.
#' @param name Argument name.
#'
#' @return Logical value.
#' @keywords internal
validate_waterpath_logical = function(value, name) {
  value = suppressWarnings(as.logical(value))
  if (!length(value) || is.na(value[1])) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  value[1]
}

#' Prepare render water path geometry
#'
#' @param waterpaths Spatial line input.
#' @param waterpath_merge Whether to merge connected linework.
#'
#' @return Spatial line input.
#' @keywords internal
prepare_render_water_path_geometry = function(
  waterpaths,
  waterpath_merge = TRUE
) {
  if (
    !inherits(
      waterpaths,
      c("sf", "sfc", "sfg", "SpatialLines", "SpatialLinesDataFrame")
    )
  ) {
    return(waterpaths)
  }
  if (inherits(waterpaths, c("SpatialLines", "SpatialLinesDataFrame"))) {
    waterpaths = sf::st_as_sf(waterpaths)
  }
  if (inherits(waterpaths, "sfg")) {
    waterpaths = sf::st_sfc(waterpaths)
  }
  waterpaths = coerce_render_path_line_geometry(waterpaths)
  if (!isTRUE(waterpath_merge) || is_empty_scene_sf(waterpaths)) {
    return(waterpaths)
  }
  geometry = if (inherits(waterpaths, "sf")) {
    sf::st_geometry(waterpaths)
  } else {
    waterpaths
  }
  merged_geometry = tryCatch(
    suppressWarnings(sf::st_line_merge(sf::st_union(geometry))),
    error = function(e) geometry
  )
  coerce_render_path_line_geometry(merged_geometry)
}

#' Densify water path coordinates
#'
#' @param coord_list List of scene coordinate matrices.
#' @param heightmap Heightmap matrix.
#' @param zscale Effective zscale.
#' @param offset Centerline offset in elevation units.
#'
#' @return List of densified coordinate matrices.
#' @keywords internal
densify_water_path_coords = function(
  coord_list,
  heightmap,
  zscale,
  offset
) {
  heightmap_scene = heightmap / zscale
  offset_scene = offset / zscale
  lapply(coord_list, function(coords) {
    densify_single_water_path_coord(
      coords = coords,
      heightmap = heightmap_scene,
      offset = offset_scene
    )
  })
}

#' Calculate water path segment sample positions
#'
#' @param heightmap Heightmap matrix scaled into scene units.
#' @param segment_start Two-value segment start coordinate.
#' @param segment_end Two-value segment end coordinate.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
calculate_water_path_segment_t = function(
  heightmap,
  segment_start,
  segment_end
) {
  calculate_water_path_triangle_boundary_t(
    heightmap = heightmap,
    segment_start = segment_start,
    segment_end = segment_end
  )
}

#' Calculate water path terrain triangle boundary positions
#'
#' @param heightmap Heightmap matrix scaled into scene units.
#' @param segment_start Two-value segment start coordinate.
#' @param segment_end Two-value segment end coordinate.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
calculate_water_path_triangle_boundary_t = function(
  heightmap,
  segment_start,
  segment_end
) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  if (nr < 2 || nc < 2) {
    return(c(0, 1))
  }
  start_row_col = spatial_water_row_col(
    heightmap,
    segment_start[[1]],
    segment_start[[2]],
    clamp = FALSE
  )
  end_row_col = spatial_water_row_col(
    heightmap,
    segment_end[[1]],
    segment_end[[2]],
    clamp = FALSE
  )
  row0 = start_row_col$row
  row1 = end_row_col$row
  col0 = start_row_col$col
  col1 = end_row_col$col
  grid_t = unique_water_path_t(c(
    0,
    1,
    calculate_water_path_axis_boundary_t(row0, row1, 1, nr),
    calculate_water_path_axis_boundary_t(col0, col1, 1, nc)
  ))
  diagonal_t = calculate_water_path_diagonal_boundary_t(
    row0 = row0,
    row1 = row1,
    col0 = col0,
    col1 = col1,
    grid_t = grid_t,
    nr = nr,
    nc = nc
  )
  unique_water_path_t(c(grid_t, diagonal_t))
}

#' Calculate water path axis boundary positions
#'
#' @param start Axis start coordinate.
#' @param end Axis end coordinate.
#' @param lower Lower axis boundary.
#' @param upper Upper axis boundary.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
calculate_water_path_axis_boundary_t = function(start, end, lower, upper) {
  delta = end - start
  eps = sqrt(.Machine$double.eps)
  if (!is.finite(delta) || abs(delta) <= eps) {
    return(numeric(0))
  }
  boundary_min = max(lower, ceiling(min(start, end)))
  boundary_max = min(upper, floor(max(start, end)))
  if (boundary_min > boundary_max) {
    return(numeric(0))
  }
  boundaries = seq(boundary_min, boundary_max)
  boundaries = boundaries[
    boundaries > min(start, end) + eps &
      boundaries < max(start, end) - eps
  ]
  (boundaries - start) / delta
}

#' Calculate water path terrain diagonal boundary positions
#'
#' @param row0 Segment start row coordinate.
#' @param row1 Segment end row coordinate.
#' @param col0 Segment start column coordinate.
#' @param col1 Segment end column coordinate.
#' @param grid_t Segment positions already split at grid boundaries.
#' @param nr Heightmap row count.
#' @param nc Heightmap column count.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
calculate_water_path_diagonal_boundary_t = function(
  row0,
  row1,
  col0,
  col1,
  grid_t,
  nr,
  nc
) {
  eps = sqrt(.Machine$double.eps)
  row_delta = row1 - row0
  col_delta = col1 - col0
  diagonal_delta = row_delta + col_delta
  if (!is.finite(diagonal_delta) || abs(diagonal_delta) <= eps) {
    return(numeric(0))
  }
  diagonal_t = numeric(0)
  for (index in seq_len(length(grid_t) - 1L)) {
    interval_start = grid_t[[index]]
    interval_end = grid_t[[index + 1L]]
    if (interval_end - interval_start <= eps) {
      next
    }
    interval_mid = (interval_start + interval_end) / 2
    row_mid = row0 + row_delta * interval_mid
    col_mid = col0 + col_delta * interval_mid
    if (row_mid < 1 || row_mid > nr || col_mid < 1 || col_mid > nc) {
      next
    }
    row_cell = pmin(pmax(floor(row_mid), 1), nr - 1)
    col_cell = pmin(pmax(floor(col_mid), 1), nc - 1)
    target_sum = row_cell + col_cell + 1
    crossing_t = (target_sum - row0 - col0) / diagonal_delta
    if (
      is.finite(crossing_t) &&
        crossing_t > interval_start + eps &&
        crossing_t < interval_end - eps
    ) {
      diagonal_t = c(diagonal_t, crossing_t)
    }
  }
  diagonal_t
}

#' Return sorted unique water path segment positions
#'
#' @param t_values Segment interpolation values.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
unique_water_path_t = function(t_values) {
  eps = sqrt(.Machine$double.eps)
  t_values = t_values[
    is.finite(t_values) &
      t_values >= -eps &
      t_values <= 1 + eps
  ]
  t_values = pmin(pmax(t_values, 0), 1)
  sort(unique(round(t_values, 12)))
}

#' Offset water path coordinates
#'
#' @param coord_list List of scene coordinate matrices.
#' @param offset Vertical offset in scene units.
#'
#' @return List of coordinate matrices.
#' @keywords internal
offset_water_path_coords = function(coord_list, offset) {
  lapply(coord_list, function(coords) {
    coords = as.matrix(coords)
    if (nrow(coords) > 0 && ncol(coords) >= 2) {
      coords[, 2] = coords[, 2] + offset
    }
    coords
  })
}

#' Densify one water path coordinate matrix
#'
#' @param coords Scene coordinate matrix.
#' @param heightmap Heightmap matrix scaled into scene units.
#' @param offset Centerline offset in scene units.
#'
#' @return Densified coordinate matrix.
#' @keywords internal
densify_single_water_path_coord = function(
  coords,
  heightmap,
  offset
) {
  coords = as.matrix(coords)
  coords = coords[
    stats::complete.cases(coords[, c(1, 3), drop = FALSE]),
    ,
    drop = FALSE
  ]
  if (nrow(coords) < 2) {
    return(coords)
  }
  segment_count = nrow(coords) - 1L
  segment_t_values = vector("list", segment_count)
  point_counts = integer(segment_count)
  for (index in seq_len(segment_count)) {
    segment_t = calculate_water_path_segment_t(
      heightmap = heightmap,
      segment_start = coords[index, c(1, 3)],
      segment_end = coords[index + 1L, c(1, 3)]
    )
    if (index > 1L) {
      segment_t = segment_t[-1L]
    }
    segment_t_values[[index]] = segment_t
    point_counts[[index]] = length(segment_t)
  }
  x_vals = numeric(sum(point_counts))
  z_vals = numeric(sum(point_counts))
  position = 1L
  for (index in seq_len(segment_count)) {
    segment_start = coords[index, c(1, 3)]
    segment_end = coords[index + 1L, c(1, 3)]
    segment_t = segment_t_values[[index]]
    next_position = position + length(segment_t) - 1L
    fill_indices = seq.int(position, next_position)
    x_vals[fill_indices] = segment_start[[1]] +
      (segment_end[[1]] - segment_start[[1]]) * segment_t
    z_vals[fill_indices] = segment_start[[2]] +
      (segment_end[[2]] - segment_start[[2]]) * segment_t
    position = next_position + 1L
  }
  y_vals = interpolate_spatial_water_height(heightmap, x_vals, z_vals)
  if (any(!is.finite(y_vals))) {
    y_vals[!is.finite(y_vals)] = min(heightmap, na.rm = TRUE)
  }
  cbind(x_vals, y_vals + offset, z_vals)
}

#' Collapse duplicated path vertices
#'
#' @param vertices Path vertex matrix.
#'
#' @return Path vertex matrix with consecutive duplicated vertices removed.
#' @keywords internal
collapse_render_highquality_path_vertices = function(vertices) {
  vertices = as.matrix(vertices)
  if (nrow(vertices) < 2) {
    return(vertices)
  }
  finite_rows = stats::complete.cases(vertices)
  vertices = vertices[finite_rows, , drop = FALSE]
  if (nrow(vertices) < 2) {
    return(vertices)
  }
  step_distance = sqrt(rowSums(
    (vertices[-1, , drop = FALSE] -
      vertices[-nrow(vertices), , drop = FALSE])^2
  ))
  keep = c(TRUE, step_distance > sqrt(.Machine$double.eps))
  vertices[keep, , drop = FALSE]
}

#' Make water path extrusion profile
#'
#' @return Two-column matrix defining a shallow rectangular extrusion profile.
#' @keywords internal
make_render_highquality_water_path_polygon = function() {
  height_ratio = waterpath_profile_height_ratio()
  matrix(
    c(
      -0.5,
      -height_ratio / 2,
      0.5,
      -height_ratio / 2,
      0.5,
      height_ratio / 2,
      -0.5,
      height_ratio / 2
    ),
    ncol = 2,
    byrow = TRUE
  )
}

#' Resolve render_highquality water path surface
#'
#' @return List containing the cached heightmap in scene units and effective zscale.
#' @keywords internal
resolve_render_highquality_water_path_surface = function() {
  heightmap = tryCatch(
    resolve_scene_render_heightmap(caller = "render_highquality"),
    error = function(e) NULL
  )
  if (!is.matrix(heightmap)) {
    heightmap = NULL
  }
  zscale = tryCatch(
    resolve_render_highquality_camera_zscale(),
    error = function(e) 1
  )
  scale_render_highquality_water_path_heightmap(
    heightmap = heightmap,
    zscale = zscale
  )
}

#' Scale water path heightmap to scene units
#'
#' @param heightmap Default `NULL`. Cached heightmap matrix.
#' @param zscale Effective zscale.
#'
#' @return List containing heightmap in scene units and zscale.
#' @keywords internal
scale_render_highquality_water_path_heightmap = function(
  heightmap = NULL,
  zscale = 1
) {
  zscale = suppressWarnings(as.numeric(zscale[1]))
  if (!is.finite(zscale) || zscale <= 0) {
    zscale = 1
  }
  if (is.null(heightmap) || !is.matrix(heightmap)) {
    return(list(heightmap = NULL, zscale = zscale))
  }
  if (abs(zscale - 1) <= sqrt(.Machine$double.eps)) {
    return(list(heightmap = heightmap, zscale = 1))
  }
  list(heightmap = heightmap / zscale, zscale = 1)
}

#' Make render_highquality water path meshes
#'
#' @param tasks Water path mesh task list.
#'
#' @return List of rayrender mesh objects.
#' @keywords internal
make_render_highquality_water_path_meshes = function(tasks) {
  if (!length(tasks)) {
    return(list())
  }
  meshes = vector("list", length(tasks))
  for (index in seq_along(tasks)) {
    meshes[[index]] = do.call(
      make_render_highquality_water_path_mesh,
      tasks[[index]]
    )
  }
  Filter(Negate(is.null), meshes)
}

#' Make water path edge centers
#'
#' @param points Path center points.
#' @param side_vectors Unit side vectors.
#' @param half_width Half stream width.
#' @param heightmap Default `NULL`. Cached heightmap matrix.
#' @param zscale Effective zscale.
#'
#' @return List with left and right edge center matrices.
#' @keywords internal
make_render_highquality_water_path_edge_centers = function(
  points,
  side_vectors,
  half_width,
  heightmap = NULL,
  zscale = 1
) {
  left_center = points + side_vectors * half_width
  right_center = points - side_vectors * half_width
  heightmap_scene = scale_render_highquality_water_path_heightmap(
    heightmap = heightmap,
    zscale = zscale
  )$heightmap
  if (is.null(heightmap_scene) || !is.matrix(heightmap_scene)) {
    return(list(left = left_center, right = right_center))
  }
  center_height = interpolate_spatial_water_height(
    heightmap_scene,
    points[, 1],
    points[, 3]
  )
  center_offset = points[, 2] - center_height
  left_center[, 2] = interpolate_spatial_water_height(
    heightmap_scene,
    left_center[, 1],
    left_center[, 3]
  ) +
    center_offset
  right_center[, 2] = interpolate_spatial_water_height(
    heightmap_scene,
    right_center[, 1],
    right_center[, 3]
  ) +
    center_offset
  list(left = left_center, right = right_center)
}

#' Densify render_highquality water path points at terrain triangle edges
#'
#' @param points Path center points in rgl scene coordinates.
#' @param width Stream width.
#' @param heightmap Default `NULL`. Cached heightmap matrix.
#' @param zscale Effective zscale.
#'
#' @return Path center points with additional terrain triangle edge samples.
#' @keywords internal
densify_render_highquality_water_path_points = function(
  points,
  width,
  heightmap = NULL,
  zscale = 1
) {
  points = as.matrix(points)
  if (nrow(points) < 2 || is.null(heightmap) || !is.matrix(heightmap)) {
    return(points)
  }
  heightmap_scene = scale_render_highquality_water_path_heightmap(
    heightmap = heightmap,
    zscale = zscale
  )$heightmap
  if (is.null(heightmap_scene) || !is.matrix(heightmap_scene)) {
    return(points)
  }
  center_height = interpolate_spatial_water_height(
    heightmap_scene,
    points[, 1],
    points[, 3]
  )
  center_offset = points[, 2] - center_height
  normals = interpolate_render_highquality_water_path_normals(
    points = points,
    heightmap = heightmap_scene,
    zscale = 1
  )
  tangents = calculate_render_highquality_water_path_tangents(
    points = points,
    normals = normals
  )
  side_vectors = normalize_render_highquality_rows(row_cross(tangents, normals))
  side_vectors = replace_invalid_render_highquality_vectors(
    side_vectors,
    fallback = c(0, 0, 1)
  )
  edge_centers = make_render_highquality_water_path_edge_centers(
    points = points,
    side_vectors = side_vectors,
    half_width = width / 2,
    heightmap = heightmap_scene,
    zscale = 1
  )
  segment_count = nrow(points) - 1L
  segment_t_values = vector("list", segment_count)
  point_counts = integer(segment_count)
  for (index in seq_len(segment_count)) {
    segment_t = unique_water_path_t(c(
      calculate_water_path_triangle_boundary_t(
        heightmap = heightmap_scene,
        segment_start = points[index, c(1, 3)],
        segment_end = points[index + 1L, c(1, 3)]
      ),
      calculate_water_path_triangle_boundary_t(
        heightmap = heightmap_scene,
        segment_start = edge_centers$left[index, c(1, 3)],
        segment_end = edge_centers$left[index + 1L, c(1, 3)]
      ),
      calculate_water_path_triangle_boundary_t(
        heightmap = heightmap_scene,
        segment_start = edge_centers$right[index, c(1, 3)],
        segment_end = edge_centers$right[index + 1L, c(1, 3)]
      )
    ))
    if (index > 1L) {
      segment_t = segment_t[-1L]
    }
    segment_t_values[[index]] = segment_t
    point_counts[[index]] = length(segment_t)
  }
  x_vals = numeric(sum(point_counts))
  z_vals = numeric(sum(point_counts))
  offset_vals = numeric(sum(point_counts))
  position = 1L
  for (index in seq_len(segment_count)) {
    segment_t = segment_t_values[[index]]
    next_position = position + length(segment_t) - 1L
    fill_indices = seq.int(position, next_position)
    x_vals[fill_indices] = points[index, 1] +
      (points[index + 1L, 1] - points[index, 1]) * segment_t
    z_vals[fill_indices] = points[index, 3] +
      (points[index + 1L, 3] - points[index, 3]) * segment_t
    offset_vals[fill_indices] = center_offset[[index]] +
      (center_offset[[index + 1L]] - center_offset[[index]]) * segment_t
    position = next_position + 1L
  }
  y_vals = interpolate_spatial_water_height(heightmap_scene, x_vals, z_vals)
  cbind(x_vals, y_vals + offset_vals, z_vals)
}

#' Make render_highquality water path mesh
#'
#' @param points Path points in rgl scene coordinates.
#' @param bbox_center Scene bounding box center.
#' @param width Stream width.
#' @param heightmap Default `NULL`. Cached heightmap matrix.
#' @param zscale Effective zscale.
#' @param material Rayrender material.
#' @param segment_start Default `1`. First segment index to emit.
#' @param segment_end Default `NULL`. Last segment index to emit.
#' @param cap_start Default `TRUE`. Whether to cap the first emitted segment.
#' @param cap_end Default `TRUE`. Whether to cap the last emitted segment.
#'
#' @return Rayrender mesh object.
#' @keywords internal
make_render_highquality_water_path_mesh = function(
  points,
  bbox_center,
  width,
  heightmap = NULL,
  zscale = 1,
  material,
  segment_start = 1L,
  segment_end = NULL,
  cap_start = TRUE,
  cap_end = TRUE
) {
  points = as.matrix(points)
  if (is.null(segment_end)) {
    points = points[stats::complete.cases(points), , drop = FALSE]
  } else if (any(!stats::complete.cases(points))) {
    return(NULL)
  }
  if (nrow(points) < 2) {
    return(NULL)
  }
  if (is.null(segment_end)) {
    points = densify_render_highquality_water_path_points(
      points = points,
      width = width,
      heightmap = heightmap,
      zscale = zscale
    )
    if (nrow(points) < 2) {
      return(NULL)
    }
  }
  if (is.null(segment_end)) {
    segment_end = nrow(points) - 1L
  }
  segment_start = suppressWarnings(as.integer(segment_start[1]))
  segment_end = suppressWarnings(as.integer(segment_end[1]))
  if (
    !is.finite(segment_start) ||
      !is.finite(segment_end) ||
      segment_start < 1L ||
      segment_end < segment_start
  ) {
    return(NULL)
  }
  segment_end = min(segment_end, nrow(points) - 1L)
  if (segment_end < segment_start) {
    return(NULL)
  }
  height_ratio = diff(range(make_render_highquality_water_path_polygon()[, 2]))
  half_width = width / 2
  half_thickness = width * height_ratio / 2
  normals = interpolate_render_highquality_water_path_normals(
    points = points,
    heightmap = heightmap,
    zscale = zscale
  )
  tangents = calculate_render_highquality_water_path_tangents(
    points = points,
    normals = normals
  )
  side_vectors = normalize_render_highquality_rows(row_cross(tangents, normals))
  side_vectors = replace_invalid_render_highquality_vectors(
    side_vectors,
    fallback = c(0, 0, 1)
  )

  edge_centers = make_render_highquality_water_path_edge_centers(
    points = points,
    side_vectors = side_vectors,
    half_width = half_width,
    heightmap = heightmap,
    zscale = zscale
  )
  left_center = edge_centers$left
  right_center = edge_centers$right
  left_normals = interpolate_render_highquality_water_path_normals(
    points = left_center,
    heightmap = heightmap,
    zscale = zscale
  )
  right_normals = interpolate_render_highquality_water_path_normals(
    points = right_center,
    heightmap = heightmap,
    zscale = zscale
  )

  left_top = left_center + left_normals * half_thickness
  right_top = right_center + right_normals * half_thickness
  left_bottom = left_center - left_normals * half_thickness
  right_bottom = right_center - right_normals * half_thickness

  segment_indices = seq.int(segment_start, segment_end)
  next_indices = segment_indices + 1L
  vertices = rbind(
    make_render_highquality_water_path_quad_rows(
      left_top[segment_indices, , drop = FALSE],
      left_top[next_indices, , drop = FALSE],
      right_top[next_indices, , drop = FALSE],
      right_top[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      left_bottom[segment_indices, , drop = FALSE],
      right_bottom[segment_indices, , drop = FALSE],
      right_bottom[next_indices, , drop = FALSE],
      left_bottom[next_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      left_bottom[segment_indices, , drop = FALSE],
      left_bottom[next_indices, , drop = FALSE],
      left_top[next_indices, , drop = FALSE],
      left_top[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      right_bottom[segment_indices, , drop = FALSE],
      right_top[segment_indices, , drop = FALSE],
      right_top[next_indices, , drop = FALSE],
      right_bottom[next_indices, , drop = FALSE]
    )
  )
  vertex_normals = rbind(
    make_render_highquality_water_path_quad_rows(
      left_normals[segment_indices, , drop = FALSE],
      left_normals[next_indices, , drop = FALSE],
      right_normals[next_indices, , drop = FALSE],
      right_normals[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      -left_normals[segment_indices, , drop = FALSE],
      -right_normals[segment_indices, , drop = FALSE],
      -right_normals[next_indices, , drop = FALSE],
      -left_normals[next_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      side_vectors[segment_indices, , drop = FALSE],
      side_vectors[next_indices, , drop = FALSE],
      side_vectors[next_indices, , drop = FALSE],
      side_vectors[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      -side_vectors[segment_indices, , drop = FALSE],
      -side_vectors[segment_indices, , drop = FALSE],
      -side_vectors[next_indices, , drop = FALSE],
      -side_vectors[next_indices, , drop = FALSE]
    )
  )
  if (isTRUE(cap_start)) {
    vertices = rbind(
      vertices,
      make_render_highquality_water_path_quad_rows(
        matrix(left_bottom[segment_start, ], nrow = 1L),
        matrix(left_top[segment_start, ], nrow = 1L),
        matrix(right_top[segment_start, ], nrow = 1L),
        matrix(right_bottom[segment_start, ], nrow = 1L)
      )
    )
    vertex_normals = rbind(
      vertex_normals,
      make_render_highquality_water_path_quad_rows(
        matrix(-tangents[segment_start, ], nrow = 1L),
        matrix(-tangents[segment_start, ], nrow = 1L),
        matrix(-tangents[segment_start, ], nrow = 1L),
        matrix(-tangents[segment_start, ], nrow = 1L)
      )
    )
  }
  if (isTRUE(cap_end)) {
    end_index = segment_end + 1L
    vertices = rbind(
      vertices,
      make_render_highquality_water_path_quad_rows(
        matrix(left_bottom[end_index, ], nrow = 1L),
        matrix(right_bottom[end_index, ], nrow = 1L),
        matrix(right_top[end_index, ], nrow = 1L),
        matrix(left_top[end_index, ], nrow = 1L)
      )
    )
    vertex_normals = rbind(
      vertex_normals,
      make_render_highquality_water_path_quad_rows(
        matrix(tangents[end_index, ], nrow = 1L),
        matrix(tangents[end_index, ], nrow = 1L),
        matrix(tangents[end_index, ], nrow = 1L),
        matrix(tangents[end_index, ], nrow = 1L)
      )
    )
  }

  quad_starts = seq(1L, nrow(vertices), by = 4L)
  indices = rbind(
    cbind(quad_starts, quad_starts + 1L, quad_starts + 2L),
    cbind(quad_starts, quad_starts + 2L, quad_starts + 3L)
  )
  vertices = sweep(vertices, 2, bbox_center, FUN = "-")
  mesh = list(
    vb = t(cbind(vertices, 1)),
    it = t(indices),
    normals = t(vertex_normals)
  )
  class(mesh) = "mesh3d"
  rayrender::mesh3d_model(
    mesh,
    override_material = TRUE,
    material = material
  )
}

#' Make water path quad rows
#'
#' @param v1 First vertex.
#' @param v2 Second vertex.
#' @param v3 Third vertex.
#' @param v4 Fourth vertex.
#'
#' @return Matrix of interleaved quad rows.
#' @keywords internal
make_render_highquality_water_path_quad_rows = function(
  v1,
  v2,
  v3,
  v4
) {
  out = matrix(NA_real_, nrow = nrow(v1) * 4L, ncol = ncol(v1))
  out[seq(1L, nrow(out), by = 4L), ] = v1
  out[seq(2L, nrow(out), by = 4L), ] = v2
  out[seq(3L, nrow(out), by = 4L), ] = v3
  out[seq(4L, nrow(out), by = 4L), ] = v4
  out
}

#' Interpolate water path normals
#'
#' @param points Path points in rgl scene coordinates.
#' @param heightmap Default `NULL`. Cached heightmap matrix.
#' @param zscale Effective zscale.
#'
#' @return Matrix of normal vectors.
#' @keywords internal
interpolate_render_highquality_water_path_normals = function(
  points,
  heightmap = NULL,
  zscale = 1
) {
  fallback = matrix(
    c(0, 1, 0),
    nrow = nrow(points),
    ncol = 3,
    byrow = TRUE
  )
  if (is.null(heightmap) || !is.matrix(heightmap)) {
    return(fallback)
  }
  zscale = suppressWarnings(as.numeric(zscale[1]))
  if (!is.finite(zscale) || zscale <= 0) {
    zscale = 1
  }
  heightmap_scene = if (abs(zscale - 1) <= sqrt(.Machine$double.eps)) {
    heightmap
  } else {
    heightmap / zscale
  }
  x = points[, 1]
  z = points[, 3]
  dx = (interpolate_spatial_water_height(heightmap_scene, x + 1, z) -
    interpolate_spatial_water_height(heightmap_scene, x - 1, z)) /
    2
  dz = (interpolate_spatial_water_height(heightmap_scene, x, z + 1) -
    interpolate_spatial_water_height(heightmap_scene, x, z - 1)) /
    2
  normals = cbind(-dx, 1, -dz)
  normals = normalize_render_highquality_rows(normals)
  replace_invalid_render_highquality_vectors(normals, fallback = c(0, 1, 0))
}

#' Calculate water path tangents
#'
#' @param points Path points.
#' @param normals Path normals.
#'
#' @return Matrix of tangent vectors.
#' @keywords internal
calculate_render_highquality_water_path_tangents = function(points, normals) {
  tangents = matrix(0, nrow = nrow(points), ncol = 3)
  tangents[1L, ] = points[2L, ] - points[1L, ]
  tangents[nrow(points), ] = points[nrow(points), ] -
    points[nrow(points) - 1L, ]
  if (nrow(points) > 2) {
    for (index in seq(2L, nrow(points) - 1L)) {
      tangents[index, ] = points[index + 1L, ] - points[index - 1L, ]
    }
  }
  tangents = tangents - normals * rowSums(tangents * normals)
  tangents = normalize_render_highquality_rows(tangents)
  replace_invalid_render_highquality_vectors(tangents, fallback = c(1, 0, 0))
}

#' Normalize matrix rows
#'
#' @param values Numeric matrix.
#'
#' @return Matrix with unit-length rows.
#' @keywords internal
normalize_render_highquality_rows = function(values) {
  values = as.matrix(values)
  lengths = sqrt(rowSums(values^2))
  values / lengths
}

#' Replace invalid vectors
#'
#' @param values Numeric matrix.
#' @param fallback Fallback vector.
#'
#' @return Numeric matrix.
#' @keywords internal
replace_invalid_render_highquality_vectors = function(values, fallback) {
  invalid = !stats::complete.cases(values) |
    sqrt(rowSums(values^2)) < sqrt(.Machine$double.eps)
  if (any(invalid)) {
    values[invalid, ] = matrix(
      fallback,
      nrow = sum(invalid),
      ncol = length(fallback),
      byrow = TRUE
    )
  }
  values
}

#' Calculate row-wise cross products
#'
#' @param x First matrix.
#' @param y Second matrix.
#'
#' @return Matrix of row-wise cross products.
#' @keywords internal
row_cross = function(x, y) {
  cbind(
    x[, 2] * y[, 3] - x[, 3] * y[, 2],
    x[, 3] * y[, 1] - x[, 1] * y[, 3],
    x[, 1] * y[, 2] - x[, 2] * y[, 1]
  )
}

make_render_highquality_water_path_material = function(
  color,
  water_material,
  water_roughness,
  water_ior,
  water_attenuation,
  water_surface_color
) {
  if (is.null(color) || length(color) == 0) {
    color = "white"
  }
  surface_color = if (water_surface_color) {
    convert_color(color, as_hex = TRUE)
  } else {
    "white"
  }
  if (identical(water_material, "microfacet")) {
    return(rayrender::microfacet(
      color = surface_color,
      roughness = water_roughness,
      transmission = TRUE,
      eta = water_ior,
      kappa = water_attenuation
    ))
  }
  attenuation = (1 - convert_color(color)) * water_attenuation
  rayrender::dielectric(
    color = surface_color,
    refraction = water_ior,
    attenuation = attenuation
  )
}
