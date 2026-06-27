#'@title make_water
#'
#'@description Makes the water in the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param waterheight Default `0`. Water level. Either a scalar, a matrix with the same dimensions as `heightmap`, or a spatial raster that can be projected/resampled to the heightmap grid.
#'@param watercolor Default `blue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param water_render_method Default `"contour"`. Water meshing method. `"contour"` clips the water mesh to the flooded region; `"legacy"` uses the previous box/grid renderer.
#'@param water_edge_extension Default `0.5`. For spatial `waterheight` inputs, amount in grid cells to expand finite water cells at boundary edges, up to a maximum of half a cell.
#'@param heightmap_extent Default `NULL`. Active scene extent for spatial `waterheight` inputs.
#'@param heightmap_crs Default `NULL`. Active scene CRS for spatial `waterheight` inputs.
#'@keywords internal
make_water = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5,
  water_render_method = c("contour", "legacy"),
  water_edge_extension = 0.5,
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  water_render_method = match.arg(water_render_method)
  if (identical(water_render_method, "legacy")) {
    return(make_water_legacy(
      heightmap = heightmap,
      waterheight = waterheight,
      watercolor = watercolor,
      zscale = zscale,
      wateralpha = wateralpha
    ))
  }
  make_water_contour(
    heightmap = heightmap,
    waterheight = waterheight,
    watercolor = watercolor,
    zscale = zscale,
    wateralpha = wateralpha,
    water_edge_extension = water_edge_extension,
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs
  )
}

#'@keywords internal
make_water_contour = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5,
  water_edge_extension = 0.5,
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  heightmap = heightmap / zscale
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  waterheight_is_spatial = is_spatial_heightmap_input(waterheight)
  waterheight = normalize_waterheight_matrix(
    waterheight,
    nr = nr,
    nc = nc,
    zscale = zscale,
    caller = "make_water",
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs
  )
  valid_water = is.finite(heightmap) & is.finite(waterheight)
  if (waterheight_is_spatial) {
    return(make_spatial_water_surface(
      waterheight = waterheight,
      heightmap = heightmap,
      valid_water = valid_water,
      watercolor = watercolor,
      wateralpha = wateralpha,
      water_edge_extension = water_edge_extension
    ))
  }
  if (!any(valid_water)) {
    warning(
      "No water rendered--no finite heightmap and water level values overlap."
    )
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }
  flooded = valid_water & heightmap < waterheight
  if (!any(flooded)) {
    warning(format_no_water_warning(heightmap, waterheight, zscale))
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }

  water_mesh = make_water_mesh_cpp(heightmap, waterheight)
  vertices = water_mesh$vertices
  if (length(vertices) > 0) {
    for (component in vertices) {
      if (is.matrix(component) && nrow(component) > 0) {
        rgl::triangles3d(
          x = component,
          indices = seq_len(nrow(component)),
          color = watercolor,
          alpha = wateralpha,
          lit = FALSE,
          front = "filled",
          back = "filled",
          texture = NULL,
          tag = "water"
        )
      }
    }
  }
  invisible(water_mesh)
}

#'@keywords internal
make_spatial_water_surface = function(
  waterheight,
  heightmap = NULL,
  valid_water,
  watercolor = "lightblue",
  wateralpha = 0.5,
  water_edge_extension = 0.5
) {
  water_edge_extension = validate_water_edge_extension(water_edge_extension)
  water_surface = waterheight
  water_surface[!valid_water] = NA_real_
  if (!any(is.finite(water_surface))) {
    warning(
      "No water rendered--no finite heightmap and water level values overlap."
    )
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }
  triangle_vertices = make_spatial_water_cell_surface(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = water_edge_extension
  )
  if (!nrow(triangle_vertices)) {
    warning(
      "No water rendered--spatial `waterdepth` does not cover any renderable heightmap cells."
    )
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }
  rgl::triangles3d(
    x = triangle_vertices,
    indices = seq_len(nrow(triangle_vertices)),
    color = watercolor,
    alpha = wateralpha,
    lit = FALSE,
    front = "filled",
    back = "filled",
    polygon_offset = -1,
    texture = NULL,
    tag = "water"
  )
  invisible(list(
    vertices = list(triangle_vertices),
    lines = matrix(nrow = 0, ncol = 3)
  ))
}

#'@keywords internal
validate_water_edge_extension = function(water_edge_extension = 0.5) {
  if (
    !is.numeric(water_edge_extension) ||
      length(water_edge_extension) != 1 ||
      is.na(water_edge_extension) ||
      water_edge_extension < 0 ||
      water_edge_extension > 0.5
  ) {
    stop(
      "`water_edge_extension` must be a single number between 0 and 0.5.",
      call. = FALSE
    )
  }
  water_edge_extension
}

#'@keywords internal
make_spatial_water_cell_surface = function(
  water_surface,
  heightmap = NULL,
  water_edge_extension = 0.5
) {
  water_edge_extension = validate_water_edge_extension(water_edge_extension)
  finite_cells = which(is.finite(water_surface), arr.ind = TRUE)
  if (!nrow(finite_cells)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  nr = nrow(water_surface)
  nc = ncol(water_surface)
  row_index = finite_cells[, 1]
  col_index = finite_cells[, 2]
  water_height = water_surface[finite_cells]
  water_mask = is.finite(water_surface)

  x_scene_min = -(nr - 1) / 2
  x_scene_max = (nr - 1) / 2
  z_scene_min = -(nc - 1) / 2
  z_scene_max = (nc - 1) / 2
  x_center = row_index - 1 - (nr - 1) / 2
  z_center = col_index - 1 - (nc - 1) / 2
  left_edge = row_index == 1L
  right_edge = row_index == nr
  bottom_edge = col_index == 1L
  top_edge = col_index == nc
  interior_left = row_index > 1L
  interior_right = row_index < nr
  interior_bottom = col_index > 1L
  interior_top = col_index < nc
  left_edge[interior_left] =
    !water_mask[cbind(row_index[interior_left] - 1L, col_index[interior_left])]
  right_edge[interior_right] =
    !water_mask[cbind(
      row_index[interior_right] + 1L,
      col_index[interior_right]
    )]
  bottom_edge[interior_bottom] =
    !water_mask[cbind(
      row_index[interior_bottom],
      col_index[interior_bottom] - 1L
    )]
  top_edge[interior_top] =
    !water_mask[cbind(row_index[interior_top], col_index[interior_top] + 1L)]

  x0 = pmax(
    x_center - 0.5 - ifelse(left_edge, water_edge_extension, 0),
    x_scene_min
  )
  x1 = pmin(
    x_center + 0.5 + ifelse(right_edge, water_edge_extension, 0),
    x_scene_max
  )
  z0 = pmax(
    z_center - 0.5 - ifelse(bottom_edge, water_edge_extension, 0),
    z_scene_min
  )
  z1 = pmin(
    z_center + 0.5 + ifelse(top_edge, water_edge_extension, 0),
    z_scene_max
  )

  renderable = x1 > x0 & z1 > z0
  if (!any(renderable)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  x0 = x0[renderable]
  x1 = x1[renderable]
  z0 = z0[renderable]
  z1 = z1[renderable]
  row_index = row_index[renderable]
  col_index = col_index[renderable]
  water_height = water_height[renderable]
  left_edge = left_edge[renderable]
  right_edge = right_edge[renderable]
  bottom_edge = bottom_edge[renderable]
  top_edge = top_edge[renderable]

  n_cells = length(water_height)
  top_vertices = matrix(NA_real_, nrow = n_cells * 6L, ncol = 3L)
  first_vertex = seq.int(1L, nrow(top_vertices), by = 6L)
  top_vertices[first_vertex, ] = cbind(x0, water_height, z0)
  top_vertices[first_vertex + 1L, ] = cbind(x1, water_height, z0)
  top_vertices[first_vertex + 2L, ] = cbind(x0, water_height, z1)
  top_vertices[first_vertex + 3L, ] = cbind(x1, water_height, z0)
  top_vertices[first_vertex + 4L, ] = cbind(x1, water_height, z1)
  top_vertices[first_vertex + 5L, ] = cbind(x0, water_height, z1)

  side_vertices = make_spatial_water_edge_sides(
    heightmap = heightmap,
    water_height = water_height,
    x0 = x0,
    x1 = x1,
    z0 = z0,
    z1 = z1,
    left_edge = left_edge,
    right_edge = right_edge,
    bottom_edge = bottom_edge,
    top_edge = top_edge,
    row_index = row_index,
    col_index = col_index,
    nr = nr,
    nc = nc
  )
  rbind(top_vertices, side_vertices)
}

#'@keywords internal
make_spatial_water_edge_sides = function(
  heightmap = NULL,
  water_height,
  x0,
  x1,
  z0,
  z1,
  left_edge,
  right_edge,
  bottom_edge,
  top_edge,
  row_index,
  col_index,
  nr,
  nc
) {
  if (is.null(heightmap) || !is.matrix(heightmap)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  cell_index = matrix(0L, nrow = nr, ncol = nc)
  cell_index[cbind(row_index, col_index)] = seq_along(row_index)
  rbind(
    make_spatial_water_side_vertices(
      side = "left",
      edge_mask = left_edge,
      x_start = x0,
      z_start = z0,
      x_end = x0,
      z_end = z1,
      water_height = water_height,
      heightmap = heightmap,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index,
      all_x0 = x0,
      all_x1 = x1,
      all_z0 = z0,
      all_z1 = z1
    ),
    make_spatial_water_side_vertices(
      side = "right",
      edge_mask = right_edge,
      x_start = x1,
      z_start = z1,
      x_end = x1,
      z_end = z0,
      water_height = water_height,
      heightmap = heightmap,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index,
      all_x0 = x0,
      all_x1 = x1,
      all_z0 = z0,
      all_z1 = z1
    ),
    make_spatial_water_side_vertices(
      side = "bottom",
      edge_mask = bottom_edge,
      x_start = x1,
      z_start = z0,
      x_end = x0,
      z_end = z0,
      water_height = water_height,
      heightmap = heightmap,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index,
      all_x0 = x0,
      all_x1 = x1,
      all_z0 = z0,
      all_z1 = z1
    ),
    make_spatial_water_side_vertices(
      side = "top",
      edge_mask = top_edge,
      x_start = x0,
      z_start = z1,
      x_end = x1,
      z_end = z1,
      water_height = water_height,
      heightmap = heightmap,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index,
      all_x0 = x0,
      all_x1 = x1,
      all_z0 = z0,
      all_z1 = z1
    )
  )
}

#'@keywords internal
make_spatial_water_side_vertices = function(
  side,
  edge_mask,
  x_start,
  z_start,
  x_end,
  z_end,
  water_height,
  heightmap,
  row_index,
  col_index,
  cell_index,
  all_x0,
  all_x1,
  all_z0,
  all_z1
) {
  if (!any(edge_mask)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  edge_index = which(edge_mask)
  clipped_edges = clip_spatial_water_side_segments(
    side = side,
    edge_index = edge_index,
    x_start = x_start,
    z_start = z_start,
    x_end = x_end,
    z_end = z_end,
    water_height = water_height,
    row_index = row_index,
    col_index = col_index,
    cell_index = cell_index,
    all_x0 = all_x0,
    all_x1 = all_x1,
    all_z0 = all_z0,
    all_z1 = all_z1
  )
  if (!length(clipped_edges$water_height)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  x_start = clipped_edges$x_start
  z_start = clipped_edges$z_start
  x_end = clipped_edges$x_end
  z_end = clipped_edges$z_end
  water_height = clipped_edges$water_height
  terrain_start = interpolate_spatial_water_height(heightmap, x_start, z_start)
  terrain_end = interpolate_spatial_water_height(heightmap, x_end, z_end)
  bottom_start = pmin(terrain_start, water_height)
  bottom_end = pmin(terrain_end, water_height)
  renderable = is.finite(bottom_start) &
    is.finite(bottom_end) &
    (water_height > bottom_start | water_height > bottom_end)
  if (!any(renderable)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  x_start = x_start[renderable]
  z_start = z_start[renderable]
  x_end = x_end[renderable]
  z_end = z_end[renderable]
  water_height = water_height[renderable]
  bottom_start = bottom_start[renderable]
  bottom_end = bottom_end[renderable]

  n_edges = length(water_height)
  vertices = matrix(NA_real_, nrow = n_edges * 6L, ncol = 3L)
  first_vertex = seq.int(1L, nrow(vertices), by = 6L)
  vertices[first_vertex, ] = cbind(x_start, water_height, z_start)
  vertices[first_vertex + 1L, ] = cbind(x_end, water_height, z_end)
  vertices[first_vertex + 2L, ] = cbind(x_start, bottom_start, z_start)
  vertices[first_vertex + 3L, ] = cbind(x_end, water_height, z_end)
  vertices[first_vertex + 4L, ] = cbind(x_end, bottom_end, z_end)
  vertices[first_vertex + 5L, ] = cbind(x_start, bottom_start, z_start)
  vertices
}

#'@keywords internal
clip_spatial_water_side_segments = function(
  side,
  edge_index,
  x_start,
  z_start,
  x_end,
  z_end,
  water_height,
  row_index,
  col_index,
  cell_index,
  all_x0,
  all_x1,
  all_z0,
  all_z1
) {
  segment_x_start = numeric()
  segment_z_start = numeric()
  segment_x_end = numeric()
  segment_z_end = numeric()
  segment_height = numeric()

  for (edge in edge_index) {
    covering_cells = get_spatial_water_covering_cells(
      edge = edge,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index
    )
    coverage = get_spatial_water_side_coverage(
      side = side,
      edge = edge,
      covering_cells = covering_cells,
      x_start = x_start,
      z_start = z_start,
      x_end = x_end,
      z_end = z_end,
      all_x0 = all_x0,
      all_x1 = all_x1,
      all_z0 = all_z0,
      all_z1 = all_z1
    )
    if (identical(side, "left") || identical(side, "right")) {
      uncovered = subtract_spatial_water_coverage(
        range(c(z_start[edge], z_end[edge])),
        coverage
      )
      if (!nrow(uncovered)) {
        next
      }
      reverse_segment = z_end[edge] < z_start[edge]
      segment_x_start = c(segment_x_start, rep(x_start[edge], nrow(uncovered)))
      segment_x_end = c(segment_x_end, rep(x_end[edge], nrow(uncovered)))
      segment_z_start = c(
        segment_z_start,
        if (reverse_segment) uncovered[, 2] else uncovered[, 1]
      )
      segment_z_end = c(
        segment_z_end,
        if (reverse_segment) uncovered[, 1] else uncovered[, 2]
      )
    } else {
      uncovered = subtract_spatial_water_coverage(
        range(c(x_start[edge], x_end[edge])),
        coverage
      )
      if (!nrow(uncovered)) {
        next
      }
      reverse_segment = x_end[edge] < x_start[edge]
      segment_x_start = c(
        segment_x_start,
        if (reverse_segment) uncovered[, 2] else uncovered[, 1]
      )
      segment_x_end = c(
        segment_x_end,
        if (reverse_segment) uncovered[, 1] else uncovered[, 2]
      )
      segment_z_start = c(segment_z_start, rep(z_start[edge], nrow(uncovered)))
      segment_z_end = c(segment_z_end, rep(z_end[edge], nrow(uncovered)))
    }
    segment_height = c(segment_height, rep(water_height[edge], nrow(uncovered)))
  }

  list(
    x_start = segment_x_start,
    z_start = segment_z_start,
    x_end = segment_x_end,
    z_end = segment_z_end,
    water_height = segment_height
  )
}

#'@keywords internal
get_spatial_water_covering_cells = function(
  edge,
  row_index,
  col_index,
  cell_index
) {
  row_range = seq.int(
    max(1L, row_index[edge] - 2L),
    min(nrow(cell_index), row_index[edge] + 2L)
  )
  col_range = seq.int(
    max(1L, col_index[edge] - 2L),
    min(ncol(cell_index), col_index[edge] + 2L)
  )
  covering_cells = as.integer(cell_index[row_range, col_range])
  unique(covering_cells[covering_cells > 0L & covering_cells != edge])
}

#'@keywords internal
get_spatial_water_side_coverage = function(
  side,
  edge,
  covering_cells,
  x_start,
  z_start,
  x_end,
  z_end,
  all_x0,
  all_x1,
  all_z0,
  all_z1
) {
  if (!length(covering_cells)) {
    return(matrix(nrow = 0, ncol = 2))
  }
  eps = sqrt(.Machine$double.eps)
  if (identical(side, "left") || identical(side, "right")) {
    edge_x = x_start[edge]
    edge_range = range(c(z_start[edge], z_end[edge]))
    spans_edge = if (identical(side, "left")) {
      all_x0[covering_cells] < edge_x - eps &
        all_x1[covering_cells] >= edge_x - eps
    } else {
      all_x0[covering_cells] <= edge_x + eps &
        all_x1[covering_cells] > edge_x + eps
    }
    coverage = cbind(
      pmax(all_z0[covering_cells], edge_range[1]),
      pmin(all_z1[covering_cells], edge_range[2])
    )
  } else {
    edge_z = z_start[edge]
    edge_range = range(c(x_start[edge], x_end[edge]))
    spans_edge = if (identical(side, "bottom")) {
      all_z0[covering_cells] < edge_z - eps &
        all_z1[covering_cells] >= edge_z - eps
    } else {
      all_z0[covering_cells] <= edge_z + eps &
        all_z1[covering_cells] > edge_z + eps
    }
    coverage = cbind(
      pmax(all_x0[covering_cells], edge_range[1]),
      pmin(all_x1[covering_cells], edge_range[2])
    )
  }
  coverage = coverage[spans_edge, , drop = FALSE]
  coverage[coverage[, 2] > coverage[, 1] + eps, , drop = FALSE]
}

#'@keywords internal
subtract_spatial_water_coverage = function(edge_range, coverage) {
  eps = sqrt(.Machine$double.eps)
  if (!nrow(coverage)) {
    return(matrix(edge_range, nrow = 1L))
  }
  coverage = coverage[order(coverage[, 1], coverage[, 2]), , drop = FALSE]
  uncovered = matrix(nrow = 0, ncol = 2)
  current_start = edge_range[1]
  for (coverage_index in seq_len(nrow(coverage))) {
    coverage_start = max(edge_range[1], coverage[coverage_index, 1])
    coverage_end = min(edge_range[2], coverage[coverage_index, 2])
    if (coverage_end <= current_start + eps) {
      next
    }
    if (coverage_start > current_start + eps) {
      uncovered = rbind(uncovered, c(current_start, coverage_start))
    }
    current_start = max(current_start, coverage_end)
    if (current_start >= edge_range[2] - eps) {
      break
    }
  }
  if (current_start < edge_range[2] - eps) {
    uncovered = rbind(uncovered, c(current_start, edge_range[2]))
  }
  uncovered
}

#'@keywords internal
interpolate_spatial_water_height = function(heightmap, x, z) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  row = pmin(pmax(x + (nr - 1) / 2 + 1, 1), nr)
  col = pmin(pmax(z + (nc - 1) / 2 + 1, 1), nc)
  row0 = floor(row)
  row1 = ceiling(row)
  col0 = floor(col)
  col1 = ceiling(col)
  row_weight = row - row0
  col_weight = col - col0

  height00 = heightmap[cbind(row0, col0)]
  height10 = heightmap[cbind(row1, col0)]
  height01 = heightmap[cbind(row0, col1)]
  height11 = heightmap[cbind(row1, col1)]
  interpolated =
    (1 - row_weight) *
    (1 - col_weight) *
    height00 +
    row_weight * (1 - col_weight) * height10 +
    (1 - row_weight) * col_weight * height01 +
    row_weight * col_weight * height11

  nearest_row = as.integer(round(row))
  nearest_col = as.integer(round(col))
  nearest_height = heightmap[cbind(nearest_row, nearest_col)]
  fallback = !is.finite(interpolated)
  interpolated[fallback] = nearest_height[fallback]
  interpolated
}

#'@keywords internal
make_water_legacy = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5
) {
  if (
    is.matrix(waterheight) ||
      is_spatial_heightmap_input(waterheight) ||
      length(waterheight) != 1
  ) {
    stop(
      "`water_render_method = \"legacy\"` only supports a scalar `waterdepth`.",
      call. = FALSE
    )
  }
  heightmap = heightmap / zscale
  na_matrix = is.na(heightmap)
  nr = nrow(heightmap)
  nc = ncol(heightmap)

  waterheight = waterheight / zscale
  if (all(heightmap >= waterheight, na.rm = TRUE)) {
    warning(
      "No water rendered--all elevations above or equal to water level. Range of heights: ",
      min(heightmap, na.rm = TRUE) * zscale,
      "-",
      max(heightmap, na.rm = TRUE) * zscale,
      ". Depth specified: ",
      waterheight * zscale
    )
  } else {
    heightlist = make_water_cpp(heightmap, na_matrix, waterheight)
    if (length(heightlist) > 0) {
      fullsides = do.call(rbind, heightlist)
      fullsides[, 3] = -fullsides[, 3]
      fullsides[, 1] = fullsides[, 1] - 1
      fullsides[, 3] = fullsides[, 3]

      fullsides[, 1] = fullsides[, 1] - (nr - 1) / 2
      fullsides[, 3] = fullsides[, 3] - (nc - 1) / 2
    }
    nr1 = nr - 1
    nc1 = nc - 1

    if (all(!na_matrix)) {
      vertices = rbind(
        matrix(
          c(
            -nr1 / 2,
            nr1 / 2,
            -nr1 / 2,
            waterheight,
            waterheight,
            waterheight,
            nc1 / 2,
            -nc1 / 2,
            -nc1 / 2
          ),
          nrow = 3L,
          ncol = 3L
        ),
        matrix(
          c(
            -nr1 / 2,
            nr1 / 2,
            nr1 / 2,
            waterheight,
            waterheight,
            waterheight,
            nc1 / 2,
            nc1 / 2,
            -nc1 / 2
          ),
          nrow = 3L,
          ncol = 3L
        )
      )
      indices = seq_len(6L)
      rgl::triangles3d(
        x = vertices,
        indices = indices,
        color = watercolor,
        alpha = wateralpha,
        lit = FALSE,
        front = "filled",
        back = "cull",
        texture = NULL,
        tag = "water"
      )
      if (length(heightlist) > 0) {
        indices = rev(seq_len(nrow(fullsides)))
        rgl::triangles3d(
          fullsides,
          indices = indices,
          lit = FALSE,
          color = watercolor,
          alpha = wateralpha,
          front = "filled",
          back = "cull",
          depth_test = "less",
          texture = NULL,
          tag = "water"
        )
      }
    } else {
      if (length(heightlist) > 0) {
        indices = rev(seq_len(nrow(fullsides)))
        rgl::triangles3d(
          fullsides,
          indices = indices,
          lit = FALSE,
          color = watercolor,
          alpha = wateralpha,
          front = "fill",
          back = "culled",
          texture = NULL,
          tag = "water"
        )
      }

      basemat = matrix(waterheight, nr, nc)
      basemat[is.na(heightmap)] = NA
      ray_surface = generate_surface(basemat, zscale = 1)

      rgl::triangles3d(
        x = ray_surface$verts,
        indices = ray_surface$inds,
        texcoords = ray_surface$texcoords,
        color = watercolor,
        alpha = wateralpha,
        back = "culled",
        front = "fill",
        lit = FALSE,
        texture = NULL,
        tag = "water"
      )
    }
  }
  invisible(NULL)
}

#'@keywords internal
normalize_waterheight_matrix = function(
  waterheight,
  nr,
  nc,
  zscale,
  caller,
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  if (is.matrix(waterheight)) {
    if (!is.numeric(waterheight)) {
      stop("`waterdepth` must be numeric.", call. = FALSE)
    }
    if (!all(dim(waterheight) == c(nr, nc))) {
      stop(
        sprintf(
          "`waterdepth` matrix must have dimensions %i x %i to match `heightmap`.",
          nr,
          nc
        ),
        call. = FALSE
      )
    }
    return(waterheight / zscale)
  }
  if (is_spatial_heightmap_input(waterheight)) {
    waterheight = resolve_spatial_waterheight_matrix(
      waterheight = waterheight,
      nr = nr,
      nc = nc,
      heightmap_extent = heightmap_extent,
      heightmap_crs = heightmap_crs,
      caller = caller
    )
    return(waterheight / zscale)
  }
  if (
    !is.numeric(waterheight) || length(waterheight) != 1 || is.na(waterheight)
  ) {
    stop(
      sprintf(
        "`waterdepth` must be a scalar, a matrix, or a spatial raster for %s().",
        caller
      ),
      call. = FALSE
    )
  }
  matrix(waterheight / zscale, nrow = nr, ncol = nc)
}

#'@keywords internal
resolve_spatial_waterheight_matrix = function(
  waterheight,
  nr,
  nc,
  heightmap_extent = NULL,
  heightmap_crs = NULL,
  caller = NULL
) {
  water_raster = coerce_spatial_waterheight_raster(waterheight)
  target_template = build_waterheight_template(
    nr = nr,
    nc = nc,
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs,
    caller = caller
  )
  source_crs = tryCatch(terra::crs(water_raster), error = function(e) "")
  target_crs = tryCatch(terra::crs(target_template), error = function(e) "")
  source_has_crs = is.character(source_crs) &&
    length(source_crs) &&
    nzchar(source_crs[1])
  target_has_crs = is.character(target_crs) &&
    length(target_crs) &&
    nzchar(target_crs[1])

  if (target_has_crs && !source_has_crs) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` inputs must have a CRS when the active heightmap has a CRS."
      ),
      call. = FALSE
    )
  }

  aligned_raster = tryCatch(
    {
      if (target_has_crs) {
        same_crs = isTRUE(tryCatch(
          scene_crs_equal(source_crs, target_crs),
          error = function(e) FALSE
        ))
        if (same_crs) {
          terra::resample(water_raster, target_template, method = "bilinear")
        } else {
          terra::project(water_raster, target_template, method = "bilinear")
        }
      } else {
        terra::resample(water_raster, target_template, method = "bilinear")
      }
    },
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not project/resample spatial `waterdepth` to the active heightmap grid: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  waterheight_matrix = raster_to_matrix(aligned_raster, verbose = FALSE)
  if (!all(dim(waterheight_matrix) == c(nr, nc))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` could not be aligned to the active heightmap grid."
      ),
      call. = FALSE
    )
  }
  waterheight_matrix
}

#'@keywords internal
coerce_spatial_waterheight_raster = function(waterheight) {
  if (is.character(waterheight)) {
    waterheight = terra::rast(waterheight)
  } else if (
    inherits(waterheight, c("RasterLayer", "RasterBrick", "RasterStack"))
  ) {
    waterheight = terra::rast(waterheight)
  }
  if (!inherits(waterheight, "SpatRaster")) {
    stop("`waterdepth` must resolve to a spatial raster.", call. = FALSE)
  }
  if (terra::nlyr(waterheight) > 1) {
    warning("`waterdepth` has multiple layers; using the first layer.")
    waterheight = waterheight[[1]]
  }
  waterheight
}

#'@keywords internal
build_waterheight_template = function(
  nr,
  nc,
  heightmap_extent = NULL,
  heightmap_crs = NULL,
  caller = NULL
) {
  if (is.null(heightmap_extent)) {
    heightmap_extent = get_scene_extent(default = NULL)
  }
  if (is.null(heightmap_extent)) {
    heightmap_extent = get_hillshade_extent(default = NULL)
  }
  if (is.null(heightmap_extent)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` inputs require an active heightmap extent."
      ),
      call. = FALSE
    )
  }
  heightmap_extent = tryCatch(
    get_extent(heightmap_extent),
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not interpret the active heightmap extent for spatial `waterdepth`: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  target_template = terra::rast(
    nrows = nc,
    ncols = nr,
    xmin = heightmap_extent["xmin"],
    xmax = heightmap_extent["xmax"],
    ymin = heightmap_extent["ymin"],
    ymax = heightmap_extent["ymax"]
  )
  if (is.null(heightmap_crs)) {
    heightmap_crs = get_scene_target_crs(
      extent = heightmap_extent,
      caller = caller
    )
  }
  heightmap_crs = waterheight_terra_crs(heightmap_crs)
  if (!is.null(heightmap_crs)) {
    terra::crs(target_template) = heightmap_crs
  }
  target_template
}

#'@keywords internal
waterheight_terra_crs = function(crs) {
  parsed_crs = try_parse_scene_crs(crs)
  if (!is.null(parsed_crs) && !is.na(parsed_crs)) {
    return(parsed_crs$wkt)
  }
  if (is.character(crs) && length(crs) && nzchar(trimws(crs[1]))) {
    return(crs[1])
  }
  NULL
}

#'@keywords internal
format_no_water_warning = function(heightmap, waterheight, zscale) {
  height_range = range(heightmap, na.rm = TRUE) * zscale
  water_range = range(waterheight, na.rm = TRUE) * zscale
  if (diff(water_range) == 0) {
    return(paste0(
      "No water rendered--all elevations above or equal to water level. Range of heights: ",
      height_range[1],
      "-",
      height_range[2],
      ". Depth specified: ",
      water_range[1]
    ))
  }
  paste0(
    "No water rendered--all elevations above or equal to water levels. Range of heights: ",
    height_range[1],
    "-",
    height_range[2],
    ". Water level range specified: ",
    water_range[1],
    "-",
    water_range[2]
  )
}

#'@keywords internal
make_waterlines_from_mesh = function(
  water_mesh,
  linecolor = "grey40",
  alpha = 1,
  linewidth = 2,
  antialias = FALSE
) {
  if (is.null(water_mesh) || is.null(water_mesh$lines)) {
    return(invisible(NULL))
  }
  segmentlist = water_mesh$lines
  if (!is.matrix(segmentlist) || nrow(segmentlist) == 0) {
    return(invisible(NULL))
  }
  rgl::segments3d(
    segmentlist,
    color = linecolor,
    lwd = linewidth,
    alpha = alpha,
    depth_mask = TRUE,
    line_antialias = antialias,
    depth_test = "lequal",
    tag = "waterlines",
    lit = FALSE
  )
  invisible(NULL)
}
