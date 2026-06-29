#'@title Render Water Layer
#'
#'@description Adds water layer to the scene, removing the previous water layer if desired.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap Default `NULL`. Height matrix or spatial raster for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param waterdepth Default `0`. Water level. Either a scalar, a matrix with the same dimensions as `heightmap`, or a spatial raster that can be projected/resampled to the heightmap grid. For spatial rasters, finite cells define the water footprint.
#'@param watercolor Default `lightblue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'If `zscale` is omitted and `heightmap` is a spatial raster, rayshader uses the raster cell resolution.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency.
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param water_render_method Default `"contour"`. Water meshing method. `"contour"` clips the water mesh to the flooded region; `"legacy"` uses the previous box/grid renderer.
#'@param water_edge_extension Default `0.5`. For spatial `waterdepth` inputs, amount in grid cells to expand finite water cells at boundary edges, up to a maximum of half a cell.
#' @param waterpaths Default `NULL`. Spatial line data used to draw stream paths. Supports
#' `sf`, `sfc`, `sfg`, `SpatialLines`, and `SpatialLinesDataFrame` line inputs.
#' @param waterpath_width Default `1`. Stream width in scene grid-cell units for
#' `render_highquality()`. The rgl preview uses the same value as line width.
#' @param waterpath_step Default `0.5`. Maximum spacing, in scene grid-cell units,
#' between stream vertices after densifying to hug the terrain.
#' @param waterpath_densify Default `TRUE`. Whether to densify stream paths and
#' resample them along the terrain before `render_highquality()` meshing. Set to
#' `FALSE` to use the vertices returned by [render_path()] directly.
#' @param waterpath_offset Default `NULL`. Vertical stream centerline offset in elevation
#' units. When `NULL`, the stream centerline is placed on the sampled surface so the
#' high-quality rectangular stream profile is rendered halfway through the terrain.
#' @param waterpath_simplify_tolerance Default `NULL`. Simplification tolerance
#' passed to [render_path()] for spatial stream paths. When `NULL`, this defaults
#' to the grid cell spacing of the active DEM. Set to `0` to disable stream
#' simplification.
#' @param waterpath_merge Default `TRUE`. Whether to merge connected stream linework before
#' rendering. This reduces visible caps between adjacent line features in `render_highquality()`.
#' @param waterpath_reorder Default `FALSE`. Whether to reorder spatial stream paths using
#' [render_path()] before rendering.
#'@param remove_water Default `TRUE`. If `TRUE`, will remove existing water layer and replace it with new layer.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'montereybay |>
#'  sphere_shade(vertical_exaggeration = 20) |>
#'  plot_3d(vertical_exaggeration = 4)
#'render_snapshot()
#'
#'#We want to add a layer of water after the initial render.
#'render_water()
#'render_snapshot()
#'
#'#Call it again to change the water depth
#'render_water(waterdepth=-1000, watercolor = "dodgerblue3")
#'render_snapshot()
#'
#'#Slice the water out to the edge
#'water_levels = matrix(
#'  0,
#'  nrow = nrow(montereybay),
#'  ncol = ncol(montereybay)
#')
#'water_levels[col(water_levels) > ncol(water_levels) / 2 + 20 |
#' col(water_levels) < ncol(water_levels) / 2-20] = -8000
#'render_water(waterdepth = water_levels, watercolor = "dodgerblue4")
#'render_snapshot()
#'
#'#Use a matrix to vary the water level across the scene
#'water_ramp = matrix(
#'  seq(-1200, -300, length.out = length(montereybay)),
#'  nrow = nrow(montereybay),
#'  ncol = ncol(montereybay)
#')
#'render_water(waterdepth = water_ramp, watercolor = "dodgerblue3")
#'render_highquality()
#'
#'#Add waterlines
#'render_camera(theta=-45)
#'render_water(waterlinecolor="white", watercolor = "dodgerblue4")
#'render_snapshot()
render_water = function(
  heightmap = NULL,
  waterdepth = 0,
  watercolor = "lightblue",
  zscale = 1,
  vertical_exaggeration = 1,
  wateralpha = 0.5,
  waterlinecolor = NULL,
  waterlinealpha = 1,
  linewidth = 2,
  water_render_method = c("contour", "legacy"),
  water_edge_extension = 0.5,
  waterpaths = NULL,
  waterpath_width = 1,
  waterpath_step = 0.5,
  waterpath_densify = TRUE,
  waterpath_offset = NULL,
  waterpath_simplify_tolerance = NULL,
  waterpath_merge = TRUE,
  waterpath_reorder = FALSE,
  remove_water = TRUE
) {
  waterdepth_missing = missing(waterdepth)
  if (is.null(waterpaths) && is_waterpath_input(waterdepth)) {
    waterpaths = waterdepth
    waterdepth = NULL
    waterdepth_missing = FALSE
  } else if (!is.null(waterpaths) && waterdepth_missing) {
    waterdepth = NULL
  }
  water_render_method = match.arg(water_render_method)
  heightmap = resolve_render_water_heightmap(
    heightmap,
    heightmap_missing = missing(heightmap),
    caller = "render_water"
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
    caller = "render_water"
  )
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  heightmap_extent = NULL
  heightmap_crs = NULL
  if (is_spatial_heightmap_input(waterdepth)) {
    heightmap_extent = resolve_scene_render_extent(
      heightmap = heightmap,
      caller = "render_water",
      error_if_missing = FALSE
    )
    heightmap_crs = attr(heightmap, "crs", exact = TRUE)
    if (is.null(heightmap_crs)) {
      heightmap_crs = tryCatch(
        get_scene_target_crs(
          extent = heightmap_extent,
          heightmap = heightmap,
          caller = "render_water"
        ),
        error = function(e) NULL
      )
    }
  }
  if (remove_water) {
    rgl::pop3d(tag = c("waterlines", "water", "water_path"))
  }
  water_mesh = list(
    vertices = list(),
    lines = matrix(nrow = 0, ncol = 3)
  )
  if (!is.null(waterdepth)) {
    water_mesh = make_water(
      heightmap,
      waterheight = waterdepth,
      wateralpha = wateralpha,
      watercolor = watercolor,
      zscale = zscale,
      water_render_method = water_render_method,
      water_edge_extension = water_edge_extension,
      heightmap_extent = heightmap_extent,
      heightmap_crs = heightmap_crs
    )
  }
  if (!is.null(waterpaths)) {
    render_water_paths(
      waterpaths = waterpaths,
      heightmap = heightmap,
      extent = resolve_scene_render_extent(
        heightmap = heightmap,
        caller = "render_water",
        error_if_missing = FALSE
      ),
      zscale = zscale,
      watercolor = watercolor,
      waterpath_width = waterpath_width,
      waterpath_step = waterpath_step,
      waterpath_densify = waterpath_densify,
      waterpath_offset = waterpath_offset,
      waterpath_simplify_tolerance = waterpath_simplify_tolerance,
      waterpath_merge = waterpath_merge,
      waterpath_reorder = waterpath_reorder
    )
  }
  if (!is.null(waterlinecolor)) {
    if (identical(water_render_method, "contour")) {
      make_waterlines_from_mesh(
        water_mesh,
        linecolor = waterlinecolor,
        alpha = waterlinealpha,
        linewidth = linewidth
      )
    } else {
      if (all(!is.na(heightmap))) {
        make_lines(
          fliplr(heightmap),
          basedepth = waterdepth,
          linecolor = waterlinecolor,
          zscale = zscale,
          linewidth = linewidth,
          alpha = waterlinealpha,
          solid = FALSE
        )
      }
      make_waterlines(
        heightmap,
        waterdepth = waterdepth,
        linecolor = waterlinecolor,
        zscale = zscale,
        alpha = waterlinealpha,
        linewidth = linewidth
      )
    }
  }
  invisible(NULL)
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
#' @param waterpath_step Maximum densified step size.
#' @param waterpath_densify Whether to densify paths.
#' @param waterpath_offset Centerline offset in elevation units.
#' @param waterpath_simplify_tolerance Default `NULL`. Simplification tolerance.
#' @param waterpath_merge Whether to merge connected linework.
#' @param waterpath_reorder Whether to reorder paths.
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
  waterpath_step = 0.5,
  waterpath_densify = TRUE,
  waterpath_offset = NULL,
  waterpath_simplify_tolerance = NULL,
  waterpath_merge = TRUE,
  waterpath_reorder = FALSE
) {
  if (!is_waterpath_input(waterpaths)) {
    stop(
      "`waterpaths` must be an sf, sfc, sfg, SpatialLines, or SpatialLinesDataFrame line object.",
      call. = FALSE
    )
  }
  waterpath_width = validate_waterpath_positive_number(
    waterpath_width,
    "waterpath_width"
  )
  waterpath_densify = validate_waterpath_logical(
    waterpath_densify,
    "waterpath_densify"
  )
  if (isTRUE(waterpath_densify)) {
    waterpath_step = validate_waterpath_positive_number(
      waterpath_step,
      "waterpath_step"
    )
  }
  waterpath_offset = resolve_waterpath_offset(
    waterpath_offset,
    waterpath_width = waterpath_width,
    zscale = zscale
  )
  waterpath_simplify_tolerance = resolve_waterpath_simplify_tolerance(
    waterpath_simplify_tolerance,
    heightmap = heightmap,
    extent = extent
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
    reorder = waterpath_reorder,
    simplify_tolerance = waterpath_simplify_tolerance,
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
      offset = waterpath_offset,
      max_step = waterpath_step
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

#' Resolve water path simplification tolerance
#'
#' @param value Default `NULL`. Requested simplification tolerance.
#' @param heightmap Heightmap matrix.
#' @param extent Default `NULL`. Heightmap extent.
#'
#' @return Numeric simplification tolerance.
#' @keywords internal
resolve_waterpath_simplify_tolerance = function(
  value = NULL,
  heightmap,
  extent = NULL
) {
  if (!is.null(value)) {
    return(validate_waterpath_positive_number(
      value,
      "waterpath_simplify_tolerance",
      allow_zero = TRUE
    ))
  }
  spacing = calculate_heightmap_grid_spacing(heightmap, extent = extent)
  if (!is.finite(spacing) || spacing <= 0) {
    return(0)
  }
  spacing
}

#' Calculate heightmap grid spacing
#'
#' @param heightmap Heightmap matrix.
#' @param extent Default `NULL`. Heightmap extent.
#'
#' @return Mean grid cell spacing in heightmap coordinate units.
#' @keywords internal
calculate_heightmap_grid_spacing = function(heightmap, extent = NULL) {
  if (is.null(extent)) {
    extent = attr(heightmap, "extent", exact = TRUE)
  }
  if (!is.null(extent) && is.matrix(heightmap)) {
    extent_values = tryCatch(
      get_extent(extent),
      error = function(e) NULL
    )
    if (!is.null(extent_values)) {
      spacings = c(
        (extent_values["xmax"] - extent_values["xmin"]) / nrow(heightmap),
        (extent_values["ymax"] - extent_values["ymin"]) / ncol(heightmap)
      )
      spacings = abs(spacings[is.finite(spacings) & spacings > 0])
      if (length(spacings) > 0) {
        return(mean(spacings))
      }
    }
  }
  zscale = suppressWarnings(as.numeric(attr(heightmap, "zscale", exact = TRUE))[
    1
  ])
  if (is.finite(zscale) && zscale > 0) {
    return(zscale)
  }
  1
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
#' @param waterpath_width Stream width in scene units.
#' @param zscale Effective zscale.
#'
#' @return Offset in elevation units.
#' @keywords internal
resolve_waterpath_offset = function(
  value = NULL,
  waterpath_width,
  zscale
) {
  if (!is.null(value)) {
    return(validate_waterpath_positive_number(
      value,
      "waterpath_offset",
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
#' @param max_step Maximum step size in scene units.
#'
#' @return List of densified coordinate matrices.
#' @keywords internal
densify_water_path_coords = function(
  coord_list,
  heightmap,
  zscale,
  offset,
  max_step
) {
  heightmap_scene = heightmap / zscale
  offset_scene = offset / zscale
  lapply(coord_list, function(coords) {
    densify_single_water_path_coord(
      coords = coords,
      heightmap = heightmap_scene,
      offset = offset_scene,
      max_step = max_step
    )
  })
}

#' Calculate water path segment sample positions
#'
#' @param heightmap Heightmap matrix scaled into scene units.
#' @param segment_start Two-value segment start coordinate.
#' @param segment_end Two-value segment end coordinate.
#' @param max_step Maximum step size in scene units.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
calculate_water_path_segment_t = function(
  heightmap,
  segment_start,
  segment_end,
  max_step
) {
  segment_delta = segment_end - segment_start
  segment_length = sqrt(sum(segment_delta^2))
  step_count = as.integer(pmax(1L, ceiling(segment_length / max_step)))
  unique_water_path_t(c(
    seq(0, 1, length.out = step_count + 1L),
    calculate_water_path_triangle_boundary_t(
      heightmap = heightmap,
      segment_start = segment_start,
      segment_end = segment_end
    )
  ))
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
#' @param max_step Maximum step size in scene units.
#'
#' @return Densified coordinate matrix.
#' @keywords internal
densify_single_water_path_coord = function(
  coords,
  heightmap,
  offset,
  max_step
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
      segment_end = coords[index + 1L, c(1, 3)],
      max_step = max_step
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

#' Resolve render_water heightmap
#'
#' @param heightmap Default `NULL`. Heightmap input.
#' @param heightmap_missing Default `FALSE`. Whether `heightmap` was omitted.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Heightmap matrix or `NULL`.
#' @keywords internal
resolve_render_water_heightmap = function(
  heightmap = NULL,
  heightmap_missing = FALSE,
  caller = NULL
) {
  if (
    !isTRUE(heightmap_missing) &&
      !is.null(heightmap) &&
      is_spatial_heightmap_input(heightmap)
  ) {
    heightmap_info = coerce_plot_3d_heightmap(heightmap)
    heightmap = heightmap_info$heightmap
    if (!is.null(heightmap_info$extent)) {
      attr(heightmap, "extent") = heightmap_info$extent
    }
    if (!is.null(heightmap_info$crs)) {
      attr(heightmap, "crs") = heightmap_info$crs
    }
    if (is.finite(heightmap_info$zscale) && heightmap_info$zscale > 0) {
      attr(heightmap, "zscale") = heightmap_info$zscale
    }
    return(heightmap)
  }
  resolve_scene_render_heightmap(
    heightmap,
    caller = caller
  )
}

#' Resolve render_water zscale
#'
#' @param zscale Default `1`. Requested zscale.
#' @param zscale_missing Default `FALSE`. Whether `zscale` was omitted.
#' @param vertical_exaggeration Default `1`. Requested vertical exaggeration.
#' @param vertical_exaggeration_missing Default `FALSE`. Whether `vertical_exaggeration` was omitted.
#' @param heightmap Default `NULL`. Resolved heightmap.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Effective zscale.
#' @keywords internal
resolve_render_water_effective_zscale = function(
  zscale = 1,
  zscale_missing = FALSE,
  vertical_exaggeration = 1,
  vertical_exaggeration_missing = FALSE,
  heightmap = NULL,
  caller = NULL
) {
  heightmap_zscale = suppressWarnings(
    as.numeric(attr(heightmap, "zscale", exact = TRUE))[1]
  )
  if (
    isTRUE(zscale_missing) &&
      is.finite(heightmap_zscale) &&
      heightmap_zscale > 0
  ) {
    zscale = heightmap_zscale
  } else {
    zscale = resolve_scene_render_zscale(
      zscale = zscale,
      zscale_missing = zscale_missing,
      caller = caller
    )
  }
  vertical_exaggeration = resolve_scene_render_vertical_exaggeration(
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = vertical_exaggeration_missing,
    caller = caller
  )
  apply_vertical_exaggeration(
    zscale = zscale,
    vertical_exaggeration = vertical_exaggeration,
    caller = caller
  )
}
