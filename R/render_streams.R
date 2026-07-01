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
#' @param width_column Default `NULL`. Column name in an `sf` stream object used
#' to set per-feature stream widths. Values must be positive finite numbers and
#' use the same units as `width`. When supplied, stream merging is disabled to
#' preserve feature attributes.
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
#' @examplesIf all(vapply(c("sf", "terra", "dplyr", "tigris", "elevatr", "rayrender", "rayvertex", "skymodelr"), requireNamespace, logical(1), quietly = TRUE)) && (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(sf)
#' library(terra)
#' library(dplyr)
#' library(tigris)
#' library(elevatr)
#'
#' options(tigris_use_cache = TRUE)
#'
#' target_crs = "EPSG:32611"
#' dem_z = 12
#' min_water_area_m2 = 5000
#' min_stream_length_m = 1
#'
#' # Start with a longitude/latitude bounding box for the Mammoth Lakes,
#' # June Lake, and Crowley Lake region.
#' scene_ll = sf::st_as_sfc(
#'   sf::st_bbox(
#'     c(
#'       xmin = -119.12,
#'       ymin = 37.52,
#'       xmax = -118.68,
#'       ymax = 37.86
#'     ),
#'     crs = sf::st_crs(4326)
#'   )
#' ) |>
#'   sf::st_as_sf()
#'
#' # Transform to UTM zone 11N so areas, lengths, and stream widths are in
#' # projected meter-like units.
#' scene = sf::st_transform(scene_ll, target_crs)
#'
#' # TIGER/Line area water includes lakes and reservoirs. We crop to the scene,
#' # keep larger standing-water features, and later sample each water body's
#' # representative elevation from the DEM.
#' water_raw = tigris::area_water(
#'   state = "CA",
#'   county = "Mono",
#'   year = 2024,
#'   class = "sf"
#' )
#'
#' water = water_raw |>
#'   sf::st_make_valid() |>
#'   sf::st_transform(target_crs) |>
#'   sf::st_crop(scene) |>
#'   dplyr::filter(MTFCC %in% c("H2030", "H2040")) |>
#'   dplyr::mutate(area_m2 = as.numeric(sf::st_area(geometry))) |>
#'   dplyr::filter(area_m2 >= min_water_area_m2)
#'
#' water = water[!sf::st_is_empty(water), ]
#'
#' # Download a DEM from AWS via elevatr and aggregate it to keep the example
#' # light enough for interactive rendering.
#' dem_raw = elevatr::get_elev_raster(
#'   locations = scene,
#'   z = dem_z,
#'   src = "aws",
#'   clip = "bbox",
#'   tmp_dir = tempdir()
#' )
#'
#' dem = terra::rast(dem_raw) |>
#'   terra::aggregate(4)
#' names(dem) = "elevation_m"
#'
#' water = sf::st_transform(water, terra::crs(dem))
#'
#' water_level = terra::extract(
#'   dem,
#'   terra::vect(water),
#'   fun = median,
#'   na.rm = TRUE,
#'   touches = TRUE
#' )
#'
#' water$water_level_m = round(water_level$elevation_m, 1)
#' water = water[is.finite(water$water_level_m), ]
#'
#' # TIGER/Line linear water gives the stream and canal network. We crop,
#' # extract line strings, drop empty features, and remove tiny segments.
#' streams_raw = tigris::linear_water(
#'   state = "CA",
#'   county = "Mono",
#'   year = 2024,
#'   class = "sf"
#' )
#'
#' streams = streams_raw |>
#'   sf::st_make_valid() |>
#'   sf::st_transform(target_crs) |>
#'   sf::st_crop(scene) |>
#'   sf::st_collection_extract("LINESTRING", warn = FALSE)
#'
#' streams = streams[!sf::st_is_empty(streams), ]
#'
#' streams = streams |>
#'   dplyr::mutate(length_m = as.numeric(sf::st_length(geometry))) |>
#'   dplyr::filter(length_m >= min_stream_length_m)
#'
#' streams = sf::st_transform(streams, terra::crs(dem))
#'
#' # Rasterize per-waterbody elevations for render_water(), then lower the
#' # terrain under polygon water so the rendered water surface is visible.
#' water_level_rast = terra::rasterize(
#'   terra::vect(water),
#'   dem,
#'   field = "water_level_m",
#'   touches = TRUE
#' )
#'
#' new_dem = dem |>
#'   indent_surface(water, 10, direction = "down")
#'
#' new_dem |>
#'   height_shade() |>
#'   plot_3d(
#'     phi = 10,
#'     zoom = 0.15,
#'     fov = 120
#'   )
#'
#' render_water(
#'   waterdepth = water_level_rast,
#'   water_edge_extension = 0.25,
#'   watercolor = "dodgerblue"
#' )
#'
#' render_streams(
#'   streams = streams,
#'   watercolor = "dodgerblue",
#'   width = 0.35,
#'   clear_previous = TRUE
#' )
#'
#' render_highquality(
#'   joined_stream_mesh = TRUE,
#'   use_extruded_paths = TRUE,
#'   sky_sun_elevation = 30,
#'   width = 800,
#'   height = 800,
#'   sky_sun_azimuth = -113,
#'   sky_args = list(hosek = FALSE),
#'   camera_lookat = c(-183.38, -1.20, -232.91),
#'   camera_location = c(-279.37, 49.51, -177.42),
#'   water_ior = 1.2,
#'   water_material = "microfacet",
#'   iso = 100 / 2^5
#' )
#' @export
render_streams = function(
  streams,
  heightmap = NULL,
  watercolor = "lightblue",
  zscale = 1,
  vertical_exaggeration = 1,
  width = 1,
  width_column = NULL,
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
  width_column = resolve_waterpath_width_column(
    width_column = width_column,
    width_column_expr = substitute(width_column),
    width_column_missing = missing(width_column)
  )
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
    waterpath_width_column = width_column,
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
#' @param waterpath_width_column Column name containing stream widths.
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
  waterpath_width_column = NULL,
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
  if (!is.null(waterpath_width_column)) {
    waterpath_width_column = validate_waterpath_width_column_name(
      waterpath_width_column
    )
    waterpath_merge = FALSE
  }
  waterpaths = prepare_render_water_path_geometry(
    waterpaths = waterpaths,
    waterpath_merge = waterpath_merge
  )
  if (is_empty_scene_sf(waterpaths)) {
    return(invisible(list()))
  }
  waterpath_width = resolve_waterpath_widths(
    waterpaths = waterpaths,
    waterpath_width = waterpath_width,
    waterpath_width_column = waterpath_width_column
  )
  path_render = render_water_path_coords_by_width(
    waterpaths = waterpaths,
    heightmap = heightmap,
    extent = extent,
    zscale = zscale,
    watercolor = watercolor,
    waterpath_width = waterpath_width
  )
  coord_list = path_render$coord_list
  coord_width = path_render$width
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
  for (coord_index in seq_along(coord_list)) {
    coord = coord_list[[coord_index]]
    if (is.matrix(coord) && nrow(coord) >= 2) {
      rgl::lines3d(
        coord,
        color = watercolor,
        tag = "water_path",
        lwd = coord_width[[coord_index]],
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

#' Resolve water path width column
#'
#' @param width_column Width column value.
#' @param width_column_expr Width column expression.
#' @param width_column_missing Whether the width column argument was omitted.
#'
#' @return Column name or `NULL`.
#' @keywords internal
resolve_waterpath_width_column = function(
  width_column = NULL,
  width_column_expr = NULL,
  width_column_missing = FALSE
) {
  if (
    isTRUE(width_column_missing) ||
      identical(width_column_expr, quote(NULL))
  ) {
    return(NULL)
  }
  if (is.character(width_column_expr)) {
    return(validate_waterpath_width_column_name(width_column_expr))
  }
  if (is.name(width_column_expr)) {
    value = tryCatch(width_column, error = function(e) NULL)
    if (is.character(value) && length(value) == 1) {
      return(validate_waterpath_width_column_name(value))
    }
    return(validate_waterpath_width_column_name(as.character(
      width_column_expr
    )))
  }
  validate_waterpath_width_column_name(width_column)
}

#' Validate water path width column name
#'
#' @param width_column Width column name.
#'
#' @return Column name.
#' @keywords internal
validate_waterpath_width_column_name = function(width_column) {
  if (
    !is.character(width_column) ||
      length(width_column) != 1 ||
      is.na(width_column) ||
      !nzchar(width_column)
  ) {
    stop("`width_column` must be a single column name.", call. = FALSE)
  }
  width_column
}

#' Resolve water path widths
#'
#' @param waterpaths Spatial line input.
#' @param waterpath_width Stream width.
#' @param waterpath_width_column Column name containing stream widths.
#'
#' @return Numeric stream widths.
#' @keywords internal
resolve_waterpath_widths = function(
  waterpaths,
  waterpath_width,
  waterpath_width_column = NULL
) {
  if (is.null(waterpath_width_column)) {
    return(validate_waterpath_positive_number(
      waterpath_width,
      "width"
    ))
  }
  if (!inherits(waterpaths, "sf")) {
    stop(
      "`width_column` can only be used with `sf` or `SpatialLinesDataFrame` stream inputs.",
      call. = FALSE
    )
  }
  if (!(waterpath_width_column %in% names(waterpaths))) {
    stop(
      sprintf(
        "`width_column` must name a column in `streams`: %s",
        waterpath_width_column
      ),
      call. = FALSE
    )
  }
  widths = suppressWarnings(as.numeric(waterpaths[[waterpath_width_column]]))
  valid = length(widths) == nrow(waterpaths) &&
    all(is.finite(widths)) &&
    all(widths > 0)
  if (!valid) {
    stop(
      sprintf(
        "`width_column` column `%s` must contain positive finite numeric values.",
        waterpath_width_column
      ),
      call. = FALSE
    )
  }
  widths
}

#' Render water path coordinates by width
#'
#' @param waterpaths Spatial line input.
#' @param heightmap Heightmap matrix.
#' @param extent Scene extent.
#' @param zscale Effective zscale.
#' @param watercolor Water color.
#' @param waterpath_width Stream widths.
#'
#' @return List containing coordinates and matching widths.
#' @keywords internal
render_water_path_coords_by_width = function(
  waterpaths,
  heightmap,
  extent,
  zscale,
  watercolor,
  waterpath_width
) {
  if (length(waterpath_width) == 1) {
    coord_list = render_water_path_coords(
      waterpaths = waterpaths,
      heightmap = heightmap,
      extent = extent,
      zscale = zscale,
      watercolor = watercolor,
      waterpath_width = waterpath_width
    )
    return(list(
      coord_list = coord_list,
      width = rep(waterpath_width, length(coord_list))
    ))
  }
  coord_list = list()
  coord_width = numeric(0)
  for (path_index in seq_along(waterpath_width)) {
    path = subset_waterpath_geometry(waterpaths, path_index)
    path_coords = render_water_path_coords(
      waterpaths = path,
      heightmap = heightmap,
      extent = extent,
      zscale = zscale,
      watercolor = watercolor,
      waterpath_width = waterpath_width[[path_index]]
    )
    if (!length(path_coords)) {
      next
    }
    coord_list = c(coord_list, path_coords)
    coord_width = c(
      coord_width,
      rep(waterpath_width[[path_index]], length(path_coords))
    )
  }
  list(coord_list = coord_list, width = coord_width)
}

#' Render water path coordinates
#'
#' @param waterpaths Spatial line input.
#' @param heightmap Heightmap matrix.
#' @param extent Scene extent.
#' @param zscale Effective zscale.
#' @param watercolor Water color.
#' @param waterpath_width Stream width.
#'
#' @return List of coordinate matrices.
#' @keywords internal
render_water_path_coords = function(
  waterpaths,
  heightmap,
  extent,
  zscale,
  watercolor,
  waterpath_width
) {
  render_path(
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
}

#' Subset water path geometry
#'
#' @param waterpaths Spatial line input.
#' @param index Feature index.
#'
#' @return Spatial line input subset.
#' @keywords internal
subset_waterpath_geometry = function(waterpaths, index) {
  if (inherits(waterpaths, "sf")) {
    return(waterpaths[index, , drop = FALSE])
  }
  if (inherits(waterpaths, "sfc")) {
    return(waterpaths[index])
  }
  waterpaths
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

#' Make joined render_highquality water path meshes
#'
#' @param tasks Water path mesh task list.
#' @param ... Additional arguments passed to
#' [make_render_highquality_joined_water_path_mesh()].
#'
#' @return List of rayrender mesh objects.
#' @keywords internal
make_render_highquality_joined_water_path_meshes = function(tasks, ...) {
  if (!length(tasks)) {
    return(list())
  }
  groups = group_render_highquality_water_path_tasks(tasks)
  meshes = list()
  warned = FALSE
  warn_fallback = function(message) {
    if (!isTRUE(warned)) {
      warning(
        "Joined stream mesh generation failed; falling back to single-line stream meshes: ",
        message,
        call. = FALSE
      )
      warned <<- TRUE
    }
  }
  for (group in groups) {
    mesh = tryCatch(
      make_render_highquality_joined_water_path_mesh(group$tasks, ...),
      error = function(e) {
        warn_fallback(conditionMessage(e))
        NULL
      }
    )
    if (is.null(mesh)) {
      warn_fallback("empty joined mesh")
      meshes = c(meshes, make_render_highquality_water_path_meshes(group$tasks))
    } else {
      meshes[[length(meshes) + 1L]] = mesh
    }
  }
  meshes
}

#' Group compatible render_highquality water path tasks
#'
#' @param tasks Water path mesh task list.
#'
#' @return List of grouped task lists.
#' @keywords internal
group_render_highquality_water_path_tasks = function(tasks) {
  groups = list()
  for (task in tasks) {
    matched_group = 0L
    for (group_index in seq_along(groups)) {
      if (
        are_render_highquality_water_path_tasks_compatible(
          task,
          groups[[group_index]]$prototype
        )
      ) {
        matched_group = group_index
        break
      }
    }
    if (matched_group == 0L) {
      groups[[length(groups) + 1L]] = list(
        prototype = task,
        tasks = list(task)
      )
    } else {
      groups[[matched_group]]$tasks[[
        length(groups[[matched_group]]$tasks) + 1L
      ]] =
        task
    }
  }
  groups
}

#' Test water path task compatibility
#'
#' @param task Water path mesh task.
#' @param prototype Prototype water path mesh task.
#'
#' @return Logical value.
#' @keywords internal
are_render_highquality_water_path_tasks_compatible = function(task, prototype) {
  same_number = function(x, y) {
    isTRUE(all.equal(
      suppressWarnings(as.numeric(x[1])),
      suppressWarnings(as.numeric(y[1])),
      tolerance = sqrt(.Machine$double.eps),
      check.attributes = FALSE
    ))
  }
  same_vector = function(x, y) {
    isTRUE(all.equal(
      suppressWarnings(as.numeric(x)),
      suppressWarnings(as.numeric(y)),
      tolerance = sqrt(.Machine$double.eps),
      check.attributes = FALSE
    ))
  }
  same_number(task$width, prototype$width) &&
    same_number(task$zscale, prototype$zscale) &&
    same_vector(task$bbox_center, prototype$bbox_center) &&
    identical(task$heightmap, prototype$heightmap) &&
    identical(task$material, prototype$material)
}

#' Make one joined render_highquality water path mesh
#'
#' @param tasks Compatible water path mesh tasks.
#' @param seal_epsilon Default `NULL`. Downward terrain sealing distance in scene
#' units. When `NULL`, uses a width-scaled epsilon.
#' @param bottom_cap Default `TRUE`. Whether to add a hidden bottom cap below the
#' terrain surface.
#'
#' @return Rayrender mesh object.
#' @keywords internal
make_render_highquality_joined_water_path_mesh = function(
  tasks,
  seal_epsilon = NULL,
  bottom_cap = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  group = prepare_render_highquality_water_path_line_group(tasks)
  if (is.null(group) || !length(group$lines)) {
    stop("No valid stream lines were available.")
  }
  lines = clamp_render_highquality_water_path_endpoints(
    group$lines,
    width = group$width
  )
  if (!length(lines)) {
    stop("Stream endpoint clamping removed all lines.")
  }
  footprint = make_render_highquality_water_path_buffer_footprint(
    lines = lines,
    width = group$width
  )
  if (is.null(footprint) || !length(footprint)) {
    stop("Stream buffering produced an empty footprint.")
  }
  terrain_triangles = make_render_highquality_water_path_valid_terrain_triangles(
    heightmap = group$heightmap,
    bbox = sf::st_bbox(footprint),
    margin = group$width
  )
  if (!nrow(terrain_triangles)) {
    stop("No valid terrain triangles intersect the stream footprint bounds.")
  }
  fragments = clip_render_highquality_water_path_footprint_to_terrain(
    footprint = footprint,
    terrain_triangles = terrain_triangles,
    area_epsilon = max(group$width, 1)^2 * 1e-12
  )
  if (!nrow(fragments)) {
    stop("The stream footprint did not intersect valid terrain.")
  }
  triangulated = triangulate_render_highquality_water_path_fragments(
    fragments = fragments,
    width = group$width
  )
  if (
    is.null(triangulated) ||
      !nrow(triangulated$vertices_xz) ||
      !nrow(triangulated$indices)
  ) {
    stop("The clipped stream footprint could not be triangulated.")
  }
  terrain_y = sample_render_highquality_water_path_surface(
    points_xz = triangulated$vertices_xz,
    heightmap = group$heightmap,
    terrain_triangles = terrain_triangles,
    tri_id = triangulated$vertex_tri_id
  )
  if (any(!is.finite(terrain_y))) {
    stop("Joined stream mesh height sampling produced non-finite values.")
  }
  height = group$width * waterpath_profile_height_ratio()
  if (is.null(seal_epsilon)) {
    seal_epsilon = max(group$width, 1) * 1e-5
  } else {
    seal_epsilon = suppressWarnings(as.numeric(seal_epsilon[1]))
    if (!is.finite(seal_epsilon) || seal_epsilon < 0) {
      stop("`seal_epsilon` must be a single non-negative number.")
    }
  }
  top_vertices = cbind(
    triangulated$vertices_xz[, 1],
    terrain_y + group$offset_scene + height,
    triangulated$vertices_xz[, 2]
  )
  top_indices = orient_render_highquality_water_path_top_indices(
    vertices = top_vertices,
    indices = triangulated$indices
  )
  if (!nrow(top_indices)) {
    stop("Joined stream mesh triangulation produced no valid top faces.")
  }
  bottom_vertices = cbind(
    triangulated$vertices_xz[, 1],
    terrain_y - seal_epsilon,
    triangulated$vertices_xz[, 2]
  )
  vertex_count = nrow(top_vertices)
  bottom_indices = matrix(integer(0), ncol = 3)
  if (isTRUE(bottom_cap)) {
    bottom_indices = cbind(
      top_indices[, 3] + vertex_count,
      top_indices[, 2] + vertex_count,
      top_indices[, 1] + vertex_count
    )
  }
  side_indices = make_render_highquality_water_path_side_faces(top_indices)
  indices = rbind(top_indices, bottom_indices, side_indices)
  if (!nrow(indices)) {
    stop("Joined stream mesh produced no faces.")
  }
  vertices = rbind(top_vertices, bottom_vertices)
  vertices = sweep(vertices, 2, group$bbox_center, FUN = "-")
  mesh = list(
    vb = t(cbind(vertices, 1)),
    it = t(indices)
  )
  class(mesh) = "mesh3d"
  rayrender::mesh3d_model(
    mesh,
    override_material = TRUE,
    material = group$material
  )
}

#' Prepare joined water path line group
#'
#' @param tasks Compatible water path mesh tasks.
#'
#' @return Prepared line group.
#' @keywords internal
prepare_render_highquality_water_path_line_group = function(tasks) {
  if (!length(tasks)) {
    return(NULL)
  }
  prototype = tasks[[1]]
  width = suppressWarnings(as.numeric(prototype$width[1]))
  if (!is.finite(width) || width <= 0) {
    return(NULL)
  }
  heightmap_scene = scale_render_highquality_water_path_heightmap(
    heightmap = prototype$heightmap,
    zscale = prototype$zscale
  )$heightmap
  if (
    is.null(heightmap_scene) ||
      !is.matrix(heightmap_scene) ||
      nrow(heightmap_scene) < 2 ||
      ncol(heightmap_scene) < 2
  ) {
    return(NULL)
  }
  lines = list()
  offset_values = numeric(0)
  for (task in tasks) {
    point_groups = split_render_highquality_water_path_task_points(task$points)
    for (points in point_groups) {
      finite_xyz = stats::complete.cases(points[, 1:3, drop = FALSE])
      if (any(finite_xyz)) {
        terrain_y = interpolate_spatial_water_height(
          heightmap_scene,
          points[finite_xyz, 1],
          points[finite_xyz, 3]
        )
        offset_values = c(
          offset_values,
          points[finite_xyz, 2] - terrain_y
        )
      }
      finite_xz = stats::complete.cases(points[, c(1, 3), drop = FALSE])
      line = points[finite_xz, c(1, 3), drop = FALSE]
      line = collapse_render_highquality_water_path_line(line)
      if (nrow(line) >= 2) {
        lines[[length(lines) + 1L]] = line
      }
    }
  }
  offset_values = offset_values[is.finite(offset_values)]
  offset_scene = if (length(offset_values)) {
    stats::median(offset_values, na.rm = TRUE)
  } else {
    0
  }
  if (!is.finite(offset_scene)) {
    offset_scene = 0
  }
  list(
    lines = lines,
    width = width,
    bbox_center = prototype$bbox_center,
    heightmap = heightmap_scene,
    zscale = 1,
    material = prototype$material,
    offset_scene = offset_scene,
    tasks = tasks
  )
}

#' Split a water path task point matrix
#'
#' @param points Path points.
#'
#' @return List of finite path point matrices.
#' @keywords internal
split_render_highquality_water_path_task_points = function(points) {
  points = as.matrix(points)
  if (!nrow(points) || ncol(points) < 3) {
    return(list())
  }
  separator = rowSums(!is.finite(points[, 1:3, drop = FALSE])) > 0
  groups = cumsum(separator)
  point_indices = split(seq_len(nrow(points)), groups)
  out = vector("list", length(point_indices))
  for (index in seq_along(point_indices)) {
    group_points = points[point_indices[[index]], , drop = FALSE]
    if (index > 1L && nrow(group_points) > 0) {
      group_points = group_points[-1L, , drop = FALSE]
    }
    group_points = group_points[
      stats::complete.cases(group_points[, c(1, 3), drop = FALSE]),
      ,
      drop = FALSE
    ]
    out[[index]] = group_points
  }
  out[vapply(out, nrow, integer(1)) > 0L]
}

#' Collapse duplicated water path line vertices
#'
#' @param line Two-column `x`/`z` line matrix.
#'
#' @return Line matrix with consecutive duplicate vertices removed.
#' @keywords internal
collapse_render_highquality_water_path_line = function(line) {
  line = as.matrix(line)
  if (!nrow(line) || ncol(line) < 2) {
    return(matrix(numeric(0), ncol = 2))
  }
  line = line[
    stats::complete.cases(line[, 1:2, drop = FALSE]),
    1:2,
    drop = FALSE
  ]
  if (nrow(line) < 2) {
    return(line)
  }
  distance = sqrt(rowSums(
    (line[-1, , drop = FALSE] - line[-nrow(line), , drop = FALSE])^2
  ))
  keep = c(TRUE, distance > sqrt(.Machine$double.eps))
  line[keep, , drop = FALSE]
}

#' Clamp nearby water path endpoints
#'
#' @param lines List of two-column `x`/`z` line matrices.
#' @param width Stream width in scene units.
#' @param snap_distance Default `width`. Maximum endpoint-to-line distance to
#' clamp.
#'
#' @return List of clamped line matrices.
#' @keywords internal
clamp_render_highquality_water_path_endpoints = function(
  lines,
  width,
  snap_distance = width
) {
  if (!length(lines)) {
    return(list())
  }
  width = suppressWarnings(as.numeric(width[1]))
  snap_distance = suppressWarnings(as.numeric(snap_distance[1]))
  if (!is.finite(width) || width <= 0 || !is.finite(snap_distance)) {
    return(lines)
  }
  source_lines = lines
  clamped_lines = lines
  for (line_index in seq_along(source_lines)) {
    line = source_lines[[line_index]]
    if (nrow(line) < 2) {
      next
    }
    endpoint_indices = unique(c(1L, nrow(line)))
    for (endpoint_index in endpoint_indices) {
      point = line[endpoint_index, ]
      nearest_distance = Inf
      nearest_point = NULL
      for (candidate_index in seq_along(source_lines)) {
        if (candidate_index == line_index) {
          next
        }
        candidate = source_lines[[candidate_index]]
        if (nrow(candidate) < 2) {
          next
        }
        segment_start = candidate[-nrow(candidate), , drop = FALSE]
        segment_end = candidate[-1L, , drop = FALSE]
        segment_delta = segment_end - segment_start
        segment_length2 = rowSums(segment_delta^2)
        valid_segment = is.finite(segment_length2) &
          segment_length2 > .Machine$double.eps
        if (!any(valid_segment)) {
          next
        }
        segment_start = segment_start[valid_segment, , drop = FALSE]
        segment_delta = segment_delta[valid_segment, , drop = FALSE]
        segment_length2 = segment_length2[valid_segment]
        point_delta = sweep(segment_start, 2, point, FUN = "-")
        t_value = -rowSums(point_delta * segment_delta) / segment_length2
        t_value = pmin(pmax(t_value, 0), 1)
        projected = segment_start + segment_delta * t_value
        distance = sqrt(rowSums(
          sweep(projected, 2, point, FUN = "-")^2
        ))
        best_index = which.min(distance)
        if (
          length(best_index) &&
            is.finite(distance[[best_index]]) &&
            distance[[best_index]] < nearest_distance
        ) {
          nearest_distance = distance[[best_index]]
          nearest_point = projected[best_index, ]
        }
      }
      if (
        !is.null(nearest_point) &&
          is.finite(nearest_distance) &&
          nearest_distance <= snap_distance
      ) {
        clamped_lines[[line_index]][endpoint_index, ] = nearest_point
      }
    }
  }
  precision = max(width, 1) * 1e-8
  out = list()
  for (line in clamped_lines) {
    line = round(line / precision) * precision
    line = collapse_render_highquality_water_path_line(line)
    if (nrow(line) >= 2) {
      out[[length(out) + 1L]] = line
    }
  }
  out
}

#' Make joined water path buffer footprint
#'
#' @param lines List of two-column `x`/`z` line matrices.
#' @param width Stream width in scene units.
#'
#' @return `sf` polygon geometry.
#' @keywords internal
make_render_highquality_water_path_buffer_footprint = function(lines, width) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  if (!length(lines)) {
    return(NULL)
  }
  line_geometries = lapply(lines, sf::st_linestring)
  line_sfc = do.call(sf::st_sfc, c(line_geometries, list(crs = sf::NA_crs_)))
  buffered = suppressWarnings(sf::st_buffer(
    line_sfc,
    dist = width / 2,
    nQuadSegs = 1,
    endCapStyle = "FLAT",
    joinStyle = "MITRE",
    mitreLimit = 1
  ))
  footprint = suppressWarnings(sf::st_union(buffered))
  footprint = validate_render_highquality_water_path_footprint(footprint)
  if (is.null(footprint) || !length(footprint)) {
    return(NULL)
  }
  area = suppressWarnings(as.numeric(sf::st_area(footprint)))
  keep = is.finite(area) & area > max(width, 1)^2 * 1e-12
  footprint = footprint[keep]
  if (!length(footprint) || all(sf::st_is_empty(footprint))) {
    return(NULL)
  }
  footprint
}

#' Validate water path footprint geometry
#'
#' @param footprint Footprint geometry.
#'
#' @return Valid polygon geometry or `NULL`.
#' @keywords internal
validate_render_highquality_water_path_footprint = function(footprint) {
  if (is.null(footprint) || !length(footprint)) {
    return(NULL)
  }
  valid = tryCatch(
    suppressWarnings(sf::st_make_valid(footprint)),
    error = function(e) NULL
  )
  if (is.null(valid)) {
    valid = tryCatch(
      suppressWarnings(sf::st_buffer(footprint, 0)),
      error = function(e) NULL
    )
  }
  if (is.null(valid) || !length(valid)) {
    return(NULL)
  }
  valid = suppressWarnings(sf::st_collection_extract(valid, "POLYGON"))
  if (!length(valid) || all(sf::st_is_empty(valid))) {
    return(NULL)
  }
  valid[!sf::st_is_empty(valid)]
}

#' Make valid terrain triangle footprints for water paths
#'
#' @param heightmap Heightmap matrix in scene units.
#' @param bbox Default `NULL`. Optional stream footprint bounding box.
#' @param margin Default `0`. Extra scene-unit margin around `bbox`.
#'
#' @return `sf` object containing terrain triangle footprints.
#' @keywords internal
make_render_highquality_water_path_valid_terrain_triangles = function(
  heightmap,
  bbox = NULL,
  margin = 0
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  empty = make_empty_render_highquality_water_path_terrain_triangles()
  if (
    is.null(heightmap) ||
      !is.matrix(heightmap) ||
      nrow(heightmap) < 2 ||
      ncol(heightmap) < 2
  ) {
    return(empty)
  }
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  row_range = c(1L, nr - 1L)
  col_range = c(1L, nc - 1L)
  if (!is.null(bbox)) {
    margin = suppressWarnings(as.numeric(margin[1]))
    if (!is.finite(margin) || margin < 0) {
      margin = 0
    }
    bbox = as.numeric(bbox)
    if (length(bbox) >= 4 && all(is.finite(bbox[1:4]))) {
      row_range = render_highquality_water_path_bbox_cell_range(
        min_value = bbox[[1]] - margin,
        max_value = bbox[[3]] + margin,
        center = (nr - 1) / 2,
        upper = nr - 1L
      )
      col_range = render_highquality_water_path_bbox_cell_range(
        min_value = bbox[[2]] - margin,
        max_value = bbox[[4]] + margin,
        center = (nc - 1) / 2,
        upper = nc - 1L
      )
    }
  }
  if (row_range[[1]] > row_range[[2]] || col_range[[1]] > col_range[[2]]) {
    return(empty)
  }
  row_center = (nr - 1) / 2
  col_center = (nc - 1) / 2
  geometries = list()
  tri_id = integer(0)
  rows = integer(0)
  cols = integer(0)
  triangle = character(0)
  next_id = 1L
  for (row in seq.int(row_range[[1]], row_range[[2]])) {
    for (col in seq.int(col_range[[1]], col_range[[2]])) {
      cell_heights = heightmap[cbind(
        c(row, row + 1L, row, row + 1L),
        c(col, col, col + 1L, col + 1L)
      )]
      if (!all(is.finite(cell_heights))) {
        next
      }
      x0 = row - 1 - row_center
      x1 = row - row_center
      z0 = col - 1 - col_center
      z1 = col - col_center
      top_ring = rbind(
        c(x0, z0),
        c(x1, z0),
        c(x0, z1),
        c(x0, z0)
      )
      bottom_ring = rbind(
        c(x1, z1),
        c(x0, z1),
        c(x1, z0),
        c(x1, z1)
      )
      geometries[[length(geometries) + 1L]] = sf::st_polygon(list(top_ring))
      tri_id = c(tri_id, next_id)
      rows = c(rows, row)
      cols = c(cols, col)
      triangle = c(triangle, "top")
      next_id = next_id + 1L
      geometries[[length(geometries) + 1L]] = sf::st_polygon(list(bottom_ring))
      tri_id = c(tri_id, next_id)
      rows = c(rows, row)
      cols = c(cols, col)
      triangle = c(triangle, "bottom")
      next_id = next_id + 1L
    }
  }
  if (!length(geometries)) {
    return(empty)
  }
  sf::st_sf(
    tri_id = tri_id,
    row = rows,
    col = cols,
    triangle = triangle,
    geometry = do.call(sf::st_sfc, c(geometries, list(crs = sf::NA_crs_)))
  )
}

#' Make empty terrain triangle sf
#'
#' @return Empty terrain triangle sf object.
#' @keywords internal
make_empty_render_highquality_water_path_terrain_triangles = function() {
  sf::st_sf(
    tri_id = integer(),
    row = integer(),
    col = integer(),
    triangle = character(),
    geometry = sf::st_sfc(crs = sf::NA_crs_)
  )
}

#' Convert a scene coordinate range to terrain cell indices
#'
#' @param min_value Minimum scene coordinate.
#' @param max_value Maximum scene coordinate.
#' @param center Terrain center offset.
#' @param upper Maximum cell index.
#'
#' @return Integer length-two cell range.
#' @keywords internal
render_highquality_water_path_bbox_cell_range = function(
  min_value,
  max_value,
  center,
  upper
) {
  start = max(1L, floor(min_value + center + 1) - 1L)
  end = min(upper, ceiling(max_value + center + 1))
  c(as.integer(start), as.integer(end))
}

#' Clip water path footprint to valid terrain
#'
#' @param footprint Stream footprint geometry.
#' @param terrain_triangles Terrain triangle `sf` object.
#' @param area_epsilon Minimum fragment area.
#'
#' @return `sf` polygon fragments.
#' @keywords internal
clip_render_highquality_water_path_footprint_to_terrain = function(
  footprint,
  terrain_triangles,
  area_epsilon = 1e-12
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  empty = terrain_triangles[0, , drop = FALSE]
  if (
    is.null(footprint) ||
      !length(footprint) ||
      !nrow(terrain_triangles)
  ) {
    return(empty)
  }
  footprint_sf = sf::st_sf(geometry = footprint)
  fragments = tryCatch(
    suppressWarnings(sf::st_intersection(terrain_triangles, footprint_sf)),
    error = function(e) NULL
  )
  if (is.null(fragments) || !nrow(fragments)) {
    return(empty)
  }
  fragments = suppressWarnings(sf::st_collection_extract(fragments, "POLYGON"))
  fragments = suppressWarnings(sf::st_cast(fragments, "POLYGON", warn = FALSE))
  if (!nrow(fragments)) {
    return(empty)
  }
  area = suppressWarnings(as.numeric(sf::st_area(fragments)))
  keep = is.finite(area) & area > area_epsilon & !sf::st_is_empty(fragments)
  fragments[keep, , drop = FALSE]
}

#' Triangulate water path footprint fragments
#'
#' @param fragments Clipped footprint fragments.
#' @param width Default `1`. Stream width in scene units.
#'
#' @return List containing `vertices_xz`, `indices`, and `vertex_tri_id`.
#' @keywords internal
triangulate_render_highquality_water_path_fragments = function(
  fragments,
  width = 1
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  if (is.null(fragments) || !nrow(fragments)) {
    return(NULL)
  }
  precision = max(width, 1) * 1e-8
  vertices = matrix(numeric(0), ncol = 2)
  indices = matrix(integer(0), ncol = 3)
  vertex_tri_id = integer(0)
  vertex_lookup = integer(0)
  for (fragment_index in seq_len(nrow(fragments))) {
    fragment = sf::st_geometry(fragments[fragment_index, , drop = FALSE])
    triangles = triangulate_render_highquality_water_path_fragment(fragment)
    if (is.null(triangles) || !length(triangles)) {
      next
    }
    for (triangle_index in seq_along(triangles)) {
      triangle_vertices = extract_render_highquality_water_path_triangle_vertices(
        triangles[[triangle_index]]
      )
      for (triangle_vertex in triangle_vertices) {
        area2 = render_highquality_water_path_signed_area2(triangle_vertex)
        if (!is.finite(area2) || abs(area2) <= precision^2) {
          next
        }
        keys = paste(
          round(triangle_vertex[, 1] / precision),
          round(triangle_vertex[, 2] / precision),
          sep = "_"
        )
        triangle_indices = integer(3)
        for (vertex_index in seq_len(3L)) {
          key = keys[[vertex_index]]
          if (key %in% names(vertex_lookup)) {
            lookup_index = vertex_lookup[[key]]
          } else {
            vertices = rbind(vertices, triangle_vertex[vertex_index, ])
            vertex_tri_id = c(vertex_tri_id, fragments$tri_id[[fragment_index]])
            lookup_index = nrow(vertices)
            vertex_lookup[[key]] = lookup_index
          }
          triangle_indices[[vertex_index]] = lookup_index
        }
        if (length(unique(triangle_indices)) == 3L) {
          indices = rbind(indices, triangle_indices)
        }
      }
    }
  }
  list(
    vertices_xz = vertices,
    indices = indices,
    vertex_tri_id = vertex_tri_id
  )
}

#' Triangulate one water path fragment
#'
#' @param fragment Fragment geometry.
#'
#' @return Triangle polygon geometry.
#' @keywords internal
triangulate_render_highquality_water_path_fragment = function(fragment) {
  triangles = NULL
  if (exists("st_triangulate_constrained", envir = asNamespace("sf"))) {
    triangles = tryCatch(
      suppressWarnings(sf::st_triangulate_constrained(fragment)),
      error = function(e) NULL
    )
    triangles = extract_render_highquality_water_path_triangle_polygons(
      triangles,
      fragment
    )
  }
  if (is.null(triangles) || !length(triangles)) {
    triangles = tryCatch(
      suppressWarnings(sf::st_triangulate(fragment)),
      error = function(e) NULL
    )
    triangles = extract_render_highquality_water_path_triangle_polygons(
      triangles,
      fragment
    )
  }
  if (is.null(triangles) || !length(triangles)) {
    triangles = earclip_render_highquality_water_path_fragment(fragment)
  }
  triangles
}

#' Extract triangle polygons
#'
#' @param triangles Triangle geometry.
#' @param fragment Source fragment geometry.
#'
#' @return Triangle polygon geometry.
#' @keywords internal
extract_render_highquality_water_path_triangle_polygons = function(
  triangles,
  fragment
) {
  if (is.null(triangles) || !length(triangles)) {
    return(NULL)
  }
  triangles = suppressWarnings(sf::st_collection_extract(triangles, "POLYGON"))
  triangles = suppressWarnings(sf::st_cast(triangles, "POLYGON", warn = FALSE))
  if (!length(triangles)) {
    return(NULL)
  }
  triangles = triangles[!sf::st_is_empty(triangles)]
  if (!length(triangles)) {
    return(NULL)
  }
  representative_points = suppressWarnings(sf::st_point_on_surface(triangles))
  inside = tryCatch(
    as.logical(sf::st_covered_by(
      representative_points,
      fragment,
      sparse = FALSE
    )[, 1]),
    error = function(e) rep(TRUE, length(triangles))
  )
  triangles[inside]
}

#' Ear-clip a simple water path fragment
#'
#' @param fragment Fragment geometry.
#'
#' @return Triangle polygon geometry or `NULL`.
#' @keywords internal
earclip_render_highquality_water_path_fragment = function(fragment) {
  geometry = fragment[[1]]
  if (!inherits(geometry, "XY") || !inherits(geometry, "POLYGON")) {
    return(NULL)
  }
  if (length(geometry) != 1L) {
    return(NULL)
  }
  ring = geometry[[1]]
  if (nrow(ring) > 1L && all(ring[1, ] == ring[nrow(ring), ])) {
    ring = ring[-nrow(ring), , drop = FALSE]
  }
  ring = collapse_render_highquality_water_path_line(ring)
  if (nrow(ring) < 3) {
    return(NULL)
  }
  if (render_highquality_water_path_ring_area(ring) < 0) {
    ring = ring[rev(seq_len(nrow(ring))), , drop = FALSE]
  }
  remaining = seq_len(nrow(ring))
  triangles = list()
  guard = 0L
  while (length(remaining) > 3L && guard < nrow(ring)^2) {
    guard = guard + 1L
    clipped = FALSE
    for (position in seq_along(remaining)) {
      previous = remaining[[ifelse(
        position == 1L,
        length(remaining),
        position - 1L
      )]]
      current = remaining[[position]]
      next_val = remaining[[ifelse(
        position == length(remaining),
        1L,
        position + 1L
      )]]
      ear = ring[c(previous, current, next_val), , drop = FALSE]
      if (render_highquality_water_path_signed_area2(ear) <= 0) {
        next
      }
      other = setdiff(remaining, c(previous, current, next_val))
      if (
        length(other) &&
          any(render_highquality_water_path_points_in_triangle(
            ring[other, , drop = FALSE],
            ear
          ))
      ) {
        next
      }
      triangles[[length(triangles) + 1L]] = ear
      remaining = remaining[-position]
      clipped = TRUE
      break
    }
    if (!clipped) {
      return(NULL)
    }
  }
  if (length(remaining) == 3L) {
    triangles[[length(triangles) + 1L]] = ring[remaining, , drop = FALSE]
  }
  if (!length(triangles)) {
    return(NULL)
  }
  triangle_geometries = lapply(triangles, function(triangle) {
    sf::st_polygon(list(rbind(triangle, triangle[1, ])))
  })
  do.call(sf::st_sfc, c(triangle_geometries, list(crs = sf::NA_crs_)))
}

#' Extract triangle vertices
#'
#' @param geometry Triangle geometry.
#'
#' @return List of three-row `x`/`z` matrices.
#' @keywords internal
extract_render_highquality_water_path_triangle_vertices = function(geometry) {
  coords = sf::st_coordinates(geometry)
  if (!nrow(coords)) {
    return(list())
  }
  coords = coords[, 1:2, drop = FALSE]
  if (nrow(coords) > 1L && all(coords[1, ] == coords[nrow(coords), ])) {
    coords = coords[-nrow(coords), , drop = FALSE]
  }
  coords = collapse_render_highquality_water_path_line(coords)
  if (nrow(coords) < 3) {
    return(list())
  }
  if (nrow(coords) == 3L) {
    return(list(coords))
  }
  out = vector("list", nrow(coords) - 2L)
  for (index in seq_len(nrow(coords) - 2L)) {
    out[[index]] = coords[c(1L, index + 1L, index + 2L), , drop = FALSE]
  }
  out
}

#' Sample joined water path terrain surface
#'
#' @param points_xz Two-column `x`/`z` point matrix.
#' @param heightmap Heightmap matrix in scene units.
#' @param terrain_triangles Default `NULL`. Terrain triangle metadata.
#' @param tri_id Default `NULL`. Terrain triangle id for each point.
#'
#' @return Numeric terrain heights.
#' @keywords internal
sample_render_highquality_water_path_surface = function(
  points_xz,
  heightmap,
  terrain_triangles = NULL,
  tri_id = NULL
) {
  points_xz = as.matrix(points_xz)
  heights = interpolate_spatial_water_height(
    heightmap,
    points_xz[, 1],
    points_xz[, 2]
  )
  if (
    !is.null(terrain_triangles) &&
      !is.null(tri_id) &&
      length(tri_id) == nrow(points_xz)
  ) {
    triangle_index = match(tri_id, terrain_triangles$tri_id)
    valid = !is.na(triangle_index)
    exact_heights = rep(NA_real_, nrow(points_xz))
    if (any(valid)) {
      exact_heights[
        valid
      ] = calculate_render_highquality_water_path_triangle_height(
        heightmap = heightmap,
        points_xz = points_xz[valid, , drop = FALSE],
        terrain_triangles = terrain_triangles[
          triangle_index[valid],
          ,
          drop = FALSE
        ]
      )
      heights[is.finite(exact_heights)] = exact_heights[is.finite(
        exact_heights
      )]
    }
  }
  heights
}

#' Calculate terrain triangle heights
#'
#' @param heightmap Heightmap matrix in scene units.
#' @param points_xz Two-column `x`/`z` point matrix.
#' @param terrain_triangles Terrain triangle metadata.
#'
#' @return Numeric heights.
#' @keywords internal
calculate_render_highquality_water_path_triangle_height = function(
  heightmap,
  points_xz,
  terrain_triangles
) {
  row_col = spatial_water_row_col(
    heightmap,
    points_xz[, 1],
    points_xz[, 2],
    clamp = FALSE
  )
  row_weight = row_col$row - terrain_triangles$row
  col_weight = row_col$col - terrain_triangles$col
  height00 = heightmap[cbind(terrain_triangles$row, terrain_triangles$col)]
  height10 = heightmap[cbind(terrain_triangles$row + 1L, terrain_triangles$col)]
  height01 = heightmap[cbind(terrain_triangles$row, terrain_triangles$col + 1L)]
  height11 = heightmap[cbind(
    terrain_triangles$row + 1L,
    terrain_triangles$col + 1L
  )]
  heights = numeric(nrow(points_xz))
  top_triangle = terrain_triangles$triangle == "top"
  heights[top_triangle] = height00[top_triangle] +
    row_weight[top_triangle] *
      (height10[top_triangle] - height00[top_triangle]) +
    col_weight[top_triangle] *
      (height01[top_triangle] - height00[top_triangle])
  heights[!top_triangle] = height11[!top_triangle] +
    (1 - col_weight[!top_triangle]) *
      (height10[!top_triangle] - height11[!top_triangle]) +
    (1 - row_weight[!top_triangle]) *
      (height01[!top_triangle] - height11[!top_triangle])
  heights
}

#' Orient joined water path top triangles upward
#'
#' @param vertices Top vertex matrix.
#' @param indices Top triangle indices.
#'
#' @return Oriented triangle index matrix.
#' @keywords internal
orient_render_highquality_water_path_top_indices = function(vertices, indices) {
  if (!nrow(indices)) {
    return(indices)
  }
  oriented = indices
  keep = rep(TRUE, nrow(oriented))
  for (index in seq_len(nrow(oriented))) {
    triangle = vertices[oriented[index, ], , drop = FALSE]
    first_edge = triangle[2L, ] - triangle[1L, ]
    second_edge = triangle[3L, ] - triangle[1L, ]
    normal = c(
      first_edge[[2]] * second_edge[[3]] - first_edge[[3]] * second_edge[[2]],
      first_edge[[3]] * second_edge[[1]] - first_edge[[1]] * second_edge[[3]],
      first_edge[[1]] * second_edge[[2]] - first_edge[[2]] * second_edge[[1]]
    )
    if (!all(is.finite(normal)) || sqrt(sum(normal^2)) <= .Machine$double.eps) {
      keep[[index]] = FALSE
      next_val
    }
    if (normal[[2]] < 0) {
      oriented[index, c(2L, 3L)] = oriented[index, c(3L, 2L)]
    }
  }
  oriented[keep, , drop = FALSE]
}

#' Make joined water path side faces
#'
#' @param top_indices Oriented top triangle indices.
#'
#' @return Side triangle index matrix.
#' @keywords internal
make_render_highquality_water_path_side_faces = function(top_indices) {
  if (!nrow(top_indices)) {
    return(matrix(integer(0), ncol = 3))
  }
  vertex_count = max(top_indices)
  directed_edges = rbind(
    top_indices[, c(1L, 2L), drop = FALSE],
    top_indices[, c(2L, 3L), drop = FALSE],
    top_indices[, c(3L, 1L), drop = FALSE]
  )
  edge_keys = paste(
    pmin(directed_edges[, 1], directed_edges[, 2]),
    pmax(directed_edges[, 1], directed_edges[, 2]),
    sep = "_"
  )
  edge_counts = tabulate(match(edge_keys, unique(edge_keys)))
  boundary_edges = directed_edges[
    edge_counts[match(edge_keys, unique(edge_keys))] == 1L,
    ,
    drop = FALSE
  ]
  if (!nrow(boundary_edges)) {
    return(matrix(integer(0), ncol = 3))
  }
  a = boundary_edges[, 1]
  b = boundary_edges[, 2]
  rbind(
    cbind(a, b + vertex_count, b),
    cbind(a, a + vertex_count, b + vertex_count)
  )
}

#' Count undirected mesh triangle edges
#'
#' @param indices Triangle index matrix.
#'
#' @return Named integer edge counts.
#' @keywords internal
validate_render_highquality_water_path_edge_counts = function(indices) {
  if (!nrow(indices)) {
    return(integer(0))
  }
  edges = rbind(
    indices[, c(1L, 2L), drop = FALSE],
    indices[, c(2L, 3L), drop = FALSE],
    indices[, c(3L, 1L), drop = FALSE]
  )
  keys = paste(
    pmin(edges[, 1], edges[, 2]),
    pmax(edges[, 1], edges[, 2]),
    sep = "_"
  )
  stats::setNames(tabulate(match(keys, unique(keys))), unique(keys))
}

#' Calculate twice signed triangle area in x/z space
#'
#' @param points Three-row point matrix.
#'
#' @return Signed area times two.
#' @keywords internal
render_highquality_water_path_signed_area2 = function(points) {
  (points[2L, 1] - points[1L, 1]) *
    (points[3L, 2] - points[1L, 2]) -
    (points[2L, 2] - points[1L, 2]) * (points[3L, 1] - points[1L, 1])
}

#' Calculate polygon ring area
#'
#' @param points Ring points.
#'
#' @return Signed area.
#' @keywords internal
render_highquality_water_path_ring_area = function(points) {
  next_index = c(seq_len(nrow(points))[-1L], 1L)
  sum(
    points[, 1] * points[next_index, 2] - points[next_index, 1] * points[, 2]
  ) /
    2
}

#' Test whether points are inside a triangle
#'
#' @param points Point matrix.
#' @param triangle Triangle point matrix.
#'
#' @return Logical vector.
#' @keywords internal
render_highquality_water_path_points_in_triangle = function(points, triangle) {
  area = render_highquality_water_path_signed_area2(triangle)
  if (!is.finite(area) || abs(area) <= .Machine$double.eps) {
    return(rep(FALSE, nrow(points)))
  }
  sign1 = render_highquality_water_path_edge_side(
    points,
    triangle[1L, ],
    triangle[2L, ]
  )
  sign2 = render_highquality_water_path_edge_side(
    points,
    triangle[2L, ],
    triangle[3L, ]
  )
  sign3 = render_highquality_water_path_edge_side(
    points,
    triangle[3L, ],
    triangle[1L, ]
  )
  eps = sqrt(.Machine$double.eps)
  (sign1 >= -eps & sign2 >= -eps & sign3 >= -eps) |
    (sign1 <= eps & sign2 <= eps & sign3 <= eps)
}

#' Calculate point side of a directed edge
#'
#' @param points Point matrix.
#' @param edge_start Edge start.
#' @param edge_end Edge end.
#'
#' @return Signed edge side values.
#' @keywords internal
render_highquality_water_path_edge_side = function(
  points,
  edge_start,
  edge_end
) {
  (points[, 1] - edge_start[[1]]) *
    (edge_end[[2]] - edge_start[[2]]) -
    (points[, 2] - edge_start[[2]]) * (edge_end[[1]] - edge_start[[1]])
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
