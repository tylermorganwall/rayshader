#'@title Detect water
#'
#'@description Detects bodies of water (of a user-defined minimum size) within an elevation matrix.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap Default `NULL`. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'All grid points are assumed to be evenly spaced. Alternatively, if heightmap is a logical matrix, each entry
#'specifies whether that point is water or not. If omitted, rayshader will use the cached hillshade/scene heightmap.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. If omitted and a cached or raster-derived
#'`zscale` is available, rayshader will reuse that value.
#'@param cutoff Default `1-1e-7`. The lower limit of the z-component of the unit normal vector to be classified as water.
#'@param min_area Default `NULL`, equivalent to `length(heightmap)/400` after
#'the heightmap is resolved. Minimum area (in grid cells) to be considered a
#'body of water.
#'@param max_height Default `NULL`. If passed, this number will specify the maximum height a point can be considered to be water.
#'@param normalvectors Default `NULL`. Pre-computed array of normal vectors from the [calculate_normal()] function. Supplying this will speed up water detection.
#'@param keep_groups Default `FALSE`. If `TRUE`, the matrix returned will retain the numbered grouping information.
#'@param progbar Default `FALSE`. If `TRUE`, turns on progress bar.
#'@param geographic_aspect Default `TRUE`. If `TRUE`, account for unequal
#'horizontal cell sizes using the supplied or cached spatial metadata.
#'@param extent Default `NULL`. Spatial extent for a matrix heightmap. If
#'omitted, rayshader uses matching cached metadata when available.
#'@param crs Default `NULL`. CRS describing the input heightmap. An explicit
#'value overrides embedded metadata on a copy of the input.
#'@return Matrix indicating whether water was detected at that point. 1 indicates water, 0 indicates no water.
#'@export
#'@examples
#'#Here we even out a portion of the volcano dataset to simulate water:
#'island_volcano = volcano
#'island_volcano[island_volcano < mean(island_volcano)] = mean(island_volcano)
#'
#'#Setting a minimum area avoids classifying small flat areas as water:
#'island_volcano |>
#'  sphere_shade(texture="imhof3", vertical_exaggeration = 20) |>
#'  add_water(detect_water(min_area = 400),color="imhof3") |>
#'  plot_map()
detect_water = function(
  heightmap = NULL,
  zscale = 1,
  cutoff = 0.9999999,
  min_area = NULL,
  max_height = NULL,
  normalvectors = NULL,
  keep_groups = FALSE,
  progbar = FALSE,
  geographic_aspect = TRUE,
  extent = NULL,
  crs = NULL
) {
  heightmap_missing = missing(heightmap) || is.null(heightmap)
  extent_missing = missing(extent)
  crs_missing = missing(crs)
  heightmap_cache_label = format_scene_cache_label(deparse(substitute(
    heightmap
  )))
  heightmap_auto_zscale = NA_real_
  if (is.logical(heightmap) && is.matrix(heightmap)) {
    aspect = if (isTRUE(geographic_aspect)) {
      normalize_geographic_aspect(attr(
        heightmap,
        "rayshader_geographic_aspect",
        exact = TRUE
      ))
    } else {
      identity_geographic_aspect()
    }
    result = flipud(heightmap)
    attr(result, "rayshader_geographic_aspect") = aspect
    return(result)
  }
  if (heightmap_missing) {
    resolved_heightmap = resolve_hillshade_heightmap(
      heightmap_missing = TRUE,
      caller = "detect_water"
    )
    heightmap = resolved_heightmap$heightmap
    allow_scene_zscale_cache = identical(resolved_heightmap$source, "scene")
    if (extent_missing) {
      extent = if (identical(resolved_heightmap$source, "scene")) {
        get_scene_extent(default = NULL)
      } else {
        get_hillshade_extent(default = NULL)
      }
    }
    if (crs_missing) {
      crs = if (identical(resolved_heightmap$source, "scene")) {
        get_scene_crs(default = NULL)
      } else {
        get_hillshade_crs(default = NULL)
      }
    }
  } else {
    allow_scene_zscale_cache = FALSE
  }
  heightmap_info = coerce_plot_3d_heightmap(
    heightmap,
    extent = extent,
    crs = crs,
    geographic_aspect = geographic_aspect
  )
  heightmap = heightmap_info$heightmap
  heightmap_auto_zscale = heightmap_info$zscale
  if (heightmap_missing) {
    heightmap_info$geographic_aspect = resolve_cached_geographic_aspect(
      source = resolved_heightmap$source,
      geographic_aspect = geographic_aspect,
      fallback = heightmap_info$geographic_aspect
    )
    if (is.finite(heightmap_info$geographic_aspect$mean_cell_meters)) {
      heightmap_auto_zscale = heightmap_info$geographic_aspect$mean_cell_meters
    }
  } else {
    cache_hillshade_input_context(heightmap_info, label = heightmap_cache_label)
  }
  attr(heightmap, "rayshader_geographic_aspect") =
    heightmap_info$geographic_aspect
  if (!is.matrix(heightmap)) {
    stop("`heightmap` must be a matrix.", call. = FALSE)
  }
  resolved_zscale = resolve_hillshade_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    caller = "detect_water",
    auto_zscale = heightmap_auto_zscale,
    allow_hillshade_cache = heightmap_missing,
    allow_scene_cache = allow_scene_zscale_cache
  )
  zscale = resolved_zscale$zscale
  if (is.null(min_area)) {
    min_area = length(heightmap) / 400
  }
  if (!is.null(normalvectors)) {
    normalvectors = correct_normal_geographic_aspect(
      normalvectors,
      heightmap_info$geographic_aspect
    )
    zmatrix = abs(normalvectors$z)
    zmatrix = abs(zmatrix)
    zmatrix[zmatrix < cutoff] = 0
    zmatrix[zmatrix >= cutoff] = 1
    zmatrix[1, ] = 0
    zmatrix[, 1] = 0
    zmatrix[nrow(zmatrix), ] = 0
    zmatrix[, ncol(zmatrix)] = 0
  } else {
    zmatrix = calculate_normal(heightmap, zscale = zscale, progbar = progbar)$z
    zmatrix = abs(zmatrix)
    zmatrix[zmatrix < cutoff] = 0
    zmatrix[zmatrix >= cutoff] = 1
    zmatrix[1, ] = 0
    zmatrix[, 1] = 0
    zmatrix[nrow(zmatrix), ] = 0
    zmatrix[, ncol(zmatrix)] = 0
  }
  if (!is.null(max_height)) {
    heightmap_padded = matrix(
      max_height + 1,
      nrow(heightmap) + 2,
      ncol(heightmap) + 2
    )
    heightmap_padded[
      2:(nrow(heightmap_padded) - 1),
      2:(ncol(heightmap_padded) - 1)
    ] = heightmap
    zmatrix[t(heightmap_padded) > max_height] = 0
  }
  padding = matrix(0, nrow = nrow(zmatrix) + 2, ncol = ncol(zmatrix) + 2)
  padding[2:(nrow(padding) - 1), 2:(ncol(padding) - 1)] = zmatrix

  water_groups = fill_find_groups(padding)
  group_table = table(water_groups[water_groups > 0])
  entries = names(group_table[group_table > min_area])
  water_groups[!(water_groups %in% entries)] = 0
  if (!keep_groups) {
    water_groups[water_groups != 0] = 1
  }

  water_groups2 = water_groups[
    c(-1, -2, -nrow(water_groups) + 1, -nrow(water_groups)),
    c(-1, -2, -ncol(water_groups) + 1, -ncol(water_groups))
  ]
  result = flipud(t(water_groups2))
  attr(result, "rayshader_geographic_aspect") =
    heightmap_info$geographic_aspect
  return(result)
}
