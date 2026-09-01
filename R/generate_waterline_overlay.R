#'@title Generate Waterline Overlay
#'
#'@description Using a height map or a boolean matrix, generates a semi-transparent waterline overlay to
#'layer onto an existing map. This uses the method described by P. Felzenszwalb & D. Huttenlocher in
#'"Distance Transforms of Sampled Functions" (Theory of Computing, Vol. 8, No. 19, September 2012)
#'to calculate the distance to the coast. This distance matrix can be returned directly by setting
#'the `return_distance_matrix` argument to `TRUE`.
#'
#'@param heightmap Default `NULL`. A two-dimensional matrix, where each entry
#'in the matrix is the elevation at that point. If `boolean = TRUE`, this will
#'instead be interpreted as a logical matrix indicating areas of water. If
#'omitted, rayshader uses the cached hillshade or scene heightmap.
#'@param width Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param resolution_multiply Default `1`. If passing in `heightmap` instead of width/height, amount to
#'increase the resolution of the overlay, which should make lines/polygons/text finer.
#'Should be combined with \code{\link[=add_overlay]{add_overlay()}} with `rescale_original = TRUE` to ensure those added details are captured
#'in the final map.
#'@param color Default `white`. Color of the lines.
#'@param linewidth Default `1`. Line width.
#'@param boolean Default `FALSE`. If `TRUE`, this is a boolean matrix (0 and 1) indicating contiguous areas in
#'which the lines are generated (instead of a height matrix, from which the boolean matrix is derived using [detect_water()])
#'@param min Default `0.001`. Percent distance (measured from the furthest point from shore) where the waterlines stop.
#'@param max Default `0.2`. Percent distance (measured from the furthest point from shore) where the waterlines begin.
#'@param breaks Default `9`. Number of water lines.
#'@param smooth Default `0`, no smoothing. Increase this to smooth water lines around corners.
#'@param fade Default `TRUE`. If `FALSE`, lines will not fade with distance from shore.
#'@param alpha_dist Default to the value specified in `max`. Percent distance (measured from the furthest point from shore) where the
#'waterlines fade entirely, when `fade = TRUE`.
#'@param alpha Default `1`. Maximum transparency for waterlines. This scales the transparency for all other levels.
#'@param falloff Default `1.3`. Multiplicative decrease in distance between each waterline level.
#'@param evenly_spaced Default `FALSE`. If `TRUE`, `falloff` will be ignored and the lines will be evenly spaced.
#'@param zscale Default `1`. Base `zscale` used for water detection when `boolean = FALSE`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be
#'10. If omitted, rayshader uses raster-derived or matching cached metadata.
#'@param cutoff Default `0.999`. Arguments passed to [detect_water()]. Ignored if `boolean = TRUE`.The lower limit of the z-component of the unit normal vector to be classified as water.
#'@param min_area Default `NULL`, equivalent to `length(heightmap)/400` after
#'the heightmap is resolved. Arguments passed to [detect_water()]. Ignored if
#'`boolean = TRUE`. Minimum area (in grid cells) to be considered water.
#'@param max_height Default `NULL`. Arguments passed to [detect_water()]. Ignored if `boolean = TRUE`. If passed, this number will specify the maximum height a point can be considered to be water.
#'@param geographic_aspect Default `TRUE`. If `TRUE`, use supplied or cached
#'spatial metadata when detecting water and measuring distance from shore.
#'@param extent Default `NULL`. Spatial extent for a matrix heightmap.
#'@param crs Default `NULL`. CRS describing the input heightmap. An explicit
#'value overrides embedded metadata on a copy of the input.
#'`FALSE`, the direction will be reversed.
#'@param return_distance_matrix Default `FALSE`. If `TRUE`, this function will return the boolean distance matrix instead of
#'contour lines.
#'@return 4-layer RGB array representing the waterline overlay.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' #Create a flat body of water for Monterey Bay
#' montbay = raster_to_matrix(montereybay_spatial)
#' montbay[montbay < 0] = 0
#'
#' #Generate base map with no lines
#' basemap = montbay |>
#'   height_shade() |>
#'   add_water(detect_water(montbay, zscale=200), color="dodgerblue4") |>
#'   add_shadow(texture_shade(montbay, detail=1/3, brightness = 15, contrast = 5),0) |>
#'   add_shadow(lamb_shade(montbay, zscale=200, vertical_exaggeration = 4),0)
#'
#' plot_map(basemap)
#' #Add waterlines
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay)) |>
#'   plot_map()
#' #Change minimum line distance:
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay, min = 0.02)) |>
#'   plot_map()
#' #Change maximum line distance
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay, max = 0.4)) |>
#'   plot_map()
#' #Smooth waterlines
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay, max = 0.4, smooth=2)) |>
#'   plot_map()
#' #Increase number of breaks
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay, breaks = 20, max=0.4)) |>
#'   plot_map()
#' #Make lines evenly spaced:
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay, evenly_spaced = TRUE)) |>
#'   plot_map()
#' #Change variable distance between each line
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay, falloff=1.5)) |>
#'   plot_map()
#' #Turn off fading
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay, fade=FALSE)) |>
#'   plot_map()
#' #Fill up the entire body of water with lines and make them all 50% transparent
#' basemap |>
#'   add_overlay(generate_waterline_overlay(montbay, fade=FALSE, max=1, alpha = 0.5, color="white",
#'                                          evenly_spaced = TRUE, breaks=50)) |>
#'   plot_map()
generate_waterline_overlay = function(
  heightmap = NULL,
  color = "white",
  linewidth = 1,
  boolean = FALSE,
  min = 0.001,
  max = 0.20,
  breaks = 9,
  smooth = 0,
  fade = TRUE,
  alpha_dist = max,
  alpha = 1,
  falloff = 1.3,
  evenly_spaced = FALSE,
  zscale = 1,
  cutoff = 0.9999999,
  width = NA,
  height = NA,
  resolution_multiply = 1,
  min_area = NULL,
  max_height = NULL,
  return_distance_matrix = FALSE,
  geographic_aspect = TRUE,
  extent = NULL,
  crs = NULL
) {
  heightmap_missing = missing(heightmap) || is.null(heightmap)
  zscale_missing = missing(zscale)
  extent_missing = missing(extent)
  crs_missing = missing(crs)
  if (heightmap_missing) {
    resolved_heightmap = resolve_hillshade_heightmap(
      heightmap_missing = TRUE,
      caller = "generate_waterline_overlay"
    )
    heightmap = resolved_heightmap$heightmap
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
  }
  if (is.null(min_area)) {
    min_area = length(heightmap) / 400
  }
  breaks = breaks + 1
  if (smooth < 0 || !is.numeric(smooth)) {
    stop("`smooth` should be a numeric value greater than or equal to zero.")
  }
  if (breaks < 1) {
    stop("`breaks` should be a value greater than or equal to one.")
  }
  if (alpha > 1 || alpha < 0) {
    stop("`alpha` should be a value greater than zero or less than one")
  }
  if (!boolean) {
    detect_water_args = list(
      heightmap = heightmap,
      cutoff = cutoff,
      min_area = min_area,
      max_height = max_height,
      geographic_aspect = geographic_aspect,
      extent = extent,
      crs = crs
    )
    if (!zscale_missing) {
      detect_water_args$zscale = zscale
    }
    is_water = do.call(detect_water, detect_water_args)
  } else {
    heightmap_info = coerce_plot_3d_heightmap(
      heightmap,
      extent = extent,
      crs = crs,
      geographic_aspect = geographic_aspect
    )
    is_water = heightmap_info$heightmap
    attr(is_water, "rayshader_geographic_aspect") =
      heightmap_info$geographic_aspect
  }
  water_aspect = normalize_geographic_aspect(attr(
    is_water,
    "rayshader_geographic_aspect",
    exact = TRUE
  ))
  water_dist = calculate_waterline_distance(
    is_water != 1,
    geographic_aspect = water_aspect
  )
  if (return_distance_matrix) {
    result = flipud(water_dist)
    attr(result, "rayshader_geographic_aspect") = water_aspect
    return(result)
  }
  water_dist_bool = scales::rescale(water_dist, to = c(0, 1))
  if (smooth != 0) {
    water_dist_bool = rayimage::render_convolution(
      water_dist_bool,
      kernel = smooth,
      kernel_dim = 21
    )
    water_dist_bool[!is_water] = 0
    water_dist_bool = scales::rescale(unclass(water_dist_bool), to = c(0, 1))
    class(water_dist_bool) = c('rayimg', 'matrix', 'array')
  }
  water_dist_bool = flipud(water_dist_bool)
  if (!evenly_spaced) {
    levels = rep(0, breaks)
    temp = max
    for (i in seq_len(breaks)) {
      levels[i] = temp
      temp = temp / falloff
    }
  } else {
    levels = seq(0, 1, length.out = breaks)
  }
  levels = scales::rescale(levels, to = c(min, max))
  overlay = generate_contour_overlay(
    water_dist_bool,
    levels = levels,
    width = width,
    height = height,
    resolution_multiply = resolution_multiply,
    color = color,
    linewidth = linewidth
  )
  if (fade) {
    alpha_vals = water_dist_bool
    alpha_vals = alpha_vals / alpha_dist
    alpha_vals[alpha_vals > 1] = 1
    alpha_vals = 1 - alpha_vals
    overlay[,, 4] = overlay[,, 4] * (t(alpha_vals))
  }
  overlay[,, 4] = overlay[,, 4] * alpha
  return(overlay)
}

#' Calculate distance from water cells to shore
#'
#' @param non_water Logical matrix whose `TRUE` cells are shoreline targets.
#' @param geographic_aspect Geographic aspect metadata.
#'
#' @return Distance matrix in normalized mean-cell units.
#' @keywords internal
calculate_waterline_distance = function(
  non_water,
  geographic_aspect = identity_geographic_aspect()
) {
  aspect = normalize_geographic_aspect(geographic_aspect)
  if (!isTRUE(aspect$enabled)) {
    return(rayimage::render_boolean_distance(non_water))
  }
  if (anyNA(non_water)) {
    stop("`non_water` must not contain missing values.", call. = FALSE)
  }
  if (all(non_water) || !any(non_water)) {
    return(rayimage::render_boolean_distance(non_water))
  }
  distance_raster = terra::rast(
    nrows = nrow(non_water),
    ncols = ncol(non_water),
    xmin = 0,
    xmax = ncol(non_water) * aspect$scale[["z"]],
    ymin = 0,
    ymax = nrow(non_water) * aspect$scale[["x"]],
    crs = "EPSG:3857"
  )
  water = !non_water
  adjacent_water = matrix(FALSE, nrow(non_water), ncol(non_water))
  for (row_offset in -1:1) {
    for (column_offset in -1:1) {
      if (row_offset == 0 && column_offset == 0) {
        next
      }
      source_rows = seq_len(nrow(water) - abs(row_offset))
      target_rows = source_rows + max(row_offset, 0)
      source_rows = source_rows + max(-row_offset, 0)
      source_columns = seq_len(ncol(water) - abs(column_offset))
      target_columns = source_columns + max(column_offset, 0)
      source_columns = source_columns + max(-column_offset, 0)
      adjacent_water[target_rows, target_columns] =
        adjacent_water[target_rows, target_columns] |
        water[source_rows, source_columns]
    }
  }
  shoreline = non_water & adjacent_water
  terra::values(distance_raster) = ifelse(
    as.vector(t(shoreline)),
    1,
    NA_real_
  )
  shoreline_points = terra::as.points(distance_raster, na.rm = TRUE)
  distance_values = terra::distance(distance_raster, shoreline_points)
  result = terra::as.matrix(distance_values, wide = TRUE)
  result[non_water] = 0
  result
}
