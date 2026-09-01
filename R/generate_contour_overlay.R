#'@title Generate Contour Overlay
#'
#'@description Calculates and returns an overlay of contour lines for the current height map.
#'
#'@param heightmap Default `NULL`. A two-dimensional matrix, where each entry
#'in the matrix is the elevation at that point. If omitted, rayshader uses the
#'cached hillshade or scene heightmap.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be
#'10. If omitted, rayshader uses raster-derived or matching cached metadata.
#'@param levels Default `NA`. Automatically generated with 10 levels. This argument specifies the exact height levels of each contour.
#'@param nlevels Default `NA`. Controls the auto-generation of levels. If levels is length-2,
#'this will automatically generate `nlevels` breaks between `levels[1]` and `levels[2]`.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as heightmap.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as heightmap.
#'@param resolution_multiply Default `1`. If passing in `heightmap` instead of width/height, amount to
#'increase the resolution of the overlay, which should make lines/polygons finer.
#'Should be combined with \code{\link[=add_overlay]{add_overlay()}} with `rescale_original = TRUE` to ensure those added details are captured
#'in the final map.
#'@param color Default `black`. Color.
#'@param linewidth Default `1`. Line width.
#'@param geographic_aspect Default `TRUE`. If `TRUE`, use supplied or cached
#'spatial metadata when deriving the default horizontal scale.
#'@param extent Default `NULL`. Spatial extent for a matrix heightmap.
#'@param crs Default `NULL`. CRS describing the input heightmap. An explicit
#'value overrides embedded metadata on a copy of the input.
#'@return Semi-transparent overlay with contours.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Add contours to the montereybay_spatial dataset
#'montereybay_spatial |>
#'  height_shade() |>
#'  add_overlay(generate_contour_overlay(montereybay_spatial))  |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  plot_map()
#'
#'#Add a different contour color for above and below water, and specify levels manually
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay_spatial, texture = water_palette)
#'monterey_range = range(raster_to_matrix(montereybay_spatial, verbose = FALSE), na.rm = TRUE)
#'breaks = seq(monterey_range[1], monterey_range[2], length.out=50)
#'water_breaks = breaks[breaks < 0]
#'land_breaks = breaks[breaks > 0]
#'
#'montereybay_spatial |>
#'  height_shade() |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay_spatial, 0, 0))  |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  add_overlay(generate_contour_overlay(montereybay_spatial, levels = water_breaks, color="white"))  |>
#'  add_overlay(generate_contour_overlay(montereybay_spatial, levels = land_breaks, color="black"))  |>
#'  plot_map()
#'#Increase the resolution of the contour to improve the appearance of lines
#'montereybay_spatial |>
#'  height_shade() |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay_spatial, 0, 0))  |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  add_overlay(generate_contour_overlay(montereybay_spatial, levels = water_breaks, color="white",
#'                                       height = nrow(montereybay_spatial)*2,
#'                                       width  = ncol(montereybay_spatial)*2))  |>
#'  add_overlay(generate_contour_overlay(montereybay_spatial, levels = land_breaks, color="black",
#'                                       height = nrow(montereybay_spatial)*2,
#'                                       width  = ncol(montereybay_spatial)*2))  |>
#'  plot_map()
#'#Increase the number of breaks and the transparency (via add_overlay)
#'montereybay_spatial |>
#'  height_shade() |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  add_overlay(generate_contour_overlay(montereybay_spatial, linewidth=2, nlevels=100,
#'                                       height = nrow(montereybay_spatial)*2, color="black",
#'                                       width  = ncol(montereybay_spatial)*2), alphalayer=0.5) |>
#'  plot_map()
#'#Manually specify the breaks with levels
#'montereybay_spatial |>
#'  height_shade() |>
#'  add_overlay(generate_contour_overlay(montereybay_spatial, linewidth=2, levels = seq(-2000,0,100))) |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  plot_map()
generate_contour_overlay = function(
  heightmap = NULL,
  levels = NA,
  nlevels = NA,
  zscale = 1,
  width = NA,
  height = NA,
  resolution_multiply = 1,
  color = "black",
  linewidth = 1,
  geographic_aspect = TRUE,
  extent = NULL,
  crs = NULL
) {
  heightmap_missing = missing(heightmap) || is.null(heightmap)
  extent_missing = missing(extent)
  crs_missing = missing(crs)
  if (heightmap_missing) {
    resolved_heightmap = resolve_hillshade_heightmap(
      heightmap_missing = TRUE,
      caller = "generate_contour_overlay"
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
  heightmap_info = coerce_plot_3d_heightmap(
    heightmap,
    extent = extent,
    crs = crs,
    geographic_aspect = geographic_aspect
  )
  if (!heightmap_missing) {
    cache_hillshade_input_context(
      heightmap_info,
      label = format_scene_cache_label(deparse(substitute(heightmap)))
    )
  }
  resolved_zscale = resolve_hillshade_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    caller = "generate_contour_overlay",
    auto_zscale = heightmap_info$zscale,
    allow_hillshade_cache = heightmap_missing,
    allow_scene_cache = heightmap_missing &&
      identical(resolved_heightmap$source, "scene")
  )
  zscale = resolved_zscale$zscale
  heightmap = heightmap_info$heightmap / zscale
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop("`sf` package required for generate_contour_overlay()")
  }
  if (!(length(find.package("isoband", quiet = TRUE)) > 0)) {
    stop("`isoband` package required for generate_contour_overlay()")
  }
  if (is.na(levels[1])) {
    if (is.na(nlevels[1])) {
      nlevels = 10
    }
    rangelevels = range(heightmap, na.rm = TRUE)
    levels = seq(rangelevels[1], rangelevels[2], length.out = nlevels + 2)
  } else if (length(levels) == 2 && !is.na(nlevels)) {
    rangelevels = range(levels, na.rm = TRUE)
    levels = seq(rangelevels[1], rangelevels[2], length.out = nlevels + 2)
  }
  levels = levels[levels > min(heightmap, na.rm = TRUE)]
  levels = levels[levels < max(heightmap, na.rm = TRUE)]
  heightmap = flipud(t(heightmap))
  isolineval = isoband::isolines(
    x = 1:ncol(heightmap),
    y = 1:nrow(heightmap),
    z = heightmap,
    levels = levels
  )
  contours = isoband::iso_to_sfg(isolineval)
  sf_contours = sf::st_sf(
    level = names(contours),
    geometry = sf::st_sfc(contours)
  )
  if (!(length(find.package("ragg", quiet = TRUE)) > 0)) {
    png_device = grDevices::png
  } else {
    png_device = ragg::agg_png
  }
  if (is.na(height)) {
    height = ncol(heightmap)
  }
  if (is.na(width)) {
    width = nrow(heightmap)
  }

  tempoverlay = tempfile(fileext = ".png")
  png_device(
    filename = tempoverlay,
    width = width * resolution_multiply,
    height = height * resolution_multiply,
    units = "px",
    bg = "transparent"
  )
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot(
    sf::st_geometry(sf_contours),
    xlim = c(1, ncol(heightmap)),
    ylim = c(1, nrow(heightmap)),
    xaxs = "i",
    yaxs = "i",
    lwd = linewidth,
    col = color
  )
  grDevices::dev.off() #resets par
  overlay_temp = rayimage::ray_read_image(tempoverlay)
  return(overlay_temp)
}
