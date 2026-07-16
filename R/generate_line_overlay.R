#'@title Generate Line Overlay
#'
#'@description Calculates and returns an overlay of lines for the current height map.
#'
#'@param geometry An `sf` object with LINESTRING geometry.
#'@param extent Default `NULL`. Either an object representing the spatial extent of the scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#'If omitted, rayshader will infer the extent from `heightmap` when possible,
#'otherwise reuse cached extent metadata from the active scene or the most recent
#'raster-backed hillshade call.
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting
#'overlay automatically. If omitted, rayshader will reuse the cached heightmap
#'from the active scene or the most recent hillshade call.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param resolution_multiply Default `1`. If passing in `heightmap` instead of width/height, amount to
#'increase the resolution of the overlay, which should make lines/polygons/text finer.
#'Should be combined with \code{\link[=add_overlay]{add_overlay()}} with `rescale_original = TRUE` to ensure those added details are captured
#'in the final map.
#'@param color Default `black`. Color of the lines.
#'@param linewidth Default `1`. Line width.
#'@param lty Default `1`. Line type. `1` is solid, `2` is dashed, `3` is dotted,`4` is dot-dash,
#'`5` is long dash, and `6` is dash-long-dash.
#'@param offset Default `c(0,0)`. Horizontal and vertical offset to apply to the line, in units of `geometry`.
#'@param data_column_width Default `NULL`. The numeric column to map the width to. The maximum width will be the value
#'specified in `linewidth`.
#'@return Semi-transparent overlay with contours.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Add the included `sf` object with roads to the montereybay dataset
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'montereybay |>
#'  height_shade() |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  |>
#'  add_overlay(generate_line_overlay(monterey_roads_sf))  |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  plot_map()
#'#Change the line width, color, and transparency
#'montereybay |>
#'  height_shade() |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  |>
#'  add_overlay(generate_line_overlay(monterey_roads_sf, linewidth=3, color="white"),
#'                                    alphalayer=0.8)  |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  plot_map()
#'#Manually specify the width and height to improve visual quality of the lines
#'montereybay |>
#'  height_shade() |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  add_overlay(generate_line_overlay(monterey_roads_sf, linewidth=3, color="white",
#'                                    width = 1080, height = 1080),
#'                                    alphalayer=0.8)  |>
#'  plot_map()
generate_line_overlay = function(
  geometry,
  extent = NULL,
  heightmap = NULL,
  width = NA,
  height = NA,
  resolution_multiply = 1,
  color = "black",
  linewidth = 1,
  lty = 1,
  data_column_width = NULL,
  offset = c(0, 0)
) {
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop("{sf} package required for generate_line_overlay()")
  }
  if (missing(geometry)) {
    stop("`geometry` must be supplied.", call. = FALSE)
  }
  if (
    !(inherits(geometry, "sf") ||
      inherits(geometry, "sfc") ||
      inherits(geometry, "sfg"))
  ) {
    stop("geometry must be {sf} object")
  }
  if (inherits(geometry, "sfg")) {
    geometry = sf::st_sfc(geometry)
  }
  heightmap = resolve_overlay_heightmap(
    heightmap = heightmap,
    heightmap_missing = missing(heightmap),
    width = width,
    height = height,
    caller = "generate_line_overlay"
  )
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "generate_line_overlay"
  )
  scene_geometry = auto_transform_scene_sf(
    sf_object = geometry,
    extent = extent,
    heightmap = heightmap,
    crs = tryCatch(sf::st_crs(geometry), error = function(e) NULL),
    caller = "generate_line_overlay"
  )
  geometry = scene_geometry$object
  if (!is.null(scene_geometry$extent)) {
    extent = scene_geometry$extent
  }
  # sf_lines_cropped = base::suppressMessages(base::suppressWarnings(sf::st_crop(geometry, extent)))

  sf_lines_cropped = geometry
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
  og_height = height
  og_width = width

  if (!is.null(data_column_width)) {
    if (data_column_width %in% colnames(sf_lines_cropped)) {
      widthvals = sf_lines_cropped[[data_column_width]] /
        max(sf_lines_cropped[[data_column_width]], na.rm = TRUE) *
        linewidth
    } else {
      warning(
        "Was not able to find data_column_width `",
        data_column_width,
        "` in {sf} object."
      )
      widthvals = linewidth
    }
  } else {
    widthvals = linewidth
  }
  if (any(offset != 0)) {
    if (length(offset) == 2) {
      line_geometry = sf::st_geometry(sf_lines_cropped)
      for (i in seq_len(length(line_geometry))) {
        line_geometry[[i]] = line_geometry[[i]] + offset
      }
      if (inherits(sf_lines_cropped, "sf")) {
        sf::st_geometry(sf_lines_cropped) = line_geometry
      } else {
        sf_lines_cropped = line_geometry
      }
    } else {
      stop("`offset` must be of length-2")
    }
  }
  recycle_line_arg = function(x, i) {
    if (length(x) == 1) {
      return(x)
    }
    x[((i - 1) %% length(x)) + 1]
  }
  draw_line_geometry = NULL
  draw_line_geometry = function(geom, feature_index) {
    geom_type = intersect(
      class(geom),
      c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION")
    )[1]
    if (is.na(geom_type)) {
      return(invisible(NULL))
    }
    if (geom_type == "LINESTRING") {
      mat = unclass(geom)
      if (nrow(mat) > 1) {
        graphics::lines(
          mat[, 1],
          mat[, 2],
          lty = recycle_line_arg(lty, feature_index),
          lwd = recycle_line_arg(widthvals, feature_index),
          col = recycle_line_arg(color, feature_index)
        )
      }
      return(invisible(NULL))
    }
    if (geom_type == "MULTILINESTRING") {
      for (line in unclass(geom)) {
        draw_line_geometry(sf::st_linestring(line), feature_index)
      }
      return(invisible(NULL))
    }
    for (child in unclass(geom)) {
      draw_line_geometry(child, feature_index)
    }
    invisible(NULL)
  }
  extent = get_extent(extent)
  tempoverlay = tempfile(fileext = ".png")
  png_device(
    filename = tempoverlay,
    width = width * resolution_multiply,
    height = height * resolution_multiply,
    units = "px",
    bg = "transparent"
  )
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(extent["xmin"], extent["xmax"]),
    ylim = c(extent["ymin"], extent["ymax"]),
    xaxs = "i",
    yaxs = "i"
  )
  line_geometry = sf::st_geometry(sf_lines_cropped)
  for (i in seq_along(line_geometry)) {
    draw_line_geometry(line_geometry[[i]], i)
  }
  grDevices::dev.off() #resets par
  overlay_temp = rayimage::ray_read_image(tempoverlay)
  return(overlay_temp)
}
