#'@title Add Water
#'
#'@description Colors water cells in a shaded map using a matrix or spatial raster mask.
#'
#'@param hillshade A three-dimensional RGB array.
#'@param water_map A numeric or logical matrix in heightmap orientation, a
#'single-layer `terra::SpatRaster` or `raster::RasterLayer`, or a raster file path.
#'Values greater than or equal to 1 (including `TRUE`) indicate water. Missing
#'values and values below 1 leave the map unchanged. Spatial masks must match
#'the cached terrain's grid, extent, and coordinate reference system when that
#'context is available; masks are not reprojected or resampled.
#'@param color Default `imhof1`. The water fill color. A hexcode or recognized color string.
#'Also includes built-in colors to match the palettes included in [sphere_shade()]:
#'(`imhof1`,`imhof2`,`imhof3`,`imhof4`, `desert`, `bw`, and `unicorn`).
#'@importFrom grDevices col2rgb rainbow
#'@return A three-dimensional RGB array with water cells colored. The result is
#'cached as the current shaded map without changing the cached terrain.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Here we even out a portion of the volcano dataset to simulate water:
#'island_volcano = volcano
#'island_volcano[island_volcano < mean(island_volcano)] = mean(island_volcano)
#'
#'#Setting a minimum area avoids classifying small flat areas as water:
#'island_volcano |>
#'  sphere_shade(texture="imhof3") |>
#'  add_water(detect_water(min_area = 400),color="imhof3") |>
#'  plot_map()
#'
#'#Color ocean cells directly from the spatial elevation raster:
#'montereybay_spatial |>
#'  sphere_shade(texture = "imhof4", vertical_exaggeration = 20) |>
#'  add_water(montereybay_spatial < 0, color = "imhof4") |>
#'  plot_map()
add_water = function(hillshade, water_map, color = "imhof1") {
  force(hillshade)
  if (missing(water_map)) {
    stop(
      "`water_map` must be supplied as a matrix or spatial raster mask.",
      call. = FALSE
    )
  }
  if (is_spatial_heightmap_input(water_map)) {
    water_map = coerce_add_water_mask(water_map)
  }
  if (
    !is.matrix(water_map) || !(is.numeric(water_map) || is.logical(water_map))
  ) {
    stop(
      "`water_map` must be a numeric or logical matrix or spatial raster mask.",
      call. = FALSE
    )
  }
  water_map[is.na(water_map)] = 0
  hillshade_cache_label = get_hillshade_map_label(
    default = format_scene_cache_label(deparse(substitute(hillshade)))
  )
  hillshade = rayimage::ray_read_image(hillshade)
  colorspace = attr(hillshade, "colorspace")
  white_point = attr(hillshade, "white_current")
  # Heightmaps index east-west first; images index north-south first.
  water_map = t(water_map)
  if (color == "imhof1") {
    color = col2rgb_linear("#e9f9ee")
  } else if (color == "imhof2") {
    color = col2rgb_linear("#337c73")
  } else if (color == "imhof3") {
    color = col2rgb_linear("#4e7982")
  } else if (color == "imhof4") {
    color = col2rgb_linear("#638d99")
  } else if (color == "desert") {
    color = col2rgb_linear("#b2f4ff")
  } else if (color == "bw") {
    color = col2rgb_linear("#ffffff")
  } else if (color != "unicorn") {
    color = col2rgb_linear(color)
  }
  if (length(dim(hillshade)) != 3) {
    if (length(dim(hillshade)) == 2) {
      if (any(hillshade > 1 | hillshade < 0)) {
        stop(
          "Error: Not a shadow matrix. Intensities must be between 0 and 1. Pass your elevation matrix to ray_shade/lamb_shade/ambient_shade/sphere_shade first."
        )
      }
      temp = array(0, dim = c(ncol(hillshade), nrow(hillshade), 3))
      temp[,, 1] = t(flipud(hillshade))
      temp[,, 2] = t(flipud(hillshade))
      temp[,, 3] = t(flipud(hillshade))
      hillshade = temp
    }
  }
  if (any(dim(water_map) != dim(hillshade)[1:2])) {
    stop(
      "`hillshade` and `water_map` dimensions must be the same; hillshade is ",
      paste0(dim(hillshade)[1:2], collapse = "x"),
      ", water_map is ",
      paste0(dim(water_map)[1:2], collapse = "x")
    )
  }
  for (i in 1:3) {
    tempmat = hillshade[,, i]
    if (color[1] != "unicorn") {
      tempmat[water_map >= 1] = color[i]
    } else {
      unicolors = t(col2rgb_linear(rainbow(256)))
      for (row in seq_len(nrow(water_map))) {
        for (col in seq_len(ncol(water_map))) {
          if (water_map[row, col] >= 1) {
            tempmat[row, col] = unicolors[i, (col - 1) %% 256 + 1]
          }
        }
      }
    }
    hillshade[,, i] = tempmat
  }
  hillshade = rayimage::ray_read_image(
    hillshade,
    assume_colorspace = colorspace,
    assume_white = white_point,
    source_linear = TRUE
  )
  cache_hillshade_map(hillshade, label = hillshade_cache_label)
  return(hillshade)
}

#' Convert a spatial water mask without replacing the terrain cache
#'
#' @param water_map A spatial raster mask or raster file path.
#' @return A water mask matrix in heightmap orientation.
#' @keywords internal
coerce_add_water_mask = function(water_map) {
  if (is.character(water_map)) {
    water_map = read_spatial_raster(water_map, caller = "add_water")
  }
  layer_count = if (inherits(water_map, "SpatRaster")) {
    terra::nlyr(water_map)
  } else {
    raster::nlayers(water_map)
  }
  if (layer_count != 1L) {
    stop("`water_map` must have exactly one raster layer.", call. = FALSE)
  }

  terrain = get_hillshade_heightmap()
  extent = get_hillshade_extent()
  crs = get_hillshade_crs()
  if (
    !is.null(terrain) &&
      any(c(ncol(water_map), nrow(water_map)) != dim(terrain))
  ) {
    stop(
      "`water_map` grid dimensions must match the cached terrain.",
      call. = FALSE
    )
  }
  if (!is.null(extent)) {
    extent = get_extent(extent)
    # Compare in cell units so large projected coordinates do not hide shifts.
    cell_size = c(
      (extent["xmax"] - extent["xmin"]) / ncol(water_map),
      (extent["ymax"] - extent["ymin"]) / nrow(water_map)
    )
    tolerance = rep(cell_size * 1e-7, each = 2)
    if (any(abs(get_extent(water_map) - extent) > tolerance)) {
      stop(
        "`water_map` extent and cell alignment must match the cached terrain.",
        call. = FALSE
      )
    }
  }
  if (!is.null(crs)) {
    mask_crs = extract_spatial_heightmap_crs(water_map)
    if (is.null(mask_crs) || !isTRUE(mask_crs == sf::st_crs(crs))) {
      stop("`water_map` CRS must match the cached terrain.", call. = FALSE)
    }
  }
  # This conversion deliberately does not cache the binary mask as elevation.
  raster_to_matrix(water_map, verbose = FALSE)
}
