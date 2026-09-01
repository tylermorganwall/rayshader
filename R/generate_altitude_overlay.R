#'@title Generate Altitude Overlay
#'
#'@description Using a hillshade and the height map, generates a semi-transparent hillshade to
#'layer onto an existing map.
#'
#'@param hillshade The hillshade to transition into.
#'@param heightmap Default `NULL`. A two-dimensional matrix, where each entry
#'in the matrix is the elevation at that point. If omitted, rayshader uses the
#'cached hillshade or scene heightmap.
#'@param start_transition Elevation threshold, or proportion of the height range if `relative = TRUE`.
#'@param end_transition Default `NULL`. Elevation threshold, or proportion of the height range if `relative = TRUE`. By default, this is equal to `start_transition`.
#'@param lower Default `TRUE`. If `TRUE`, the overlay is most opaque at lower elevations. If `FALSE`, the direction is reversed.
#'@param relative Default `FALSE`. If `TRUE`, interpret `start_transition` and `end_transition` as proportions of the height range in 0..1.
#'@param extent Default `NULL`. Spatial extent for a matrix heightmap.
#'@param crs Default `NULL`. CRS describing the input heightmap. An explicit
#'value overrides embedded metadata on a copy of the input.
#'@return 4-layer RGB array representing the semi-transparent hillshade.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Create a bathymetric hillshade
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay_spatial, texture = water_palette)
#'plot_map(bathy_hs)
#'
#'#Set everything below 0m to water palette
#'montereybay_spatial |>
#'  sphere_shade() |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay_spatial, 0, 0))  |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  plot_map()
#'
#'#Add snow peaks by setting `lower = FALSE`
#'snow_palette = "white"
#'snow_hs = height_shade(montereybay_spatial, texture = snow_palette)
#'
#'#Set the snow transition region from 500m to 1200m
#'montereybay_spatial |>
#'  sphere_shade(texture = "desert") |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay_spatial, 0, 0))  |>
#'  add_overlay(generate_altitude_overlay(snow_hs, montereybay_spatial, 500, 1200, lower=FALSE))  |>
#'  add_shadow(ambient_shade(vertical_exaggeration = 4,maxsearch=100),0) |>
#'  plot_map()
generate_altitude_overlay = function(
  hillshade,
  heightmap = NULL,
  start_transition,
  end_transition = NULL,
  lower = TRUE,
  relative = FALSE,
  extent = NULL,
  crs = NULL
) {
  heightmap_missing = missing(heightmap) || is.null(heightmap)
  if (heightmap_missing) {
    heightmap = resolve_hillshade_heightmap(
      heightmap_missing = TRUE,
      caller = "generate_altitude_overlay"
    )$heightmap
  }
  if (is.null(end_transition)) {
    end_transition = start_transition
  }
  heightmap_info = coerce_plot_3d_heightmap(
    heightmap,
    extent = extent,
    crs = crs
  )
  if (!heightmap_missing) {
    cache_hillshade_input_context(
      heightmap_info,
      label = format_scene_cache_label(deparse(substitute(heightmap)))
    )
  }
  heightmap = heightmap_info$heightmap

  if (relative) {
    if (
      !is.numeric(start_transition) ||
        length(start_transition) != 1 ||
        start_transition < 0 ||
        start_transition > 1
    ) {
      stop("`start_transition` must be in [0, 1] when `relative = TRUE`.")
    }
    if (
      !is.numeric(end_transition) ||
        length(end_transition) != 1 ||
        end_transition < 0 ||
        end_transition > 1
    ) {
      stop("`end_transition` must be in [0, 1] when `relative = TRUE`.")
    }

    height_rng = range(heightmap, na.rm = TRUE)
    height_span = diff(height_rng)

    start_transition = height_rng[1] + start_transition * height_span
    end_transition = height_rng[1] + end_transition * height_span
  }

  if (length(dim(hillshade)) == 2) {
    hillarray = array(0, dim = c(nrow(hillshade), ncol(hillshade), 4))
    hillarray[,, 1] = hillshade
    hillarray[,, 2] = hillshade
    hillarray[,, 3] = hillshade
    hillshade = hillarray
  }

  if (dim(hillshade)[3] == 3) {
    temp_hillshade = array(1, dim = c(dim(hillshade)[1:2], 4))
    temp_hillshade[,, 4] = 1
    temp_hillshade[,, 1:3] = hillshade
    hillshade = temp_hillshade
  }

  heightmap = t(heightmap)

  if (any(dim(heightmap) != dim(hillshade)[1:2])) {
    heightmap = rayimage::render_resized(heightmap, dims = dim(hillshade)[1:2])
  }

  trans_map = heightmap

  if (start_transition == end_transition) {
    if (!lower) {
      trans_map_temp = trans_map - start_transition
      trans_map[trans_map_temp < 0] = 0
      trans_map[trans_map_temp >= 0] = 1
    } else {
      trans_map_temp = trans_map - start_transition
      trans_map[trans_map_temp < 0] = 1
      trans_map[trans_map_temp >= 0] = 0
    }
    hillshade[,, 4] = trans_map
    return(hillshade)
  }

  if (lower) {
    transition_region = end_transition - start_transition
    trans_map_temp = (trans_map - start_transition) / transition_region
    trans_map = 1 - trans_map_temp
    trans_map[trans_map_temp < 0] = 1
    trans_map[trans_map_temp >= 1] = 0
  } else {
    transition_region = end_transition - start_transition
    trans_map_temp = (trans_map - start_transition) / transition_region
    trans_map = trans_map_temp
    trans_map[trans_map_temp < 0] = 0
    trans_map[trans_map_temp >= 1] = 1
  }

  hillshade[,, 4] = trans_map

  return(rayimage::ray_read_image(
    hillshade,
    assume_colorspace = rayimage::CS_SRGB,
    assume_white = "D65"
  ))
}
