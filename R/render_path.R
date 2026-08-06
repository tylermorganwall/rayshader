#'@title Render Path
#'
#'@description Adds a 3D path to the current scene, using x/y coordinates in the reference
#'system defined by the extent object. If no altitude is provided, the path will be elevated a constant offset
#'above the heightmap. If the path goes off the edge, the nearest height on the heightmap will be used.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param x Default `NULL`. Vector of x coordinates (or other coordinate in the same coordinate reference system as extent).
#'Ignored if `y` is an `sf` or `SpatialLineDataFrame` object.
#'@param y Vector of y coordinates (or other coordinate in the same coordinate reference system as extent).
#'Can also be an `sf` or `SpatialLineDataFrame` object.
#'@param lat Default `NULL`. Alias for `y` for geographic workflows.
#'@param long Default `NULL`. Alias for `x` for geographic workflows.
#'@param altitude Default `NULL`. Elevation of each point, in units of the elevation matrix (scaled by zscale).
#'If left `NULL`, this will be just the elevation value at ths surface, offset by `offset`. If a single value,
#'all data will be rendered at that altitude.
#'@param groups Default `NULL`. Integer vector specifying the grouping of each x/y path segment, if x/y are
#'specified as numeric vectors (as opposed to `sf` or `SpatialLineDataFrame` objects, where this information
#'is built-in to the object).
#'@param extent Either an object representing the spatial extent of the 3D scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use extent metadata cached by [plot_3d()] or [plot_gg()].
#'@param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#'to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#'for non-ggplot scenes.
#'@param crs Default `NULL`. CRS of the input numeric x/y coordinates, or CRS to assign to CRS-less spatial data before transforming it into the active scene CRS. If spatial data already carries a CRS, that CRS is used automatically.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#' All points are assumed to be evenly spaced.
#'@param resample_evenly Default `FALSE`. If `TRUE`, this will re-sample the path evenly from beginning to end, which can help vastly
#'reduce the number of points used to draw it (which can improve the performance of [render_highquality()] and \code{\link[=render_snapshot]{render_snapshot()}} when using `software_render = TRUE`).
#'This function works only if `reorder = TRUE`, or if the sf object is already ordered from beginning to end.
#'@param resample_n Default `360`. Number of breaks in which to evenly resample the line if `resample_evenly = TRUE`.
#'@param linewidth Default `3`. The line width.
#'@param antialias Default `FALSE`. If `TRUE`, the line with be have anti-aliasing applied. NOTE: anti-aliasing can cause some unpredictable behavior with transparent surfaces.
#'@param color Default `black`. Color of the line. Use `"height"` to color the path by the cached [plot_gg()] height aesthetic palette.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param reorder Default `FALSE`. If `TRUE`, this will attempt to re-order the rows within an `sf` object with
#'multiple paths to be one continuous, end-to-end path. This happens in two steps: merging duplicate
#'paths that have end points that match with another object (within `reorder_duplicate_tolerance` distance), and then
#'merges them (within `reorder_merge_tolerance` distance) to form a continuous path.
#'@param reorder_first_index Default `1`. The index (row) of the `sf` object in which to begin the reordering
#'process. This merges and reorders paths within `reorder_merge_tolerance` distance until it cannot
#'merge any more, and then repeats the process in the opposite direction.
#'@param reorder_duplicate_tolerance Default `0.1`. Lines that have start and end points (does not matter which)
#'within this tolerance that match a line already processed (order determined by `reorder_first_index`) will be
#'discarded.
#'@param reorder_merge_tolerance Default `1`. Lines that have start points that are within this distance
#'to a previously processed line's end point (order determined by `reorder_first_index`) will be reordered
#'within the `sf` object to form a continuous, end-to-end path.
#'@param simplify_tolerance Default `0` (no simplification). If greater than zero, simplifies
#'the path to the tolerance specified. This happens after the data has been merged if `reorder = TRUE`.
#'If the input data is specified with long-lat coordinates and `sf_use_s2()` returns `TRUE`,
#'then the value of simplify_tolerance must be specified in meters.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing paths.
#'@param return_coords Default `FALSE`. If `TRUE`, this will return the internal rayshader coordinates of the path, instead of
#'plotting the line.
#'@param tag Default `"path3d"`. The rgl tag to use when adding the path to the scene.
#'@param filter_to_extent Default `TRUE`. If `TRUE`, path data outside the scene extent is omitted. Spatial line inputs are cropped to the extent. For scenes created with [plot_gg()], filtering uses the ggplot panel extent rather than the full rendered 3D ggplot extent.
#'@param ... Optional z-axis arguments passed to [render_zaxis()], such as
#'`zaxis = TRUE`, `zaxis_location`, `zaxis_breaks`, and `zaxis_labels`.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Starting at Moss Landing in Monterey Bay, we are going to simulate a flight of a bird going
#'#out to sea and diving for food.
#'
#'#First, create simulated lat/long data
#'set.seed(2009)
#'moss_landing_coord = c(36.806807, -121.793332)
#'x_vel_out = -0.001 + rnorm(1000)[1:300]/1000
#'y_vel_out = rnorm(1000)[1:300]/200
#'z_out = c(seq(0,2000,length.out = 180), seq(2000,0,length.out=10),
#'          seq(0,2000,length.out = 100), seq(2000,0,length.out=10))
#'
#'bird_track_lat = list()
#'bird_track_long = list()
#'bird_track_lat[[1]] = moss_landing_coord[1]
#'bird_track_long[[1]] = moss_landing_coord[2]
#'for(i in 2:300) {
#' bird_track_lat[[i]] = bird_track_lat[[i-1]] + y_vel_out[i]
#' bird_track_long[[i]] = bird_track_long[[i-1]] + x_vel_out[i]
#'}
#'
#'
#'#Render the 3D map
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4,water=TRUE,
#'          shadowcolor="#40310a", watercolor="#233aa1", background = "tan",
#'          theta=210,  phi=22, zoom=0.20, fov=55)
#'
#'#Pass in the latitude/longitude coordinates and altitudes of the track.
#'render_path(lat = unlist(bird_track_lat), long = unlist(bird_track_long),
#'            altitude = z_out, color="white", antialias=TRUE)
#'render_snapshot()
#'#We'll set the altitude to right above the water to give the tracks a "shadow".
#'render_path(lat = unlist(bird_track_lat), long = unlist(bird_track_long),
#'            altitude = 10, color="black", antialias=TRUE)
#'render_camera(theta=30,phi=35,zoom=0.45,fov=70)
#'render_snapshot()
#'
#'#Remove the path:
#'render_path(clear_previous=TRUE)
#'
#'#Finally, we can also plot just GPS coordinates offset from the surface by leaving altitude `NULL`
#'# Here we plot a spiral of values surrounding Moss Landing.
#'
#'t = seq(0,2*pi,length.out=1000)
#'circle_coords_lat = moss_landing_coord[1] + 0.5 * t/8 * sin(t*6)
#'circle_coords_long = moss_landing_coord[2] + 0.5 * t/8 *  cos(t*6)
#'render_path(lat = unlist(circle_coords_lat), long = unlist(circle_coords_long),
#'            color="red", antialias=TRUE,offset=100, linewidth=5)
#'render_camera(theta = 160, phi=33, zoom=0.4, fov=55)
#'render_snapshot()
#'
#'#And all of these work with `render_highquality()`. Here, I set `use_extruded_paths = TRUE`
#'#to get thick continuous paths.
#'render_highquality(line_radius=1, min_variance = 0,
#'                   use_extruded_paths = TRUE, samples = 16)
#'#We can also change the material of the objects by setting the `rgl_materials`
#'#argument in `render_highquality()`
#'render_highquality(line_radius=1, min_variance = 0, samples = 16,
#'                   use_extruded_paths = TRUE,
#'                   rgl_materials = list(path3d = list(
#'                     material = rayrender::glossy,
#'                     args = list(gloss = 0.5, reflectance = 0.2)
#'                   )))
#'#Render the path with a neon light material
#'render_highquality(light = FALSE, samples = 16,
#'                   line_radius = 0.1,
#'                   ground_size = 0,
#'                   use_extruded_paths = TRUE,
#'                   rgl_materials = list(path3d = list(
#'                     material = rayrender::light,
#'                     args = list(importance_sample = FALSE,
#'                                 color = "purple", intensity = 2)
#'                   )))
#'
#'#For transmissive materials (like `dielectric`), we should specify that the path
#'#should be rendered with an extruded path. We'll use the `attenuation` argument in
#'#the `dielectric` function to specify a realistic glass color.
#'render_path(clear_previous = TRUE,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long),
#'            color="white", offset=200, linewidth=5)
#'render_highquality(line_radius=1, min_variance = 0, samples = 16,
#'                   lightsize = 2000, lightintensity = 10,
#'                   use_extruded_paths = TRUE,
#'                   rgl_materials = list(path3d = list(
#'                     material = rayrender::dielectric,
#'                     args = list(refraction = 1.5, attenuation = c(0.05,0.2,0.2))
#'                   )))
render_path = function(
  y = NULL,
  x = NULL,
  altitude = NULL,
  groups = NULL,
  extent = NULL,
  panel = NULL,
  zscale = 1,
  vertical_exaggeration = 1,
  heightmap = NULL,
  resample_evenly = FALSE,
  resample_n = 360,
  reorder = FALSE,
  reorder_first_index = 1,
  reorder_duplicate_tolerance = 0.1,
  reorder_merge_tolerance = 1,
  simplify_tolerance = 0,
  linewidth = 0.5,
  color = "black",
  antialias = FALSE,
  offset = 5,
  clear_previous = FALSE,
  return_coords = FALSE,
  tag = "path3d",
  lat = NULL,
  long = NULL,
  crs = NULL,
  filter_to_extent = TRUE,
  ...
) {
  validate_filter_to_extent(filter_to_extent, caller = "render_path")
  xy_inputs = resolve_render_xy_aliases(
    x = x,
    y = y,
    long = long,
    lat = lat,
    missing_x = missing(x),
    missing_y = missing(y),
    missing_long = missing(long),
    missing_lat = missing(lat),
    caller = "render_path"
  )
  x = xy_inputs$x
  y = xy_inputs$y
  input_crs = if (is.null(crs)) xy_inputs$source_crs else crs
  lat = y
  long = x
  zaxis_split = split_zaxis_dots(list(...))
  zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_path"
  )
  heightmap = resolve_scene_render_heightmap(
    heightmap,
    caller = "render_path"
  )
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "render_path",
    panel = panel,
    error_if_missing = FALSE
  )
  zaxis_args = normalize_scene_zaxis_args(
    zaxis_args = zaxis_split$zaxis_args,
    altitude = altitude,
    extent = extent,
    heightmap = heightmap
  )
  if (rgl::cur3d() == 0 && !return_coords) {
    stop("No rgl window currently open.")
  }
  if (clear_previous) {
    rgl::pop3d(tag = tag)
    if (is.null(lat)) {
      render_zaxis_from_dots(
        zaxis_args = zaxis_args,
        extent = extent,
        panel = panel,
        zscale = zscale,
        heightmap = heightmap,
        caller = "render_path"
      )
      return(invisible())
    }
  }
  if (resample_evenly) {
    if (
      !is.numeric(resample_n) ||
        length(resample_n) != 1 ||
        !is.finite(resample_n) ||
        resample_n <= 1
    ) {
      stop(
        "`resample_n` must be a single finite number greater than 1 when `resample_evenly = TRUE`.",
        call. = FALSE
      )
    }
    xyz = render_path(
      extent = extent,
      panel = panel,
      lat = lat,
      long = long,
      altitude = altitude,
      zscale = zscale,
      vertical_exaggeration = 1,
      heightmap = heightmap,
      offset = offset,
      resample_evenly = FALSE,
      reorder = reorder,
      reorder_first_index = reorder_first_index,
      reorder_duplicate_tolerance = reorder_duplicate_tolerance,
      reorder_merge_tolerance = reorder_merge_tolerance,
      simplify_tolerance = simplify_tolerance,
      clear_previous = FALSE,
      return_coords = TRUE,
      crs = crs,
      filter_to_extent = filter_to_extent
    )
    if (!length(xyz)) {
      if (return_coords) {
        return(list())
      }
      render_zaxis_from_dots(
        zaxis_args = zaxis_args,
        extent = extent,
        panel = panel,
        zscale = zscale,
        heightmap = heightmap,
        caller = "render_path"
      )
      return(invisible(NULL))
    }
    xyz = lapply(xyz, get_interpolated_points_path, n = resample_n)
    xyz = do.call(
      "rbind",
      lapply(xyz, \(x) rbind(x, matrix(NA, ncol = 3, nrow = 1)))
    )
    if (!return_coords) {
      color = resolve_ggplot_height_palette_color(
        color = color,
        values = xyz[, 2] * zscale,
        heightmap = heightmap,
        caller = "render_path"
      )
      if (length(linewidth) > 1) {
        if (length(linewidth) == nrow(xyz)) {
          linewidth = (linewidth[seq_len(length(linewidth))[-1]] +
            linewidth[seq_len(length(linewidth) - 1)]) /
            2
        }
        color_length = length(color)
        for (i in seq_len(nrow(xyz) - 1)) {
          rgl::lines3d(
            xyz[i:(i + 1), ],
            color = color[((i - 1) %% color_length) + 1],
            tag = tag,
            lwd = linewidth[i],
            line_antialias = antialias
          )
        }
        render_zaxis_from_dots(
          zaxis_args = zaxis_args,
          extent = extent,
          panel = panel,
          zscale = zscale,
          heightmap = heightmap,
          caller = "render_path"
        )
        return(invisible())
      } else {
        rgl::lines3d(
          xyz,
          color = color,
          tag = tag,
          lwd = linewidth,
          line_antialias = antialias
        )
        render_zaxis_from_dots(
          zaxis_args = zaxis_args,
          extent = extent,
          panel = panel,
          zscale = zscale,
          heightmap = heightmap,
          caller = "render_path"
        )
        return(invisible())
      }
    } else {
      return(xyz)
    }
  }

  #Remove empty geometries
  if (inherits(lat, "sf")) {
    lat = lat[!sf::st_is_empty(lat), ]
  }
  if (reorder && inherits(lat, "sf")) {
    lat = ray_merge_reorder(
      lat,
      start_index = reorder_first_index,
      merge_tolerance = reorder_merge_tolerance,
      duplicate_tolerance = reorder_duplicate_tolerance
    )
  }

  if (
    simplify_tolerance > 0 &&
      (inherits(lat, "sf") || inherits(lat, "sfc_LINESTRING"))
  ) {
    lat = sf::st_sf(sf::st_simplify(
      lat,
      dTolerance = simplify_tolerance,
      preserveTopology = TRUE
    ))
    lat = lat[!sf::st_is_empty(lat), ]
    lat = suppressWarnings(sf::st_cast(
      sf::st_cast(lat, "MULTILINESTRING"),
      "LINESTRING"
    ))
  }
  geometry_transformed = FALSE
  if (inherits(lat, "SpatialLinesDataFrame") || inherits(lat, "SpatialLines")) {
    lat = sf::st_as_sf(lat)
  }
  if (inherits(lat, "sfg")) {
    lat = sf::st_sfc(lat)
  }
  lat = coerce_render_path_line_geometry(lat)
  if (
    inherits(lat, "sf") ||
      inherits(lat, "sfc")
  ) {
    scene_path = auto_transform_scene_sf(
      sf_object = lat,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      crs = crs,
      caller = "render_path"
    )
    lat = scene_path$object
    if (!is.null(scene_path$extent)) {
      extent = scene_path$extent
    }
    filtered_path = filter_scene_sf_to_extent(
      sf_object = lat,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      filter_to_extent = filter_to_extent,
      caller = "render_path"
    )
    lat = filtered_path$object
    lat = coerce_render_path_line_geometry(lat)
    if (is_empty_scene_sf(lat)) {
      if (return_coords) {
        return(list())
      }
      render_zaxis_from_dots(
        zaxis_args = zaxis_args,
        extent = extent,
        panel = panel,
        zscale = zscale,
        heightmap = heightmap,
        caller = "render_path"
      )
      return(invisible(NULL))
    }
    geometry_transformed = TRUE
  }

  if (inherits(lat, "sf")) {
    latlong = sf::st_coordinates(lat)
    if (ncol(latlong) == 3) {
      long = latlong[, 1]
      lat = latlong[, 2]
      groups = latlong[, 3]
    } else if (ncol(latlong) == 4) {
      long = latlong[, 1]
      lat = latlong[, 2]
      groups = interaction(latlong[, 3], latlong[, 4])
    }
  } else if (inherits(lat, "sfc")) {
    latlong = sf::st_coordinates(lat)
    if (ncol(latlong) == 3) {
      long = latlong[, 1]
      lat = latlong[, 2]
      groups = latlong[, 3]
    } else if (ncol(latlong) == 4) {
      long = latlong[, 1]
      lat = latlong[, 2]
      groups = interaction(latlong[, 3], latlong[, 4])
    }
  } else if (is.null(groups)) {
    groups = rep(1, length(lat))
  }
  if (!geometry_transformed && !is.null(lat) && !is.null(long)) {
    scene_xy = auto_transform_scene_xy(
      x = long,
      y = lat,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      crs = input_crs,
      caller = "render_path"
    )
    long = scene_xy$x
    lat = scene_xy$y
    if (!is.null(scene_xy$extent)) {
      extent = scene_xy$extent
    }
  }
  if (!geometry_transformed && !is.null(lat) && !is.null(long)) {
    n_path_coords_before_filter = length(lat)
    filtered_xy = filter_scene_xy_to_extent(
      x = long,
      y = lat,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      filter_to_extent = filter_to_extent,
      caller = "render_path"
    )
    long = filtered_xy$x
    lat = filtered_xy$y
    if (length(filtered_xy$keep) == n_path_coords_before_filter) {
      groups = subset_render_arg(
        groups,
        filtered_xy$keep,
        n_path_coords_before_filter
      )
      altitude = subset_render_arg(
        altitude,
        filtered_xy$keep,
        n_path_coords_before_filter
      )
    }
    if (!length(lat) || !length(long)) {
      if (return_coords) {
        return(list())
      }
      render_zaxis_from_dots(
        zaxis_args = zaxis_args,
        extent = extent,
        panel = panel,
        zscale = zscale,
        heightmap = heightmap,
        caller = "render_path"
      )
      return(invisible(NULL))
    }
  }
  path_altitude_values = altitude
  path_coord_count = length(lat)
  split_lat = split(lat, groups)
  split_long = split(long, groups)

  if (length(altitude) == length(lat)) {
    split_altitude = split(altitude, groups)
    single_altitude = FALSE
  } else {
    single_altitude = TRUE
  }

  if (!is.null(altitude)) {
    offset = 0
  }

  coord_list = list()
  for (group in seq_along(split_lat)) {
    lat = split_lat[[group]]
    long = split_long[[group]]
    if (length(lat) < 2 || length(long) < 2) {
      next
    }
    if (!single_altitude) {
      altitude = split_altitude[[group]]
    }
    coord_list[[group]] = transform_into_heightmap_coords(
      extent,
      heightmap,
      lat,
      long,
      altitude,
      offset,
      zscale,
      filter_bounds = FALSE,
      crs = crs,
      panel = panel,
      transform_scene = FALSE,
      caller = "render_path"
    )
  }
  coord_list = coord_list[lengths(coord_list) > 0]
  if (!length(coord_list)) {
    if (return_coords) {
      return(list())
    }
    render_zaxis_from_dots(
      zaxis_args = zaxis_args,
      extent = extent,
      panel = panel,
      zscale = zscale,
      heightmap = heightmap,
      caller = "render_path"
    )
    return(invisible(NULL))
  }
  path_scene_altitude = unlist(lapply(coord_list, function(coord) {
    coord[, 2] * zscale
  }))
  if (
    !is.null(path_altitude_values) &&
      length(path_altitude_values) != 1 &&
      length(path_altitude_values) != path_coord_count
  ) {
    path_altitude_values = path_scene_altitude
  }
  path_color_values = if (!is.null(path_altitude_values)) {
    path_altitude_values
  } else {
    path_scene_altitude
  }
  if (
    length(path_color_values) != 1 &&
      length(path_color_values) != length(path_scene_altitude)
  ) {
    path_color_values = path_scene_altitude
  }
  color = resolve_ggplot_height_palette_color(
    color = color,
    values = path_color_values,
    heightmap = heightmap,
    caller = "render_path"
  )
  cache_altitude_zaxis_data(
    source = "path",
    altitude = path_altitude_values,
    scene_altitude = path_scene_altitude,
    label = "path"
  )
  if (!return_coords) {
    if (length(linewidth) > 1) {
      if (length(coord_list) == 1) {
        xyz = do.call("rbind", coord_list)
        if (length(linewidth) == nrow(xyz)) {
          linewidth = (linewidth[seq_len(length(linewidth))[-1]] +
            linewidth[seq_len(length(linewidth) - 1)]) /
            2
        }
        if (length(linewidth) != (nrow(xyz) - 1)) {
          stop(
            "`linewidth` must have one value per path segment.",
            call. = FALSE
          )
        }
        color_length = length(color)
        for (i in seq_len(nrow(xyz) - 1)) {
          rgl::lines3d(
            xyz[i:(i + 1), ],
            color = color[((i - 1) %% color_length) + 1],
            tag = tag,
            lwd = linewidth[i],
            line_antialias = antialias
          )
        }
      } else {
        if (length(coord_list) != length(linewidth)) {
          stop(
            "`linewidth` must have one value per path feature.",
            call. = FALSE
          )
        }
        color_length = length(color)
        for (i in seq_len(length(coord_list))) {
          rgl::lines3d(
            coord_list[[i]],
            color = color[((i - 1) %% color_length) + 1],
            tag = tag,
            lwd = linewidth[i],
            line_antialias = antialias
          )
        }
      }
    } else {
      xyz = do.call(
        "rbind",
        lapply(coord_list, \(x) rbind(x, matrix(NA, ncol = 3, nrow = 1)))
      )
      xyz = xyz[-nrow(xyz), ]
      rgl::lines3d(
        xyz,
        color = color,
        tag = tag,
        lwd = linewidth,
        line_antialias = antialias
      )
    }
    render_zaxis_from_dots(
      zaxis_args = zaxis_args,
      extent = extent,
      panel = panel,
      zscale = zscale,
      heightmap = heightmap,
      caller = "render_path"
    )
  } else {
    return(coord_list)
  }
}

#' Coerce render path geometry to line strings
#'
#' @param path Spatial path input.
#'
#' @return Spatial path input with concrete line geometries.
#' @keywords internal
coerce_render_path_line_geometry = function(path) {
  if (!inherits(path, c("sf", "sfc"))) {
    return(path)
  }
  if (is_empty_scene_sf(path)) {
    return(path)
  }
  path = suppressWarnings(sf::st_collection_extract(path, "LINESTRING"))
  if (inherits(path, "sf")) {
    path = path[!sf::st_is_empty(path), , drop = FALSE]
  } else {
    path = path[!sf::st_is_empty(path)]
  }
  if (is_empty_scene_sf(path)) {
    return(path)
  }
  suppressWarnings(sf::st_cast(path, "LINESTRING"))
}

#' Is render line input
#'
#' @param x Object to test.
#'
#' @return Logical value.
#' @keywords internal
is_render_line_input = function(x) {
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


#' Convert scene coordinates to heightmap row and column coordinates
#'
#' @param heightmap Heightmap matrix.
#' @param x X scene coordinate.
#' @param z Z scene coordinate.
#' @param clamp Default `TRUE`. Whether to clamp coordinates to the heightmap.
#'
#' @return List with `row` and `col` coordinates.
#' @keywords internal
render_heightmap_row_col = function(heightmap, x, z, clamp = TRUE) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  row = x + (nr - 1) / 2 + 1
  col = z + (nc - 1) / 2 + 1
  if (isTRUE(clamp)) {
    row = pmin(pmax(row, 1), nr)
    col = pmin(pmax(col, 1), nc)
  }
  list(row = row, col = col)
}

#' Interpolate a render heightmap
#'
#' @param heightmap Heightmap matrix.
#' @param x X scene coordinate.
#' @param z Z scene coordinate.
#'
#' @return Numeric terrain heights.
#' @keywords internal
interpolate_render_heightmap_height = function(heightmap, x, z) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  if (nr < 2L || nc < 2L) {
    return(rep(heightmap[1L, 1L], length(x)))
  }
  row_col = render_heightmap_row_col(heightmap, x, z)
  row = row_col$row
  col = row_col$col
  row0 = pmin(pmax(floor(row), 1), nr - 1L)
  row1 = row0 + 1L
  col0 = pmin(pmax(floor(col), 1), nc - 1L)
  col1 = col0 + 1L
  row_weight = row - row0
  col_weight = col - col0

  height00 = heightmap[cbind(row0, col0)]
  height10 = heightmap[cbind(row1, col0)]
  height01 = heightmap[cbind(row0, col1)]
  height11 = heightmap[cbind(row1, col1)]
  top_triangle = row_weight + col_weight <= 1
  interpolated = numeric(length(row))
  interpolated[top_triangle] = height00[top_triangle] +
    row_weight[top_triangle] *
      (height10[top_triangle] - height00[top_triangle]) +
    col_weight[top_triangle] *
      (height01[top_triangle] - height00[top_triangle])
  interpolated[!top_triangle] = height11[!top_triangle] +
    (1 - col_weight[!top_triangle]) *
      (height10[!top_triangle] - height11[!top_triangle]) +
    (1 - row_weight[!top_triangle]) *
      (height01[!top_triangle] - height11[!top_triangle])

  nearest_row = as.integer(round(row))
  nearest_col = as.integer(round(col))
  nearest_height = heightmap[cbind(nearest_row, nearest_col)]
  fallback = !is.finite(interpolated)
  interpolated[fallback] = nearest_height[fallback]
  interpolated
}

#' Prepare render line geometry
#'
#' @param lines Spatial line input.
#' @param merge Whether to merge connected linework.
#' @param exclude_polygons Default `NULL`. Polygon geometry to remove before
#' merging.
#' @param line_argument Default `"lines"`. Line argument name used in errors.
#' @param polygon_argument Default `"exclude_polygons"`. Polygon argument name
#' used in errors.
#'
#' @return Normalized `sf` line geometry with stable feature lineage columns.
#' @keywords internal
prepare_render_line_geometry = function(
  lines,
  merge = TRUE,
  exclude_polygons = NULL,
  line_argument = "lines",
  polygon_argument = "exclude_polygons"
) {
  normalize_source_feature_ids = function(lines) {
    if (
      "render_line_source_feature_id" %in%
        names(lines) &&
        length(lines$render_line_source_feature_id) == nrow(lines)
    ) {
      return(lapply(lines$render_line_source_feature_id, function(value) {
        value = suppressWarnings(as.integer(unlist(value, use.names = FALSE)))
        sort(unique(value[is.finite(value)]))
      }))
    }
    lapply(seq_len(nrow(lines)), as.integer)
  }

  if (!is_render_line_input(lines)) {
    stop(
      sprintf(
        "`%s` must be an sf, sfc, sfg, SpatialLines, or SpatialLinesDataFrame line object.",
        line_argument
      ),
      call. = FALSE
    )
  }
  if (inherits(lines, c("SpatialLines", "SpatialLinesDataFrame"))) {
    lines = sf::st_as_sf(lines)
  }
  if (inherits(lines, "sfg")) {
    lines = sf::st_sfc(lines)
  }
  if (inherits(lines, "sfc")) {
    lines = sf::st_sf(geometry = lines)
  }
  lines$render_line_source_feature_id = I(
    normalize_source_feature_ids(lines)
  )
  lines = coerce_render_path_line_geometry(lines)
  if (inherits(lines, "sfc")) {
    lines = sf::st_sf(geometry = lines)
  }
  if (!is_empty_scene_sf(lines)) {
    lines$render_line_feature_id = seq_len(nrow(lines))
  } else {
    lines$render_line_feature_id = integer(0)
  }

  if (!is.null(exclude_polygons) && !is_empty_scene_sf(lines)) {
    if (
      !inherits(
        exclude_polygons,
        c(
          "sf",
          "sfc",
          "sfg",
          "SpatVector",
          "SpatialPolygons",
          "SpatialPolygonsDataFrame"
        )
      )
    ) {
      stop(
        sprintf(
          paste0(
            "`%s` must be an sf, sfc, sfg, SpatVector, SpatialPolygons, ",
            "or SpatialPolygonsDataFrame polygon object."
          ),
          polygon_argument
        ),
        call. = FALSE
      )
    }
    polygon_geometry = if (inherits(exclude_polygons, "sfg")) {
      sf::st_sfc(exclude_polygons)
    } else if (inherits(exclude_polygons, "sfc")) {
      exclude_polygons
    } else {
      sf::st_geometry(sf::st_as_sf(exclude_polygons))
    }
    if (
      length(polygon_geometry) > 0 &&
        !all(sf::st_is_empty(polygon_geometry))
    ) {
      polygon_geometry = sf::st_make_valid(polygon_geometry)
      polygon_types = as.character(sf::st_geometry_type(
        polygon_geometry,
        by_geometry = TRUE
      ))
      if (
        !all(
          polygon_types %in%
            c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION")
        )
      ) {
        stop(
          sprintf(
            "`%s` must contain only polygon or multipolygon geometries.",
            polygon_argument
          ),
          call. = FALSE
        )
      }
      polygon_geometry = suppressWarnings(
        sf::st_collection_extract(polygon_geometry, "POLYGON")
      )
      polygon_geometry = polygon_geometry[
        !sf::st_is_empty(polygon_geometry)
      ]
      if (!length(polygon_geometry)) {
        stop(
          sprintf(
            "`%s` does not contain any non-empty polygon geometries.",
            polygon_argument
          ),
          call. = FALSE
        )
      }

      line_crs = sf::st_crs(lines)
      polygon_crs = sf::st_crs(polygon_geometry)
      line_has_crs = !is.na(line_crs)
      polygon_has_crs = !is.na(polygon_crs)
      if (xor(line_has_crs, polygon_has_crs)) {
        stop(
          sprintf(
            "`%s` and `%s` must both have a CRS or both be CRS-less.",
            line_argument,
            polygon_argument
          ),
          call. = FALSE
        )
      }
      if (
        line_has_crs &&
          polygon_has_crs &&
          !scene_crs_equal(line_crs, polygon_crs)
      ) {
        polygon_geometry = sf::st_transform(polygon_geometry, line_crs)
      }
      polygon_union = suppressWarnings(sf::st_union(polygon_geometry))
      lines = tryCatch(
        suppressWarnings(sf::st_difference(lines, polygon_union)),
        error = function(e) {
          stop(
            sprintf(
              "Could not remove `%s` from `%s`: %s",
              polygon_argument,
              line_argument,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
      lines = coerce_render_path_line_geometry(lines)
      lines$render_line_feature_id = seq_len(nrow(lines))
    }
  }
  if (!isTRUE(merge) || is_empty_scene_sf(lines)) {
    return(lines)
  }

  geometry = sf::st_geometry(lines)
  source_feature_id = lines$render_line_source_feature_id
  merged_geometry = tryCatch(
    suppressWarnings(sf::st_line_merge(sf::st_union(geometry))),
    error = function(e) geometry
  )
  merged_geometry = coerce_render_path_line_geometry(merged_geometry)
  if (is_empty_scene_sf(merged_geometry)) {
    return(sf::st_sf(
      render_line_feature_id = integer(0),
      render_line_source_feature_id = I(list()),
      geometry = merged_geometry
    ))
  }
  source_overlap = sf::st_relate(
    merged_geometry,
    geometry,
    pattern = "1********"
  )
  merged_source_feature_id = lapply(source_overlap, function(source_row) {
    sort(unique(as.integer(unlist(
      source_feature_id[source_row],
      use.names = FALSE
    ))))
  })
  sf::st_sf(
    render_line_feature_id = seq_along(merged_geometry),
    render_line_source_feature_id = I(merged_source_feature_id),
    geometry = merged_geometry
  )
}


#' Create render line path data
#'
#' @param coords List of path coordinate matrices.
#' @param width Numeric width per path.
#' @param feature_id Integer normalized feature ID per path.
#' @param source_feature_id List of source feature IDs per path.
#'
#' @return Structured internal path-data list.
#' @keywords internal
new_render_line_path_data = function(
  coords,
  width,
  feature_id,
  source_feature_id
) {
  path_count = length(coords)
  if (
    length(width) != path_count ||
      length(feature_id) != path_count ||
      length(source_feature_id) != path_count
  ) {
    stop("Render line path data fields must have equal lengths.", call. = FALSE)
  }
  structure(
    list(
      coords = coords,
      width = as.numeric(width),
      feature_id = as.integer(feature_id),
      source_feature_id = source_feature_id
    ),
    class = c("render_line_path_data", "list")
  )
}

#' Convert spatial line features to scene path data
#'
#' @param lines Normalized `sf` line features carrying render feature IDs.
#' @param heightmap Heightmap matrix.
#' @param extent Scene extent.
#' @param zscale Effective zscale.
#'
#' @return Structured path data without assigned widths.
#' @keywords internal
render_spatial_line_path_data = function(
  lines,
  heightmap,
  extent,
  zscale
) {
  scene_path = auto_transform_scene_sf(
    sf_object = lines,
    extent = extent,
    heightmap = heightmap,
    caller = "render_path"
  )
  lines = scene_path$object
  if (!is.null(scene_path$extent)) {
    extent = scene_path$extent
  }
  filtered_path = filter_scene_sf_to_extent(
    sf_object = lines,
    extent = extent,
    heightmap = heightmap,
    filter_to_extent = TRUE,
    caller = "render_path"
  )
  lines = coerce_render_path_line_geometry(filtered_path$object)
  if (is_empty_scene_sf(lines)) {
    return(list(
      coords = list(),
      feature_id = integer(),
      source_feature_id = list()
    ))
  }
  if (!inherits(lines, "sf")) {
    stop("Spatial render line metadata requires an `sf` object.", call. = FALSE)
  }

  line_coordinates = sf::st_coordinates(lines)
  if (ncol(line_coordinates) == 3L) {
    groups = line_coordinates[, 3L]
    geometry_index = line_coordinates[, 3L]
  } else if (ncol(line_coordinates) == 4L) {
    groups = interaction(line_coordinates[, 3L], line_coordinates[, 4L])
    geometry_index = line_coordinates[, 4L]
  } else {
    stop(
      "Spatial render lines must have two-dimensional coordinates.",
      call. = FALSE
    )
  }
  coordinate_rows = split(seq_len(nrow(line_coordinates)), groups)
  coordinate_rows = coordinate_rows[lengths(coordinate_rows) >= 2L]
  if (!length(coordinate_rows)) {
    return(list(
      coords = list(),
      feature_id = integer(),
      source_feature_id = list()
    ))
  }
  source_row = unname(vapply(
    coordinate_rows,
    function(rows) {
      path_source_row = unique(as.integer(geometry_index[rows]))
      if (length(path_source_row) != 1L) {
        stop(
          "A spatial render path group must belong to exactly one feature.",
          call. = FALSE
        )
      }
      path_source_row
    },
    integer(1L)
  ))
  path_lengths = lengths(coordinate_rows)
  path_group = rep.int(seq_along(coordinate_rows), path_lengths)
  rows = unlist(coordinate_rows, use.names = FALSE)
  transformed_coords = transform_into_heightmap_coords(
    extent,
    heightmap,
    line_coordinates[rows, 2L],
    line_coordinates[rows, 1L],
    NULL,
    0,
    zscale,
    filter_bounds = FALSE,
    transform_scene = FALSE,
    caller = "render_path",
    missing_height_group = path_group
  )
  coord_rows = unname(split(seq_len(nrow(transformed_coords)), path_group))
  coords = lapply(coord_rows, function(rows) {
    transformed_coords[rows, , drop = FALSE]
  })
  feature_id = vapply(
    source_row,
    function(row) {
      as.integer(lines$render_line_feature_id[[row]])
    },
    integer(1L)
  )
  source_feature_id = lapply(source_row, function(row) {
    lines$render_line_source_feature_id[[row]]
  })
  list(
    coords = coords,
    feature_id = feature_id,
    source_feature_id = source_feature_id
  )
}

#' Render line coordinates by width
#'
#' @param lines Normalized spatial line input.
#' @param heightmap Heightmap matrix.
#' @param extent Scene extent.
#' @param zscale Effective zscale.
#' @param color Line color.
#' @param width Feature widths.
#' @param force_by_feature Default `FALSE`. Whether to preserve every feature
#' as a distinct rendered path when widths are equal.
#'
#' @return Structured internal path-data list.
#' @keywords internal
render_line_coords_by_width = function(
  lines,
  heightmap,
  extent,
  zscale,
  color,
  width,
  force_by_feature = FALSE
) {
  render_coords = function(lines, width) {
    render_path(
      y = lines,
      extent = extent,
      zscale = zscale,
      vertical_exaggeration = 1,
      heightmap = heightmap,
      offset = 0,
      linewidth = width,
      color = color,
      return_coords = TRUE,
      tag = "water_path"
    )
  }

  feature_count = if (inherits(lines, "sf")) {
    nrow(lines)
  } else if (inherits(lines, "sfc")) {
    length(lines)
  } else {
    NA_integer_
  }
  feature_id = if (
    inherits(lines, "sf") &&
      "render_line_feature_id" %in% names(lines)
  ) {
    as.integer(lines$render_line_feature_id)
  } else if (is.finite(feature_count)) {
    seq_len(feature_count)
  } else {
    1L
  }
  source_feature_id = if (
    inherits(lines, "sf") &&
      "render_line_source_feature_id" %in% names(lines)
  ) {
    lapply(lines$render_line_source_feature_id, function(value) {
      sort(unique(as.integer(value)))
    })
  } else if (is.finite(feature_count)) {
    lapply(seq_len(feature_count), as.integer)
  } else {
    list(1L)
  }
  if (
    inherits(lines, "sf") &&
      (isTRUE(force_by_feature) || length(width) > 1L)
  ) {
    spatial_path_data = render_spatial_line_path_data(
      lines = lines,
      heightmap = heightmap,
      extent = extent,
      zscale = zscale
    )
    path_feature_index = match(
      spatial_path_data$feature_id,
      feature_id
    )
    if (anyNA(path_feature_index)) {
      stop(
        "Spatial render path feature metadata did not match the input features.",
        call. = FALSE
      )
    }
    path_width = if (length(width) == 1L) {
      rep(width, length(spatial_path_data$coords))
    } else {
      width[path_feature_index]
    }
    return(new_render_line_path_data(
      coords = spatial_path_data$coords,
      width = path_width,
      feature_id = spatial_path_data$feature_id,
      source_feature_id = spatial_path_data$source_feature_id
    ))
  }
  if (
    isTRUE(force_by_feature) &&
      is.finite(feature_count) &&
      feature_count > 1L &&
      length(width) == 1L
  ) {
    width = rep(width, feature_count)
  }
  if (length(width) == 1L) {
    coords = render_coords(lines = lines, width = width)
    return(new_render_line_path_data(
      coords = coords,
      width = rep(width, length(coords)),
      feature_id = rep(feature_id[[1L]], length(coords)),
      source_feature_id = rep(source_feature_id[1L], length(coords))
    ))
  }

  coords = list()
  coord_width = numeric(0)
  coord_feature_id = integer(0)
  coord_source_feature_id = list()
  for (path_index in seq_along(width)) {
    path = if (inherits(lines, "sf")) {
      lines[path_index, , drop = FALSE]
    } else if (inherits(lines, "sfc")) {
      lines[path_index]
    } else {
      lines
    }
    path_coords = render_coords(
      lines = path,
      width = width[[path_index]]
    )
    if (!length(path_coords)) {
      next
    }
    coords = c(coords, path_coords)
    coord_width = c(
      coord_width,
      rep(width[[path_index]], length(path_coords))
    )
    coord_feature_id = c(
      coord_feature_id,
      rep(feature_id[[path_index]], length(path_coords))
    )
    coord_source_feature_id = c(
      coord_source_feature_id,
      rep(source_feature_id[path_index], length(path_coords))
    )
  }
  new_render_line_path_data(
    coords = coords,
    width = coord_width,
    feature_id = coord_feature_id,
    source_feature_id = coord_source_feature_id
  )
}

#' Densify render line coordinates
#'
#' @param coords List of scene coordinate matrices.
#' @param heightmap Heightmap matrix.
#' @param zscale Effective zscale.
#' @param offset Centerline offset in elevation units.
#'
#' @return List of densified coordinate matrices.
#' @keywords internal
densify_render_line_coords = function(coords, heightmap, zscale, offset) {
  heightmap_scene = heightmap / zscale
  offset_scene = offset / zscale
  lapply(coords, function(path_coords) {
    densify_single_render_line_coord(
      coords = path_coords,
      heightmap = heightmap_scene,
      offset = offset_scene
    )
  })
}

#' Calculate render line segment sample positions
#'
#' @param heightmap Heightmap matrix scaled into scene units.
#' @param segment_start Two-value segment start coordinate.
#' @param segment_end Two-value segment end coordinate.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
calculate_render_line_segment_t = function(
  heightmap,
  segment_start,
  segment_end
) {
  calculate_render_line_triangle_boundary_t(
    heightmap = heightmap,
    segment_start = segment_start,
    segment_end = segment_end
  )
}

#' Calculate render line terrain triangle boundary positions
#'
#' @param heightmap Heightmap matrix scaled into scene units.
#' @param segment_start Two-value segment start coordinate.
#' @param segment_end Two-value segment end coordinate.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
calculate_render_line_triangle_boundary_t = function(
  heightmap,
  segment_start,
  segment_end
) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  if (nr < 2 || nc < 2) {
    return(c(0, 1))
  }
  start_row_col = render_heightmap_row_col(
    heightmap,
    segment_start[[1]],
    segment_start[[2]],
    clamp = FALSE
  )
  end_row_col = render_heightmap_row_col(
    heightmap,
    segment_end[[1]],
    segment_end[[2]],
    clamp = FALSE
  )
  row0 = start_row_col$row
  row1 = end_row_col$row
  col0 = start_row_col$col
  col1 = end_row_col$col
  grid_t = unique_render_line_t(c(
    0,
    1,
    calculate_render_line_axis_boundary_t(row0, row1, 1, nr),
    calculate_render_line_axis_boundary_t(col0, col1, 1, nc)
  ))
  diagonal_t = calculate_render_line_diagonal_boundary_t(
    row0 = row0,
    row1 = row1,
    col0 = col0,
    col1 = col1,
    grid_t = grid_t,
    nr = nr,
    nc = nc
  )
  unique_render_line_t(c(grid_t, diagonal_t))
}

#' Calculate render line axis boundary positions
#'
#' @param start Axis start coordinate.
#' @param end Axis end coordinate.
#' @param lower Lower axis boundary.
#' @param upper Upper axis boundary.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
calculate_render_line_axis_boundary_t = function(start, end, lower, upper) {
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

#' Calculate render line terrain diagonal boundary positions
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
calculate_render_line_diagonal_boundary_t = function(
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

#' Return sorted unique render line segment positions
#'
#' @param t_values Segment interpolation values.
#'
#' @return Numeric vector of segment interpolation values.
#' @keywords internal
unique_render_line_t = function(t_values) {
  eps = sqrt(.Machine$double.eps)
  t_values = t_values[
    is.finite(t_values) &
      t_values >= -eps &
      t_values <= 1 + eps
  ]
  t_values = pmin(pmax(t_values, 0), 1)
  sort(unique(round(t_values, 12)))
}

#' Offset render line coordinates
#'
#' @param coords List of scene coordinate matrices.
#' @param offset Vertical offset in scene units.
#'
#' @return List of coordinate matrices.
#' @keywords internal
offset_render_line_coords = function(coords, offset) {
  lapply(coords, function(path_coords) {
    path_coords = as.matrix(path_coords)
    if (nrow(path_coords) > 0 && ncol(path_coords) >= 2) {
      path_coords[, 2] = path_coords[, 2] + offset
    }
    path_coords
  })
}

#' Densify one render line coordinate matrix
#'
#' @param coords Scene coordinate matrix.
#' @param heightmap Heightmap matrix scaled into scene units.
#' @param offset Centerline offset in scene units.
#'
#' @return Densified coordinate matrix.
#' @keywords internal
densify_single_render_line_coord = function(coords, heightmap, offset) {
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
    segment_t = calculate_render_line_segment_t(
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
  y_vals = interpolate_render_heightmap_height(heightmap, x_vals, z_vals)
  if (any(!is.finite(y_vals))) {
    y_vals[!is.finite(y_vals)] = min(heightmap, na.rm = TRUE)
  }
  cbind(x_vals, y_vals + offset, z_vals)
}
