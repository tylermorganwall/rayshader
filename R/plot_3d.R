is_spatial_heightmap_input = function(x) {
  is.character(x) ||
    inherits(x, "SpatRaster") ||
    inherits(x, c("RasterLayer", "RasterBrick", "RasterStack"))
}

is_spatial_heightmap_longlat = function(heightmap) {
  if (inherits(heightmap, "SpatRaster")) {
    return(isTRUE(tryCatch(terra::is.lonlat(heightmap), error = function(e) {
      FALSE
    })))
  }
  if (inherits(heightmap, c("RasterLayer", "RasterBrick", "RasterStack"))) {
    return(isTRUE(tryCatch(raster::isLonLat(heightmap), error = function(e) {
      FALSE
    })))
  }
  FALSE
}

extract_longlat_heightmap_zscale = function(heightmap, resolution) {
  if (length(resolution) < 2 || any(!is.finite(resolution[1:2]))) {
    return(NA_real_)
  }
  resolution = abs(resolution[1:2])
  if (any(resolution <= 0)) {
    return(NA_real_)
  }
  extent = tryCatch(get_extent(heightmap), error = function(e) NULL)
  if (is.null(extent)) {
    return(NA_real_)
  }
  center_lon = mean(extent[c("xmin", "xmax")])
  center_lat = mean(extent[c("ymin", "ymax")])
  if (!is.finite(center_lon) || !is.finite(center_lat)) {
    return(NA_real_)
  }
  lat_min = max(-90, center_lat - resolution[2] / 2)
  lat_max = min(90, center_lat + resolution[2] / 2)
  x_points = rbind(
    c(center_lon - resolution[1] / 2, center_lat),
    c(center_lon + resolution[1] / 2, center_lat)
  )
  y_points = rbind(
    c(center_lon, lat_min),
    c(center_lon, lat_max)
  )
  distances = tryCatch(
    {
      if (inherits(heightmap, "SpatRaster")) {
        crs_value = terra::crs(heightmap)
        c(
          as.numeric(terra::distance(terra::vect(x_points, crs = crs_value))),
          as.numeric(terra::distance(terra::vect(y_points, crs = crs_value)))
        )
      } else {
        c(
          raster::pointDistance(
            x_points[1, , drop = FALSE],
            x_points[2, , drop = FALSE],
            lonlat = TRUE
          ),
          raster::pointDistance(
            y_points[1, , drop = FALSE],
            y_points[2, , drop = FALSE],
            lonlat = TRUE
          )
        )
      }
    },
    error = function(e) NA_real_
  )
  distances = distances[is.finite(distances) & distances > 0]
  if (length(distances) == 0) {
    return(NA_real_)
  }
  mean(distances)
}

extract_spatial_heightmap_zscale = function(heightmap) {
  resolution = NULL
  if (inherits(heightmap, "SpatRaster")) {
    resolution = terra::res(heightmap)
  } else if (
    inherits(heightmap, c("RasterLayer", "RasterBrick", "RasterStack"))
  ) {
    resolution = raster::res(heightmap)
  }
  resolution = suppressWarnings(as.numeric(resolution))
  if (is_spatial_heightmap_longlat(heightmap)) {
    longlat_zscale = extract_longlat_heightmap_zscale(heightmap, resolution)
    if (is.finite(longlat_zscale) && longlat_zscale > 0) {
      return(longlat_zscale)
    }
  }
  positive_resolution = abs(resolution[is.finite(resolution) & resolution > 0])
  if (length(positive_resolution) == 0) {
    return(NA_real_)
  }
  mean(positive_resolution)
}

extract_spatial_heightmap_crs = function(heightmap) {
  if (inherits(heightmap, "SpatRaster")) {
    crs_candidates = list(
      tryCatch(terra::crs(heightmap), error = function(e) NULL),
      tryCatch(terra::crs(heightmap, proj = TRUE), error = function(e) NULL)
    )
    for (crs_candidate in crs_candidates) {
      parsed_crs = try_parse_scene_crs(crs_candidate)
      if (!is.null(parsed_crs)) {
        return(parsed_crs)
      }
    }
    return(NULL)
  }
  if (inherits(heightmap, c("RasterLayer", "RasterBrick", "RasterStack"))) {
    crs_val = tryCatch(raster::crs(heightmap), error = function(e) NULL)
    if (is.null(crs_val)) {
      return(NULL)
    }
    crs_candidates = list(
      tryCatch(comment(crs_val), error = function(e) NULL),
      crs_val,
      tryCatch(methods::slot(crs_val, "projargs"), error = function(e) NULL),
      tryCatch(as.character(crs_val), error = function(e) NULL),
      tryCatch(raster::projection(heightmap), error = function(e) NULL)
    )
    for (crs_candidate in crs_candidates) {
      parsed_crs = try_parse_scene_crs(crs_candidate)
      if (!is.null(parsed_crs)) {
        return(parsed_crs)
      }
    }
    return(NULL)
  }
  NULL
}

coerce_plot_3d_heightmap = function(heightmap) {
  info = list(
    heightmap = heightmap,
    extent = NULL,
    crs = NULL,
    zscale = NA_real_,
    is_spatial = FALSE
  )
  if (!is_spatial_heightmap_input(heightmap)) {
    return(info)
  }
  if (is.character(heightmap)) {
    heightmap = raster::raster(heightmap)
  }
  if (inherits(heightmap, "SpatRaster") && terra::nlyr(heightmap) > 1) {
    warning("`heightmap` has multiple layers; using the first layer.")
    heightmap = heightmap[[1]]
  }
  if (inherits(heightmap, c("RasterBrick", "RasterStack"))) {
    warning("`heightmap` has multiple layers; using the first layer.")
    heightmap = raster::raster(heightmap, layer = 1)
  }
  info$is_spatial = TRUE
  info$extent = get_extent(heightmap)
  info$crs = extract_spatial_heightmap_crs(heightmap)
  info$zscale = extract_spatial_heightmap_zscale(heightmap)
  info$heightmap = raster_to_matrix(heightmap, verbose = FALSE)
  info
}

#'@title Plot 3D
#'
#'@description Displays the shaded map in 3D with the `rgl` package.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'Note: Calling [plot_3d()] resets the scene cache for the [render_snapshot()], [render_depth()], and [render_highquality()]
#'
#'@param hillshade Hillshade/image to be added to 3D surface map.
#'@param heightmap Elevation input for the surface. Can be:
#'a two-dimensional matrix (each entry is elevation),
#'a `RasterLayer`/`RasterBrick`/`RasterStack`,
#'a `terra::SpatRaster`, or a raster filename.
#'Spatial raster inputs are automatically converted to matrix form with [raster_to_matrix()].
#'For spatial raster inputs, rayshader also caches the raster extent and CRS for downstream `render_*` calls.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
#'If `zscale` is not supplied and `heightmap` is a spatial raster, rayshader automatically
#'uses the raster cell resolution (mean x/y resolution). Geographic longitude/latitude
#'rasters are converted to an approximate meter-per-cell value at the raster center.
#'@param vertical_exaggeration Default `1`. One-off multiplier applied to the
#'effective visual relief for this scene. Values greater than `1` increase
#'apparent relief and values between `0` and `1` flatten it. This does not
#'update cached hillshade `zscale` metadata.
#'@param extent Default `NULL`. Optional extent metadata to cache with the scene for later
#'calls (e.g., [render_zaxis()] without an explicit `extent`). Accepts any input supported
#'by [get_extent()] (numeric xmin/xmax/ymin/ymax vector, `raster`, `terra`, `sf`, or `sp` objects).
#'If omitted and `heightmap` is a spatial raster, extent is extracted automatically.
#'@param baseshape Default `rectangle`. Shape of the base. Options are `c("rectangle","circle","hex")`.
#'@param solid Default `TRUE`. If `FALSE`, just the surface is rendered.
#'@param soliddepth Default `auto`, which sets it to the lowest elevation in the matrix minus one unit (scaled by zscale). Depth of the solid base. If heightmap is uniform and set on `auto`, this is automatically set to a slightly lower level than the uniform elevation.
#'@param solidcolor Default `grey20`. Base color.
#'@param solidlinecolor Default `grey30`. Base edge line color.
#'@param shadow Default `TRUE`. If `FALSE`, no shadow is rendered.
#'@param shadowdepth Default `auto`, which sets it to `soliddepth - soliddepth/10`. Depth of the shadow layer.
#'@param shadowcolor Default `auto`. Color of the shadow, automatically computed as `shadow_darkness`
#'the luminance of the `background` color in the CIELuv colorspace if not specified.
#'@param shadow_darkness Default `0.5`. Darkness of the shadow, if `shadowcolor = "auto"`.
#'@param shadowwidth Default `auto`, which sizes it to 1/10th the smallest dimension of `heightmap`. Width of the shadow in units of the matrix.
#'@param water Default `FALSE`. If `TRUE`, a water layer is rendered.
#'@param waterdepth Default `0`. Water level.
#'@param watercolor Default `lightblue`. Color of the water.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency.
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param lineantialias Default `FALSE`. Whether to anti-alias the lines in the scene.
#'@param soil Default `FALSE`. Whether to draw the solid base with a textured soil layer.
#'@param soil_freq Default `0.1`. Frequency of soil clumps. Higher frequency values give smaller soil clumps.
#'@param soil_levels Default `16`. Fractal level of the soil.
#'@param soil_color_light Default `"#b39474"`. Light tint of soil.
#'@param soil_color_dark Default `"#8a623b"`. Dark tint of soil.
#'@param soil_gradient Default `2`. Sharpness of the soil darkening gradient. `0` turns off the gradient entirely.
#'@param soil_gradient_darken Default `4`. Amount to darken the `soil_color_dark` value for the deepest soil layers. Higher
#'numbers increase the darkening effect.
#'@param theta Default `45`. Rotation around z-axis.
#'@param phi Default `45`. Azimuth angle.
#'@param fov Default `0`--isometric. Field-of-view angle.
#'@param zoom Default `1`. Zoom factor.
#'@param background Default `grey10`. Color of the background.
#'@param windowsize Default `600`. Position, width, and height of the `rgl` device displaying the plot.
#'If a single number, viewport will be a square and located in upper left corner.
#'If two numbers, (e.g. `c(600,800)`), user will specify width and height separately.
#'If four numbers (e.g. `c(200,0,600,800)`), the first two coordinates
#'specify the location of the x-y coordinates of the bottom-left corner of the viewport on the screen,
#'and the next two (or one, if square) specify the window size. NOTE: The absolute positioning of the
#'window does not currently work on macOS (tested on Mojave), but the size can still be specified.
#'@param precomputed_normals Default `NULL`. Takes the output of `calculate_normals()` to save
#' computing normals internally.
#'@param triangulate Default `FALSE`. Reduce the size of the 3D model by triangulating the height map.
#'Set this to `TRUE` if generating the model is slow, or moving it is choppy. Will also reduce the size
#'of 3D models saved to disk.
#'@param max_error Default `0.001`. Maximum allowable error when triangulating the height map,
#'when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with [render_highquality()], or need
#'to save a smaller 3D OBJ file to disk with [save_obj()],
#'@param max_tri Default `0`, which turns this setting off and uses `max_error`.
#'Maximum number of triangles allowed with triangulating the
#'height map, when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with [render_highquality()], or need
#'to save a smaller 3D OBJ file to disk with [save_obj()],
#'@param verbose Default `TRUE`, if `interactive()`. Prints information about the mesh triangulation
#'if `triangulate = TRUE`.
#'@param plot_new Default `TRUE`, opens new window with each [plot_3d()] call. If `FALSE`,
#'the data will be plotted in the same window.
#'@param close_previous Default `TRUE`. Closes any previously open `rgl` window. If `FALSE`,
#'old windows will be kept open.
#'@param clear_previous Default `TRUE`. Clears the previously open `rgl` window if `plot_new = FALSE`.
#'
#'@import rgl
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Plotting a spherical texture map of the built-in `montereybay` dataset.
#'montereybay |>
#'  sphere_shade(texture="desert", vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4)
#'render_snapshot()
#'
#'montereybay |>
#'  sphere_shade(texture="desert", vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4)
#'render_zaxis(zaxis_location = "bottomleft")
#'render_snapshot()
#'
#'#With a water layer
#'montereybay |>
#'  sphere_shade(texture="imhof2", vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4, water = TRUE, watercolor="imhof2",
#'          waterlinecolor="white", waterlinealpha=0.5)
#'render_snapshot()
#'
#'#With a soil texture to the base
#'montereybay |>
#'  sphere_shade(texture="imhof3", vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4, water = TRUE,  watercolor="imhof4",
#'          waterlinecolor="white", waterlinealpha=0.5, soil=TRUE)
#'render_camera(theta=225, phi=7, zoom=0.5, fov=67)
#'render_snapshot()
#'
#'#We can also change the base by setting "baseshape" to "hex" or "circle"
#'montereybay |>
#'  sphere_shade(texture="imhof1", vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4, water = TRUE, watercolor="imhof1", theta=-45, zoom=0.7,
#'          waterlinecolor="white", waterlinealpha=0.5,baseshape="circle")
#'render_snapshot()
#'
#'montereybay |>
#'  sphere_shade(texture="imhof1", vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4, water = TRUE, watercolor="imhof1", theta=-45, zoom=0.7,
#'          waterlinecolor="white", waterlinealpha=0.5,baseshape="hex")
#'render_snapshot()
#'
#'
#'
#'#Or we can carve out the region of interest ourselves, by setting those entries to NA
#'#to the elevation map passed into `plot_3d`
#'
#'#Here, we only include the deep bathymetry data by setting all points greater than -10
#'#in the copied elevation matrix to NA.
#'
#'mb_water = montereybay
#'mb_water[mb_water > -10] = NA
#'
#'montereybay |>
#'  sphere_shade(texture="imhof1", vertical_exaggeration = 10) |>
#'  plot_3d(mb_water, vertical_exaggeration = 4, water = TRUE, watercolor="imhof1", theta=-45,
#'          waterlinecolor="white", waterlinealpha=0.5)
#'render_snapshot()
plot_3d = function(
  hillshade,
  heightmap,
  zscale = 1,
  vertical_exaggeration = 1,
  baseshape = "rectangle",
  solid = TRUE,
  soliddepth = "auto",
  solidcolor = "grey20",
  solidlinecolor = "grey30",
  shadow = TRUE,
  shadowdepth = "auto",
  shadowcolor = "auto",
  shadow_darkness = 0.5,
  shadowwidth = "auto",
  water = FALSE,
  waterdepth = 0,
  watercolor = "dodgerblue",
  wateralpha = 0.5,
  waterlinecolor = NULL,
  waterlinealpha = 1,
  linewidth = 2,
  lineantialias = FALSE,
  soil = FALSE,
  soil_freq = 0.1,
  soil_levels = 16,
  soil_color_light = "#b39474",
  soil_color_dark = "#8a623b",
  soil_gradient = 2,
  soil_gradient_darken = 4,
  theta = 45,
  phi = 45,
  fov = 0,
  zoom = 1,
  background = "white",
  windowsize = 600,
  precomputed_normals = NULL,
  triangulate = FALSE,
  max_error = 0,
  max_tri = 0,
  verbose = FALSE,
  plot_new = TRUE,
  close_previous = TRUE,
  clear_previous = TRUE,
  extent = NULL
) {
  if (!plot_new && clear_previous) {
    rgl::clear3d()
  }
  force(hillshade)
  heightmap_was_missing = missing(heightmap)
  zscale_was_missing = missing(zscale)
  vertical_exaggeration_was_missing = missing(vertical_exaggeration)
  extent_was_missing = missing(extent)
  heightmap_cache_label = NULL
  zscale_cache_input_label = format_scene_cache_label(deparse(substitute(
    zscale
  )))
  vertical_exaggeration_cache_label = if (vertical_exaggeration_was_missing) {
    "plot_3d_default_vertical_exaggeration"
  } else {
    format_scene_cache_label(deparse(substitute(vertical_exaggeration)))
  }
  resolved_heightmap = NULL
  if (heightmap_was_missing) {
    resolved_heightmap = resolve_hillshade_heightmap(
      heightmap_missing = TRUE,
      caller = "plot_3d"
    )
    heightmap = resolved_heightmap$heightmap
    heightmap_cache_label = resolved_heightmap$label
    allow_scene_zscale_cache = identical(resolved_heightmap$source, "scene")
  } else {
    heightmap_cache_label = format_scene_cache_label(deparse(substitute(
      heightmap
    )))
    allow_scene_zscale_cache = FALSE
  }
  heightmap_info = coerce_plot_3d_heightmap(heightmap)
  heightmap = heightmap_info$heightmap
  auto_extent = heightmap_info$extent
  auto_crs = heightmap_info$crs
  auto_zscale = heightmap_info$zscale
  explicit_heightmap_uses_default_zscale = !heightmap_was_missing &&
    !(is.finite(auto_zscale) && auto_zscale > 0)
  extent_cache_value = NULL
  extent_cache_label = NULL
  if (!extent_was_missing && !is.null(extent)) {
    extent_cache_value = extent
    extent_cache_label = format_scene_cache_label(deparse(substitute(extent)))
  } else if (!is.null(auto_extent)) {
    extent_cache_value = auto_extent
    extent_cache_label = format_scene_cache_label(sprintf(
      "%s_auto_extent",
      heightmap_cache_label
    ))
  } else if (
    heightmap_was_missing &&
      !is.null(resolved_heightmap) &&
      identical(resolved_heightmap$source, "hillshade")
  ) {
    extent_cache_value = get_hillshade_extent(default = NULL)
    extent_cache_label = get_hillshade_extent_label(default = NULL)
  } else if (
    heightmap_was_missing &&
      !is.null(resolved_heightmap) &&
      identical(resolved_heightmap$source, "scene")
  ) {
    extent_cache_value = get_scene_extent(default = NULL)
    extent_cache_label = get_scene_extent_label(default = NULL)
  }
  resolved_zscale = resolve_hillshade_zscale(
    zscale = zscale,
    zscale_missing = zscale_was_missing,
    caller = "plot_3d",
    auto_zscale = auto_zscale,
    allow_hillshade_cache = !explicit_heightmap_uses_default_zscale,
    allow_scene_cache = allow_scene_zscale_cache
  )
  zscale = resolved_zscale$zscale
  base_zscale = zscale
  zscale_cache_label = switch(
    resolved_zscale$source,
    explicit = zscale_cache_input_label,
    auto = format_scene_cache_label(sprintf(
      "%s_auto_zscale",
      heightmap_cache_label
    )),
    hillshade = resolved_zscale$label,
    scene = resolved_zscale$label,
    NULL
  )
  resolved_vertical_exaggeration = resolve_vertical_exaggeration(
    vertical_exaggeration = vertical_exaggeration,
    caller = "plot_3d"
  )
  cached_scene_vertical_exaggeration = resolved_vertical_exaggeration
  if (
    vertical_exaggeration_was_missing &&
      identical(resolved_zscale$source, "scene")
  ) {
    prior_scene_vertical_exaggeration =
      get_scene_vertical_exaggeration(default = NA_real_)
    if (
      is.finite(prior_scene_vertical_exaggeration) &&
        prior_scene_vertical_exaggeration > 0
    ) {
      resolved_vertical_exaggeration = prior_scene_vertical_exaggeration
      cached_scene_vertical_exaggeration = prior_scene_vertical_exaggeration
      vertical_exaggeration_cache_label =
        get_scene_vertical_exaggeration_label(default = NULL)
    }
  }
  zscale = apply_vertical_exaggeration(
    zscale = zscale,
    vertical_exaggeration = resolved_vertical_exaggeration,
    caller = "plot_3d"
  )
  crs_cache_value = NULL
  crs_cache_label = NULL
  if (!is.null(auto_crs)) {
    crs_cache_value = auto_crs
    crs_cache_label = format_scene_cache_label(sprintf(
      "%s_auto_crs",
      heightmap_cache_label
    ))
  } else if (
    heightmap_was_missing &&
      !is.null(resolved_heightmap) &&
      identical(resolved_heightmap$source, "hillshade")
  ) {
    crs_cache_value = get_hillshade_crs(default = NULL)
    crs_cache_label = get_hillshade_crs_label(default = NULL)
  } else if (
    heightmap_was_missing &&
      !is.null(resolved_heightmap) &&
      identical(resolved_heightmap$source, "scene")
  ) {
    crs_cache_value = get_scene_crs(default = NULL)
    crs_cache_label = get_scene_crs_label(default = NULL)
  }
  reset_scene_context(
    clear_scene_metadata = TRUE,
    clear_scene_cache = TRUE
  )
  if (shadowcolor == "auto") {
    shadowcolor = convert_color(
      darken_color(background, darken = shadow_darkness),
      as_hex = TRUE
    )
  }
  #Set window size and position
  if (length(windowsize) == 1) {
    windowsize = c(0, 0, windowsize, windowsize)
  } else if (length(windowsize) == 2) {
    windowsize = c(0, 0, windowsize)
  } else if (length(windowsize) == 3) {
    windowsize = c(
      windowsize[1],
      windowsize[2],
      windowsize[1] + windowsize[3],
      windowsize[2] + windowsize[3]
    )
  } else if (length(windowsize) == 4) {
    windowsize = c(
      windowsize[1],
      windowsize[2],
      windowsize[1] + windowsize[3],
      windowsize[2] + windowsize[4]
    )
  } else {
    stop(paste0(
      "Don't know what to do with `windowsize` argument of length ",
      length(windowsize)
    ))
  }
  heightmap = generate_base_shape(heightmap, baseshape)
  hillshade = rayimage::render_clamp(hillshade)

  if (is.null(heightmap)) {
    stop(
      "heightmap argument missing--need to input both hillshade and original elevation matrix"
    )
  }
  min_height = min(heightmap, na.rm = TRUE)
  max_height = max(heightmap, na.rm = TRUE)
  if (soliddepth == "auto") {
    if (min_height != max_height) {
      soliddepth = min_height /
        zscale -
        (max_height / zscale - min_height / zscale) / 5
    } else {
      max_dim = max(dim(heightmap))
      soliddepth = min_height / zscale - max_dim / 25
    }
  } else {
    if (soliddepth > min_height) {
      message(sprintf(
        "`soliddepth` (set to %f) must be less than or equal to heightmap minimum value (%f). Setting to min(heightmap)",
        soliddepth,
        min_height
      ))
      soliddepth = min_height / zscale
    } else {
      soliddepth = soliddepth / zscale
    }
  }
  if (solid) {
    min_height_shadow = min(c(min_height, soliddepth * zscale))
  } else {
    min_height_shadow = min_height
  }
  if (shadowdepth == "auto") {
    if (min_height_shadow != max_height) {
      if (solid) {
        shadowdepth = soliddepth -
          (max_height / zscale - min_height_shadow / zscale) / 5
      } else {
        shadowdepth = min_height_shadow /
          zscale -
          (max_height / zscale - min_height_shadow / zscale) / 5
      }
    } else {
      if (solid) {
        max_dim = max(dim(heightmap))
        shadowdepth = soliddepth - max_dim / 25
      } else {
        max_dim = max(dim(heightmap))
        shadowdepth = min_height - max_dim / 25
      }
    }
  } else {
    if (shadowdepth > min_height) {
      message(sprintf(
        "`shadowdepth` (set to %f) is greater to heightmap minimum value (%f). Shadow will appear to be intersecting 3D model.",
        shadowdepth,
        min_height
      ))
    } else {
      shadowdepth = shadowdepth / zscale
    }
  }
  if (shadowwidth == "auto") {
    shadowwidth = max(floor(min(dim(heightmap)) / 10), 5)
  }
  if (water) {
    if (watercolor == "imhof1") {
      watercolor = "#defcf5"
    } else if (watercolor == "imhof2") {
      watercolor = "#337c73"
    } else if (watercolor == "imhof3") {
      watercolor = "#4e7982"
    } else if (watercolor == "imhof4") {
      watercolor = "#638d99"
    } else if (watercolor == "desert") {
      watercolor = "#caf0f7"
    } else if (watercolor == "bw") {
      watercolor = "#dddddd"
    } else if (watercolor == "unicorn") {
      watercolor = "#ff00ff"
    }
    if (is.null(waterlinecolor)) {
    } else if (waterlinecolor == "imhof1") {
      waterlinecolor = "#f9fffb"
    } else if (waterlinecolor == "imhof2") {
      waterlinecolor = "#8accc4"
    } else if (waterlinecolor == "imhof3") {
      waterlinecolor = "#8cd4e2"
    } else if (waterlinecolor == "imhof4") {
      waterlinecolor = "#c7dfe5"
    } else if (waterlinecolor == "desert") {
      waterlinecolor = "#cde3f2"
    } else if (waterlinecolor == "bw") {
      waterlinecolor = "#ffffff"
    } else if (waterlinecolor == "unicorn") {
      waterlinecolor = "#ffd1fb"
    }
  }
  tempmap = tempfile(fileext = ".png")
  rayimage::ray_write_image(hillshade, tempmap)
  precomputed = FALSE
  if (is.list(precomputed_normals)) {
    normals = precomputed_normals
    precomputed = TRUE
  }
  if (triangulate && any(is.na(heightmap))) {
    if (interactive()) {
      message(
        "`triangulate = TRUE` cannot be currently set if any NA values present--settings `triangulate = FALSE`"
      )
    }
    triangulate = FALSE
  }

  if (close_previous && rgl::cur3d() != 0) {
    rgl::close3d()
  }
  if (plot_new || rgl::cur3d() == 0) {
    rgl::open3d(
      windowRect = windowsize,
      mouseMode = c("none", "polar", "fov", "zoom", "pull")
    )
  }
  cache_scene_context_token()
  rgl::view3d(zoom = zoom, phi = phi, theta = theta, fov = fov)
  attributes(heightmap) = attributes(heightmap)["dim"]
  tag_surface = sprintf(
    "surface_tris-dim_%i_%i",
    nrow(heightmap),
    ncol(heightmap)
  )
  if (!triangulate) {
    if (!precomputed) {
      normals = calculate_normal(heightmap, zscale = zscale)
    }
    dim(heightmap) = unname(dim(heightmap))
    normalsx = (t(normals$x[c(-1, -nrow(normals$x)), c(-1, -ncol(normals$x))]))
    normalsy = (t(normals$z[c(-1, -nrow(normals$z)), c(-1, -ncol(normals$z))]))
    normalsz = (t(normals$y[c(-1, -nrow(normals$y)), c(-1, -ncol(normals$y))]))
    replace_na_vals = is.na(normalsx) | is.na(normalsy) | is.na(normalsz)
    normalsx[replace_na_vals] = 0
    normalsy[replace_na_vals] = 1
    normalsz[replace_na_vals] = 0

    ray_surface = generate_surface(heightmap, zscale = zscale)
    rgl::triangles3d(
      x = ray_surface$verts,
      indices = ray_surface$inds,
      texcoords = ray_surface$texcoords,
      normals = matrix(c(normalsz, normalsy, -normalsx), ncol = 3L),
      texture = tempmap,
      color = "white",
      lit = FALSE,
      tag = tag_surface,
      back = "culled"
    )
  } else {
    tris = terrainmeshr::triangulate_matrix(
      heightmap,
      maxError = max_error,
      maxTriangles = max_tri,
      start_index = 0L,
      verbose = verbose
    )
    index_vals = seq_len(nrow(tris))
    # if(!precomputed) {
    #   normals = calculate_normal(heightmap,zscale=zscale)
    # }
    # normalsx = as.vector(t(flipud(normals$x[c(-1,-nrow(normals$x)),c(-1,-ncol(normals$x))])))
    # normalsy = as.vector(t(flipud(normals$z[c(-1,-nrow(normals$z)),c(-1,-ncol(normals$z))])))
    # normalsz = as.vector(t(flipud(normals$y[c(-1,-nrow(normals$y)),c(-1,-ncol(normals$y))])))
    tris[, 2] = tris[, 2] / zscale
    nr = nrow(heightmap)
    nc = ncol(heightmap)
    # rn = tris[,1]+1
    # cn = tris[,3]+1

    # normal_comp = matrix(c(normalsz[rn + nr*(cn-1)],normalsy[rn + nr*(cn-1)],-normalsx[rn + nr*(cn-1)]),ncol=3)
    texcoords = tris[, c(1, 3)]
    texcoords[, 1] = texcoords[, 1] / (nr - 1)
    texcoords[, 2] = texcoords[, 2] / (nc - 1)
    tris[, 1] = tris[, 1] - (nr - 1) / 2 # +1
    tris[, 3] = tris[, 3] - (nc - 1) / 2
    tris[, 3] = -tris[, 3]

    rgl::triangles3d(
      tris,
      texcoords = texcoords,
      indices = index_vals,
      back = "cull",
      #normals = normal_comp,
      texture = tempmap,
      lit = FALSE,
      color = "white",
      tag = tag_surface
    )
  }
  rgl::bg3d(color = background, texture = NULL)
  if (solid && !triangulate) {
    make_base(
      heightmap,
      basedepth = soliddepth,
      basecolor = solidcolor,
      zscale = zscale,
      soil = soil,
      soil_freq = soil_freq,
      soil_levels = soil_levels,
      soil_color1 = soil_color_light,
      soil_color2 = soil_color_dark,
      soil_gradient = soil_gradient,
      gradient_darken = soil_gradient_darken
    )
  } else if (solid && triangulate) {
    make_base_triangulated(tris, basedepth = soliddepth, basecolor = solidcolor)
  }
  if (!is.null(solidlinecolor) && solid) {
    make_lines(
      heightmap,
      basedepth = soliddepth,
      linecolor = solidlinecolor,
      zscale = zscale,
      linewidth = linewidth
    )
  }
  if (shadow) {
    make_shadow(heightmap, shadowdepth, shadowwidth, background, shadowcolor)
  }
  if (water) {
    make_water(
      heightmap,
      waterheight = waterdepth,
      wateralpha = wateralpha,
      watercolor = watercolor,
      zscale = zscale
    )
  }
  if (!is.null(waterlinecolor) && water) {
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
      linewidth = linewidth,
      antialias = lineantialias
    )
  }
  cache_hillshade_heightmap(heightmap, label = heightmap_cache_label)
  cache_hillshade_zscale(base_zscale, label = zscale_cache_label)
  cache_scene_heightmap(heightmap, label = heightmap_cache_label)
  cache_scene_zscale(base_zscale, label = zscale_cache_label)
  cache_scene_vertical_exaggeration(
    cached_scene_vertical_exaggeration,
    label = vertical_exaggeration_cache_label
  )
  cache_scene_extent(extent_cache_value, label = extent_cache_label)
  cache_scene_crs(crs_cache_value, label = crs_cache_label)
  invisible(NULL)
}
