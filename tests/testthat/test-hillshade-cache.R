clear_hillshade_test_cache = function() {
  clear_hillshade_cache()
  reset_scene_context(
    clear_scene_metadata = TRUE,
    clear_scene_cache = TRUE
  )
  invisible(NULL)
}

test_that("hillshade functions reuse cached heightmap", {
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  sphere_shade(volcano, vertical_exaggeration = 50)

  expect_equal(get_hillshade_heightmap(), volcano)
  expect_null(get_hillshade_zscale(default = NULL))
  expect_equal(
    height_shade(),
    height_shade(volcano)
  )
  expect_equal(
    constant_shade(color = "red", alpha = 0.5),
    constant_shade(volcano, color = "red", alpha = 0.5)
  )
  expect_equal(
    texture_shade(detail = 0.25, contrast = 2, brightness = 1),
    texture_shade(volcano, detail = 0.25, contrast = 2, brightness = 1)
  )
})

test_that("detect_water reuses cached hillshade heightmap and zscale", {
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())
  withr::local_options(list(rayshader.verbose_scene_cache = TRUE))

  volcano_water = volcano
  volcano_water[volcano_water < mean(volcano_water)] = mean(volcano_water)
  raster_heightmap = raster::raster(volcano_water)
  raster::res(raster_heightmap) = c(30, 30)

  sphere_shade(raster_heightmap)

  out = character()
  cached_water = withCallingHandlers(
    detect_water(min_area = 25),
    message = function(cnd) {
      out <<- c(out, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )

  expect_equal(
    cached_water,
    detect_water(raster_heightmap, min_area = 25)
  )
  expect_true(any(grepl("hillshade_heightmap", out, fixed = TRUE)))
  expect_true(any(grepl("hillshade_zscale", out, fixed = TRUE)))
})

test_that("terrain-derived overlays reuse the cached heightmap", {
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  heightmap = matrix(seq_len(400), nrow = 20, ncol = 20)
  height_shade(heightmap)
  contour_cached = generate_contour_overlay()
  contour_explicit = generate_contour_overlay(heightmap)
  expect_equal(contour_cached, contour_explicit)

  overlay = constant_shade(heightmap, color = "white")
  altitude_cached = generate_altitude_overlay(
    overlay,
    start_transition = 200
  )
  altitude_explicit = generate_altitude_overlay(
    overlay,
    heightmap,
    start_transition = 200
  )
  expect_equal(altitude_cached, altitude_explicit)

  water_heightmap = heightmap
  water_heightmap[6:15, 6:15] = 0
  height_shade(water_heightmap)
  waterline_cached = generate_waterline_overlay(
    min_area = 1,
    return_distance_matrix = TRUE
  )
  waterline_explicit = generate_waterline_overlay(
    water_heightmap,
    min_area = 1,
    return_distance_matrix = TRUE
  )
  expect_equal(waterline_cached, waterline_explicit)
})

test_that("hillshade map functions cache the latest map texture", {
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  sphere_map = sphere_shade(volcano, vertical_exaggeration = 50)
  expect_equal(get_hillshade_map(), sphere_map)

  height_map = height_shade(volcano)
  expect_equal(get_hillshade_map(), height_map)

  constant_map = constant_shade(volcano, color = "red", alpha = 0.5)
  expect_equal(get_hillshade_map(), constant_map)

  overlay_map = add_overlay(
    sphere_map,
    height_shade(volcano),
    alphalayer = 0.5
  )
  expect_equal(get_hillshade_map(), overlay_map)

  shadow_map = add_shadow(
    sphere_map,
    ray_shade(volcano, zscale = 50, maxsearch = 10, sunaltitude = 25),
    max_darken = 0.5
  )
  expect_equal(get_hillshade_map(), shadow_map)

  volcano_water = volcano
  volcano_water[volcano_water < mean(volcano_water)] = mean(volcano_water)
  water_map = add_water(
    sphere_map,
    detect_water(volcano_water, min_area = 25),
    color = "desert"
  )
  expect_equal(get_hillshade_map(), water_map)
})

test_that("new explicit 2D heightmap invalidates stale cached hillshade metadata", {
  skip_if_not_installed("sf")
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  ray_shade(volcano, zscale = 50, sunaltitude = 25, sunangle = 225)
  expect_equal(get_hillshade_zscale(), 50)

  height_shade(volcano)
  expect_null(get_hillshade_zscale(default = NULL))

  texture_shade(volcano, detail = 0.25, contrast = 2, brightness = 1)
  expect_null(get_hillshade_zscale(default = NULL))

  raster_heightmap = raster::raster(volcano)
  raster::extent(raster_heightmap) = c(10, 20, 30, 40)
  raster::res(raster_heightmap) = c(30, 30)
  raster::crs(raster_heightmap) = sf::st_crs(3857)$wkt
  sphere_shade(raster_heightmap)
  expect_equal(get_hillshade_zscale(), 30)
  expect_equal(
    unname(get_extent(get_hillshade_extent())),
    unname(get_extent(raster_heightmap))
  )
  expect_equal(get_hillshade_crs()$epsg, 3857)
  expect_false(sf::st_is_longlat(get_hillshade_crs()))

  height_shade(volcano)
  expect_null(get_hillshade_zscale(default = NULL))
  expect_null(get_hillshade_extent(default = NULL))
  expect_null(get_hillshade_crs(default = NULL))
})

test_that("lonlat SpatRaster hillshade caches meter zscale for plot_3d", {
  skip_if_not_installed("terra")
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  rast = terra::rast(
    nrows = 6,
    ncols = 6,
    xmin = -122.366765,
    xmax = -121.366765,
    ymin = 36.179392,
    ymax = 37.179392,
    crs = "EPSG:4326"
  )
  terra::values(rast) = seq_len(terra::ncell(rast))

  auto_zscale = extract_spatial_heightmap_zscale(rast)
  expect_gt(auto_zscale, 10000)
  expect_lt(auto_zscale, 25000)

  hillshade = sphere_shade(rast)
  expect_equal(get_hillshade_zscale(), auto_zscale, tolerance = 1e-8)
  expect_equal(
    hillshade,
    sphere_shade(raster_to_matrix(rast, verbose = FALSE), zscale = auto_zscale)
  )

  hillshade = sphere_shade(rast)
  expect_no_condition(plot_3d_test(
    hillshade,
    shadow = FALSE,
    solid = FALSE,
    windowsize = c(100, 100)
  ))
  expect_equal(get_scene_zscale(), auto_zscale, tolerance = 1e-8)
  expect_equal(
    unname(get_extent(get_scene_extent())),
    unname(get_extent(rast))
  )
  if (requireNamespace("sf", quietly = TRUE)) {
    expect_true(sf::st_is_longlat(get_scene_crs()))
  }
})

test_that("raster-backed hillshade metadata supports cached 2D spatial overlays", {
  skip_if_not_installed("sf")
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  polygon_ll = sf::st_as_sfc(sf::st_bbox(
    c(
      xmin = -122.5,
      xmax = -121.5,
      ymin = 36.5,
      ymax = 37.5
    ),
    crs = sf::st_crs(4326)
  ))
  polygon_merc = sf::st_transform(polygon_ll, 3857)
  polygon_bbox = sf::st_bbox(polygon_merc)

  raster_heightmap = raster::raster(volcano)
  raster::extent(raster_heightmap) = c(
    polygon_bbox["xmin"],
    polygon_bbox["xmax"],
    polygon_bbox["ymin"],
    polygon_bbox["ymax"]
  )
  raster::crs(raster_heightmap) = sf::st_crs(3857)$wkt

  base_map = height_shade(raster_heightmap)

  polygon_overlay = generate_polygon_overlay(
    geometry = sf::st_sf(id = 1, geometry = polygon_ll),
    palette = "dodgerblue3",
    linecolor = NA
  )
  line_overlay = generate_line_overlay(
    geometry = sf::st_sf(
      id = 1,
      geometry = sf::st_sfc(
        sf::st_linestring(matrix(
          c(-122.4, 36.6, -121.6, 37.4),
          ncol = 2,
          byrow = TRUE
        )),
        crs = sf::st_crs(4326)
      )
    ),
    color = "grey20",
    linewidth = 2
  )

  expect_equal(dim(polygon_overlay)[1:2], dim(base_map)[1:2])
  expect_equal(dim(line_overlay)[1:2], dim(base_map)[1:2])
  expect_true(any(polygon_overlay[,, 4] > 0))
  expect_true(any(line_overlay[,, 4] > 0))
})

test_that("line overlays fill non-square raster extent instead of preserving plot aspect", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  rast = terra::rast(
    nrows = 100,
    ncols = 200,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100,
    crs = "EPSG:3857"
  )
  terra::values(rast) = seq_len(terra::ncell(rast))
  invisible(height_shade(rast))

  left_edge = sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(
        c(0, 0, 0, 100),
        ncol = 2,
        byrow = TRUE
      )),
      crs = 3857
    )
  )
  overlay = generate_line_overlay(left_edge, linewidth = 2)
  alpha = overlay[,, 4] > 0
  cols = which(colSums(alpha) > 0)

  expect_equal(dim(overlay)[1:2], c(100, 200))
  expect_lte(min(cols), 2)
})

test_that("invalid cached hillshade CRS does not abort spatial overlays", {
  skip_if_not_installed("sf")
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  cache_hillshade_heightmap(volcano, label = "volcano")
  cache_hillshade_extent(raster::extent(c(0, nrow(volcano), 0, ncol(volcano))))
  cache_hillshade_crs("not a crs")

  expect_no_error(generate_polygon_overlay(
    geometry = sf::st_sf(
      id = 1,
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(10, 10, 20, 10, 20, 20, 10, 20, 10, 10),
          ncol = 2,
          byrow = TRUE
        )))
      )
    ),
    palette = "dodgerblue3",
    linecolor = NA
  ))
})

test_that("generate_polygon_overlay returns transparent overlay for empty crop", {
  skip_if_not_installed("sf")

  empty_overlay = generate_polygon_overlay(
    geometry = sf::st_sf(
      id = 1,
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(200, 200, 210, 200, 210, 210, 200, 210, 200, 200),
          ncol = 2,
          byrow = TRUE
        )))
      )
    ),
    extent = c(0, nrow(volcano), 0, ncol(volcano)),
    heightmap = volcano,
    palette = "dodgerblue3",
    linecolor = NA
  )

  expect_equal(dim(empty_overlay), c(ncol(volcano), nrow(volcano), 4))
  expect_true(all(empty_overlay[,, 4] == 0))
})

test_that("sphere_shade explicit zscale overrides and caches hillshade zscale", {
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  sphere_shade(volcano, zscale = 12)
  expect_equal(get_hillshade_zscale(), 12)

  sphere_shade(vertical_exaggeration = 5)
  expect_equal(get_hillshade_zscale(), 12)

  sphere_shade(zscale = 8)
  expect_equal(get_hillshade_zscale(), 8)
})

test_that("vertical_exaggeration is one-off for cached hillshade and scene zscale", {
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  exaggerated_shade = sphere_shade(
    volcano,
    zscale = 12,
    vertical_exaggeration = 2
  )
  expect_equal(get_hillshade_zscale(), 12)
  expect_equal(
    exaggerated_shade,
    sphere_shade(volcano, zscale = 6)
  )

  clear_hillshade_test_cache()
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  hillshade = sphere_shade(volcano)
  expect_no_condition(plot_3d_test(
    hillshade,
    heightmap = volcano,
    zscale = 50,
    vertical_exaggeration = 2,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_equal(get_hillshade_zscale(), 50)
  expect_equal(get_scene_zscale(), 50)
  expect_equal(get_scene_vertical_exaggeration(), 2)
  expect_equal(get_scene_effective_zscale(), 25)
})

test_that("plot_3d vertical_exaggeration is independent of hillshade exaggeration", {
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  hillshade = volcano |>
    sphere_shade(zscale = 20, vertical_exaggeration = 20) |>
    add_shadow(
      ray_shade(vertical_exaggeration = 4, maxsearch = 10),
      0.5
    )
  expect_equal(get_hillshade_zscale(), 20)

  expect_no_condition(plot_3d_test(
    hillshade,
    vertical_exaggeration = 1,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_equal(get_scene_zscale(), 20)
  expect_equal(get_scene_vertical_exaggeration(), 1)
  expect_equal(get_scene_effective_zscale(), 20)
})

test_that("ray based hillshade functions reuse cached zscale", {
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  sphere_shade(volcano)

  expect_equal(
    ray_shade(
      zscale = 50,
      sunaltitude = 25,
      sunangle = 225,
      maxsearch = 10
    ),
    ray_shade(
      volcano,
      zscale = 50,
      sunaltitude = 25,
      sunangle = 225,
      maxsearch = 10
    )
  )
  expect_equal(get_hillshade_zscale(), 50)
  expect_equal(
    lamb_shade(sunaltitude = 25, sunangle = 225),
    lamb_shade(volcano, sunaltitude = 25, sunangle = 225, zscale = 50)
  )
  expect_equal(
    ambient_shade(
      sunbreaks = 3,
      maxsearch = 10,
      anglebreaks = seq(10, 20, by = 5)
    ),
    ambient_shade(
      volcano,
      sunbreaks = 3,
      maxsearch = 10,
      anglebreaks = seq(10, 20, by = 5),
      zscale = 50
    )
  )
})

test_that("cloud_shade reuses cached heightmap and supports vertical_exaggeration", {
  testthat::skip_if_not_installed("ambient")
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  heightmap = volcano[
    seq(1, nrow(volcano), by = 4),
    seq(1, ncol(volcano), by = 4)
  ]
  sphere_shade(heightmap, zscale = 12)

  expect_equal(
    cloud_shade(
      heightmap,
      layers = 2,
      seed = 2
    ),
    cloud_shade(
      heightmap,
      zscale = 12,
      layers = 2,
      seed = 2
    )
  )
  expect_equal(
    cloud_shade(
      vertical_exaggeration = 3,
      layers = 2,
      seed = 2
    ),
    cloud_shade(
      heightmap,
      zscale = 12,
      vertical_exaggeration = 3,
      layers = 2,
      seed = 2
    )
  )
  expect_equal(get_hillshade_zscale(), 12)
  expect_equal(
    cloud_shade(
      heightmap,
      zscale = 12,
      vertical_exaggeration = 3,
      layers = 2,
      seed = 2
    ),
    cloud_shade(
      heightmap,
      zscale = 4,
      layers = 2,
      seed = 2
    )
  )
})

test_that("plot_3d uses cached hillshade heightmap and zscale", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  withr::local_options(list(rayshader.verbose_scene_cache = TRUE))
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  hillshade = volcano |>
    sphere_shade(vertical_exaggeration = 50) |>
    add_overlay(height_shade(), 0.5) |>
    add_shadow(
      ray_shade(zscale = 50, maxsearch = 10, sunaltitude = 25),
      0.5
    ) |>
    add_shadow(
      ambient_shade(
        sunbreaks = 3,
        maxsearch = 10,
        anglebreaks = seq(10, 20, by = 5)
      ),
      0.2
    )

  out = character()
  expect_no_error(withCallingHandlers(
    plot_3d_test(
      hillshade,
      shadow = FALSE,
      water = FALSE,
      windowsize = c(200, 200)
    ),
    message = function(cnd) {
      out <<- c(out, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))

  expect_true(any(grepl("hillshade_heightmap", out, fixed = TRUE)))
  expect_true(any(grepl("hillshade_zscale", out, fixed = TRUE)))
  expect_equal(get_scene_heightmap(), volcano)
  expect_equal(get_scene_zscale(), 50)
  expect_equal(get_hillshade_heightmap(), volcano)
  expect_equal(get_hillshade_zscale(), 50)
})

test_that("plot_3d explicit matrix heightmap does not reuse stale cached zscale", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  hillshade = sphere_shade(volcano)
  expect_no_condition(plot_3d_test(
    hillshade,
    heightmap = volcano,
    zscale = 3,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_equal(get_hillshade_zscale(), 3)

  rgl::close3d()

  expect_no_condition(plot_3d_test(
    hillshade,
    heightmap = volcano,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_equal(get_scene_zscale(), 1)
  expect_equal(get_hillshade_zscale(), 1)
})

test_that("plot_3d cached hillshade heightmap does not reuse stale scene zscale", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  old_hillshade = sphere_shade(volcano, zscale = 200)
  expect_no_condition(plot_3d_test(
    old_hillshade,
    heightmap = volcano,
    zscale = 200,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_equal(get_scene_zscale(), 200)

  new_hillshade = sphere_shade(volcano, vertical_exaggeration = 2)
  expect_null(get_hillshade_zscale(default = NULL))
  expect_equal(get_scene_zscale(), 200)

  expect_no_condition(plot_3d_test(
    new_hillshade,
    vertical_exaggeration = 1 / 2,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_equal(get_scene_zscale(), 1)
  expect_equal(get_scene_vertical_exaggeration(), 1 / 2)
  expect_equal(get_scene_effective_zscale(), 2)
  expect_equal(get_hillshade_zscale(), 1)
})

test_that("radiance_shade reuses cached 2D hillshade state without a scene", {
  skip_if_not_installed("rayrender")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  withr::local_options(list(rayshader.verbose_scene_cache = TRUE))
  clear_hillshade_test_cache()
  withr::defer(clear_hillshade_test_cache())

  sphere_map = sphere_shade(volcano, vertical_exaggeration = 50)
  rgl::close3d()

  out = character()
  radiance_error = NULL
  expect_no_error(withCallingHandlers(
    tryCatch(
      radiance_shade(samples = 1, light = FALSE, shadow = FALSE),
      error = function(e) {
        radiance_error <<- conditionMessage(e)
        NULL
      }
    ),
    message = function(cnd) {
      out <<- c(out, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))
  expect_true(any(grepl("hillshade_heightmap", out, fixed = TRUE)))
  expect_true(any(grepl("hillshade_map", out, fixed = TRUE)))
  if (!is.null(radiance_error)) {
    expect_false(grepl(
      "No rgl window currently open and no `heightmap` supplied",
      radiance_error,
      fixed = TRUE
    ))
  }

  out = character()
  radiance_error = NULL
  expect_no_error(withCallingHandlers(
    tryCatch(
      radiance_shade(
        texture = sphere_map,
        samples = 1,
        light = FALSE,
        shadow = FALSE
      ),
      error = function(e) {
        radiance_error <<- conditionMessage(e)
        NULL
      }
    ),
    message = function(cnd) {
      out <<- c(out, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))
  expect_true(any(grepl("hillshade_heightmap", out, fixed = TRUE)))
  expect_false(any(grepl("hillshade_map", out, fixed = TRUE)))
  if (!is.null(radiance_error)) {
    expect_false(grepl(
      "No rgl window currently open and no `heightmap` supplied",
      radiance_error,
      fixed = TRUE
    ))
  }
})
