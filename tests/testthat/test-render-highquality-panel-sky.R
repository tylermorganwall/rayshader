test_that("faceted maps use the selected panel's bounds and display CRS for skies", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rayrender")
  skip_if_not_installed("skymodelr")
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  withr::defer(clear_hillshade_cache())
  points = sf::st_as_sf(
    data.frame(
      x = c(-76, -74, 19, 21),
      y = c(39, 41, 49, 51),
      group = rep(c("a", "b"), each = 2),
      z = 1:4
    ),
    coords = c("x", "y"),
    crs = 4326
  )
  polygons = sf::st_buffer(sf::st_transform(points, 3857), 100000)
  chart = ggplot2::ggplot(polygons, ggplot2::aes(fill = z)) +
    ggplot2::geom_sf() +
    ggplot2::facet_wrap(~group) +
    ggplot2::coord_sf(crs = 3857)
  plot_gg_test(chart, width = 2, height = 1.5, raytrace = FALSE, shadow = FALSE)
  info = get_cached_plot_gg_panel_info()
  center = sf::st_sfc(
    sf::st_point(c(
      mean(c(info$data_xmin[1], info$data_xmax[1])),
      mean(c(info$data_ymin[1], info$data_ymax[1]))
    )),
    crs = 3857
  )
  expected = unname(sf::st_coordinates(sf::st_transform(center, 4326))[1, ])
  # Both panels have fixed map limits, despite occupying different parts of
  # the canvas. Neither the margins nor the other facet may shift the sun.
  for (panel in 1:2) {
    location = resolve_cached_extent_center_latlong(panel = panel)
    expect_equal(c(location$long, location$lat), expected, tolerance = 1e-8)
  }
  cache_hillshade_extent(c(xmin = 0, xmax = 2, ymin = 0, ymax = 2))
  cache_hillshade_crs(sf::st_crs(4326))
  expect_error(resolve_cached_extent_center_latlong(), "faceted ggplot")
  expect_error(
    resolve_cached_extent_center_latlong(panel = 99),
    "Available panels"
  )
  expect_error(
    resolve_cached_extent_center_latlong(panel = c(1, 2)),
    "single ggplot panel"
  )
  expect_error(
    resolve_cached_extent_center_latlong(panel = NA),
    "single ggplot panel"
  )

  # Exercise the cache contract with distinct per-panel projections and map
  # bounds, so using panel one's CRS or the full-canvas extent cannot pass.
  info$data_xmin[2] = 10
  info$data_xmax[2] = 30
  info$data_ymin[2] = 40
  info$data_ymax[2] = 50
  cache_plot_gg_panel_info(info)
  transform = get_cached_plot_gg_transform_info()
  transform$panel_params[[2]]$crs = sf::st_crs(4326)
  cache_plot_gg_transform_info(transform)
  location = resolve_cached_extent_center_latlong(panel = 2)
  expect_equal(c(location$long, location$lat), c(20, 45))
  # A nonspatial chart must not borrow a previous terrain's geolocation.
  transform_cartesian = transform
  transform_cartesian$coord = ggplot2::coord_cartesian()
  cache_plot_gg_transform_info(transform_cartesian)
  expect_null(resolve_cached_extent_center_latlong(panel = 2))
  cache_plot_gg_transform_info(transform)

  skip_if_not(render_highquality_supports_sky_light())
  sky_time = as.POSIXct("2025-03-21 13:00:00", tz = "UTC")
  expect_error(
    render_highquality(datetime = sky_time, return_scene = TRUE),
    "faceted ggplot"
  )
  scene = render_highquality(
    datetime = sky_time,
    panel = 2,
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_equal(c(sky$long, sky$lat), c(20, 45))
  # Explicit geographic coordinates do not require a facet selection.
  scene = render_highquality(
    datetime = sky_time,
    lat = 10,
    long = 15,
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_equal(c(sky$long, sky$lat), c(15, 10))
  scene = render_highquality(
    datetime = sky_time,
    lat = 12,
    panel = 2,
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_equal(c(sky$long, sky$lat), c(20, 12))

  target = list(long = 20, lat = 45, altitude = 0, panel = 2)
  position = list(long = -75, lat = 40, altitude = 100, panel = 1)
  scene = render_highquality(
    datetime = sky_time,
    camera_lookat = target,
    camera_location = position,
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_equal(c(sky$long, sky$lat), c(20, 45))
  scene = render_highquality(
    datetime = sky_time,
    camera_location = position,
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_equal(c(sky$long, sky$lat), expected, tolerance = 1e-8)
  scene = render_highquality(
    datetime = sky_time,
    panel = 1,
    camera_lookat = target,
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_equal(c(sky$long, sky$lat), expected, tolerance = 1e-8)

  # The top-level panel also supplies a default for spatial camera transforms.
  target_without_panel = target
  target_without_panel$panel = NULL
  expect_equal(
    resolve_render_highquality_camera_point(
      target_without_panel,
      "camera_lookat",
      panel = 2
    ),
    resolve_render_highquality_camera_point(target, "camera_lookat")
  )
  expect_equal(
    resolve_render_highquality_camera_point(target, "camera_lookat", panel = 1),
    resolve_render_highquality_camera_point(target, "camera_lookat")
  )
})
