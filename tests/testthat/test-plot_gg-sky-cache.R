test_that("SpatRaster ggplots cache their rendered CRS for datetime skies", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")
  skip_if_not_installed("magick")
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  withr::defer(clear_hillshade_cache())
  terrain = terra::rast(
    nrows = 12,
    ncols = 16,
    xmin = 320000,
    xmax = 324000,
    ymin = 4304000,
    ymax = 4307000,
    crs = "EPSG:26918",
    vals = seq_len(192) / 10
  )
  names(terrain) = "height"
  rgb = c(terrain + 80, terrain + 100, terrain + 60)
  names(rgb) = c("red", "green", "blue")
  height_layer = tidyterra::geom_spatraster(
    data = terrain,
    ggplot2::aes(fill = height),
    maxcell = Inf
  )
  color_layer = tidyterra::geom_spatraster_rgb(data = rgb, maxcell = Inf)
  # A different display projection must replace the raster's source CRS.
  for (target_crs in c(26918, 26917)) {
    coord = ggplot2::coord_sf(crs = target_crs)
    color_plot = ggplot2::ggplot() + height_layer + color_layer + coord
    height_plot = ggplot2::ggplot() + color_layer + height_layer + coord
    result = plot_gg_test(
      color_plot,
      gg_obj_height = height_plot,
      width = 2,
      height = 2,
      reduce_size = 0.2,
      raytrace = FALSE,
      shadow = FALSE,
      vertical_exaggeration = 1,
      save_height_matrix = TRUE
    )
    # No previous sphere_shade()/height_shade() cache may supply the location.
    clear_hillshade_cache()
    expect_equal(get_scene_crs(), sf::st_crs(target_crs))
    expect_equal(attr(result, "crs"), sf::st_crs(target_crs))
    extent = get_scene_data_filter_extent(panel = 1)
    center = sf::st_sfc(
      sf::st_point(c(
        mean(extent[c("xmin", "xmax")]),
        mean(extent[c("ymin", "ymax")])
      )),
      crs = target_crs
    )
    expected = sf::st_coordinates(sf::st_transform(center, 4326))[1, ]
    location = resolve_cached_extent_center_latlong()
    expect_equal(location$long, unname(expected["X"]), tolerance = 1e-8)
    expect_equal(location$lat, unname(expected["Y"]), tolerance = 1e-8)
    expect_identical(location$source, "ggplot_panel")
    # Exercise the actual datetime path, not a mocked location resolver.
    if (render_highquality_supports_sky_light()) {
      sky_time = as.POSIXct("2025-03-21 13:00:00", tz = "EST")
      scene = render_highquality(datetime = sky_time, return_scene = TRUE)
      sky = rayrender::get_infinite_light(scene, "sky")
      expect_equal(sky$lat, unname(expected["Y"]), tolerance = 1e-8)
      expect_equal(sky$long, unname(expected["X"]), tolerance = 1e-8)
      expect_identical(sky$datetime, sky_time)
    }
  }
  # A later Cartesian chart must not inherit the map's coordinate system.
  chart = ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3, z = 1:3),
    ggplot2::aes(x, y, fill = z)
  ) +
    ggplot2::geom_tile()
  plot_gg_test(chart, width = 1, height = 1, raytrace = FALSE, shadow = FALSE)
  expect_null(get_scene_crs())
  expect_null(resolve_cached_extent_center_latlong())
})
