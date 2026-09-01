test_that("automatic scale bars use pretty geographic distances", {
  skip_if_not_installed("sf")

  wide_scene = list(
    dimensions = c(80000, 40000),
    metric = TRUE,
    crs = sf::st_crs(4326)
  )
  wide_specification = resolve_render_scalebar_specification(
    scene_info = wide_scene
  )

  expect_equal(wide_specification$position, "S")
  expect_equal(wide_specification$label_unit, "km")
  expect_equal(wide_specification$limits, c(0, 10, 20, 30, 40))
  expect_equal(wide_specification$represented_distance, 40000)
  expect_equal(diff(wide_specification$scale_length), 0.5)

  tall_scene = wide_scene
  tall_scene$dimensions = rev(wide_scene$dimensions)
  expect_equal(
    resolve_render_scalebar_specification(scene_info = tall_scene)$position,
    "W"
  )

  small_scene = wide_scene
  small_scene$dimensions = c(500, 300)
  small_specification = resolve_render_scalebar_specification(
    scene_info = small_scene
  )
  expect_equal(small_specification$label_unit, "m")
  expect_equal(max(small_specification$limits), 250)
})

test_that("automatic scale bars use projected and raw scene units", {
  skip_if_not_installed("sf")

  metric_scene = list(
    dimensions = c(800, 400),
    metric = TRUE,
    crs = sf::st_crs(3857)
  )
  metric_specification = resolve_render_scalebar_specification(
    scene_info = metric_scene
  )
  expect_equal(metric_specification$label_unit, "m")
  expect_equal(metric_specification$represented_distance, 400)

  hidden_unit = resolve_render_scalebar_specification(
    limits = c(0, 200),
    label_unit = "",
    scene_info = metric_scene
  )
  expect_equal(hidden_unit$label_unit, "")
  expect_equal(hidden_unit$represented_distance, 200)

  feet_scene = metric_scene
  feet_scene$crs = sf::st_crs(2230)
  feet_specification = resolve_render_scalebar_specification(
    scene_info = feet_scene
  )
  expect_equal(feet_specification$label_unit, "ft")
  expect_equal(
    feet_specification$represented_distance,
    max(feet_specification$limits) * 1200 / 3937
  )

  raw_scene = list(
    dimensions = c(80, 40),
    metric = FALSE,
    crs = NULL
  )
  raw_specification = resolve_render_scalebar_specification(
    scene_info = raw_scene
  )
  expect_equal(raw_specification$label_unit, "")
  expect_equal(raw_specification$limits, c(0, 10, 20, 30, 40))
  expect_equal(diff(raw_specification$scale_length), 0.5)
})

test_that("explicit scale-bar arguments override automatic values", {
  skip_if_not_installed("sf")

  scene_info = list(
    dimensions = c(100000, 50000),
    metric = TRUE,
    crs = sf::st_crs(4326)
  )
  explicit_specification = resolve_render_scalebar_specification(
    limits = c(0, 20, 40),
    position = "n",
    scale_length = c(0.2, 0.8),
    label_unit = "km",
    scene_info = scene_info
  )
  expect_equal(explicit_specification$limits, c(0, 20, 40))
  expect_equal(explicit_specification$position, "N")
  expect_equal(explicit_specification$scale_length, c(0.2, 0.8))
  expect_equal(explicit_specification$label_unit, "km")

  derived_length = resolve_render_scalebar_specification(
    limits = c(0, 20),
    position = "S",
    label_unit = "km",
    scene_info = scene_info
  )
  expect_equal(derived_length$scale_length, c(0.4, 0.6))

  expect_error(
    resolve_render_scalebar_specification(
      limits = 0,
      scene_info = scene_info
    ),
    "at least one positive value",
    fixed = TRUE
  )
  expect_error(
    resolve_render_scalebar_specification(
      position = "invalid",
      scene_info = scene_info
    ),
    "must be one of",
    fixed = TRUE
  )
})

test_that("render_scalebar uses cached scene measurements", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  height_raster = terra::rast(
    nrows = 10,
    ncols = 20,
    xmin = -75,
    xmax = -74,
    ymin = 40,
    ymax = 40.5,
    crs = "EPSG:4326"
  )
  terra::values(height_raster) = 0
  plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    windowsize = c(100, 100)
  )

  specification = render_scalebar()

  expect_equal(specification$position, "S")
  expect_equal(specification$label_unit, "km")
  expect_equal(max(specification$limits), 40)
  expect_equal(
    specification$represented_distance / max(specification$scene_dimensions),
    0.5,
    tolerance = 0.01
  )
  expect_true(all(
    c("scalebar_col1", "scalebar_col2", "text_scalebar") %in%
      rgl::ids3d(tags = TRUE)$tag
  ))
})
