test_that("missing heights do not overwrite horizontal coordinates", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(10, nrow = 20, ncol = 20)
  heightmap[10:11, 10:11] = NA_real_
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    sphere_shade(heightmap),
    heightmap,
    zscale = 1,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250),
    extent = extent
  ))

  expected = transform_into_heightmap_coords(
    extent = extent,
    heightmap = heightmap,
    lat = c(5, 10),
    long = c(5, 10),
    altitude = 0,
    zscale = 1,
    transform_scene = FALSE
  )
  expect_warning(
    actual <- transform_into_heightmap_coords(
      extent = extent,
      heightmap = heightmap,
      lat = c(5, 10),
      long = c(5, 10),
      zscale = 1,
      transform_scene = FALSE
    ),
    "Some coords outside of heightmap extent"
  )

  expect_equal(actual[, c(1, 3)], expected[, c(1, 3)])
  expect_equal(actual[, 2], c(10, 10))
})
