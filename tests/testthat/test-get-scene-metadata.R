test_that("get_scene_metadata returns spatial and scene-coordinate bounds", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  heightmap = matrix(seq_len(48), nrow = 8, ncol = 6)
  extent = c(xmin = 100, xmax = 170, ymin = 200, ymax = 250)

  plot_3d_test(
    height_shade(heightmap),
    heightmap = heightmap,
    zscale = 10,
    vertical_exaggeration = 2,
    extent = extent,
    crs = 3857,
    geographic_aspect = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  )

  metadata = get_scene_metadata()

  expect_identical(metadata$plot_type, "plot_3d")
  expect_equal(metadata$extent, extent)
  expect_equal(
    metadata$extent_3d,
    c(extent, zmin = min(heightmap), zmax = max(heightmap))
  )
  expect_true(scene_crs_equal(metadata$crs, sf::st_crs(3857)))
  expect_equal(metadata$zscale, 10)
  expect_equal(metadata$vertical_exaggeration, 2)
  expect_equal(metadata$effective_zscale, 5)
  expect_equal(metadata$heightmap_dimensions, c(rows = 8, columns = 6))
  expect_equal(metadata$elevation_range, c(min = 1, max = 48))
  expect_equal(metadata$scene_elevation_range, c(min = 0.2, max = 9.6))
  expect_equal(metadata$scene_bounds["x", ], c(min = -3.5, max = 3.5))
  expect_equal(metadata$scene_bounds["y", ], c(min = 0.2, max = 9.6))
  expect_equal(metadata$scene_bounds["z", ], c(min = -2.5, max = 2.5))
  expect_equal(metadata$scene_dimensions, c(x = 7, y = 9.4, z = 5))
  expect_equal(metadata$map_units_per_scene_unit, c(x = 10, y = 10))
  expect_equal(
    metadata$scene_axis_mapping,
    c(x = "x", y = "-z", elevation = "y")
  )
  expect_false(metadata$triangulate)
  expect_null(metadata$panel_extents)
  expect_null(metadata$panel_info)
  expect_null(metadata$heightmap)

  metadata_with_heightmap = get_scene_metadata(include_heightmap = TRUE)
  expect_equal(metadata_with_heightmap$heightmap, heightmap)
})

test_that("get_scene_metadata reports plot_gg panel metadata", {
  skip_if_not_installed("ggplot2")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  suppressWarnings(plot_gg_test(
    ggplot2::ggplot(mtcars) +
      ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg)),
    width = 3,
    height = 2,
    windowsize = c(300, 200),
    raytrace = FALSE,
    shadow = FALSE
  ))

  metadata = get_scene_metadata()

  expect_identical(metadata$plot_type, "plot_gg")
  expect_s3_class(metadata$panel_info, "data.frame")
  expect_length(metadata$panel_extents, 1L)
  expect_equal(metadata$extent, metadata$panel_extents[[1L]])
})

test_that("get_scene_metadata includes geographic scene scaling", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  heightmap = matrix(seq_len(80), nrow = 10, ncol = 8)
  extent = c(xmin = -75, xmax = -74.9, ymin = 60, ymax = 60.1)

  plot_3d_test(
    height_shade(heightmap),
    heightmap = heightmap,
    zscale = 1000,
    extent = extent,
    crs = 4326,
    geographic_aspect = TRUE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  )

  metadata = get_scene_metadata()
  aspect = metadata$geographic_aspect

  expect_true(aspect$active)
  expect_true(all(is.finite(aspect$scale)))
  expect_equal(
    metadata$scene_dimensions[["x"]],
    (nrow(heightmap) - 1) * aspect$scale[["x"]]
  )
  expect_equal(
    metadata$scene_dimensions[["z"]],
    (ncol(heightmap) - 1) * aspect$scale[["z"]]
  )
  expect_equal(metadata$meters_per_scene_unit, aspect$mean_cell_meters)
})

test_that("get_scene_metadata requires a complete active scene cache", {
  local_rgl_use_null()
  reset_scene_context(
    clear_scene_metadata = TRUE,
    clear_scene_cache = TRUE
  )

  expect_error(
    get_scene_metadata(),
    "No cached scene metadata found",
    fixed = TRUE
  )
  expect_error(
    get_scene_metadata(include_heightmap = NA),
    "`include_heightmap` must be a single TRUE/FALSE value.",
    fixed = TRUE
  )
})
