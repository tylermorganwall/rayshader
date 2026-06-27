clear_raster_overlay_test_cache = function() {
  clear_hillshade_cache()
  reset_scene_context(
    clear_scene_metadata = TRUE,
    clear_scene_cache = TRUE
  )
  invisible(NULL)
}

test_that("generate_raster_overlay draws finite raster cells with a single color", {
  skip_if_not_installed("terra")
  clear_raster_overlay_test_cache()
  withr::defer(clear_raster_overlay_test_cache())

  height_raster = terra::rast(
    nrows = 4,
    ncols = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = seq_len(terra::ncell(height_raster))
  base_map = height_shade(height_raster)

  data_raster = height_raster
  terra::values(data_raster) = 1

  overlay = generate_raster_overlay(
    data_raster,
    palette = "red",
    alpha = 0.25
  )

  expect_equal(dim(overlay), dim(base_map))
  expect_equal(dim(overlay)[1:2], c(4, 5))
  expect_true(all(overlay[,, 4] == 0.25))
})

test_that("generate_raster_overlay maps raster values through a palette", {
  skip_if_not_installed("terra")
  clear_raster_overlay_test_cache()
  withr::defer(clear_raster_overlay_test_cache())

  data_raster = terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2
  )
  terra::values(data_raster) = c(0, 1, 0, 1)

  overlay = generate_raster_overlay(
    data_raster,
    extent = data_raster,
    width = 2,
    height = 2,
    palette = c("black", "white"),
    alpha = 1,
    resample_method = "near"
  )

  expect_equal(dim(overlay), c(2, 2, 4))
  expect_equal(sort(unique(as.vector(overlay[,, 1]))), c(0, 1))
  expect_true(all(overlay[,, 4] == 1))
})
