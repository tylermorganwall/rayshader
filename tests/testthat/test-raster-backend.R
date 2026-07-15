test_that("spatial raster reads prefer terra", {
  skip_if_not_installed("terra")

  raster_file = tempfile(fileext = ".tif")
  withr::defer(unlink(raster_file))
  source = terra::rast(matrix(seq_len(12), nrow = 3, ncol = 4))
  terra::writeRaster(source, raster_file)

  result = read_spatial_raster(raster_file, caller = "test")
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::values(result), terra::values(source))
  expect_equal(
    raster_to_matrix(raster_file, verbose = FALSE),
    raster_to_matrix(source, verbose = FALSE)
  )
})

test_that("spatial raster reads warn when falling back to raster", {
  skip_if_not_installed("raster")
  skip_if_not_installed("terra")

  raster_file = tempfile(fileext = ".tif")
  withr::defer(unlink(raster_file))
  terra::writeRaster(terra::rast(matrix(1:4, nrow = 2)), raster_file)
  testthat::local_mocked_bindings(
    has_spatial_raster_package = function(package) {
      identical(package, "raster")
    }
  )

  result = NULL
  expect_warning(
    result <- read_spatial_raster(raster_file, caller = "test"),
    "will soon be deprecated"
  )
  expect_s4_class(result, "RasterLayer")

  result_matrix = NULL
  expect_warning(
    result_matrix <- raster_to_matrix(raster_file, verbose = FALSE),
    "will soon be deprecated"
  )
  expect_equal(dim(result_matrix), c(2, 2))
})

test_that("spatial raster reads fail when no backend is available", {
  testthat::local_mocked_bindings(
    has_spatial_raster_package = function(package) FALSE
  )

  expect_error(
    read_spatial_raster("missing.tif", caller = "test"),
    "`terra` package is required"
  )
  expect_error(
    resize_matrix(volcano, width = 20, height = 10),
    "`terra` package is required"
  )
})

test_that("resize_matrix uses terra with a warned raster fallback", {
  skip_if_not_installed("terra")

  resized = resize_matrix(volcano, width = 20, height = 10)
  expect_equal(dim(resized), c(10, 20))
  expect_true(all(is.finite(resized)))

  skip_if_not_installed("raster")
  testthat::local_mocked_bindings(
    has_spatial_raster_package = function(package) {
      identical(package, "raster")
    }
  )
  fallback = NULL
  expect_warning(
    fallback <- resize_matrix(volcano, width = 20, height = 10),
    "will soon be deprecated"
  )
  expect_equal(dim(fallback), c(10, 20))
})
