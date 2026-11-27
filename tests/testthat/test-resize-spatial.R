make_resize_test_raster = function() {
  raster = terra::rast(
    nrows = 4,
    ncols = 8,
    xmin = 10,
    xmax = 18,
    ymin = 20,
    ymax = 24,
    crs = "EPSG:3857"
  )
  terra::values(raster) = seq_len(terra::ncell(raster))
  names(raster) = "elevation"
  raster
}

make_resize_test_categories = function() {
  values = c(
    1,
    1,
    2,
    2,
    1,
    2,
    2,
    2,
    1,
    1,
    1,
    2,
    2,
    2,
    1,
    2
  )
  terra::as.factor(terra::rast(nrows = 4, ncols = 4, vals = values))
}

test_that("scale is the relative-size argument", {
  skip_if_not_installed("terra")
  raster = make_resize_test_raster()

  up = resize_spatial(raster, scale = 2)
  down = resize_spatial(raster, scale = 0.5)

  expect_equal(c(terra::nrow(up), terra::ncol(up)), c(8, 16))
  expect_equal(c(terra::nrow(down), terra::ncol(down)), c(2, 4))
})

test_that("explicit dimensions and single-axis dimensions are absolute", {
  skip_if_not_installed("terra")
  raster = make_resize_test_raster()

  explicit = resize_spatial(raster, width = 6, height = 3)
  width_only = resize_spatial(raster, width = 4)
  height_only = resize_spatial(raster, height = 2)
  one_cell = resize_spatial(raster, width = 1, height = 1)

  expect_equal(c(terra::nrow(explicit), terra::ncol(explicit)), c(3, 6))
  expect_equal(c(terra::nrow(width_only), terra::ncol(width_only)), c(2, 4))
  expect_equal(c(terra::nrow(height_only), terra::ncol(height_only)), c(2, 4))
  expect_equal(c(terra::nrow(one_cell), terra::ncol(one_cell)), c(1, 1))
})

test_that("explicit dimensions must be positive whole numbers", {
  skip_if_not_installed("terra")
  raster = make_resize_test_raster()

  expect_error(
    resize_spatial(raster, width = 3.5),
    "`width` must be a positive whole number"
  )
  expect_error(
    resize_spatial(raster, height = 2.5),
    "`height` must be a positive whole number"
  )
  expect_error(resize_spatial(raster, width = 0), "positive whole number")
})

test_that("mixed-direction resizing is rejected", {
  skip_if_not_installed("terra")
  raster = make_resize_test_raster()

  expect_error(
    resize_spatial(raster, width = 4, height = 8),
    "one axis would be downsampled while the other is upsampled",
    fixed = TRUE
  )

  expect_no_error(resize_spatial(raster, width = 4, height = 4))
})

test_that("no-op resizing returns the input geometry unchanged", {
  skip_if_not_installed("terra")
  raster = make_resize_test_raster()

  result = resize_spatial(raster)

  expect_identical(result, raster)
  expect_equal(as.vector(terra::ext(result)), as.vector(terra::ext(raster)))
  expect_true(terra::same.crs(result, raster))
  expect_identical(names(result), names(raster))
})

test_that("resizing preserves extent CRS and layer names", {
  skip_if_not_installed("terra")
  first = make_resize_test_raster()
  second = first * 2
  names(second) = "slope"
  raster = c(first, second)

  result = resize_spatial(raster, scale = 2)

  expect_equal(as.vector(terra::ext(result)), as.vector(terra::ext(raster)))
  expect_true(terra::same.crs(result, raster))
  expect_identical(names(result), c("elevation", "slope"))
})

test_that("categorical defaults use modal downsampling and nearest upsampling", {
  skip_if_not_installed("terra")
  raster = make_resize_test_categories()

  down = resize_spatial(raster, scale = 0.5)
  down_expected = terra::aggregate(raster, fact = 2, fun = "modal")
  up = resize_spatial(raster, scale = 2)
  up_template = terra::rast(
    nrows = 8,
    ncols = 8,
    xmin = terra::xmin(raster),
    xmax = terra::xmax(raster),
    ymin = terra::ymin(raster),
    ymax = terra::ymax(raster)
  )
  up_expected = terra::resample(raster, up_template, method = "near")

  expect_equal(terra::values(down), terra::values(down_expected))
  expect_equal(terra::values(up), terra::values(up_expected))
})

test_that("explicit categorical methods override automatic defaults", {
  skip_if_not_installed("terra")
  raster = make_resize_test_categories()

  down = resize_spatial(raster, scale = 0.5, method_down = "max")
  up = resize_spatial(raster, scale = 2, method_up = "bilinear")

  expect_equal(terra::values(down, mat = FALSE), rep(2, 4))
  expect_true(any(terra::values(up, mat = FALSE) %% 1 != 0))
})

test_that("mode remains an alias for modal", {
  skip_if_not_installed("terra")
  raster = make_resize_test_categories()

  alias_result = resize_spatial(raster, scale = 0.5, method_down = "mode")
  modal_result = resize_spatial(raster, scale = 0.5, method_down = "modal")

  expect_equal(terra::values(alias_result), terra::values(modal_result))
})

test_that("dots and write_args are routed to separate operations", {
  skip_if_not_installed("terra")
  raster = make_resize_test_raster()
  aggregate_dots = NULL
  resample_dots = NULL
  write_dots = NULL

  testthat::local_mocked_bindings(
    aggregate = function(x, fact, fun, ...) {
      aggregate_dots <<- list(...)
      result = terra::rast(
        nrows = 2,
        ncols = 4,
        ext = terra::ext(x),
        crs = terra::crs(x)
      )
      terra::values(result) = 1
      result
    },
    resample = function(x, y, method, threads, ...) {
      resample_dots <<- list(...)
      terra::values(y) = 1
      y
    },
    writeRaster = function(x, filename, overwrite, ...) {
      write_dots <<- list(...)
      x
    },
    .package = "terra"
  )

  resize_spatial(
    raster,
    scale = 0.5,
    operation_marker = "aggregate",
    filename = "aggregate.tif",
    write_args = list(write_marker = "write")
  )
  expect_identical(aggregate_dots$operation_marker, "aggregate")
  expect_null(write_dots$operation_marker)
  expect_identical(write_dots$write_marker, "write")

  resize_spatial(
    raster,
    width = 7,
    height = 3,
    operation_marker = "resample",
    filename = "resample.tif",
    write_args = list(write_marker = "write")
  )
  expect_identical(resample_dots$operation_marker, "resample")
  expect_null(write_dots$operation_marker)
  expect_identical(write_dots$write_marker, "write")
})

test_that("write_args must be a named list", {
  skip_if_not_installed("terra")
  raster = make_resize_test_raster()

  expect_error(resize_spatial(raster, write_args = "no"), "must be a list")
  expect_error(
    resize_spatial(raster, write_args = list("unnamed")),
    "fully named list"
  )
})

test_that("filename output applies write_args for no-op and resized output", {
  skip_if_not_installed("terra")
  raster = make_resize_test_raster()
  no_op_file = tempfile(fileext = ".tif")
  resized_file = tempfile(fileext = ".tif")

  resize_spatial(
    raster,
    filename = no_op_file,
    write_args = list(datatype = "INT2S")
  )
  resize_spatial(
    raster,
    scale = 0.5,
    filename = resized_file,
    write_args = list(datatype = "INT2S")
  )

  expect_true(file.exists(no_op_file))
  expect_true(file.exists(resized_file))
  expect_identical(terra::datatype(terra::rast(no_op_file)), "INT2S")
  expect_identical(terra::datatype(terra::rast(resized_file)), "INT2S")
})

test_that("resize output contains NA rather than NaN", {
  skip_if_not_installed("terra")
  raster = terra::rast(nrows = 4, ncols = 4, vals = seq_len(16))
  raster[2, 2] = NA
  logical_raster = raster > 0

  up = resize_spatial(raster, scale = 2)
  down = resize_spatial(
    raster,
    scale = 0.5,
    method_down = "mean",
    na.rm = FALSE
  )
  already_normalized = resize_spatial(
    logical_raster,
    scale = 0.5,
    method_down = "all",
    na.rm = FALSE
  )

  for (result in list(up, down, already_normalized)) {
    values = terra::values(result, mat = FALSE)
    expect_true(any(is.na(values)))
    expect_false(any(is.nan(values)))
  }
})

test_that("file-backed resize output contains NA rather than NaN", {
  skip_if_not_installed("terra")
  raster = terra::rast(nrows = 4, ncols = 4, vals = seq_len(16))
  raster[2, 2] = NA
  filename = tempfile(fileext = ".tif")

  result = resize_spatial(
    raster,
    scale = 2,
    filename = filename,
    overwrite = TRUE
  )
  values = terra::values(result, mat = FALSE)

  expect_true(file.exists(filename))
  expect_true(any(is.na(values)))
  expect_false(any(is.nan(values)))
})
