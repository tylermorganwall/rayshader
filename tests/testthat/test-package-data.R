test_that("montereybay is matrix package data", {
  expect_true(exists(
    "montereybay",
    envir = asNamespace("rayshader"),
    inherits = FALSE
  ))
  expect_true(is.matrix(rayshader::montereybay))
  expect_equal(dim(rayshader::montereybay), c(540, 540))
  expect_equal(
    range(rayshader::montereybay),
    c(-2805.60680603027, 1527.43639923096),
    tolerance = 1e-10
  )
  expect_true(isTRUE(attr(rayshader::montereybay, "rayshader_data")))
})

test_that("montereybay_spatial is built from the matrix on load", {
  skip_if_not_installed("terra")

  expect_true(exists(
    "montereybay_spatial",
    envir = asNamespace("rayshader"),
    inherits = FALSE
  ))
  expect_s4_class(rayshader::montereybay_spatial, "SpatRaster")
  expect_equal(
    raster_to_matrix(rayshader::montereybay_spatial, verbose = FALSE),
    rayshader::montereybay,
    ignore_attr = TRUE
  )
})

test_that("montereybay_spatial retains the current spatial metadata", {
  skip_if_not_installed("terra")

  expect_equal(
    unname(as.vector(terra::ext(rayshader::montereybay_spatial))),
    c(
      -122.366805557045,
      -121.366805585045,
      36.179398134725,
      37.179398106725
    ),
    tolerance = 1e-12
  )
  expect_true(terra::is.lonlat(rayshader::montereybay_spatial))
  expect_identical(names(rayshader::montereybay_spatial), "Band1")
  expect_false(exists(
    ".montereybay_packed",
    envir = asNamespace("rayshader"),
    inherits = FALSE
  ))
})
