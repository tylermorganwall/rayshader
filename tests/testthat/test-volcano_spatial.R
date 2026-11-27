test_that("volcano_spatial has the expected geometry and values", {
  volcano_dem = volcano_spatial()

  expect_s4_class(volcano_dem, "SpatRaster")
  expect_equal(
    c(nrow(volcano_dem), ncol(volcano_dem), terra::nlyr(volcano_dem)),
    c(87, 61, 1)
  )

  expect_equal(
    unname(terra::res(volcano_dem)),
    c(10, 10)
  )

  expect_equal(
    unname(as.vector(terra::ext(volcano_dem))),
    c(2667400, 2668010, 6478700, 6479570)
  )

  expect_true(
    terra::same.crs(volcano_dem, "EPSG:27200")
  )

  expect_equal(names(volcano_dem), "elevation")
  expect_equal(terra::units(volcano_dem), "m")

  expect_equal(
    terra::xFromCol(volcano_dem, c(1, 61)),
    c(2667405, 2668005)
  )

  expect_equal(
    terra::yFromRow(volcano_dem, c(1, 87)),
    c(6479565, 6478705)
  )

  expected = datasets::volcano[
    rev(seq_len(nrow(datasets::volcano))),
    rev(seq_len(ncol(datasets::volcano))),
    drop = FALSE
  ]

  expect_equal(
    unname(as.matrix(volcano_dem, wide = TRUE)),
    unname(expected)
  )
})

test_that("volcano_spatial returns independent copies", {
  first = volcano_spatial()
  second = volcano_spatial()

  original_extent = unname(as.vector(terra::ext(second)))

  terra::set.ext(first, c(0, 1, 0, 1))

  expect_equal(
    unname(as.vector(terra::ext(second))),
    original_extent
  )

  expect_equal(
    unname(as.vector(terra::ext(volcano_spatial()))),
    original_extent
  )
})
