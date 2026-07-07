test_that("resize_spatial preserves NA values without returning NaN", {
  r = terra::rast(nrows = 4, ncols = 4, vals = seq_len(16))
  r[2, 2] = NA

  up = resize_spatial(r, scale = 2)
  up_values = terra::values(up, mat = FALSE)

  expect_true(any(is.na(up_values)))
  expect_false(any(is.nan(up_values)))

  down = resize_spatial(r, scale = 0.5, method_down = "mean", na.rm = FALSE)
  down_values = terra::values(down, mat = FALSE)

  expect_true(any(is.na(down_values)))
  expect_false(any(is.nan(down_values)))
})

test_that("resize_spatial filename output returns normalized NA values", {
  r = terra::rast(nrows = 4, ncols = 4, vals = seq_len(16))
  r[2, 2] = NA
  filename = tempfile(fileext = ".tif")

  out = resize_spatial(r, scale = 2, filename = filename, overwrite = TRUE)
  out_values = terra::values(out, mat = FALSE)

  expect_true(file.exists(filename))
  expect_true(any(is.na(out_values)))
  expect_false(any(is.nan(out_values)))
})
