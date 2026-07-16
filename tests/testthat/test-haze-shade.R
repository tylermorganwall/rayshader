test_that("haze_shade returns bounded RGBA output", {
  heightmap = matrix(seq(-100, 500, length.out = 48), nrow = 6, ncol = 8)

  haze = haze_shade(heightmap)
  alpha = haze[,, 4]

  expect_equal(dim(haze), c(8, 6, 4))
  expect_true(all(is.finite(alpha)))
  expect_true(all(alpha >= 0 & alpha <= 1))
})

test_that("zero optical depth stays transparent for extreme elevations", {
  heightmap = matrix(c(-1e308, -1, 0, 1e308), nrow = 2)

  haze = haze_shade(
    heightmap,
    optical_depth = 0,
    scale_height = 1e-300,
    reference_height = 1e308
  )

  expect_equal(as.numeric(haze[,, 4]), rep(0, 4))
  expect_false(any(is.nan(haze[,, 4])))
})

test_that("extreme ranges and small scale heights remain finite", {
  extreme = matrix(c(-1e308, -1e100, 0, 1e100, 1e308, 1), nrow = 2)
  small_scale = matrix(c(-10, -1, 0, 1, 10, 100), nrow = 2)

  extreme_haze = haze_shade(
    extreme,
    optical_depth = 0.5,
    scale_height = 1,
    reference_height = 0
  )
  small_scale_haze = haze_shade(
    small_scale,
    optical_depth = 0.5,
    scale_height = 1e-300,
    reference_height = 0
  )

  for (haze in list(extreme_haze, small_scale_haze)) {
    expect_true(all(is.finite(haze[,, 4])))
    expect_true(all(haze[,, 4] >= 0 & haze[,, 4] <= 1))
  }
})

test_that("flat and negative heightmaps use valid automatic scale heights", {
  flat = haze_shade(matrix(-20, 5, 7))
  negative = haze_shade(matrix(seq(-500, -100, length.out = 35), 5, 7))

  expect_true(all(is.finite(flat[,, 4])))
  expect_equal(length(unique(as.vector(flat[,, 4]))), 1)
  expect_true(all(is.finite(negative[,, 4])))
})

test_that("non-finite height cells remain transparent after blur", {
  heightmap = matrix(seq(0, 1, length.out = 625), 25, 25)
  heightmap[3, 4] = NA_real_
  heightmap[7, 8] = Inf
  heightmap[11, 12] = -Inf

  unblurred = haze_shade(heightmap, blur = 0)
  blurred = haze_shade(heightmap, blur = 1)
  transparent = !is.finite(t(heightmap))

  expect_true(all(unblurred[,, 4][transparent] == 0))
  expect_true(all(blurred[,, 4][transparent] == 0))
})

test_that("absolute and relative scale heights agree", {
  heightmap = matrix(seq(0, 100, length.out = 100), 10, 10)

  absolute = haze_shade(
    heightmap,
    optical_depth = 0.7,
    scale_height = 25,
    reference_height = 0
  )
  relative = haze_shade(
    heightmap,
    optical_depth = 0.7,
    scale_height = 0.25,
    scale_height_relative = TRUE,
    reference_height = 0
  )

  expect_equal(relative, absolute, tolerance = 1e-10)
})

test_that("relative scale-height validation uses the documented interval", {
  heightmap = matrix(1:16, 4, 4)

  expect_error(
    haze_shade(heightmap, scale_height = 0, scale_height_relative = TRUE),
    "in (0, 1]",
    fixed = TRUE
  )
  expect_error(
    haze_shade(heightmap, scale_height = 1.1, scale_height_relative = TRUE),
    "in (0, 1]",
    fixed = TRUE
  )
})

test_that("blur zero preserves the stable unblurred calculation", {
  heightmap = matrix(seq(-2, 4, length.out = 30), 5, 6)
  optical_depth = 0.3
  scale_height = 2
  reference_height = -1

  haze = haze_shade(
    heightmap,
    optical_depth = optical_depth,
    scale_height = scale_height,
    reference_height = reference_height,
    blur = 0
  )
  tau = optical_depth * exp(-(t(heightmap) - reference_height) / scale_height)
  expected = -expm1(-tau)

  expect_equal(as.numeric(haze[,, 4]), as.numeric(expected), tolerance = 1e-12)
})

test_that("positive pixel-space blur is bounded and broadens a peak", {
  heightmap = matrix(100, 51, 51)
  heightmap[26, 26] = 0

  small = haze_shade(
    heightmap,
    optical_depth = 10,
    scale_height = 1,
    reference_height = 0,
    blur = 1
  )
  large = haze_shade(
    heightmap,
    optical_depth = 10,
    scale_height = 1,
    reference_height = 0,
    blur = 4
  )
  small_alpha = small[,, 4]
  large_alpha = large[,, 4]
  coordinates = expand.grid(row = seq_len(51), column = seq_len(51))
  distance_squared = (coordinates$row - 26)^2 + (coordinates$column - 26)^2
  small_spread = sum(as.vector(small_alpha) * distance_squared) /
    sum(small_alpha)
  large_spread = sum(as.vector(large_alpha) * distance_squared) /
    sum(large_alpha)

  expect_equal(dim(small), dim(large))
  expect_true(all(is.finite(small_alpha)))
  expect_true(all(is.finite(large_alpha)))
  expect_true(all(small_alpha >= 0 & small_alpha <= 1))
  expect_true(all(large_alpha >= 0 & large_alpha <= 1))
  expect_lt(max(large_alpha), max(small_alpha))
  expect_gt(large_spread, small_spread)
})

test_that("unreasonable blur kernels fail before convolution", {
  expect_error(
    haze_shade(matrix(1, 20, 20), blur = 100),
    "Gaussian kernel cannot exceed 501 by 501 pixels",
    fixed = TRUE
  )
  expect_error(
    haze_shade(matrix(1, 5, 5), blur = 2),
    "too large for the output dimensions",
    fixed = TRUE
  )
})

test_that("explicit and cached heightmap workflows are equivalent", {
  heightmap = matrix(seq(0, 100, length.out = 100), 10, 10)

  explicit = haze_shade(heightmap, optical_depth = 0.7, scale_height = 20)
  sphere_shade(heightmap)
  cached = haze_shade(optical_depth = 0.7, scale_height = 20)

  expect_equal(cached, explicit)
  expect_equal(rayshader:::get_hillshade_map(), cached)
})

test_that("haze_shade caches the returned haze map", {
  heightmap = matrix(seq(0, 10, length.out = 25), 5, 5)

  haze = haze_shade(heightmap)

  expect_equal(rayshader:::get_hillshade_map(), haze)
})
