test_that("normal calculation preserves plane direction at every edge and corner", {
  for (scales in list(c(1, 1), c(0.8, 1.2))) {
    for (slope in c(0, 15, 20, 100)) {
      plane = outer(seq_len(7), seq_len(9), function(row, col) {
        slope * (3 * row - 2 * col)
      })
      normals = calculate_normal_cpp(plane, FALSE, scales[1], scales[2])
      expected = c(-2 * slope / scales[1], -3 * slope / scales[2], 1)
      expected = expected / sqrt(sum(expected^2))
      expect_equal(normals$x, matrix(expected[1], 7, 9), tolerance = 1e-12)
      expect_equal(normals$y, matrix(expected[2], 7, 9), tolerance = 1e-12)
      expect_equal(normals$z, matrix(expected[3], 7, 9), tolerance = 1e-12)
    }
  }
})

test_that("sphere texture lookups clamp to both valid zero-based endpoints", {
  heightmap = matrix(0, 4, 6)
  texture = array(seq_len(4 * 4 * 3) / (4 * 4 * 3), c(4, 4, 3))
  # Cardinal normals land exactly on the texture boundary; oversized supplied
  # normals also exercise the lower-bound clamp without relying on a warning.
  for (normal_x in c(-2, -1, 1, 2)) {
    normals = list(
      x = matrix(normal_x, 8, 6),
      y = matrix(0, 8, 6),
      z = matrix(0, 8, 6)
    )
    attr(normals, "geographic_aspect") = identity_geographic_aspect()
    expect_no_warning(
      image <- sphere_shade(
        heightmap,
        texture = texture,
        normal_vectors = normals,
        sun_angle = 0,
        geographic_aspect = FALSE
      )
    )
    row = if (normal_x > 0) 1 else 4
    for (channel in 1:3) {
      expect_equal(
        as.numeric(image[,, channel]),
        rep(texture[row, 3, channel], 24)
      )
    }
  }
})

test_that("steep spatial terrain produces finite shading with unit boundary normals", {
  skip_if_not_installed("terra")
  dem = terra::rast(
    nrows = 7,
    ncols = 9,
    xmin = -122,
    xmax = -121.7,
    ymin = 36.5,
    ymax = 36.7,
    crs = "EPSG:4326"
  )
  terra::values(dem) = as.vector(outer(seq_len(7), seq_len(9), function(x, y) {
    300 * x - 200 * y
  }))
  for (exaggeration in c(15, 20, 100)) {
    normals = calculate_normal(dem, zscale = 1 / exaggeration)
    magnitude = sqrt(normals$x^2 + normals$y^2 + normals$z^2)
    expect_equal(
      as.numeric(magnitude),
      rep(1, length(magnitude)),
      tolerance = 1e-12
    )
    expect_true(all(normals$z > 0))
    expect_no_warning(
      image <- sphere_shade(dem, vertical_exaggeration = exaggeration)
    )
    expect_true(all(is.finite(image)))
    expect_true(all(image >= 0 & image <= 1))
  }
})

test_that("native texture lookup rejects invalid indices before accessing memory", {
  texture = matrix(1:16, 4, 4)
  expect_equal(
    as.numeric(construct_matrix(texture, 1L, 2L, c(0L, 3L), c(0L, 3L))),
    c(1, 16)
  )
  for (invalid in c(-1L, 4L, NA_integer_)) {
    expect_error(construct_matrix(texture, 1L, 1L, invalid, 0L), "zero-based")
    expect_error(construct_matrix(texture, 1L, 1L, 0L, invalid), "zero-based")
  }
  expect_error(construct_matrix(texture, 2L, 2L, 0L, 0L), "dimensions")
})
