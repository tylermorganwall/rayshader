test_that("indexed metric road neighbors preserve exact distance relationships", {
  set.seed(194)
  xy = rbind(
    matrix(runif(400, -2, 2), ncol = 2),
    c(0, 0),
    c(.01, 0),
    c(.01 + 1e-10, 0),
    c(0, 0),
    c(.006, .008)
  )
  for (offset in c(0, 4300000)) {
    points = sf::st_as_sf(
      data.frame(x = xy[, 1] + offset, y = xy[, 2] + offset),
      coords = c("x", "y"),
      crs = 26918
    )
    original = points
    for (tolerance in c(0, .01, .25, 10)) {
      expected = unclass(sf::st_is_within_distance(
        points,
        points,
        dist = tolerance
      ))
      attributes(expected) = NULL
      actual = find_render_road_point_neighbors(points, tolerance)
      expect_identical(actual, expected)
    }
    expect_identical(points, original)
  }
})
