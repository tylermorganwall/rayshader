test_that("ordinary miter contact footprints remain unchanged", {
  points = rbind(c(0, 1, 0), c(5, 2, 0), c(10, 3, 5))
  frames = calculate_render_road_vertex_frames(points)
  station = calculate_road_path_cumulative_distance(points)
  actual = build_render_road_contact_footprint(points, 2, station, frames)
  offset = frames$side * frames$miter_scale
  left = right = points
  left[, c(1, 3)] = left[, c(1, 3)] + offset
  right[, c(1, 3)] = right[, c(1, 3)] - offset
  first = 1:2
  expect_identical(actual$vertices, rbind(left, right))
  expect_identical(actual$station, rep(station, 2))
  expect_identical(
    actual$indices,
    rbind(
      cbind(first, first + 1L, first + 4L),
      cbind(first, first + 4L, first + 3L)
    )
  )
})

test_that("sharp and reversing joins retain finite bounded footprints", {
  for (points in list(
    rbind(c(0, 0, 0), c(10, 0, 0), c(0, 0, .5)),
    rbind(c(0, 0, 0), c(10, 0, 0), c(0, 0, 0))
  )) {
    frames = calculate_render_road_vertex_frames(points)
    expect_true(anyNA(frames$miter_scale))
    station = calculate_road_path_cumulative_distance(points)
    footprint = build_render_road_contact_footprint(points, 2, station, frames)
    expect_true(all(is.finite(footprint$vertices)))
    expect_true(all(is.finite(footprint$station)))
    expect_true(all(
      footprint$indices >= 1 & footprint$indices <= nrow(footprint$vertices)
    ))
    # Beyond the turnaround there is no outgoing segment. The rounded disk
    # still covers the full width there, at the turnaround's own station.
    beyond = footprint$vertices[, 1] > 10.9
    expect_true(any(beyond))
    expect_true(all(footprint$station[beyond] == station[2]))
    expect_lt(max(footprint$vertices[, 1]), 11.03)
    a = footprint$vertices[footprint$indices[, 1], c(1, 3)]
    b = footprint$vertices[footprint$indices[, 2], c(1, 3)] - a
    c = footprint$vertices[footprint$indices[, 3], c(1, 3)] - a
    expect_true(all(abs(b[, 1] * c[, 2] - b[, 2] * c[, 1]) > 1e-14))
    envelope = sample_render_road_terrain_envelope(
      points,
      2,
      matrix(7, 51, 51),
      1,
      c(1, 1)
    )
    expect_true(all(is.finite(envelope$floor)))
    expect_equal(range(envelope$floor), c(7, 7))
  }
})

test_that("terrain underneath a rounded turnaround contributes to clearance", {
  points = rbind(c(0, 0, 0), c(10, 0, 0), c(0, 0, 0))
  h = matrix(0, 51, 51)
  # Grid x=11 lies past the centerline endpoint, inside the width-4 join.
  h[37, 26] = 9
  envelope = sample_render_road_terrain_envelope(points, 4, h, 1, c(1, 1))
  expect_equal(max(envelope$floor), 9, tolerance = 1e-8)
  expect_equal(
    max(envelope$floor[envelope$distance == 10]),
    9,
    tolerance = 1e-8
  )
})

test_that("the DC hairpin retains a finite terrain envelope", {
  fixture = readRDS(test_path("fixtures", "road-contact-hairpin.rds"))
  points = fixture$points
  # Keep the original turn geometry but place it inside a small test DEM.
  points[, c(1, 3)] = sweep(points[, c(1, 3)], 2, points[1, c(1, 3)])
  expect_true(all(is.finite(points)))
  expect_true(anyNA(calculate_render_road_vertex_frames(points)$miter_scale))
  envelope = sample_render_road_terrain_envelope(
    points,
    fixture$width,
    matrix(7, 101, 101),
    1,
    fixture$texture_world_scale
  )
  expect_true(all(is.finite(envelope$floor)))
  expect_equal(range(envelope$floor), c(7, 7))
  expect_equal(
    range(envelope$distance),
    range(calculate_road_path_cumulative_distance(
      points,
      fixture$texture_world_scale
    )),
    tolerance = 1e-7
  )
})
