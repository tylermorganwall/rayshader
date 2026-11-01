# Independent small-grid fixtures exercise holes and clipped boundaries without
# a graphics context. Production code also accepts irregular triangulations.
water_coverage_fixture = function(
  keep = function(x, z) TRUE,
  height = function(x, z) 0,
  wkt = 'POLYGON ((-1 -1,5 -1,5 5,-1 5,-1 -1))',
  level = 3
) {
  triangles = list()
  for (x in 0:3) {
    for (z in 0:3) {
      if (!keep(x, z)) {
        next
      }
      p = cbind(c(x, x + 1, x, x + 1), 0, c(z, z, z + 1, z + 1))
      p[, 2] = vapply(seq_len(4), function(i) height(p[i, 1], p[i, 3]), 0)
      triangles = c(triangles, list(p[c(1, 2, 3), ], p[c(2, 4, 3), ]))
    }
  }
  a = do.call(rbind, lapply(triangles, function(t) t[1, ]))
  b = do.call(rbind, lapply(triangles, function(t) t[2, ]))
  c = do.call(rbind, lapply(triangles, function(t) t[3, ]))
  list(
    footprints = list(geometry = sf::st_as_sfc(wkt), levels = level),
    terrain = list(
      a = a,
      b = b,
      c = c,
      xmin = pmin(a[, 1], b[, 1], c[, 1]),
      xmax = pmax(a[, 1], b[, 1], c[, 1]),
      zmin = pmin(a[, 3], b[, 3], c[, 3]),
      zmax = pmax(a[, 3], b[, 3], c[, 3]),
      origin = c(0, 0),
      step = c(4, 4),
      bins = 1L,
      faces = list('0' = seq_len(nrow(a))),
      magnitude = 4
    ),
    zscale = 1,
    tolerance = 1e-6,
    clearance = 1e-6
  )
}

# Inspect the entire solid, not just cap area: every edge has two oppositely
# directed incident faces, including the walls along missing-terrain holes.
water_coverage_check = function(mesh, expected_area, expected_heights) {
  expect_false(is.null(mesh))
  if (is.null(mesh)) {
    return(invisible(NULL))
  }
  v = mesh$vertices
  a = v[seq(1, nrow(v), 3), , drop = FALSE]
  b = v[seq(2, nrow(v), 3), , drop = FALSE]
  c = v[seq(3, nrow(v), 3), , drop = FALSE]
  u = b - a
  w = c - a
  normals = cbind(
    u[, 2] * w[, 3] - u[, 3] * w[, 2],
    u[, 3] * w[, 1] - u[, 1] * w[, 3],
    u[, 1] * w[, 2] - u[, 2] * w[, 1]
  )
  expect_true(all(rowSums(normals^2) > 0))
  top = normals[, 2] > 0
  expect_equal(sum(normals[top, 2]) / 2, expected_area, tolerance = 1e-9)
  expect_equal(range(v[, 2]), expected_heights, tolerance = 1e-9)
  expect_equal(
    sum(a * normals) / 6,
    expected_area * diff(expected_heights),
    tolerance = 1e-9
  )
  key = apply(v, 1, function(p) paste(sprintf('%.17g', p), collapse = ','))
  ids = matrix(match(key, unique(key)), ncol = 3, byrow = TRUE)
  first = c(ids[, 1], ids[, 2], ids[, 3])
  last = c(ids[, 2], ids[, 3], ids[, 1])
  edge = paste(pmin(first, last), pmax(first, last))
  expect_true(all(table(edge) == 2L))
  expect_true(all(tapply(sign(last - first), edge, sum) == 0))
  invisible(NULL)
}

test_that('coverage masks retain holes, separate islands, and outside cuts', {
  skip_if_not_installed('sf')
  for (case in list(
    list(keep = function(x, z) !(x == 1 && z == 1), area = 15),
    list(keep = function(x, z) x != 1, area = 12),
    list(
      keep = function(x, z) !(x == 1 && z == 1),
      area = 2.5,
      wkt = 'POLYGON ((-1 -1,1.5 -1,1.5 2,-1 2,-1 -1))'
    )
  )) {
    args = case[names(case) != 'area']
    fixture = do.call(water_coverage_fixture, args)
    result = do.call(
      build_polygon_water_meshes_cpp,
      c(fixture, list(threads = 2L))
    )
    expect_identical(attr(result, 'coverage_error'), '')
    expect_identical(result[[1]]$error, '')
    water_coverage_check(result[[1]]$mesh, case$area, c(-fixture$clearance, 3))
  }
})

test_that('shoreline sampling retains subtriangle ponds and polygon holes', {
  skip_if_not_installed('sf')
  fixture = water_coverage_fixture(
    wkt = paste0(
      'POLYGON ((0.1 0.1,0.4 0.1,0.4 0.4,0.1 0.4,0.1 0.1),',
      '(0.2 0.2,0.2 0.3,0.3 0.3,0.3 0.2,0.2 0.2))'
    )
  )
  result = do.call(
    build_polygon_water_meshes_cpp,
    c(fixture, list(threads = 1L))
  )
  expect_identical(result[[1]]$error, '')
  water_coverage_check(result[[1]]$mesh, .08, c(-fixture$clearance, 3))
  expect_lt(attr(result, 'work')[['mask_faces']], nrow(fixture$terrain$a))
})

test_that('interior terrain retains deep pits and coincident flat patches', {
  skip_if_not_installed('sf')
  for (height in list(
    function(x, z) if (x == 2 && z == 2) -5 else 0,
    function(x, z) if (x >= 1 && x <= 3 && z >= 1 && z <= 3) 1 else 0
  )) {
    fixture = water_coverage_fixture(
      height = height,
      wkt = 'POLYGON ((0.1 0.1,3.9 0.1,3.9 3.9,0.1 3.9,0.1 0.1))',
      level = 1
    )
    result = do.call(
      build_polygon_water_meshes_cpp,
      c(fixture, list(threads = 1L))
    )
    reference = do.call(build_polygon_water_part, c(list(i = 1L), fixture))
    expect_identical(result[[1]]$error, '')
    water_coverage_check(
      result[[1]]$mesh,
      3.8^2,
      range(reference$mesh$vertices[, 2])
    )
    expect_gt(attr(result, 'work')[['interior_faces']], 0)
  }
})

test_that('nonflat terrain enclosed by a polygon retains inferred level weights', {
  skip_if_not_installed('sf')
  fixture = water_coverage_fixture(
    height = function(x, z) x + z / 3,
    level = NA_real_
  )
  result = do.call(
    build_polygon_water_meshes_cpp,
    c(fixture, list(threads = 1L))
  )
  reference = do.call(build_polygon_water_part, c(list(i = 1L), fixture))
  expect_identical(result[[1]]$error, '')
  water_coverage_check(
    result[[1]]$mesh,
    16,
    range(reference$mesh$vertices[, 2])
  )
})

test_that('coverage is rebuilt when the terrain changes between calls', {
  skip_if_not_installed('sf')
  complete = water_coverage_fixture()
  hole = water_coverage_fixture(keep = function(x, z) !(x == 1 && z == 1))
  for (fixture in list(complete, hole, complete)) {
    result = do.call(
      build_polygon_water_meshes_cpp,
      c(fixture, list(threads = 1L))
    )
    water_coverage_check(
      result[[1]]$mesh,
      nrow(fixture$terrain$a) / 2,
      c(-fixture$clearance, 3)
    )
  }
})

test_that('water bottom caps remain below flat terrain at render precision', {
  skip_if_not_installed('sf')
  terrain_level = -11.435
  water_level = terrain_level + 0.0015
  fixture = water_coverage_fixture(
    height = function(x, z) terrain_level,
    level = water_level
  )
  # The DC scene spans roughly 1500 scene units. A minimum-depth water solid
  # must remain separated after rgl stores both it and the terrain as floats.
  fixture$clearance = 16 * 2^-23 * 750
  native = do.call(
    build_polygon_water_meshes_cpp,
    c(fixture, list(threads = 1L))
  )[[1]]
  fallback = do.call(build_polygon_water_part, c(list(i = 1L), fixture))
  expect_identical(native$error, '')
  for (record in list(native, fallback)) {
    v = record$mesh$vertices
    expect_equal(min(v[, 2]), terrain_level - fixture$clearance)
    expect_equal(max(v[, 2]), water_level)
    expect_equal(unique(record$mesh$lines[, 2]), water_level)
    # Round exactly as an rgl vertex upload does, without requiring a display.
    heights = c(min(v[, 2]), terrain_level, max(v[, 2]))
    float_heights = readBin(
      writeBin(heights, raw(), size = 4),
      'numeric',
      n = 3,
      size = 4
    )
    expect_lt(float_heights[1], float_heights[2])
    expect_gt(float_heights[3], float_heights[2])
    water_coverage_check(
      record$mesh,
      16,
      c(terrain_level - fixture$clearance, water_level)
    )
  }
})
