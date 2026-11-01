# A tiny indexed surface keeps thread/ordering tests independent of rgl.
parallel_water_fixture = function(count = 4096L) {
  terrain = list(
    a = rbind(c(0, 0, 0), c(1, 0, 0)),
    b = rbind(c(1, 0, 0), c(1, 0, 1)),
    c = rbind(c(0, 0, 1), c(0, 0, 1)),
    xmin = c(0, 0),
    xmax = c(1, 1),
    zmin = c(0, 0),
    zmax = c(1, 1),
    origin = c(0, 0),
    step = c(1, 1),
    bins = 1L,
    faces = list('0' = 1:2),
    magnitude = 1
  )
  geometry = sf::st_as_sfc(c(
    'POLYGON ((0.1 0.1,0.4 0.1,0.4 0.4,0.1 0.4,0.1 0.1))',
    'POLYGON ((0.1 0.1,0.9 0.1,0.9 0.9,0.1 0.9,0.1 0.1),(0.3 0.3,0.3 0.7,0.7 0.7,0.7 0.3,0.3 0.3))',
    'POLYGON ((2 2,3 2,3 3,2 3,2 2))',
    'POLYGON ((0.1 0.1,0.9 0.9,0.1 0.9,0.9 0.1,0.1 0.1))'
  ))
  list(
    footprints = list(
      geometry = geometry[rep(1:4, length.out = count)],
      levels = rep(c(1, NA, 1, 1), length.out = count)
    ),
    terrain = terrain,
    zscale = 1,
    tolerance = 1e-6,
    clearance = 1e-6
  )
}

test_that('parallel water preserves ordered geometry and omission diagnostics', {
  skip_if_not_installed('sf')
  withr::local_options(cores = 2L)
  fixture = parallel_water_fixture()
  expected = do.call(
    build_polygon_water_parts,
    c(fixture, list(parallel = FALSE))
  )
  expect_warning(
    actual <- do.call(
      build_polygon_water_parts,
      c(fixture, list(parallel = TRUE))
    ),
    NA
  )
  expect_identical(actual, expected)
  # Compare a separate implementation geometrically: constructing the footprint
  # directly can choose another starting vertex/winding than a triangle union.
  # Invalid-input GEOS diagnostics can vary across GEOS versions.
  reference = do.call(build_polygon_water_part, c(list(i = 1L), fixture))
  ordered_vertices = function(mesh) {
    v = unique(mesh$vertices)
    v[do.call(order, as.data.frame(v)), , drop = FALSE]
  }
  expect_equal(
    ordered_vertices(actual[[1]]$mesh),
    ordered_vertices(reference$mesh)
  )
  expect_identical(actual[[1]]$warnings, reference$warnings)
  expect_true(!is.null(actual[[1]]$mesh))
  expect_true(!is.null(actual[[2]]$mesh))
  expect_null(actual[[3]]$mesh)
  expect_null(actual[[4]]$mesh)
  expect_match(
    conditionMessage(actual[[4]]$warnings[[1]]),
    'Omitted water polygon 4:'
  )
  # A supplied top below all supporting terrain is omitted without a warning.
  fixture$footprints$levels[1] = -1
  record = do.call(build_polygon_water_part, c(list(i = 1L), fixture))
  expect_null(record$mesh)
  expect_length(record$warnings, 0L)
})

test_that('native dispatch respects batch size, parallel switch and core limits', {
  skip_if_not_installed('sf')
  withr::local_options(cores = 4L)
  observed = integer()
  local_mocked_bindings(
    .package = "rayshader",
    build_polygon_water_meshes_cpp = function(
      footprints,
      terrain,
      zscale,
      tolerance,
      clearance,
      threads
    ) {
      observed <<- c(observed, threads)
      rep(list(list(mesh = NULL, error = '')), length(footprints$geometry))
    }
  )
  small = parallel_water_fixture(4L)
  large = parallel_water_fixture()
  invisible(do.call(build_polygon_water_parts, c(small, list(parallel = TRUE))))
  invisible(do.call(
    build_polygon_water_parts,
    c(large, list(parallel = FALSE))
  ))
  invisible(do.call(build_polygon_water_parts, c(large, list(parallel = TRUE))))
  expect_identical(observed, c(1L, 1L, 4L))
  for (cores in list(1L, NA_real_, Inf, '4', numeric(), c(1, 2))) {
    options(cores = cores)
    invisible(do.call(
      build_polygon_water_parts,
      c(large, list(parallel = TRUE))
    ))
    expect_identical(tail(observed, 1), 1L)
  }
})

test_that('native geometry failures retain order and subsequent calls recover', {
  skip_if_not_installed('sf')
  fixture = parallel_water_fixture(16L)
  serial = do.call(
    build_polygon_water_meshes_cpp,
    c(fixture, list(threads = 1L))
  )
  threaded = do.call(
    build_polygon_water_meshes_cpp,
    c(fixture, list(threads = 4L))
  )
  expect_identical(threaded, serial)
  expect_true(nzchar(threaded[[4]]$error))
  expect_null(threaded[[4]]$mesh)
  expect_true(!is.null(threaded[[5]]$mesh))
  again = do.call(
    build_polygon_water_meshes_cpp,
    c(fixture, list(threads = 4L))
  )
  expect_identical(again, serial)
  fixture$footprints$geometry = fixture$footprints$geometry[FALSE]
  fixture$footprints$levels = numeric()
  expect_identical(
    do.call(build_polygon_water_meshes_cpp, c(fixture, list(threads = 4L))),
    list()
  )
})

test_that('native extrusion detects components sharing a vertical edge', {
  skip_if_not_installed('sf')
  fixture = parallel_water_fixture(1L)
  # Two triangular islands touch only at (1, 1). Independently closed caps
  # would create four incident wall faces along their shared vertical edge.
  fixture$terrain$a = rbind(c(0, 0, 0), c(1, 0, 1))
  fixture$terrain$b = rbind(c(1, 0, 0), c(2, 0, 2))
  fixture$terrain$c = rbind(c(1, 0, 1), c(1, 0, 2))
  fixture$terrain$step = c(2, 2)
  fixture$terrain$magnitude = 2
  fixture$footprints$geometry = sf::st_as_sfc(
    'POLYGON ((-1 -1,3 -1,3 3,-1 3,-1 -1))'
  )
  result = do.call(
    build_polygon_water_meshes_cpp,
    c(fixture, list(threads = 2L))
  )
  expect_null(result[[1]]$mesh)
  expect_match(result[[1]]$error, 'closed, consistently oriented mesh')
})

test_that('union conserves the covered footprint across GEOS versions', {
  skip_if_not_installed('sf')
  fixture = readRDS(test_path('fixtures', 'polygon-water-union.rds'))
  # This DC footprint loses a piece in GEOS 3.11's union despite correct
  # intersections. Newer engines may construct it directly; either way the
  # public builder must retain all of its supported area.
  actual = do.call(
    build_polygon_water_parts,
    c(fixture, list(parallel = FALSE))
  )
  reference = do.call(build_polygon_water_part, c(list(i = 1L), fixture))
  top_area = function(mesh) {
    v = mesh$vertices
    a = v[seq(1, nrow(v), 3), , drop = FALSE]
    b = v[seq(2, nrow(v), 3), , drop = FALSE]
    c = v[seq(3, nrow(v), 3), , drop = FALSE]
    area = ((b[, 3] - a[, 3]) *
      (c[, 1] - a[, 1]) -
      (b[, 1] - a[, 1]) * (c[, 3] - a[, 3])) /
      2
    sum(area[area > 0])
  }
  expect_length(actual[[1]]$warnings, 0L)
  expect_equal(
    top_area(actual[[1]]$mesh),
    top_area(reference$mesh),
    tolerance = 1e-9
  )
})

test_that('rejected native boundaries retry through the established geometry path', {
  skip_if_not_installed('sf')
  fixture = parallel_water_fixture(4L)
  expected = lapply(seq_len(4L), function(i) {
    do.call(build_polygon_water_part, c(list(i = i), fixture))
  })
  local_mocked_bindings(
    .package = 'rayshader',
    build_polygon_water_meshes_cpp = function(...) {
      rep(list(list(mesh = NULL, error = 'boundary rejected')), 4L)
    }
  )
  actual = do.call(build_polygon_water_parts, c(fixture, list(parallel = TRUE)))
  expect_identical(actual, expected)
  expect_length(actual[[1]]$warnings, 0L)
  expect_length(actual[[4]]$warnings, 1L)
})

test_that('interrupts propagate without restarting native construction', {
  skip_if_not_installed('sf')
  fixture = parallel_water_fixture(4L)
  calls = 0L
  local_mocked_bindings(
    .package = "rayshader",
    build_polygon_water_meshes_cpp = function(...) {
      calls <<- calls + 1L
      signalCondition(structure(
        list(message = 'test interrupt', call = NULL),
        class = c('interrupt', 'condition')
      ))
    }
  )
  interrupted = tryCatch(
    do.call(build_polygon_water_parts, c(fixture, list(parallel = TRUE))),
    interrupt = function(e) TRUE
  )
  expect_true(interrupted)
  expect_identical(calls, 1L)
})

test_that('render_water validates the parallel switch before construction', {
  for (value in list(NA, NULL, 'yes')) {
    expect_error(render_water(1, parallel = value), 'parallel')
  }
})
