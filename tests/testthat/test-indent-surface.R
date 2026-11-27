make_shift_test_polygon = function(xmin, xmax, ymin, ymax, crs = NULL) {
  polygon = sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )))
  if (is.null(crs)) {
    return(sf::st_sfc(polygon))
  }
  sf::st_sfc(polygon, crs = crs)
}

test_that("shift_terrain abruptly lowers and raises matrix terrain", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, nrow = 4, ncol = 4)
  water = make_shift_test_polygon(1, 3, 1, 3)

  lowered = shift_terrain(
    heightmap,
    water,
    amount = -2,
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )
  raised = shift_terrain(
    heightmap,
    water,
    amount = 3,
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )

  expect_equal(sum(lowered == 8), 4)
  expect_equal(sum(lowered == 10), 12)
  expect_equal(sum(raised == 13), 4)
  expect_equal(sum(raised == 10), 12)
})

test_that("shift_terrain accepts signed vectors and feature columns", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, nrow = 4, ncol = 4)
  geometry = sf::st_sf(
    shift = c(-2, 5),
    geometry = c(
      make_shift_test_polygon(0, 2, 0, 4),
      make_shift_test_polygon(2, 4, 0, 4)
    )
  )

  vector_result = shift_terrain(
    heightmap,
    geometry,
    amount = c(-2, 5),
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )
  column_result = shift_terrain(
    heightmap,
    geometry,
    amount = "shift",
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )

  expect_equal(vector_result, column_result)
  expect_equal(sum(vector_result == 8), 8)
  expect_equal(sum(vector_result == 15), 8)
})

test_that("uniform feature amounts use one transition field", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  geometry = sf::st_sf(
    geometry = c(
      make_shift_test_polygon(1, 4, 1, 7),
      make_shift_test_polygon(4, 7, 1, 7)
    )
  )
  original_rasterize = rayshader:::rasterize_indent_surface_feature_mask
  rasterize_calls = 0
  testthat::local_mocked_bindings(
    rasterize_indent_surface_feature_mask = function(...) {
      rasterize_calls <<- rasterize_calls + 1
      original_rasterize(...)
    },
    .package = "rayshader"
  )

  result = shift_terrain(
    matrix(10, 8, 8),
    geometry,
    amount = c(-4, -4),
    transition = 2,
    extent = c(0, 8, 0, 8),
    touches = FALSE
  )

  expect_equal(rasterize_calls, 1)
  expect_true(any(result < 10))
})

test_that("transition overlap honors max and non-default reducers", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  polygon = make_shift_test_polygon(1, 7, 1, 7)
  geometry = sf::st_sf(
    amount = c(2, 4),
    geometry = c(polygon, polygon)
  )
  arguments = list(
    heightmap = matrix(10, 8, 8),
    geometry = geometry,
    amount = "amount",
    transition = 2,
    extent = c(0, 8, 0, 8),
    touches = FALSE
  )

  max_result = do.call(shift_terrain, arguments)
  min_result = do.call(shift_terrain, c(arguments, list(fun = "min")))
  function_result = do.call(
    shift_terrain,
    c(arguments, list(fun = function(values) mean(values)))
  )

  expect_equal(max(max_result), 14)
  expect_equal(max(min_result), 12)
  expect_equal(max(function_result), 13)
})

test_that("cell transition units reproduce unit-resolution map behavior", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, 8, 8)
  polygon = make_shift_test_polygon(1, 7, 1, 7)
  arguments = list(
    heightmap = heightmap,
    geometry = polygon,
    amount = -4,
    transition = 2,
    extent = c(0, 8, 0, 8),
    touches = FALSE
  )

  map_result = do.call(
    shift_terrain,
    c(arguments, list(transition_units = "map"))
  )
  cell_result = do.call(
    shift_terrain,
    c(arguments, list(transition_units = "cells"))
  )

  expect_equal(cell_result, map_result, tolerance = 1e-8)
  expect_equal(sort(unique(as.vector(heightmap - cell_result))), c(0, 1, 3, 4))
})

test_that("map transition units use projected raster distances", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = terra::rast(
    nrows = 8,
    ncols = 8,
    xmin = 0,
    xmax = 80,
    ymin = 0,
    ymax = 80,
    crs = "EPSG:3857"
  )
  terra::values(heightmap) = 10
  polygon = make_shift_test_polygon(10, 70, 10, 70, crs = 3857)

  result = shift_terrain(
    heightmap,
    polygon,
    amount = -4,
    transition = 20,
    transition_units = "map",
    touches = FALSE
  )
  shift = 10 - terra::values(result, mat = FALSE)

  expect_equal(sort(unique(shift)), c(0, 1, 3, 4))
  expect_true(terra::same.crs(result, heightmap))
})

test_that("CRS-less matrix transitions do not fabricate a CRS", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, 6, 6)
  surface = rayshader:::prepare_indent_surface_matrix(
    heightmap,
    extent = c(0, 6, 0, 6),
    caller = "shift_terrain"
  )
  expect_identical(terra::crs(surface$template), "")

  result = NULL
  expect_no_warning({
    result = shift_terrain(
      heightmap,
      make_shift_test_polygon(1, 5, 1, 5),
      amount = -2,
      transition = 2,
      transition_units = "map",
      extent = c(0, 6, 0, 6),
      touches = FALSE
    )
  })
  expect_true(is.matrix(result))
})

test_that("non-square raster transitions use explicit distance units", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = terra::rast(
    nrows = 6,
    ncols = 6,
    xmin = 0,
    xmax = 12,
    ymin = 0,
    ymax = 6
  )
  terra::crs(heightmap) = ""
  terra::values(heightmap) = 10
  polygon = make_shift_test_polygon(2, 10, 1, 5)

  cell_result = shift_terrain(
    heightmap,
    polygon,
    amount = -4,
    transition = 2,
    transition_units = "cells",
    touches = FALSE
  )
  map_result = shift_terrain(
    heightmap,
    polygon,
    amount = -4,
    transition = 2,
    transition_units = "map",
    touches = FALSE
  )

  expect_false(isTRUE(all.equal(
    terra::values(cell_result),
    terra::values(map_result)
  )))
  expect_equal(min(terra::values(cell_result)), 7)
  expect_equal(min(terra::values(map_result)), 6)
})

test_that("a full-raster polygon has no artificial boundary transition", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  result = shift_terrain(
    matrix(10, 6, 6),
    make_shift_test_polygon(0, 6, 0, 6),
    amount = -4,
    transition = 2,
    transition_units = "cells",
    extent = c(0, 6, 0, 6),
    touches = FALSE
  )

  expect_equal(unique(as.vector(result)), 6)
})

test_that("NA feature amounts skip their features", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  geometry = sf::st_sf(
    amount = c(-2, NA_real_),
    geometry = c(
      make_shift_test_polygon(0, 2, 0, 4),
      make_shift_test_polygon(2, 4, 0, 4)
    )
  )
  result = shift_terrain(
    matrix(10, 4, 4),
    geometry,
    amount = "amount",
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )

  expect_equal(sum(result == 8), 8)
  expect_equal(sum(result == 10), 8)
})

test_that("matrix and SpatRaster inputs preserve their return contracts", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  polygon = make_shift_test_polygon(1, 3, 1, 3)
  matrix_result = shift_terrain(
    matrix(10, 4, 4),
    polygon,
    amount = -2,
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )

  raster = terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4
  )
  terra::crs(raster) = ""
  terra::values(raster) = 10
  raster_result = shift_terrain(
    raster,
    polygon,
    amount = -2,
    touches = FALSE
  )

  expect_true(is.matrix(matrix_result))
  expect_s4_class(raster_result, "SpatRaster")
})

test_that("explicit raster CRS overrides metadata without mutating input", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )
  terra::values(heightmap) = 10
  original_crs = terra::crs(heightmap)
  result = shift_terrain(
    heightmap,
    make_shift_test_polygon(1, 3, 1, 3, crs = 3857),
    amount = -2,
    crs = 3857,
    touches = FALSE
  )

  expect_true(terra::same.crs(result, "EPSG:3857"))
  expect_true(terra::same.crs(terra::crs(heightmap), original_crs))
  expect_equal(sum(terra::values(result) == 8), 4)
})

test_that("RasterLayer inputs warn and return SpatRaster output", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("raster")

  heightmap = raster::raster(
    nrows = 4,
    ncols = 4,
    xmn = 0,
    xmx = 4,
    ymn = 0,
    ymx = 4,
    crs = sf::st_crs(3857)$wkt
  )
  raster::values(heightmap) = 10

  result = NULL
  expect_warning(
    {
      result = shift_terrain(
        heightmap,
        make_shift_test_polygon(1, 3, 1, 3, crs = 3857),
        amount = -2,
        touches = FALSE
      )
    },
    "will soon be deprecated"
  )

  expect_s4_class(result, "SpatRaster")
  expect_equal(sum(terra::values(result) == 8), 4)
})

test_that("character paths and multilayer rasters retain spatial contracts", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  layer = terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4
  )
  terra::crs(layer) = "EPSG:3857"
  terra::values(layer) = 10
  multilayer = c(layer, layer + 10)
  filename = tempfile(fileext = ".tif")
  terra::writeRaster(layer, filename)

  multilayer_result = NULL
  expect_warning(
    {
      multilayer_result = shift_terrain(
        multilayer,
        make_shift_test_polygon(1, 3, 1, 3, crs = 3857),
        amount = -2,
        touches = FALSE
      )
    },
    "multiple layers"
  )
  path_result = shift_terrain(
    filename,
    make_shift_test_polygon(1, 3, 1, 3, crs = 3857),
    amount = -2,
    touches = FALSE
  )

  expect_equal(terra::nlyr(multilayer_result), 1)
  expect_s4_class(path_result, "SpatRaster")
})
