test_that("indent_surface lowers and raises matrix heightmaps by a constant amount", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, nrow = 4, ncol = 4)
  water = sf::st_sfc(sf::st_polygon(list(rbind(
    c(1, 1),
    c(3, 1),
    c(3, 3),
    c(1, 3),
    c(1, 1)
  ))))

  lowered = indent_surface(
    heightmap,
    water,
    amount = 2,
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )
  expect_true(is.matrix(lowered))
  expect_equal(dim(lowered), dim(heightmap))
  expect_equal(sum(lowered == 8), 4)
  expect_equal(sum(lowered == 10), 12)

  raised = indent_surface(
    heightmap,
    water,
    amount = 2,
    direction = "up",
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )
  expect_equal(sum(raised == 12), 4)
  expect_equal(sum(raised == 10), 12)
})

test_that("indent_surface uses per-feature amount columns", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, nrow = 4, ncol = 4)
  water = sf::st_sf(
    depth = c(2, 5),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0),
        c(2, 0),
        c(2, 4),
        c(0, 4),
        c(0, 0)
      ))),
      sf::st_polygon(list(rbind(
        c(2, 0),
        c(4, 0),
        c(4, 4),
        c(2, 4),
        c(2, 0)
      )))
    )
  )

  lowered = indent_surface(
    heightmap,
    water,
    amount = "depth",
    extent = c(0, 4, 0, 4),
    touches = FALSE
  )

  expect_equal(sum(lowered == 8), 8)
  expect_equal(sum(lowered == 5), 8)
})

test_that("indent_surface transitions amount from polygon edges", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, nrow = 8, ncol = 8)
  water = sf::st_sfc(sf::st_polygon(list(rbind(
    c(1, 1),
    c(7, 1),
    c(7, 7),
    c(1, 7),
    c(1, 1)
  ))))

  lowered = indent_surface(
    heightmap,
    water,
    amount = 4,
    transition = 2,
    extent = c(0, 8, 0, 8),
    touches = FALSE
  )

  expect_equal(sum(lowered == 9), 20)
  expect_equal(sum(lowered == 7), 12)
  expect_equal(sum(lowered == 6), 4)
  expect_equal(sum(lowered == 10), 28)
})

test_that("indent_surface handles transition equal to depth", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, nrow = 12, ncol = 12)
  water = sf::st_sfc(sf::st_polygon(list(rbind(
    c(1, 1),
    c(11, 1),
    c(11, 11),
    c(1, 11),
    c(1, 1)
  ))))

  lowered = indent_surface(
    heightmap,
    water,
    amount = 4,
    transition = 4,
    extent = c(0, 12, 0, 12),
    touches = FALSE
  )
  depth = as.vector(heightmap - lowered)

  expect_equal(sort(unique(depth)), c(0, 0.5, 1.5, 2.5, 3.5, 4))
  expect_equal(sum(depth == 0.5), 36)
  expect_equal(sum(depth == 1.5), 28)
  expect_equal(sum(depth == 2.5), 20)
  expect_equal(sum(depth == 3.5), 12)
  expect_equal(sum(depth == 4), 4)
})

test_that("indent_surface transition ignores raster boundaries as edges", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  inside = matrix(TRUE, nrow = 4, ncol = 4)
  expect_false(any(rayshader:::indent_surface_edge_cells(inside)))

  inside[, 4] = FALSE
  expected = matrix(FALSE, nrow = 4, ncol = 4)
  expected[, 3] = TRUE
  expect_equal(rayshader:::indent_surface_edge_cells(inside), expected)

  heightmap = matrix(10, nrow = 6, ncol = 6)
  water = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0),
    c(6, 0),
    c(6, 6),
    c(0, 6),
    c(0, 0)
  ))))

  lowered = indent_surface(
    heightmap,
    water,
    amount = 4,
    transition = 2,
    extent = c(0, 6, 0, 6),
    touches = FALSE
  )

  expect_equal(unique(as.vector(heightmap - lowered)), 4)
})

test_that("indent_surface transition uses max amount for overlaps", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = matrix(10, nrow = 8, ncol = 8)
  water = sf::st_sf(
    depth = c(2, 4),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(1, 1),
        c(7, 1),
        c(7, 7),
        c(1, 7),
        c(1, 1)
      ))),
      sf::st_polygon(list(rbind(
        c(1, 1),
        c(7, 1),
        c(7, 7),
        c(1, 7),
        c(1, 1)
      )))
    )
  )

  lowered = indent_surface(
    heightmap,
    water,
    amount = "depth",
    transition = 2,
    extent = c(0, 8, 0, 8),
    touches = FALSE
  )

  expect_equal(sum(lowered == 9), 20)
  expect_equal(sum(lowered == 7), 12)
  expect_equal(sum(lowered == 6), 4)
  expect_false(any(lowered == 9.5))
})

test_that("indent_surface returns spatial rasters for spatial raster inputs", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  heightmap = terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:3857"
  )
  terra::values(heightmap) = 10
  water = sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(1, 1),
      c(3, 1),
      c(3, 3),
      c(1, 3),
      c(1, 1)
    ))),
    crs = sf::st_crs(3857)
  )

  lowered = indent_surface(
    heightmap,
    water,
    amount = 2,
    touches = FALSE
  )

  expect_true(inherits(lowered, "SpatRaster"))
  expect_equal(
    c(terra::nrow(lowered), terra::ncol(lowered), terra::nlyr(lowered)),
    c(terra::nrow(heightmap), terra::ncol(heightmap), terra::nlyr(heightmap))
  )
  expect_true(terra::same.crs(lowered, heightmap))
  expect_equal(sum(as.numeric(terra::values(lowered)) == 8), 4)
  expect_equal(sum(as.numeric(terra::values(lowered)) == 10), 12)
})

test_that("indent_surface preserves RasterLayer output class", {
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
  water = sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(1, 1),
      c(3, 1),
      c(3, 3),
      c(1, 3),
      c(1, 1)
    ))),
    crs = sf::st_crs(3857)
  )

  lowered = indent_surface(
    heightmap,
    water,
    amount = 2,
    touches = FALSE
  )

  expect_true(inherits(lowered, "RasterLayer"))
  expect_equal(sum(raster::values(lowered) == 8), 4)
  expect_equal(sum(raster::values(lowered) == 10), 12)
})
