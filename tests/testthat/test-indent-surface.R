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
