test_that("spatial water masks preserve orientation and terrain context", {
  skip_if_not_installed("terra")
  clear_hillshade_cache()
  withr::defer(clear_hillshade_cache())
  terrain = terra::rast(
    nrows = 3,
    ncols = 5,
    xmin = 10,
    xmax = 60,
    ymin = 20,
    ymax = 50,
    crs = "EPSG:3857"
  )
  terra::values(terrain) = c(
    -2,
    3,
    4,
    5,
    6,
    7,
    8,
    -9,
    10,
    11,
    12,
    13,
    14,
    15,
    -16
  )
  image = height_shade(terrain)
  keys = c(
    "hillshade_heightmap",
    "hillshade_heightmap_label",
    "hillshade_extent",
    "hillshade_crs",
    "hillshade_zscale",
    "hillshade_geographic_aspect"
  )
  before = mget(keys, envir = ray_cache_scene_envir)
  mask = terrain < 0
  actual = add_water(image, mask, color = "red")
  expected = add_water(
    image,
    raster_to_matrix(mask, verbose = FALSE),
    color = "red"
  )
  expect_equal(actual, expected)
  expect_identical(mget(keys, envir = ray_cache_scene_envir), before)
  expect_identical(get_hillshade_map(), expected)
  # Input rows run north to south; the RGB map's first dimension runs south to north.
  wet = matrix(
    c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(as.numeric(actual[,, 1][wet]), rep(1, 3))
  expect_equal(as.numeric(actual[,, 2][wet]), rep(0, 3))
  expect_equal(actual[,, 3][!wet], image[,, 3][!wet])

  file = tempfile(fileext = ".tif")
  withr::defer(unlink(file))
  terra::writeRaster(mask, file)
  expect_equal(add_water(image, file, color = "red"), expected)
  if (requireNamespace("raster", quietly = TRUE)) {
    expect_equal(
      add_water(image, raster::raster(mask), color = "red"),
      expected
    )
  }
})

test_that("spatial water masks reject misaligned grids without changing caches", {
  skip_if_not_installed("terra")
  clear_hillshade_cache()
  withr::defer(clear_hillshade_cache())
  terrain = terra::rast(
    nrows = 3,
    ncols = 5,
    xmin = 1e6,
    xmax = 1e6 + 50,
    ymin = 2e6,
    ymax = 2e6 + 30,
    crs = "EPSG:3857"
  )
  terra::values(terrain) = seq_len(15) - 5
  image = height_shade(terrain)
  before = as.list(ray_cache_scene_envir)
  mask = terrain < 0
  expect_error(add_water(image, terra::shift(mask, dx = 0.01)), "alignment")
  expect_error(
    add_water(
      image,
      terra::crop(mask, terra::ext(1e6, 1e6 + 40, 2e6, 2e6 + 30))
    ),
    "grid dimensions"
  )
  wrong_crs = terra::deepcopy(mask)
  terra::crs(wrong_crs) = "EPSG:32610"
  expect_error(add_water(image, wrong_crs), "CRS")
  terra::crs(wrong_crs) = ""
  expect_error(add_water(image, wrong_crs), "CRS")
  expect_error(add_water(image, c(mask, mask)), "exactly one")
  expect_identical(as.list(ray_cache_scene_envir), before)
})

test_that("matrix masks remain supported and missing cells leave the map unchanged", {
  clear_hillshade_cache()
  withr::defer(clear_hillshade_cache())
  terrain = matrix(seq_len(15), nrow = 5)
  image = height_shade(terrain)
  mask = terrain < 3
  mask[3, 2] = NA
  dry_missing = mask
  dry_missing[is.na(dry_missing)] = FALSE
  expect_equal(add_water(image, mask), add_water(image, dry_missing))
  expect_error(add_water(image, matrix(0, 4, 3)), "dimensions must be the same")
  expect_error(add_water(image, matrix(0, 5, 2)), "dimensions must be the same")
  expect_error(add_water(image), "must be supplied")
  expect_error(add_water(image, 1:15), "numeric or logical matrix")
})

test_that("spatial masks also work without cached spatial metadata", {
  skip_if_not_installed("terra")
  clear_hillshade_cache()
  withr::defer(clear_hillshade_cache())
  mask = terra::rast(nrows = 3, ncols = 5, vals = c(1, rep(0, 13), NA))
  image = array(0.5, c(3, 5, 3))
  expect_equal(
    add_water(image, mask),
    add_water(image, raster_to_matrix(mask, verbose = FALSE))
  )
  expect_null(get_hillshade_heightmap())
  expect_null(get_hillshade_extent())
  expect_null(get_hillshade_crs())
})


test_that("piped shading establishes the terrain cache before validating the mask", {
  skip_if_not_installed("terra")
  clear_hillshade_cache()
  withr::defer(clear_hillshade_cache())
  terrain = terra::rast(
    nrows = 3,
    ncols = 5,
    vals = seq_len(15) - 4,
    xmin = 10,
    xmax = 60,
    ymin = 20,
    ymax = 50,
    crs = "EPSG:3857"
  )
  height_shade(matrix(seq_len(12), 4, 3))
  result = terrain |> height_shade() |> add_water(terrain < 0)
  expect_equal(
    get_hillshade_heightmap(),
    raster_to_matrix(terrain, verbose = FALSE)
  )
  expect_identical(get_hillshade_map(), result)
  # The inverse case must also reject the mask against the new, not old, cache.
  expect_error(
    matrix(seq_len(12), 4, 3) |> height_shade() |> add_water(terrain < 0),
    "grid dimensions"
  )
})
