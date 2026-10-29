test_that("mixed water geometries retain the original feature elevations", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  water = sf::st_as_sf(
    data.frame(
      level = c(91, 92, 3, 94, 5, 96),
      wkt = c(
        "POINT (0 0)",
        "LINESTRING (0 0, 12 8)",
        "POLYGON ((1 1, 5 1, 5 7, 1 7, 1 1), (2 2, 2 6, 4 6, 4 2, 2 2))",
        "MULTIPOINT ((0 0), (12 8))",
        "MULTIPOLYGON (((7 1, 11 1, 11 7, 7 7, 7 1)))",
        "POLYGON EMPTY"
      )
    ),
    wkt = "wkt",
    crs = 3857
  )
  convert = function(input, elevation = NULL) {
    water_sf_to_raster(
      input,
      matrix(1, 12, 8),
      c(xmin = 0, xmax = 12, ymin = 0, ymax = 8),
      3857,
      water_elevation = elevation
    )
  }
  expected = convert(water[c(3, 5), ], "level")
  for (actual in list(
    convert(water, "level"),
    convert(water, water$level),
    convert(sf::st_geometry(water), water$level)
  )) {
    expect_equal(terra::values(actual), terra::values(expected))
    expect_true(terra::compareGeom(actual, expected))
  }
  expect_equal(
    sort(unique(na.omit(as.numeric(terra::values(expected))))),
    c(3, 5)
  )
  # Dropped features do not fill their point/line cells or the polygon hole.
  expect_true(is.na(terra::extract(expected, matrix(c(3, 4), ncol = 2))[1, 1]))
  expect_equal(
    terra::values(convert(water, 7)),
    terra::values(convert(water[c(3, 5), ], 7))
  )
  expect_equal(
    terra::values(convert(water)),
    terra::values(convert(water[c(3, 5), ]))
  )
})

test_that("water retains polygon parts within mixed geometry collections", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  geometry = sf::st_as_sfc(
    c(
      "LINESTRING (0 0, 12 8)",
      "GEOMETRYCOLLECTION (POINT (1 1), POLYGON ((2 2, 5 2, 5 6, 2 6, 2 2)), LINESTRING (0 0, 12 8))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (0 0, 12 8))"
    ),
    crs = 3857
  )
  actual = water_sf_to_raster(
    geometry,
    matrix(1, 12, 8),
    c(0, 12, 0, 8),
    3857,
    water_elevation = c(91, 4, 93)
  )
  expected = water_sf_to_raster(
    sf::st_as_sfc("POLYGON ((2 2, 5 2, 5 6, 2 6, 2 2))", crs = 3857),
    matrix(1, 12, 8),
    c(0, 12, 0, 8),
    3857,
    water_elevation = 4
  )
  expect_equal(terra::values(actual), terra::values(expected))
})

test_that("water with no nonempty polygon features is a no-op", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  geometry = sf::st_as_sfc(
    c(
      "POINT (1 1)",
      "LINESTRING (0 0, 12 8)",
      "POLYGON EMPTY",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (0 0, 12 8))"
    ),
    crs = 3857
  )
  for (input in list(
    geometry,
    sf::st_sf(geometry = geometry),
    geometry[FALSE]
  )) {
    expect_null(water_sf_to_raster(
      input,
      matrix(1, 12, 8),
      c(0, 12, 0, 8),
      3857
    ))
  }
})

test_that("render_water draws mixed inputs through the public API", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  withr::local_options(rgl.useNULL = TRUE)
  terrain = terra::rast(
    nrows = 8,
    ncols = 12,
    xmin = 0,
    xmax = 12,
    ymin = 0,
    ymax = 8,
    crs = "EPSG:3857",
    vals = 0
  )
  plot_3d(height_shade(terrain), terrain, shadow = FALSE, solid = FALSE)
  withr::defer(rgl::close3d())
  withr::defer(reset_scene_context(
    clear_scene_metadata = TRUE,
    clear_scene_cache = TRUE
  ))
  water = sf::st_as_sfc(
    c(
      "LINESTRING (0 0, 12 8)",
      "POLYGON ((2 2, 5 2, 5 6, 2 6, 2 2))"
    ),
    crs = 3857
  )
  expect_no_condition(render_water(water, water_elevation = c(100, 1)))
  expect_true(any(rgl::ids3d(tags = TRUE)$tag == "water"))
  before = rgl::ids3d()
  expect_null(render_water(water[1], clear_previous = FALSE))
  expect_equal(rgl::ids3d(), before)
})
