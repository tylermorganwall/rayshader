test_that("Bryce GeoTIFF scenes auto-cache raster metadata for render_buildings()", {
  skip_if_not_installed("sf")
  skip_if_not_installed("raster")
  skip_if_not_installed("raybevel")
  skip_if_not_installed("rayvertex")
  skip_if_not(bryce_raybevel_backend_available_test())
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  fixtures = load_bryce_building_scene_fixtures_test()
  setup_bryce_buildings_plot3d_scene_test(fixtures)

  expected_aspect = calculate_geographic_aspect(
    fixtures$heightmap,
    extent = get_extent(fixtures$raster_crop),
    crs = sf::st_crs(raster::crs(fixtures$raster_crop)),
    extent_is_cell_bounds = TRUE
  )
  expect_equal(
    get_scene_zscale(),
    expected_aspect$mean_cell_meters,
    tolerance = 1e-8
  )
  expect_equal(
    get_scene_extent(),
    get_extent(fixtures$raster_crop),
    tolerance = 1e-8
  )
  expect_true(scene_crs_equal(
    get_scene_crs(),
    sf::st_crs(raster::crs(fixtures$raster_crop))
  ))
  expect_equal(dim(get_scene_heightmap()), dim(fixtures$heightmap))

  sample_rows = unique(round(c(
    1,
    nrow(fixtures$heightmap) / 2,
    nrow(fixtures$heightmap)
  )))
  sample_cols = unique(round(c(
    1,
    ncol(fixtures$heightmap) / 2,
    ncol(fixtures$heightmap)
  )))
  expect_equal(
    get_scene_heightmap()[sample_rows, sample_cols, drop = FALSE],
    fixtures$heightmap[sample_rows, sample_cols, drop = FALSE],
    tolerance = 1e-8
  )

  expect_no_condition(render_buildings(
    fixtures$buildings_4326,
    data_column_top = "height_m",
    material = "grey70",
    roof_material = "white",
    angle = 30,
    clear_previous = TRUE
  ))

  ids = get_ids_with_labels(typeval = "obj")
  building_ids = ids$id[ids$tag == "obj_raymesh_building"]
  expect_gte(length(building_ids), 1)

  expected_polygons = rayshader:::transform_polygon_into_raycoords(
    polygon = fixtures$buildings_4326,
    heightmap = get_scene_heightmap(),
    e = get_scene_extent(),
    top = fixtures$buildings_4326$height_m,
    bottom = 0,
    caller = "test"
  )
  expected_bbox = sf::st_bbox(expected_polygons)
  building_vertices = do.call(
    rbind,
    lapply(building_ids, function(id) rgl::rgl.attrib(id, "vertices"))
  )

  expect_equal(
    range(building_vertices[, 1]),
    c(-unname(expected_bbox["xmax"]), -unname(expected_bbox["xmin"])),
    tolerance = 1e-4
  )
  expect_equal(
    range(building_vertices[, 3]),
    c(unname(expected_bbox["ymin"]), unname(expected_bbox["ymax"])),
    tolerance = 1e-4
  )
})

test_that("Bryce GeoTIFF building snapshots match goldens", {
  testthat::skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("raster")
  skip_if_not_installed("raybevel")
  skip_if_not_installed("rayvertex")
  skip_if_not(bryce_raybevel_backend_available_test())
  skip_if(
    rgl::rgl.useNULL(),
    message = "Software golden snapshots require a live rgl device."
  )

  expect_bryce_buildings_snapshot_test(
    "plot3d_render_buildings_bryce.png",
    render_call = function(fixtures) {
      render_buildings(
        fixtures$buildings_4326,
        data_column_top = "height_m",
        material = "grey70",
        roof_material = "white",
        angle = 30,
        clear_previous = TRUE
      )
    }
  )
})
