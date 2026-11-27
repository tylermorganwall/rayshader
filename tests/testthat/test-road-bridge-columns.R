bridge_column_scene = function(valley = FALSE) {
  rgl::open3d(useNULL = TRUE)
  raster = terra::rast(
    nrows = 101,
    ncols = 101,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100,
    crs = "EPSG:3857"
  )
  terra::values(raster) = if (valley) {
    as.vector(pmin(10, outer((-50:50 / 10)^2, (-50:50 / 10)^2, `+`)))
  } else {
    0
  }
  plot_3d(
    constant_shade(raster),
    raster,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    window_size = c(200, 200)
  )
  get_scene_heightmap()
}

bridge_column_crossing = function() {
  sf::st_sf(
    bridge = c(FALSE, TRUE),
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(5, 50), c(95, 50))),
      sf::st_linestring(rbind(c(50, 5), c(50, 95))),
      crs = 3857
    )
  )
}

test_that("bridge-only OSM columns activate profiles automatically", {
  skip_if_not_installed("osqp")
  height = bridge_column_scene()
  on.exit(rgl::close3d())
  roads = bridge_column_crossing()
  roads$bridge = c(NA_character_, "yes")
  original = roads
  paths = render_roads(
    roads,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  expect_identical(attr(paths, "profile_diagnostics")$solver, "sparse_qp")
  expect_equal(max(paths[[1]][, 2]), 0, tolerance = 1e-6)
  expect_equal(min(paths[[2]][, 2]), 5.5, tolerance = 1e-3)
  expect_false(attr(paths, "terrain_following")[[2]])
  expect_identical(roads, original)
  expect_length(get_scene_road_meshes(), 2)
})

test_that("custom logical bridge columns work with quoted and bare names", {
  skip_if_not_installed("osqp")
  height = bridge_column_scene()
  on.exit(rgl::close3d())
  roads = bridge_column_crossing()
  roads$is_overpass = roads$bridge
  roads$bridge = FALSE
  bare = render_roads(
    roads,
    bridge = is_overpass,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  quoted = render_roads(
    roads,
    bridge = "is_overpass",
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  expect_equal(bare[[2]], quoted[[2]])
  expect_equal(min(bare[[2]][, 2]), 5.5, tolerance = 1e-3)
  roads$clearance = c(NA, 7)
  higher = render_roads(
    roads,
    bridge = is_overpass,
    layer_height = clearance,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  expect_equal(min(higher[[2]][, 2]), 7, tolerance = 1e-3)
  expect_error(
    render_roads(roads, bridge = "missing_bridge"),
    "bridge.*not found"
  )
  expect_error(render_roads(roads, layer = "missing_layer"), "layer.*not found")
  expect_error(
    render_roads(roads, bridge = is_overpass, offset = 1),
    "cannot be combined"
  )
})

test_that("layer ordering and explicit structure opt-out are preserved", {
  skip_if_not_installed("osqp")
  height = bridge_column_scene()
  on.exit(rgl::close3d())
  roads = bridge_column_crossing()
  roads$layer = c(0, 2)
  auto = render_roads(
    roads,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  roads$levels = roads$layer
  custom = render_roads(
    roads,
    layer = levels,
    bridge = bridge,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  expect_equal(custom[[2]], auto[[2]])
  values = resolve_render_road_osm_layer_values(roads, "layer")
  expect_identical(values$layer, c(0, 2))
  expect_true(all(values$explicit))
  ordinary = render_roads(
    roads,
    layer = NULL,
    bridge = NULL,
    merge = FALSE,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  expect_null(attr(ordinary, "profile_diagnostics"))
  expect_equal(max(ordinary[[2]][, 2]), 0, tolerance = 1e-6)
})

test_that("isolated bridges span a terrain depression without a crossing road", {
  skip_if_not_installed("osqp")
  height = bridge_column_scene(valley = TRUE)
  on.exit(rgl::close3d())
  road = bridge_column_crossing()[1, ]
  road$bridge = TRUE
  draped = render_roads(
    road,
    bridge = NULL,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  bridge = render_roads(
    road,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  expect_lt(min(draped[[1]][, 2]), 1)
  expect_gt(min(bridge[[1]][, 2]), 9)
  expect_gt(attr(bridge, "profile_diagnostics")$active_fragment_count, 0)
  # Bridge status still seeds a span when a producer explicitly assigns layer 0.
  road$layer = 0
  explicit = render_roads(
    road,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    parallel = FALSE
  )
  expect_gt(min(explicit[[1]][, 2]), 9)
  expect_gt(attr(explicit, "profile_diagnostics")$active_fragment_count, 0)
})

test_that("false and missing bridge flags do not activate structure fragments", {
  roads = bridge_column_crossing()
  roads$bridge = c(FALSE, NA)
  roads$layer = NA_real_
  prepared = prepare_render_road_layer_features(roads, "layer")
  topology = build_render_road_layer_topology(prepared)
  expect_length(topology$prospective_solve_fragment_id, 0)
  expect_equal(prepared$fragments$render_road_layer, c(0, 0))
  expect_identical(
    is_truthy_render_road_osm_tag(c(
      "no",
      "false",
      "0",
      "",
      NA,
      "yes",
      "viaduct"
    )),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
})


test_that("bridge-only forks solve both surface approaches at a shared height", {
  skip_if_not_installed("osqp")
  height = bridge_column_scene()
  on.exit(rgl::close3d())
  # Neither branch is the bridge's named continuation. Both must nevertheless
  # join its elevated endpoint, while the interior crossing stays underneath.
  roads = sf::st_sf(
    bridge = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    name = c("main", "main", "left ramp", "right ramp", "underpass"),
    highway = c(
      "motorway",
      "motorway",
      "motorway_link",
      "motorway_link",
      "primary"
    ),
    geometry = sf::st_sfc(
      lapply(
        list(
          rbind(c(1, 50), c(20, 50)),
          rbind(c(20, 50), c(60, 50)),
          rbind(c(60, 50), c(90, 70)),
          rbind(c(60, 50), c(90, 30)),
          rbind(c(40, 1), c(40, 99))
        ),
        sf::st_linestring
      ),
      crs = 3857
    )
  )
  paths = render_roads(
    roads,
    heightmap = height,
    zscale = 1,
    vertical_exaggeration = 1,
    lane_texture = TRUE,
    parallel = FALSE
  )
  diagnostics = attr(paths, "profile_diagnostics")
  expect_equal(diagnostics$active_fragment_count, 5)
  expect_true(diagnostics$engineering_audit_passed)
  bridge_end = tail(paths[[2]][, 2], 1)
  expect_gt(bridge_end, 1)
  expect_equal(paths[[3]][1, 2], bridge_end, tolerance = 1e-6)
  expect_equal(paths[[4]][1, 2], bridge_end, tolerance = 1e-6)
  expect_equal(tail(paths[[1]][, 2], 1), paths[[2]][1, 2], tolerance = 1e-6)
  expect_lt(max(abs(paths[[5]][, 2])), 1e-6)
})
