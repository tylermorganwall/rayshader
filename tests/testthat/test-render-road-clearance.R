test_that("terrain contacts find interior peaks and are winding independent", {
  terrain = matrix(0, 7, 7)
  terrain[4, 4] = 5
  vertices = rbind(c(-2, 1, -2), c(2, 1, -2), c(2, 1, 2))
  contacts = sample_render_road_terrain_contacts_cpp(
    vertices,
    matrix(1:3, nrow = 1),
    c(0, 4, 4),
    terrain,
    1,
    c(1, 1)
  )
  reversed = sample_render_road_terrain_contacts_cpp(
    vertices,
    matrix(3:1, nrow = 1),
    c(0, 4, 4),
    terrain,
    1,
    c(1, 1)
  )
  # All three road vertices are above ground. The interior terrain peak is not.
  expect_equal(min(contacts$clearance), -4)
  expect_equal(min(reversed$clearance), -4)
  expect_equal(max(contacts$floor), 5)
  vertices[, 1] = vertices[, 1] * 2
  vertices[, 3] = vertices[, 3] / 2
  scaled = sample_render_road_terrain_contacts_cpp(
    vertices,
    matrix(1:3, nrow = 1),
    c(0, 4, 4),
    terrain,
    2,
    c(2, .5)
  )
  expect_equal(min(scaled$clearance), -3)
})

test_that("physical floor remains distinct from the profile fitting target", {
  skip_if_not_installed("sf")
  skip_if_not_installed("igraph")
  skip_if_not_installed("osqp")
  roads = sf::st_sf(
    layer = 1,
    bridge = "yes",
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(0, 0), c(100, 0))),
      crs = 3857
    )
  )
  topology = build_render_road_layer_topology(prepare_render_road_layer_features(
    roads,
    layer_column = "layer"
  ))
  reference = data.frame(
    distance = c(0, 40, 50, 60, 100),
    elevation = 0,
    floor = c(0, 0, 5, 0, 0)
  )
  problem = build_render_road_profile_problem(topology, list(reference))
  expect_true(all(problem$controls$terrain == 0))
  solved = solve_render_road_profile_problem(problem)
  evaluated = evaluate_render_road_profile_at(
    solved$problem,
    solved,
    topology$fragments$render_road_fragment_id[[1]],
    reference$distance
  )
  expect_gte(min(evaluated$height - reference$floor), -.05)
  expect_true(solved$engineering_audit$passed)
  expect_equal(
    solved$problem$terrain_profiles[[1]]$elevation,
    rep(0, nrow(solved$problem$terrain_profiles[[1]]))
  )
  expect_error(
    build_render_road_profile_problem(
      topology,
      list(transform(reference, floor = NA_real_))
    ),
    "finite"
  )
})

test_that("mixed road chains preserve local terrain-following runs", {
  tasks = lapply(seq_len(3), function(i) {
    task = list(
      points = rbind(c(i - 1, 0, 0), c(i, 0, 0)),
      width = 1,
      texture_world_scale = c(1, 1),
      terrain_following = i != 2,
      rgl_id = i,
      road_color = "grey",
      material = NULL,
      texture_file = NULL,
      texture_length = 1,
      texture_repeats = 1
    )
    attr(task, "mesh_topology") = list(
      mesh_chain_id = 1L,
      road_path_id = i,
      render_road_fragment_id = i,
      render_road_feature_id = i,
      member_order = i,
      orientation = 1L,
      closed = FALSE,
      road_lanes = 1L
    )
    task
  })
  chains = assemble_render_road_mesh_chain_tasks(tasks)
  expect_length(chains, 3)
  expect_identical(
    vapply(chains, `[[`, logical(1), "terrain_following"),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(chains[[1]]$points[2, ], chains[[2]]$points[1, ])
  expect_equal(chains[[2]]$points[2, ], chains[[3]]$points[1, ])
})

test_that("layered road tessellation clears an off-center terrain peak", {
  skip_if_not_installed("sf")
  skip_if_not_installed("igraph")
  skip_if_not_installed("osqp")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  terrain = matrix(0, 31, 31)
  terrain[16, 17] = 3
  plot_3d_test(constant_shade(terrain), terrain, solid = FALSE, shadow = FALSE)
  roads = sf::st_sf(
    layer = 1,
    bridge = "yes",
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(-12, 0), c(12, 0))),
      crs = 3857
    )
  )
  solved = solve_render_road_path_profiles(
    coord_list = list(rbind(c(-12, 0, 0), c(12, 0, 0))),
    coord_feature = 1L,
    coord_width = 4,
    roads = roads,
    heightmap = terrain,
    layer_column = "layer"
  )
  expect_gt(nrow(solved[[1]]), 2)
  expect_true(attr(solved, "profile_diagnostics")$engineering_audit_passed)
  mesh = make_render_highquality_road_chain_mesh(
    points = solved[[1]],
    bbox_center = c(0, 0, 0),
    width = 4,
    heightmap = terrain,
    terrain_following = FALSE,
    material = rayrender::diffuse(),
    return_mesh = TRUE
  )
  top = colSums(matrix(mesh$normals[2, mesh$it], nrow = 3)) > 1.5
  contacts = sample_render_road_terrain_contacts_cpp(
    t(mesh$vb[1:3, ]),
    t(mesh$it[, top, drop = FALSE]),
    rep(0, ncol(mesh$vb)),
    terrain,
    1,
    c(1, 1)
  )
  expect_gte(min(contacts$clearance), -.01)
})
