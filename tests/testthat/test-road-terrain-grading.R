test_that('terrain grading cuts intersected cells and clears triangle interiors', {
  terrain = matrix(0, 11, 11)
  terrain[6, 6] = 5
  vertices = rbind(c(-2, 1, -2), c(2, 1.2, -2), c(2, 1.4, 2))
  faces = matrix(1:3, nrow = 1)
  result = sample_render_road_terrain_contacts_cpp(
    vertices,
    faces,
    rep(0, 3),
    terrain,
    2,
    c(1, 1),
    TRUE
  )
  graded = attr(result, 'heightmap')
  expect_true(all(graded <= terrain))
  expect_lt(graded[6, 6], terrain[6, 6])
  expect_equal(graded[c(1, 11), ], terrain[c(1, 11), ])
  expect_equal(graded[, c(1, 11)], terrain[, c(1, 11)])
  clearance = sample_render_road_terrain_contacts_cpp(
    vertices,
    faces,
    rep(0, 3),
    graded,
    2,
    c(1, 1)
  )
  expect_gte(min(clearance$clearance), .01 - 1e-10)
  expect_equal(terrain[6, 6], 5)
  reverse = sample_render_road_terrain_contacts_cpp(
    vertices,
    faces[, 3:1, drop = FALSE],
    rep(0, 3),
    terrain,
    2,
    c(1, 1),
    TRUE
  )
  expect_equal(attr(reverse, 'heightmap'), graded)
})

test_that('grading replaces both displayed terrain and cached elevations', {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  terrain = matrix(1, 11, 11)
  plot_3d_test(constant_shade(terrain), terrain, solid = FALSE, shadow = FALSE)
  old_id = get_ids_with_labels(typeval = c('surface', 'surface_tris'))$id
  graded = terrain
  graded[5:7, 5:7] = .2
  new_id = replace_render_road_scene_terrain(graded, 1)
  expect_false(old_id %in% rgl::ids3d()$id)
  expect_true(new_id %in% rgl::ids3d()$id)
  expect_equal(get_scene_context_value('scene_heightmap'), graded)
  expect_equal(
    min(rgl::rgl.attrib(new_id, 'vertices')[, 2]),
    .2,
    tolerance = 1e-7
  )
  expect_equal(dim(rgl::rgl.attrib(new_id, 'texcoords')), c(600L, 2L))
  expect_true(all(is.finite(rgl::rgl.attrib(new_id, 'normals'))))
  geometry = generate_surface(graded, 1)
  expect_equal(
    unname(rgl::rgl.attrib(new_id, 'texcoords')),
    geometry$texcoords[as.vector(geometry$inds), ],
    tolerance = 1e-7
  )
  expect_gt(min(rgl::rgl.attrib(new_id, 'normals')[, 2]), .5)
})

test_that('ground junction contacts resist bank uplift with feasible crossings', {
  skip_if_not_installed('sf')
  skip_if_not_installed('igraph')
  skip_if_not_installed('osqp')
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  terrain = matrix(3, 121, 121)
  terrain[61, 63] = 8
  plot_3d_test(constant_shade(terrain), terrain, solid = FALSE, shadow = FALSE)
  lines = list(
    rbind(c(-50, 0), c(0, 0)),
    rbind(c(0, 0), c(50, 0)),
    rbind(c(0, 0), c(0, 50)),
    rbind(c(-25, -50), c(-25, 50))
  )
  roads = sf::st_sf(
    layer = c(0, 0, 0, 1),
    bridge = c('no', 'no', 'no', 'yes'),
    geometry = sf::st_sfc(lapply(lines, sf::st_linestring), crs = 3857)
  )
  coords = lapply(lines, function(x) cbind(x[, 1], 3, -x[, 2]))
  solved = solve_render_road_path_profiles(
    coord_list = coords,
    coord_feature = 1:4,
    coord_width = rep(4, 4),
    roads = roads,
    heightmap = terrain,
    layer_column = 'layer',
    terrain_grading = TRUE
  )
  expect_true(attr(solved, 'profile_diagnostics')$engineering_audit_passed)
  contacts = c(tail(solved[[1]][, 2], 1), solved[[2]][1, 2], solved[[3]][1, 2])
  expect_lte(max(contacts), 3 + .01)
  expect_lt(diff(range(contacts)), .001)
  expect_false(any(attr(solved, 'terrain_following')))
})

test_that('grading retains a terrain datum for an isolated bridge', {
  skip_if_not_installed('sf')
  skip_if_not_installed('igraph')
  skip_if_not_installed('osqp')
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  terrain = matrix(0, 31, 31)
  terrain[16, 17] = 3
  plot_3d_test(constant_shade(terrain), terrain, solid = FALSE, shadow = FALSE)
  roads = sf::st_sf(
    layer = 1,
    bridge = 'yes',
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(-12, 0), c(12, 0))),
      crs = 3857
    )
  )
  solved = solve_render_road_path_profiles(
    list(rbind(c(-12, 0, 0), c(12, 0, 0))),
    1L,
    4,
    roads,
    terrain,
    'layer',
    terrain_grading = TRUE
  )
  expect_true(attr(solved, 'profile_diagnostics')$engineering_audit_passed)
  expect_gte(max(solved[[1]][, 2]), 3 - .05)
})

test_that('grading skips explicit underground mesh features', {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  rgl::open3d()
  terrain = matrix(2, 11, 11)
  mesh = rgl::tmesh3d(
    t(rbind(c(-2, 0, -2), c(2, 0, 2), c(2, 0, -2))),
    matrix(1:3, nrow = 3),
    homogeneous = FALSE
  )
  id = rgl::lines3d(c(-2, 2), c(0, 0), c(-2, 2))
  attr(mesh, 'render_road_mesh_specification') = list(rgl_id = id)
  register_render_road_path_info(id, list(feature_id = 42L))
  expect_equal(
    grade_render_road_terrain(list(mesh), 42L, terrain, 1, c(1, 1)),
    terrain
  )
  expect_lt(
    min(grade_render_road_terrain(list(mesh), integer(), terrain, 1, c(1, 1))),
    0
  )
})
