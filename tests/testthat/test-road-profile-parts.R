test_that("clipped road parts retain source identity through solving and grading", {
  skip_if_not_installed("osqp")
  raster = terra::rast(
    nrows = 51,
    ncols = 51,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100,
    crs = "EPSG:3857"
  )
  terra::values(raster) = 0
  on.exit(rgl::close3d())
  loop = rbind(
    c(20, 50),
    c(30, 70),
    c(50, 70),
    c(60, 50),
    c(50, 30),
    c(30, 30),
    c(20, 50),
    c(10, 50)
  )
  # The connector is outside the scene: its two visible parts must not be joined.
  clipped = rbind(c(10, 30), c(120, 30), c(120, 70), c(10, 70))
  multipart = list(rbind(c(10, 20), c(40, 20)), rbind(c(60, 80), c(90, 80)))
  cases = list(
    sf::st_linestring(loop),
    sf::st_linestring(clipped),
    sf::st_multilinestring(multipart)
  )
  for (geometry in cases) {
    for (grading in c(FALSE, TRUE)) {
      plot_3d(
        constant_shade(raster),
        raster,
        solid = FALSE,
        shadow = FALSE,
        water = FALSE,
        window_size = c(200, 200)
      )
      roads = sf::st_sf(
        bridge = TRUE,
        geometry = sf::st_sfc(geometry, crs = 3857)
      )
      original = roads
      paths = render_roads(roads, terrain_grading = grading, parallel = FALSE)
      expect_length(paths, 2)
      expect_identical(roads, original)
      members = attr(paths, "mesh_topology")$path_members
      expect_identical(members$road_path_id, 1:2)
      expect_identical(members$render_road_fragment_id, 1:2)
      expect_identical(members$render_road_feature_id, c(1L, 1L))
      expect_identical(members$mapping_status, rep("mapped", 2))
      expect_true(all(vapply(paths, function(x) all(is.finite(x)), logical(1))))
      expect_equal(attr(paths, "profile_diagnostics")$active_fragment_count, 2)
      # Compare footprint, including part direction, against the non-solving path.
      raw = render_roads(
        roads,
        bridge = NULL,
        layer = NULL,
        merge = FALSE,
        densify = FALSE,
        parallel = FALSE
      )
      for (i in 1:2) {
        expect_equal(
          paths[[i]][c(1, nrow(paths[[i]])), c(1, 3)],
          raw[[i]][c(1, nrow(raw[[i]])), c(1, 3)],
          tolerance = 1e-8
        )
      }
    }
  }
})

test_that("DC profile convergence retains strict constraint bounds", {
  skip_if_not_installed("osqp")
  fixture = readRDS(test_path("fixtures", "road-profile-dc-convergence.rds"))
  problem = compile_render_road_profile_problem(
    fixture$specification,
    fixture$context
  )
  solution = solve_render_road_profile_problem(
    problem,
    maximum_iterations = 100000
  )
  expect_s3_class(solution, "render_road_profile_solution")
  expect_true(solution$engineering_audit$passed)
  expect_lte(solution$engineering_audit$maximum_violation, 0.05)
  # These are the original physical bounds, not relaxed to make OSQP converge.
  expect_identical(solution$problem$settings, problem$settings)
})
