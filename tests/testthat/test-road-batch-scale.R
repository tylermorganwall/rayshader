test_that("batched solved roads preserve each task's elevation scale", {
  # The same flat deck is built with three scene scales. Include an ordinary
  # terrain-following road in the batch to exercise both grouping branches.
  tasks = lapply(c(1, 20, 100, 20), function(zscale) {
    list(
      points = cbind(c(-2, 2), 5 / zscale, 0),
      bbox_center = c(0, 0, 0),
      width = 0.6,
      heightmap = matrix(0, 9, 9),
      zscale = zscale,
      material = list(),
      terrain_following = FALSE,
      return_mesh = TRUE
    )
  })
  tasks[[4]]$terrain_following = TRUE
  for (parallel in c(FALSE, TRUE)) {
    prepared = prepare_render_highquality_road_chain_meshes(
      tasks,
      parallel = parallel
    )
    native = build_render_highquality_prepared_road_meshes(
      prepared,
      tasks,
      parallel = parallel
    )
    for (i in seq_along(tasks)) {
      expect_null(prepared[[i]]$error)
      mesh = finalize_render_highquality_road_chain_mesh(
        prepared[[i]]$prepared,
        native[[i]]
      )
      individual = do.call(make_render_highquality_road_chain_mesh, tasks[[i]])
      expect_equal(mesh$vb, individual$vb)
      expect_equal(diff(range(mesh$vb[2, ])) * tasks[[i]]$zscale, 0.11)
      expect_equal(max(mesh$vb[2, ]) * tasks[[i]]$zscale, 5.055)
    }
  }
})

test_that("public bridge rendering caches thin pavement at non-unit zscale", {
  skip_if_not_installed("osqp")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  raster = terra::rast(
    nrows = 101,
    ncols = 101,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100,
    crs = "EPSG:3857"
  )
  terra::values(raster) = 0
  plot_3d(
    constant_shade(raster),
    raster,
    zscale = 20,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE
  )
  roads = sf::st_sf(
    bridge = c(FALSE, TRUE),
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(5, 50), c(95, 50))),
      sf::st_linestring(rbind(c(50, 5), c(50, 95))),
      crs = 3857
    )
  )
  paths = render_roads(
    roads,
    zscale = 20,
    lane_texture = TRUE,
    parallel = FALSE
  )
  expect_identical(attr(paths, "profile_diagnostics")$solver, "sparse_qp")
  meshes = get_scene_road_meshes()
  expect_length(meshes, 2)
  for (mesh in meshes) {
    # The solver can leave a small longitudinal gradient. Compare each
    # vertical top/bottom pair instead of mistaking it for slab thickness.
    columns = split(
      seq_len(ncol(mesh$vb)),
      paste(mesh$vb[1, ], mesh$vb[3, ], sep = ":")
    )
    thickness = vapply(
      columns,
      function(index) diff(range(mesh$vb[2, index])) * 20,
      numeric(1)
    )
    expect_equal(
      unname(thickness),
      rep(0.11, length(thickness)),
      tolerance = 1e-6
    )
  }
})
