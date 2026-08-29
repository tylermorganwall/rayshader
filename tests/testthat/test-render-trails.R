test_that("volcano_trails contains the bundled public OSM walking paths", {
  skip_if_not_installed("sf")

  expect_s3_class(volcano_trails, "sf")
  expect_equal(nrow(volcano_trails), 160L)
  expect_true(sf::st_crs(volcano_trails) == sf::st_crs(27200))
  expect_true(all(sf::st_geometry_type(volcano_trails) == "LINESTRING"))
  expect_setequal(
    unique(volcano_trails$highway),
    c("footway", "path", "pedestrian", "steps")
  )
  expect_false(any(
    volcano_trails$access %in% c("private", "customers", "no"),
    na.rm = TRUE
  ))
  expect_true(any(volcano_trails$name == "Puhi Huia Road", na.rm = TRUE))

  trail_bbox = sf::st_bbox(volcano_trails)
  expect_gte(unname(trail_bbox[["xmin"]]), 2667400)
  expect_lte(unname(trail_bbox[["xmax"]]), 2668010)
  expect_gte(unname(trail_bbox[["ymin"]]), 6478700)
  expect_lte(unname(trail_bbox[["ymax"]]), 6479570)
})

test_that("render_trails builds independent solid terrain-following meshes", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("rayrender")

  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  height_raster = terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = rep(seq(9000, 0, length.out = 10), each = 10)
  trails = sf::st_sf(
    trail_id = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(
        c(20, 20, 50, 80),
        ncol = 2,
        byrow = TRUE
      )),
      sf::st_linestring(matrix(
        c(50, 80, 80, 20),
        ncol = 2,
        byrow = TRUE
      )),
      crs = 3857
    )
  )

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    windowsize = c(200, 200)
  ))
  trail_coords = render_trails(
    trails,
    color = "grey50",
    width = 2,
    width_units = "meters",
    densify = FALSE,
    height = 0.1,
    parallel = FALSE
  )

  expect_length(trail_coords, 2L)
  expect_equal(
    attr(trail_coords, "path_members")$trail_path_id,
    1:2
  )
  expect_false(any(
    c(
      "maximum_grade",
      "continuation_grade_tolerance",
      "merge"
    ) %in%
      names(formals(render_trails))
  ))

  trail_ids = get_ids_with_labels(typeval = "trail_path")
  expect_equal(nrow(trail_ids), 2L)
  expect_equal(
    nrow(get_ids_with_labels(typeval = "trail_mesh_preview")),
    0L
  )
  expect_equal(
    vapply(
      trail_ids$id,
      function(id) rgl::material3d("alpha", id = id),
      numeric(1)
    ),
    rep(1, 2L)
  )
  cached_meshes = get_scene_trail_meshes()
  expect_length(cached_meshes, 2L)
  expected_width = 2 / get_scene_geographic_aspect()$mean_cell_meters
  for (mesh in cached_meshes) {
    expect_s3_class(mesh, "mesh3d")
    expect_gt(ncol(mesh$it), 0L)
    specification = attr(mesh, "render_trail_mesh_specification")
    expect_equal(specification$color, "grey50")
    expect_equal(specification$width, expected_width, tolerance = 1e-8)
    expect_equal(specification$height, 0.1)
    expect_true(specification$rgl_id %in% trail_ids$id)
    expect_null(attr(mesh, "render_stream_mesh_specification"))
  }

  trail_models = make_render_highquality_cached_trail_meshes(
    cached_meshes,
    bbox_center = c(0, 0, 0)
  )
  expect_length(trail_models, 2L)

  high_quality_scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    parallel = FALSE
  )
  expect_equal(sum(high_quality_scene$shape == "mesh3d"), 2L)

  expect_no_condition(render_trails(
    trails,
    color = "grey50",
    width = 2,
    width_units = "meters",
    densify = FALSE,
    height = 0.1,
    preview = "mesh",
    parallel = FALSE
  ))
  mesh_preview_ids = get_ids_with_labels(typeval = "trail_mesh_preview")
  expect_equal(nrow(mesh_preview_ids), 2L)
  expect_true(all(vapply(
    mesh_preview_ids$id,
    function(id) {
      identical(rgl::material3d("specular", id = id), "#000000") &&
        identical(rgl::material3d("shininess", id = id), 0)
    },
    logical(1)
  )))
  expect_true(all(vapply(
    get_ids_with_labels(typeval = "trail_path")$id,
    function(id) identical(rgl::material3d("alpha", id = id), 0),
    logical(1)
  )))
})

test_that("render_trails clear-only calls remove previews and cached meshes", {
  skip_if_not_installed("rayrender")

  local_rgl_use_null()
  rgl::open3d()
  on.exit(rgl::close3d(), add = TRUE)
  on.exit(cache_scene_trail_meshes(NULL), add = TRUE)

  trail_path_id = rgl::points3d(0, 0, 0, tag = "trail_path")
  trail_mesh_id = rgl::points3d(0, 0, 0, tag = "trail_mesh_preview")
  cache_scene_trail_meshes(list(list(test = TRUE)))

  expect_no_condition(render_trails(clear_previous = TRUE))
  expect_false(trail_path_id %in% rgl::ids3d()$id)
  expect_false(trail_mesh_id %in% rgl::ids3d()$id)
  expect_null(get_scene_trail_meshes())
})
