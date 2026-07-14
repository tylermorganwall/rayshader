test_that("render_roads caches road metadata by rgl id", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 5,
    ncols = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 5,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  roads = sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(1, 2.5, 4, 2.5), ncol = 2, byrow = TRUE)),
      crs = 3857
    )
  )

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  road_coords = NULL
  expect_no_condition(
    road_coords <- render_roads(
      roads,
      heightmap = height_raster,
      roadcolor = "#303030",
      width = 0.5,
      lane_texture = TRUE
    )
  )

  road_path_ids = get_ids_with_labels(typeval = "road_path")
  expect_equal(nrow(road_path_ids), 1)
  expect_equal(rgl::material3d("lwd", id = road_path_ids$id[[1]]), 0.5)
  expect_equal(
    unique(unlist(lapply(
      road_coords,
      function(coord) unique(coord[, 2])
    ))),
    0
  )

  road_info = get_render_road_path_info(road_path_ids$id[[1]])
  expect_true(file.exists(road_info$texture_file))
  expect_equal(road_info$texture_mapping, "auto")
  expect_equal(road_info$texture_length, 13)
  expect_equal(road_info$texture_repeats, road_info$road_length / 13)

  scene = render_highquality(return_scene = TRUE, light = FALSE)
  expect_true(any(vapply(
    scene$material,
    function(material) {
      identical(material$image, road_info$texture_file)
    },
    logical(1)
  )))
  expect_no_condition(rayrender:::process_scene(scene))
})

test_that("render_roads fits lane texture repeats to each road length", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 7,
    ncols = 7,
    xmin = 0,
    xmax = 6,
    ymin = 0,
    ymax = 6,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  roads = sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(1, 2, 4, 2), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 4, 6, 4), ncol = 2, byrow = TRUE)),
      crs = 3857
    )
  )

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_no_condition(render_roads(
    roads,
    heightmap = height_raster,
    lane_texture = TRUE,
    lane_texture_length = 2,
    merge = FALSE
  ))

  road_path_ids = get_ids_with_labels(typeval = "road_path")
  expect_equal(nrow(road_path_ids), 2)
  expect_equal(
    sort(vapply(
      road_path_ids$id,
      function(id) rgl::material3d("lwd", id = id),
      numeric(1)
    )),
    c(12, 12)
  )
  road_info = lapply(road_path_ids$id, get_render_road_path_info)
  expect_equal(
    sort(vapply(road_info, `[[`, numeric(1), "road_length")),
    c(3, 5),
    tolerance = 1e-8
  )
  expect_equal(
    sort(vapply(road_info, `[[`, numeric(1), "texture_length")),
    c(2, 2),
    tolerance = 1e-8
  )
  expect_equal(
    sort(vapply(road_info, `[[`, numeric(1), "texture_repeats")),
    c(3 / 2, 5 / 2),
    tolerance = 1e-8
  )
})

test_that("road mesh texture coordinates repeat along the path", {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")

  fixed_mapping = resolve_road_lane_texture_mapping(
    coord_list = list(matrix(c(0, 0, 0, 5, 0, 0), ncol = 3, byrow = TRUE)),
    lane_texture_length = 2,
    lane_texture_mapping = "fixed"
  )
  expect_equal(fixed_mapping$texture_length, 2)
  expect_true(is.na(fixed_mapping$texture_repeats))

  default_mapping = resolve_road_lane_texture_mapping(
    coord_list = list(matrix(c(0, 0, 0, 26, 0, 0), ncol = 3, byrow = TRUE)),
    lane_texture_length = resolve_road_lane_texture_length(
      NULL,
      lane_dash_length = 3,
      lane_gap_length = 10
    ),
    lane_texture_mapping = "auto"
  )
  expect_equal(default_mapping$texture_length, 13)
  expect_equal(default_mapping$texture_repeats, 2)
  expect_equal(
    resolve_road_lane_dash_fraction(
      NULL,
      lane_dash_length = 3,
      lane_gap_length = 10
    ),
    3 / 13
  )
  expect_equal(
    resolve_render_road_width(
      road_width = NULL,
      lanes = 2,
      lane_width = 3,
      texture_world_scale = c(2, 2)
    ),
    6
  )
  expect_equal(
    resolve_render_road_width(
      road_width = NULL,
      lanes = 3,
      lane_width = 3,
      texture_world_scale = c(1, 1)
    ),
    15
  )
  expect_equal(
    calculate_road_lane_marking_positions(2),
    list(edge_lines = c(1 / 8, 7 / 8), dividers = 1 / 2)
  )
  expect_equal(
    calculate_road_lane_marking_positions(3),
    list(edge_lines = c(1 / 10, 9 / 10), dividers = c(11 / 30, 19 / 30))
  )
  expect_equal(
    make_render_highquality_road_path_polygon(),
    matrix(
      c(
        -0.5,
        0,
        0.5,
        0,
        0.5,
        0.11,
        -0.5,
        0.11
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
  expect_gt(
    max(make_render_highquality_road_path_polygon()[, 2]),
    max(make_render_highquality_water_path_polygon()[, 2])
  )

  texture_file = make_road_lane_texture()
  road_mesh = make_render_highquality_road_path_mesh(
    points = matrix(c(0, 0, 0, 5, 0, 0), ncol = 3, byrow = TRUE),
    bbox_center = c(0, 0, 0),
    width = 1,
    heightmap = matrix(0, nrow = 7, ncol = 7),
    zscale = 1,
    material = rayrender::diffuse(
      color = "white",
      image_texture = texture_file,
      image_repeat = 1
    ),
    texture_file = texture_file,
    texture_length = 1
  )

  mesh_info = road_mesh$shape_info[[1]]$mesh_info[[1]]
  expect_equal(range(mesh_info$vertices[, 2]), c(0, 0.11), tolerance = 1e-8)
  expect_equal(range(mesh_info$texcoords[, 1]), c(0, 1))
  expect_gt(max(mesh_info$texcoords[, 2]), 1)

  repeated_road_mesh = make_render_highquality_road_path_mesh(
    points = matrix(c(0, 0, 0, 5, 0, 0), ncol = 3, byrow = TRUE),
    bbox_center = c(0, 0, 0),
    width = 1,
    heightmap = matrix(0, nrow = 7, ncol = 7),
    zscale = 1,
    material = rayrender::diffuse(
      color = "white",
      image_texture = texture_file,
      image_repeat = 1
    ),
    texture_file = texture_file,
    texture_length = 2,
    texture_repeats = 3
  )
  repeated_mesh_info = repeated_road_mesh$shape_info[[1]]$mesh_info[[1]]
  expect_equal(max(repeated_mesh_info$texcoords[, 2]), 3)
})
