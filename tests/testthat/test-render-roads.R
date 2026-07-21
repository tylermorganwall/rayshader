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

test_that("colored generated lane textures preserve the road color", {
  roadcolor = "#536878"
  texture_file = make_road_lane_texture(
    roadcolor = roadcolor,
    lanes = 2,
    size = 128
  )
  texture = png::readPNG(texture_file)

  expect_equal(
    unname(texture[128, 32, ]),
    as.vector(col2rgb(roadcolor)) / 255,
    tolerance = 1 / 255
  )

  stripe_color = "#d6ad3d"
  striped_texture_file = make_road_lane_texture(
    lanes = 6,
    lane_color = stripe_color,
    centerline_color = stripe_color,
    size = 128
  )
  striped_texture = png::readPNG(striped_texture_file)
  divider_columns = round(
    calculate_road_lane_marking_positions(6)$dividers * 127
  ) +
    1L
  expect_equal(
    striped_texture[1, divider_columns, ],
    array(
      rep(
        as.vector(col2rgb(stripe_color)) / 255,
        each = length(divider_columns)
      ),
      dim = c(length(divider_columns), 3)
    ),
    tolerance = 1 / 255
  )
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

test_that("road offsets use local quadratic transitions", {
  long_road = list(cbind(
    c(0, 25, 50, 100, 150, 175, 200),
    0,
    0
  ))
  long_road_offset = offset_render_road_path_coords(
    coord_list = long_road,
    offset = 10,
    transition_length = 50
  )[[1]]

  expect_equal(
    long_road_offset[
      match(c(0, 25, 50, 100, 150, 175, 200), long_road_offset[, 1]),
      2
    ],
    c(0, 7.5, 10, 10, 10, 7.5, 0)
  )

  short_road = list(cbind(c(0, 25, 50, 75, 100), 0, 0))
  short_road_offset = offset_render_road_path_coords(
    coord_list = short_road,
    offset = 10,
    transition_length = 80
  )[[1]]

  expect_equal(
    short_road_offset[
      match(c(0, 25, 50, 75, 100), short_road_offset[, 1]),
      2
    ],
    c(0, 7.5, 10, 7.5, 0)
  )
  uneven_terrain = list(cbind(
    c(0, 25, 50, 75, 100),
    c(0, 50, 20, 100, 0),
    0
  ))
  uneven_terrain_offset = offset_render_road_path_coords(
    coord_list = uneven_terrain,
    offset = 10,
    transition_length = 25
  )[[1]]

  expect_equal(
    uneven_terrain_offset[
      match(c(0, 25, 50, 75, 100), uneven_terrain_offset[, 1]),
      2
    ],
    c(0, 30, 30, 30, 0)
  )

  two_point_road = list(matrix(
    c(0, 0, 0, 100, 4, 0),
    ncol = 3,
    byrow = TRUE
  ))
  two_point_road_offset = offset_render_road_path_coords(
    coord_list = two_point_road,
    offset = 10,
    transition_length = 80
  )[[1]]

  expect_equal(
    two_point_road_offset[c(1, nrow(two_point_road_offset)), 2],
    c(0, 4)
  )
  expect_equal(max(two_point_road_offset[, 2]), 12)
  expect_equal(
    offset_render_road_path_coords(
      coord_list = short_road,
      offset = 10,
      transition_length = 0
    )[[1]][, 2],
    rep(10, 5)
  )
})

maximum_test_road_grade = function(coords, zscale = 1) {
  horizontal = sqrt(rowSums(
    (coords[-1L, c(1, 3), drop = FALSE] -
      coords[-nrow(coords), c(1, 3), drop = FALSE])^2
  ))
  max(abs(diff(coords[, 2]) * zscale / horizontal), na.rm = TRUE)
}

maximum_test_road_grade_change = function(coords, zscale = 1) {
  horizontal = sqrt(rowSums(
    (coords[-1L, c(1, 3), drop = FALSE] -
      coords[-nrow(coords), c(1, 3), drop = FALSE])^2
  ))
  grade = diff(coords[, 2]) * zscale / horizontal
  if (length(grade) < 2L) {
    return(0)
  }
  max(abs(diff(grade)), na.rm = TRUE)
}

test_that("road layers use the lower road height at each intersection", {
  skip_if_not_installed("sf")

  base_road = matrix(
    c(0, 0, 50, 100, 0, 50),
    ncol = 3,
    byrow = TRUE
  )
  upper_road = matrix(
    c(50, 0, 0, 50, 40, 50, 50, 0, 100),
    ncol = 3,
    byrow = TRUE
  )
  layered = elevate_render_road_layer_coords(
    coord_list = list(base_road, upper_road),
    layer = c(0, 1),
    layer_explicit = c(FALSE, TRUE),
    layer_spacing = 10
  )

  expect_equal(layered[[1]], base_road)
  expect_equal(max(layered[[2]][, 2]), 40)
  expect_gte(
    interpolate_render_road_path_height(layered[[2]], 50),
    interpolate_render_road_path_height(layered[[1]], 50) + 10
  )
  layered_distance = calculate_road_path_cumulative_distance(layered[[2]])
  upper_terrain = vapply(
    layered_distance,
    function(distance) {
      interpolate_render_road_path_height(upper_road, distance)
    },
    numeric(1)
  )
  expect_true(all(layered[[2]][, 2] >= upper_terrain))
  expect_lte(maximum_test_road_grade(layered[[2]]), 0.07 + 1e-8)
  expect_equal(attr(layered, "terrain_following"), c(TRUE, FALSE))

  dense_layers = elevate_render_road_layer_coords(
    coord_list = list(base_road, upper_road),
    layer = c(3, 9),
    layer_explicit = c(TRUE, TRUE),
    layer_spacing = 4
  )
  expect_equal(max(dense_layers[[1]][, 2]), 0)
  expect_equal(max(dense_layers[[2]][, 2]), 40)

  explicit_heights = elevate_render_road_layer_coords(
    coord_list = list(base_road, upper_road),
    layer = c(0, 1),
    layer_explicit = c(FALSE, TRUE),
    layer_height = c(NA, 12)
  )
  expect_equal(max(explicit_heights[[2]][, 2]), 40)
})

test_that("layer graph low-pass preserves terrain floors through joins", {
  skip_if_not_installed("sf")

  lower_road = matrix(
    c(0, 0, 50, 100, 0, 50),
    ncol = 3,
    byrow = TRUE
  )
  upper_first = matrix(
    c(50, 0, 0, 50, 8, 25, 50, 0, 50),
    ncol = 3,
    byrow = TRUE
  )
  upper_second = matrix(
    c(50, 0, 50, 50, 6, 75, 50, 0, 100),
    ncol = 3,
    byrow = TRUE
  )
  terrain = list(lower_road, upper_first, upper_second)
  layered = elevate_render_road_layer_coords(
    coord_list = terrain,
    layer = c(0, 1, 1),
    layer_explicit = c(FALSE, TRUE, TRUE),
    layer_spacing = 5
  )

  for (path in 2:3) {
    layered_distance = calculate_road_path_cumulative_distance(layered[[path]])
    terrain_distance = calculate_road_path_cumulative_distance(terrain[[path]])
    terrain_height = stats::approx(
      terrain_distance,
      terrain[[path]][, 2],
      xout = layered_distance,
      rule = 2
    )$y
    expect_true(all(layered[[path]][, 2] >= terrain_height))
    expect_lte(maximum_test_road_grade(layered[[path]]), 0.07 + 1e-8)
    expect_lte(
      maximum_test_road_grade_change(layered[[path]]),
      0.002 + 1e-8
    )
  }
  first_grade = diff(tail(layered[[2]][, 2], 2)) /
    sqrt(sum(diff(tail(layered[[2]][, c(1, 3), drop = FALSE], 2))^2))
  second_grade = diff(head(layered[[3]][, 2], 2)) /
    sqrt(sum(diff(head(layered[[3]][, c(1, 3), drop = FALSE], 2))^2))
  expect_equal(tail(layered[[2]][, 2], 1), layered[[3]][1, 2])
  expect_lte(abs(first_grade - second_grade), 0.002 + 1e-8)
})

test_that("positive road layers ignore terrain below their endpoint baseline", {
  skip_if_not_installed("sf")

  lower_road = matrix(
    c(0, 0, 50, 100, 0, 50),
    ncol = 3,
    byrow = TRUE
  )
  upper_road = matrix(
    c(
      50,
      10,
      0,
      50,
      -20,
      25,
      50,
      -40,
      50,
      50,
      -10,
      75,
      50,
      20,
      100
    ),
    ncol = 3,
    byrow = TRUE
  )
  layered = elevate_render_road_layer_coords(
    coord_list = list(lower_road, upper_road),
    layer = c(0, 1),
    layer_explicit = c(FALSE, TRUE),
    layer_spacing = 5
  )

  endpoint_height = layered[[2]][c(1L, nrow(layered[[2]])), 2]
  endpoint_baseline = endpoint_height[[1L]] +
    diff(endpoint_height) * layered[[2]][, 3] / 100
  expect_true(all(layered[[2]][, 2] >= endpoint_baseline))
  expect_gte(
    layered[[2]][layered[[2]][, 3] == 50, 2],
    mean(endpoint_height)
  )
  expect_lte(maximum_test_road_grade(layered[[2]]), 0.07 + 1e-8)

  high_lower_road = lower_road
  high_lower_road[, 2] = 20
  uplifted = elevate_render_road_layer_coords(
    coord_list = list(high_lower_road, upper_road),
    layer = c(0, 1),
    layer_explicit = c(FALSE, TRUE),
    layer_spacing = 5
  )
  uplifted_baseline = 10 + uplifted[[2]][, 3] / 10
  expect_true(all(uplifted[[2]][, 2] >= uplifted_baseline))
  expect_equal(uplifted[[2]][uplifted[[2]][, 3] == 50, 2], 25)

  negative_layered = elevate_render_road_layer_coords(
    coord_list = list(lower_road, upper_road),
    layer = c(-2, -1),
    layer_explicit = c(TRUE, TRUE),
    layer_spacing = 5
  )
  expect_equal(
    negative_layered[[2]][negative_layered[[2]][, 3] == 50, 2],
    5
  )
  expect_lt(min(negative_layered[[2]][, 2]), 10)
})

test_that("layer transitions propagate over every adjoining endpoint branch", {
  skip_if_not_installed("sf")

  lower_road = matrix(
    c(50, 0, 0, 50, 0, 100),
    ncol = 3,
    byrow = TRUE
  )
  left_ground = matrix(
    c(0, 0, 50, 25, 10, 50),
    ncol = 3,
    byrow = TRUE
  )
  bridge_left = matrix(
    c(25, 10, 50, 50, 15, 50),
    ncol = 3,
    byrow = TRUE
  )
  bridge_right = matrix(
    c(50, 15, 50, 75, 20, 50),
    ncol = 3,
    byrow = TRUE
  )
  right_ground = matrix(
    c(75, 20, 50, 100, 0, 50),
    ncol = 3,
    byrow = TRUE
  )
  perpendicular_ground = matrix(
    c(25, 10, 50, 25, 0, 75),
    ncol = 3,
    byrow = TRUE
  )
  layered = elevate_render_road_layer_coords(
    coord_list = list(
      lower_road,
      left_ground,
      bridge_left,
      bridge_right,
      right_ground,
      perpendicular_ground
    ),
    layer = c(0, 0, 1, 1, 0, 0),
    layer_explicit = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    layer_spacing = 5
  )

  expect_equal(tail(layered[[2]][, 2], 1), layered[[3]][1, 2])
  expect_equal(tail(layered[[3]][, 2], 1), layered[[4]][1, 2])
  expect_equal(tail(layered[[4]][, 2], 1), layered[[5]][1, 2])
  expect_equal(layered[[6]][1, 2], layered[[3]][1, 2])
  expect_true(all(
    vapply(
      layered[2:6],
      maximum_test_road_grade,
      numeric(1)
    ) <=
      0.07 + 1e-8
  ))
  expect_equal(
    attr(layered, "terrain_following"),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("branched endpoint graphs cluster every endpoint within tolerance", {
  paths = list(
    matrix(c(-10, 0, 0, 0, 0, 0), ncol = 3, byrow = TRUE),
    matrix(c(0.0008, 0, 0, 10, 0, 0), ncol = 3, byrow = TRUE),
    matrix(c(0.0004, 0, 0, 0, 0, 10), ncol = 3, byrow = TRUE)
  )
  graph = build_render_road_endpoint_graph(
    coord_list = paths,
    layer = c(1, 1, 1),
    layer_explicit = c(TRUE, TRUE, TRUE)
  )

  expect_length(unique(graph$endpoint_node[c(2, 3, 5)]), 1L)
  expect_equal(nrow(graph$same_layer_edges), 3L)
})

test_that("near-endpoint crossings propagate clearance without a short drop", {
  skip_if_not_installed("sf")

  lower_road = matrix(
    c(0, 0, 1, 100, 0, 1),
    ncol = 3,
    byrow = TRUE
  )
  upper_road = matrix(
    c(50, 0, 0, 50, -100, 50, 50, 0, 100),
    ncol = 3,
    byrow = TRUE
  )
  layered = elevate_render_road_layer_coords(
    coord_list = list(lower_road, upper_road),
    layer = c(0, 1),
    layer_explicit = c(FALSE, TRUE),
    layer_spacing = 5
  )

  expect_true(all(is.finite(layered[[2]])))
  expect_lte(maximum_test_road_grade(layered[[2]]), 0.07 + 1e-8)
  expect_gte(
    interpolate_render_road_path_height(layered[[2]], 1),
    interpolate_render_road_path_height(layered[[1]], 50) + 5
  )
  expect_gte(
    min(layered[[2]][, 2]),
    min(layered[[2]][c(1L, nrow(layered[[2]])), 2])
  )
})

test_that("degenerate paths do not poison a layered endpoint graph", {
  skip_if_not_installed("sf")

  lower_road = matrix(c(0, 0, 50, 100, 0, 50), ncol = 3, byrow = TRUE)
  upper_road = matrix(c(50, 0, 0, 50, 0, 100), ncol = 3, byrow = TRUE)
  degenerate = matrix(
    c(10, 0, 10, 10, NaN, 10, 10, 0, 10),
    ncol = 3,
    byrow = TRUE
  )

  expect_no_condition(
    layered <- elevate_render_road_layer_coords(
      coord_list = list(lower_road, upper_road, degenerate),
      layer = c(0, 1, 0),
      layer_explicit = c(FALSE, TRUE, FALSE),
      layer_spacing = 5
    )
  )
  expect_true(all(vapply(
    layered,
    function(coords) all(is.finite(coords)),
    logical(1)
  )))
  expect_lte(maximum_test_road_grade(layered[[2]]), 0.07 + 1e-8)
})

test_that("higher layers sample the completed lower-layer curve", {
  skip_if_not_installed("sf")

  base_road = matrix(
    c(0, 0, 25, 100, 0, 25),
    ncol = 3,
    byrow = TRUE
  )
  middle_road = matrix(
    c(50, 0, 0, 50, 0, 25, 50, 0, 75, 50, 0, 100),
    ncol = 3,
    byrow = TRUE
  )
  top_road = matrix(
    c(0, 0, 75, 100, 0, 75),
    ncol = 3,
    byrow = TRUE
  )
  layered = elevate_render_road_layer_coords(
    coord_list = list(base_road, middle_road, top_road),
    layer = c(0, 1, 2),
    layer_explicit = c(FALSE, TRUE, TRUE),
    layer_spacing = 5
  )

  middle_at_base_crossing = interpolate_render_road_path_height(
    layered[[2]],
    25
  )
  base_at_middle_crossing = interpolate_render_road_path_height(
    layered[[1]],
    50
  )
  middle_at_top_crossing = interpolate_render_road_path_height(
    layered[[2]],
    75
  )
  top_at_middle_crossing = interpolate_render_road_path_height(
    layered[[3]],
    50
  )
  expect_gte(middle_at_base_crossing, base_at_middle_crossing + 5)
  expect_gte(top_at_middle_crossing, middle_at_top_crossing + 5)
  expect_lte(maximum_test_road_grade(layered[[2]]), 0.07 + 1e-8)
  expect_lte(maximum_test_road_grade(layered[[3]]), 0.07 + 1e-8)
})

test_that("multiple crossings create multiple exact quadratic anchors", {
  skip_if_not_installed("sf")

  lower_road = matrix(
    c(
      0,
      0,
      50,
      25,
      2,
      50,
      75,
      8,
      50,
      100,
      10,
      50
    ),
    ncol = 3,
    byrow = TRUE
  )
  upper_road = matrix(
    c(
      0,
      0,
      0,
      25,
      0,
      50,
      50,
      0,
      100,
      75,
      0,
      50,
      100,
      0,
      0
    ),
    ncol = 3,
    byrow = TRUE
  )
  layered = elevate_render_road_layer_coords(
    coord_list = list(lower_road, upper_road),
    layer = c(0, 1),
    layer_explicit = c(FALSE, TRUE),
    layer_spacing = 5
  )
  upper = layered[[2]]
  upper_distance = calculate_road_path_cumulative_distance(upper_road)

  expect_gte(
    interpolate_render_road_path_height(upper, upper_distance[[2L]]),
    7
  )
  expect_gte(
    interpolate_render_road_path_height(upper, upper_distance[[4L]]),
    13
  )
  expect_lte(maximum_test_road_grade(upper), 0.07 + 1e-8)
  expect_true(all(is.finite(upper)))
})

test_that("road layer groups can contain multiple intersections", {
  skip_if_not_installed("sf")

  upper_road = matrix(
    c(0, 0, 50, 50, 3, 50, 100, 6, 50),
    ncol = 3,
    byrow = TRUE
  )
  lower_left = matrix(
    c(25, 0, 0, 25, 0, 100),
    ncol = 3,
    byrow = TRUE
  )
  lower_right = matrix(
    c(75, 0, 0, 75, 0, 100),
    ncol = 3,
    byrow = TRUE
  )
  layered = elevate_render_road_layer_coords(
    coord_list = list(upper_road, lower_left, lower_right),
    layer = c(1, 0, 0),
    layer_explicit = c(TRUE, FALSE, FALSE),
    layer_spacing = 10
  )

  upper = layered[[1]]
  expect_true(all(
    upper[match(c(25, 75), upper[, 1]), 2] >= c(10, 10)
  ))
  expect_lte(maximum_test_road_grade(upper), 0.07 + 1e-8)
  expect_equal(attr(layered, "terrain_following"), c(FALSE, TRUE, TRUE))

  overlapping = list(
    matrix(c(0, 0, 0, 100, 0, 0), ncol = 3, byrow = TRUE),
    matrix(c(25, 0, 0, 75, 0, 0), ncol = 3, byrow = TRUE)
  )
  overlap_intersection = find_render_road_layer_intersections(
    coord_list = overlapping,
    layer_explicit = c(FALSE, TRUE)
  )
  expect_equal(nrow(overlap_intersection), 1)
  expect_equal(overlap_intersection$distance_a, 50)
  expect_equal(overlap_intersection$distance_b, 25)

  overlapping_layered = elevate_render_road_layer_coords(
    coord_list = overlapping,
    layer = c(0, 1),
    layer_explicit = c(FALSE, TRUE),
    layer_spacing = 5
  )
  expect_equal(max(overlapping_layered[[2]][, 2]), 5)
  expect_lte(
    maximum_test_road_grade(overlapping_layered[[2]]),
    0.07 + 1e-8
  )
})

test_that("road layer intersections ignore adjoining endpoints", {
  skip_if_not_installed("sf")

  adjoining = list(
    matrix(c(0, 0, 0, 50, 0, 0), ncol = 3, byrow = TRUE),
    matrix(c(50, 0, 0, 100, 0, 0), ncol = 3, byrow = TRUE)
  )
  intersections = find_render_road_layer_intersections(
    coord_list = adjoining,
    layer_explicit = c(TRUE, TRUE)
  )
  expect_equal(nrow(intersections), 0)

  separate = list(
    adjoining[[1]],
    matrix(c(0, 0, 10, 50, 0, 10), ncol = 3, byrow = TRUE)
  )
  processed = elevate_render_road_layer_coords(
    coord_list = separate,
    layer = c(0, 1),
    layer_explicit = c(FALSE, TRUE),
    layer_spacing = 5
  )
  expect_equal(processed[[1]], separate[[1]])
  expect_equal(
    processed[[2]][c(1L, nrow(processed[[2]])), c(1, 3)],
    separate[[2]][, c(1, 3)]
  )
  expect_equal(processed[[2]][, 2], rep(0, nrow(processed[[2]])))
  expect_equal(attr(processed, "terrain_following"), c(TRUE, FALSE))
})

test_that("road point collapse preserves the final endpoint", {
  points = matrix(
    c(0, 0, 0, 1, 0, 0, 1.0005, 0, 0),
    ncol = 3,
    byrow = TRUE
  )
  collapsed = collapse_render_highquality_road_path_points(points)

  expect_equal(collapsed[nrow(collapsed), ], points[nrow(points), ])
  expect_true(all(diff(collapsed[, 1]) > 1e-3))
})

test_that("render_roads accepts layer and feature height columns", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 11,
    ncols = 11,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  roads = sf::st_sf(
    osm_layer = c(NA_character_, "1"),
    bridge_height = c(NA_real_, 7),
    lane_count = c(2L, 4L),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(1, 5, 9, 5), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(5, 1, 5, 9), ncol = 2, byrow = TRUE)),
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
  road_coords = render_roads(
    roads,
    heightmap = height_raster,
    zscale = 1,
    vertical_exaggeration = 1,
    layer = osm_layer,
    layer_height = bridge_height,
    lanes = lane_count,
    lane_texture = TRUE
  )

  expect_equal(
    vapply(road_coords, function(x) max(x[, 2]), numeric(1)),
    c(0, 7)
  )
  expect_lte(maximum_test_road_grade(road_coords[[2]]), 0.07 + 1e-8)
  road_path_ids = get_ids_with_labels(typeval = "road_path")
  terrain_following = vapply(
    road_path_ids$id,
    function(id) get_render_road_path_info(id)$terrain_following,
    logical(1)
  )
  expect_equal(terrain_following, c(TRUE, FALSE))
  road_info = lapply(road_path_ids$id, get_render_road_path_info)
  expect_equal(vapply(road_info, `[[`, numeric(1), "road_width"), c(12, 18))
  expect_false(identical(
    road_info[[1]]$texture_file,
    road_info[[2]]$texture_file
  ))
})

test_that("elevated road meshes preserve absolute profiles", {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")

  points = matrix(
    c(3, 1, 3, 6, 4, 6, 9, 1, 9),
    ncol = 3,
    byrow = TRUE
  )
  sloped_heightmap = outer(seq_len(11), seq_len(11), `+`) * 10
  road_mesh = make_render_highquality_road_path_mesh(
    points = points,
    bbox_center = c(0, 0, 0),
    width = 2,
    heightmap = sloped_heightmap,
    zscale = 1,
    material = rayrender::diffuse(color = "white"),
    terrain_following = FALSE
  )
  vertices = road_mesh$shape_info[[1]]$mesh_info[[1]]$vertices

  expect_true(all(is.finite(vertices)))
  expect_equal(range(vertices[, 2]), c(1, 4.11), tolerance = 1e-8)
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
    points = matrix(
      c(0, 0, 0, 0, 0, 0, 5, 0, 0),
      ncol = 3,
      byrow = TRUE
    ),
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
  texture_triangles = mesh_info$indices + 1L
  texture_triangle_area = apply(texture_triangles, 1, function(index) {
    texture_vertices = mesh_info$texcoords[index, , drop = FALSE]
    abs(
      (texture_vertices[2, 1] - texture_vertices[1, 1]) *
        (texture_vertices[3, 2] - texture_vertices[1, 2]) -
        (texture_vertices[2, 2] - texture_vertices[1, 2]) *
          (texture_vertices[3, 1] - texture_vertices[1, 1])
    )
  })
  expect_true(all(texture_triangle_area > 0))

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

test_that("road mesh sweep avoids invalid triangles at tightly spaced bends", {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")

  texture_file = make_road_lane_texture()
  road_mesh = make_render_highquality_road_path_mesh(
    points = matrix(
      c(
        -5,
        0,
        0,
        0,
        0,
        0,
        0.001,
        0,
        0.1,
        5,
        0,
        0.2
      ),
      ncol = 3,
      byrow = TRUE
    ),
    bbox_center = c(0, 0, 0),
    width = 4,
    heightmap = matrix(0, nrow = 40, ncol = 40),
    zscale = 1,
    material = rayrender::diffuse(
      color = "white",
      image_texture = texture_file,
      image_repeat = 1
    ),
    texture_file = texture_file,
    texture_length = 13
  )
  mesh_info = road_mesh$shape_info[[1]]$mesh_info[[1]]
  triangles = mesh_info$indices + 1L
  triangle_a = mesh_info$vertices[triangles[, 2], , drop = FALSE] -
    mesh_info$vertices[triangles[, 1], , drop = FALSE]
  triangle_b = mesh_info$vertices[triangles[, 3], , drop = FALSE] -
    mesh_info$vertices[triangles[, 1], , drop = FALSE]
  face_normals = row_cross(triangle_a, triangle_b)
  face_lengths = sqrt(rowSums(face_normals^2))
  face_normals = face_normals / face_lengths
  normal_dots = cbind(
    rowSums(face_normals * mesh_info$normals[triangles[, 1], , drop = FALSE]),
    rowSums(face_normals * mesh_info$normals[triangles[, 2], , drop = FALSE]),
    rowSums(face_normals * mesh_info$normals[triangles[, 3], , drop = FALSE])
  )
  texture_a = mesh_info$texcoords[triangles[, 2], , drop = FALSE] -
    mesh_info$texcoords[triangles[, 1], , drop = FALSE]
  texture_b = mesh_info$texcoords[triangles[, 3], , drop = FALSE] -
    mesh_info$texcoords[triangles[, 1], , drop = FALSE]
  texture_area = abs(
    texture_a[, 1] * texture_b[, 2] - texture_a[, 2] * texture_b[, 1]
  )

  expect_true(all(is.finite(mesh_info$vertices)))
  expect_true(all(is.finite(mesh_info$normals)))
  expect_true(all(is.finite(mesh_info$texcoords)))
  expect_true(all(face_lengths > 1e-10))
  expect_true(all(texture_area > 1e-8))
  expect_true(all(normal_dots > 0))
})

test_that("road mesh drops sub-millimeter densification fragments", {
  texture_world_scale = c(8.3025458175032, 8.30626221228124)
  points = matrix(
    c(
      171.7856399224056,
      2.627375012955684,
      -65.75296854852728,
      171.7318851113765,
      2.625636889675848,
      -65.73188511137654,
      171.7318725585938,
      2.625636577606201,
      -65.73188018798828,
      171.5,
      2.62,
      -65.6
    ),
    ncol = 3,
    byrow = TRUE
  )
  fragment_world_delta = (points[3, c(1, 3)] - points[2, c(1, 3)]) *
    texture_world_scale

  expect_lt(sqrt(sum(fragment_world_delta^2)), 1e-3)
  expect_equal(
    collapse_render_highquality_road_path_points(
      points,
      texture_world_scale = texture_world_scale
    ),
    points[c(1, 2, 4), , drop = FALSE]
  )
})

make_test_render_road_topology_lines = function(
  coordinates,
  layer,
  osm_id = seq_along(coordinates),
  ref = NA_character_,
  name = NA_character_,
  highway = "primary"
) {
  count = length(coordinates)
  recycle = function(value) rep(value, length.out = count)
  sf::st_sf(
    layer = recycle(layer),
    osm_id = recycle(osm_id),
    ref = recycle(ref),
    name = recycle(name),
    highway = recycle(highway),
    geometry = sf::st_sfc(
      lapply(coordinates, sf::st_linestring),
      crs = 32615
    )
  )
}

test_that("road topology preserves parent features and boundary endpoints", {
  skip_if_not_installed("sf")
  skip_if_not_installed("igraph")

  roads = sf::st_sf(
    layer = 1,
    osm_id = "parent-way",
    ref = "A 1",
    name = "Preserved Road",
    highway = "primary",
    geometry = sf::st_sfc(
      sf::st_multilinestring(list(
        rbind(c(0, 0), c(0, 0), c(10, 0)),
        rbind(c(10, 0), c(20, 0))
      )),
      crs = 32615
    )
  )
  boundary = sf::st_as_sfc(sf::st_bbox(
    c(xmin = 0, ymin = -10, xmax = 20, ymax = 10),
    crs = sf::st_crs(32615)
  ))
  prepared = prepare_render_road_layer_features(
    roads,
    layer_column = "layer",
    boundary = boundary,
    boundary_tolerance = 0.01
  )

  expect_equal(nrow(prepared$source_fragments), 2L)
  expect_equal(prepared$source_fragments$render_road_feature_id, c(1L, 1L))
  expect_equal(
    prepared$source_fragments$render_road_way_id,
    rep("parent-way", 2)
  )
  expect_equal(prepared$source_fragments$render_road_ref, rep("A 1", 2))
  expect_equal(
    nrow(unclass(sf::st_geometry(prepared$source_fragments)[[1L]])),
    2L
  )
  expect_equal(sum(prepared$endpoints$supplied_boundary), 2L)
})

test_that("road topology retains local and repeated exact crossings", {
  skip_if_not_installed("sf")
  skip_if_not_installed("igraph")

  three_way = make_test_render_road_topology_lines(
    list(
      rbind(c(-20, 0), c(20, 0)),
      rbind(c(0, -20), c(0, 20)),
      rbind(c(-15, -15), c(15, 15))
    ),
    layer = c(0, 1, 2)
  )
  three_way_topology = build_render_road_layer_topology(
    prepare_render_road_layer_features(three_way, "layer")
  )

  expect_equal(nrow(three_way_topology$crossings), 1L)
  expect_equal(three_way_topology$crossings$participant_count, 3L)
  expect_equal(nrow(three_way_topology$crossing_pairs), 3L)
  expect_equal(three_way_topology$crossing_participants$local_order, 1:3)
  expect_equal(nrow(three_way_topology$junctions), 0L)

  repeated = make_test_render_road_topology_lines(
    list(
      rbind(c(-20, 0), c(20, 0)),
      rbind(
        c(-15, -10),
        c(-10, 10),
        c(-5, -10),
        c(0, 10),
        c(5, -10),
        c(10, 10),
        c(15, -10)
      )
    ),
    layer = c(0, 1)
  )
  repeated_topology = build_render_road_layer_topology(
    prepare_render_road_layer_features(repeated, "layer")
  )
  expect_equal(nrow(repeated_topology$crossings), 6L)
  expect_equal(nrow(repeated_topology$crossing_pairs), 6L)

  overlap = make_test_render_road_topology_lines(
    list(
      rbind(c(-10, 0), c(10, 0)),
      rbind(c(0, 0), c(20, 0))
    ),
    layer = c(0, 1)
  )
  overlap_topology = build_render_road_layer_topology(
    prepare_render_road_layer_features(overlap, "layer")
  )
  expect_equal(nrow(overlap_topology$overlaps), 1L)
  expect_true(overlap_topology$overlaps$layer_relationship)
})

test_that("road topology keeps branch continuation choices conservative", {
  skip_if_not_installed("sf")
  skip_if_not_installed("igraph")

  branch = make_test_render_road_topology_lines(
    list(
      rbind(c(-20, 0), c(0, 0)),
      rbind(c(0, 0), c(20, 0)),
      rbind(c(0, 0), c(0, 15)),
      rbind(c(-15, -20), c(0, -20)),
      rbind(c(0, -20), c(14.1, -14.9)),
      rbind(c(0, -20), c(14.1, -25.1))
    ),
    layer = 0,
    osm_id = c("main", "main", "branch", "fork-in", "fork-up", "fork-down"),
    ref = c("US 1", "US 1", NA, NA, NA, NA),
    name = c("Main Street", "Main Street", "Branch Street", NA, NA, NA),
    highway = c(rep("primary", 3), rep("residential", 3))
  )
  branch_topology = build_render_road_layer_topology(
    prepare_render_road_layer_features(branch, "layer")
  )

  expect_equal(nrow(branch_topology$junctions), 2L)
  expect_equal(branch_topology$junctions$participant_count, c(3L, 3L))
  expect_equal(nrow(branch_topology$crossings), 0L)
  expect_equal(nrow(branch_topology$selected_continuations), 1L)
  expect_equal(nrow(branch_topology$ambiguous_continuations), 2L)
  expect_equal(
    sort(c(
      branch_topology$selected_continuations$fragment_a,
      branch_topology$selected_continuations$fragment_b
    )),
    c(1L, 2L)
  )

  fragmented = make_test_render_road_topology_lines(
    list(
      rbind(c(-20, 0), c(-0.05, 0)),
      rbind(c(0.05, 0), c(20, 0)),
      rbind(c(0, 0.15), c(0, 10))
    ),
    layer = c(1, 1, 0),
    osm_id = c("fragment-a", "fragment-b", "unrelated"),
    ref = c("I 10", "I 10", NA),
    name = c("Interstate 10", "Interstate 10", "Side Road"),
    highway = c("motorway", "motorway", "residential")
  )
  fragmented_topology = build_render_road_layer_topology(
    prepare_render_road_layer_features(fragmented, "layer")
  )

  expect_equal(nrow(fragmented_topology$selected_continuations), 1L)
  expect_equal(
    fragmented_topology$selected_continuations$endpoint_distance,
    0.1,
    tolerance = 1e-8
  )
  expect_false(any(fragmented_topology$selected_continuations$fragment_a == 3L))
  expect_false(any(fragmented_topology$selected_continuations$fragment_b == 3L))
  expect_equal(
    length(unique(
      fragmented_topology$components$solve_component_id
    )),
    2L
  )
})

test_that("plot_render_road_topology exports plots and diagnostics", {
  skip_if_not_installed("sf")
  skip_if_not_installed("igraph")

  roads = make_test_render_road_topology_lines(
    list(
      rbind(c(-10, 0), c(10, 0)),
      rbind(c(0, -10), c(0, 10))
    ),
    layer = c(0, 1)
  )
  output = tempfile(fileext = ".png")
  on.exit(unlink(output), add = TRUE)
  topology = plot_render_road_topology(
    roads,
    layer = layer,
    views = "overview",
    filename = output,
    width = 600,
    height = 400,
    res = 72
  )

  expect_true(file.exists(output))
  expect_s3_class(topology, "render_road_topology")
  expect_s3_class(topology$graph, "igraph")
  expect_equal(nrow(topology$crossings), 1L)
  expect_equal(topology$plot$filename, output)
})
