reset_render_people_test_caches = function() {
  reset_scene_context(
    clear_scene_metadata = TRUE,
    clear_scene_cache = TRUE
  )
  rayshader:::clear_hillshade_cache()
  invisible(NULL)
}

test_that("person_obj() resolves every bundled pose and sex", {
  poses = c(
    "clapping",
    "ironman",
    "slipping",
    "stack",
    "standing",
    "stop",
    "stop_one_hand",
    "stretch",
    "walking",
    "rocky",
    "yelling"
  )
  models = expand.grid(
    pose = poses,
    sex = c("male", "female"),
    stringsAsFactors = FALSE
  )

  paths = vapply(
    seq_len(nrow(models)),
    function(i) {
      rayshader:::person_obj(models$pose[i], models$sex[i])
    },
    character(1)
  )

  expect_true(all(file.exists(paths)))
  expect_equal(tools::file_ext(paths), rep("txt", length(paths)))
  expect_equal(length(unique(paths)), 2 * length(poses))
  expect_true(all(grepl("raypeople", paths, fixed = TRUE)))
  expect_identical(
    basename(paths),
    paste0(
      "person_",
      ifelse(models$sex == "male", "man", "woman"),
      "_",
      models$pose,
      ".txt"
    )
  )
  expect_identical(
    rayshader:::resolve_person_pose(c("standing", "walking")),
    c("standing", "walking")
  )
  expect_error(
    rayshader:::person_obj(c("standing", "walking")),
    "single pose name"
  )
  expect_error(rayshader:::person_obj("sitting"), "should be one of")
  expect_error(rayshader:::person_obj(sex = "robot"), "should be one of")
})

test_that("render_people() groups line arguments and orders point coordinates", {
  render_people_args = names(formals(render_people))
  expect_identical(render_people_args[[1]], "location")
  expect_identical(
    render_people_args[4:8],
    c(
      "line",
      "line_spacing",
      "line_terrain_spacing",
      "line_pattern",
      "line_align_terrain"
    )
  )
  expect_lt(match("x", render_people_args), match("y", render_people_args))
})

test_that("person patterns repeat in placement order", {
  expect_equal(
    rayshader:::resolve_person_pattern("MF", 6),
    rep(c("male", "female"), 3)
  )
  expect_equal(
    rayshader:::resolve_person_pattern("FMF", 8),
    c("female", "male", "female", "female", "male", "female", "female", "male")
  )
  expect_equal(
    rayshader:::resolve_person_pattern(NULL, 3, "female"),
    rep("female", 3)
  )
  expect_error(rayshader:::resolve_person_pattern("MX", 2), "only M and F")

  expect_equal(
    rayshader:::resolve_person_pattern_colors(
      c("red", "blue"),
      line_pattern = "MF",
      n = 5
    ),
    c("red", "blue", "red", "blue", "red")
  )
  expect_equal(
    rayshader:::resolve_person_pattern_colors(
      rainbow(5),
      line_pattern = "MF",
      n = 5
    ),
    rainbow(5)
  )
  expect_equal(
    rayshader:::resolve_person_pattern_colors(
      c(1, 0, 0),
      line_pattern = "FMF",
      n = 6
    ),
    c(1, 0, 0)
  )
  expect_error(
    rayshader:::resolve_person_pattern_colors(
      c("red", "blue", "green"),
      line_pattern = "MF",
      n = 6
    ),
    "one value per entry"
  )
})

test_that("render_people() vectorizes pose across point placements", {
  calls = list()
  testthat::local_mocked_bindings(
    render_obj = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "rayshader"
  )

  person_angles = matrix(seq_len(12), ncol = 3, byrow = TRUE)
  expect_no_condition(render_people(
    x = 1:4,
    y = rep(5, 4),
    pose = c("standing", "walking", "standing", "rocky"),
    sex = "female",
    altitude = 11:14,
    color = c("red", "blue", "green", "black"),
    offset = 21:24,
    angle = person_angles
  ))

  expect_length(calls, 3)
  expect_match(calls[[1]]$filename, "person_woman_standing\\.txt$")
  expect_match(calls[[2]]$filename, "person_woman_walking\\.txt$")
  expect_match(calls[[3]]$filename, "person_woman_rocky\\.txt$")
  expect_equal(calls[[1]]$x, c(1, 3))
  expect_equal(calls[[2]]$x, 2)
  expect_equal(calls[[3]]$x, 4)
  expect_equal(calls[[1]]$altitude, c(11, 13))
  expect_equal(calls[[1]]$color, c("red", "green"))
  expect_equal(calls[[1]]$offset, c(21, 23))
  expect_equal(calls[[1]]$angle, person_angles[c(1, 3), , drop = FALSE])
  expect_true(all(vapply(
    calls,
    function(call) {
      identical(call$clear_previous, FALSE)
    },
    logical(1)
  )))

  calls = list()
  expect_no_condition(render_people(
    x = 5,
    y = 6,
    altitude = 1:3,
    pose = c("standing", "walking", "rocky")
  ))
  expect_length(calls, 3)
  expect_true(all(vapply(calls, function(call) call$x == 5, logical(1))))
  expect_true(all(vapply(calls, function(call) call$y == 6, logical(1))))

  expect_error(
    render_people(
      x = 1:3,
      y = 1:3,
      pose = c("standing", "walking")
    ),
    "one value or one value per person"
  )
})

test_that("render_people() vectorizes pose for spatial point locations", {
  skip_if_not_installed("sf")
  reset_render_people_test_caches()
  withr::defer(reset_render_people_test_caches())

  locations = sf::st_sfc(
    sf::st_point(c(-77.00, 38.90)),
    sf::st_point(c(-76.99, 38.91)),
    sf::st_point(c(-76.98, 38.92)),
    crs = 4326
  )
  calls = list()
  testthat::local_mocked_bindings(
    render_obj = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "rayshader"
  )

  expect_no_condition(render_people(
    location = locations,
    pose = c("standing", "walking", "standing")
  ))

  expect_length(calls, 2)
  expect_equal(calls[[1]]$x, c(-77.00, -76.98))
  expect_equal(calls[[1]]$y, c(38.90, 38.92))
  expect_equal(calls[[2]]$x, -76.99)
  expect_equal(calls[[2]]$y, 38.91)
  expect_false(calls[[1]]$transform_scene)
  expect_null(calls[[1]]$crs)
})

test_that("person line sampling follows outgoing segments", {
  skip_if_not_installed("sf")

  line = sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(matrix(
      c(0, 0, 4, 0, 4, 4),
      ncol = 2,
      byrow = TRUE
    )))
  )
  samples = rayshader:::sample_person_line_geometry(line, line_spacing = 2)

  expect_equal(samples$x, c(0, 2, 4, 4, 4))
  expect_equal(samples$y, c(0, 0, 0, 2, 4))
  expect_equal(samples$dx, c(1, 1, 0, 0, 0))
  expect_equal(samples$dy, c(0, 0, 1, 1, 1))
  expect_equal(
    rayshader:::person_line_yaw(c(1, 0, -1, 0), c(0, 1, 0, -1)),
    c(-90, -180, 90, 0)
  )
})

test_that("person line spacing follows the rendered terrain surface", {
  skip_if_not_installed("sf")
  reset_render_people_test_caches()
  withr::defer(reset_render_people_test_caches())

  line = sf::st_sfc(sf::st_linestring(matrix(
    c(0, 5, 10, 5),
    ncol = 2,
    byrow = TRUE
  )))
  heightmap = outer(
    0:10,
    0:10,
    function(row, column) row
  )
  extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10)

  terrain_samples = rayshader:::sample_person_line(
    line = line,
    line_spacing = 2,
    extent = extent,
    heightmap = heightmap,
    zscale = 1,
    caller = "test"
  )
  expect_equal(
    terrain_samples$x,
    seq(0, 14, by = 2) / sqrt(2),
    tolerance = 1e-7
  )
  expect_equal(
    sqrt(diff(terrain_samples$x)^2 + diff(terrain_samples$x)^2),
    rep(2, 7),
    tolerance = 1e-7
  )

  scaled_samples = rayshader:::sample_person_line(
    line = line,
    line_spacing = 2,
    extent = extent,
    heightmap = heightmap,
    zscale = 2,
    caller = "test"
  )
  expect_equal(
    scaled_samples$x,
    seq.int(0, 11) * 2 / sqrt(5),
    tolerance = 1e-7
  )

  planar_samples = rayshader:::sample_person_line(
    line = line,
    line_spacing = 2,
    line_terrain_spacing = FALSE,
    extent = extent,
    heightmap = heightmap,
    zscale = 1,
    caller = "test"
  )
  expect_equal(planar_samples$x, seq(0, 10, by = 2))
})

test_that("person terrain orientation preserves up and line-forward vectors", {
  heightmap = outer(
    0:10,
    0:10,
    function(row, column) row + 2 * column
  )
  angles = rayshader:::person_terrain_line_angles(
    x = 5,
    y = 5,
    line_angle = -90,
    extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    heightmap = heightmap,
    zscale = 1
  )
  rotation = rayvertex:::generate_rot_matrix(
    angles[1, ] * pi / 180,
    1:3
  )
  expected_up = c(-1, 1, -2) / sqrt(6)
  horizontal_forward = c(1, 0, 0)
  expected_forward = c(
    horizontal_forward[1],
    -sum(horizontal_forward[c(1, 3)] * expected_up[c(1, 3)]) /
      expected_up[2],
    horizontal_forward[3]
  )
  expected_forward = expected_forward / sqrt(sum(expected_forward^2))

  expect_equal(rotation[2, ], expected_up, tolerance = 1e-7)
  expect_equal(rotation[3, ], expected_forward, tolerance = 1e-7)
  expect_equal(
    rotation[3, c(1, 3)] / sqrt(sum(rotation[3, c(1, 3)]^2)),
    horizontal_forward[c(1, 3)],
    tolerance = 1e-7
  )
  expect_false(isTRUE(all.equal(angles[1, ], c(0, -90, 0))))

  line_angles = seq(-180, 135, by = 45)
  circle_angles = rayshader:::person_terrain_line_angles(
    x = rep(5, length(line_angles)),
    y = rep(5, length(line_angles)),
    line_angle = line_angles,
    extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    heightmap = heightmap,
    zscale = 1
  )
  circle_rotations = lapply(
    seq_along(line_angles),
    function(index) {
      rayvertex:::generate_rot_matrix(
        circle_angles[index, ] * pi / 180,
        1:3
      )
    }
  )
  circle_up = do.call(rbind, lapply(circle_rotations, function(x) x[2, ]))
  circle_forward = do.call(
    rbind,
    lapply(circle_rotations, function(x) x[3, c(1, 3)])
  )
  circle_forward = circle_forward / sqrt(rowSums(circle_forward^2))
  expected_horizontal = cbind(
    -sin(line_angles * pi / 180),
    cos(line_angles * pi / 180)
  )
  expect_equal(
    circle_up,
    matrix(expected_up, nrow = length(line_angles), ncol = 3, byrow = TRUE),
    tolerance = 1e-7
  )
  expect_equal(circle_forward, expected_horizontal, tolerance = 1e-7)

  flat_angles = rayshader:::person_terrain_line_angles(
    x = 5,
    y = 5,
    line_angle = -90,
    extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    heightmap = matrix(0, nrow = 11, ncol = 11),
    zscale = 1
  )
  expect_equal(flat_angles[1, ], c(0, -90, 0), tolerance = 1e-7)
})

test_that("person line spacing is measured in meters across CRSs", {
  skip_if_not_installed("sf")
  reset_render_people_test_caches()
  withr::defer(reset_render_people_test_caches())

  expect_equal(
    rayshader:::resolve_person_line_spacing(2, sf::st_crs(32611)),
    2
  )
  expect_equal(
    rayshader:::resolve_person_line_spacing(2, sf::st_crs(2230)),
    6.5616667,
    tolerance = 1e-6
  )

  geographic_line = sf::st_sfc(
    sf::st_linestring(matrix(
      c(-77, 38.9, -76.9999, 38.9),
      ncol = 2,
      byrow = TRUE
    )),
    crs = 4326
  )
  samples = rayshader:::sample_person_line(
    line = geographic_line,
    line_spacing = 2,
    caller = "test"
  )
  expect_gte(length(samples$x), 4)
  expect_true(all(diff(samples$x) > 0))
})

test_that("render_people() patterns and orients models along a line", {
  skip_if_not_installed("sf")
  reset_render_people_test_caches()
  withr::defer(reset_render_people_test_caches())

  line = sf::st_sfc(sf::st_linestring(matrix(
    c(0, 0, 10, 0),
    ncol = 2,
    byrow = TRUE
  )))
  calls = list()
  testthat::local_mocked_bindings(
    render_obj = function(...) {
      calls[[length(calls) + 1]] <<- list(...)
      invisible(NULL)
    },
    .package = "rayshader"
  )

  expect_no_condition(render_people(
    line = line,
    pose = "stretch",
    line_pattern = "FMF",
    line_spacing = 2,
    color = c("red", "blue", "green"),
    angle = c(0, 10, 0),
    clear_previous = TRUE
  ))

  expect_length(calls, 2)
  expect_match(calls[[1]]$filename, "person_woman_stretch\\.txt$")
  expect_match(calls[[2]]$filename, "person_man_stretch\\.txt$")
  expect_equal(calls[[1]]$x, c(0, 4, 6, 10))
  expect_equal(calls[[2]]$x, c(2, 8))
  expect_equal(calls[[1]]$y, rep(0, 4))
  expect_equal(calls[[2]]$y, rep(0, 2))
  expect_equal(calls[[1]]$angle[, 2], rep(-80, 4))
  expect_equal(calls[[2]]$angle[, 2], rep(-80, 2))
  expect_equal(calls[[1]]$color, c("red", "green", "red", "green"))
  expect_equal(calls[[2]]$color, c("blue", "blue"))
  expect_false(calls[[1]]$clear_previous)
  expect_false(calls[[2]]$clear_previous)
  expect_false(calls[[1]]$transform_scene)

  calls = list()
  expect_no_condition(render_people(
    location = line,
    pose = c("standing", "walking", "standing"),
    sex = "female",
    line_spacing = 5
  ))
  expect_length(calls, 2)
  expect_match(calls[[1]]$filename, "person_woman_standing\\.txt$")
  expect_equal(calls[[1]]$x, c(0, 10))
  expect_match(calls[[2]]$filename, "person_woman_walking\\.txt$")
  expect_equal(calls[[2]]$x, 5)
})

test_that("render_people() aligns and spaces line placements on terrain", {
  skip_if_not_installed("sf")
  reset_render_people_test_caches()
  withr::defer(reset_render_people_test_caches())

  line = sf::st_sfc(sf::st_linestring(matrix(
    c(0, 5, 10, 5),
    ncol = 2,
    byrow = TRUE
  )))
  heightmap = outer(
    0:10,
    0:10,
    function(row, column) row + column
  )
  extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  calls = list()
  testthat::local_mocked_bindings(
    render_obj = function(...) {
      calls[[length(calls) + 1]] <<- list(...)
      invisible(NULL)
    },
    .package = "rayshader"
  )

  render_people(
    line = line,
    line_spacing = 5,
    extent = extent,
    heightmap = heightmap,
    zscale = 1
  )
  expect_length(calls, 1)
  expect_equal(calls[[1]]$x, c(0, 5, 10) / sqrt(2), tolerance = 1e-7)
  expect_true(any(abs(calls[[1]]$angle[, c(1, 3)]) > 1e-7))

  calls = list()
  render_people(
    line = line,
    line_spacing = 5,
    extent = extent,
    heightmap = heightmap,
    zscale = 1,
    line_terrain_spacing = FALSE,
    line_align_terrain = FALSE
  )
  expect_equal(calls[[1]]$x, c(0, 5, 10))
  expect_equal(calls[[1]]$angle[, 1], rep(0, 3))
  expect_equal(calls[[1]]$angle[, 2], rep(-90, 3))
  expect_equal(calls[[1]]$angle[, 3], rep(0, 3))
})

test_that("render_people() validates line placement arguments", {
  skip_if_not_installed("sf")
  line = sf::st_sfc(sf::st_linestring(matrix(
    c(0, 0, 10, 0),
    ncol = 2,
    byrow = TRUE
  )))

  expect_error(
    render_people(line = line, line_spacing = 0),
    "single positive"
  )
  expect_error(
    render_people(line = line, line_align_terrain = NA),
    "single logical"
  )
  expect_error(
    render_people(line = line, line_terrain_spacing = NA),
    "single logical"
  )
  expect_error(render_people(line = line, x = 1), "cannot be combined")
  expect_error(
    render_people(line_pattern = "MF"),
    "requires line geometry"
  )
  expect_error(
    render_people(line = line, location = line),
    "only one of `line` or `location`"
  )
})

test_that("render_people() preserves the native person scale", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    extent = extent,
    zscale = 10,
    vertical_exaggeration = 2,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_people(
    x = 15,
    y = 10,
    extent = extent,
    altitude = 0,
    clear_previous = TRUE,
    lit = FALSE
  ))

  person_ids = get_ids_with_labels()
  person_id = person_ids$id[person_ids$tag == "objperson"][1]
  person_vertices = rgl::rgl.attrib(person_id, "vertices")
  standing_mesh = rayvertex::read_obj(rayshader:::person_obj("standing"))
  native_vertices = standing_mesh$vertices[[1]]
  native_height = diff(range(native_vertices[, 2]))
  native_center = apply(
    native_vertices,
    2,
    function(values) mean(range(values))
  )
  placement = transform_into_heightmap_coords(
    extent = extent,
    heightmap = heightmap,
    lat = 10,
    long = 15,
    altitude = 0,
    offset = 0,
    zscale = 10 / 2
  )[1, ]
  rendered_center = apply(
    person_vertices,
    2,
    function(values) mean(range(values))
  )

  expect_equal(
    diff(range(person_vertices[, 2])),
    native_height / (10 / 2),
    tolerance = 1e-6
  )
  expect_lt(
    max(abs(rendered_center - (placement + native_center / (10 / 2)))),
    1e-6
  )
})

test_that("render_people() selects sexes and clears only prior people", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    extent = extent,
    zscale = 1,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  render_people(
    x = 8,
    y = 10,
    extent = extent,
    pose = "standing",
    sex = "male",
    altitude = 0,
    clear_previous = TRUE,
    lit = FALSE
  )
  render_people(
    x = 12,
    y = 10,
    extent = extent,
    pose = "walking",
    sex = "female",
    altitude = 0,
    lit = FALSE
  )
  expect_equal(sum(get_ids_with_labels()$tag == "objperson"), 2)

  render_people(
    x = 10,
    y = 10,
    extent = extent,
    pose = "clapping",
    sex = "female",
    altitude = 0,
    clear_previous = TRUE,
    lit = FALSE
  )
  expect_equal(sum(get_ids_with_labels()$tag == "objperson"), 1)

  render_people(
    x = c(8, 12),
    y = c(10, 10),
    extent = extent,
    pose = c("standing", "walking"),
    sex = "female",
    altitude = 0,
    clear_previous = TRUE,
    lit = FALSE
  )
  expect_equal(sum(get_ids_with_labels()$tag == "objperson"), 2)
})

test_that("render_people() renders vectorized poses along a line", {
  skip_if_not_installed("sf")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    extent = extent,
    zscale = 1,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))
  line = sf::st_sfc(sf::st_linestring(matrix(
    c(5, 10, 15, 10),
    ncol = 2,
    byrow = TRUE
  )))

  expect_no_condition(render_people(
    line = line,
    extent = extent,
    pose = rep(c("stretch", "standing"), 3),
    line_pattern = "MF",
    line_spacing = 2,
    line_terrain_spacing = FALSE,
    altitude = 0,
    clear_previous = TRUE,
    lit = FALSE
  ))
  expect_equal(sum(get_ids_with_labels()$tag == "objperson"), 2)
})

test_that("render_people() stacks altitude vectors at one spatial point", {
  skip_if_not_installed("sf")
  skip_if_not_installed("raster")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  washington_monument_people = sf::st_point(c(-77.035249, 38.889462)) |>
    sf::st_sfc(crs = 4326)
  monument_xy = sf::st_coordinates(
    sf::st_transform(washington_monument_people, 3857)
  )[1, ]
  elevation = suppressWarnings(raster::raster(
    nrows = 20,
    ncols = 20,
    xmn = monument_xy[1] - 1000,
    xmx = monument_xy[1] + 1000,
    ymn = monument_xy[2] - 1000,
    ymx = monument_xy[2] + 1000,
    crs = "EPSG:3857"
  ))
  raster::values(elevation) = 0
  heightmap = raster_to_matrix(elevation)
  expect_no_condition(plot_3d_test(
    sphere_shade(heightmap),
    elevation,
    zscale = 1,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  altitudes = seq(0, 500, by = 2)
  expect_no_warning(render_people(
    location = washington_monument_people,
    altitude = altitudes,
    pose = "stack",
    color = "black",
    lit = FALSE
  ))

  person_ids = get_ids_with_labels()
  person_id = person_ids$id[person_ids$tag == "objperson"][1]
  person_vertices = rgl::rgl.attrib(person_id, "vertices")
  stack_mesh = rayvertex::read_obj(rayshader:::person_obj("stack"))
  native_vertices = stack_mesh$vertices[[1]]
  placement = transform_into_heightmap_coords(
    extent = get_scene_extent(),
    heightmap = get_scene_heightmap(),
    lat = monument_xy[2],
    long = monument_xy[1],
    altitude = 0,
    zscale = get_scene_effective_zscale(),
    transform_scene = FALSE
  )[1, ]

  expect_equal(
    range(person_vertices[, 1]),
    range(native_vertices[, 1]) + placement[1],
    tolerance = 1e-6
  )
  expect_equal(
    range(person_vertices[, 3]),
    range(native_vertices[, 3]) + placement[3],
    tolerance = 1e-6
  )
  expect_equal(
    range(person_vertices[, 2]),
    c(
      min(native_vertices[, 2]) + min(altitudes),
      max(native_vertices[, 2]) + max(altitudes)
    ),
    tolerance = 1e-6
  )
})
