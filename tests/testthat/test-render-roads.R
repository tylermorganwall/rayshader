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
  preview_vertices = rgl::rgl.attrib(road_path_ids$id[[1]], "vertices")
  expect_equal(unname(unique(preview_vertices[, 2])), 0.01, tolerance = 1e-7)
  source_vertices = get_render_source_vertices(road_path_ids$id[[1]])
  expect_equal(unname(unique(source_vertices[, 2])), 0, tolerance = 1e-7)

  road_info = get_render_road_path_info(road_path_ids$id[[1]])
  expect_equal(road_info$rgl_preview_offset, 0.01)
  expect_true(file.exists(road_info$texture_file))
  expect_equal(road_info$lanes, 2L)
  expect_equal(road_info$width, 0.5)
  expect_equal(road_info$texture_length, 13)
  expect_equal(road_info$texture_repeats, 3 / 13)
  cached_road_meshes = get_scene_road_meshes()
  expect_length(cached_road_meshes, 1L)
  expect_s3_class(cached_road_meshes[[1L]], "mesh3d")
  expect_equal(
    attr(
      cached_road_meshes[[1L]],
      "render_road_mesh_specification"
    )$rgl_id,
    road_path_ids$id[[1L]]
  )

  scene = render_highquality(return_scene = TRUE, light = FALSE)
  expect_true(any(vapply(
    scene$material,
    function(material) {
      identical(material$image, road_info$texture_file)
    },
    logical(1)
  )))
  expect_no_condition(rayrender:::process_scene(scene))
  overridden_scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    rgl_materials = list(road_path = rayrender::metal())
  )
  overridden_road_materials = overridden_scene$material[
    overridden_scene$shape == "mesh3d"
  ]
  expect_true(any(vapply(
    overridden_road_materials,
    function(material) {
      identical(material$type, rayrender::metal()[[1L]]$type)
    },
    logical(1)
  )))

  captured = new.env(parent = emptyenv())
  captured$build_count = 0L
  captured$line_progress_totals = integer()
  original_get_render_source_vertices = get_render_source_vertices
  testthat::local_mocked_bindings(
    make_render_highquality_road_path_meshes = function(
      tasks,
      verbose = FALSE,
      parallel = FALSE
    ) {
      captured$build_count = captured$build_count + 1L
      stop("cached roads should not be rebuilt")
    },
    get_render_source_vertices = function(id) {
      if (id %in% road_path_ids$id) {
        stop("cached road paths should not be prepared")
      }
      original_get_render_source_vertices(id)
    },
    new_render_highquality_progress_bar = function(verbose, label, total) {
      if (identical(label, "Preparing line paths")) {
        captured$line_progress_totals = c(
          captured$line_progress_totals,
          total
        )
      }
      NULL
    },
    .package = "rayshader"
  )
  expect_no_condition(render_highquality(return_scene = TRUE, light = FALSE))
  expect_no_condition(render_highquality(
    return_scene = TRUE,
    light = FALSE,
    parallel = FALSE
  ))
  expect_equal(captured$build_count, 0L)
  expect_equal(captured$line_progress_totals, c(0L, 0L))
  expect_identical(get_scene_road_meshes(), cached_road_meshes)
})

test_that("render_roads can preview the exact cached road meshes", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = 0,
    xmax = 12,
    ymin = 0,
    ymax = 12,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  roads = sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(2, 6, 10, 6), ncol = 2, byrow = TRUE)),
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
    width = 0.75,
    preview = "mesh"
  ))

  road_path_ids = get_ids_with_labels(typeval = "road_path")
  mesh_preview_ids = get_ids_with_labels(typeval = "road_mesh_preview")
  expect_equal(nrow(road_path_ids), 1L)
  expect_gt(nrow(mesh_preview_ids), 0L)
  expect_equal(
    rgl::material3d("alpha", id = road_path_ids$id[[1L]]),
    0
  )
  expect_true(all(vapply(
    get_scene_road_meshes(),
    inherits,
    logical(1),
    "mesh3d"
  )))
  cached_mesh = get_scene_road_meshes()[[1L]]
  cached_specification = attr(
    cached_mesh,
    "render_road_mesh_specification"
  )
  expect_equal(
    cached_specification$material[[1L]]$properties[[1L]],
    convert_color("#303030", linear = TRUE),
    tolerance = 1e-10
  )
  cached_vertices = t(cached_mesh$vb[1:3, , drop = FALSE])
  preview_vertices = rgl::rgl.attrib(
    mesh_preview_ids$id[[1L]],
    "vertices"
  )
  expect_equal(
    unname(apply(preview_vertices, 2, range)),
    unname(apply(cached_vertices, 2, range)),
    tolerance = 1e-7
  )

  expect_no_condition(render_roads(
    roads,
    heightmap = height_raster,
    width = 0.75,
    offset = 1,
    preview = "mesh"
  ))
  expect_gt(nrow(get_ids_with_labels(typeval = "road_mesh_preview")), 0L)
  expect_equal(
    rgl::material3d(
      "alpha",
      id = get_ids_with_labels(typeval = "road_path")$id[[1L]]
    ),
    0
  )
})

test_that("render_roads appends, replaces, and resets cached road meshes", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = 0,
    xmax = 12,
    ymin = 0,
    ymax = 12,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  make_road = function(y) {
    sf::st_sf(
      id = 1,
      geometry = sf::st_sfc(
        sf::st_linestring(matrix(c(2, y, 10, y), ncol = 2, byrow = TRUE)),
        crs = 3857
      )
    )
  }
  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))

  expect_no_condition(render_roads(
    make_road(4),
    heightmap = height_raster,
    width = 0.75
  ))
  expect_length(get_scene_road_meshes(), 1L)

  expect_no_condition(render_roads(
    make_road(8),
    heightmap = height_raster,
    width = 0.75,
    clear_previous = FALSE
  ))
  appended_meshes = get_scene_road_meshes()
  road_path_ids = get_ids_with_labels(typeval = "road_path")$id
  expect_length(appended_meshes, 2L)
  expect_length(road_path_ids, 2L)
  expect_setequal(
    vapply(
      appended_meshes,
      function(mesh) {
        attr(mesh, "render_road_mesh_specification")$rgl_id
      },
      integer(1)
    ),
    road_path_ids
  )

  expect_no_condition(render_roads(
    make_road(6),
    heightmap = height_raster,
    width = 0.75
  ))
  expect_length(get_scene_road_meshes(), 1L)
  expect_length(get_ids_with_labels(typeval = "road_path")$id, 1L)

  reset_scene_context(
    clear_scene_metadata = FALSE,
    clear_scene_cache = TRUE
  )
  expect_null(get_scene_road_meshes())
})

test_that("non-rgl converters remove the road preview offset", {
  rgl::open3d(useNULL = TRUE)
  on.exit(rgl::close3d(), add = TRUE)
  road_geometry_id = rgl::triangles3d(
    x = c(0, 1, 0),
    y = rep(0.01, 3),
    z = c(0, 0, 1),
    color = "white",
    tag = "polygon3d"
  )
  register_render_road_path_info(
    road_geometry_id,
    list(rgl_preview_offset = 0.01)
  )
  on.exit(clear_render_road_path_info(road_geometry_id), add = TRUE)

  raymesh = convert_rgl_to_raymesh(
    save_shadow = FALSE,
    include_ids = road_geometry_id
  )
  expect_equal(
    raymesh$vertices[[1L]][, 2],
    rep(0, 3),
    tolerance = 1e-7
  )

  obj_file = tempfile(fileext = ".obj")
  save_obj(obj_file, save_texture = FALSE)
  obj_vertex_lines = grep("^v ", readLines(obj_file), value = TRUE)
  obj_vertices = do.call(
    rbind,
    lapply(strsplit(sub("^v ", "", obj_vertex_lines), " +"), as.numeric)
  )
  expect_equal(obj_vertices[, 2], rep(0, 3), tolerance = 1e-7)
})

test_that("spatial road paths transform and crop once without changing path data", {
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
  terra::values(height_raster) = seq_len(terra::ncell(height_raster))
  roads = sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(1, 1, 9, 9), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(20, 20, 30, 30), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(
        c(-2, 2, 5, 2, 12, 2, 5, 4, -2, 4),
        ncol = 2,
        byrow = TRUE
      )),
      sf::st_multilinestring(list(
        matrix(c(2, 8, 8, 8), ncol = 2, byrow = TRUE),
        matrix(c(2, 6, 8, 6), ncol = 2, byrow = TRUE)
      )),
      crs = 3857
    )
  )
  prepared = prepare_render_line_geometry(roads, merge = FALSE)
  widths = seq_len(nrow(prepared))

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  filter_calls = new.env(parent = emptyenv())
  filter_calls$count = 0L
  transform_calls = new.env(parent = emptyenv())
  transform_calls$count = 0L
  original_filter = filter_scene_sf_to_extent
  original_transform = transform_into_heightmap_coords
  testthat::local_mocked_bindings(
    filter_scene_sf_to_extent = function(...) {
      filter_calls$count = filter_calls$count + 1L
      original_filter(...)
    },
    transform_into_heightmap_coords = function(...) {
      transform_calls$count = transform_calls$count + 1L
      original_transform(...)
    },
    .package = "rayshader"
  )
  actual = suppressWarnings(render_line_coords_by_width(
    lines = prepared,
    heightmap = height_raster,
    extent = terra::ext(height_raster),
    zscale = 1,
    color = "black",
    width = widths,
    force_by_feature = TRUE
  ))

  expect_equal(filter_calls$count, 1L)
  expect_equal(transform_calls$count, 1L)
  expected_coords = list()
  expected_width = numeric()
  expected_feature_id = integer()
  expected_source_feature_id = list()
  for (feature_index in seq_len(nrow(prepared))) {
    feature_coords = suppressWarnings(render_path(
      y = prepared[feature_index, , drop = FALSE],
      extent = terra::ext(height_raster),
      zscale = 1,
      vertical_exaggeration = 1,
      heightmap = height_raster,
      offset = 0,
      linewidth = widths[[feature_index]],
      color = "black",
      return_coords = TRUE,
      tag = "water_path"
    ))
    if (!length(feature_coords)) {
      next
    }
    expected_coords = c(expected_coords, feature_coords)
    expected_width = c(
      expected_width,
      rep(widths[[feature_index]], length(feature_coords))
    )
    expected_feature_id = c(
      expected_feature_id,
      rep(
        prepared$render_line_feature_id[[feature_index]],
        length(feature_coords)
      )
    )
    expected_source_feature_id = c(
      expected_source_feature_id,
      rep(
        prepared$render_line_source_feature_id[feature_index],
        length(feature_coords)
      )
    )
  }

  expect_equal(actual$coords, expected_coords, tolerance = 0)
  expect_equal(actual$width, expected_width)
  expect_equal(actual$feature_id, expected_feature_id)
  expect_equal(actual$source_feature_id, expected_source_feature_id)
})

test_that("render_roads defers and restores rgl redraw state", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
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

  redraw_state = new.env(parent = emptyenv())
  redraw_state$during_registration = logical()
  original_register = register_render_road_path_info
  testthat::local_mocked_bindings(
    register_render_road_path_info = function(id, info) {
      redraw_state$during_registration = c(
        redraw_state$during_registration,
        rgl::par3d("skipRedraw")
      )
      original_register(id, info)
    },
    .package = "rayshader"
  )

  expect_false(rgl::par3d("skipRedraw"))
  expect_no_condition(render_roads(roads, heightmap = height_raster, width = 1))
  expect_false(rgl::par3d("skipRedraw"))

  previous_rgl_parameters = rgl::par3d(skipRedraw = TRUE)
  on.exit(rgl::par3d(previous_rgl_parameters), add = TRUE)
  expect_no_condition(render_roads(roads, heightmap = height_raster, width = 1))
  expect_true(rgl::par3d("skipRedraw"))
  expect_true(all(redraw_state$during_registration))
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
  stripe_rgb = as.vector(col2rgb(stripe_color)) / 255
  stripe_distance = apply(
    abs(sweep(striped_texture[1, , ], 2, stripe_rgb, "-")),
    1,
    max
  )
  striped_columns = which(stripe_distance <= 1 / 255)
  stripe_runs = split(
    striped_columns,
    cumsum(c(TRUE, diff(striped_columns) > 1L))
  )
  divider_columns = as.integer(round(vapply(
    stripe_runs,
    mean,
    numeric(1)
  )))
  expect_equal(divider_columns, c(27L, 46L, 65L, 83L, 102L))
  expect_equal(
    striped_texture[1, divider_columns, ],
    array(
      rep(
        stripe_rgb,
        each = length(divider_columns)
      ),
      dim = c(length(divider_columns), 3)
    ),
    tolerance = 1 / 255
  )
  centerline_distance = apply(
    abs(sweep(striped_texture[, divider_columns[[3L]], ], 2, stripe_rgb, "-")),
    1,
    max
  )
  expect_equal(
    which(centerline_distance <= 1 / 255),
    seq_len(floor(128 * 3 / 13))
  )

  colors = c(
    road = "#303030",
    lane = "#2457a6",
    centerline = "#f3b61f",
    edge = "#d45d35"
  )
  srgb_texture_file = make_road_lane_texture(
    roadcolor = colors[["road"]],
    lanes = 4,
    lane_color = colors[["lane"]],
    centerline_color = colors[["centerline"]],
    edge_line_color = colors[["edge"]],
    size = 128
  )
  srgb_texture = png::readPNG(srgb_texture_file)
  linear_texture = unclass(rayimage::ray_read_image(srgb_texture_file))
  for (color in colors) {
    expected_srgb = convert_color(color)
    expected_linear = convert_color(color, linear = TRUE)
    srgb_distance = apply(
      abs(sweep(srgb_texture, 3, expected_srgb, "-")),
      c(1, 2),
      max
    )
    linear_distance = apply(
      abs(sweep(linear_texture, 3, expected_linear, "-")),
      c(1, 2),
      max
    )
    expect_true(any(srgb_distance <= 1 / 255))
    expect_true(any(linear_distance <= 1e-7))
  }
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
    sort(vapply(road_info, `[[`, numeric(1), "texture_length")),
    c(2, 2),
    tolerance = 1e-8
  )
  expect_equal(
    sort(vapply(road_info, `[[`, numeric(1), "texture_repeats")),
    c(3 / 2, 5 / 2),
    tolerance = 1e-8
  )

  expect_no_condition(render_roads(
    roads,
    heightmap = height_raster,
    lane_texture = TRUE,
    lane_texture_length = 2,
    lane_texture_mapping = "fixed",
    merge = FALSE
  ))
  fixed_path_ids = get_ids_with_labels(typeval = "road_path")
  fixed_info = lapply(fixed_path_ids$id, get_render_road_path_info)
  expect_true(all(vapply(
    fixed_info,
    function(info) is.null(info$texture_repeats),
    logical(1)
  )))
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

test_that("OSM lane tags use directional and road-class fallbacks", {
  skip_if_not_installed("sf")

  roads = sf::st_sf(
    lanes = c("3", "2;3", NA, NA, "1.5", "7"),
    `lanes:forward` = c(NA, NA, "2", NA, NA, NA),
    `lanes:backward` = c(NA, NA, "1", NA, NA, NA),
    highway = c(
      "primary",
      "primary",
      "primary",
      "motorway_link",
      "secondary",
      "motorway"
    ),
    oneway = c("no", "no", "no", "yes", "no", "yes"),
    geometry = sf::st_sfc(
      lapply(seq_len(6L), function(index) {
        sf::st_linestring(rbind(c(0, index), c(1, index)))
      }),
      crs = 3857
    ),
    check.names = FALSE
  )

  expect_equal(
    resolve_render_road_lane_values(
      roads,
      lanes = NULL,
      lanes_column = "lanes"
    ),
    c(3L, 2L, 3L, 1L, 2L, 7L)
  )
  lane_evidence = resolve_render_road_lane_evidence(
    roads,
    lanes_column = "lanes"
  )
  expect_equal(
    lane_evidence$lane_count,
    c(3L, 2L, 3L, NA_integer_, NA_integer_, 7L)
  )
  expect_equal(
    lane_evidence$source,
    c(
      "selected_column",
      "selected_column",
      "directional_sum",
      "unavailable",
      "unavailable",
      "selected_column"
    )
  )
})

test_that("render_roads accepts layer and feature height columns", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("osqp")
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
  profile_diagnostics = attr(road_coords, "profile_diagnostics")
  expect_identical(profile_diagnostics$solver, "sparse_qp")
  expect_equal(profile_diagnostics$active_fragment_count, 2L)
  expect_equal(profile_diagnostics$solve_component_count, 1L)
  expect_true(profile_diagnostics$engineering_audit_passed)
  expect_lte(maximum_test_road_grade(road_coords[[2]]), 0.07 + 1e-8)
  road_path_ids = get_ids_with_labels(typeval = "road_path")
  terrain_following = vapply(
    road_path_ids$id,
    function(id) get_render_road_path_info(id)$terrain_following,
    logical(1)
  )
  expect_equal(terrain_following, c(TRUE, FALSE))
  road_info = lapply(road_path_ids$id, get_render_road_path_info)
  expect_equal(vapply(road_info, `[[`, numeric(1), "width"), c(12, 18))
  expect_false(identical(
    road_info[[1]]$texture_file,
    road_info[[2]]$texture_file
  ))
})

test_that("render_roads accepts raw OSM bridge and lane metadata", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("osqp")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 101,
    ncols = 101,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  roads = sf::st_sf(
    osm_id = c("surface", "unlayered-bridge"),
    highway = c("primary", "motorway_link"),
    lanes = c("2", NA_character_),
    oneway = c("no", "yes"),
    bridge = c(NA_character_, "yes"),
    tunnel = NA_character_,
    location = NA_character_,
    layer = NA_character_,
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(5, 50), c(95, 50))),
      sf::st_linestring(rbind(c(50, 5), c(50, 95))),
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
    layer = layer,
    lanes = lanes
  )

  expect_equal(length(road_coords), 2L)
  expect_equal(max(road_coords[[1]][, 2]), 0, tolerance = 1e-6)
  expect_equal(min(road_coords[[2]][, 2]), 5.5, tolerance = 1e-3)
  expect_lt(max(road_coords[[2]][, 2]), 5.6)
  expect_identical(
    attr(road_coords, "profile_diagnostics")$solver,
    "sparse_qp"
  )
  road_path_ids = get_ids_with_labels(typeval = "road_path")
  road_info = lapply(road_path_ids$id, get_render_road_path_info)
  expect_equal(
    vapply(road_info, `[[`, numeric(1), "width"),
    c(12, 9)
  )
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
  road_mesh = make_render_highquality_road_chain_mesh(
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
  expect_equal(max(vertices[, 2]), 4.055, tolerance = 1e-8)
  expect_lt(min(vertices[, 2]), 1)
})

test_that("native road meshing matches the R reference and threaded output", {
  point_count = 200L
  point_x = seq(0, 100, length.out = point_count)
  points = cbind(
    point_x,
    sin(point_x / 8) * 0.1,
    sin(point_x / 10) * 2
  )
  sections = calculate_render_road_vertex_sections(
    points = points,
    left_distance = rep(0.5, point_count),
    right_distance = rep(0.5, point_count)
  )
  station = calculate_road_path_cumulative_distance(points)
  mesh_args = list(
    sections = sections,
    station = station,
    total_length = utils::tail(station, 1L),
    bbox_center = c(50, 0, 0)
  )

  reference_normals = calculate_render_road_surface_normals_reference(
    sections$left_top,
    sections$right_top
  )
  serial_normals = calculate_render_road_surface_normals(
    sections$left_top,
    sections$right_top,
    parallel = FALSE
  )
  threaded_normals = calculate_render_road_surface_normals(
    sections$left_top,
    sections$right_top,
    parallel = TRUE
  )
  expect_equal(serial_normals, reference_normals, tolerance = 1e-12)
  expect_identical(threaded_normals, serial_normals)

  reference_mesh = do.call(build_render_road_section_mesh_reference, mesh_args)
  serial_mesh = do.call(
    build_render_road_section_mesh,
    c(mesh_args, list(parallel = FALSE))
  )
  threaded_mesh = do.call(
    build_render_road_section_mesh,
    c(mesh_args, list(parallel = TRUE))
  )
  expect_equal(serial_mesh$vb, reference_mesh$vb, tolerance = 1e-12)
  expect_equal(serial_mesh$it, reference_mesh$it)
  expect_equal(serial_mesh$normals, reference_mesh$normals, tolerance = 1e-12)
  expect_equal(
    serial_mesh$texcoords,
    reference_mesh$texcoords,
    tolerance = 1e-12
  )
  expect_equal(
    attr(serial_mesh, "render_road_mesh_diagnostics"),
    attr(reference_mesh, "render_road_mesh_diagnostics"),
    tolerance = 1e-12
  )
  expect_identical(threaded_mesh, serial_mesh)
})

test_that("batched terrain preparation matches R and threaded output", {
  heightmap = outer(
    seq_len(48L),
    seq_len(44L),
    function(row, column) sin(row / 5) + cos(column / 7)
  )
  points = matrix(
    c(
      -12,
      0.2,
      -10,
      -5,
      0.4,
      -2,
      3,
      0.1,
      5,
      11,
      0.3,
      12
    ),
    ncol = 3L,
    byrow = TRUE
  )
  densify_job = list(
    points = points,
    width = 2.5,
    terrain_following = TRUE
  )
  densify_jobs = rep(list(densify_job), 12L)
  densify_reference = densify_render_highquality_path_points(
    points = points,
    width = densify_job$width,
    heightmap = heightmap,
    zscale = 2
  )
  densify_serial = densify_render_road_paths_batch_cpp(
    densify_jobs,
    heightmap = heightmap,
    zscale = 2,
    parallel = FALSE,
    verbose = FALSE
  )
  densify_threaded = densify_render_road_paths_batch_cpp(
    densify_jobs,
    heightmap = heightmap,
    zscale = 2,
    parallel = TRUE,
    verbose = FALSE
  )

  expect_equal(
    unname(densify_serial[[1L]]),
    unname(densify_reference),
    tolerance = 1e-12
  )
  expect_identical(densify_threaded, densify_serial)

  frames = calculate_render_road_vertex_frames(points)
  left_distance = c(0.8, 1, 1.2, 0.9)
  right_distance = c(1.1, 1, 0.9, 0.8)
  side = cbind(frames$side[, 1L], 0, frames$side[, 2L])
  left_surface = points +
    side * (left_distance * frames$miter_scale)
  right_surface = points -
    side * (right_distance * frames$miter_scale)
  heightmap_scene = heightmap / 2
  center_height = interpolate_render_heightmap_height(
    heightmap_scene,
    points[, 1L],
    points[, 3L]
  )
  center_offset = points[, 2L] - center_height
  left_surface[, 2L] = interpolate_render_heightmap_height(
    heightmap_scene,
    left_surface[, 1L],
    left_surface[, 3L]
  ) +
    center_offset
  right_surface[, 2L] = interpolate_render_heightmap_height(
    heightmap_scene,
    right_surface[, 1L],
    right_surface[, 3L]
  ) +
    center_offset
  left_normal = interpolate_render_highquality_normals(
    left_surface,
    heightmap,
    zscale = 2
  )
  right_normal = interpolate_render_highquality_normals(
    right_surface,
    heightmap,
    zscale = 2
  )
  left_top = left_surface + left_normal * 0.055
  right_top = right_surface + right_normal * 0.055
  section_reference = list(
    left_bottom = left_top - left_normal * 0.11,
    right_bottom = right_top - right_normal * 0.11,
    left_top = left_top,
    right_top = right_top,
    left_normal = left_normal,
    right_normal = right_normal
  )
  section_job = list(
    points = points,
    left_distance = left_distance,
    right_distance = right_distance,
    side = frames$side,
    miter_scale = frames$miter_scale,
    terrain_following = TRUE
  )
  section_jobs = rep(list(section_job), 12L)
  section_serial = sample_render_road_sections_batch_cpp(
    section_jobs,
    heightmap = heightmap,
    zscale = 2,
    parallel = FALSE,
    verbose = FALSE
  )
  section_threaded = sample_render_road_sections_batch_cpp(
    section_jobs,
    heightmap = heightmap,
    zscale = 2,
    parallel = TRUE,
    verbose = FALSE
  )

  expect_equal(
    unname(section_serial[[1L]]),
    unname(section_reference),
    tolerance = 1e-12
  )
  expect_identical(section_threaded, section_serial)
  expect_output(
    densify_render_road_paths_batch_cpp(
      densify_jobs[1:2],
      heightmap,
      zscale = 2,
      parallel = TRUE,
      verbose = TRUE
    ),
    "Densifying road paths: 100%"
  )
  expect_output(
    sample_render_road_sections_batch_cpp(
      section_jobs[1:2],
      heightmap,
      zscale = 2,
      parallel = TRUE,
      verbose = TRUE
    ),
    "Sampling road terrain sections: 100%"
  )
})

test_that("batched road jobs are identical in serial and parallel", {
  make_prepared_job = function(
    points,
    closed = FALSE,
    material_sections = NULL
  ) {
    prepare_render_highquality_road_chain_mesh(
      points = points,
      bbox_center = c(0, 0, 0),
      width = 0.4,
      material = list(name = "default"),
      texture_length = 2,
      terrain_following = FALSE,
      material_sections = material_sections,
      closed = closed,
      return_mesh = TRUE
    )$job
  }
  open_points = cbind(
    seq(0, 6),
    sin(seq(0, 6)) / 10,
    c(0, 0.2, 0.5, 0.4, 0.8, 1, 1.1)
  )
  loop_angle = seq(0, 2 * pi, length.out = 9L)[-9L]
  closed_points = cbind(cos(loop_angle), 0, sin(loop_angle))
  material_sections = list(
    list(
      station_start = 0,
      station_end = 3,
      texture_file = NULL,
      texture_length = 1,
      texture_repeats = NULL,
      material = list(name = "first")
    ),
    list(
      station_start = 3,
      station_end = 6,
      texture_file = NULL,
      texture_length = 3,
      texture_repeats = 2,
      material = list(name = "second")
    )
  )
  jobs = list(
    make_prepared_job(open_points[1:2, , drop = FALSE]),
    make_prepared_job(open_points),
    make_prepared_job(closed_points, closed = TRUE),
    make_prepared_job(
      open_points,
      material_sections = material_sections
    )
  )

  serial = build_render_highquality_road_mesh_batch_cpp(
    jobs,
    parallel = FALSE,
    verbose = FALSE
  )
  threaded = build_render_highquality_road_mesh_batch_cpp(
    jobs,
    parallel = TRUE,
    verbose = FALSE
  )

  expect_identical(threaded, serial)
  expect_true(all(vapply(serial, `[[`, logical(1), "success")))
  expect_equal(vapply(serial, function(result) length(result$meshes), 1L), {
    c(1L, 1L, 1L, 2L)
  })
  expect_true(all(vapply(
    unlist(lapply(serial, `[[`, "meshes"), recursive = FALSE),
    function(mesh) {
      all(is.finite(mesh$vertices)) &&
        all(is.finite(mesh$vertex_normals)) &&
        all(is.finite(mesh$texcoords))
    },
    logical(1)
  )))
  expect_output(
    build_render_highquality_road_mesh_batch_cpp(
      jobs[1L],
      parallel = FALSE,
      verbose = TRUE
    ),
    "Converting roads to meshes: 100%"
  )
})

test_that("batched road jobs capture expected native geometry failures", {
  left_top = rbind(c(0, 0.1, -1), c(1, 0.1, -1), c(0, 0.1, -1))
  right_top = rbind(c(0, 0.1, 1), c(1, 0.1, 1), c(0, 0.1, 1))
  vertical_drop = matrix(rep(c(0, 0.2, 0), 3L), 3L, 3L, byrow = TRUE)
  sections = list(
    left_top = left_top,
    right_top = right_top,
    left_bottom = left_top - vertical_drop,
    right_bottom = right_top - vertical_drop,
    frames = list(
      incoming_tangent = matrix(
        c(1, 0, 1, 0, -1, 0),
        3L,
        2L,
        byrow = TRUE
      ),
      outgoing_tangent = matrix(
        c(1, 0, -1, 0, -1, 0),
        3L,
        2L,
        byrow = TRUE
      )
    )
  )
  invalid_job = list(
    sections = sections,
    bbox_center = c(0, 0, 0),
    closed = FALSE,
    mesh_sections = list(list(
      section_index = 1:3,
      texture_v = 0:2,
      closing_v = 2,
      cap_start = TRUE,
      cap_end = TRUE,
      closed = FALSE
    ))
  )
  valid_job = prepare_render_highquality_road_chain_mesh(
    points = cbind(0:2, 0, 0),
    bbox_center = c(0, 0, 0),
    width = 0.2,
    material = list(),
    terrain_following = FALSE,
    return_mesh = TRUE
  )$job

  results = build_render_highquality_road_mesh_batch_cpp(
    list(invalid_job, valid_job),
    parallel = TRUE,
    verbose = FALSE
  )

  expect_false(results[[1L]]$success)
  expect_match(results[[1L]]$error, "no common outward shading hemisphere")
  expect_length(results[[1L]]$meshes, 0L)
  expect_true(results[[2L]]$success)
  expect_length(results[[2L]]$meshes, 1L)
})

test_that("road world scale converts geographic scene cells to metres", {
  skip_if_not_installed("sf")

  projected_scale = calculate_road_path_world_scale(
    heightmap = matrix(0, nrow = 11, ncol = 21),
    extent = c(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000),
    crs = sf::st_crs(32632)
  )
  expect_equal(projected_scale, c(100, 50), tolerance = 1e-7)

  geographic_scale = calculate_road_path_world_scale(
    heightmap = matrix(0, nrow = 101, ncol = 101),
    extent = c(
      xmin = 7.76,
      xmax = 8.02,
      ymin = 46.52,
      ymax = 46.80
    ),
    crs = sf::st_crs(4326)
  )
  expect_equal(geographic_scale, c(198.9, 311.2), tolerance = 1)
  expect_true(all(geographic_scale > 100))
})

test_that("stabilized road frames preserve unit cross-section widths", {
  points = cbind(
    seq(0, 10, by = 2),
    0,
    c(0, 0.5, 1.5, 3, 5, 7.5)
  )
  frames = calculate_render_road_stabilized_vertex_frames(
    points = points,
    left_distance = rep(0.5, nrow(points)),
    right_distance = rep(0.5, nrow(points)),
    texture_world_scale = c(1, 1),
    guide_step_fraction = 0.2
  )

  expect_equal(frames$miter_scale, rep(1, nrow(points)))
  expect_equal(
    frames$join_style,
    c("endpoint", rep("stabilized", 4), "endpoint")
  )
})

test_that("road width and mesh texture coordinates follow public settings", {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")

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
  texture_file = make_road_lane_texture()
  road_mesh = make_render_highquality_road_chain_mesh(
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
  expect_equal(
    range(mesh_info$vertices[, 2]),
    c(-0.055, 0.055),
    tolerance = 1e-8
  )
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

  repeated_road_mesh = make_render_highquality_road_chain_mesh(
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
  road_mesh = make_render_highquality_road_chain_mesh(
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

test_that("road mesh sweep stabilizes narrow closed loops", {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")

  loop_x = c(
    0,
    -0.659934,
    -0.949731,
    -1.594436,
    -1.652689,
    -1.653554,
    -1.659934,
    -1.760409,
    -1.659934,
    -1.658622,
    -1.657342,
    -1.23523,
    -1.150157,
    -0.659934,
    -0.552698,
    -0.393536,
    -0.298016,
    -0.200248,
    0.340066,
    0.538476,
    0.516297,
    0.509003,
    0.491287,
    0.340066,
    0.281515,
    0.2476,
    -0.026489,
    -0.023078,
    -0.01551,
    -0.001795
  )
  loop_z = c(
    0,
    0.517151,
    0.744247,
    1.249466,
    1.737,
    1.744247,
    1.797638,
    2.63855,
    2.741592,
    2.742935,
    2.744247,
    3.177155,
    3.234467,
    3.564758,
    3.637009,
    3.744247,
    3.808609,
    3.744247,
    3.38858,
    3.25798,
    2.744247,
    2.575317,
    2.164963,
    1.903885,
    1.802795,
    1.744247,
    1.271042,
    1.107391,
    0.744247,
    0.086105
  )
  road_mesh = make_render_highquality_road_chain_mesh(
    points = cbind(loop_x, 0, loop_z),
    bbox_center = c(0, 0, 0),
    width = 0.39644566178321838,
    material = rayrender::diffuse(color = "white"),
    texture_world_scale = c(30.24465, 30.29328),
    terrain_following = FALSE,
    closed = TRUE,
    return_mesh = TRUE
  )
  diagnostics = attr(road_mesh, "render_road_mesh_diagnostics")

  expect_equal(
    diagnostics$sweep_stabilization$initial_inverted_segment,
    c(16L, 17L, 30L)
  )
  expect_equal(diagnostics$sweep_stabilization$guide_step_fraction, 0.6)
  expect_equal(diagnostics$removed_quad_count, 0L)
  expect_gt(diagnostics$minimum_triangle_area, 0)
  expect_true(all(is.finite(road_mesh$vb)))
  expect_true(all(is.finite(road_mesh$normals)))
})

test_that("terrain-buffered road fallback handles self-overlapping sweeps", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")

  task = list(
    points = matrix(
      c(0, 0, 0, 5, 0, 0, 4.9, 0, 0.1),
      ncol = 3,
      byrow = TRUE
    ),
    bbox_center = c(0, 0, 0),
    width = 4,
    heightmap = matrix(0, nrow = 9, ncol = 9),
    zscale = 1,
    material = rayrender::diffuse(color = "white"),
    texture_world_scale = c(1, 1),
    terrain_following = TRUE
  )

  expect_warning(
    meshes <- make_render_highquality_road_path_meshes(list(task)),
    "terrain-buffered road mesh fallback"
  )
  diagnostics = attr(meshes, "mesh_chain_diagnostics")

  expect_length(meshes, 1L)
  expect_equal(diagnostics$buffered_fallback_count, 1L)
  expect_equal(length(diagnostics$buffered_fallback_chain_id), 1L)
  expect_true(attr(meshes[[1L]], "render_road_buffered_fallback")$used)
  fallback_vertices = meshes[[1L]]$shape_info[[1L]]$mesh_info[[1L]]$vertices
  expect_equal(
    range(fallback_vertices[, 2]),
    c(-0.055, 0.055),
    tolerance = 1e-8
  )
  expect_no_condition(rayrender:::process_scene(meshes[[1L]]))
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

test_that("road topology infers missing OSM structure layers", {
  skip_if_not_installed("sf")
  skip_if_not_installed("igraph")

  roads = sf::st_sf(
    layer = c(NA, NA, NA, NA, 3),
    bridge = c(NA, "yes", NA, NA, "yes"),
    tunnel = c(NA, NA, "yes", NA, NA),
    location = c(NA, NA, NA, "elevated", NA),
    highway = c("primary", rep("motorway_link", 4)),
    osm_id = paste0("way-", seq_len(5L)),
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(-120, 0), c(120, 0))),
      sf::st_linestring(rbind(c(-60, -30), c(-60, 30))),
      sf::st_linestring(rbind(c(0, -30), c(0, 30))),
      sf::st_linestring(rbind(c(60, -30), c(60, 30))),
      sf::st_linestring(rbind(c(100, -30), c(100, 30))),
      crs = 32615
    )
  )
  topology = build_render_road_layer_topology(
    prepare_render_road_layer_features(roads, "layer")
  )
  fragments = topology$fragments

  expect_equal(fragments$render_road_layer, c(0, 1, -1, 1, 3))
  expect_equal(
    fragments$render_road_layer_explicit,
    c(FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  expect_equal(
    fragments$render_road_layer_inferred,
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    fragments$render_road_layer_source,
    c(
      "implicit_surface",
      "bridge",
      "tunnel",
      "elevated_location",
      "explicit_layer"
    )
  )
  expect_equal(nrow(topology$crossing_pairs), 4L)
  expect_true(all(
    fragments$render_road_fragment_id[-1L] %in%
      topology$prospective_solve_seed_fragment_id
  ))
})

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
  expect_equal(nrow(overlap_topology$layer_overlaps), 1L)
  expect_true(overlap_topology$layer_overlaps$layer_relationship)
})
calculate_render_road_vertex_frames_reference = function(
  points,
  closed = FALSE,
  miter_limit = 4
) {
  points = as.matrix(points)
  point_count = nrow(points)
  segment_start = if (closed) {
    seq_len(point_count)
  } else {
    seq_len(point_count - 1L)
  }
  segment_end = if (closed) {
    c(seq.int(2L, point_count), 1L)
  } else {
    seq.int(2L, point_count)
  }
  segment_delta = points[segment_end, c(1, 3), drop = FALSE] -
    points[segment_start, c(1, 3), drop = FALSE]
  segment_length = sqrt(rowSums(segment_delta^2))
  segment_tangent = segment_delta / segment_length
  incoming_tangent = matrix(NA_real_, nrow = point_count, ncol = 2L)
  outgoing_tangent = matrix(NA_real_, nrow = point_count, ncol = 2L)
  if (closed) {
    incoming_tangent = segment_tangent[
      c(point_count, seq_len(point_count - 1L)),
      ,
      drop = FALSE
    ]
    outgoing_tangent = segment_tangent
  } else {
    incoming_tangent[1L, ] = segment_tangent[1L, ]
    incoming_tangent[-1L, ] = segment_tangent
    outgoing_tangent[-point_count, ] = segment_tangent
    outgoing_tangent[point_count, ] = segment_tangent[nrow(segment_tangent), ]
  }
  endpoint = !closed & seq_len(point_count) %in% c(1L, point_count)
  join_rows = lapply(seq_len(point_count), function(index) {
    if (endpoint[[index]]) {
      tangent = if (index == 1L) {
        outgoing_tangent[index, ]
      } else {
        incoming_tangent[index, ]
      }
      side = c(-tangent[[2]], tangent[[1]])
      return(data.frame(
        join_style = "endpoint",
        side_x = side[[1]],
        side_z = side[[2]],
        miter_scale = 1,
        turn_cross = 0,
        turn_dot = 1,
        stringsAsFactors = FALSE
      ))
    }
    join = resolve_render_road_join_style(
      incoming_tangent[index, ],
      outgoing_tangent[index, ],
      miter_limit = miter_limit
    )
    data.frame(
      join_style = join$style,
      side_x = join$side_x,
      side_z = join$side_z,
      miter_scale = join$miter_scale,
      turn_cross = join$turn_cross,
      turn_dot = join$turn_dot,
      stringsAsFactors = FALSE
    )
  })
  joins = do.call(rbind, join_rows)
  list(
    incoming_tangent = incoming_tangent,
    outgoing_tangent = outgoing_tangent,
    side = as.matrix(joins[, c("side_x", "side_z"), drop = FALSE]),
    miter_scale = joins$miter_scale,
    join_style = joins$join_style,
    turn_cross = joins$turn_cross,
    turn_dot = joins$turn_dot,
    segment_length = segment_length
  )
}

test_that("compiled road vertex frames match the reference implementation", {
  set.seed(20260808)
  random_points = cbind(
    cumsum(runif(80L, 0.1, 2)),
    rnorm(80L),
    cumsum(rnorm(80L))
  )
  cases = list(
    list(
      points = matrix(c(0, 0, 0, 1, 0, 0, 2, 0, 0), ncol = 3L, byrow = TRUE),
      closed = FALSE
    ),
    list(
      points = matrix(c(0, 0, 0, 1, 0, 0, 1, 0, 1), ncol = 3L, byrow = TRUE),
      closed = FALSE
    ),
    list(
      points = matrix(
        c(0, 0, 0, 1, 0, 0, 0.01, 0, 0.01),
        ncol = 3L,
        byrow = TRUE
      ),
      closed = FALSE
    ),
    list(
      points = matrix(
        c(0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1),
        ncol = 3L,
        byrow = TRUE
      ),
      closed = TRUE
    ),
    list(points = random_points, closed = FALSE)
  )

  for (case in cases) {
    reference = calculate_render_road_vertex_frames_reference(
      case$points,
      closed = case$closed
    )
    compiled = calculate_render_road_vertex_frames(
      case$points,
      closed = case$closed
    )
    expect_equal(compiled$join_style, reference$join_style)
    for (name in setdiff(names(reference), "join_style")) {
      expect_equal(
        unname(compiled[[name]]),
        unname(reference[[name]]),
        tolerance = 1e-14
      )
    }
  }
})
