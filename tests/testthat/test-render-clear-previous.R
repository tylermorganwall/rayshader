test_that("clear-only detection always invokes its handler when clearing", {
  clear_count = 0L
  clear_layer = function() {
    clear_count <<- clear_count + 1L
  }

  expect_false(is_render_clear_only_call(
    FALSE,
    quote(render_points(clear_previous = FALSE)),
    clear_layer
  ))
  expect_equal(clear_count, 0L)

  expect_true(is_render_clear_only_call(
    TRUE,
    quote(render_points(clear_previous = TRUE)),
    clear_layer
  ))
  expect_equal(clear_count, 1L)

  expect_false(is_render_clear_only_call(
    TRUE,
    quote(render_points(x = 1, clear_previous = TRUE)),
    clear_layer
  ))
  expect_equal(clear_count, 2L)

  expect_false(is_render_clear_only_call(
    TRUE,
    quote(render_points(x = 1)),
    clear_layer
  ))
  expect_equal(clear_count, 3L)

  expect_true(is_render_clear_only_call(
    TRUE,
    quote(render_path(clear_previous = TRUE, tag = "custom_path")),
    clear_layer,
    routing_arguments = "tag"
  ))
  expect_equal(clear_count, 4L)
})

test_that("clear-only render calls remove their existing layers", {
  local_rgl_use_null()
  rgl::open3d()
  on.exit(rgl::close3d(), add = TRUE)

  clear_calls = list(
    beveled_polygons = list(
      call = quote(render_beveled_polygons(clear_previous = TRUE)),
      tags = "obj_raymesh_beveled_polygon"
    ),
    buildings = list(
      call = quote(render_buildings(clear_previous = TRUE)),
      tags = "obj_raymesh_building"
    ),
    contours = list(
      call = quote(render_contours(clear_previous = TRUE)),
      tags = "contour3d"
    ),
    labels = list(
      call = quote(render_label(clear_previous = TRUE)),
      tags = c("textline", "raytext")
    ),
    multipolygons = list(
      call = quote(render_multipolygonz(clear_previous = TRUE)),
      tags = "obj_multipolygon"
    ),
    objects = list(
      call = quote(render_obj(clear_previous = TRUE)),
      tags = "obj"
    ),
    paths = list(
      call = quote(render_path(clear_previous = TRUE)),
      tags = "path3d"
    ),
    people = list(
      call = quote(render_people(clear_previous = TRUE)),
      tags = "objperson"
    ),
    points = list(
      call = quote(render_points(clear_previous = TRUE)),
      tags = "points3d"
    ),
    polygons = list(
      call = quote(render_polygons(clear_previous = TRUE)),
      tags = "polygon3d"
    ),
    raymeshes = list(
      call = quote(render_raymesh(clear_previous = TRUE)),
      tags = "obj_raymesh"
    ),
    roads = list(
      call = quote(render_roads(clear_previous = TRUE)),
      tags = c("road_path", "road_mesh_preview")
    ),
    streams = list(
      call = quote(render_streams(clear_previous = TRUE)),
      tags = "water_path"
    ),
    trees = list(
      call = quote(render_tree(clear_previous = TRUE)),
      tags = "objtree"
    ),
    water = list(
      call = quote(render_water(clear_previous = TRUE)),
      tags = c("waterlines", "water")
    )
  )

  for (clear_call in clear_calls) {
    for (tag in clear_call$tags) {
      rgl::points3d(0, 0, 0, tag = tag)
    }

    result = NULL
    expect_no_condition({
      result = withVisible(eval(clear_call$call))
    })
    expect_false(result$visible)

    remaining_tags = rgl::ids3d(tags = TRUE)$tag
    expect_false(any(clear_call$tags %in% remaining_tags))
  }
})

test_that("renderers clear before validating a replacement layer", {
  local_rgl_use_null()
  rgl::open3d()
  on.exit(rgl::close3d(), add = TRUE)

  rgl::points3d(0, 0, 0, tag = "textline")
  rgl::points3d(0, 0, 0, tag = "raytext")

  expect_error(
    render_label(
      text = "replacement",
      clear_previous = TRUE,
      filter_to_extent = NA
    ),
    "`filter_to_extent` must be a single TRUE/FALSE value",
    fixed = TRUE
  )

  remaining_tags = rgl::ids3d(tags = TRUE)$tag
  expect_false(any(c("textline", "raytext") %in% remaining_tags))

  rgl::points3d(0, 0, 0, tag = "obj_raymesh")

  expect_error(
    render_raymesh(
      clear_previous = TRUE,
      swap_yz_transform = "invalid"
    ),
    "`swap_yz_transform` must be either \"swap\" or \"rotate\".",
    fixed = TRUE
  )

  expect_false("obj_raymesh" %in% rgl::ids3d(tags = TRUE)$tag)
})

test_that("road and stream clear-only calls clear render metadata", {
  local_rgl_use_null()
  rgl::open3d()
  on.exit(rgl::close3d(), add = TRUE)
  on.exit(cache_scene_road_meshes(NULL), add = TRUE)
  on.exit(cache_scene_stream_meshes(NULL), add = TRUE)
  on.exit(clear_render_road_path_info(), add = TRUE)
  on.exit(clear_render_water_path_info(), add = TRUE)

  road_id = rgl::points3d(0, 0, 0, tag = "road_path")
  stream_id = rgl::points3d(0, 0, 0, tag = "water_path")
  register_render_road_path_info(road_id, list(test = TRUE))
  register_render_water_path_info(stream_id, list(test = TRUE))
  cache_scene_road_meshes(list(list(test = TRUE)))
  cache_scene_stream_meshes(list(list(test = TRUE)))

  expect_no_condition(render_roads(clear_previous = TRUE))
  expect_no_condition(render_streams(clear_previous = TRUE))

  expect_null(get_render_road_path_info(road_id))
  expect_null(get_render_water_path_info(stream_id))
  expect_null(get_scene_road_meshes())
  expect_null(get_scene_stream_meshes())
})

test_that("clear-only calls honor custom render tags", {
  local_rgl_use_null()
  rgl::open3d()
  on.exit(rgl::close3d(), add = TRUE)

  custom_calls = list(
    list(
      call = quote(render_multipolygonz(
        clear_previous = TRUE,
        rgl_tag = "_custom"
      )),
      tag = "obj_custom"
    ),
    list(
      call = quote(render_obj(clear_previous = TRUE, rgl_tag = "_custom")),
      tag = "obj_custom"
    ),
    list(
      call = quote(render_path(clear_previous = TRUE, tag = "custom_path")),
      tag = "custom_path"
    ),
    list(
      call = quote(render_raymesh(
        clear_previous = TRUE,
        rgl_tag = "_custom",
        rgl_tag_prefix = "custom"
      )),
      tag = "custom_custom"
    )
  )

  for (custom_call in custom_calls) {
    rgl::points3d(0, 0, 0, tag = custom_call$tag)

    expect_no_condition(eval(custom_call$call))
    expect_false(custom_call$tag %in% rgl::ids3d(tags = TRUE)$tag)
  }
})
