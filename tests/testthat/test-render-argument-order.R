render_formal_names = function(function_name) {
  setdiff(
    names(formals(get(function_name, envir = asNamespace("rayshader")))),
    "..."
  )
}

test_that("scene-aware render functions put their primary input first", {
  primary_arguments = c(
    render_beveled_polygons = "polygon",
    render_buildings = "polygon",
    render_camera = "location",
    render_clouds = "start_altitude",
    render_compass = "angle",
    render_contours = "levels",
    render_depth = "focus",
    render_floating_overlay = "overlay",
    render_highquality = "filename",
    render_multipolygonz = "sfobj",
    render_obj = "filename",
    render_path = "y",
    render_people = "location",
    render_points = "location",
    render_polygons = "polygon",
    render_raymesh = "raymesh",
    render_roads = "roads",
    render_snapshot = "filename",
    render_streams = "streams",
    render_trails = "trails",
    render_tree = "location",
    render_water = "waterdepth",
    render_zaxis = "zaxis_data"
  )

  for (function_name in names(primary_arguments)) {
    expect_identical(
      render_formal_names(function_name)[[1]],
      unname(primary_arguments[[function_name]]),
      info = function_name
    )
  }
})

test_that("scene-aware render functions put shared scene inputs last", {
  cache_suffixes = list(
    render_beveled_polygons = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_buildings = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_camera = "panel",
    render_clouds = c("zscale", "vertical_exaggeration", "heightmap"),
    render_compass = c("zscale", "vertical_exaggeration"),
    render_contours = c("zscale", "vertical_exaggeration", "heightmap"),
    render_depth = c(
      "heightmap",
      "zscale",
      "cache_scene",
      "reset_scene_cache"
    ),
    render_floating_overlay = c(
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_highquality = c("cache_scene", "reset_scene_cache"),
    render_multipolygonz = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_obj = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_path = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_people = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_points = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_polygons = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_raymesh = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_roads = c("zscale", "vertical_exaggeration", "heightmap"),
    render_snapshot = c("cache_scene", "reset_scene_cache"),
    render_streams = c("zscale", "vertical_exaggeration", "heightmap"),
    render_trails = c("zscale", "vertical_exaggeration", "heightmap"),
    render_tree = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    ),
    render_water = c("zscale", "vertical_exaggeration", "heightmap"),
    render_zaxis = c(
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    )
  )

  for (function_name in names(cache_suffixes)) {
    function_arguments = render_formal_names(function_name)
    expected_suffix = cache_suffixes[[function_name]]
    expect_identical(
      tail(function_arguments, length(expected_suffix)),
      expected_suffix,
      info = function_name
    )
  }
})

test_that("low-level coordinate render arguments keep x before y", {
  coordinate_functions = c(
    "render_compass",
    "render_obj",
    "render_people",
    "render_points",
    "render_raymesh",
    "render_tree"
  )

  for (function_name in coordinate_functions) {
    function_arguments = render_formal_names(function_name)
    expect_true(
      match("x", function_arguments) < match("y", function_arguments),
      info = function_name
    )
  }
})

test_that("render functions derive the base mask from the heightmap", {
  render_function_names = ls(
    envir = asNamespace("rayshader"),
    pattern = "^render_"
  )
  render_functions = Filter(
    is.function,
    mget(render_function_names, envir = asNamespace("rayshader"))
  )

  for (function_name in names(render_functions)) {
    expect_false(
      "baseshape" %in% names(formals(render_functions[[function_name]])),
      info = function_name
    )
  }
})
