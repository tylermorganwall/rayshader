test_that("simple people retain poses, transforms and material groups", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  h = matrix(0, 10, 10)
  plot_3d_test(height_shade(h), h, zscale = 4, solid = FALSE, shadow = FALSE)
  xyz = cbind(c(-2, -1, 1, 2), 0, 0)
  angles = cbind(0, c(0, 45, 90, 135), 0)
  render_people(
    xyz = xyz,
    pose = c("walking", "standing", "walking", "standing"),
    color = c("red", "blue", "red", "blue"),
    angle = angles,
    simple_rgl = TRUE,
    load_normals = FALSE
  )
  layers = get_scene_person_instances()
  expect_equal(sum(vapply(layers, nrow, integer(1))), 4)
  expect_length(layers, 2)
  expect_true(all(vapply(
    layers,
    function(x) all(x$scale_y == 0.25),
    logical(1)
  )))
  expect_equal(layers[[1]]$angle_y, c(0, 90))
  expect_equal(layers[[1]]$x, c(-2, 1))
  expect_false(any(get_ids_with_labels()$tag == "objperson"))
  expect_equal(
    sum(vapply(
      get_ids_with_labels(typeval = "person_preview")$id,
      function(id) nrow(rgl::rgl.attrib(id, "vertices")),
      integer(1)
    )),
    4
  )
  scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    cache_scene = FALSE
  )
  instances = scene[scene$shape == "instance", ]
  expect_equal(nrow(instances), 2)
  expect_equal(
    sum(vapply(
      instances$shape_info,
      function(x) length(x$shape_properties$x_values),
      integer(1)
    )),
    4
  )
  expect_false(any(scene$shape == "sphere"))
  render_people(clear_previous = TRUE)
  expect_null(get_scene_person_instances())
  expect_length(get_ids_with_labels(typeval = "person_preview")$id, 0)
  render_people(xyz = xyz[1, , drop = FALSE], simple_rgl = TRUE)
  reset_scene_context(clear_scene_metadata = FALSE, clear_scene_cache = TRUE)
  expect_null(get_scene_person_instances())
})

test_that("simple line people preserve terrain placement and model variants", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  r = terra::rast(
    nrows = 11,
    ncols = 11,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10,
    crs = "EPSG:3857",
    vals = 5
  )
  plot_3d_test(constant_shade(r), r, zscale = 2, solid = FALSE, shadow = FALSE)
  line = sf::st_sfc(sf::st_linestring(rbind(c(2, 5), c(8, 5))), crs = 3857)
  render_people(
    line = line,
    line_pattern = "MF",
    line_spacing = 2,
    line_terrain_spacing = FALSE,
    line_align_terrain = FALSE,
    simple_rgl = TRUE
  )
  layers = get_scene_person_instances()
  expect_equal(sum(vapply(layers, nrow, integer(1))), 4)
  expect_length(
    unique(vapply(layers, function(x) x$model_file[1], character(1))),
    2
  )
  expect_true(all(vapply(
    layers,
    function(x) all(abs(x$y - 2.5) < 1e-6),
    logical(1)
  )))
  # Replacement with the full preview must clear both the points and cache.
  render_people(x = 5, y = 5, clear_previous = TRUE)
  expect_null(get_scene_person_instances())
  expect_length(get_ids_with_labels(typeval = "person_preview")$id, 0)
  expect_gt(length(get_ids_with_labels(typeval = "objperson")$id), 0)
  expect_error(render_people(x = 5, y = 5, simple_rgl = NA), "simple_rgl")
})
