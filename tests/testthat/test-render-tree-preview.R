setup_simple_tree_preview_scene = function() {
  heightmap = outer(
    seq_len(20L),
    seq_len(20L),
    function(row, column) row + column
  )
  plot_3d_test(
    height_shade(heightmap),
    heightmap = heightmap,
    zscale = 5,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  )
  list(
    heightmap = heightmap,
    extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  )
}

test_that("tree preview mode accepts points and cache-only rendering", {
  expect_equal(resolve_render_tree_preview_mode(FALSE), "full")
  expect_equal(resolve_render_tree_preview_mode(TRUE), "point")
  expect_equal(resolve_render_tree_preview_mode("points"), "point")
  expect_equal(resolve_render_tree_preview_mode("none"), "none")
  expect_error(
    resolve_render_tree_preview_mode("invalid"),
    "preview_simple"
  )
})

test_that("simple tree previews cache transforms and use rayrender instances", {
  skip_if_not_installed("rayvertex")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  scene = setup_simple_tree_preview_scene()
  angles = cbind(0, c(0, 30, 60), 0)

  render_tree(
    x = c(8, 10, 12),
    y = c(8, 10, 12),
    extent = scene$extent,
    heightmap = scene$heightmap,
    tree_height = c(5, 10, 15),
    angle = angles,
    preview_simple = TRUE,
    preview_point_size = 9,
    clear_previous = TRUE
  )

  tree_ids = get_ids_with_labels()
  preview_ids = tree_ids$id[tree_ids$tag == "tree_preview"]
  instance_layers = get_scene_tree_instances()
  components = instance_layers[[1L]]
  preview_colors = rgl::rgl.attrib(preview_ids, "colors")[, 1:3, drop = FALSE]

  expect_length(preview_ids, 1L)
  expect_equal(nrow(rgl::rgl.attrib(preview_ids, "vertices")), 3L)
  expect_equal(rgl::material3d(id = preview_ids)$size, 9)
  expect_equal(
    unname(preview_colors[1L, ]),
    unname(grDevices::col2rgb("#22aa22")[, 1L] / 255),
    tolerance = 1e-7
  )
  expect_false(any(tree_ids$tag == "objtree"))
  expect_length(instance_layers, 1L)
  expect_equal(
    unname(as.integer(table(components$component))),
    c(3L, 3L)
  )
  expect_equal(components$angle_y, rep(angles[, 2L], 2L))
  expect_true(all(is.finite(as.matrix(components[, c(
    "x",
    "y",
    "z",
    "scale_x",
    "scale_y",
    "scale_z"
  )]))))

  instance_models = make_render_highquality_cached_tree_instances(
    instance_layers = instance_layers,
    bbox_center = c(0, 0, 0),
    override_material = FALSE,
    material = rayrender::diffuse(),
    rgl_materials = list(),
    calculate_consistent_normals = TRUE
  )
  instance_counts = vapply(
    instance_models,
    function(model) {
      length(model$shape_info[[1L]]$shape_properties$x_values)
    },
    integer(1)
  )

  expect_length(instance_models, 2L)
  expect_true(all(vapply(
    instance_models,
    function(model) identical(model$shape[[1L]], "instance"),
    logical(1)
  )))
  expect_equal(unname(instance_counts), c(3L, 3L))
  expect_true(all(vapply(
    instance_models,
    function(model) {
      length(unique(model$shape_info[[1L]]$shape_properties$scale_y)) > 1L
    },
    logical(1)
  )))

  high_quality_scene = render_highquality(
    return_scene = TRUE,
    width = 100,
    height = 100,
    samples = 1,
    cache_scene = FALSE
  )
  expect_equal(sum(high_quality_scene$shape == "instance"), 2L)

  render_tree(
    x = c(8, 10, 12),
    y = c(8, 10, 12),
    extent = scene$extent,
    heightmap = scene$heightmap,
    tree_height = c(5, 10, 15),
    crown_width_ratio = c(0.65, 0.9, 0.65),
    preview_simple = "none",
    clear_previous = TRUE
  )
  varied_models = make_render_highquality_cached_tree_instances(
    instance_layers = get_scene_tree_instances(),
    bbox_center = c(0, 0, 0),
    override_material = FALSE,
    material = rayrender::diffuse(),
    rgl_materials = list(),
    calculate_consistent_normals = TRUE
  )
  varied_counts = vapply(
    varied_models,
    function(model) {
      length(model$shape_info[[1L]]$shape_properties$x_values)
    },
    integer(1)
  )

  expect_length(varied_models, 4L)
  expect_equal(sort(unname(varied_counts)), c(1L, 1L, 2L, 2L))
})

test_that("tree preview point size is validated", {
  expect_error(
    render_tree(preview_simple = TRUE, preview_point_size = 0),
    "preview_point_size"
  )
  expect_error(
    render_tree(preview_simple = TRUE, preview_point_size = c(2, 3)),
    "preview_point_size"
  )
  expect_error(
    render_tree(preview_simple = TRUE, preview_point_size = Inf),
    "preview_point_size"
  )
})

test_that("cache-only tree previews draw nothing and clear with the tree layer", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  scene = setup_simple_tree_preview_scene()

  render_tree(
    x = c(8, 12),
    y = c(8, 12),
    extent = scene$extent,
    heightmap = scene$heightmap,
    tree_height = 8,
    preview_simple = "none",
    clear_previous = TRUE
  )

  tree_tags = get_ids_with_labels()$tag
  expect_false(any(tree_tags %in% c("objtree", "tree_preview")))
  expect_length(get_scene_tree_instances(), 1L)

  reset_scene_context(
    clear_scene_metadata = FALSE,
    clear_scene_cache = TRUE
  )
  expect_null(get_scene_tree_instances())

  cache_scene_tree_instances(data.frame(test = TRUE))
  expect_invisible(render_tree(clear_previous = TRUE))
  expect_null(get_scene_tree_instances())
})
