test_that("render_highquality() resolves rgl material overrides", {
  skip_if_not_installed("rayrender")

  expect_false("point_material" %in% names(formals(render_highquality)))
  expect_false("path_material" %in% names(formals(render_highquality)))

  material = make_render_highquality_rgl_material(
    rayrender::diffuse,
    color = c(0.2, 0.3, 0.4),
    name = "points3d"
  )
  expect_true(is_rayrender_material(material))
  expect_equal(material[[1]]$properties[[1]], c(0.2, 0.3, 0.4))

  material_lookup = setNames(
    list(rayrender::metal()),
    "123"
  )
  expect_true(is_rayrender_material(resolve_render_highquality_rgl_material(
    rgl_materials = material_lookup,
    id = 123,
    tag = "points3d",
    color = c(1, 0, 0)
  )))
  expect_null(resolve_render_highquality_rgl_material(
    rgl_materials = material_lookup,
    id = 456,
    tag = "points3d",
    color = c(1, 0, 0)
  ))
})

test_that("render_highquality() applies rgl material overrides by tag and id", {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  matrix(0, 3, 3) |>
    sphere_shade() |>
    plot_3d(zscale = 1, solid = FALSE)
  render_points(
    x = c(1, 2),
    y = c(1, 2),
    z = c(1, 1),
    color = c("red", "blue"),
    size = 2
  )
  point_ids = get_ids_with_labels(typeval = "points3d")$id
  surface_id = get_ids_with_labels(typeval = "surface")$id[[1]]

  expect_error(
    render_highquality(point_material = rayrender::metal),
    "Use `rgl_materials` instead"
  )

  scene_tag = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    rgl_materials = list(points3d = rayrender::metal)
  )
  sphere_materials = scene_tag$material[scene_tag$shape == "sphere"]
  expect_equal(
    vapply(sphere_materials, function(material) material$type, integer(1)),
    rep(rayrender::metal()[[1]]$type, length(sphere_materials))
  )

  scene_id = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    rgl_materials = setNames(
      list(rayrender::metal()),
      as.character(point_ids[[1]])
    )
  )
  sphere_materials = scene_id$material[scene_id$shape == "sphere"]
  expect_equal(
    vapply(sphere_materials, function(material) material$type, integer(1)),
    rep(rayrender::metal()[[1]]$type, length(sphere_materials))
  )

  scene_mesh_id = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    rgl_materials = setNames(
      list(rayrender::metal()),
      as.character(surface_id)
    )
  )
  mesh_materials = scene_mesh_id$material[scene_mesh_id$shape == "raymesh"]
  expect_equal(mesh_materials[[1]]$type, rayrender::metal()[[1]]$type)
})

test_that("render_highquality() defaults label and z-axis overlays to screen space", {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  sphere_shade(heightmap) |>
    plot_3d(
      zscale = 10,
      shadow = FALSE,
      water = FALSE,
      windowsize = c(300, 300)
    )
  render_label(
    x = 10,
    y = 10,
    text = "A",
    heightmap = heightmap,
    extent = extent,
    zscale = 10,
    altitude = 10
  )
  render_zaxis(
    extent = extent,
    heightmap = heightmap,
    zscale = 10,
    zaxis_breaks = c(0, 10),
    zaxis_title = "Z"
  )

  scene = render_highquality(return_scene = TRUE, light = FALSE)
  screen_text = attr(scene, "screen_text")
  screen_line = attr(scene, "screen_line")

  expect_s3_class(screen_text, "ray_screen_text")
  expect_s3_class(screen_line, "ray_screen_line")
  expect_true("A" %in% screen_text$label)
  expect_true("Z" %in% screen_text$label)
  expect_gte(nrow(screen_line), 3)

  world_scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    text_render = "world",
    line_render = "world"
  )
  expect_null(attr(world_scene, "screen_text"))
  expect_null(attr(world_scene, "screen_line"))
})

test_that("render_highquality() can render paths as screen-space lines", {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  sphere_shade(heightmap) |>
    plot_3d(
      zscale = 10,
      shadow = FALSE,
      water = FALSE,
      windowsize = c(300, 300)
    )
  render_path(
    x = c(2, 18),
    y = c(2, 18),
    heightmap = heightmap,
    extent = extent,
    zscale = 10
  )

  auto_scene = render_highquality(return_scene = TRUE, light = FALSE)
  screen_scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    line_render = "screen"
  )

  expect_null(attr(auto_scene, "screen_line"))
  expect_s3_class(attr(screen_scene, "screen_line"), "ray_screen_line")
})
