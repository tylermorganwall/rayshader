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

  scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    text_occlusion_tolerance = 0.02,
    line_occlusion_tolerance = 0.03,
    screen_text_args = list(
      background_color = "yellow",
      background_alpha = 0.25,
      halo_color = "white",
      halo_expand = 2
    ),
    screen_line_args = list(
      lineend = "butt",
      clip = FALSE
    )
  )
  screen_text = attr(scene, "screen_text")
  screen_line = attr(scene, "screen_line")

  expect_s3_class(screen_text, "ray_screen_text")
  expect_s3_class(screen_line, "ray_screen_line")
  expect_true("A" %in% screen_text$label)
  expect_true("Z" %in% screen_text$label)
  expect_gte(nrow(screen_line), 3)
  screen_text_labels = trimws(as.character(screen_text$label))
  zaxis_label_rows = screen_text_labels %in% c("0", "10")
  zaxis_title_row = screen_text_labels == "Z"
  zaxis_tick_rows = abs(screen_line$x - screen_line$xend) < 1e-8 &
    abs(screen_line$y - screen_line$yend) < 1e-8 &
    abs(screen_line$z - screen_line$zend) < 1e-8
  zaxis_tick_y = sort(screen_line$y[zaxis_tick_rows])
  expect_equal(
    sort(screen_text$y[zaxis_label_rows]),
    zaxis_tick_y,
    tolerance = 1e-6
  )
  expect_equal(unique(screen_text$size[zaxis_label_rows]), 16 * 0.8)
  expect_equal(screen_text$size[zaxis_title_row], 16)
  expect_equal(
    screen_text$y[zaxis_title_row],
    mean(zaxis_tick_y),
    tolerance = 1e-6
  )
  expect_true(all(screen_text$occlusion))
  expect_equal(unique(screen_text$occlusion_mode), "label")
  expect_equal(unique(screen_text$occlusion_tolerance), 0.02)
  expect_equal(unique(screen_text$background_color), "yellow")
  expect_equal(unique(screen_text$background_alpha), 0.25)
  expect_equal(unique(screen_text$halo_color), "white")
  expect_equal(unique(screen_text$halo_expand), 2)
  expect_true(all(screen_line$occlusion))
  expect_equal(unique(screen_line$occlusion_mode), "line")
  expect_equal(unique(screen_line$occlusion_tolerance), 0.03)
  expect_equal(unique(screen_line$lineend), "butt")
  expect_false(any(screen_line$clip))

  world_scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    text_render = "world",
    line_render = "world"
  )
  expect_null(attr(world_scene, "screen_text"))
  expect_null(attr(world_scene, "screen_line"))
  expect_error(
    render_highquality(
      return_scene = TRUE,
      light = FALSE,
      text_occlusion_mode = "line"
    ),
    "`text_occlusion_mode` must be one of"
  )
  expect_error(
    render_highquality(
      return_scene = TRUE,
      light = FALSE,
      line_occlusion_mode = "label"
    ),
    "`line_occlusion_mode` must be one of"
  )
  expect_error(
    render_highquality(
      return_scene = TRUE,
      light = FALSE,
      screen_text_args = "not a list"
    ),
    "`screen_text_args` must be a named list"
  )
  expect_error(
    render_highquality(
      return_scene = TRUE,
      light = FALSE,
      screen_line_args = list(not_a_screen_line_arg = TRUE)
    ),
    "`screen_line_args` contains unsupported argument"
  )
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
