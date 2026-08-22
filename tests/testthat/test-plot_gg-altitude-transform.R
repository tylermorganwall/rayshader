library(ggplot2)

test_that("ggplot scenes transform mapped overlay altitudes into scene units", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg, color = mpg))

  expect_no_condition(suppressWarnings(plot_gg_test(
    p,
    width = 2,
    raytrace = FALSE,
    windowsize = c(300, 300)
  )))

  gg_extent = rayshader:::get_ggplot_extent()
  scene_heightmap = get_scene_heightmap()
  scene_zscale = get_scene_effective_zscale()
  altitude_vals = c(min(mtcars$disp), max(mtcars$disp))
  scene_height_range = range(scene_heightmap[is.finite(scene_heightmap)])

  xyz = transform_into_heightmap_coords(
    extent = gg_extent,
    heightmap = scene_heightmap,
    lat = c(min(mtcars$mpg), max(mtcars$mpg)),
    long = c(min(mtcars$wt), max(mtcars$wt)),
    altitude = altitude_vals,
    offset = 0,
    zscale = scene_zscale
  )

  expected_y = scales::rescale(
    altitude_vals,
    to = scene_height_range,
    from = range(altitude_vals)
  ) /
    scene_zscale

  expect_equal(xyz[, 2], expected_y, tolerance = 1e-6)
})

test_that("ggplot scenes without mapped height keep raw overlay altitudes", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg))

  expect_no_condition(suppressWarnings(plot_gg_test(
    p,
    width = 2,
    raytrace = FALSE,
    windowsize = c(300, 300)
  )))

  gg_extent = rayshader:::get_ggplot_extent()
  scene_zscale = get_scene_effective_zscale()
  altitude_vals = c(100, 200)

  xyz = transform_into_heightmap_coords(
    extent = gg_extent,
    heightmap = get_scene_heightmap(),
    lat = c(15, 30),
    long = c(2, 4),
    altitude = altitude_vals,
    offset = 0,
    zscale = scene_zscale
  )

  expect_equal(xyz[, 2], altitude_vals / scene_zscale, tolerance = 1e-6)
})

test_that("flat substrate ggplot scenes keep panels flat but map overlay altitudes", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg, color = mpg))

  height_matrix = suppressWarnings(plot_gg_test(
    p,
    width = 2,
    raytrace = FALSE,
    flat_substrate = TRUE,
    save_height_matrix = TRUE,
    windowsize = c(300, 300)
  ))

  panel_info = attr(height_matrix, "ggplot_panel_info", exact = TRUE)
  panel_mask = matrix(FALSE, nrow(height_matrix), ncol(height_matrix))
  for (i in seq_len(nrow(panel_info))) {
    panel_rows = floor(panel_info$panel_xmin[i]):ceiling(panel_info$panel_xmax[
      i
    ])
    panel_cols = floor(panel_info$panel_ymin[i]):ceiling(panel_info$panel_ymax[
      i
    ])
    panel_rows = pmax(1, pmin(nrow(height_matrix), panel_rows))
    panel_cols = pmax(1, pmin(ncol(height_matrix), panel_cols))
    panel_mask[panel_rows, panel_cols] = TRUE
  }

  expect_equal(range(height_matrix[panel_mask]), c(0, 0), tolerance = 1e-12)
  expect_gt(max(height_matrix[!panel_mask], na.rm = TRUE), 0)
  surface_id = get_ids_with_labels(typeval = "surface_tris")$id[1]
  surface_vertices = rgl::rgl.attrib(surface_id, "vertices")
  expect_equal(min(surface_vertices[, 2]), 0, tolerance = 1e-12)
  expect_gt(max(surface_vertices[, 2]), 0)
  expect_gt(nrow(get_ids_with_labels(typeval = "base")), 0)
  transform_info = get_cached_plot_gg_transform_info(default = NULL)
  expect_true(transform_info$height_is_mapped)

  gg_extent = rayshader:::get_ggplot_extent()
  scene_height_range = range(height_matrix[is.finite(height_matrix)])
  scene_zscale = get_scene_effective_zscale()
  altitude_vals = c(100, 200)

  xyz = transform_into_heightmap_coords(
    extent = gg_extent,
    heightmap = get_scene_heightmap(),
    lat = c(15, 30),
    long = c(2, 4),
    altitude = altitude_vals,
    offset = 0,
    zscale = scene_zscale
  )

  expected_y = scales::rescale(
    altitude_vals,
    to = scene_height_range,
    from = range(altitude_vals)
  ) /
    scene_zscale

  expect_equal(xyz[, 2], expected_y, tolerance = 1e-6)
})

test_that("render point colors can use cached ggplot height palette", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg, color = mpg)) +
    scale_color_gradient(
      limits = c(10, 35),
      low = "#0000FF",
      high = "#FF0000"
    )

  suppressWarnings(plot_gg_test(
    p,
    width = 2,
    height_aes = "color",
    raytrace = FALSE,
    flat_substrate = TRUE,
    windowsize = c(300, 300)
  ))
  expect_true(
    rayshader:::get_cached_plot_gg_transform_info(
      default = NULL
    )$height_is_mapped
  )

  altitude_vals = c(10, 20, 35)
  mapped_colors = rayshader:::map_plot_gg_height_palette(
    altitude_vals,
    caller = "render_points"
  )
  expect_equal(mapped_colors[c(1, 3)], c("#0000FF", "#FF0000"))

  expect_no_condition(render_points(
    x = c(2, 3, 4),
    y = c(15, 20, 25),
    altitude = altitude_vals,
    color = "height",
    size = 5
  ))

  rgl_ids = rgl::ids3d(tags = TRUE)
  point_id = rgl_ids$id[rgl_ids$tag == "points3d"][1]
  actual_colors = rgl::rgl.attrib(point_id, "colors")
  point_vertices = rgl::rgl.attrib(point_id, "vertices")
  expected_colors = t(grDevices::col2rgb(mapped_colors, alpha = TRUE) / 255)
  expected_y = scales::rescale(
    altitude_vals,
    to = range(get_scene_heightmap()[is.finite(get_scene_heightmap())]),
    from = range(altitude_vals)
  ) /
    get_scene_effective_zscale()

  expect_equal(unname(actual_colors), unname(expected_colors), tolerance = 1e-6)
  expect_equal(point_vertices[, 2], expected_y, tolerance = 1e-6)
})

test_that("render_label() uses cached plot_gg height scale and vertical exaggeration", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot(mtcars) +
    geom_point(aes(x = mpg, y = wt, color = hp))

  suppressWarnings(plot_gg_test(
    p,
    width = 2,
    height_aes = "color",
    raytrace = FALSE,
    flat_substrate = TRUE,
    vertical_exaggeration = 600,
    windowsize = c(300, 300)
  ))

  expect_no_condition(render_points(
    x = mtcars$mpg,
    y = mtcars$wt,
    altitude = mtcars$hp,
    size = 5,
    color = "black"
  ))
  label_index = order(mtcars$hp, decreasing = TRUE)[1]
  ids = get_ids_with_labels()
  point_id = ids$id[ids$tag == "points3d"][1]
  point_vertices = rgl::rgl.attrib(point_id, "vertices")

  expect_no_condition(render_label(
    x = mtcars$mpg[label_index],
    y = mtcars$wt[label_index],
    altitude = mtcars$hp[label_index],
    relativez = TRUE,
    line = FALSE,
    text = rownames(mtcars)[label_index],
    freetype = FALSE,
    clear_previous = TRUE
  ))

  ids = get_ids_with_labels()
  label_id = ids$id[ids$tag == "raytext"][1]
  label_vertices = rgl::rgl.attrib(label_id, "vertices")

  expect_equal(
    unname(label_vertices[1, 2]),
    unname(point_vertices[label_index, 2]),
    tolerance = 1e-6
  )
})

test_that("ggplot z-axis breaks use mapped height positions but keep raw labels", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg, color = mpg)) +
    labs(color = "Miles per gallon")

  expect_no_condition(suppressWarnings(plot_gg_test(
    p,
    width = 2,
    raytrace = FALSE,
    windowsize = c(300, 300)
  )))

  gg_extent = rayshader:::get_ggplot_extent()
  scene_heightmap = get_scene_heightmap()
  scene_zscale = get_scene_effective_zscale()
  altitude_vals = c(min(mtcars$disp), max(mtcars$disp))
  breaks = altitude_vals + c(10, -10)
  labels = c("low", "high")
  scene_height_range = range(scene_heightmap[is.finite(scene_heightmap)])

  expect_no_condition(render_points(
    x = c(min(mtcars$wt), max(mtcars$wt)),
    y = c(min(mtcars$mpg), max(mtcars$mpg)),
    extent = gg_extent,
    altitude = altitude_vals,
    color = "red",
    size = 4,
    clear_previous = TRUE
  ))
  expect_no_condition(render_zaxis(
    zaxis_data = "point",
    zaxis_breaks = breaks,
    zaxis_labels = labels
  ))

  ids = get_ids_with_labels()
  tick_id = ids$id[ids$tag == "zaxis_ticks"][1]
  tick_verts = rgl::rgl.attrib(tick_id, "vertices")
  label_ids = ids$id[ids$tag == "zaxis_labels"]
  label_texts = unlist(lapply(
    label_ids,
    function(id) trimws(as.character(rgl::rgl.attrib(id, "texts")))
  ))

  expected_y = sort(
    scales::rescale(
      breaks,
      to = scene_height_range,
      from = range(altitude_vals)
    ) /
      scene_zscale
  )

  expect_equal(sort(tick_verts[, 2]), expected_y, tolerance = 1e-6)
  expect_true(all(labels %in% label_texts))
})

test_that("standalone ggplot z-axis defaults use mapped height scale labels", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot(mtcars) +
    geom_point(aes(x = wt, y = mpg, color = mpg)) +
    labs(color = "Miles per gallon")

  expect_no_condition(suppressWarnings(plot_gg_test(
    p,
    width = 2,
    raytrace = FALSE,
    windowsize = c(300, 300)
  )))

  gg_extent = get_ggplot_extent()
  scene_heightmap = get_scene_heightmap()
  scene_zscale = get_scene_effective_zscale()
  height_transform = get_scene_height_transform(
    heightmap = scene_heightmap,
    extent = gg_extent
  )
  raw_range = range(as.numeric(height_transform$height_range))
  raw_breaks = pretty(raw_range, n = 4)
  raw_breaks = raw_breaks[is.finite(raw_breaks)]
  scene_breaks = map_scene_altitudes(
    raw_breaks,
    height_transform = height_transform,
    reference_values = raw_range
  )
  draw_idx = abs(scene_breaks) > .Machine$double.eps^0.5
  expected_y = sort(scene_breaks[draw_idx] / scene_zscale)
  expected_labels = format(
    raw_breaks,
    trim = TRUE,
    scientific = FALSE
  )[draw_idx]

  expect_no_condition(render_zaxis(zaxis_location = "panel_bottomleft"))

  ids = get_ids_with_labels()
  tick_id = ids$id[ids$tag == "zaxis_ticks"][1]
  tick_verts = rgl::rgl.attrib(tick_id, "vertices")
  label_ids = ids$id[ids$tag == "zaxis_labels"]
  label_texts = unlist(lapply(
    label_ids,
    function(id) trimws(as.character(rgl::rgl.attrib(id, "texts")))
  ))
  title_id = ids$id[ids$tag == "zaxis_title"][1]
  title_text = paste0(
    as.character(rgl::rgl.attrib(title_id, "texts")),
    collapse = ""
  )

  expect_equal(sort(tick_verts[, 2]), expected_y, tolerance = 1e-6)
  expect_true(all(expected_labels %in% label_texts))
  expect_gt(max(suppressWarnings(as.numeric(label_texts)), na.rm = TRUE), 1)
  expect_equal(title_text, "Miles per gallon")
})

test_that("standalone ggplot z-axis auto-title uses implicit mapped height label", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot(mtcars) +
    geom_point(aes(x = mpg, y = disp, color = cyl)) +
    scale_color_continuous(limits = c(0, 8))

  expect_no_condition(suppressWarnings(plot_gg_test(
    p,
    width = 2,
    raytrace = FALSE,
    windowsize = c(300, 300)
  )))

  transform_info = get_cached_plot_gg_transform_info(default = NULL)
  expect_equal(transform_info$height_label, "cyl")

  expect_no_condition(render_zaxis(zaxis_location = "panel_bottomleft"))

  ids = get_ids_with_labels()
  axis_id = ids$id[ids$tag == "zaxis_axis"][1]
  title_id = ids$id[ids$tag == "zaxis_title"][1]
  axis_verts = rgl::rgl.attrib(axis_id, "vertices")
  title_verts = rgl::rgl.attrib(title_id, "vertices")
  title_text = paste0(
    as.character(rgl::rgl.attrib(title_id, "texts")),
    collapse = ""
  )
  title_gap = sqrt(
    (title_verts[1, 1] - axis_verts[1, 1])^2 +
      (title_verts[1, 3] - axis_verts[1, 3])^2
  )
  axis_span = diff(range(axis_verts[, 2]))

  expect_equal(title_text, "cyl")
  expect_lt(title_gap / axis_span, 0.15)
})

test_that("plot_3d scenes keep raw altitude values in transform_into_heightmap_coords", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = volcano
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(300, 300)
  ))

  extent = c(
    xmin = 0,
    xmax = nrow(heightmap),
    ymin = 0,
    ymax = ncol(heightmap)
  )
  altitude_vals = c(100, 200)
  xyz = transform_into_heightmap_coords(
    extent = extent,
    heightmap = heightmap,
    lat = c(10, 20),
    long = c(10, 20),
    altitude = altitude_vals,
    offset = 0,
    zscale = 10
  )

  expect_equal(xyz[, 2], altitude_vals / 10, tolerance = 1e-6)
})

test_that("transform_into_heightmap_coords() does not warn about derived altitude when altitude is explicit", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = volcano
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(300, 300)
  ))

  extent = c(
    xmin = 0,
    xmax = nrow(heightmap),
    ymin = 0,
    ymax = ncol(heightmap)
  )
  expect_warning(
    transform_into_heightmap_coords(
      extent = extent,
      heightmap = heightmap,
      lat = -100,
      long = -100,
      offset = 0,
      zscale = 10,
      filter_bounds = TRUE
    ),
    "altitude of those points"
  )
  expect_no_warning(transform_into_heightmap_coords(
    extent = extent,
    heightmap = heightmap,
    lat = -100,
    long = -100,
    altitude = 1000,
    offset = 0,
    zscale = 10,
    filter_bounds = TRUE
  ))
})
