test_that("height mapping uses trained limits for single values and subsets", {
  p = ggplot2::ggplot(data.frame(x = 1:3, value = c(20, 50, 80))) +
    ggplot2::geom_point(ggplot2::aes(x, x, colour = value)) +
    ggplot2::scale_colour_continuous(limits = c(0, 100))
  scale = ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  mapping = list(height_scale = scale, height_inverted = FALSE)
  colors_before = scale$map(c(20, 50, 80))
  expect_equal(map_scene_altitudes(c(20, 50, 80), mapping), c(.2, .5, .8))
  expect_equal(map_scene_altitudes(50, mapping), .5)
  expect_equal(map_scene_altitudes(c(50, 80), mapping), c(.5, .8))
  expect_equal(map_scene_altitudes(c(50, 50, NA), mapping), c(.5, .5, NA))
  expect_equal(map_scene_altitudes(c(-1, 101), mapping), c(0, 0))
  expect_equal(scale$map(c(20, 50, 80)), colors_before)
  mapping$height_inverted = TRUE
  expect_equal(map_scene_altitudes(c(20, 50, 80), mapping), c(.8, .5, .2))
})

test_that("height mapping respects nonlinear scales and out-of-bounds policies", {
  data = data.frame(x = 1:3, value = c(1, 10, 100))
  for (trans in c("log10", "sqrt", "reverse")) {
    p = ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x, x, colour = value)) +
      ggplot2::scale_colour_continuous(
        transform = trans,
        limits = c(1, 100),
        oob = scales::squish
      )
    scale = ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
    mapping = list(height_scale = scale, height_inverted = FALSE)
    expected = switch(
      trans,
      log10 = .5,
      sqrt = (sqrt(10) - 1) / 9,
      reverse = 90 / 99
    )
    expect_equal(map_scene_altitudes(10, mapping), expected)
    expect_equal(
      map_scene_altitudes(1000, mapping),
      if (trans == "reverse") 0 else 1
    )
    mapping$height_inverted = TRUE
    expect_equal(map_scene_altitudes(10, mapping), 1 - expected)
  }
  expect_equal(map_scene_altitudes(c(10, 100), NULL), c(10, 100))
})

test_that("height mapping retains ggplot bin boundaries", {
  p = ggplot2::ggplot(data.frame(x = 1:3, value = c(0, 50, 100))) +
    ggplot2::geom_point(ggplot2::aes(x, x, colour = value)) +
    ggplot2::scale_colour_steps(limits = c(0, 100), breaks = 50)
  scale = ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  mapping = list(height_scale = scale, height_inverted = FALSE)
  expect_equal(map_scene_altitudes(c(25, 75), mapping), c(.25, .75))
  expect_equal(map_scene_altitudes(75, mapping), .75)
})

test_that("polygon columns and point altitudes share the ggplot height scale", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  polygons = sf::st_sf(
    value = c(10, 100),
    lower = c(1, 10),
    geometry = sf::st_sfc(
      lapply(0:1, function(x) {
        sf::st_polygon(list(rbind(
          c(x, 0),
          c(x + .8, 0),
          c(x + .8, 1),
          c(x, 1),
          c(x, 0)
        )))
      }),
      crs = 3857
    )
  )
  original = polygons
  p = ggplot2::ggplot(polygons) +
    ggplot2::geom_sf(ggplot2::aes(fill = value), colour = NA) +
    ggplot2::scale_fill_continuous(transform = "log10", limits = c(1, 1000)) +
    ggplot2::theme(legend.position = "none")
  suppressWarnings(plot_gg_test(
    p,
    width = 2,
    height = 2,
    raytrace = FALSE,
    flat_substrate = TRUE,
    height_aes = "fill"
  ))
  zscale = get_scene_effective_zscale()
  expect_equal(
    get_scene_height_transform(
      get_scene_heightmap(),
      get_ggplot_extent()
    )$height_range,
    c(1, 1000)
  )
  render_polygons(
    polygons,
    data_column_top = "value",
    bottom = 0,
    color = "height",
    lit = FALSE
  )
  ids = get_ids_with_labels(typeval = "polygon3d")$id
  heights = lapply(ids, function(id) {
    range(rgl::rgl.attrib(id, "vertices")[, 2])
  })
  expect_equal(heights[[1]], c(0, 1 / 3) / zscale, tolerance = 1e-6)
  expect_equal(heights[[2]], c(0, 2 / 3) / zscale, tolerance = 1e-6)
  expected_color = ggplot2::ggplot_build(p)$data[[1]]$fill[1]
  expect_equal(map_plot_gg_height_palette(10), expected_color)
  expect_equal(
    unname(rgl::rgl.attrib(ids[1], "colors")[1, 1:3]),
    unname(grDevices::col2rgb(expected_color)[, 1] / 255),
    tolerance = 1e-6
  )
  render_polygons(
    polygons[2, ],
    data_column_top = "value",
    data_column_bottom = "lower",
    bottom = 0,
    lit = FALSE,
    clear_previous = TRUE
  )
  id = get_ids_with_labels(typeval = "polygon3d")$id[1]
  expect_equal(
    range(rgl::rgl.attrib(id, "vertices")[, 2]),
    c(1 / 3, 2 / 3) / zscale,
    tolerance = 1e-6
  )
  render_points(x = 1.4, y = .5, altitude = 100, crs = 3857)
  id = get_ids_with_labels(typeval = "points3d")$id[1]
  expect_equal(
    unname(rgl::rgl.attrib(id, "vertices")[1, 2]),
    (2 / 3) / zscale,
    tolerance = 1e-6
  )
  render_polygons(
    polygons[2, ],
    data_column_top = "value",
    bottom = 0,
    scale_data = .01,
    lit = FALSE,
    clear_previous = TRUE
  )
  id = get_ids_with_labels(typeval = "polygon3d")$id[1]
  expect_equal(
    range(rgl::rgl.attrib(id, "vertices")[, 2]),
    c(0, 1) / zscale,
    tolerance = 1e-6
  )
  suppressWarnings(plot_gg_test(
    p,
    width = 2,
    height = 2,
    raytrace = FALSE,
    flat_substrate = TRUE,
    height_aes = "fill",
    invert = TRUE
  ))
  zscale = get_scene_effective_zscale()
  render_polygons(
    polygons[1, ],
    data_column_top = "value",
    bottom = 0,
    lit = FALSE
  )
  id = get_ids_with_labels(typeval = "polygon3d")$id[1]
  expect_equal(
    range(rgl::rgl.attrib(id, "vertices")[, 2]),
    c(0, 2 / 3) / zscale,
    tolerance = 1e-6
  )
  expect_equal(polygons, original)
})
