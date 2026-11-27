test_that("render_points() filters point placements to the scene extent", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    sphere_shade(heightmap),
    heightmap,
    zscale = 1,
    shadow = FALSE,
    water = FALSE,
    window_size = c(250, 250),
    extent = extent
  ))

  expect_no_condition(render_points(
    x = c(10, 25),
    y = c(10, 10),
    altitude = c(0, 0),
    size = c(4, 8),
    color = c("red", "blue"),
    clear_previous = TRUE
  ))
  point_id = get_ids_with_labels()$id[get_ids_with_labels()$tag == "points3d"][
    1
  ]
  expect_equal(nrow(rgl::rgl.attrib(point_id, "vertices")), 1)

  expect_no_condition(render_points(
    x = c(10, 25),
    y = c(10, 10),
    altitude = c(0, 0),
    clear_previous = TRUE,
    filter_to_extent = FALSE
  ))
  point_id = get_ids_with_labels()$id[get_ids_with_labels()$tag == "points3d"][
    1
  ]
  expect_equal(nrow(rgl::rgl.attrib(point_id, "vertices")), 2)
})

test_that("ggplot filtering uses the panel extent instead of the full 3D extent", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  p = ggplot2::ggplot(mtcars) +
    ggplot2::geom_point(ggplot2::aes(wt, mpg))
  expect_no_condition(suppressWarnings(plot_gg_test(
    p,
    width = 3,
    height = 3,
    window_size = c(600, 600),
    raytrace = FALSE,
    shadow = FALSE
  )))
  scene_extent = get_ggplot_extent()
  panel_info = attr(scene_extent, "panel_info")
  x_outside_panel = mean(c(scene_extent["xmin"], panel_info$data_xmin))
  y_inside_panel = mean(c(panel_info$data_ymin, panel_info$data_ymax))

  expect_lt(x_outside_panel, panel_info$data_xmin)
  expect_gt(x_outside_panel, scene_extent["xmin"])

  expect_no_condition(render_points(
    x = x_outside_panel,
    y = y_inside_panel,
    altitude = 0,
    clear_previous = TRUE
  ))
  expect_false(any(get_ids_with_labels()$tag == "points3d"))

  expect_no_condition(render_points(
    x = x_outside_panel,
    y = y_inside_panel,
    altitude = 0,
    clear_previous = TRUE,
    filter_to_extent = FALSE
  ))
  expect_true(any(get_ids_with_labels()$tag == "points3d"))
})

test_that("render_path() crops spatial line inputs to the scene extent", {
  skip_if_not_installed("sf")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    sphere_shade(heightmap),
    heightmap,
    zscale = 1,
    shadow = FALSE,
    water = FALSE,
    window_size = c(250, 250),
    extent = extent
  ))
  line = sf::st_sfc(sf::st_linestring(matrix(
    c(-10, 10, 10, 10, 30, 10),
    ncol = 2,
    byrow = TRUE
  )))
  coords = render_path(
    lat = line,
    altitude = 0,
    return_coords = TRUE
  )
  cropped_line = suppressMessages(suppressWarnings(sf::st_crop(
    sf::st_sf(geometry = line),
    sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 20, ymax = 20))
  )))
  cropped_coords = sf::st_coordinates(cropped_line)
  expected = transform_into_heightmap_coords(
    extent = extent,
    heightmap = heightmap,
    lat = cropped_coords[, 2],
    long = cropped_coords[, 1],
    altitude = rep(0, nrow(cropped_coords)),
    zscale = 1,
    transform_scene = FALSE,
    caller = "test"
  )
  expect_equal(length(coords), 1)
  expect_equal(coords[[1]], expected, tolerance = 1e-6)
})

test_that("geographic extent clipping preserves southern polygons inside a wide panel", {
  skip_if_not_installed("sf")
  old_s2 = sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  extent = c(xmin = -128, xmax = -64, ymin = 23, ymax = 51)
  polygons = sf::st_sf(
    name = c("Texas tip", "Florida tip", "outside", "crossing"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-100, 25.84),
        c(-96, 25.84),
        c(-96, 30),
        c(-100, 30),
        c(-100, 25.84)
      ))),
      sf::st_polygon(list(rbind(
        c(-82, 24.56),
        c(-80, 24.56),
        c(-80, 28),
        c(-82, 28),
        c(-82, 24.56)
      ))),
      sf::st_polygon(list(rbind(
        c(-100, 20),
        c(-96, 20),
        c(-96, 21),
        c(-100, 21),
        c(-100, 20)
      ))),
      sf::st_polygon(list(rbind(
        c(-130, 30),
        c(-126, 30),
        c(-126, 32),
        c(-130, 32),
        c(-130, 30)
      ))),
      crs = 4269
    )
  )
  original = polygons
  for (use_s2 in c(TRUE, FALSE)) {
    suppressMessages(sf::sf_use_s2(use_s2))
    filtered = filter_scene_sf_to_extent(polygons, extent = extent)
    expect_identical(sf::sf_use_s2(), use_s2)
    expect_identical(filtered$source_index, c(1L, 2L, 4L))
    expect_identical(filtered$object$name, polygons$name[c(1, 2, 4)])
    expect_identical(sf::st_crs(filtered$object), sf::st_crs(polygons))
    for (i in 1:2) {
      expect_equal(
        sf::st_bbox(filtered$object[i, ]),
        sf::st_bbox(polygons[i, ])
      )
    }
    expect_equal(
      as.numeric(sf::st_bbox(filtered$object[3, ])),
      c(-128, 30, -126, 32)
    )
  }
  expect_identical(polygons, original)
})

test_that("geographic preserve_z filtering uses the planar extent without changing vertices", {
  skip_if_not_installed("sf")
  old_s2 = sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  polygon = sf::st_polygon(list(rbind(
    c(-100, 24, 7),
    c(-96, 24, 8),
    c(-96, 25, 9),
    c(-100, 25, 10),
    c(-100, 24, 7)
  )))
  polygons = sf::st_sfc(polygon, crs = 4269)
  filtered = filter_scene_sf_to_extent(
    polygons,
    extent = c(xmin = -128, xmax = -64, ymin = 23, ymax = 51),
    preserve_z = TRUE
  )
  expect_identical(filtered$source_index, 1L)
  expect_equal(
    sf::st_coordinates(filtered$object),
    sf::st_coordinates(polygons)
  )
  expect_identical(sf::st_crs(filtered$object), sf::st_crs(polygons))
  expect_true(sf::sf_use_s2())
})
