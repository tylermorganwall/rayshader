test_that("screen text preserves rgl vertical alignment and unclamped offsets", {
  for (vertical in c(-0.5, 0, 0.5, 1, 1.5)) {
    adj = c(0.75, vertical)
    expect_equal(get_render_highquality_screen_text_just(adj, 1), 0.75)
    expect_equal(get_render_highquality_screen_text_just(adj, 2), 1 - vertical)
    expect_equal(
      get_render_highquality_screen_text_just(rbind(c(0, 0), adj), 2, 2),
      1 - vertical
    )
  }
  expect_equal(get_render_highquality_screen_text_just(NULL, 2), 0.5)
})

test_that("top z-axis titles preserve rgl anchors and bottom alignment", {
  skip_if_not_installed("rayrender")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(0, 20, 40)
  plot_3d_test(
    constant_shade(heightmap),
    heightmap,
    solid = FALSE,
    shadow = FALSE,
    window_size = c(1500, 500)
  )
  captured = NULL
  local_mocked_bindings(
    render_scene = function(...) {
      captured <<- list(...)
      invisible(NULL)
    },
    .package = "rayrender"
  )
  for (camera_fov in c(0, 30)) {
    render_camera(theta = 25, phi = 25, zoom = 0.35, fov = camera_fov)
    render_zaxis(
      breaks = c(0, 10, 20),
      title = "Births (1974)",
      title_location = "top",
      title_offset = 1.5,
      label_size = 1,
      title_size = 1.1
    )
    title_id = subset(rgl::ids3d(tags = TRUE), tag == "zaxis_title")$id
    anchor = rgl::rgl.attrib(tail(title_id, 1), "vertices")
    rgl_position = unname(rgl::rgl.user2window(anchor))
    render_highquality(light = FALSE, plot = FALSE, text_size = 20)
    labels = captured$screen_text
    title = tail(labels[labels$label == "Births (1974)", ], 1)
    # rgl adj_y=0 anchors the bottom of the title; rayrender's equivalent is 1.
    expect_equal(title$vjust, 1)
    expect_equal(title$hjust, 1)
    expect_equal(title$size, 22)
    ticks = labels[trimws(labels$label) %in% c("0", "10", "20"), ]
    expect_true(all(ticks$vjust == 0.5))
    projected = rayrender:::project_points_to_screen(
      as.matrix(title[, c("x", "y", "z")]),
      list(
        lookfrom = captured$lookfrom,
        lookat = captured$lookat,
        camera_up = c(0, 1, 0),
        fov = captured$fov,
        ortho_dimensions = captured$ortho_dimensions,
        nx = captured$width,
        ny = captured$height
      )
    )
    # Compare independent rgl and rayrender camera projections in output pixels.
    expect_equal(
      projected$x,
      1 + rgl_position[, 1] * (captured$width - 1),
      tolerance = 1e-5
    )
    expect_equal(
      projected$y,
      1 + (1 - rgl_position[, 2]) * (captured$height - 1),
      tolerance = 1e-5
    )
  }
})
