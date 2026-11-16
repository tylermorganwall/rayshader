test_that("movie scene extraction retains z-axis screen labels and lines", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(1:100, 10, 10)
  plot_3d_test(
    constant_shade(heightmap),
    heightmap,
    zscale = 10,
    shadow = FALSE,
    window_size = c(100, 100)
  )
  render_zaxis(breaks = c(0, 50, 100), title = "Horsepower (hp)")
  captured = NULL
  testthat::local_mocked_bindings(
    render_animation_rcpp = function(scene, render_info, ...) {
      captured <<- render_info
      list()
    },
    .package = "rayrender"
  )
  keyframes = data.frame(
    x = c(0, 2),
    y = c(10, 10),
    z = c(30, 30),
    dx = 0,
    dy = 0,
    dz = 0,
    aperture = 0,
    fov = 40,
    focal = 30,
    orthox = 1,
    orthoy = 1,
    upx = 0,
    upy = 1,
    upz = 0
  )
  render_movie_hq(
    keyframes = keyframes,
    frames = 2,
    render_highquality_args = list(light = FALSE),
    preview = FALSE,
    plot_scene = FALSE,
    progress = FALSE,
    denoise = FALSE,
    samples = 1
  )
  expect_true(captured$screen_text_preview$active)
  expect_true(captured$screen_line_preview$active)
  expect_gte(length(captured$screen_text_preview$overlays), 4)
  expect_gt(length(captured$screen_line_preview$lines), 0)

  # Explicit overrides passed to the movie renderer can still suppress overlays.
  render_movie_hq(
    keyframes = keyframes,
    frames = 2,
    render_highquality_args = list(light = FALSE),
    screen_text = NULL,
    screen_line = NULL,
    preview = FALSE,
    plot_scene = FALSE,
    progress = FALSE,
    denoise = FALSE,
    samples = 1
  )
  expect_false(captured$screen_text_preview$active)
  expect_false(captured$screen_line_preview$active)

  # The direct high-quality animation entry point must preserve the same layers.
  render_highquality(
    animation_camera_coords = keyframes,
    light = FALSE,
    preview = FALSE,
    plot_scene = FALSE,
    progress = FALSE,
    denoise = FALSE,
    samples = 1
  )
  expect_true(captured$screen_text_preview$active)
  expect_true(captured$screen_line_preview$active)
})
