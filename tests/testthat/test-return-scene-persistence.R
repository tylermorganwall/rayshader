test_that("returned scenes retain resolved camera, settings and textures", {
  skip_if_not_installed("rayrender")
  skip_if_not("add_camera" %in% getNamespaceExports("rayrender"))
  rgl::open3d(useNULL = TRUE)
  on.exit(rgl::close3d())
  heights = outer(seq_len(5), seq_len(5), function(x, y) sin(x) * cos(y))
  plot_3d(height_shade(heights), heights, zscale = 2, window_size = c(120, 80))
  returned = render_highquality(
    return_scene = TRUE,
    width = 32,
    height = 24,
    samples = 1,
    iso = 2.5,
    fov = 35,
    aperture = 0,
    preview = FALSE,
    parallel = FALSE,
    denoise = FALSE,
    progress = FALSE,
    plot = FALSE
  )
  camera = rayrender::get_camera(returned)
  expect_equal(camera$iso, 2.5)
  expect_equal(camera$motion$fov, 35)
  expect_equal(camera$motion$aperture, 0)
  expect_equal(attr(returned, "ray_render_settings")$width, 32)
  expect_equal(dim(rayrender::render_scene(returned))[1:2], c(24, 32))
})
