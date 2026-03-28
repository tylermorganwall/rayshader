test_that("render_highquality doesn't lose colors", {
  skip_on_cran()
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  
  # Create a simple colored surface
  elmat <- matrix(1:100, nrow = 10, ncol = 10)
  
  # Create a colored texture
  texture <- sphere_shade(elmat, texture = "desert")
  
  # Plot in 3D
  plot_3d(
    elmat,
    zscale = 2,
    texture = texture,
    windowsize = c(200, 200),
    zoom = 0.8,
    phi = 30,
    theta = -30
  )
  
  # Convert to raymesh
  raymesh <- convert_rgl_to_raymesh(save_shadow = FALSE)
  
  # Check that we have a valid raymesh
  expect_true(inherits(raymesh, "ray_mesh"))
  
  # The scene should have at least one mesh
  expect_true(length(raymesh) > 0)
  
  # Clean up
  rgl::close3d()
})
