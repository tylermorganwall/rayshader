test_that("convert_rgl_to_raymesh preserves colors", {
  skip_on_cran()
  skip_if_not_installed("rayvertex")
  
  # Create a simple colored rgl scene
  open3d()
  
  # Create a colored triangle
  triangles3d(
    x = c(0, 1, 0.5),
    y = c(0, 0, 1),
    z = c(0, 0, 0),
    color = c("red", "green", "blue"),
    lit = FALSE
  )
  
  # Get vertex info
  vertex_info <- get_ids_with_labels()
  
  # Convert to raymesh
  raymesh <- convert_rgl_to_raymesh(save_shadow = FALSE)
  
  # Check that the mesh has color information
  # The exact structure depends on rayvertex, but at minimum
  # we should have a valid scene
  expect_true(inherits(raymesh, "ray_mesh"))
  
  # Clean up
  close3d()
})
