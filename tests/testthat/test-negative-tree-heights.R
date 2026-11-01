test_that('negative tree sizes are dropped with associated instance attributes', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  h = matrix(0, 20, 20)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  plot_3d_test(height_shade(h), h, shadow = FALSE, water = FALSE)
  angles = cbind(0, c(10, 20, 30), 0)
  render_tree(
    x = c(5, 10, 15),
    y = c(5, 10, 15),
    extent = extent,
    tree_height = c(5, -2, 10),
    angle = angles,
    crown_color = c('red', 'green', 'blue'),
    trunk_color = c('black', 'white', 'brown'),
    crown_width = c(2, 3, 4),
    trunk_radius = c(.2, .3, .4),
    preview_simple = 'none',
    clear_previous = TRUE
  )
  actual = get_scene_tree_instances()
  render_tree(
    x = c(5, 15),
    y = c(5, 15),
    extent = extent,
    tree_height = c(5, 10),
    angle = angles[c(1, 3), , drop = FALSE],
    crown_color = c('red', 'blue'),
    trunk_color = c('black', 'brown'),
    crown_width = c(2, 4),
    trunk_radius = c(.2, .4),
    preview_simple = 'none',
    clear_previous = TRUE
  )
  expect_identical(actual, get_scene_tree_instances())
  render_tree(
    x = c(5, 10),
    y = c(5, 10),
    extent = extent,
    tree_height = -1,
    preview_simple = 'none',
    clear_previous = TRUE
  )
  expect_null(get_scene_tree_instances())
  render_tree(
    x = c(5, 10),
    y = c(5, 10),
    extent = extent,
    tree_height = c(-1, -2),
    clear_previous = TRUE
  )
  expect_false(any(rgl::ids3d(tags = TRUE)$tag == 'objtree'))
  # Negative absolute elevations can still describe valid trees below sea level.
  h = matrix(-10, 20, 20)
  plot_3d_test(height_shade(h), h, shadow = FALSE, water = FALSE, zscale = 1)
  render_tree(
    x = c(5, 10),
    y = c(5, 10),
    extent = extent,
    tree_height = c(-5, -12),
    absolute_height = TRUE,
    preview_simple = 'none',
    clear_previous = TRUE
  )
  expect_equal(nrow(get_scene_tree_instances()[[1]]), 2L)
})
