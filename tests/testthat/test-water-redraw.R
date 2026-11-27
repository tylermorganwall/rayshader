water_redraw_scene = function() {
  plot_3d_test(
    array(1, c(5, 5, 3)),
    matrix(0, 5, 5),
    zscale = 1,
    solid = FALSE,
    shadow = FALSE
  )
}

test_that('water defers visible redraws across meshes, lines and clearing', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  water_redraw_scene()
  triangle_states = line_states = clear_states = logical()
  triangles = rgl::triangles3d
  segments = rgl::segments3d
  pop = rgl::pop3d
  local_mocked_bindings(
    .package = 'rgl',
    triangles3d = function(...) {
      triangle_states <<- c(triangle_states, rgl::par3d('skipRedraw'))
      triangles(...)
    },
    segments3d = function(...) {
      line_states <<- c(line_states, rgl::par3d('skipRedraw'))
      segments(...)
    },
    pop3d = function(...) {
      clear_states <<- c(clear_states, rgl::par3d('skipRedraw'))
      pop(...)
    }
  )
  render_water(1, water_line_color = 'white')
  expect_gt(length(triangle_states), 0L)
  expect_gt(length(line_states), 0L)
  expect_gt(length(clear_states), 0L)
  expect_true(all(c(triangle_states, line_states, clear_states)))
  expect_false(rgl::par3d('skipRedraw'))
  ids = rgl::ids3d(tags = TRUE)
  expect_true(any(ids$tag == 'water'))
  expect_true(any(ids$tag == 'waterlines'))
  render_water(clear_previous = TRUE)
  expect_false(rgl::par3d('skipRedraw'))
  expect_false(any(rgl::ids3d(tags = TRUE)$tag %in% c('water', 'waterlines')))
  expect_true(all(clear_states))
})

test_that('water retains caller-owned redraw suppression', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  water_redraw_scene()
  rgl::par3d(skipRedraw = TRUE)
  render_water(1)
  expect_true(rgl::par3d('skipRedraw'))
  render_water(clear_previous = TRUE)
  expect_true(rgl::par3d('skipRedraw'))
})

test_that('water restores redraw state when construction fails', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  water_redraw_scene()
  local_mocked_bindings(.package = 'rayshader', make_water = function(...) {
    expect_true(rgl::par3d('skipRedraw'))
    stop('construction failed')
  })
  for (prior in c(FALSE, TRUE)) {
    rgl::par3d(skipRedraw = prior)
    expect_error(render_water(1), 'construction failed')
    expect_identical(rgl::par3d('skipRedraw'), prior)
  }
})

test_that('water restores redraw state on interruption', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  water_redraw_scene()
  local_mocked_bindings(.package = 'rayshader', make_water = function(...) {
    signalCondition(structure(
      list(message = 'test cancellation', call = NULL),
      class = c('interrupt', 'condition')
    ))
  })
  interrupted = tryCatch(render_water(1), interrupt = function(e) TRUE)
  expect_true(interrupted)
  expect_false(rgl::par3d('skipRedraw'))
})

test_that('redraw cleanup does not reopen a closed device', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  water_redraw_scene()
  local_mocked_bindings(.package = 'rayshader', make_water = function(...) {
    rgl::close3d()
    stop('device closed')
  })
  expect_error(render_water(1), 'device closed')
  expect_identical(rgl::cur3d(), 0L)
})
