local_rgl_use_null = function(env = parent.frame()) {
  old_options = options(rgl.useNULL = TRUE)
  withr::defer(options(old_options), envir = env)
  invisible(NULL)
}

disable_rgl_test_wheel = function() {
  if (rgl::cur3d() == 0) {
    return(invisible(NULL))
  }
  mouse_mode = rgl::par3d("mouseMode")
  if (!("wheel" %in% names(mouse_mode))) {
    return(invisible(NULL))
  }
  mouse_mode["wheel"] = "none"
  rgl::par3d(mouseMode = mouse_mode)
  invisible(NULL)
}

with_rgl_test_wheel_disabled = function(expr) {
  vis = withVisible(force(expr))
  disable_rgl_test_wheel()
  if (isTRUE(vis$visible)) {
    vis$value
  } else {
    invisible(vis$value)
  }
}

plot_3d_test = function(...) {
  with_rgl_test_wheel_disabled(plot_3d(...))
}

plot_gg_test = function(...) {
  with_rgl_test_wheel_disabled(plot_gg(...))
}
