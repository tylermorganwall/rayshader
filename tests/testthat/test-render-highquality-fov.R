test_that("explicit high-quality FOV overrides rgl without changing its camera", {
  skip_if_not_installed("rayrender")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(0, 4, 4)
  plot_3d_test(
    constant_shade(heightmap),
    heightmap,
    solid = FALSE,
    shadow = FALSE
  )
  rendered = NULL
  local_mocked_bindings(
    render_scene = function(...) {
      rendered <<- list(...)
      "render-result"
    },
    .package = "rayrender"
  )

  for (preview_fov in c(35, 0)) {
    rgl::par3d(FOV = preview_fov, zoom = 0.65)
    projection = rgl::par3d("projMatrix")
    expected = if (preview_fov == 0) {
      0
    } else {
      2 * atan(1 / projection[2, 2]) * 180 / pi
    }
    render_highquality(light = FALSE, plot = FALSE)
    expect_equal(rendered$fov, expected)
    render_highquality(fov = NULL, light = FALSE, plot = FALSE)
    expect_equal(rendered$fov, expected)

    for (override in c(55, 0, 360)) {
      expect_identical(
        render_highquality(fov = override, light = FALSE, plot = FALSE),
        "render-result"
      )
      expect_equal(rendered$fov, override)
      expect_equal(sum(names(rendered) == "fov"), 1)
      if (override == 0) {
        expect_equal(
          rendered$ortho_dimensions,
          c(2 / projection[1, 1], 2 / projection[2, 2])
        )
      }
      expect_equal(rgl::par3d("FOV"), preview_fov)
      expect_equal(rgl::par3d("projMatrix"), projection)
    }
    render_highquality(
      fov = 0,
      ortho_dimensions = c(20, 10),
      light = FALSE,
      plot = FALSE
    )
    expect_equal(rendered$ortho_dimensions, c(20, 10))
  }
})

test_that("high-quality FOV rejects invalid scalar values", {
  for (invalid in list(NA_real_, Inf, NaN, numeric(), c(30, 40), "30", TRUE)) {
    expect_error(
      render_highquality(fov = invalid),
      "`fov` must be NULL or a single finite number",
      fixed = TRUE
    )
  }
})
