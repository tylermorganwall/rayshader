test_that("old color arguments forward required values and explicit NULL", {
  expected = create_texture(
    'white',
    'black',
    'red',
    'blue',
    'grey',
    corner_colors = NULL
  )
  expect_warning(
    actual <- create_texture(
      lightcolor = 'white',
      shadowcolor = 'black',
      leftcolor = 'red',
      rightcolor = 'blue',
      centercolor = 'grey',
      cornercolors = NULL
    ),
    'lightcolor.*deprecated.*light_color'
  )
  expect_equal(actual, expected)
  expect_false(any(
    c('lightcolor', 'shadowcolor', 'cornercolors') %in%
      names(formals(create_texture))
  ))
  expect_true('...' %in% names(formals(create_texture)))
})

test_that("old shading names preserve promises and current-name behavior", {
  terrain = matrix(seq_len(25), 5)
  evaluations = 0L
  expect_warning(
    actual <- lamb_shade(
      terrain,
      sunangle = {
        evaluations = evaluations + 1L
        120
      },
      sunaltitude = 30
    ),
    'sunangle.*deprecated.*sun_angle'
  )
  expect_equal(evaluations, 1L)
  expect_equal(
    as.numeric(actual),
    as.numeric(lamb_shade(terrain, sun_angle = 120, sun_altitude = 30))
  )
  expect_warning(
    shaded <- ray_shade(
      terrain,
      sunangle = 120,
      sunaltitude = 30,
      maxsearch = 5,
      multicore = FALSE
    ),
    'sunangle.*deprecated.*sun_angle'
  )
  expect_equal(
    as.numeric(shaded),
    as.numeric(ray_shade(
      terrain,
      sun_angle = 120,
      sun_altitude = 30,
      max_search = 5,
      multicore = FALSE
    ))
  )
})

test_that("duplicate aliases and unknown arguments fail without partial matching", {
  expect_error(
    lamb_shade(matrix(0, 3, 3), sun_angle = 45, sunangle = 90),
    'more than once.*sun_angle'
  )
  expect_error(
    do.call(lamb_shade, list(matrix(0, 3, 3), sunangle = 45, sunangle = 90)),
    'more than once.*sun_angle'
  )
  expect_error(
    lamb_shade(matrix(0, 3, 3), sunangl = 45),
    'unused argument.*sunangl'
  )
  expect_error(
    plot_3d(window_siz_typo = 128),
    'unused argument.*window_siz_typo'
  )
})

test_that("legacy plot names set the viewport and infer water from waterdepth", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  terrain = matrix(0, 5, 5)
  expect_warning(
    plot_3d_test(
      height_shade(terrain),
      terrain,
      windowsize = c(160, 120),
      waterdepth = 1,
      watercolor = 'red',
      solid = FALSE,
      shadow = FALSE
    ),
    'windowsize.*deprecated.*window_size'
  )
  expect_equal(
    unname(rgl::par3d('windowRect')[3:4] - rgl::par3d('windowRect')[1:2]),
    c(160, 120)
  )
  expect_gt(nrow(get_ids_with_labels(typeval = 'water')), 0)
  expect_false(any(
    c('windowsize', 'waterdepth', 'watercolor') %in% names(formals(plot_3d))
  ))
})

test_that("legacy high-quality lighting retains unrelated renderer arguments", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  terrain = matrix(0, 3, 3)
  plot_3d_test(height_shade(terrain), terrain, solid = FALSE, shadow = FALSE)
  captured = NULL
  local_mocked_bindings(
    render_scene = function(...) {
      captured <<- list(...)
      'rendered'
    },
    .package = 'rayrender'
  )
  expect_warning(
    value <- render_highquality(
      lightdirection = 90,
      lightaltitude = 50,
      lightsize = 4,
      lightintensity = 10,
      lightcolor = 'red',
      samples = 2,
      max_depth = 3
    ),
    'lightdirection.*deprecated.*light_direction'
  )
  expect_identical(value, 'rendered')
  expect_equal(captured$max_depth, 3)
  expect_equal(captured$samples, 2)
  lights = rayrender:::ray_scene_infinite_lights(captured$scene)
  expect_gt(length(lights), 0)
})
