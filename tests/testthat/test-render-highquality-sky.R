local_highquality_sky_scene = function(env = parent.frame()) {
  skip_if_not_installed("rayrender")
  skip_if_not_installed("skymodelr")
  local_rgl_use_null(env)
  heightmap = matrix(seq_len(16), nrow = 4)
  plot_3d_test(
    constant_shade(heightmap),
    heightmap,
    zscale = 2,
    solid = FALSE,
    shadow = FALSE,
    window_size = c(100, 100)
  )
  withr::defer(rgl::close3d(), envir = env)
}

test_that("native skies include celestial disks with haze and altitude off", {
  local_highquality_sky_scene()
  skip_if_not(render_highquality_supports_sky_light())
  sky_time = as.POSIXct("2026-06-21 12:00:00", tz = "UTC")
  scene = render_highquality(
    lat = 40.7,
    long = -74,
    datetime = sky_time,
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_true(sky$atmosphere)
  expect_true(sky$sun)
  expect_true(sky$moon)
  expect_false(sky$haze)
  expect_false(sky$query_altitude)
  expect_identical(attr(scene, "integrator_type", exact = TRUE), "nee")
  expect_equal(sky$meters_per_unit, 1)
  expect_identical(sky$datetime, sky_time)
  expect_null(attr(scene, "environment_light", exact = TRUE))
  expect_null(attr(scene, "environment_light_bake_white", exact = TRUE))
  expect_length(attr(scene, "ray_infinite_lights"), 1)
  expect_false(any(scene$shape == "sphere"))
})

test_that("native skies use terrain scale, original elevation, and true north", {
  local_highquality_sky_scene()
  skip_if_not(render_highquality_supports_sky_light())
  aspect = identity_geographic_aspect()
  aspect$active = TRUE
  aspect$mean_cell_meters = 100
  aspect$north_rotation = 12
  cache_scene_geographic_aspect(aspect)
  rgl::par3d(scale = c(1, 2, 1))
  bbox = rgl::par3d("bbox")
  centered_origin = vapply(split(bbox, rep(1:3, each = 2)), mean, 0) *
    c(1, -2, 1)
  sky_time = as.POSIXct("2026-06-21 12:00:00", tz = "UTC")
  scene = render_highquality(
    lat = 40.7,
    long = -74,
    datetime = sky_time,
    sky_altitude = 250,
    sky_query_altitude = TRUE,
    sky_args = list(rotation = 5),
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_equal(sky$meters_per_unit, 100)
  expect_equal(sky$atmosphere_origin, unname(centered_origin))
  expect_equal(sky$rotation, 17)
  expect_equal(sky$sky_args$altitude, 250)
  expect_true(sky$query_altitude)
  expect_false(sky$haze)
  expect_null(attr(scene, "rotate_env", exact = TRUE))

  rendered = NULL
  testthat::local_mocked_bindings(
    render_scene = function(...) {
      rendered <<- list(...)
      "render-result"
    },
    .package = "rayrender"
  )
  expect_identical(
    render_highquality(
      lat = 40.7,
      long = -74,
      datetime = sky_time,
      sky_haze = TRUE,
      sky_args = list(rotation = 5),
      plot = FALSE
    ),
    "render-result"
  )
  sky = rayrender::get_infinite_light(rendered$scene, "sky")
  expect_equal(sky$rotation, 17)
  expect_true(sky$haze)
  expect_true(sky$query_altitude)
  expect_null(rendered$rotate_env)
  expect_identical(rendered$integrator_type, "nee")
  expect_null(rendered$environment_light)
  expect_null(rendered$environment_light_bake_white)

  render_highquality(
    lat = 40.7,
    long = -74,
    datetime = sky_time,
    sky_args = list(rotation = 5),
    rotate_env = 30,
    plot = FALSE
  )
  sky = rayrender::get_infinite_light(rendered$scene, "sky")
  expect_equal(sky$rotation, 5)
  expect_equal(rendered$rotate_env, 30)
})

test_that("native controls and explicit sky overrides reach the light", {
  local_highquality_sky_scene()
  skip_if_not(render_highquality_supports_sky_light())
  arguments = list(
    lat = 40.7,
    long = -74,
    datetime = as.POSIXct("2026-06-21 12:00:00", tz = "UTC"),
    return_scene = TRUE
  )
  scene = do.call(
    render_highquality,
    c(
      arguments,
      list(
        sky_args = list(
          haze = TRUE,
          meters_per_unit = 25,
          atmosphere_origin = c(2, -4, 3),
          sun = FALSE,
          moon = TRUE,
          moon_resolution = 128,
          sampling_resolution = 32,
          visibility = 120
        )
      )
    )
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_true(sky$haze)
  expect_true(sky$query_altitude)
  expect_false(sky$sun)
  expect_true(sky$moon)
  expect_equal(sky$moon_resolution, 128)
  expect_equal(sky$meters_per_unit, 25)
  expect_equal(sky$atmosphere_origin, c(2, -4, 3))
  expect_equal(sky$sky_args$visibility, 120)
  expect_equal(sky$sky_args$resolution, 32)

  scene = do.call(
    render_highquality,
    c(
      arguments,
      list(
        sky_haze = FALSE,
        sky_query_altitude = FALSE,
        sky_args = list(haze = TRUE, query_altitude = TRUE)
      )
    )
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_false(sky$haze)
  expect_false(sky$query_altitude)
  expect_error(
    do.call(
      render_highquality,
      c(
        arguments,
        list(
          sky_haze = TRUE,
          sky_query_altitude = FALSE
        )
      )
    ),
    "requires `sky_query_altitude = TRUE`",
    fixed = TRUE
  )
  expect_error(
    do.call(
      render_highquality,
      c(
        arguments,
        list(
          sky_args = list(haze = TRUE, query_altitude = FALSE)
        )
      )
    ),
    "requires `sky_query_altitude = TRUE`",
    fixed = TRUE
  )
})

test_that("native skies can derive location from cached scene metadata", {
  local_highquality_sky_scene()
  skip_if_not(render_highquality_supports_sky_light())
  testthat::local_mocked_bindings(
    resolve_cached_extent_center_latlong = function(...) {
      list(lat = 39, long = -75)
    },
    .package = "rayshader"
  )
  scene = render_highquality(
    datetime = as.POSIXct("2026-06-21 12:00:00", tz = "UTC"),
    return_scene = TRUE
  )
  sky = rayrender::get_infinite_light(scene, "sky")
  expect_equal(sky$lat, 39)
  expect_equal(sky$long, -75)
})

test_that("native skies and their integrator survive both animation paths", {
  local_highquality_sky_scene()
  skip_if_not(render_highquality_supports_sky_light())
  motion = as.data.frame(matrix(0, nrow = 2, ncol = 14))
  animated = NULL
  testthat::local_mocked_bindings(
    generate_camera_motion = function(...) motion,
    render_animation = function(...) {
      animated <<- list(...)
      "animation-result"
    },
    .package = "rayrender"
  )
  arguments = list(
    lat = 40.7,
    long = -74,
    datetime = as.POSIXct("2026-06-21 12:00:00", tz = "UTC"),
    sky_haze = TRUE,
    rotate_env = 30
  )
  do.call(
    render_highquality,
    c(arguments, list(animation_camera_coords = motion))
  )
  sky = rayrender::get_infinite_light(animated$scene, "sky")
  expect_true(sky$haze)
  expect_true(sky$query_altitude)
  expect_identical(animated$integrator_type, "nee")
  expect_equal(animated$rotate_env, 30)

  expect_identical(
    render_movie_hq(
      frames = 2,
      keyframes = data.frame(x = c(0, 1), y = 2, z = 4),
      render_highquality_args = arguments
    ),
    "animation-result"
  )
  sky = rayrender::get_infinite_light(animated$scene, "sky")
  expect_true(sky$haze)
  expect_true(sky$query_altitude)
  expect_identical(animated$integrator_type, "nee")
  expect_equal(animated$rotate_env, 30)
  expect_null(animated$environment_light)
  expect_identical(animated$camera_motion, motion)

  render_movie_hq(
    frames = 2,
    keyframes = data.frame(x = c(0, 1), y = 2, z = 4),
    render_highquality_args = arguments,
    rotate_env = 60
  )
  expect_equal(animated$rotate_env, 60)
})

test_that("image skies remain available and reject native-only controls", {
  local_highquality_sky_scene()
  generated = list()
  testthat::local_mocked_bindings(
    generate_sky_latlong = function(...) {
      args = list(...)
      generated[[length(generated) + 1L]] <<- args
      file.create(args$filename)
      invisible(args$filename)
    },
    .package = "skymodelr"
  )
  arguments = list(
    lat = 39.54321,
    long = -74,
    datetime = as.POSIXct("2026-06-21 12:00:00", tz = "UTC"),
    return_scene = TRUE
  )
  for (options in list(list(hosek = TRUE), list(resolution = 16))) {
    scene = do.call(render_highquality, c(arguments, list(sky_args = options)))
    expect_null(attr(scene, "ray_infinite_lights", exact = TRUE))
    expect_true(file.exists(attr(scene, "environment_light")))
    expect_true(attr(scene, "environment_light_bake_white"))
    expect_error(
      do.call(
        render_highquality,
        c(
          arguments,
          list(
            sky_args = options,
            sky_query_altitude = TRUE
          )
        )
      ),
      "Image|image-only"
    )
  }
  expect_equal(generated[[1]]$lon, -74)
  expect_equal(generated[[2]]$resolution, 16)
  expect_error(
    render_highquality(
      sky_sun_elevation = 15,
      sky_haze = TRUE,
      return_scene = TRUE
    ),
    "Direct Sun angles"
  )

  testthat::local_mocked_bindings(
    render_highquality_supports_sky_light = function() FALSE,
    .package = "rayshader"
  )
  for (altitude in c(300, 600)) {
    scene = do.call(
      render_highquality,
      c(arguments, list(sky_altitude = altitude))
    )
    expect_null(attr(scene, "ray_infinite_lights", exact = TRUE))
    expect_true(attr(scene, "environment_light_bake_white"))
    expect_equal(tail(generated, 1)[[1]]$altitude, altitude)
  }
  expect_error(
    do.call(render_highquality, c(arguments, list(sky_haze = TRUE))),
    "compatible rayrender/skymodelr versions",
    fixed = TRUE
  )
})
