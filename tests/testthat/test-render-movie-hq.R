test_that("render_movie_hq generates motion from saved keyframes", {
  keyframes = data.frame(
    x = c(0, 1),
    y = c(2, 3),
    z = c(4, 5)
  )
  camera_motion = as.data.frame(matrix(0, nrow = 12, ncol = 14))
  scene_args = NULL
  motion_args = NULL
  animation_args = NULL

  testthat::local_mocked_bindings(
    render_highquality = function(...) {
      scene_args <<- list(...)
      scene = list(label = "rendered-scene")
      attr(scene, "environment_light") = "sky.exr"
      attr(scene, "environment_light_bake_white") = TRUE
      scene
    },
    .package = "rayshader"
  )
  testthat::local_mocked_bindings(
    get_saved_keyframes = function() keyframes,
    generate_camera_motion = function(
      positions,
      frames,
      type,
      closed,
      damp_motion
    ) {
      motion_args <<- list(
        positions = positions,
        frames = frames,
        type = type,
        closed = closed,
        damp_motion = damp_motion
      )
      camera_motion
    },
    render_animation = function(
      scene,
      camera_motion,
      environment_light_bake_white = NULL,
      ...
    ) {
      animation_args <<- c(
        list(
          scene = scene,
          camera_motion = camera_motion,
          environment_light_bake_white = environment_light_bake_white
        ),
        list(...)
      )
      "animation-result"
    },
    .package = "rayrender"
  )

  value = withVisible(render_movie_hq(
    frames = 12,
    filename = "movie.mp4",
    samples = 4,
    progress = FALSE,
    render_highquality_args = list(light = FALSE, return_scene = FALSE)
  ))

  expect_false(value$visible)
  expect_identical(value$value, "animation-result")
  expect_identical(scene_args, list(light = FALSE, return_scene = TRUE))
  expect_identical(motion_args$positions, keyframes)
  expect_identical(motion_args$frames, 12)
  expect_identical(motion_args$type, "linear")
  expect_true(motion_args$closed)
  expect_true(motion_args$damp_motion)
  expect_identical(animation_args$scene$label, "rendered-scene")
  expect_identical(animation_args$camera_motion, camera_motion)
  expect_identical(animation_args$environment_light, "sky.exr")
  expect_true(animation_args$environment_light_bake_white)
  expect_identical(animation_args$filename, "movie.mp4")
  expect_identical(animation_args$samples, 4)
  expect_false(animation_args$progress)
})

test_that("render_movie_hq validates scene extraction arguments", {
  expect_error(
    render_movie_hq(render_highquality_args = "light = FALSE"),
    "`render_highquality_args` must be a list.",
    fixed = TRUE
  )
})

test_that("render_movie_hq stops before scene extraction when no keyframes exist", {
  scene_extracted = FALSE

  testthat::local_mocked_bindings(
    render_highquality = function(...) {
      scene_extracted <<- TRUE
      "rendered-scene"
    },
    .package = "rayshader"
  )
  testthat::local_mocked_bindings(
    get_saved_keyframes = function() data.frame(),
    .package = "rayrender"
  )

  expect_error(
    render_movie_hq(),
    "No keyframes saved",
    fixed = TRUE
  )
  expect_false(scene_extracted)
})
