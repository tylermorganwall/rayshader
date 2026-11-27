test_that("default high-quality lighting uses infinite disks with the existing controls", {
  skip_if_not_installed("rayrender", "0.42.3")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(0, 4, 4)
  plot_3d_test(
    constant_shade(heightmap),
    heightmap,
    solid = FALSE,
    shadow = FALSE
  )

  scene = render_highquality(return_scene = TRUE)
  lights = attr(scene, "ray_infinite_lights")
  expect_length(lights, 1)
  disk = lights[[1]]
  expect_identical(disk$type, "uniform_disk")
  expect_equal(disk$direction, c(0.5, sqrt(0.5), 0.5))
  expect_equal(disk$angular_diameter, 5)
  expect_equal(disk$intensity, 500)
  expect_equal(disk$color, c(1, 1, 1))
  expect_false(any(scene$shape == "sphere"))

  scene = render_highquality(
    light_direction = c(0, 90),
    light_altitude = 0,
    light_color = c("red", "blue"),
    light_intensity = c(10, 20),
    light_size = c(2, 8),
    scene_elements = rayrender::sphere(radius = 0.1),
    return_scene = TRUE
  )
  lights = attr(scene, "ray_infinite_lights")
  expect_length(lights, 2)
  expect_equal(lights[[1]]$direction, c(0, 0, 1))
  expect_equal(lights[[2]]$direction, c(-1, 0, 0))
  expect_equal(lights[[1]]$color, c(1, 0, 0))
  expect_equal(lights[[2]]$color, c(0, 0, 1))
  expect_equal(
    vapply(lights, function(x) x$intensity, 0, USE.NAMES = FALSE),
    c(10, 20)
  )
  expect_equal(
    vapply(lights, function(x) x$angular_diameter, 0, USE.NAMES = FALSE),
    c(2, 8)
  )
  expect_length(unique(vapply(lights, function(x) x$name, "")), 2)
  expect_equal(sum(scene$shape == "sphere"), 1)

  scene = render_highquality(
    light_size = 12,
    camera_location = c(0, 1000, 1000),
    return_scene = TRUE
  )
  expect_equal(attr(scene, "ray_infinite_lights")[[1]]$angular_diameter, 12)

  scene = render_highquality(light = FALSE, return_scene = TRUE)
  expect_null(attr(scene, "ray_infinite_lights"))
})
