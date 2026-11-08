test_that("high-quality lighting supports both disk and older spherical lights", {
  skip_if_not_installed("rayrender")
  skip_if_not(all(
    c("disk_light", "add_infinite_light") %in% getNamespaceExports("rayrender")
  ))
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(0, 4, 4)
  plot_3d_test(
    constant_shade(heightmap),
    heightmap,
    solid = FALSE,
    shadow = FALSE
  )
  observer_radius = rgl::par3d("observer")[3]
  spheres = disks = list()
  original_sphere = rayrender::sphere
  original_disk = rayrender::disk_light
  local_mocked_bindings(
    sphere = function(...) {
      spheres[[length(spheres) + 1L]] <<- list(...)
      original_sphere(...)
    },
    disk_light = function(...) {
      disks[[length(disks) + 1L]] <<- list(...)
      original_disk(...)
    },
    render_scene = function(...) list(...),
    .package = "rayrender"
  )
  args = list(
    light_direction = c(0, 90),
    light_altitude = 30,
    light_size = c(2, 3),
    light_color = c("red", "blue"),
    light_intensity = c(100, 200),
    plot = FALSE
  )
  do.call(render_highquality, args)
  expect_length(disks, 2)
  expect_length(spheres, 0)
  expected_vectors = list(c(0, .5, cospi(30 / 180)), c(-cospi(30 / 180), .5, 0))
  for (i in 1:2) {
    expect_equal(disks[[i]]$direction, expected_vectors[[i]])
    expect_equal(disks[[i]]$angular_diameter, args$light_size[i])
    expect_equal(disks[[i]]$intensity, args$light_intensity[i])
    expect_equal(disks[[i]]$color, args$light_color[i])
  }
  # Change only the capability lookup for this function invocation, without
  # mutating rayrender's exports or affecting unrelated scene-camera checks.
  older_render = render_highquality
  environment(older_render) = new.env(parent = environment(render_highquality))
  for (missing_export in c("disk_light", "add_infinite_light")) {
    environment(older_render)$getNamespaceExports = function(ns) {
      setdiff(base::getNamespaceExports(ns), missing_export)
    }
    spheres = disks = list()
    do.call(older_render, args)
    expect_length(disks, 0)
    expect_length(spheres, 2)
    for (i in 1:2) {
      location = unlist(spheres[[i]][c("x", "y", "z")], use.names = FALSE)
      expect_equal(location, observer_radius * 5 * expected_vectors[[i]])
      expect_equal(spheres[[i]]$radius, args$light_size[i])
      expect_equal(
        spheres[[i]]$material,
        rayrender::light(
          color = args$light_color[i],
          intensity = args$light_intensity[i]
        )
      )
    }
    spheres = list()
    older_render(light_size = NULL, plot = FALSE)
    expect_equal(spheres[[1]]$radius, observer_radius / 5)
    spheres = list()
    older_render(light = FALSE, plot = FALSE)
    expect_length(spheres, 0)
  }
})
