render_camera_lookat_from_rgl_test = function() {
  user_matrix = rgl::par3d("userMatrix")
  scene_bbox = rgl::par3d("bbox")
  scene_center = c(
    mean(scene_bbox[1:2]),
    mean(scene_bbox[3:4]),
    mean(scene_bbox[5:6])
  )
  scaled_offset = solve(
    user_matrix[1:3, 1:3],
    user_matrix[1:3, 4]
  )
  scene_center - as.numeric(scaled_offset) / rgl::par3d("scale")
}

setup_render_camera_spatial_scene_test = function() {
  elevation = suppressWarnings(raster::raster(
    nrows = 20,
    ncols = 20,
    xmn = 0,
    xmx = 1000,
    ymn = 0,
    ymx = 1000,
    crs = "EPSG:3857"
  ))
  raster::values(elevation) = seq_len(raster::ncell(elevation))
  heightmap = raster_to_matrix(elevation)
  expect_no_condition(plot_3d_test(
    sphere_shade(heightmap),
    elevation,
    zscale = 10,
    vertical_exaggeration = 2,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))
  invisible(heightmap)
}

test_that("render_camera() looks at WGS84 coordinates and spatial points", {
  skip_if_not_installed("sf")
  skip_if_not_installed("raster")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  setup_render_camera_spatial_scene_test()
  target_long = 0.0045
  target_lat = 0.006
  target = sf::st_sf(
    name = "target",
    geometry = sf::st_sfc(
      sf::st_point(c(target_long, target_lat)),
      crs = 4326
    )
  )
  scene_xy = sf::st_coordinates(sf::st_transform(target, get_scene_crs()))
  expected_lookat = transform_into_heightmap_coords(
    extent = get_scene_extent(),
    heightmap = get_scene_heightmap(),
    lat = scene_xy[, 2],
    long = scene_xy[, 1],
    zscale = get_scene_effective_zscale(),
    transform_scene = FALSE,
    caller = "test"
  )[1, ]

  expect_no_condition(render_camera(
    theta = 35,
    phi = 30,
    zoom = 0.7,
    fov = 50,
    lat = target_lat,
    long = target_long
  ))
  expect_equal(
    render_camera_lookat_from_rgl_test(),
    unname(expected_lookat),
    tolerance = 1e-6
  )
  latlong_rotation = rgl::par3d("userMatrix")[1:3, 1:3]

  expect_no_condition(render_camera(location = target))
  expect_equal(
    render_camera_lookat_from_rgl_test(),
    unname(expected_lookat),
    tolerance = 1e-6
  )
  expect_equal(
    rgl::par3d("userMatrix")[1:3, 1:3],
    latlong_rotation,
    tolerance = 1e-6
  )
  expect_equal(rgl::par3d("zoom"), 0.7, tolerance = 1e-6)
  expect_equal(rgl::par3d("FOV"), 50, tolerance = 1e-6)

  target_altitude = 250
  expected_altitude_lookat = transform_into_heightmap_coords(
    extent = get_scene_extent(),
    heightmap = get_scene_heightmap(),
    lat = scene_xy[, 2],
    long = scene_xy[, 1],
    altitude = target_altitude,
    zscale = get_scene_effective_zscale(),
    transform_scene = FALSE,
    caller = "test"
  )[1, ]
  expect_equal(expected_altitude_lookat[2], 50)

  expect_no_condition(render_camera(
    lat = target_lat,
    long = target_long,
    altitude = target_altitude
  ))
  expect_equal(
    render_camera_lookat_from_rgl_test(),
    unname(expected_altitude_lookat),
    tolerance = 1e-6
  )

  expect_no_condition(render_camera(
    location = target,
    altitude = target_altitude
  ))
  expect_equal(
    render_camera_lookat_from_rgl_test(),
    unname(expected_altitude_lookat),
    tolerance = 1e-6
  )
})

test_that("render_camera() validates geographic look-at targets", {
  skip_if_not_installed("sf")
  skip_if_not_installed("raster")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  setup_render_camera_spatial_scene_test()
  target = sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(0.0045, 0.006)), crs = 4326)
  )
  expect_error(
    render_camera(lat = 0.006),
    "must be supplied together"
  )
  expect_error(
    render_camera(location = target, lat = 0.006, long = 0.0045),
    "either `location` or `lat` and `long`"
  )
  expect_error(
    render_camera(altitude = 100),
    "requires `location` or both `lat` and `long`"
  )

  multiple_targets = rbind(target, target)
  expect_error(
    render_camera(location = multiple_targets),
    "exactly one POINT"
  )
})

test_that("render_camera() requires a cached CRS for latitude and longitude", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  expect_no_condition(plot_3d_test(
    sphere_shade(heightmap),
    heightmap,
    extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))
  expect_error(
    render_camera(lat = 10, long = 20),
    "no cached CRS"
  )
})
