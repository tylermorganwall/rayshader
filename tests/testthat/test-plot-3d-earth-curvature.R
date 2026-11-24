test_that("a spatial DEM bends below its center tangent plane in both terrain meshes", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  dem = terra::rast(
    nrows = 5,
    ncols = 7,
    xmin = -1.75,
    xmax = 1.75,
    ymin = -1.25,
    ymax = 1.25,
    crs = "EPSG:4326"
  )
  terra::values(dem) = 100
  texture = sphere_shade(dem)
  for (triangulate in c(FALSE, TRUE)) {
    plot_3d_test(
      texture,
      earth_curvature = TRUE,
      triangulate = triangulate,
      solid = FALSE,
      shadow = FALSE,
      vertical_exaggeration = 3,
      precomputed_normals = stop("flat terrain normals were used")
    )
    surface = get_ids_with_labels(typeval = "surface_tris")$id[1]
    vertices = rgl::rgl.attrib(surface, "vertices")
    meters_per_unit = get_scene_geographic_aspect()$mean_cell_meters
    rendered_m = vertices[, 2] * meters_per_unit
    center = vertices[, 1] == 0 & vertices[, 3] == 0
    expect_true(any(center))
    expect_equal(unique(rendered_m[center]), 300, tolerance = 1e-6)
    # The center-to-east-edge distance is 1.5 degrees along the equator.
    east_edge = vertices[, 1] == max(vertices[, 1]) & vertices[, 3] == 0
    radius_m = 6371008.8
    expected_drop = radius_m * (1 - cos(1.5 * pi / 180))
    expect_true(any(east_edge))
    expect_lt(abs(unique(rendered_m[east_edge])[1] - (300 - expected_drop)), 10)
    expect_equal(as.vector(get_hillshade_heightmap()), rep(100, 35))
    expect_lt(min(get_scene_heightmap()), 100)
  }
  # With only the scene cache available, a new flat plot recovers the source DEM.
  clear_hillshade_cache()
  plot_3d_test(texture, solid = FALSE, shadow = FALSE)
  expect_equal(as.vector(get_scene_heightmap()), rep(100, 35))
})

test_that("curvature uses metric axis spacing and centers even grids between samples", {
  aspect = list(
    mean_cell_meters = 75000,
    cell_meters = c(x = 100000, z = 50000)
  )
  heightmap = matrix(0, 4, 6)
  heightmap[1, 1] = NA_real_
  radius_m = 6371008.8
  # The four central samples are 50 km east/west and 25 km north/south.
  center_drop = radius_m * (1 - cos(sqrt(50000^2 + 25000^2) / radius_m))
  curved = apply_earth_curvature(heightmap, aspect, zscale = 150000)
  expect_equal(as.vector(curved[2:3, 3:4]), rep(-2 * center_drop, 4))
  expect_equal(curved[1, 3], curved[4, 4])
  expect_lt(curved[1, 3], curved[2, 2])
  expect_identical(is.na(curved), is.na(heightmap))
})

test_that("curvature requires a logical flag and metric spatial information", {
  expect_error(plot_3d(NULL, earth_curvature = NA), "single TRUE/FALSE")
  dem = matrix(0, 5, 5)
  expect_error(
    plot_3d(array(1, c(5, 5, 3)), dem, earth_curvature = TRUE),
    "valid extent and CRS"
  )
})

test_that("spatial layers share the scene datum without bending explicit DEMs twice", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  dem = matrix(50, 9, 7)
  extent = c(xmin = -8, xmax = 8, ymin = -3, ymax = 3)
  texture = array(1, c(7, 9, 3))
  plot_3d_test(
    texture,
    dem,
    extent = extent,
    crs = 4326,
    earth_curvature = TRUE,
    vertical_exaggeration = 4,
    solid = FALSE,
    shadow = FALSE
  )
  metadata = get_scene_metadata(include_heightmap = TRUE)
  expect_true(metadata$earth_curvature)
  expect_equal(metadata$heightmap, dem)
  get_vertices = function(tag) {
    objects = rgl::ids3d(tags = TRUE)
    rgl::rgl.attrib(tail(objects$id[objects$tag == tag], 1), "vertices")
  }
  terrain_id = get_ids_with_labels("surface_tris")$id[1]
  terrain = rgl::rgl.attrib(terrain_id, "vertices")
  east = terrain[terrain[, 1] == max(terrain[, 1]) & terrain[, 3] == 0, 2]
  for (altitude in list(NULL, 50)) {
    render_points(x = 8, y = 0, altitude = altitude, offset = 0)
    expect_equal(as.numeric(get_vertices("points3d")[1, 2]), unname(east))
    render_points(
      x = 8,
      y = 0,
      altitude = altitude,
      heightmap = dem,
      offset = 0
    )
    expect_equal(as.numeric(get_vertices("points3d")[1, 2]), unname(east))
  }
  render_label(
    x = 8,
    y = 0,
    z = 10,
    relative_z = TRUE,
    offset = 0,
    text = "coast",
    freetype = FALSE
  )
  expect_equal(
    as.numeric(get_vertices("raytext")[1, 2]),
    unname(east) + 10 / metadata$effective_zscale,
    tolerance = 1e-6
  )
  render_label(
    x = 8,
    y = 0,
    z = 60,
    relative_z = FALSE,
    offset = 0,
    text = "coast",
    freetype = FALSE
  )
  expect_equal(
    as.numeric(get_vertices("raytext")[1, 2]),
    unname(east) + 10 / metadata$effective_zscale,
    tolerance = 1e-6
  )
  target = resolve_render_camera_lookat(location = c(8, 0), altitude = 50)
  expect_equal(target[2], unname(east), tolerance = 1e-6)
  draped = scene_draping_heightmap(dem, metadata$effective_zscale)
  expect_identical(
    scene_draping_heightmap(draped, metadata$effective_zscale),
    draped
  )
  expect_equal(
    as.numeric(draped[9, 4]) / metadata$effective_zscale,
    unname(east),
    tolerance = 1e-6
  )
  render_path(lat = c(0, 0), long = c(-8, 8), altitude = 50)
  path = get_vertices("path3d")
  expect_gt(max(path[, 2], na.rm = TRUE), east + 0.1)
  plot_3d_test(
    texture,
    dem,
    extent = extent,
    crs = 4326,
    solid = FALSE,
    shadow = FALSE
  )
  expect_false(get_scene_metadata()$earth_curvature)
  render_points(x = 8, y = 0, altitude = 0, offset = 0)
  expect_equal(as.numeric(get_vertices("points3d")[1, 2]), 0)
})

test_that("water and floating textures follow curvature across their interiors", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  dem = matrix(-100, 9, 9)
  texture = array(1, c(9, 9, 4))
  plot_3d_test(
    texture,
    dem,
    extent = c(xmin = -8, xmax = 8, ymin = -8, ymax = 8),
    crs = 4326,
    solid = FALSE,
    shadow = FALSE,
    water_input = 0,
    water_render_method = "raster",
    earth_curvature = TRUE
  )
  water_ids = function() {
    rgl::ids3d(tags = TRUE)$id[rgl::ids3d(tags = TRUE)$tag == "water"]
  }
  first = rgl::rgl.attrib(water_ids()[1], "vertices")
  expect_gt(diff(range(first[, 2])), 0.1)
  render_water(
    water_input = 0,
    water_render_method = "raster",
    clear_previous = TRUE
  )
  expect_equal(rgl::rgl.attrib(water_ids()[1], "vertices"), first)
  # A physical water level below the DEM must remain dry even at the corners.
  expect_warning(
    render_water(
      water_input = -120,
      water_render_method = "raster",
      clear_previous = TRUE
    ),
    "No water"
  )
  expect_length(water_ids(), 0)
  render_floating_overlay(texture, altitude = 0)
  ids = rgl::ids3d(tags = TRUE)
  overlay = rgl::rgl.attrib(
    ids$id[ids$tag == "floating_overlay_tris"],
    "vertices"
  )
  expect_gt(nrow(overlay), 6)
  expect_equal(
    unname(overlay[, 2]),
    unname(-scene_curvature_drop(overlay[, 1], overlay[, 3])),
    tolerance = 1e-6
  )
})

test_that("broad polygon caps follow the local altitude between source vertices", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  extent = c(xmin = -8, xmax = 8, ymin = -8, ymax = 8)
  plot_3d_test(
    array(1, c(9, 9, 3)),
    matrix(0, 9, 9),
    extent = extent,
    crs = 4326,
    earth_curvature = TRUE,
    solid = FALSE,
    shadow = FALSE
  )
  region = sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
    extent,
    crs = sf::st_crs(4326)
  )))
  render_polygons(region, top = 0, bottom = 0, parallel = FALSE, lit = FALSE)
  v = rgl::rgl.attrib(get_ids_with_labels("polygon3d")$id[1], "vertices")
  expect_gt(diff(range(v[, 2])), 0.1)
  expect_equal(
    unname(v[, 2]),
    -unname(scene_curvature_drop(v[, 1], v[, 3])),
    tolerance = 1e-6
  )
})

test_that("projected roads, trails, and streams drape over the same curved DEM", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  plot_3d_test(
    array(1, c(9, 13, 3)),
    matrix(0, 13, 9),
    extent = c(xmin = -600000, xmax = 600000, ymin = -400000, ymax = 400000),
    crs = 3857,
    earth_curvature = TRUE,
    solid = FALSE,
    shadow = FALSE
  )
  line = sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(
        cbind(c(-400000, 0, 400000), c(0, 0, 0))
      ),
      crs = 3857
    )
  )
  for (render in list(render_roads, render_trails, render_streams)) {
    coords = render(line, width = 0.1, offset = 0, parallel = FALSE)[[1]]
    outer = which.max(abs(coords[, 1]))
    center = which.min(abs(coords[, 1]))
    # Road placement can add sub-meter clearance at the pavement edge.
    ground = -scene_curvature_drop(coords[outer, 1], coords[outer, 3])
    expect_lt(
      abs(coords[outer, 2] - ground) *
        get_scene_geographic_aspect()$mean_cell_meters,
      1
    )
    expect_gt(coords[center, 2], coords[outer, 2])
  }
})
