water_mesh_vertices = function(mesh) {
  if (!length(mesh$vertices)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, mesh$vertices)
}

water_mesh_triangle_normals = function(vertices) {
  triangle_count = nrow(vertices) / 3
  normals = matrix(NA_real_, nrow = triangle_count, ncol = 3)
  centers = matrix(NA_real_, nrow = triangle_count, ncol = 3)
  for (i in seq_len(triangle_count)) {
    triangle = vertices[seq.int(3 * i - 2, 3 * i), , drop = FALSE]
    first_edge = triangle[2, ] - triangle[1, ]
    second_edge = triangle[3, ] - triangle[1, ]
    normals[i, ] = c(
      first_edge[2] * second_edge[3] - first_edge[3] * second_edge[2],
      first_edge[3] * second_edge[1] - first_edge[1] * second_edge[3],
      first_edge[1] * second_edge[2] - first_edge[2] * second_edge[1]
    )
    centers[i, ] = colMeans(triangle)
  }
  list(normals = normals, centers = centers)
}

test_that("make_water_mesh_cpp handles empty and clipped water", {
  heightmap = matrix(2, nrow = 3, ncol = 3)
  waterheight = matrix(1, nrow = 3, ncol = 3)

  mesh = make_water_mesh_cpp(heightmap, waterheight)
  expect_equal(length(mesh$vertices), 0)
  expect_equal(nrow(mesh$lines), 0)

  heightmap = matrix(c(0, 0, 2, 2), nrow = 2, ncol = 2)
  waterheight = matrix(1, nrow = 2, ncol = 2)
  mesh = make_water_mesh_cpp(heightmap, waterheight)
  vertices = water_mesh_vertices(mesh)

  expect_equal(length(mesh$vertices), 1)
  expect_gt(nrow(vertices), 0)
  expect_false(any(
    abs(vertices[, 2] - 1) < 1e-8 & abs(vertices[, 3] - 0.5) < 1e-8
  ))
  expect_true(any(abs(mesh$lines[, 3]) < 1e-8))
})

test_that("make_water_mesh_cpp separates disconnected and diagonal water", {
  heightmap = matrix(2, nrow = 5, ncol = 5)
  heightmap[1:2, 1:2] = 0
  heightmap[4:5, 4:5] = 0
  waterheight = matrix(1, nrow = 5, ncol = 5)

  mesh = make_water_mesh_cpp(heightmap, waterheight)
  expect_equal(length(mesh$vertices), 2)

  heightmap = matrix(c(0, 2, 2, 0), nrow = 2, ncol = 2)
  waterheight = matrix(1, nrow = 2, ncol = 2)
  mesh = make_water_mesh_cpp(heightmap, waterheight)
  expect_equal(length(mesh$vertices), 2)
})

test_that("make_water_mesh_cpp handles NA holes and variable water levels", {
  heightmap = matrix(0, nrow = 5, ncol = 5)
  waterheight = matrix(1, nrow = 5, ncol = 5)
  mesh_no_hole = make_water_mesh_cpp(heightmap, waterheight)

  heightmap[3, 3] = NA
  mesh_with_hole = make_water_mesh_cpp(heightmap, waterheight)
  expect_gt(nrow(mesh_with_hole$lines), nrow(mesh_no_hole$lines))

  heightmap = matrix(0, nrow = 3, ncol = 3)
  waterheight = outer(seq(0.5, 1.5, length.out = 3), rep(1, 3))
  mesh = make_water_mesh_cpp(heightmap, waterheight)
  vertices = water_mesh_vertices(mesh)
  expect_equal(max(vertices[, 2], na.rm = TRUE), 1.5, tolerance = 1e-8)
  expect_true(any(abs(vertices[, 2] - 0.5) < 1e-8))
})

test_that("spatial waterdepth inputs align to the heightmap grid", {
  skip_if_not_installed("terra")

  water_raster = terra::rast(
    nrows = 8,
    ncols = 8,
    xmin = -2,
    xmax = 6,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:3857"
  )
  xy = terra::xyFromCell(water_raster, seq_len(terra::ncell(water_raster)))
  terra::values(water_raster) = ifelse(
    xy[, 1] >= 1 & xy[, 1] <= 3,
    2,
    -2
  )

  waterheight = normalize_waterheight_matrix(
    water_raster,
    nr = 4,
    nc = 4,
    zscale = 1,
    caller = "test",
    heightmap_extent = c(xmin = 0, xmax = 4, ymin = 0, ymax = 4),
    heightmap_crs = "EPSG:3857"
  )

  expect_equal(dim(waterheight), c(4, 4))
  expect_true(any(waterheight > 1, na.rm = TRUE))
  expect_true(any(waterheight < -1, na.rm = TRUE))
})

test_that("spatial waterdepth inputs project to the heightmap CRS", {
  skip_if_not_installed("terra")

  water_raster = terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 0.01,
    ymin = 0,
    ymax = 0.01,
    crs = "EPSG:4326"
  )
  terra::values(water_raster) = 5

  waterheight = normalize_waterheight_matrix(
    water_raster,
    nr = 5,
    nc = 5,
    zscale = 1,
    caller = "test",
    heightmap_extent = c(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000),
    heightmap_crs = "EPSG:3857"
  )

  expect_equal(dim(waterheight), c(5, 5))
  expect_true(any(abs(waterheight - 5) < 1e-8, na.rm = TRUE))
})

test_that("make_water_mesh_cpp emits outward map-edge sidewall normals", {
  heightmap = matrix(0, nrow = 3, ncol = 3)
  waterheight = matrix(1, nrow = 3, ncol = 3)
  mesh = make_water_mesh_cpp(heightmap, waterheight)
  triangle_info = water_mesh_triangle_normals(water_mesh_vertices(mesh))

  sidewalls = abs(triangle_info$normals[, 2]) < 1e-8
  normals = triangle_info$normals[sidewalls, , drop = FALSE]
  centers = triangle_info$centers[sidewalls, , drop = FALSE]

  expect_equal(nrow(normals), 16)
  expect_true(all(normals[abs(centers[, 1] + 1) < 1e-8, 1] < 0))
  expect_true(all(normals[abs(centers[, 1] - 1) < 1e-8, 1] > 0))
  expect_true(all(normals[abs(centers[, 3] + 1) < 1e-8, 3] < 0))
  expect_true(all(normals[abs(centers[, 3] - 1) < 1e-8, 3] > 0))
})

test_that("water rendering API validates method and matrix inputs", {
  heightmap = matrix(0, nrow = 3, ncol = 3)
  waterheight = matrix(1, nrow = 3, ncol = 3)

  expect_error(
    make_water(
      heightmap,
      waterheight = waterheight,
      water_render_method = "legacy"
    ),
    "only supports a scalar"
  )
  expect_error(
    make_water(heightmap, waterheight = matrix(1, nrow = 2, ncol = 2)),
    "must have dimensions 3 x 3"
  )
  expect_error(
    make_water(heightmap, waterheight = 1, water_render_method = "bad"),
    "'arg' should be one of"
  )
})

test_that("legacy water rendering rejects spatial waterdepth inputs", {
  skip_if_not_installed("terra")

  heightmap = matrix(0, nrow = 3, ncol = 3)
  waterheight = terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3
  )
  terra::values(waterheight) = 1

  expect_error(
    make_water(
      heightmap,
      waterheight = waterheight,
      water_render_method = "legacy"
    ),
    "only supports a scalar"
  )
})

test_that("plot_3d creates separate water ids for disconnected contour water", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(2, nrow = 8, ncol = 8)
  heightmap[2:3, 2:3] = 0
  heightmap[6:7, 6:7] = 0
  texture = constant_shade(heightmap)

  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    solid = FALSE,
    shadow = FALSE,
    water = TRUE,
    waterdepth = 1,
    windowsize = c(200, 200)
  ))

  water_ids = get_ids_with_labels(typeval = "water")
  expect_equal(nrow(water_ids), 2)
})

test_that("plot_3d and render_water accept spatial waterdepth rasters", {
  skip_if_not_installed("terra")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  water_raster = terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:3857"
  )
  terra::values(water_raster) = 1
  texture = constant_shade(height_raster)

  expect_no_condition(plot_3d_test(
    texture,
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    water = TRUE,
    waterdepth = water_raster,
    windowsize = c(200, 200)
  ))
  expect_gt(nrow(get_ids_with_labels(typeval = "water")), 0)

  expect_no_condition(render_water(waterdepth = water_raster))
  expect_gt(nrow(get_ids_with_labels(typeval = "water")), 0)
})

test_that("convert_rgl_to_raymesh handles contour water ids", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(2, nrow = 8, ncol = 8)
  heightmap[2:3, 2:3] = 0
  heightmap[6:7, 6:7] = 0
  texture = constant_shade(heightmap)

  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    solid = FALSE,
    shadow = FALSE,
    water = TRUE,
    waterdepth = 1,
    windowsize = c(200, 200)
  ))

  expect_no_condition(ray_scene <- convert_rgl_to_raymesh(save_shadow = FALSE))
  expect_false(is_empty_raymesh_scene(ray_scene))
})
