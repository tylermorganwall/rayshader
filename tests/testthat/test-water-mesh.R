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

  expect_no_condition(render_water(heightmap = height_raster, waterdepth = 1))
  expect_gt(nrow(get_ids_with_labels(typeval = "water")), 0)
})

test_that("render_streams draws spatial stream paths as water paths", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 5,
    ncols = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 5,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  stream = sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      sf::st_geometrycollection(list(
        sf::st_linestring(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE))
      )),
      sf::st_linestring(matrix(c(2, 2, 4, 4), ncol = 2, byrow = TRUE)),
      crs = 3857
    )
  )
  expect_s3_class(sf::st_geometry(stream), "sfc_GEOMETRY")

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_no_condition(render_streams(
    stream,
    heightmap = height_raster,
    watercolor = "dodgerblue",
    width = 0.5
  ))

  expect_equal(nrow(get_ids_with_labels(typeval = "water")), 0)
  water_path_ids = get_ids_with_labels(typeval = "water_path")
  expect_equal(nrow(water_path_ids), 1)

  water_path_vertices = rgl::rgl.attrib(water_path_ids$id[[1]], "vertices")
  expect_gt(nrow(water_path_vertices), 2)
  segmented_vertices = matrix(
    c(
      0,
      0,
      0,
      1,
      0,
      0,
      1,
      0,
      0,
      2,
      0,
      0,
      2,
      0,
      0,
      3,
      0,
      0
    ),
    ncol = 3,
    byrow = TRUE
  )
  separated_vertices = rbind(
    segmented_vertices[1:2, , drop = FALSE],
    c(NA, NA, NA),
    segmented_vertices[3:4, , drop = FALSE]
  )
  separated_vertices_split = split_render_highquality_path_vertices(
    separated_vertices
  )
  expect_length(separated_vertices_split, 2)
  expect_equal(separated_vertices_split[[1]], segmented_vertices[1:2, ])
  expect_equal(separated_vertices_split[[2]], segmented_vertices[3:4, ])
  expect_equal(
    collapse_render_highquality_path_vertices(segmented_vertices),
    matrix(
      c(
        0,
        0,
        0,
        1,
        0,
        0,
        2,
        0,
        0,
        3,
        0,
        0
      ),
      ncol = 3,
      byrow = TRUE
    )
  )
  expect_equal(
    make_render_highquality_water_path_polygon(),
    matrix(
      c(
        -0.5,
        -0.1,
        0.5,
        -0.1,
        0.5,
        0.1,
        -0.5,
        0.1
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
  sparse_stream_coords = matrix(
    c(
      -2,
      0,
      0,
      2,
      0,
      0
    ),
    ncol = 3,
    byrow = TRUE
  )
  undensified_stream_coords = offset_water_path_coords(
    list(sparse_stream_coords),
    offset = 2
  )[[1]]
  densified_stream_coords = densify_water_path_coords(
    coord_list = list(sparse_stream_coords),
    heightmap = matrix(0, nrow = 5, ncol = 5),
    zscale = 1,
    offset = 2
  )[[1]]
  expect_equal(nrow(undensified_stream_coords), 2)
  expect_equal(undensified_stream_coords[, 2], c(2, 2))
  expect_gt(nrow(densified_stream_coords), nrow(sparse_stream_coords))
  expect_equal(unique(densified_stream_coords[, 2]), 2)
  expect_false(validate_waterpath_logical(FALSE, "waterpath_densify"))
  expect_error(
    validate_waterpath_logical(NA, "waterpath_densify"),
    "`waterpath_densify` must be TRUE or FALSE.",
    fixed = TRUE
  )
  water_path_mesh = make_render_highquality_water_path_mesh(
    points = matrix(
      c(
        0,
        0,
        0,
        1,
        0,
        0
      ),
      ncol = 3,
      byrow = TRUE
    ),
    bbox_center = c(0, 0, 0),
    width = 1,
    heightmap = matrix(0, nrow = 3, ncol = 3),
    zscale = 1,
    material = rayrender::dielectric()
  )
  water_path_mesh_vertices =
    water_path_mesh$shape_info[[1]]$mesh_info[[1]]$vertices
  water_path_meshes = make_render_highquality_water_path_meshes(
    list(
      list(
        points = matrix(
          c(
            0,
            0,
            0,
            1,
            0,
            0
          ),
          ncol = 3,
          byrow = TRUE
        ),
        bbox_center = c(0, 0, 0),
        width = 1,
        heightmap = matrix(0, nrow = 3, ncol = 3),
        zscale = 1,
        material = rayrender::dielectric()
      ),
      list(
        points = matrix(c(0, 0, 0), ncol = 3),
        bbox_center = c(0, 0, 0),
        width = 1,
        heightmap = matrix(0, nrow = 3, ncol = 3),
        zscale = 1,
        material = rayrender::dielectric()
      )
    )
  )
  expect_length(water_path_meshes, 1)
  expect_equal(
    range(water_path_mesh_vertices[, 2]),
    c(-0.1, 0.1),
    tolerance = 1e-8
  )
  expect_equal(
    range(water_path_mesh_vertices[, 3]),
    c(-0.5, 0.5),
    tolerance = 1e-8
  )
  sloped_heightmap = matrix(
    rep(seq_len(5), times = 5),
    nrow = 5,
    ncol = 5
  )
  scaled_water_path_heightmap =
    scale_render_highquality_water_path_heightmap(
      heightmap = sloped_heightmap,
      zscale = 2
    )
  expect_equal(scaled_water_path_heightmap$heightmap, sloped_heightmap / 2)
  expect_equal(scaled_water_path_heightmap$zscale, 1)
  expect_equal(
    scale_render_highquality_water_path_heightmap(
      heightmap = sloped_heightmap,
      zscale = 1
    )$heightmap,
    sloped_heightmap
  )
  expected_normal = c(-1, 1, 0) / sqrt(2)
  expect_equal(
    interpolate_render_highquality_water_path_normals(
      points = matrix(c(0, 0, 0), ncol = 3),
      heightmap = sloped_heightmap,
      zscale = 1
    )[1, ],
    expected_normal,
    tolerance = 1e-8
  )
  expect_equal(
    interpolate_render_highquality_water_path_normals(
      points = matrix(c(0, 0, 0), ncol = 3),
      heightmap = sloped_heightmap,
      zscale = 2
    )[1, ],
    interpolate_render_highquality_water_path_normals(
      points = matrix(c(0, 0, 0), ncol = 3),
      heightmap = scaled_water_path_heightmap$heightmap,
      zscale = scaled_water_path_heightmap$zscale
    )[1, ],
    tolerance = 1e-8
  )
  z_sloped_heightmap = matrix(
    rep(seq_len(5), each = 5),
    nrow = 5,
    ncol = 5
  )
  sloped_points = cbind(
    c(0, 1),
    interpolate_spatial_water_height(z_sloped_heightmap, c(0, 1), c(0, 0)),
    c(0, 0)
  )
  sloped_normals = interpolate_render_highquality_water_path_normals(
    points = sloped_points,
    heightmap = z_sloped_heightmap,
    zscale = 1
  )
  sloped_tangents = calculate_render_highquality_water_path_tangents(
    points = sloped_points,
    normals = sloped_normals
  )
  sloped_side_vectors = normalize_render_highquality_rows(row_cross(
    sloped_tangents,
    sloped_normals
  ))
  sloped_edge_centers = make_render_highquality_water_path_edge_centers(
    points = sloped_points,
    side_vectors = sloped_side_vectors,
    half_width = 0.5,
    heightmap = z_sloped_heightmap,
    zscale = 1
  )
  expect_equal(
    sloped_edge_centers$left[, 2],
    interpolate_spatial_water_height(
      z_sloped_heightmap,
      sloped_edge_centers$left[, 1],
      sloped_edge_centers$left[, 3]
    ),
    tolerance = 1e-8
  )
  expect_equal(
    sloped_edge_centers$right[, 2],
    interpolate_spatial_water_height(
      z_sloped_heightmap,
      sloped_edge_centers$right[, 1],
      sloped_edge_centers$right[, 3]
    ),
    tolerance = 1e-8
  )
  highquality_densified_points = densify_render_highquality_water_path_points(
    points = matrix(c(-1.25, 0, 0, 1.25, 0, 0), ncol = 3, byrow = TRUE),
    width = 0.5,
    heightmap = matrix(0, nrow = 5, ncol = 5),
    zscale = 1
  )
  expect_true(all(
    c(-1, 0, 1) %in%
      round(
        highquality_densified_points[, 1],
        8
      )
  ))
  expect_true(all(
    abs(range(as.numeric(water_path_vertices[, 2]))) < 1e-8
  ))
  expect_equal(
    resolve_waterpath_offset(NULL),
    0
  )
  expect_equal(
    resolve_waterpath_offset(0),
    0
  )
  expect_equal(
    rgl::material3d("lwd", id = water_path_ids$id[[1]]),
    0.5
  )

  scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    water_ior = 1.25,
    water_attenuation = 0.2
  )
  expect_false(any(vapply(
    scene$material,
    function(material) {
      identical(material$image, NA_character_) ||
        identical(material$bump_texture, NA_character_)
    },
    logical(1)
  )))
  expect_no_condition(rayrender:::process_scene(scene))
  material_types = vapply(
    scene$material,
    function(material) {
      material$type
    },
    integer(1)
  )
  expected_material_type = if (rayrender::has_denoiser()) {
    rayrender::microfacet(transmission = TRUE)[[1]]$type
  } else {
    rayrender::dielectric()[[1]]$type
  }
  expect_true(expected_material_type %in% material_types)
  if (rayrender::has_denoiser()) {
    microfacet_material = scene$material[[which(
      material_types == expected_material_type
    )[[1]]]]
    microfacet_info = microfacet_material$glossyinfo[[1]]
    expect_equal(microfacet_info[4:6], rep(1.25, 3))
  }
})

test_that("render_streams reads widths from an sf column", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 5,
    ncols = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 5,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  streams = sf::st_sf(
    stream_width = c(0.25, 0.75),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(2, 2, 4, 4), ncol = 2, byrow = TRUE)),
      crs = 3857
    )
  )

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_no_condition(render_streams(
    streams,
    heightmap = height_raster,
    watercolor = "dodgerblue",
    width_column = "stream_width",
    merge = TRUE
  ))

  water_path_ids = get_ids_with_labels(typeval = "water_path")
  expect_equal(nrow(water_path_ids), 2)
  water_path_widths = vapply(
    water_path_ids$id,
    function(id) rgl::material3d("lwd", id = id),
    numeric(1)
  )
  expect_equal(sort(water_path_widths), c(0.25, 0.75))
  expect_error(
    render_streams(
      streams,
      heightmap = height_raster,
      width_column = "missing_width"
    ),
    "`width_column` must name a column in `streams`: missing_width",
    fixed = TRUE
  )
})

test_that("water path densification samples terrain triangle boundaries", {
  heightmap = matrix(0, nrow = 5, ncol = 5)

  boundary_t = calculate_water_path_triangle_boundary_t(
    heightmap = heightmap,
    segment_start = c(-1.25, 0),
    segment_end = c(1.25, 0)
  )
  expect_true(all(c(0.1, 0.5, 0.9) %in% round(boundary_t, 8)))

  diagonal_t = calculate_water_path_triangle_boundary_t(
    heightmap = heightmap,
    segment_start = c(-0.9, -0.9),
    segment_end = c(-0.6, -0.2)
  )
  expect_true(any(abs(diagonal_t - 0.8) < 1e-8))

  densified = densify_single_water_path_coord(
    coords = matrix(c(-1.25, 0, 0, 1.25, 0, 0), ncol = 3, byrow = TRUE),
    heightmap = heightmap,
    offset = 0
  )
  expect_true(all(c(-1, 0, 1) %in% round(densified[, 1], 8)))
})

test_that("joined stream endpoint clamping snaps nearby branch endpoints", {
  main_line = matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE)
  branch_line = matrix(c(0.25, 0.75, 0.25, 0.2), ncol = 2, byrow = TRUE)

  clamped = clamp_render_highquality_water_path_endpoints(
    list(main_line, branch_line),
    width = 0.25
  )
  expect_equal(clamped[[2]][2, ], c(0.25, 0), tolerance = 1e-8)

  unclamped = clamp_render_highquality_water_path_endpoints(
    list(main_line, branch_line),
    width = 0.1
  )
  expect_equal(unclamped[[2]][2, ], c(0.25, 0.2), tolerance = 1e-8)

  collapsing_branch = matrix(c(0, 0.2, 0, -0.2), ncol = 2, byrow = TRUE)
  collapsed = clamp_render_highquality_water_path_endpoints(
    list(main_line, collapsing_branch),
    width = 0.25
  )
  expect_length(collapsed, 1)
  expect_equal(collapsed[[1]], main_line)
})

test_that("joined stream buffers dissolve junction footprints", {
  skip_if_not_installed("sf")

  collinear = make_render_highquality_water_path_buffer_footprint(
    lines = list(
      matrix(c(-1, 0, 0.5, 0), ncol = 2, byrow = TRUE),
      matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)
    ),
    width = 0.2
  )
  expect_equal(as.numeric(sf::st_area(collinear)), 0.4, tolerance = 1e-8)

  t_junction = make_render_highquality_water_path_buffer_footprint(
    lines = list(
      matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE),
      matrix(c(0, -1, 0, 0), ncol = 2, byrow = TRUE)
    ),
    width = 0.2
  )
  expect_equal(
    length(suppressWarnings(sf::st_cast(t_junction, "POLYGON"))),
    1
  )

  x_junction = make_render_highquality_water_path_buffer_footprint(
    lines = list(
      matrix(c(-1, -1, 1, 1), ncol = 2, byrow = TRUE),
      matrix(c(-1, 1, 1, -1), ncol = 2, byrow = TRUE)
    ),
    width = 0.2
  )
  expect_equal(
    length(suppressWarnings(sf::st_cast(x_junction, "POLYGON"))),
    1
  )

  acute = make_render_highquality_water_path_buffer_footprint(
    lines = list(
      matrix(c(0, 0, 1, 0.05), ncol = 2, byrow = TRUE),
      matrix(c(0, 0, 1, -0.05), ncol = 2, byrow = TRUE)
    ),
    width = 0.1
  )
  expect_lt(as.numeric(sf::st_bbox(acute)[["xmax"]]), 1.2)
})

test_that("joined stream terrain overlay clips invalid cells and closes meshes", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rayrender")

  heightmap_with_hole = matrix(0, nrow = 5, ncol = 5)
  heightmap_with_hole[3, 3] = NA
  terrain_triangles = make_render_highquality_water_path_valid_terrain_triangles(
    heightmap_with_hole
  )
  expect_equal(nrow(terrain_triangles), 24)

  flat_task = list(
    list(
      points = matrix(c(-1, 0, 0, 1, 0, 0), ncol = 3, byrow = TRUE),
      bbox_center = c(0, 0, 0),
      width = 1,
      heightmap = matrix(0, nrow = 5, ncol = 5),
      zscale = 1,
      material = rayrender::dielectric()
    )
  )
  flat_mesh = make_render_highquality_joined_water_path_mesh(flat_task)
  flat_info = flat_mesh$shape_info[[1]]$mesh_info[[1]]
  expect_equal(max(flat_info$vertices[, 2]), 0.2, tolerance = 1e-8)
  expect_lt(min(flat_info$vertices[, 2]), 0)
  expect_true(all(
    validate_render_highquality_water_path_edge_counts(flat_info$indices) == 2
  ))

  sloped_heightmap = matrix(rep(seq_len(5), times = 5), nrow = 5, ncol = 5)
  sloped_task = flat_task
  sloped_task[[1]]$heightmap = sloped_heightmap
  sloped_task[[1]]$points[, 2] = interpolate_spatial_water_height(
    sloped_heightmap,
    sloped_task[[1]]$points[, 1],
    sloped_task[[1]]$points[, 3]
  )
  sloped_mesh = make_render_highquality_joined_water_path_mesh(sloped_task)
  sloped_info = sloped_mesh$shape_info[[1]]$mesh_info[[1]]
  terrain_y = interpolate_spatial_water_height(
    sloped_heightmap,
    sloped_info$vertices[, 1],
    sloped_info$vertices[, 3]
  )
  y_delta = sloped_info$vertices[, 2] - terrain_y
  expect_true(all(
    abs(y_delta - 0.2) < 1e-8 |
      abs(y_delta + 1e-5) < 1e-8
  ))
})

test_that("render_highquality can render joined stream paths", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("rayrender")
  skip_if_not_installed("rayvertex")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 5,
    ncols = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 5,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  streams = sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(1, 2.5, 4, 2.5), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(2.5, 1, 2.5, 2.45), ncol = 2, byrow = TRUE)),
      crs = 3857
    )
  )

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_no_condition(render_streams(
    streams,
    heightmap = height_raster,
    watercolor = "dodgerblue",
    width = 0.5,
    merge = FALSE
  ))
  scene = render_highquality(
    return_scene = TRUE,
    light = FALSE,
    joined_stream_mesh = TRUE
  )
  expect_no_condition(rayrender:::process_scene(scene))
})

test_that("render_highquality validates joined stream mesh option", {
  expect_true("joined_stream_mesh" %in% names(formals(render_highquality)))
  expect_error(
    render_highquality(joined_stream_mesh = NA),
    "`joined_stream_mesh` must be TRUE or FALSE.",
    fixed = TRUE
  )
})

test_that("spatial water height interpolation matches terrain triangles", {
  saddle_heightmap = matrix(c(0, 0, 0, 10), nrow = 2, ncol = 2)

  expect_equal(
    interpolate_spatial_water_height(
      saddle_heightmap,
      x = -0.25,
      z = -0.25
    ),
    0
  )
  expect_equal(
    interpolate_spatial_water_height(
      saddle_heightmap,
      x = 0.25,
      z = 0.25
    ),
    5
  )
})

test_that("render_water resolves zscale from explicit spatial heightmaps", {
  skip_if_not_installed("terra")

  height_raster = terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 40,
    ymin = 0,
    ymax = 40,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0

  heightmap = resolve_render_water_heightmap(
    height_raster,
    heightmap_missing = FALSE,
    caller = "test"
  )

  expect_equal(attr(heightmap, "zscale", exact = TRUE), 10)
  expect_equal(
    resolve_render_water_effective_zscale(
      zscale = 1,
      zscale_missing = TRUE,
      vertical_exaggeration = 1,
      vertical_exaggeration_missing = FALSE,
      heightmap = heightmap,
      caller = "test"
    ),
    10
  )
  expect_equal(
    resolve_render_water_effective_zscale(
      zscale = 5,
      zscale_missing = FALSE,
      vertical_exaggeration = 1,
      vertical_exaggeration_missing = FALSE,
      heightmap = heightmap,
      caller = "test"
    ),
    5
  )
})

test_that("render_water scales spatial waterdepth rasters by zscale and vertical_exaggeration", {
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

  water_level_rast = terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:3857"
  )
  terra::values(water_level_rast) = 100

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    zscale = 10,
    vertical_exaggeration = 2,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))

  expect_no_condition(render_water(waterdepth = water_level_rast))
  water_ids = get_ids_with_labels(typeval = "water")
  water_verts = rgl::rgl.attrib(water_ids$id[1], "vertices")
  expect_equal(max(water_verts[, 2], na.rm = TRUE), 20, tolerance = 1e-6)

  expect_no_condition(render_water(
    waterdepth = water_level_rast,
    zscale = 20,
    vertical_exaggeration = 2
  ))
  water_ids = get_ids_with_labels(typeval = "water")
  water_verts = rgl::rgl.attrib(water_ids$id[1], "vertices")
  expect_equal(max(water_verts[, 2], na.rm = TRUE), 10, tolerance = 1e-6)
})

test_that("render_water clamps spatial waterdepth edges to terrain", {
  skip_if_not_installed("terra")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 6,
    ncols = 6,
    xmin = 0,
    xmax = 6,
    ymin = 0,
    ymax = 6,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 80
  water_level_rast = height_raster
  water_values = rep(NA_real_, terra::ncell(water_level_rast))
  water_values[c(15, 16, 21, 22)] = 100
  terra::values(water_level_rast) = water_values

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    zscale = 10,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))

  expect_no_condition(render_water(
    waterdepth = water_level_rast,
    water_edge_clamp = TRUE
  ))
  water_ids = get_ids_with_labels(typeval = "water")
  water_verts = rgl::rgl.attrib(water_ids$id[1], "vertices")
  expect_equal(max(water_verts[, 2], na.rm = TRUE), 8, tolerance = 1e-6)
})

test_that("spatial waterdepth rasters render finite cells at equal terrain height", {
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
  terra::values(height_raster) = 100
  water_level_rast = height_raster

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    zscale = 10,
    solid = FALSE,
    shadow = FALSE,
    water = TRUE,
    waterdepth = water_level_rast,
    windowsize = c(200, 200)
  ))

  water_ids = get_ids_with_labels(typeval = "water")
  expect_gt(nrow(water_ids), 0)
  water_verts = rgl::rgl.attrib(water_ids$id[1], "vertices")
  expect_equal(max(water_verts[, 2], na.rm = TRUE), 10, tolerance = 1e-6)
  expect_equal(
    rgl::material3d("polygon_offset", id = water_ids$id[1]),
    c(-1, -1)
  )
})

test_that("spatial waterdepth rasters render cell footprints instead of inset vertices", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 4, ncol = 4)
  water_surface[2:3, 2:3] = 10
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = matrix(0, nrow = 4, ncol = 4),
    valid_water = is.finite(water_surface)
  )
  vertices = water_mesh_vertices(water_mesh)

  expect_equal(nrow(vertices), 72)
  expect_equal(range(vertices[, 1]), c(-1.5, 1.5), tolerance = 1e-8)
  expect_equal(range(vertices[, 3]), c(-1.5, 1.5), tolerance = 1e-8)
  expect_equal(range(vertices[, 2]), c(0, 10), tolerance = 1e-8)
})

test_that("spatial waterdepth rasters can render isolated finite cells", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 4, ncol = 4)
  water_surface[2, 2] = 10
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = matrix(0, nrow = 4, ncol = 4),
    valid_water = is.finite(water_surface)
  )
  vertices = water_mesh_vertices(water_mesh)

  expect_equal(nrow(vertices), 30)
  expect_equal(range(vertices[, 1]), c(-1.5, 0.5), tolerance = 1e-8)
  expect_equal(range(vertices[, 3]), c(-1.5, 0.5), tolerance = 1e-8)
  expect_equal(range(vertices[, 2]), c(0, 10), tolerance = 1e-8)
})

test_that("spatial waterdepth edge extension can be disabled", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 4, ncol = 4)
  water_surface[2:3, 2:3] = 10
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = matrix(0, nrow = 4, ncol = 4),
    valid_water = is.finite(water_surface),
    water_edge_extension = 0
  )
  vertices = water_mesh_vertices(water_mesh)

  expect_equal(nrow(vertices), 72)
  expect_equal(range(vertices[, 1]), c(-1, 1), tolerance = 1e-8)
  expect_equal(range(vertices[, 3]), c(-1, 1), tolerance = 1e-8)
})

test_that("spatial waterdepth edge extension stops at terrain contact", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 4, ncol = 4)
  water_surface[2:3, 2:3] = 10

  flat_bank = matrix(10, nrow = 4, ncol = 4)
  flat_bank[2:3, 2:3] = 0
  flat_bank_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = flat_bank,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5
  )
  flat_bank_vertices = water_mesh_vertices(flat_bank_mesh)
  flat_bank_terrain = interpolate_spatial_water_surface_height(
    flat_bank,
    flat_bank_vertices[, 1],
    flat_bank_vertices[, 3]
  )

  expect_equal(range(flat_bank_vertices[, 1]), c(-1.5, 1.5), tolerance = 1e-8)
  expect_equal(range(flat_bank_vertices[, 3]), c(-1.5, 1.5), tolerance = 1e-8)
  expect_false(any(flat_bank_vertices[, 2] < flat_bank_terrain - 1e-6))

  rising_bank = matrix(12, nrow = 4, ncol = 4)
  rising_bank[2:3, 2:3] = 0
  rising_bank_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = rising_bank,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5
  )
  rising_bank_vertices = water_mesh_vertices(rising_bank_mesh)
  rising_bank_terrain = interpolate_spatial_water_surface_height(
    rising_bank,
    rising_bank_vertices[, 1],
    rising_bank_vertices[, 3]
  )

  expect_equal(
    range(rising_bank_vertices[, 1]),
    c(-4 / 3, 4 / 3),
    tolerance = 1e-5
  )
  expect_equal(
    range(rising_bank_vertices[, 3]),
    c(-4 / 3, 4 / 3),
    tolerance = 1e-5
  )
  expect_false(any(rising_bank_vertices[, 2] < rising_bank_terrain - 1e-6))
})

test_that("spatial waterdepth edge extension does not expand into high banks", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 4, ncol = 4)
  water_surface[2:3, 2:3] = 10
  heightmap = matrix(20, nrow = 4, ncol = 4)
  heightmap[2:3, 2:3] = 0

  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5
  )
  vertices = water_mesh_vertices(water_mesh)
  terrain = interpolate_spatial_water_surface_height(
    heightmap,
    vertices[, 1],
    vertices[, 3]
  )

  expect_equal(range(vertices[, 1]), c(-1, 1), tolerance = 1e-8)
  expect_equal(range(vertices[, 3]), c(-1, 1), tolerance = 1e-8)
  expect_false(any(vertices[, 2] < terrain - 1e-6))
})

test_that("spatial waterdepth edge extension stops at NA terrain cuts", {
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 5, ncol = 5)
  heightmap[1, ] = NA_real_
  heightmap[, 5] = NA_real_

  water_surface = matrix(NA_real_, nrow = 5, ncol = 5)
  water_surface[2:4, 2:4] = 10
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(heightmap) & is.finite(water_surface),
    water_edge_extension = 0.5
  )
  vertices = water_mesh_vertices(water_mesh)
  terrain = interpolate_spatial_water_surface_height(
    heightmap,
    vertices[, 1],
    vertices[, 3]
  )

  expect_equal(range(vertices[, 1]), c(-1, 2), tolerance = 1e-5)
  expect_equal(range(vertices[, 3]), c(-2, 1), tolerance = 1e-5)
  expect_true(all(is.finite(terrain)))
  expect_false(any(vertices[, 2] < terrain - 1e-6))

  heightmap = matrix(0, nrow = 5, ncol = 5)
  heightmap[, 3] = NA_real_
  water_surface = matrix(NA_real_, nrow = 5, ncol = 5)
  water_surface[2:4, ] = 10
  strip_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(heightmap) & is.finite(water_surface),
    water_edge_extension = 0.5
  )
  strip_vertices = water_mesh_vertices(strip_mesh)
  strip_terrain = interpolate_spatial_water_surface_height(
    heightmap,
    strip_vertices[, 1],
    strip_vertices[, 3]
  )

  expect_true(all(abs(strip_vertices[, 3]) >= 1 - 1e-8))
  expect_true(all(is.finite(strip_terrain)))
  expect_false(any(strip_vertices[, 2] < strip_terrain - 1e-6))
})

test_that("spatial water sidewalls are clipped to the expanded water footprint", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 5, ncol = 5)
  water_surface[2:4, 2:4] = 10
  water_surface[3, 3] = NA_real_

  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = matrix(0, nrow = 5, ncol = 5),
    valid_water = is.finite(water_surface)
  )
  vertices = water_mesh_vertices(water_mesh)
  triangle_info = water_mesh_triangle_normals(vertices)
  sidewalls = abs(triangle_info$normals[, 2]) < 1e-8
  sidewall_centers = triangle_info$centers[sidewalls, , drop = FALSE]

  expect_equal(nrow(vertices), 120)
  expect_false(any(
    abs(sidewall_centers[, 1]) < 1e-8 &
      abs(sidewall_centers[, 3]) < 0.5
  ))
  expect_false(any(
    abs(sidewall_centers[, 3]) < 1e-8 &
      abs(sidewall_centers[, 1]) < 0.5
  ))

  water_mesh_no_extension = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = matrix(0, nrow = 5, ncol = 5),
    valid_water = is.finite(water_surface),
    water_edge_extension = 0
  )
  vertices_no_extension = water_mesh_vertices(water_mesh_no_extension)

  expect_equal(nrow(vertices_no_extension), 144)
})

test_that("spatial waterdepth edge sides follow local terrain heights", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 4, ncol = 4)
  water_surface[2, 2] = 10
  heightmap = matrix(0, nrow = 4, ncol = 4)
  heightmap[1:2, 1:2] = 4
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0
  )
  vertices = water_mesh_vertices(water_mesh)

  expect_true(any(abs(vertices[, 2] - 4) < 1e-8))
  expect_equal(max(vertices[, 2]), 10, tolerance = 1e-8)
})

test_that("spatial water edge clamp lowers surfaces to exterior sidewall terrain", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 6, ncol = 6)
  water_surface[3:4, 3:4] = 10
  heightmap = matrix(8, nrow = 6, ncol = 6)

  unclamped_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5
  )
  clamped_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5,
    water_edge_clamp = TRUE
  )
  unclamped_vertices = water_mesh_vertices(unclamped_mesh)
  clamped_vertices = water_mesh_vertices(clamped_mesh)
  clamped_info = water_mesh_triangle_normals(clamped_vertices)
  clamped_sidewalls = abs(clamped_info$normals[, 2]) < 1e-8

  expect_equal(max(unclamped_vertices[, 2]), 10, tolerance = 1e-8)
  expect_equal(range(clamped_vertices[, 2]), c(8, 8), tolerance = 1e-8)
  expect_false(any(clamped_sidewalls))
})

test_that("spatial water edge clamp uses largest eligible sidewall height", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 7, ncol = 7)
  water_surface[3:5, 3:5] = 10
  heightmap = matrix(5, nrow = 7, ncol = 7)
  heightmap[3:5, 3:5] = 0
  heightmap[2, ] = 8
  heightmap[, 2] = 8

  adjusted = adjust_spatial_water_surface_to_edge_terrain(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = 0.5
  )

  expect_equal(adjusted[3, 3], 5, tolerance = 1e-8)
  expect_equal(adjusted[5, 5], 5, tolerance = 1e-8)

  clamped_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5,
    water_edge_clamp = TRUE
  )
  clamped_vertices = water_mesh_vertices(clamped_mesh)
  clamped_info = water_mesh_triangle_normals(clamped_vertices)
  clamped_sidewalls = abs(clamped_info$normals[, 2]) < 1e-8

  expect_false(any(clamped_sidewalls))
})

test_that("spatial water edge clamp flattens connected footprint levels", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 5, ncol = 5)
  water_surface[2:4, 2:4] = 10
  water_surface[2, 2] = 5
  water_surface[4, 4] = 6
  heightmap = matrix(10, nrow = 5, ncol = 5)
  heightmap[is.finite(water_surface)] = 0

  adjusted = adjust_spatial_water_surface_to_edge_terrain(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = 0.5
  )
  clamped_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5,
    water_edge_clamp = TRUE
  )
  clamped_vertices = water_mesh_vertices(clamped_mesh)
  clamped_info = water_mesh_triangle_normals(clamped_vertices)
  clamped_sidewalls = abs(clamped_info$normals[, 2]) < 1e-8

  expect_equal(unique(na.omit(as.vector(adjusted))), 10, tolerance = 1e-8)
  expect_equal(range(clamped_vertices[, 2]), c(10, 10), tolerance = 1e-8)
  expect_false(any(clamped_sidewalls))
})

test_that("spatial water edge clamp is applied per connected footprint", {
  water_surface = matrix(NA_real_, nrow = 6, ncol = 6)
  water_surface[2, 2] = 10
  water_surface[5, 5] = 10
  heightmap = matrix(8, nrow = 6, ncol = 6)
  heightmap[4:6, 4:6] = 6

  adjusted = adjust_spatial_water_surface_to_edge_terrain(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = 0
  )

  expect_equal(adjusted[2, 2], 8, tolerance = 1e-8)
  expect_equal(adjusted[5, 5], 6, tolerance = 1e-8)
})

test_that("spatial water edge clamp ignores heightmap boundary edges", {
  water_surface = matrix(NA_real_, nrow = 4, ncol = 4)
  water_surface[1:2, 1:2] = 10
  heightmap = matrix(10, nrow = 4, ncol = 4)
  heightmap[1, 1] = 8

  adjusted = adjust_spatial_water_surface_to_edge_terrain(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = 0
  )

  expect_equal(adjusted[1, 1], 10, tolerance = 1e-8)
  expect_equal(adjusted[2, 2], 10, tolerance = 1e-8)
})

test_that("spatial water edge clamp ignores expanded heightmap boundary edges", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 5, ncol = 5)
  water_surface[2:4, 2:4] = 10
  heightmap = matrix(10, nrow = 5, ncol = 5)
  heightmap[2:4, 2:4] = 0
  heightmap[1, ] = 5

  adjusted = adjust_spatial_water_surface_to_edge_terrain(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = 0.5
  )
  clamped_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5,
    water_edge_clamp = TRUE
  )
  clamped_vertices = water_mesh_vertices(clamped_mesh)
  clamped_info = water_mesh_triangle_normals(clamped_vertices)
  clamped_sidewalls = abs(clamped_info$normals[, 2]) < 1e-8
  sidewall_centers = clamped_info$centers[clamped_sidewalls, , drop = FALSE]
  sidewall_on_scene_edge = abs(sidewall_centers[, 1] + 2) < 1e-8 |
    abs(sidewall_centers[, 1] - 2) < 1e-8 |
    abs(sidewall_centers[, 3] + 2) < 1e-8 |
    abs(sidewall_centers[, 3] - 2) < 1e-8

  expect_equal(unique(na.omit(as.vector(adjusted))), 10, tolerance = 1e-8)
  expect_equal(max(clamped_vertices[, 2]), 10, tolerance = 1e-8)
  expect_true(any(clamped_sidewalls))
  expect_true(all(sidewall_on_scene_edge))
})

test_that("spatial water edge clamp restores low edge cells on open cuts", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 5, ncol = 5)
  water_surface[1:3, 2:4] = 10
  water_surface[1, 2:4] = 5
  heightmap = matrix(10, nrow = 5, ncol = 5)
  heightmap[is.finite(water_surface)] = 0

  adjusted = adjust_spatial_water_surface_to_edge_terrain(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = 0.5
  )
  clamped_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0.5,
    water_edge_clamp = TRUE
  )
  clamped_vertices = water_mesh_vertices(clamped_mesh)
  clamped_info = water_mesh_triangle_normals(clamped_vertices)
  clamped_sidewalls = abs(clamped_info$normals[, 2]) < 1e-8
  sidewall_centers = clamped_info$centers[clamped_sidewalls, , drop = FALSE]

  expect_equal(adjusted[1, 2], 10, tolerance = 1e-8)
  expect_equal(max(clamped_vertices[, 2]), 10, tolerance = 1e-8)
  expect_true(any(clamped_sidewalls))
  expect_true(all(abs(sidewall_centers[, 1] + 2) < 1e-8))
})

test_that("spatial water edge clamp ignores NA slice edges", {
  water_surface = matrix(10, nrow = 5, ncol = 5)
  water_surface[3, 3] = NA_real_
  heightmap = matrix(8, nrow = 5, ncol = 5)
  heightmap[3, 3] = NA_real_

  adjusted = adjust_spatial_water_surface_to_edge_terrain(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = 0
  )

  expect_equal(adjusted[1, 1], 10, tolerance = 1e-8)
  expect_equal(adjusted[3, 2], 10, tolerance = 1e-8)
  expect_true(is.na(adjusted[3, 3]))
})

test_that("spatial water edge clamp keeps sidewalls at void cuts", {
  local_rgl_use_null()

  water_surface = matrix(10, nrow = 5, ncol = 5)
  water_surface[3, 3] = NA_real_
  heightmap = matrix(8, nrow = 5, ncol = 5)
  heightmap[3, 3] = NA_real_
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(heightmap) & is.finite(water_surface),
    water_edge_extension = 0,
    water_edge_clamp = TRUE
  )
  vertices = water_mesh_vertices(water_mesh)
  triangle_info = water_mesh_triangle_normals(vertices)
  sidewalls = abs(triangle_info$normals[, 2]) < 1e-8

  expect_true(any(sidewalls))
  expect_true(any(
    sidewalls &
      abs(triangle_info$centers[, 1]) < 0.6 &
      abs(triangle_info$centers[, 3]) < 0.6
  ))
})

test_that("spatial polygon water fits full raster coverage", {
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  local_rgl_use_null()

  heightmap = matrix(10, nrow = 7, ncol = 7)
  heightmap[3:5, 3:5] = 0
  water_surface = matrix(5, nrow = 7, ncol = 7)

  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_render_method = "polygon",
    water_edge_extension = 0
  )
  vertices = water_mesh_vertices(water_mesh)
  triangle_info = water_mesh_triangle_normals(vertices)
  sidewalls = abs(triangle_info$normals[, 2]) < 1e-8

  expect_gt(nrow(vertices), 0)
  expect_equal(range(vertices[, 2]), c(10, 10), tolerance = 1e-8)
  expect_false(any(sidewalls))
  expect_equal(range(vertices[, 1]), c(-3, 3), tolerance = 1e-8)
  expect_equal(range(vertices[, 3]), c(-3, 3), tolerance = 1e-8)
})

test_that("spatial polygon water fits contour area to raster coverage", {
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  local_rgl_use_null()

  coord = seq(-3, 3)
  heightmap = outer(coord, coord, function(x, z) sqrt(x^2 + z^2))
  water_surface = matrix(NA_real_, nrow = 7, ncol = 7)
  water_surface[3:5, 3:5] = 100
  component_mask = is.finite(water_surface)
  component_footprint = make_spatial_water_component_footprint(component_mask)
  target_area = spatial_water_polygon_area(component_footprint)
  fit = fit_spatial_water_component_polygon(
    component_mask = component_mask,
    heightmap = heightmap,
    component_footprint = component_footprint,
    fallback_level = 100
  )

  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_render_method = "polygon",
    water_edge_extension = 0.5
  )
  vertices = water_mesh_vertices(water_mesh)
  triangle_info = water_mesh_triangle_normals(vertices)
  sidewalls = abs(triangle_info$normals[, 2]) < 1e-8

  expect_equal(fit$area, target_area, tolerance = 1e-3)
  expect_lt(fit$level, 100)
  expect_equal(max(vertices[, 2]), fit$level, tolerance = 1e-8)
  expect_false(any(sidewalls))
  expect_lt(max(abs(vertices[, 1])), 2)
  expect_lt(max(abs(vertices[, 3])), 2)
})

test_that("spatial polygon water rejects open contour floods", {
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  local_rgl_use_null()

  heightmap = matrix(rep(seq(0, 10, length.out = 50), each = 50), 50, 50)
  water_surface = matrix(NA_real_, nrow = 50, ncol = 50)
  water_surface[20:30, 20:30] = 100
  component_mask = is.finite(water_surface)
  component_footprint = make_spatial_water_component_footprint(component_mask)
  target_area = spatial_water_polygon_area(component_footprint)
  target_area_limit = target_area +
    spatial_water_polygon_perimeter(component_footprint)
  candidate = spatial_water_component_area_fit_at_level(
    heightmap = heightmap,
    component_footprint = component_footprint,
    level = stats::median(heightmap[component_mask]),
    target_area = target_area,
    target_area_limit = target_area_limit,
    cache = new.env(parent = emptyenv())
  )
  fit = fit_spatial_water_component_polygon(
    component_mask = component_mask,
    heightmap = heightmap,
    component_footprint = component_footprint,
    fallback_level = 100
  )

  expect_gt(candidate$area, target_area_limit)
  expect_true(candidate$rejected)
  expect_null(fit)
})

test_that("spatial polygon water samples levels across DEM range", {
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  local_rgl_use_null()

  heightmap = matrix(seq(0, 1, length.out = 25), nrow = 5, ncol = 5)
  water_surface = matrix(1000, nrow = 5, ncol = 5)
  component_mask = is.finite(water_surface)
  component_footprint = make_spatial_water_component_footprint(component_mask)
  fit = fit_spatial_water_component_polygon(
    component_mask = component_mask,
    heightmap = heightmap,
    component_footprint = component_footprint,
    fallback_level = max(water_surface)
  )

  expect_lte(fit$level, max(heightmap) + 1e-8)
  expect_gte(fit$level, min(heightmap) - 1e-8)
})

test_that("spatial polygon water can fit components with mirai", {
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  skip_if_not_installed("mirai")
  local_rgl_use_null()
  compute_profile = spatial_water_mirai_compute_profile()
  can_start_daemons = tryCatch(
    {
      mirai::daemons(1, .compute = compute_profile)
      TRUE
    },
    error = function(e) FALSE,
    finally = {
      try(mirai::daemons(0, .compute = compute_profile), silent = TRUE)
    }
  )
  if (!can_start_daemons) {
    skip("mirai daemons are unavailable in this environment")
  }

  heightmap = matrix(10, nrow = 12, ncol = 12)
  heightmap[3:4, 3:4] = 0
  heightmap[9:10, 9:10] = 0
  water_surface = matrix(NA_real_, nrow = 12, ncol = 12)
  water_surface[3:4, 3:4] = 100
  water_surface[9:10, 9:10] = 100

  serial_mesh = make_spatial_water_polygon_surface(
    water_surface = water_surface,
    heightmap = heightmap,
    parallel = FALSE
  )
  parallel_mesh = make_spatial_water_polygon_surface(
    water_surface = water_surface,
    heightmap = heightmap,
    parallel = 2
  )

  expect_equal(parallel_mesh$vertices, serial_mesh$vertices, tolerance = 1e-8)
  expect_equal(parallel_mesh$lines, serial_mesh$lines, tolerance = 1e-8)
})

test_that("spatial polygon water selects only intersecting contour islands", {
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  local_rgl_use_null()

  heightmap = matrix(10, nrow = 9, ncol = 9)
  heightmap[2:3, 2:3] = 0
  heightmap[7:8, 7:8] = 0
  water_surface = matrix(NA_real_, nrow = 9, ncol = 9)
  water_surface[2:3, 2:3] = 100

  terrain_band = make_spatial_water_level_polygon(heightmap, 5)
  component_footprint = make_spatial_water_component_footprint(
    is.finite(water_surface)
  )
  selected_band = select_spatial_water_component_polygons(
    terrain_band = terrain_band,
    component_footprint = component_footprint
  )
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_render_method = "polygon"
  )
  vertices = water_mesh_vertices(water_mesh)

  expect_equal(length(terrain_band), 2)
  expect_equal(length(selected_band), 1)
  expect_equal(spatial_water_polygon_area(selected_band), 3.5)
  expect_lt(max(vertices[, 1]), 0)
  expect_lt(max(vertices[, 3]), 0)
})

test_that("spatial polygon water edge level ignores scene and void edges", {
  heightmap = matrix(0, nrow = 5, ncol = 5)
  heightmap[1, ] = 100
  heightmap[3, ] = 2
  component_mask = matrix(FALSE, nrow = 5, ncol = 5)
  component_mask[1:3, ] = TRUE

  expect_equal(
    mean_spatial_water_component_edge_height(component_mask, heightmap),
    2
  )

  heightmap[4, ] = NA_real_
  expect_false(is.finite(mean_spatial_water_component_edge_height(
    component_mask,
    heightmap
  )))
})

test_that("spatial polygon water keeps sidewalls at scene and NA cuts", {
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 5, ncol = 5)
  heightmap[1, 1] = 5
  heightmap[3, 3] = NA_real_
  water_surface = matrix(5, nrow = 5, ncol = 5)

  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(heightmap) & is.finite(water_surface),
    water_render_method = "polygon",
    water_edge_extension = 0
  )
  vertices = water_mesh_vertices(water_mesh)
  triangle_info = water_mesh_triangle_normals(vertices)
  sidewalls = abs(triangle_info$normals[, 2]) < 1e-8
  sidewall_centers = triangle_info$centers[sidewalls, , drop = FALSE]
  scene_edge = abs(sidewall_centers[, 1]) >= 2 - 1e-8 |
    abs(sidewall_centers[, 3]) >= 2 - 1e-8

  expect_true(any(sidewalls))
  expect_true(any(scene_edge))
  expect_true(any(!scene_edge))
  expect_true(all(abs(sidewall_centers[!scene_edge, 1]) <= 1 + 1e-8))
  expect_true(all(abs(sidewall_centers[!scene_edge, 3]) <= 1 + 1e-8))
})

test_that("spatial polygon water supports non-square heightmaps", {
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 4, ncol = 6)
  heightmap[1, 1] = 5
  water_surface = matrix(5, nrow = 4, ncol = 6)

  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_render_method = "polygon",
    water_edge_extension = 0
  )
  vertices = water_mesh_vertices(water_mesh)

  expect_gt(nrow(vertices), 0)
  expect_equal(range(vertices[, 1]), c(-1.5, 1.5), tolerance = 1e-8)
  expect_equal(range(vertices[, 3]), c(-2.5, 2.5), tolerance = 1e-8)
  expect_equal(max(vertices[, 2]), 5, tolerance = 1e-8)
})

test_that("render_water accepts spatial polygon water method", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")
  skip_if_not_installed("decido")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 4,
    ncols = 6,
    xmin = 0,
    xmax = 6,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0
  water_level_rast = height_raster
  terra::values(water_level_rast) = 50

  expect_no_condition(plot_3d_test(
    constant_shade(height_raster),
    height_raster,
    zscale = 10,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  expect_no_condition(render_water(
    waterdepth = water_level_rast,
    water_render_method = "polygon"
  ))
  water_ids = get_ids_with_labels(typeval = "water")
  water_verts = rgl::rgl.attrib(water_ids$id[1], "vertices")
  expect_equal(max(water_verts[, 2], na.rm = TRUE), 0, tolerance = 1e-6)
})

test_that("spatial water top is clipped against interior terrain peaks", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 3, ncol = 3)
  water_surface[2, 2] = 10
  heightmap = matrix(0, nrow = 3, ncol = 3)
  heightmap[2, 2] = 20
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0
  )
  vertices = water_mesh_vertices(water_mesh)
  triangle_info = water_mesh_triangle_normals(vertices)
  top_triangles = abs(triangle_info$normals[, 2]) > 1e-8
  top_centers = triangle_info$centers[top_triangles, , drop = FALSE]
  center_terrain = interpolate_spatial_water_surface_height(
    heightmap,
    top_centers[, 1],
    top_centers[, 3]
  )

  expect_true(all(top_centers[, 2] >= center_terrain - 1e-6))
  expect_false(any(
    abs(top_centers[, 1]) < 0.1 &
      abs(top_centers[, 3]) < 0.1
  ))
})

test_that("spatial water sidewall bottoms follow terrain breakpoints", {
  heightmap = matrix(rep(c(0, 4, 1, 6, 2), each = 5), nrow = 5)

  vertices = make_spatial_water_sidewall_vertices(
    heightmap = heightmap,
    x_start = -1.5,
    z_start = -1.5,
    x_end = -1.5,
    z_end = 1.5,
    water_height = 20
  )
  bottom_vertices = vertices[vertices[, 2] < 20 - 1e-8, , drop = FALSE]
  terrain = interpolate_spatial_water_surface_height(
    heightmap,
    bottom_vertices[, 1],
    bottom_vertices[, 3]
  )

  expect_gt(nrow(vertices), 6)
  expect_gt(length(unique(round(bottom_vertices[, 3], 8))), 2)
  expect_equal(bottom_vertices[, 2], terrain, tolerance = 1e-8)
})

test_that("spatial water sidewalls are drawn when adjacent water is lower", {
  local_rgl_use_null()

  water_surface = matrix(NA_real_, nrow = 3, ncol = 3)
  water_surface[2, 2] = 10
  water_surface[3, 2] = 5
  heightmap = matrix(0, nrow = 3, ncol = 3)
  water_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0
  )
  vertices = water_mesh_vertices(water_mesh)
  triangle_info = water_mesh_triangle_normals(vertices)
  sidewalls = abs(triangle_info$normals[, 2]) < 1e-8
  internal_sidewalls = which(
    sidewalls &
      abs(triangle_info$centers[, 1] - 0.5) < 1e-8 &
      abs(triangle_info$centers[, 3]) < 0.5
  )
  reaches_ground = vapply(
    internal_sidewalls,
    function(triangle_index) {
      triangle = vertices[
        seq.int(3L * triangle_index - 2L, 3L * triangle_index),
        ,
        drop = FALSE
      ]
      any(abs(triangle[, 2]) < 1e-8) &&
        any(abs(triangle[, 2] - 10) < 1e-8)
    },
    logical(1)
  )

  expect_true(any(reaches_ground))

  water_surface[3, 2] = 10
  equal_level_mesh = make_spatial_water_surface(
    waterheight = water_surface,
    heightmap = heightmap,
    valid_water = is.finite(water_surface),
    water_edge_extension = 0
  )
  equal_level_vertices = water_mesh_vertices(equal_level_mesh)
  equal_level_info = water_mesh_triangle_normals(equal_level_vertices)
  equal_level_sidewalls = abs(equal_level_info$normals[, 2]) < 1e-8

  expect_false(any(
    equal_level_sidewalls &
      abs(equal_level_info$centers[, 1] - 0.5) < 1e-8 &
      abs(equal_level_info$centers[, 3]) < 0.5
  ))
})

test_that("plot_3d renders explicit spatial waterdepth and applies cached zscale", {
  skip_if_not_installed("terra")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  height_raster = terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 40,
    ymin = 0,
    ymax = 40,
    crs = "EPSG:3857"
  )
  terra::values(height_raster) = 0

  water_level_rast = terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 40,
    ymin = 0,
    ymax = 40,
    crs = "EPSG:3857"
  )
  terra::values(water_level_rast) = 100

  hillshade = sphere_shade(height_raster, vertical_exaggeration = 2)
  expect_no_condition(plot_3d_test(
    hillshade,
    vertical_exaggeration = 2,
    waterdepth = water_level_rast,
    solid = FALSE,
    shadow = FALSE,
    windowsize = c(200, 200)
  ))

  water_ids = get_ids_with_labels(typeval = "water")
  expect_gt(nrow(water_ids), 0)
  water_verts = rgl::rgl.attrib(water_ids$id[1], "vertices")
  expect_equal(max(water_verts[, 2], na.rm = TRUE), 20, tolerance = 1e-6)
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
