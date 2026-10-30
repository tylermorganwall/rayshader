# These helpers inspect the public rgl result, including its winding and volume.
polygon_water_test_vertices = function() {
  objects = rgl::ids3d(tags = TRUE)
  ids = objects$id[objects$tag == 'water']
  do.call(rbind, lapply(ids, function(id) rgl::rgl.attrib(id, 'vertices')))
}
polygon_water_test_top_area = function(v) {
  a = v[seq(1, nrow(v), 3), , drop = FALSE]
  b = v[seq(2, nrow(v), 3), , drop = FALSE]
  c = v[seq(3, nrow(v), 3), , drop = FALSE]
  area = ((b[, 3] - a[, 3]) *
    (c[, 1] - a[, 1]) -
    (b[, 1] - a[, 1]) * (c[, 3] - a[, 3])) /
    2
  sum(area[area > 0])
}
polygon_water_test_scene = function(triangulate = FALSE, vals = 0) {
  r = terra::rast(
    nrows = 8,
    ncols = 12,
    xmin = 0,
    xmax = 12,
    ymin = 0,
    ymax = 8,
    crs = 'EPSG:3857',
    vals = vals
  )
  plot_3d_test(
    height_shade(r),
    r,
    zscale = 1,
    triangulate = triangulate,
    geographic_aspect = FALSE,
    solid = FALSE,
    shadow = FALSE
  )
  r
}

test_that('exact polygon water retains subcell footprints, holes, and closed outward faces', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  p = sf::st_as_sfc(
    'POLYGON ((4.1 3.1,4.4 3.1,4.4 3.4,4.1 3.4,4.1 3.1),(4.2 3.2,4.2 3.3,4.3 3.3,4.3 3.2,4.2 3.2))',
    crs = 3857
  )
  for (triangulate in c(FALSE, TRUE)) {
    polygon_water_test_scene(triangulate)
    render_water(
      p,
      water_elevation = 1,
      water_line_color = 'white'
    )
    v = polygon_water_test_vertices()
    expected_area = (.3^2 - .1^2) * 11 / 12 * 7 / 8
    expect_equal(
      polygon_water_test_top_area(v),
      expected_area,
      tolerance = 1e-6
    )
    expect_equal(range(v[, 2]), c(0, 1))
    a = v[seq(1, nrow(v), 3), ]
    b = v[seq(2, nrow(v), 3), ]
    c = v[seq(3, nrow(v), 3), ]
    cross = cbind(
      b[, 2] * c[, 3] - b[, 3] * c[, 2],
      b[, 3] * c[, 1] - b[, 1] * c[, 3],
      b[, 1] * c[, 2] - b[, 2] * c[, 1]
    )
    expect_equal(sum(a * cross) / 6, expected_area, tolerance = 1e-6)
    keys = apply(round(v, 6), 1, paste, collapse = ',')
    faces = matrix(keys, ncol = 3, byrow = TRUE)
    edges = rbind(faces[, 1:2], faces[, 2:3], faces[, c(3, 1)])
    expect_true(all(
      table(apply(edges, 1, function(e) paste(sort(e), collapse = '|'))) == 2
    ))
    expect_gt(
      nrow(rgl::rgl.attrib(
        get_ids_with_labels(typeval = 'waterlines')$id,
        'vertices'
      )),
      0
    )
  }
})

test_that('polygon clearance is conditional and respects sensitivity and elevation scale', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  r = polygon_water_test_scene()
  p = sf::st_as_sfc('POLYGON ((2 2,5 2,5 5,2 5,2 2))', crs = 3857)
  for (level in c(NA, 0, 0.01, 1)) {
    render_water(
      p,
      water_render_method = 'polygon',
      water_elevation = if (is.na(level)) NULL else level
    )
    top = max(polygon_water_test_vertices()[, 2])
    if (is.na(level) || level == 0) {
      expect_gt(top, 0)
      expect_lt(top, 0.001)
    } else {
      expect_equal(top, level, tolerance = 1e-6)
    }
  }
  render_water(
    p,
    water_render_method = 'polygon',
    water_elevation = -.01,
    water_surface_tolerance = .02
  )
  expect_gt(max(polygon_water_test_vertices()[, 2]), 0)
  expect_error(
    render_water(
      p,
      water_render_method = 'polygon',
      water_surface_tolerance = -1
    ),
    'water_surface_tolerance'
  )
  terra::values(r) = 100
  plot_3d_test(
    height_shade(r),
    r,
    zscale = 100,
    solid = FALSE,
    shadow = FALSE,
    water_input = p,
    water_render_method = 'polygon',
    water_elevation = 100
  )
  expect_gt(max(polygon_water_test_vertices()[, 2]), 1)
  expect_equal(min(polygon_water_test_vertices()[, 2]), 1)
})

test_that('polygon bottoms and coincidence use the actual rendered terrain', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  polygon_water_test_scene(TRUE)
  # Substitute a planar terrain sloping across a square; the heightmap cache
  # deliberately remains zero. Exact intersections determine the bottom.
  rgl::pop3d(tag = 'surface_tris-dim_12_8')
  v = rbind(
    c(-5, 0, -3),
    c(-5, 0, 3),
    c(5, 2, -3),
    c(5, 2, -3),
    c(-5, 0, 3),
    c(5, 2, 3)
  )
  rgl::triangles3d(v, tag = 'surface_tris-dim_12_8')
  p = sf::st_as_sfc('POLYGON ((2 2,5 2,5 5,2 5,2 2))', crs = 3857)
  render_water(p, water_render_method = 'polygon', water_elevation = 3)
  v = polygon_water_test_vertices()
  expect_equal(min(v[, 2]), (min(v[, 1]) + 5) * .2, tolerance = 1e-6)
  expect_equal(max(v[, 2]), 3)
})

test_that('polygon water supports native raster footprints and nonspatial levels', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  r = polygon_water_test_scene()
  w = terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 4.1,
    xmax = 4.3,
    ymin = 3.1,
    ymax = 3.3,
    crs = 'EPSG:3857',
    vals = 1
  )
  render_water(w, water_render_method = 'polygon')
  expect_equal(
    polygon_water_test_top_area(polygon_water_test_vertices()),
    .2^2 * 11 / 12 * 7 / 8,
    tolerance = 1e-6
  )
  render_water(1, water_render_method = 'polygon')
  expect_equal(
    polygon_water_test_top_area(polygon_water_test_vertices()),
    11 * 7,
    tolerance = 1e-6
  )
  h = matrix(0, 12, 8)
  h[5:6, 4:5] = NA_real_
  plot_3d_test(
    array(1, c(12, 8, 3)),
    h,
    solid = FALSE,
    shadow = FALSE,
    water_input = 1,
    water_render_method = 'polygon'
  )
  expect_lt(polygon_water_test_top_area(polygon_water_test_vertices()), 11 * 7)
})

test_that('polygon parts retain original elevations and alternate input CRSs', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  polygon_water_test_scene()
  p = sf::st_as_sf(
    data.frame(
      level = c(90, 1, 2),
      wkt = c(
        'POINT (1 1)',
        'POLYGON ((2 2,3 2,3 3,2 3,2 2))',
        'POLYGON ((6 2,7 2,7 3,6 3,6 2))'
      )
    ),
    wkt = 'wkt',
    crs = 3857
  )
  render_water(
    sf::st_transform(p, 4326),
    water_render_method = 'polygon',
    water_elevation = 'level'
  )
  expect_equal(sort(unique(polygon_water_test_vertices()[, 2])), c(0, 1, 2))
})


test_that('polygon is the public default', {
  expect_identical(
    formals(render_water)$water_render_method,
    quote(c("polygon", "raster", "optimized", "legacy"))
  )
  expect_identical(
    formals(plot_3d)$water_render_method,
    quote(c("polygon", "raster", "optimized", "legacy"))
  )
})


test_that("roundoff-collinear shorelines do not drop a river polygon", {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  # A clipped DC river outline with nearly collinear terrain-edge crossings.
  g = readRDS(test_path("fixtures", "polygon-water-shoreline.rds"))
  expected_area = as.numeric(sf::st_area(g))
  geometry = sf::st_sfc(
    lapply(g, function(polygon) {
      sf::st_polygon(lapply(polygon, function(ring) {
        ring[, 2] = -ring[, 2]
        ring
      }))
    }),
    crs = 3857
  )
  rgl::open3d()
  cache_scene_context_token()
  rgl::triangles3d(
    rbind(
      c(-100, 0, -100),
      c(-100, 0, 100),
      c(100, 0, -100),
      c(100, 0, -100),
      c(-100, 0, 100),
      c(100, 0, 100)
    ),
    tag = "surface_tris-dim_201_201"
  )
  expect_no_warning(
    mesh <- make_polygon_water(
      matrix(0, 201, 201),
      geometry,
      1,
      c(xmin = -100, xmax = 100, ymin = -100, ymax = 100),
      3857,
      water_elevation = 1
    )
  )
  expect_length(mesh$vertices, 1)
  expect_equal(
    polygon_water_test_top_area(do.call(rbind, mesh$vertices)),
    expected_area,
    tolerance = 1e-7
  )
})
