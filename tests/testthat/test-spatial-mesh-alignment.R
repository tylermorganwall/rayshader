test_that("absolute mesh vertices align with spatial points at every zscale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("rayvertex")
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  r = terra::rast(
    nrows = 12,
    ncols = 20,
    xmin = 100,
    xmax = 500,
    ymin = 600,
    ymax = 900,
    crs = "EPSG:3857",
    vals = 0
  )
  xyz = rbind(
    c(200, 700, 20),
    c(300, 700, 20),
    c(300, 800, 20),
    c(200, 700, 20)
  )
  building = sf::st_sfc(sf::st_multipolygon(list(list(xyz))), crs = 3857)
  original_xy = NULL
  for (scale in c(5, 30)) {
    plot_3d_test(
      height_shade(r),
      r,
      zscale = scale,
      geographic_aspect = FALSE,
      solid = FALSE,
      shadow = FALSE
    )
    h = get_scene_heightmap()
    expected = transform_into_heightmap_coords(
      get_scene_extent(),
      h,
      long = xyz[1:3, 1],
      lat = xyz[1:3, 2],
      altitude = xyz[1:3, 3],
      zscale = scale,
      transform_scene = FALSE
    )
    render_multipolygonz(building, lit = FALSE)
    ids = rgl::ids3d(tags = TRUE)
    id = ids$id[startsWith(ids$tag, "obj")]
    actual = unique(rgl::rgl.attrib(id, "vertices"))
    for (j in 1:3) {
      expect_equal(sort(actual[, j]), sort(expected[, j]), tolerance = 1e-5)
    }
    if (!is.null(original_xy)) {
      expect_equal(actual[, c(1, 3)], original_xy)
    }
    original_xy = actual[, c(1, 3)]
    obj_file = tempfile(fileext = ".obj")
    withr::defer(unlink(obj_file))
    save_multipolygonz_to_obj(building, obj_file)
    before_obj = rgl::ids3d()$id
    render_obj(obj_file, clear_previous = TRUE, lit = FALSE)
    ids = rgl::ids3d(tags = TRUE)
    actual_obj = unique(rgl::rgl.attrib(
      setdiff(ids$id, before_obj),
      "vertices"
    ))
    for (i in seq_len(nrow(expected))) {
      expect_lt(min(sqrt(rowSums(sweep(actual_obj, 2, expected[i, ])^2))), 1e-5)
      expect_lt(min(sqrt(rowSums(sweep(actual, 2, expected[i, ])^2))), 1e-5)
    }
    rgl::close3d()
  }
})

test_that("raster water retains caps above a different terrain triangulation", {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  rgl::open3d()
  cache_scene_context_token()
  cache_scene_triangulate(TRUE)
  # The fixed-grid diagonal joins the high corners; the opposite diagonal
  # joins the low corners. Clipping to the former leaves a hole over the latter.
  h = matrix(c(0, 2, 2, 0), 2, 2)
  w = matrix(1, 2, 2)
  mesh = make_spatial_water_surface(
    w,
    h,
    is.finite(w),
    water_edge_extension = 0
  )
  v = do.call(rbind, mesh$vertices)
  a = v[seq(1, nrow(v), 3), ]
  b = v[seq(2, nrow(v), 3), ]
  c = v[seq(3, nrow(v), 3), ]
  top = a[, 2] == 1 & b[, 2] == 1 & c[, 2] == 1
  area = ((b[, 3] - a[, 3]) *
    (c[, 1] - a[, 1]) -
    (b[, 1] - a[, 1]) * (c[, 3] - a[, 3])) /
    2
  expect_true(all(area[top] > 0))
  expect_equal(sum(area[top]), 1)
  expect_equal(range(v[, c(1, 3)]), c(-.5, .5))
})

test_that("triangulated raster caps still exclude missing terrain", {
  h = matrix(0, 5, 5)
  h[3, 3] = NA_real_
  w = matrix(1, 5, 5)
  w[3, 3] = NA_real_
  v = make_spatial_water_cell_surface(
    w,
    h,
    water_edge_extension = 0,
    clip_to_terrain = FALSE
  )
  a = v[seq(1, nrow(v), 3), ]
  b = v[seq(2, nrow(v), 3), ]
  c = v[seq(3, nrow(v), 3), ]
  top = a[, 2] == 1 & b[, 2] == 1 & c[, 2] == 1
  centers = (a + b + c) / 3
  expect_false(any(abs(centers[top, 1]) < 1 & abs(centers[top, 3]) < 1))
})

test_that("ggplot water cells share the scene's spatial coordinate mapping", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  r = terra::rast(
    nrows = 12,
    ncols = 16,
    xmin = 320000,
    xmax = 324000,
    ymin = 4304000,
    ymax = 4307000,
    crs = "EPSG:26918",
    vals = 0
  )
  terra::values(r) = seq_len(terra::ncell(r)) / 20
  names(r) = "height"
  p = ggplot2::ggplot() +
    tidyterra::geom_spatraster(
      data = r,
      ggplot2::aes(fill = height),
      maxcell = Inf
    ) +
    ggplot2::coord_sf() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  h = plot_gg_test(
    p,
    width = 1,
    height = 1,
    raytrace = FALSE,
    shadow = FALSE,
    solid = FALSE,
    save_height_matrix = TRUE,
    triangulate = TRUE,
    vertical_exaggeration = 1
  )
  expect_true(get_scene_triangulate())
  e = get_ggplot_extent()
  template = build_waterheight_template(nrow(h), ncol(h), e, sf::st_crs(26918))
  # Endpoints and an asymmetric interior pixel must agree with vector layers.
  rc = rbind(c(1, 1), c(ncol(h), nrow(h)), c(30, 80))
  xy = terra::xyFromCell(
    template,
    terra::cellFromRowCol(template, rc[, 1], rc[, 2])
  )
  actual = transform_into_heightmap_coords(
    e,
    h,
    long = xy[, 1],
    lat = xy[, 2],
    altitude = 0,
    transform_scene = FALSE
  )
  expect_equal(
    unname(actual[, 1]),
    rc[, 2] - (nrow(h) + 1) / 2,
    tolerance = 1e-8
  )
  expect_equal(
    unname(actual[, 3]),
    rc[, 1] - (ncol(h) + 1) / 2,
    tolerance = 1e-8
  )
  # Polygon rasterization must use exactly the same template as raster input.
  water = sf::st_as_sfc(
    'POLYGON ((321000 4305000, 322000 4305000, 322000 4306000, 321000 4306000, 321000 4305000))',
    crs = 26918
  )
  wr = water_sf_to_raster(
    water,
    h,
    e,
    sf::st_crs(26918),
    water_elevation = 20.0001
  )
  expect_true(terra::compareGeom(wr, template))
  aligned = resolve_spatial_waterheight_matrix(
    wr,
    nrow(h),
    ncol(h),
    e,
    sf::st_crs(26918)
  )
  expect_identical(
    as.numeric(aligned),
    as.numeric(raster_to_matrix(wr, verbose = FALSE))
  )
  # Test buildings in the same ggplot, including a different source CRS.
  vertices = rbind(
    c(321000, 4305000, 20),
    c(322000, 4305000, 20),
    c(322000, 4306000, 20),
    c(321000, 4305000, 20)
  )
  building = sf::st_sfc(sf::st_multipolygon(list(list(vertices))), crs = 26918)
  render_multipolygonz(sf::st_transform(building, 4326), lit = FALSE)
  ids = rgl::ids3d(tags = TRUE)
  actual = unique(rgl::rgl.attrib(
    ids$id[startsWith(ids$tag, "obj")],
    "vertices"
  ))
  expected = transform_into_heightmap_coords(
    e,
    h,
    long = vertices[1:3, 1],
    lat = vertices[1:3, 2],
    altitude = 20,
    zscale = get_scene_zscale(),
    crs = 26918
  )
  for (j in 1:3) {
    expect_equal(sort(actual[, j]), sort(expected[, j]), tolerance = 1e-5)
  }
})

test_that("inferred water clearance survives elevation-to-scene scaling", {
  water = sf::st_as_sfc("POLYGON ((1 1, 9 1, 9 9, 1 9, 1 1))", crs = 3857)
  h = matrix(100, 30, 40)
  extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  wr = water_sf_to_raster(water, h, extent, 3857, zscale = 1000)
  level = unique(na.omit(as.numeric(terra::values(wr))))
  clearance = (level - 100) / 1000
  expect_gte(clearance, 16 * 2^-23 * (max(dim(h)) - 1) / 2 - 1e-12)
  explicit = water_sf_to_raster(
    water,
    h,
    extent,
    3857,
    water_elevation = 100,
    zscale = 1000
  )
  expect_equal(unique(na.omit(as.numeric(terra::values(explicit)))), 100)
})

test_that("both public water entry points preserve caps on simplified terrain", {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  r = terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:3857",
    vals = rep(c(0, 2), length.out = 9)
  )
  w = r
  terra::values(w) = 1
  for (triangulate in c(TRUE, FALSE, TRUE)) {
    plot_3d_test(
      height_shade(r),
      r,
      zscale = 1,
      triangulate = triangulate,
      max_error = 0,
      shadow = FALSE,
      solid = FALSE,
      water = TRUE,
      water_input = w,
      water_edge_extension = 0,
      geographic_aspect = FALSE
    )
    expect_identical(get_scene_triangulate(), triangulate)
    for (entry in c("plot_3d", "render_water")) {
      if (entry == "render_water") {
        render_water(w, water_edge_extension = 0)
      }
      id = get_ids_with_labels(typeval = "water")$id
      v = rgl::rgl.attrib(id, "vertices")
      a = v[seq(1, nrow(v), 3), ]
      b = v[seq(2, nrow(v), 3), ]
      c = v[seq(3, nrow(v), 3), ]
      top = a[, 2] == 1 & b[, 2] == 1 & c[, 2] == 1
      area = ((b[, 3] - a[, 3]) *
        (c[, 1] - a[, 1]) -
        (b[, 1] - a[, 1]) * (c[, 3] - a[, 3])) /
        2
      if (triangulate) {
        expect_equal(sum(area[top]), 4, info = entry)
      } else {
        expect_gt(sum(area[top]), 0)
        expect_lt(sum(area[top]), 4)
      }
    }
  }
})


test_that("terrain cache records the regular-grid fallback for missing heights", {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  h = matrix(0, 5, 5)
  h[3, 3] = NA_real_
  plot_3d_test(
    array(1, dim = c(5, 5, 3)),
    h,
    triangulate = TRUE,
    shadow = FALSE,
    solid = FALSE
  )
  expect_false(get_scene_triangulate())
})
