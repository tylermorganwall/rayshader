test_that("geographic aspect resolves metric cell dimensions", {
  skip_if_not_installed("sf")

  heightmap = matrix(0, nrow = 9, ncol = 7)
  extent = c(xmin = -75, xmax = -74.2, ymin = 60, ymax = 60.6)
  aspect = calculate_geographic_aspect(
    heightmap,
    extent = extent,
    crs = 4326
  )

  expect_true(aspect$enabled)
  expect_lt(aspect$scale[["x"]], 1)
  expect_gt(aspect$scale[["z"]], 1)
  expect_equal(mean(aspect$scale), 1)
  expect_equal(aspect$center_latitude, 60.3, tolerance = 1e-6)

  identity = calculate_geographic_aspect(
    heightmap,
    extent = extent,
    crs = 4326,
    geographic_aspect = FALSE
  )
  expect_false(identity$enabled)
  expect_equal(identity$scale, c(x = 1, z = 1))
  expect_equal(identity$cell_meters, aspect$cell_meters)

  projected = calculate_geographic_aspect(
    heightmap,
    extent = c(xmin = 0, xmax = 800, ymin = 0, ymax = 1200),
    crs = 3857
  )
  expect_lt(projected$scale[["x"]], projected$scale[["z"]])
  expect_equal(mean(projected$scale), 1)

  equator = calculate_geographic_aspect(
    heightmap,
    extent = c(xmin = -75, xmax = -74.2, ymin = -0.3, ymax = 0.3),
    crs = 4326
  )
  expect_lt(abs(equator$scale[["x"]] - 1), abs(aspect$scale[["x"]] - 1))

  missing_metadata = calculate_geographic_aspect(
    heightmap,
    extent = NULL,
    crs = 4326
  )
  expect_equal(missing_metadata, identity_geographic_aspect())
})

test_that("geometry-aware 2D shades preserve dimensions and use aspect", {
  skip_if_not_installed("sf")

  x = seq(-1, 1, length.out = 12)
  z = seq(-1, 1, length.out = 10)
  heightmap = outer(x, z, function(x_value, z_value) {
    exp(-(x_value^2 + 2 * z_value^2) * 3) * 100
  })
  extent = c(xmin = -75, xmax = -73.9, ymin = 60, ymax = 60.9)

  normals = calculate_normal(
    heightmap,
    zscale = 1,
    extent = extent,
    crs = 4326
  )
  identity_normals = calculate_normal(
    heightmap,
    zscale = 1,
    geographic_aspect = FALSE,
    extent = extent,
    crs = 4326
  )
  expect_true(attr(normals, "geographic_aspect")$enabled)
  expect_false(attr(identity_normals, "geographic_aspect")$enabled)
  expect_false(isTRUE(all.equal(normals$x, identity_normals$x)))
  legacy_normals = calculate_normal(
    heightmap,
    geographic_aspect = FALSE
  )
  for (component in c("x", "y", "z")) {
    expect_equal(identity_normals[[component]], legacy_normals[[component]])
  }

  sphere = sphere_shade(
    heightmap,
    zscale = 1000,
    extent = extent,
    crs = 4326,
    progbar = FALSE
  )
  sphere_identity = sphere_shade(
    heightmap,
    zscale = 1000,
    geographic_aspect = FALSE,
    extent = extent,
    crs = 4326,
    progbar = FALSE
  )
  expect_equal(dim(sphere), c(ncol(heightmap), nrow(heightmap), 4))
  expect_false(isTRUE(all.equal(sphere, sphere_identity)))

  lambert = lamb_shade(
    heightmap,
    zscale = 1000,
    extent = extent,
    crs = 4326
  )
  lambert_identity = lamb_shade(
    heightmap,
    zscale = 1000,
    geographic_aspect = FALSE,
    extent = extent,
    crs = 4326
  )
  expect_equal(dim(lambert), c(ncol(heightmap), nrow(heightmap)))
  expect_false(isTRUE(all.equal(lambert, lambert_identity)))

  shadows = ray_shade(
    heightmap,
    zscale = 1000,
    maxsearch = 8,
    anglebreaks = 25,
    extent = extent,
    crs = 4326,
    progbar = FALSE
  )
  shadow_identity = ray_shade(
    heightmap,
    zscale = 1000,
    maxsearch = 8,
    anglebreaks = 25,
    geographic_aspect = FALSE,
    extent = extent,
    crs = 4326,
    progbar = FALSE
  )
  expect_equal(dim(shadows), c(ncol(heightmap), nrow(heightmap)))
  expect_false(isTRUE(all.equal(shadows, shadow_identity)))

  texture_explicit = texture_shade(
    heightmap,
    dx = 1,
    dy = 1,
    geographic_aspect = TRUE,
    extent = extent,
    crs = 4326
  )
  texture_identity = texture_shade(
    heightmap,
    dx = 1,
    dy = 1,
    geographic_aspect = FALSE,
    extent = extent,
    crs = 4326
  )
  expect_equal(texture_explicit, texture_identity)

  ambient = ambient_shade(
    heightmap,
    anglebreaks = 45,
    sunbreaks = 3,
    maxsearch = 6,
    zscale = 1,
    extent = extent,
    crs = 4326,
    progbar = FALSE
  )
  ambient_identity = ambient_shade(
    heightmap,
    anglebreaks = 45,
    sunbreaks = 3,
    maxsearch = 6,
    zscale = 1,
    geographic_aspect = FALSE,
    extent = extent,
    crs = 4326,
    progbar = FALSE
  )
  expect_equal(dim(ambient), c(ncol(heightmap), nrow(heightmap)))
  expect_false(isTRUE(all.equal(ambient, ambient_identity)))

  corrected_once = correct_normal_geographic_aspect(
    identity_normals,
    attr(normals, "geographic_aspect")
  )
  corrected_twice = correct_normal_geographic_aspect(
    corrected_once,
    attr(normals, "geographic_aspect")
  )
  expect_equal(corrected_twice, corrected_once)
})

test_that("raster shading chains reuse exact cached cell-bound aspect", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  dem = terra::rast(
    nrows = 5,
    ncols = 7,
    xmin = -75,
    xmax = -74.3,
    ymin = 60,
    ymax = 60.5,
    crs = "EPSG:4326"
  )
  terra::values(dem) = seq_len(terra::ncell(dem))
  clear_hillshade_cache()
  constant_shade(dem)
  cached_aspect = get_hillshade_geographic_aspect()

  chained = lamb_shade(zscale = 1000)
  direct = lamb_shade(dem, zscale = 1000)

  expect_true(cached_aspect$enabled)
  expect_equal(chained, direct, tolerance = 1e-12)
})

test_that("plot_3d caches and applies geographic aspect", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  heightmap = outer(seq_len(8), seq_len(6), `+`)
  extent = c(xmin = -75, xmax = -74.3, ymin = 60, ymax = 60.5)
  texture = constant_shade(heightmap, color = "white")

  plot_3d_test(
    texture,
    heightmap,
    zscale = 1,
    geographic_aspect = TRUE,
    extent = extent,
    crs = 4326,
    solid = TRUE,
    shadow = TRUE,
    windowsize = c(100, 100)
  )

  aspect = get_scene_geographic_aspect()
  expect_true(aspect$enabled)
  surface_id = get_ids_with_labels(typeval = "surface_tris")$id[[1]]
  vertices = rgl::rgl.attrib(surface_id, "vertices")
  expect_equal(
    diff(range(vertices[, 1])),
    (nrow(heightmap) - 1) * aspect$scale[["x"]],
    tolerance = 1e-6
  )
  expect_equal(
    diff(range(vertices[, 3])),
    (ncol(heightmap) - 1) * aspect$scale[["z"]],
    tolerance = 1e-6
  )

  mapped = transform_into_heightmap_coords(
    extent = extent,
    heightmap = heightmap,
    long = extent[["xmax"]],
    lat = extent[["ymin"]],
    altitude = 0,
    zscale = 1,
    transform_scene = FALSE
  )
  row_col = render_heightmap_row_col(
    heightmap,
    x = mapped[1, 1],
    z = mapped[1, 3]
  )
  expect_equal(row_col$row, nrow(heightmap))
  expect_equal(row_col$col, ncol(heightmap))

  polygon = sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(
          -74.9,
          60.1,
          -74.8,
          60.1,
          -74.8,
          60.2,
          -74.9,
          60.2,
          -74.9,
          60.1
        ),
        ncol = 2,
        byrow = TRUE
      ))),
      crs = 4326
    )
  )
  original_polygon = polygon
  render_polygons(polygon, top = 1, bottom = 0, lit = FALSE)
  polygon_id = get_ids_with_labels(typeval = "polygon3d")$id[[1]]
  polygon_vertices = rgl::rgl.attrib(polygon_id, "vertices")
  expected_x_span = 0.1 /
    unname(diff(extent[c("xmin", "xmax")])) *
    nrow(heightmap) *
    aspect$scale[["x"]]
  expected_z_span = 0.1 /
    unname(diff(extent[c("ymin", "ymax")])) *
    ncol(heightmap) *
    aspect$scale[["z"]]
  expect_equal(
    diff(range(polygon_vertices[, 1])),
    expected_x_span,
    tolerance = 1e-6
  )
  expect_equal(
    diff(range(polygon_vertices[, 3])),
    expected_z_span,
    tolerance = 1e-6
  )
  expect_equal(polygon, original_polygon)

  rgl::close3d()
  plot_3d_test(
    texture,
    heightmap,
    zscale = 1,
    geographic_aspect = FALSE,
    extent = extent,
    crs = 4326,
    solid = FALSE,
    shadow = FALSE,
    windowsize = c(100, 100)
  )
  expect_false(get_scene_geographic_aspect()$enabled)
  identity_surface_id = get_ids_with_labels(typeval = "surface_tris")$id[[1]]
  identity_vertices = rgl::rgl.attrib(identity_surface_id, "vertices")
  expect_equal(diff(range(identity_vertices[, 1])), nrow(heightmap) - 1)
  expect_equal(diff(range(identity_vertices[, 3])), ncol(heightmap) - 1)
})

test_that("building polygon conversion uses corrected scene scaling", {
  skip_if_not_installed("sf")

  polygon = sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0),
      ncol = 2,
      byrow = TRUE
    ))))
  )
  aspect = list(
    enabled = TRUE,
    scale = c(x = 0.5, z = 1.5),
    cell_meters = c(x = 1, z = 3),
    mean_cell_meters = 2,
    center_latitude = 60
  )

  converted = transform_polygon_custom_crs(
    polygon,
    orig_extent = c(0, 10, 0, 10),
    new_extent = c(5, -5, 5, -5),
    geographic_aspect = aspect
  )

  expect_equal(
    as.numeric(sf::st_bbox(converted)),
    c(-2.5, -7.5, 2.5, 7.5)
  )
})

test_that("explicit CRS overrides copied spatial input", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  dem = terra::rast(
    nrows = 4,
    ncols = 5,
    xmin = -75,
    xmax = -74,
    ymin = 40,
    ymax = 41,
    crs = "EPSG:3857"
  )
  terra::values(dem) = seq_len(terra::ncell(dem))
  original_dem_crs = terra::crs(dem)
  dem_info = coerce_plot_3d_heightmap(dem, crs = 4326)
  expect_equal(dem_info$crs, sf::st_crs(4326))
  expect_identical(terra::crs(dem), original_dem_crs)

  point = sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(-75, 40)), crs = 3857)
  )
  original_point = point
  transformed = transform_scene_sf_to_target_crs(
    point,
    target_crs = 32618,
    crs = 4326,
    caller = "test"
  )
  expect_equal(sf::st_crs(transformed$object), sf::st_crs(32618))
  expect_equal(point, original_point)
})

test_that("spatial render heightmaps align to the active scene grid", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  local_rgl_use_null()

  reset_scene_context()
  rgl::open3d()
  on.exit(rgl::close3d(), add = TRUE)
  cache_scene_context_token()
  scene_heightmap = matrix(0, nrow = 6, ncol = 4)
  scene_extent = c(
    xmin = -8350000,
    xmax = -8340000,
    ymin = 4860000,
    ymax = 4870000
  )
  cache_scene_heightmap(scene_heightmap)
  cache_scene_extent(scene_extent)
  cache_scene_crs(3857)
  cache_scene_geographic_aspect(calculate_geographic_aspect(
    scene_heightmap,
    extent = scene_extent,
    crs = 3857
  ))

  source_extent = sf::st_bbox(
    sf::st_transform(
      sf::st_as_sfc(sf::st_bbox(scene_extent, crs = sf::st_crs(3857))),
      4326
    )
  )
  source = terra::rast(
    nrows = 3,
    ncols = 5,
    xmin = source_extent[["xmin"]],
    xmax = source_extent[["xmax"]],
    ymin = source_extent[["ymin"]],
    ymax = source_extent[["ymax"]],
    crs = "EPSG:4326"
  )
  terra::values(source) = seq_len(terra::ncell(source))
  source_copy = terra::deepcopy(source)

  aligned = suppressWarnings(resolve_scene_render_heightmap(
    source,
    caller = "test"
  ))

  expect_equal(dim(aligned), dim(scene_heightmap))
  expect_equal(attr(aligned, "extent"), scene_extent)
  expect_equal(attr(aligned, "crs"), sf::st_crs(3857))
  expect_equal(terra::values(source), terra::values(source_copy))
  expect_identical(terra::crs(source), terra::crs(source_copy))
})

test_that("plot_gg reports and disables geographic aspect", {
  skip_if_not_installed("ggplot2")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  plot = ggplot2::ggplot(
    data.frame(x = 1:3, y = c(1, 3, 2)),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()

  expect_message(
    suppressWarnings(plot_gg_test(
      plot,
      width = 2,
      height = 2,
      raytrace = FALSE,
      shadow = FALSE,
      windowsize = c(100, 100),
      geographic_aspect = TRUE
    )),
    "geographic_aspect.*ignored"
  )
  expect_false(get_scene_geographic_aspect()$enabled)
})

test_that("terrain-following line tangents use corrected scene directions", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  rgl::open3d()
  cache_scene_context_token()
  heightmap = matrix(0, nrow = 11, ncol = 11)
  extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  cache_scene_heightmap(heightmap)
  cache_scene_extent(extent)
  cache_scene_geographic_aspect(list(
    enabled = TRUE,
    scale = c(x = 0.5, z = 1.5),
    cell_meters = c(x = 1, z = 3),
    mean_cell_meters = 2,
    center_latitude = 60
  ))
  line = sf::st_sfc(sf::st_linestring(matrix(
    c(0, 0, 10, 10),
    ncol = 2,
    byrow = TRUE
  )))

  samples = sample_person_line(
    line = line,
    line_spacing = 2,
    extent = extent,
    heightmap = heightmap,
    zscale = 1,
    caller = "test"
  )

  expected_angle = atan2(-1, -3) * 180 / pi
  expect_equal(samples$angle, rep(expected_angle, length(samples$angle)))
})
