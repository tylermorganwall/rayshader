test_that("spatial height plots preserve original sub-PNG elevation precision", {
  skip_if_not_installed("tidyterra")
  r = terra::rast(
    nrows = 40,
    ncols = 40,
    xmin = 0,
    xmax = 400,
    ymin = 0,
    ymax = 400,
    crs = "EPSG:3857"
  )
  terra::values(r) = seq(-719, 830, length.out = terra::ncell(r))
  names(r) = "height"
  p = ggplot2::ggplot() +
    tidyterra::geom_spatraster(
      data = r,
      ggplot2::aes(fill = height),
      maxcell = Inf,
      interpolate = FALSE
    ) +
    ggplot2::coord_sf(expand = FALSE)
  built = ggplot2::ggplot_build(p)
  info = list(
    height_use_data_scale = TRUE,
    height_is_mapped = TRUE,
    coord = built$layout$coord,
    panel_params = built$layout$panel_params,
    height_scale = built$plot$scales$get_scales("fill")
  )
  panel = data.frame(
    data_xmin = 0,
    data_xmax = 400,
    data_ymin = 0,
    data_ymax = 400,
    extent_xmin = 5,
    extent_xmax = 395,
    extent_ymin = 5,
    extent_ymax = 395
  )
  image_heights = matrix(-719, 40, 40)
  heights = restore_plot_gg_raster_precision(image_heights, p, panel, info)
  expect_equal(heights, t(as.matrix(r, wide = TRUE)))
  expect_length(unique(as.numeric(heights)), 1600L)
  expect_equal(heights[1, 1], -719)
  expect_equal(heights[40, 40], 830)
  # Facets and visible overlays retain their original image-based semantics.
  expect_identical(
    restore_plot_gg_raster_precision(
      image_heights,
      p,
      rbind(panel, panel),
      info
    ),
    image_heights
  )
  mixed = p +
    ggplot2::geom_point(data = data.frame(x = 200, y = 200), ggplot2::aes(x, y))
  expect_identical(
    restore_plot_gg_raster_precision(
      image_heights,
      mixed,
      panel,
      info
    ),
    image_heights
  )
})

test_that("restored terrain and spatial overlays share absolute elevations", {
  skip_if_not_installed("tidyterra")
  r = terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100,
    crs = "EPSG:3857"
  )
  terra::values(r) = seq(100.001, 100.999, length.out = 100)
  names(r) = "height"
  p = ggplot2::ggplot() +
    tidyterra::geom_spatraster(
      data = r,
      ggplot2::aes(fill = height),
      maxcell = Inf
    ) +
    ggplot2::coord_sf(expand = FALSE)
  # tidyterra adds an empty CRS-only sf layer, which can warn while drawing.
  suppressWarnings(plot_gg(
    p,
    p,
    width = 3,
    height = 3,
    vertical_exaggeration = 1,
    triangulate = FALSE,
    shadow = FALSE,
    solid = FALSE
  ))
  on.exit(rgl::close3d())
  h = get_scene_heightmap()
  zscale = get_scene_effective_zscale()
  surface = get_ids_with_labels(c("surface", "surface_tris"))$id
  y = rgl::rgl.attrib(surface, "vertices")[, 2]
  expect_equal(sort(y), sort(as.numeric(h / zscale)), tolerance = 1e-6)

  elevation = 110.12345
  render_points(lat = 50, long = 50, altitude = elevation, crs = 3857)
  point = get_ids_with_labels("points3d")$id
  expect_equal(
    unname(rgl::rgl.attrib(point, "vertices")[1, 2]) * zscale,
    elevation,
    tolerance = 1e-6
  )
  ring = rbind(c(40, 40), c(60, 40), c(60, 60), c(40, 60), c(40, 40))
  footprint = sf::st_sfc(sf::st_polygon(list(ring)), crs = 3857)
  render_water(footprint, water_elevation = elevation)
  water = get_ids_with_labels("water")$id
  expect_equal(
    max(rgl::rgl.attrib(water, "vertices")[, 2]) * zscale,
    elevation,
    tolerance = 1e-6
  )
  building = sf::st_sfc(
    sf::st_multipolygon(list(list(cbind(ring, elevation)))),
    crs = 3857
  )
  render_multipolygonz(building, rgl_tag = "-precision-check")
  building_id = get_ids_with_labels("obj")$id
  expect_equal(
    min(rgl::rgl.attrib(building_id, "vertices")[, 2]) * zscale,
    elevation,
    tolerance = 1e-6
  )
})
