test_that('polygon water inserts one object with unchanged disconnected geometry', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  terrain = terra::rast(
    nrows = 8,
    ncols = 12,
    xmin = 0,
    xmax = 12,
    ymin = 0,
    ymax = 8,
    crs = 'EPSG:3857',
    vals = 0
  )
  plot_3d_test(
    height_shade(terrain),
    terrain,
    zscale = 1,
    solid = FALSE,
    shadow = FALSE,
    geographic_aspect = FALSE
  )
  water = sf::st_as_sfc(
    c(
      'POLYGON ((2 2,5 2,5 5,2 5,2 2),(3 3,3 4,4 4,4 3,3 3))',
      'POLYGON ((7 2,9 2,9 4,7 4,7 2))'
    ),
    crs = 3857
  )
  levels = c(1, 2)
  colors = c('blue', 'cyan', 'navy', 'green', 'white')
  alpha = c(.2, .4, .6, .8, 1)
  expected_vertices = expected_colors = expected_lines = list()
  for (i in seq_along(water)) {
    render_water(
      water[i],
      water_elevation = levels[i],
      water_color = colors,
      water_alpha = alpha,
      water_line_color = 'white'
    )
    ids = rgl::ids3d(tags = TRUE)
    id = ids$id[ids$tag == 'water']
    expect_length(id, 1L)
    v = rgl::rgl.attrib(id, 'vertices')
    material = rgl::rgl.attrib(id, 'colors')
    expected_vertices[[i]] = v
    expected_colors[[i]] = material[
      rep(seq_len(nrow(material)), length.out = nrow(v)),
      ,
      drop = FALSE
    ]
    expected_lines[[i]] = rgl::rgl.attrib(
      ids$id[ids$tag == 'waterlines'],
      'vertices'
    )
  }
  # Count actual insertion calls as well as final scene objects. The renderer
  # must preserve disjoint geometry and material recycling while uploading once.
  insertions = 0L
  triangles = rgl::triangles3d
  local_mocked_bindings(.package = 'rgl', triangles3d = function(...) {
    insertions <<- insertions + 1L
    expect_true(rgl::par3d('skipRedraw'))
    triangles(...)
  })
  render_water(
    water,
    water_elevation = levels,
    water_color = colors,
    water_alpha = alpha,
    water_line_color = 'white'
  )
  expect_identical(insertions, 1L)
  ids = rgl::ids3d(tags = TRUE)
  id = ids$id[ids$tag == 'water']
  expect_length(id, 1L)
  expect_equal(
    rgl::rgl.attrib(id, 'vertices'),
    do.call(rbind, expected_vertices)
  )
  expect_equal(rgl::rgl.attrib(id, 'colors'), do.call(rbind, expected_colors))
  expect_length(ids$id[ids$tag == 'waterlines'], 1L)
  expect_equal(
    rgl::rgl.attrib(ids$id[ids$tag == 'waterlines'], 'vertices'),
    do.call(rbind, expected_lines)
  )
  expect_false(rgl::par3d('skipRedraw'))
  render_water(clear_previous = TRUE)
  expect_false(any(rgl::ids3d(tags = TRUE)$tag %in% c('water', 'waterlines')))
})
