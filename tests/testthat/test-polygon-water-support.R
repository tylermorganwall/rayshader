test_that('polygon support excludes edge contacts and retains separated terrain faces', {
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  rgl::open3d()
  cache_scene_context_token()
  # Two covered faces at different elevations, plus a third face that touches
  # the footprint only along an edge. The empty gap represents missing terrain.
  rgl::triangles3d(
    rbind(
      c(0, 0, 0),
      c(1, 0, 0),
      c(0, 0, 1),
      c(2, 10, 0),
      c(3, 10, 0),
      c(2, 10, 1),
      c(1, 20, 1),
      c(2, 20, 1),
      c(1, 20, 2)
    ),
    tag = 'surface_tris-dim_4_4'
  )
  terrain = index_polygon_water_terrain(matrix(0, 4, 4), 1)
  polygon = sf::st_as_sfc('POLYGON ((-.1 -.1,3.1 -.1,3.1 1,-.1 1,-.1 -.1))')
  support = polygon_water_terrain_support(polygon, terrain)
  expect_equal(sum(as.numeric(sf::st_area(support$geometry))), 1)
  expect_equal(sort(unique(support$heights)), c(0, 10))
  expect_equal(sort(unname(support$min_height)), c(0, 10))
  expect_equal(sort(unname(support$max_height)), c(0, 10))
  # Bounding boxes overlap, but the polygon lies outside the actual triangle.
  outside = sf::st_as_sfc('POLYGON ((.6 .6,.8 .6,.8 .8,.6 .8,.6 .6))')
  expect_null(polygon_water_terrain_support(outside, terrain))
})
