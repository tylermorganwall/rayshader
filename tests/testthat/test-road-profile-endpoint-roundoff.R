test_that("terrain profiles tolerate roundoff but reject missing endpoint coverage", {
  skip_if_not_installed("sf")
  topology = list(
    fragments = sf::st_sf(
      render_road_fragment_id = 1L,
      geometry = sf::st_sfc(
        sf::st_linestring(rbind(c(0, 0), c(100, 0))),
        crs = 3857
      )
    )
  )
  profile = data.frame(
    distance = c(2e-6, 50, 100 - 3e-6),
    elevation = c(5, 6, 7)
  )
  normalized = normalize_render_road_terrain_profiles(topology, list(profile))[[
    1
  ]]
  expect_equal(range(normalized$distance), c(0, 100))
  expect_equal(normalized$elevation[c(1, nrow(normalized))], c(5, 7))
  expect_equal(normalized$elevation[normalized$distance == 50], 6)
  profile$distance[3] = 99.99
  expect_error(
    normalize_render_road_terrain_profiles(topology, list(profile)),
    "cover both fragment endpoints"
  )
  profile$distance = c(0.01, 50, 100)
  expect_error(
    normalize_render_road_terrain_profiles(topology, list(profile)),
    "cover both fragment endpoints"
  )
})
