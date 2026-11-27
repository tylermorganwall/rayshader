test_that("topology row binding preserves sf schemas and geometry", {
  rows = lapply(1:4, function(i) {
    sf::st_sf(
      id = as.integer(i),
      distance = i / 3,
      valid = i != 2,
      label = if (i == 3) NA_character_ else "road",
      shape = sf::st_sfc(sf::st_point(c(i, i^2)), crs = 26918)
    )
  })
  expect_identical(bind_render_road_topology_rows(rows), do.call(rbind, rows))
  expect_identical(bind_render_road_topology_rows(rows[1]), rows[[1]])
  line_rows = lapply(1:4, function(i) {
    sf::st_sf(
      id = as.integer(i),
      geometry = sf::st_sfc(
        sf::st_linestring(rbind(c(i, 0), c(i, 5))),
        crs = 26918
      )
    )
  })
  expect_identical(
    bind_render_road_topology_rows(line_rows),
    do.call(rbind, line_rows)
  )
})

test_that("planar intersection cache preserves metric events and input CRS", {
  roads = sf::st_sf(
    layer = c(0, 1, 0, 0),
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(0, 0), c(10, 0))),
      sf::st_linestring(rbind(c(5, -5), c(5, 5))),
      sf::st_linestring(rbind(c(2, 0), c(7, 0))),
      sf::st_linestring(rbind(c(10, 0), c(12, 3))),
      crs = 3857
    )
  )
  for (geographic in c(FALSE, TRUE)) {
    input = if (geographic) sf::st_transform(roads, 4326) else roads
    prepared = prepare_render_road_layer_features(input, "layer")
    original = prepared
    planar = prepared
    planar$intersection_geometry = sf::st_geometry(prepared$fragments)
    sf::st_crs(planar$intersection_geometry) = NA
    for (pair in list(c(1L, 2L), c(1L, 3L), c(1L, 4L))) {
      direct = intersect_render_road_fragment_pair(
        prepared,
        pair[1],
        pair[2],
        1L,
        .01
      )
      cached = intersect_render_road_fragment_pair(
        planar,
        pair[1],
        pair[2],
        1L,
        .01
      )
      expect_identical(cached, direct)
    }
    find_render_road_layer_events(prepared)
    expect_identical(prepared, original)
    expect_identical(sf::st_crs(input), sf::st_crs(original$source_fragments))
  }
})
