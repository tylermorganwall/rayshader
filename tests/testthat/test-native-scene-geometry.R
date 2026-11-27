test_that("native contact batches preserve scalar geometry and bridge relations", {
  geometry = lapply(0:63, function(i) {
    sf::st_linestring(rbind(c(i, 0), c(i, 10)))
  })
  geometry = c(
    geometry,
    list(
      sf::st_linestring(rbind(c(-1, 5), c(64, 5))),
      sf::st_linestring(rbind(c(0, 2), c(0, 8)))
    )
  )
  roads = sf::st_sf(
    layer = NA_real_,
    bridge = c(rep(NA, 64), "yes", NA),
    geometry = sf::st_sfc(geometry, crs = 3857)
  )
  prepared = prepare_render_road_layer_features(roads, "layer")
  pairs = rbind(cbind(seq_len(64L), 65L), c(1L, 66L), c(65L, 66L))
  expected = lapply(seq_len(nrow(pairs)), function(i) {
    intersect_render_road_fragment_pair(
      prepared,
      pairs[i, 1],
      pairs[i, 2],
      i,
      .01
    )
  })
  points = bind_render_road_topology_rows(unlist(
    lapply(expected, `[[`, "points"),
    recursive = FALSE
  ))
  overlaps = unlist(lapply(expected, `[[`, "overlaps"), recursive = FALSE)
  one = build_render_road_contact_batch(prepared, pairs, .01)
  expect_equal(one$points, points, tolerance = 1e-12)
  expect_equal(one$overlaps, overlaps, tolerance = 1e-12)
  expect_length(one$failures, 0)
})

test_that("native continuation scores retain metadata and directional decisions", {
  set.seed(20261101)
  n = 256L
  endpoints = sf::st_sf(
    render_road_endpoint_id = seq_len(n),
    render_road_fragment_id = seq_len(n),
    render_road_feature_id = rep(seq_len(n / 2), each = 2),
    endpoint_side = rep(c("start", "end"), n / 2),
    direction_x = cos(seq_len(n)),
    direction_y = sin(seq_len(n)),
    render_road_way_id = sample(c(NA, "Way", " way ", "other"), n, TRUE),
    render_road_ref = sample(c(NA, " REF ", "ref"), n, TRUE),
    render_road_name = sample(c(NA, "Street", "road"), n, TRUE),
    render_road_highway = sample(c(NA, "footway", " FootWay "), n, TRUE),
    render_road_lanes = sample(c(NA_integer_, 1:4), n, TRUE),
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(i) sf::st_point(c(i %/% 2, 0))),
      crs = 3857
    )
  )
  pairs = cbind(seq_len(n - 1L), 2:n)
  xy = sf::st_coordinates(endpoints)[, 1:2, drop = FALSE]
  metadata = vapply(
    c(
      "render_road_way_id",
      "render_road_ref",
      "render_road_name",
      "render_road_highway"
    ),
    function(column) {
      values = tolower(trimws(as.character(endpoints[[column]])))
      values[is.na(values) | !nzchar(values)] = NA_character_
      as.integer(factor(values))
    },
    integer(n)
  )
  expected = bind_render_road_topology_rows(lapply(
    seq_len(nrow(pairs)),
    function(i) {
      score_render_road_continuation_pair(
        endpoints,
        xy,
        pairs[i, 1],
        pairs[i, 2],
        i,
        .01,
        cos(pi / 6)
      )
    }
  ))
  output = score_road_continuations_cpp(
    cbind(xy, endpoints$direction_x, endpoints$direction_y),
    metadata,
    endpoints$render_road_feature_id,
    endpoints$render_road_lanes,
    pairs,
    .01,
    cos(pi / 6)
  )
  for (name in names(output)) {
    expect_equal(
      output[[name]],
      expected[[name]],
      tolerance = 1e-12,
      info = name
    )
  }
})

test_that("batched building faces retain holes, winding, and source vertices", {
  exterior = cbind(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)), 3)
  hole = cbind(rbind(c(1, 1), c(1, 2), c(2, 2), c(2, 1), c(1, 1)), 3)
  polygons = lapply(seq_len(128), function(i) {
    rings = lapply(list(exterior, hole), function(x) {
      x[, 1] = x[, 1] + i * 10 + 400000
      x[, 2] = x[, 2] + 4300000
      if (i %% 2) {
        x = x[nrow(x):1, , drop = FALSE]
      }
      if (i %% 3) {
        x = x[, c(1, 3, 2)]
      }
      x
    })
    sf::st_multipolygon(list(rings))
  })
  object = sf::st_sf(geometry = sf::st_sfc(polygons))
  expected = lapply(polygons, function(polygon) {
    rings = lapply(polygon[[1]], function(ring) ring[-1, , drop = FALSE])
    result = multipolygonz_triangulate_face(rings)
    colnames(result$vertices) = c("X", "Y", "Z")
    result
  })
  single = multipolygonz_mesh_faces(object)
  expect_equal(single, expected, tolerance = 1e-12)
  # A failed face between valid faces must retain its diagnostic index and must
  # not shift subsequent output slots when the native batch falls back to R.
  invalid = sf::st_multipolygon(list(list(cbind(c(0, 1, 2, 0), 0, 0))))
  object = sf::st_sf(
    geometry = sf::st_sfc(c(polygons[1], list(invalid), polygons[-1]))
  )
  expect_warning(
    result <- multipolygonz_mesh_faces(object),
    "Face 2: zero-area face"
  )
  expect_identical(result, single)
})
