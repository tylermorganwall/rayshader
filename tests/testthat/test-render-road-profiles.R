skip_render_road_profile_test_dependencies = function(solver = FALSE) {
  skip_if_not_installed("sf")
  skip_if_not_installed("igraph")
  skip_if_not_installed("Matrix")
  if (solver) {
    skip_if_not_installed("osqp")
  }
}

make_render_road_profile_test_line = function(coordinates) {
  sf::st_linestring(matrix(coordinates, ncol = 2, byrow = TRUE))
}

make_render_road_profile_test_arc = function(
  radius,
  angle_start,
  angle_end,
  point_count = 25L
) {
  angle = seq(angle_start, angle_end, length.out = point_count) * pi / 180
  sf::st_linestring(cbind(radius * cos(angle), radius * sin(angle)))
}

make_render_road_profile_test_roads = function(
  lines,
  layer,
  way_id,
  clearance = 5.5,
  bridge = NULL,
  tunnel = NULL,
  location = NULL
) {
  road_count = length(lines)
  if (is.null(bridge)) {
    bridge = ifelse(layer > 0, "yes", NA_character_)
  }
  if (is.null(tunnel)) {
    tunnel = rep(NA_character_, road_count)
  }
  if (is.null(location)) {
    location = rep(NA_character_, road_count)
  }
  sf::st_sf(
    layer = layer,
    clearance = rep(clearance, length.out = road_count),
    osm_id = way_id,
    ref = way_id,
    name = way_id,
    highway = rep("primary", road_count),
    bridge = bridge,
    tunnel = tunnel,
    location = location,
    geometry = sf::st_sfc(lines, crs = 3857)
  )
}

build_render_road_profile_test_topology = function(roads) {
  prepared = prepare_render_road_layer_features(
    roads = roads,
    layer_column = "layer",
    layer_height_column = "clearance"
  )
  build_render_road_layer_topology(prepared)
}

build_render_road_profile_test_problem = function(
  topology,
  terrain_elevation = 0,
  terrain_spacing = 1,
  explicit_controls = NULL,
  settings = list()
) {
  fragments = topology$fragments
  terrain_elevation = rep(
    terrain_elevation,
    length.out = max(fragments$render_road_feature_id)
  )
  terrain_profiles = lapply(seq_len(nrow(fragments)), function(row) {
    fragment_length = as.numeric(sf::st_length(fragments[row, ]))
    distance = sort(unique(c(
      seq(0, fragment_length, by = terrain_spacing),
      fragment_length
    )))
    data.frame(
      distance = distance,
      elevation = rep(
        terrain_elevation[[fragments$render_road_feature_id[[row]]]],
        length(distance)
      )
    )
  })
  names(terrain_profiles) = as.character(
    fragments$render_road_fragment_id
  )
  arguments = utils::modifyList(
    list(
      topology = topology,
      terrain_profiles = terrain_profiles,
      explicit_controls = explicit_controls,
      layer_spacing = 5.5,
      maximum_grade = 0.07,
      maximum_grade_rate = 1e-3,
      curvature_weight = 100,
      grade_weight = 1,
      terrain_reference_weight = 1e-3,
      anchor_grade_weight = 10,
      uplift_weight = 1e-5
    ),
    settings
  )
  do.call(build_render_road_profile_problem, arguments)
}

extract_render_road_profile_test_way = function(
  problem,
  solution,
  way_id,
  sample_spacing = 0.25
) {
  fragment_id = problem$topology$fragments$render_road_fragment_id[
    problem$topology$fragments$render_road_way_id == way_id
  ]
  sample_distances = lapply(problem$terrain_profiles, function(profile) {
    maximum = max(profile$distance)
    sort(unique(c(seq(0, maximum, by = sample_spacing), maximum)))
  })
  profiles = evaluate_render_road_profiles(
    problem,
    solution,
    sample_distances
  )$profiles[as.character(fragment_id)]
  profile = do.call(rbind, profiles)
  profile = stats::aggregate(height ~ x, data = profile, FUN = mean)
  profile[order(profile$x), , drop = FALSE]
}

test_that("surface equality closure stops after one terminal partner", {
  skip_render_road_profile_test_dependencies()

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-120, 0, 120, 0)),
      make_render_road_profile_test_line(c(0, -100, 0, 100)),
      make_render_road_profile_test_line(c(-60, 60, 60, 60)),
      make_render_road_profile_test_line(c(-80, -25, -80, 25))
    ),
    layer = c(0, 0, 0, 1),
    way_id = c("surface-a", "surface-b", "surface-c", "bridge")
  )
  topology = build_render_road_profile_test_topology(roads)
  fragment_id = setNames(
    topology$fragments$render_road_fragment_id,
    topology$fragments$render_road_way_id
  )

  expect_true(
    fragment_id[["surface-a"]] %in%
      topology$prospective_solve_expandable_fragment_id
  )
  expect_true(
    fragment_id[["surface-b"]] %in%
      topology$prospective_solve_terminal_ground_fragment_id
  )
  expect_false(
    fragment_id[["surface-c"]] %in%
      topology$prospective_solve_fragment_id
  )
  active_equality = topology$prospective_solve_junction_equality_pairs
  expect_true(any(
    active_equality$fragment_a == fragment_id[["surface-a"]] &
      active_equality$fragment_b == fragment_id[["surface-b"]] |
      active_equality$fragment_a == fragment_id[["surface-b"]] &
        active_equality$fragment_b == fragment_id[["surface-a"]]
  ))
  expect_false(any(
    active_equality$fragment_a == fragment_id[["surface-c"]] |
      active_equality$fragment_b == fragment_id[["surface-c"]]
  ))
})

test_that("metadata-only tunnels seed independently and remain bounded", {
  skip_render_road_profile_test_dependencies(solver = TRUE)

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(0, -100, 0, 100)),
      make_render_road_profile_test_line(c(100, -100, 100, 100))
    ),
    layer = c(NA_real_, -1),
    way_id = c("metadata-tunnel", "untagged-negative"),
    bridge = c(NA_character_, NA_character_),
    tunnel = c("yes", NA_character_)
  )
  topology = build_render_road_profile_test_topology(roads)
  fragment_id = setNames(
    topology$fragments$render_road_fragment_id,
    topology$fragments$render_road_way_id
  )
  tunnel_id = fragment_id[["metadata-tunnel"]]
  negative_id = fragment_id[["untagged-negative"]]
  tunnel_row = match(
    tunnel_id,
    topology$fragments$render_road_fragment_id
  )

  expect_false(topology$fragments$render_road_layer_explicit[[tunnel_row]])
  expect_true(tunnel_id %in% topology$prospective_solve_seed_fragment_id)
  expect_true(tunnel_id %in% topology$prospective_solve_fragment_id)
  expect_false(
    tunnel_id %in%
      topology$prospective_solve_deferred_profile_fragment_id
  )
  expect_false(negative_id %in% topology$prospective_solve_seed_fragment_id)
  expect_false(negative_id %in% topology$prospective_solve_fragment_id)
  expect_true(
    negative_id %in%
      topology$prospective_solve_deferred_profile_fragment_id
  )

  problem = build_render_road_profile_test_problem(
    topology,
    terrain_elevation = 20,
    settings = list(underground_reference_depth = 6)
  )
  solution = solve_render_road_profile_problem(
    problem,
    maximum_iterations = 100000
  )

  expect_equal(problem$spans$reference, "underground_terrain")
  expect_equal(
    solution$controls$height,
    rep(14, nrow(solution$controls)),
    tolerance = 1e-4
  )
  expect_true(solution$engineering_audit$passed)
})

test_that("dense ranks and mixed events compile pair-specific constraints", {
  skip_render_road_profile_test_dependencies()

  dense_roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-120, 0, 120, 0)),
      make_render_road_profile_test_line(c(0, -120, 0, 120)),
      make_render_road_profile_test_line(c(-100, -100, 100, 100))
    ),
    layer = c(0, 1, 1),
    way_id = c("lower", "upper-a", "upper-b")
  )
  dense_topology = build_render_road_profile_test_topology(dense_roads)
  dense_problem = build_render_road_profile_test_problem(dense_topology)
  dense_participants = dense_topology$crossing_participants

  expect_equal(
    dense_participants$local_order[
      order(dense_participants$render_road_layer)
    ],
    c(1, 2, 2)
  )
  expect_equal(nrow(dense_problem$clearances), 2L)
  expect_equal(dense_problem$clearances$lower_rank, c(1, 1))
  expect_equal(dense_problem$clearances$upper_rank, c(2, 2))
  expect_equal(nrow(dense_problem$junction_equalities), 1L)

  mixed_roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-120, 0, 120, 0)),
      make_render_road_profile_test_line(c(0, -120, 0, 120)),
      make_render_road_profile_test_line(c(-80, -80, 0, 0))
    ),
    layer = c(0, 1, 0),
    way_id = c("surface", "bridge", "branch")
  )
  mixed_topology = build_render_road_profile_test_topology(mixed_roads)
  mixed_problem = build_render_road_profile_test_problem(mixed_topology)
  mixed_fragment_id = setNames(
    mixed_topology$fragments$render_road_fragment_id,
    mixed_topology$fragments$render_road_way_id
  )

  expect_equal(nrow(mixed_problem$clearances), 1L)
  expect_equal(nrow(mixed_problem$junction_equalities), 1L)
  expect_equal(
    sort(c(
      mixed_problem$clearances$lower_fragment_id,
      mixed_problem$clearances$upper_fragment_id
    )),
    sort(unname(mixed_fragment_id[c("surface", "bridge")]))
  )
  expect_equal(
    sort(c(
      mixed_problem$junction_equalities$fragment_a,
      mixed_problem$junction_equalities$fragment_b
    )),
    sort(unname(mixed_fragment_id[c("surface", "branch")]))
  )
})

test_that("positive-layer branches share height but only through pairs share grade", {
  skip_render_road_profile_test_dependencies(solver = TRUE)

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-140, 0, 0, 0)),
      make_render_road_profile_test_line(c(0, 0, 140, 0)),
      make_render_road_profile_test_line(c(0, -120, 0, 0)),
      make_render_road_profile_test_line(c(-60, -80, 60, -80))
    ),
    layer = c(1, 1, 1, 0),
    way_id = c("through", "through", "side-branch", "surface")
  )
  topology = build_render_road_profile_test_topology(roads)
  problem = build_render_road_profile_test_problem(topology)
  solution = solve_render_road_profile_problem(
    problem,
    maximum_iterations = 100000
  )
  audit = audit_render_road_profiles(
    solution$problem,
    solution,
    tolerance = 1e-6
  )

  expect_equal(nrow(problem$junction_equalities), 3L)
  expect_equal(nrow(problem$continuation_equalities), 1L)
  expect_true(audit$passed)
  expect_lt(max(abs(audit$junctions$height_residual)), 1e-6)
  expect_lt(
    max(abs(audit$continuations$oriented_grade_residual)),
    1e-6
  )
})

test_that("profiles are invariant to splitting a through road", {
  skip_render_road_profile_test_dependencies(solver = TRUE)

  make_split_roads = function(split_bridge) {
    bridge_lines = if (split_bridge) {
      list(
        make_render_road_profile_test_line(c(-90, 0, -30, 0)),
        make_render_road_profile_test_line(c(-30, 0, 30, 0)),
        make_render_road_profile_test_line(c(30, 0, 90, 0))
      )
    } else {
      list(make_render_road_profile_test_line(c(-90, 0, 90, 0)))
    }
    make_render_road_profile_test_roads(
      lines = c(
        list(
          make_render_road_profile_test_line(c(0, -120, 0, 120)),
          make_render_road_profile_test_line(c(-180, 0, -90, 0))
        ),
        bridge_lines,
        list(make_render_road_profile_test_line(c(90, 0, 180, 0)))
      ),
      layer = c(0, 0, rep(1, length(bridge_lines)), 0),
      way_id = c("lower", rep("through", length(bridge_lines) + 2L))
    )
  }

  unsplit_topology = build_render_road_profile_test_topology(
    make_split_roads(FALSE)
  )
  unsplit_problem = build_render_road_profile_test_problem(
    unsplit_topology,
    explicit_controls = list(
      numeric(0),
      numeric(0),
      c(60, 120),
      numeric(0)
    )
  )
  split_topology = build_render_road_profile_test_topology(
    make_split_roads(TRUE)
  )
  split_problem = build_render_road_profile_test_problem(split_topology)
  unsplit_solution = solve_render_road_profile_problem(
    unsplit_problem,
    maximum_iterations = 100000
  )
  split_solution = solve_render_road_profile_problem(
    split_problem,
    maximum_iterations = 100000
  )
  unsplit_profile = extract_render_road_profile_test_way(
    unsplit_solution$problem,
    unsplit_solution,
    "through"
  )
  split_profile = extract_render_road_profile_test_way(
    split_solution$problem,
    split_solution,
    "through"
  )
  station = seq(-180, 180, by = 0.5)
  unsplit_height = stats::approx(
    unsplit_profile$x,
    unsplit_profile$height,
    station
  )$y
  split_height = stats::approx(
    split_profile$x,
    split_profile$height,
    station
  )$y

  expect_lt(max(abs(unsplit_height - split_height)), 1e-5)
})

test_that("adaptive refinement removes a between-control chord dip", {
  skip_render_road_profile_test_dependencies(solver = TRUE)

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(0, -120, 0, 120)),
      make_render_road_profile_test_line(c(-160, 0, -80, 0)),
      make_render_road_profile_test_line(c(-80, 0, 80, 0)),
      make_render_road_profile_test_line(c(80, 0, 160, 0))
    ),
    layer = c(0, 1, 1, 1),
    way_id = c("lower", "upper", "upper", "upper")
  )
  topology = build_render_road_profile_test_topology(roads)
  problem = build_render_road_profile_test_problem(
    topology,
    settings = list(
      maximum_grade = 0.15,
      maximum_grade_rate = 1.4e-3,
      terrain_reference_weight = 1e-3,
      uplift_weight = 1e-2
    )
  )
  initial_solution = solve_render_road_profile_components_once(
    problem = problem,
    verbose = FALSE,
    absolute_tolerance = 1e-7,
    relative_tolerance = 1e-7,
    maximum_iterations = 100000
  )
  initial_continuous = find_render_road_profile_continuous_violations(
    problem,
    initial_solution,
    tolerance = 1e-6
  )
  solution = solve_render_road_profile_problem(
    problem,
    maximum_iterations = 100000
  )

  expect_lt(initial_continuous$continuous_chord_margin, -0.3)
  expect_gt(nrow(initial_continuous$requests), 0L)
  expect_gt(solution$refinement_iterations, 0L)
  expect_gte(
    solution$continuous_diagnostics$continuous_chord_margin,
    -1e-3
  )
  expect_equal(solution$engineering_audit$tolerance, 1e-3)
  expect_true(solution$engineering_audit$passed)
})

test_that("closed loops use input-order-invariant periodic support arcs", {
  skip_render_road_profile_test_dependencies()

  make_loop_roads = function(loop_order = seq_len(4L)) {
    loop_lines = list(
      make_render_road_profile_test_arc(80, 180, 270),
      make_render_road_profile_test_arc(80, 270, 360),
      make_render_road_profile_test_arc(80, 0, 90),
      make_render_road_profile_test_arc(80, 90, 180)
    )
    loop_clearance = c(10, 10, 5.5, 5.5)
    make_render_road_profile_test_roads(
      lines = c(
        list(
          make_render_road_profile_test_line(c(-130, 60, 130, 60)),
          make_render_road_profile_test_line(c(-130, -60, 130, -60))
        ),
        loop_lines[loop_order]
      ),
      layer = c(0, 0, rep(1, 4)),
      way_id = c(
        "surface-top",
        "surface-bottom",
        rep("elevated-loop", 4)
      ),
      clearance = c(5.5, 5.5, loop_clearance[loop_order])
    )
  }
  support_signature = function(problem) {
    loop_span = problem$spans[
      problem$spans$reference_regime == "elevated" &
        problem$spans$closed,
      ,
      drop = FALSE
    ]
    expect_equal(nrow(loop_span), 1L)
    expect_identical(loop_span$reference, "periodic_chord")
    arcs = problem$support_arcs[
      problem$support_arcs$span_id == loop_span$span_id,
      ,
      drop = FALSE
    ]
    control_id = sort(unique(c(
      arcs$start_control_id,
      arcs$end_control_id
    )))
    signature = do.call(
      rbind,
      lapply(control_id, function(control) {
        control_row = problem$controls[control, , drop = FALSE]
        fragment_row = match(
          control_row$render_road_fragment_id,
          problem$topology$fragments$render_road_fragment_id
        )
        xy = interpolate_render_road_metric_line(
          sf::st_geometry(problem$topology$fragments)[[fragment_row]],
          control_row$distance
        )
        clearance = problem$clearances$clearance[
          match(control, problem$clearances$upper_control_id)
        ]
        data.frame(
          x = round(xy[1, 1], 6),
          y = round(xy[1, 2], 6),
          clearance = clearance
        )
      })
    )
    expect_equal(nrow(arcs), 4L)
    expect_true(all(arcs$arc_length > 0))
    signature[order(signature$x, signature$y), , drop = FALSE]
  }

  original_problem = build_render_road_profile_test_problem(
    build_render_road_profile_test_topology(make_loop_roads()),
    terrain_elevation = c(0, 0, rep(-8, 4)),
    settings = list(
      maximum_grade = 0.12,
      maximum_grade_rate = 5e-3,
      uplift_weight = 0.1
    )
  )
  permuted_problem = build_render_road_profile_test_problem(
    build_render_road_profile_test_topology(
      make_loop_roads(c(3, 1, 4, 2))
    ),
    terrain_elevation = c(0, 0, rep(-8, 4)),
    settings = list(
      maximum_grade = 0.12,
      maximum_grade_rate = 5e-3,
      uplift_weight = 0.1
    )
  )

  expect_equal(
    support_signature(original_problem),
    support_signature(permuted_problem),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})
