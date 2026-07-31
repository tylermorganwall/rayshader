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

test_that("candidate ground anchors emit immediate junction equalities", {
  skip_render_road_profile_test_dependencies()

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-100, 0, 0, 0)),
      make_render_road_profile_test_line(c(0, 0, 100, 5)),
      make_render_road_profile_test_line(c(0, 0, 100, -5)),
      make_render_road_profile_test_line(c(-50, -20, -50, 20))
    ),
    layer = c(0, 0, 0, 1),
    way_id = c("active-surface", "approach-a", "approach-b", "bridge")
  )
  topology = build_render_road_profile_test_topology(roads)
  fragment_id = setNames(
    topology$fragments$render_road_fragment_id,
    topology$fragments$render_road_way_id
  )
  active_surface_id = fragment_id[["active-surface"]]
  approach_id = fragment_id[c("approach-a", "approach-b")]
  active_endpoint_id = topology$endpoints$render_road_endpoint_id[
    topology$endpoints$render_road_fragment_id == active_surface_id &
      topology$endpoints$endpoint_side == "end"
  ]

  expect_true(
    active_endpoint_id %in% topology$candidate_anchor_endpoint_id
  )
  expect_true(
    active_surface_id %in%
      topology$prospective_solve_expandable_fragment_id
  )
  expect_true(all(
    approach_id %in%
      topology$prospective_solve_terminal_ground_fragment_id
  ))
  expect_true(all(
    approach_id %in% topology$prospective_solve_fragment_id
  ))

  active_equality = topology$prospective_solve_junction_equality_pairs
  equality_partner = c(
    active_equality$fragment_b[
      active_equality$fragment_a == active_surface_id
    ],
    active_equality$fragment_a[
      active_equality$fragment_b == active_surface_id
    ]
  )
  expect_setequal(equality_partner, approach_id)
  expect_false(any(
    active_equality$fragment_a %in%
      approach_id &
      active_equality$fragment_b %in% approach_id
  ))
  expect_false(any(
    topology$selected_continuations$fragment_a == active_surface_id |
      topology$selected_continuations$fragment_b == active_surface_id
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
  junctions = problem$junction_equalities
  expect_lt(
    max(abs(
      solution$controls$height[junctions$control_b] -
        solution$controls$height[junctions$control_a]
    )),
    1e-6
  )
  continuations = problem$continuation_equalities
  expect_lt(
    max(abs(
      continuations$sign_a *
        solution$controls$grade[continuations$control_a] -
        continuations$sign_b *
          solution$controls$grade[continuations$control_b]
    )),
    1e-6
  )
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

test_that("native profile evaluation preserves interval and clamp semantics", {
  specification = list(
    fragment_id = 7L,
    fragment_component = 1L,
    control_start = 0L,
    control_count = 3L,
    control_row = 0:2,
    control_distance = c(0, 1e-12, 1e12),
    control_tolerance = 1e-14,
    underground = TRUE,
    terrain_start = 0L,
    terrain_count = 2L,
    terrain_distance = c(0, 1e12),
    terrain_elevation = c(0, 0),
    chord_span_id = integer(0),
    chord_fragment_index = integer(0),
    chord_span_offset = numeric(0),
    chord_orientation = integer(0),
    chord_fragment_length = numeric(0),
    arc_span_id = integer(0),
    arc_start_control = integer(0),
    arc_end_control = integer(0),
    arc_start_station = numeric(0),
    arc_end_station = numeric(0),
    arc_length = numeric(0),
    arc_span_length = numeric(0),
    arc_closed = logical(0),
    arc_id = integer(0),
    overlap_id = integer(0),
    overlap_lower_fragment_index = integer(0),
    overlap_upper_fragment_index = integer(0),
    overlap_lower_start = numeric(0),
    overlap_lower_end = numeric(0),
    overlap_upper_start = numeric(0),
    overlap_upper_end = numeric(0),
    overlap_clearance = numeric(0),
    prior_overlap_id = integer(0),
    prior_lower_distance = numeric(0),
    prior_upper_distance = numeric(0),
    finite_geometry = TRUE,
    finite_control_terrain = TRUE
  )
  height = c(1, 2, 3)
  grade = c(0.1, -0.2, 0.3)
  distance = c(
    -1,
    0,
    1e-12,
    1e-12,
    1.001e-12,
    5e11,
    1e12,
    2e12
  )
  reference_evaluate = function(distance) {
    distance = pmin(
      pmax(distance, 0),
      utils::tail(
        specification$control_distance,
        1L
      )
    )
    interval = findInterval(
      distance,
      specification$control_distance,
      all.inside = TRUE,
      rightmost.closed = TRUE
    )
    interval = pmin(interval, length(height) - 1L)
    second = interval + 1L
    interval_length = specification$control_distance[second] -
      specification$control_distance[interval]
    local_distance = distance -
      specification$control_distance[interval]
    grade_change = grade[second] - grade[interval]
    list(
      distance = distance,
      height = height[interval] +
        grade[interval] * local_distance +
        grade_change * local_distance^2 / (2 * interval_length),
      grade = grade[interval] +
        grade_change * local_distance / interval_length,
      control_a = interval,
      control_b = second
    )
  }

  reference = reference_evaluate(distance)
  native = evaluate_render_road_profiles_cpp(
    fragment_index = rep.int(0L, length(distance)),
    distance = distance,
    control_start = specification$control_start,
    control_count = specification$control_count,
    control_row = specification$control_row,
    control_distance = specification$control_distance,
    height = height,
    grade = grade
  )

  expect_equal(native$distance, reference$distance, tolerance = 0)
  expect_equal(native$height, reference$height, tolerance = 1e-12)
  expect_equal(native$grade, reference$grade, tolerance = 1e-14)
  expect_identical(native$control_a, reference$control_a)
  expect_identical(native$control_b, reference$control_b)
})

test_that("native continuous audit matches the R reference and stays compact", {
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
  problem = build_render_road_profile_test_problem(
    build_render_road_profile_test_topology(roads),
    settings = list(
      maximum_grade = 0.15,
      maximum_grade_rate = 1.4e-3,
      terrain_reference_weight = 1e-3,
      uplift_weight = 1e-2
    )
  )
  solution = solve_render_road_profile_components_once(
    problem = problem,
    verbose = FALSE,
    absolute_tolerance = 1e-7,
    relative_tolerance = 1e-7,
    maximum_iterations = 100000
  )
  reference = find_render_road_profile_continuous_violations_r_reference(
    problem,
    solution,
    tolerance = 1e-6
  )
  native = find_render_road_profile_continuous_violations(
    problem,
    solution,
    tolerance = 1e-6,
    diagnostics = TRUE
  )
  compact = find_render_road_profile_continuous_violations(
    problem,
    solution,
    tolerance = 1e-6,
    diagnostics = FALSE
  )

  expect_equal(
    native$continuous_terrain_margin,
    reference$continuous_terrain_margin,
    tolerance = 1e-12
  )
  expect_equal(
    native$continuous_chord_margin,
    reference$continuous_chord_margin,
    tolerance = 1e-12
  )
  expect_equal(native$terrain, reference$terrain, tolerance = 1e-12)
  expect_equal(native$chord, reference$chord, tolerance = 1e-12)
  expect_identical(
    native$requests$type,
    reference$requests$type
  )
  expect_identical(
    native$requests$event_id,
    reference$requests$event_id
  )
  expect_equal(
    native$requests[c("distance_a", "distance_b")],
    reference$requests[c("distance_a", "distance_b")],
    tolerance = max(problem$controls$control_tolerance),
    ignore_attr = TRUE
  )
  expect_identical(
    compact$finite_profile_coordinates,
    reference$finite_profile_coordinates
  )
  expect_false(any(c("terrain", "chord", "overlap") %in% names(compact)))

  engineering = audit_render_road_profiles(
    problem,
    solution,
    tolerance = 1e-6,
    continuous = compact
  )
  expect_equal(
    engineering$continuous_chord_margin,
    compact$continuous_chord_margin,
    tolerance = 0
  )
})

test_that("native overlap requests and non-finite flags preserve R semantics", {
  skip_render_road_profile_test_dependencies(solver = TRUE)

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-140, 0, 140, 0)),
      make_render_road_profile_test_line(c(-100, 0, 100, 0))
    ),
    layer = c(0, 1),
    way_id = c("lower", "upper")
  )
  problem = build_render_road_profile_test_problem(
    build_render_road_profile_test_topology(roads)
  )
  solution = solve_render_road_profile_components_once(
    problem = problem,
    verbose = FALSE,
    absolute_tolerance = 1e-7,
    relative_tolerance = 1e-7,
    maximum_iterations = 100000
  )
  upper_fragment = problem$overlap_relations$upper_fragment_id[[1L]]
  upper_control = solution$controls$render_road_fragment_id == upper_fragment
  solution$controls$height[upper_control] =
    solution$controls$height[upper_control] - 10
  reference = find_render_road_profile_continuous_violations_r_reference(
    problem,
    solution,
    tolerance = 1e-6
  )
  native = find_render_road_profile_continuous_violations(
    problem,
    solution,
    tolerance = 1e-6,
    diagnostics = TRUE
  )

  expect_equal(
    native$continuous_overlap_clearance_margin,
    reference$continuous_overlap_clearance_margin,
    tolerance = 1e-12
  )
  expect_identical(native$requests$type, reference$requests$type)
  expect_identical(native$requests$event_id, reference$requests$event_id)
  expect_equal(
    native$requests[c("distance_a", "distance_b")],
    reference$requests[c("distance_a", "distance_b")],
    tolerance = max(problem$controls$control_tolerance),
    ignore_attr = TRUE
  )

  problem$controls$terrain[[1L]] = Inf
  problem$audit_specification =
    prepare_render_road_profile_audit_specification(problem)
  solution$controls$terrain[[1L]] = Inf
  reference_finite =
    find_render_road_profile_continuous_violations_r_reference(
      problem,
      solution,
      tolerance = 1e-6
    )$finite_profile_coordinates
  native_finite = find_render_road_profile_continuous_violations(
    problem,
    solution,
    tolerance = 1e-6,
    diagnostics = TRUE
  )$finite_profile_coordinates
  expect_identical(native_finite, reference_finite)
})

expect_render_road_profile_compiler_parity = function(reference, native) {
  table_fields = c(
    "controls",
    "intervals",
    "spans",
    "support_arcs",
    "anchors",
    "clearances",
    "overlap_relations",
    "junction_equalities",
    "continuation_equalities",
    "chord_controls",
    "curvature_terms",
    "constraints"
  )
  for (field in table_fields) {
    expect_equal(
      native[[field]],
      reference[[field]],
      tolerance = 1e-12,
      ignore_attr = TRUE,
      info = field
    )
  }
  expect_equal(native$P, reference$P, tolerance = 1e-12)
  expect_equal(native$q, reference$q, tolerance = 1e-12)
  expect_equal(native$A, reference$A, tolerance = 1e-12)
  expect_equal(native$lower, reference$lower, tolerance = 1e-12)
  expect_equal(native$upper, reference$upper, tolerance = 1e-12)
  expect_identical(
    native$variable_component,
    reference$variable_component
  )
  expect_identical(
    as.integer(native$diagnostics$constraint_counts),
    as.integer(reference$diagnostics$constraint_counts)
  )
  expect_identical(
    names(native$diagnostics$constraint_counts),
    names(reference$diagnostics$constraint_counts)
  )
}

test_that("native problem compiler matches reference overlap rebuilds", {
  skip_render_road_profile_test_dependencies()

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-140, 0, 140, 0)),
      make_render_road_profile_test_line(c(-100, 0, 100, 0))
    ),
    layer = c(0, 1),
    way_id = c("lower", "upper")
  )
  topology = build_render_road_profile_test_topology(roads)
  native = build_render_road_profile_test_problem(topology)
  reference = do.call(
    build_render_road_profile_problem_r_reference,
    c(
      list(
        topology = native$topology,
        terrain_profiles = native$terrain_profiles
      ),
      native$settings
    )
  )
  expect_render_road_profile_compiler_parity(reference, native)

  overlap = native$overlap_relations[1L, , drop = FALSE]
  fraction = 0.37
  adaptive = data.frame(
    type = c(
      "terrain_floor",
      "terrain_floor",
      "terrain_floor",
      "overlap_clearance"
    ),
    fragment_a = c(
      overlap$lower_fragment_id[[1L]],
      overlap$lower_fragment_id[[1L]],
      overlap$lower_fragment_id[[1L]],
      overlap$lower_fragment_id[[1L]]
    ),
    distance_a = c(
      native$settings$control_tolerance / 2,
      native$settings$control_tolerance * 3 / 4,
      native$fragment_length[[
        as.character(overlap$lower_fragment_id[[1L]])
      ]] -
        native$settings$control_tolerance / 2,
      overlap$lower_distance_start[[1L]] +
        fraction *
          (overlap$lower_distance_end[[1L]] -
            overlap$lower_distance_start[[1L]])
    ),
    fragment_b = c(
      NA_integer_,
      NA_integer_,
      NA_integer_,
      overlap$upper_fragment_id[[1L]]
    ),
    distance_b = c(
      NA_real_,
      NA_real_,
      NA_real_,
      overlap$upper_distance_start[[1L]] +
        fraction *
          (overlap$upper_distance_end[[1L]] -
            overlap$upper_distance_start[[1L]])
    ),
    event_id = c(
      NA_integer_,
      NA_integer_,
      NA_integer_,
      overlap$overlap_id[[1L]]
    ),
    clearance = c(
      NA_real_,
      NA_real_,
      NA_real_,
      overlap$clearance[[1L]]
    ),
    source_margin = rep(-1, 4),
    stringsAsFactors = FALSE
  )
  reference_rebuild = do.call(
    build_render_road_profile_problem_r_reference,
    c(
      list(
        topology = native$topology,
        terrain_profiles = native$terrain_profiles,
        adaptive_constraints = adaptive
      ),
      native$settings
    )
  )
  specification = native$profile_specification
  native$topology = NULL
  native$terrain_profiles = NULL
  native_rebuild = rebuild_render_road_profile_problem(native, adaptive)

  expect_identical(native_rebuild$profile_specification, specification)
  expect_render_road_profile_compiler_parity(
    reference_rebuild,
    native_rebuild
  )
})

test_that("native compiler specification is transient numerical input", {
  skip_render_road_profile_test_dependencies()

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_arc(80, 0, 120),
      make_render_road_profile_test_arc(80, 120, 240),
      make_render_road_profile_test_arc(80, 240, 360),
      make_render_road_profile_test_line(c(-120, 0, 120, 0))
    ),
    layer = c(1, 1, 1, 0),
    way_id = c("loop", "loop", "loop", "ground")
  )
  native = build_render_road_profile_test_problem(
    build_render_road_profile_test_topology(roads),
    settings = list(
      maximum_grade = 0.12,
      maximum_grade_rate = 5e-3
    )
  )
  reference = do.call(
    build_render_road_profile_problem_r_reference,
    c(
      list(
        topology = native$topology,
        terrain_profiles = native$terrain_profiles
      ),
      native$settings
    )
  )
  specification_objects = unlist(
    lapply(native$profile_specification, unclass),
    recursive = TRUE
  )

  expect_false(any(vapply(
    specification_objects,
    inherits,
    logical(1),
    what = "sf"
  )))
  expect_false(any(
    vapply(
      specification_objects,
      typeof,
      character(1)
    ) ==
      "externalptr"
  ))
  expect_render_road_profile_compiler_parity(reference, native)
})

test_that("native adaptive loop preserves the R refinement path", {
  skip_render_road_profile_test_dependencies(solver = TRUE)

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-130, 60, 130, 60)),
      make_render_road_profile_test_line(c(-130, -60, 130, -60)),
      make_render_road_profile_test_arc(80, 180, 270),
      make_render_road_profile_test_arc(80, 270, 360),
      make_render_road_profile_test_arc(80, 0, 90),
      make_render_road_profile_test_arc(80, 90, 180)
    ),
    layer = c(0, 0, rep(1, 4)),
    way_id = c(
      "surface-top",
      "surface-bottom",
      rep("elevated-loop", 4)
    ),
    clearance = c(5.5, 5.5, 10, 10, 5.5, 5.5)
  )
  problem = build_render_road_profile_test_problem(
    build_render_road_profile_test_topology(roads),
    settings = list(
      maximum_grade = 0.12,
      maximum_grade_rate = 5e-3
    )
  )
  settings = list(
    maximum_iterations = 100000,
    profile_tolerance = 1e-3
  )
  reference = do.call(
    solve_render_road_profile_problem_r_reference,
    c(list(problem = problem), settings)
  )
  native = do.call(
    solve_render_road_profile_problem,
    c(list(problem = problem), settings)
  )

  expect_identical(
    native$controls$control_id,
    reference$controls$control_id
  )
  expect_equal(
    native$controls$height,
    reference$controls$height,
    tolerance = 1e-7
  )
  expect_equal(
    native$controls$grade,
    reference$controls$grade,
    tolerance = 1e-9
  )
  expect_identical(native$components$status, reference$components$status)
  expect_identical(
    native$refinement_iterations,
    reference$refinement_iterations
  )
  expect_equal(
    native$continuous_diagnostics$continuous_terrain_margin,
    reference$continuous_diagnostics$continuous_terrain_margin,
    tolerance = 1e-8
  )
  expect_equal(
    native$continuous_diagnostics$continuous_chord_margin,
    reference$continuous_diagnostics$continuous_chord_margin,
    tolerance = 1e-8
  )
  expect_equal(
    native$continuous_diagnostics$continuous_overlap_clearance_margin,
    reference$continuous_diagnostics$continuous_overlap_clearance_margin,
    tolerance = 1e-8
  )

  request_type = c(
    "terrain_floor",
    "no_dip_chord",
    "overlap_clearance"
  )
  expect_length(
    native$refinement_requests,
    length(reference$refinement_requests)
  )
  for (iteration in seq_along(reference$refinement_requests)) {
    native_requests = as.data.frame(
      native$refinement_requests[[iteration]],
      stringsAsFactors = FALSE
    )
    native_requests$type = request_type[native_requests$type]
    expect_equal(
      native_requests,
      reference$refinement_requests[[iteration]],
      tolerance = 0,
      ignore_attr = TRUE,
      info = paste("refinement", iteration - 1L)
    )
  }

  rendered_reference = unlist(
    lapply(
      problem$topology$fragments$render_road_fragment_id,
      function(fragment_id) {
        distance =
          problem$terrain_profiles[[as.character(fragment_id)]]$distance
        evaluate_render_road_profile_at(
          reference$problem,
          reference,
          fragment_id,
          distance
        )$height
      }
    ),
    use.names = FALSE
  )
  expect_equal(
    native$rendered_elevation$elevation,
    rendered_reference,
    tolerance = 1e-7
  )
  expect_equal(
    native$timing$callback_count,
    nrow(native$components) *
      (native$refinement_iterations + 1L)
  )
})

test_that("native adaptive failures preserve existing condition classes", {
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
  problem = build_render_road_profile_test_problem(
    build_render_road_profile_test_topology(roads),
    settings = list(
      maximum_grade = 0.15,
      maximum_grade_rate = 1.4e-3,
      terrain_reference_weight = 1e-3,
      uplift_weight = 1e-2
    )
  )
  reference_refinement = tryCatch(
    solve_render_road_profile_problem_r_reference(
      problem,
      maximum_iterations = 100000,
      maximum_refinement_iterations = 0
    ),
    error = identity
  )
  native_refinement = tryCatch(
    solve_render_road_profile_problem(
      problem,
      maximum_iterations = 100000,
      maximum_refinement_iterations = 0
    ),
    error = identity
  )
  expect_s3_class(
    reference_refinement,
    "render_road_profile_refinement_failure"
  )
  expect_identical(class(native_refinement), class(reference_refinement))

  reference_solver = tryCatch(
    solve_render_road_profile_problem_r_reference(
      problem,
      maximum_iterations = 1
    ),
    error = identity
  )
  native_solver = tryCatch(
    solve_render_road_profile_problem(
      problem,
      maximum_iterations = 1
    ),
    error = identity
  )
  expect_s3_class(reference_solver, "render_road_profile_infeasible")
  expect_identical(class(native_solver), class(reference_solver))
})

test_that("native adaptive results retain no external pointer", {
  skip_render_road_profile_test_dependencies(solver = TRUE)

  roads = make_render_road_profile_test_roads(
    lines = list(
      make_render_road_profile_test_line(c(-100, 0, 100, 0)),
      make_render_road_profile_test_line(c(0, -100, 0, 100))
    ),
    layer = c(0, 1),
    way_id = c("surface", "bridge")
  )
  solution = solve_render_road_profile_problem(
    build_render_road_profile_test_problem(
      build_render_road_profile_test_topology(roads)
    ),
    maximum_iterations = 100000
  )
  contains_external_pointer = function(object) {
    if (typeof(object) == "externalptr") {
      return(TRUE)
    }
    if (methods::is(object, "S4")) {
      return(any(vapply(
        methods::slotNames(object),
        function(slot_name) {
          contains_external_pointer(methods::slot(object, slot_name))
        },
        logical(1)
      )))
    }
    if (is.list(object)) {
      return(any(vapply(
        object,
        contains_external_pointer,
        logical(1)
      )))
    }
    FALSE
  }

  expect_false(contains_external_pointer(solution))
})
