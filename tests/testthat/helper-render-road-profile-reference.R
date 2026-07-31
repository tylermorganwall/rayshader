# Reference-only helpers for focused road-profile tests.
#
# These ordinary R implementations intentionally remain outside the package
# namespace. They provide independent geometry and OSQP checks without
# retaining the superseded evaluator, compiler, or adaptive loop in
# production.
interpolate_render_road_metric_line = function(geometry, distance) {
  info = calculate_render_road_metric_line_distances(geometry)
  distance = pmin(pmax(as.numeric(distance), 0), info$length)
  interval = findInterval(distance, info$distance, all.inside = TRUE)
  interval = pmin(interval, length(info$distance) - 1L)
  run = info$distance[interval + 1L] - info$distance[interval]
  fraction = ifelse(
    run > 0,
    (distance - info$distance[interval]) / run,
    0
  )
  start = info$coordinates[interval, , drop = FALSE]
  end = info$coordinates[interval + 1L, , drop = FALSE]
  start + fraction * (end - start)
}

solve_render_road_profile_components_once = function(
  problem,
  verbose,
  absolute_tolerance,
  relative_tolerance,
  maximum_iterations
) {
  component_id = sort(unique(problem$variable_component))
  solution = rep(NA_real_, length(problem$q))
  component_rows = vector("list", length(component_id))
  component_result = vector("list", length(component_id))
  accepted_status = c("solved", "solved inaccurate")
  for (component_index in seq_along(component_id)) {
    current_component = component_id[[component_index]]
    variables = which(problem$variable_component == current_component)
    constraint_rows = which(
      problem$constraints$solve_component_id == current_component
    )
    component_matrix = problem$A[constraint_rows, , drop = FALSE]
    outside_variables = setdiff(
      which(Matrix::colSums(abs(component_matrix)) > 0),
      variables
    )
    if (length(outside_variables)) {
      stop(
        "A road profile component constraint references another component.",
        call. = FALSE
      )
    }
    result = osqp::solve_osqp(
      P = problem$P[variables, variables, drop = FALSE],
      q = problem$q[variables],
      A = problem$A[constraint_rows, variables, drop = FALSE],
      l = problem$lower[constraint_rows],
      u = problem$upper[constraint_rows],
      pars = osqp::osqpSettings(
        verbose = verbose,
        eps_abs = absolute_tolerance,
        eps_rel = relative_tolerance,
        max_iter = as.integer(maximum_iterations),
        polishing = TRUE
      )
    )
    status = tolower(result$info$status)
    component_result[[component_index]] = result
    component_rows[[component_index]] = data.frame(
      solve_component_id = current_component,
      status = result$info$status,
      iterations = result$info$iter,
      objective = result$info$obj_val,
      primal_residual = result$info$prim_res,
      dual_residual = result$info$dual_res,
      stringsAsFactors = FALSE
    )
    if (!(status %in% accepted_status) || any(!is.finite(result$x))) {
      diagnostics = diagnose_render_road_profile_component(
        problem,
        current_component,
        result$info$status
      )
      condition = structure(
        list(
          message = sprintf(
            "Road profile component %d was not solved: %s.",
            current_component,
            result$info$status
          ),
          call = NULL,
          diagnostics = diagnostics
        ),
        class = c(
          "render_road_profile_infeasible",
          "error",
          "condition"
        )
      )
      stop(condition)
    }
    solution[variables] = result$x
  }
  controls = problem$controls
  controls$height = solution[controls$height_variable]
  controls$grade = solution[controls$grade_variable]
  solved = list(
    problem = problem,
    solution = solution,
    controls = controls,
    components = do.call(rbind, component_rows),
    solver_results = component_result
  )
  class(solved) = c("render_road_profile_solution", class(solved))
  solved
}
