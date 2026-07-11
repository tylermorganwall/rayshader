#'@title make_water
#'
#'@description Makes the water in the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param waterheight Default `0`. Water level. Either a scalar, a matrix with the same dimensions as `heightmap`, or a spatial raster that can be projected/resampled to the heightmap grid.
#'@param watercolor Default `blue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param water_render_method Default `"contour"`. Water meshing method. `"contour"` clips scalar/matrix water meshes and uses the raster-cell renderer for spatial water rasters; `"raster"` explicitly uses spatial water raster cells; `"polygon"` fits each spatial water component to a DEM contour by matching contour area to raster footprint area, initialized from covered DEM values; `"legacy"` uses the previous box/grid renderer.
#'@param water_edge_extension Default `0.5`. For spatial `waterheight` inputs, amount in grid cells to expand finite water cells at boundary edges, up to a maximum of half a cell.
#'@param water_edge_clamp Default `FALSE`. For spatial `waterheight` inputs, if `TRUE`, resolves each connected water footprint to a single level, then lowers it by the largest finite exterior sidewall height after edge expansion. Heightmap-boundary and NA-slice edges are ignored when computing the lowering amount.
#'@param parallel Default `FALSE`. If `TRUE`, spatial polygon water components are fit in parallel using `mirai`. A positive numeric value sets the worker count.
#'@param heightmap_extent Default `NULL`. Active scene extent for spatial `waterheight` inputs.
#'@param heightmap_crs Default `NULL`. Active scene CRS for spatial `waterheight` inputs.
#'@keywords internal
make_water = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5,
  water_render_method = c("contour", "raster", "polygon", "legacy"),
  water_edge_extension = 0.5,
  water_edge_clamp = FALSE,
  parallel = FALSE,
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  water_render_method = match.arg(water_render_method)
  if (identical(water_render_method, "legacy")) {
    return(make_water_legacy(
      heightmap = heightmap,
      waterheight = waterheight,
      watercolor = watercolor,
      zscale = zscale,
      wateralpha = wateralpha
    ))
  }
  make_water_contour(
    heightmap = heightmap,
    waterheight = waterheight,
    watercolor = watercolor,
    zscale = zscale,
    wateralpha = wateralpha,
    water_render_method = water_render_method,
    water_edge_extension = water_edge_extension,
    water_edge_clamp = water_edge_clamp,
    parallel = parallel,
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs
  )
}

#'@keywords internal
make_water_contour = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5,
  water_render_method = c("contour", "raster", "polygon"),
  water_edge_extension = 0.5,
  water_edge_clamp = FALSE,
  parallel = FALSE,
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  water_render_method = match.arg(water_render_method)
  heightmap = heightmap / zscale
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  waterheight_is_spatial = is_spatial_heightmap_input(waterheight)
  waterheight = normalize_waterheight_matrix(
    waterheight,
    nr = nr,
    nc = nc,
    zscale = zscale,
    caller = "make_water",
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs
  )
  valid_water = is.finite(heightmap) & is.finite(waterheight)
  if (waterheight_is_spatial) {
    return(make_spatial_water_surface(
      waterheight = waterheight,
      heightmap = heightmap,
      valid_water = valid_water,
      watercolor = watercolor,
      wateralpha = wateralpha,
      water_render_method = water_render_method,
      water_edge_extension = water_edge_extension,
      water_edge_clamp = water_edge_clamp,
      parallel = parallel
    ))
  }
  if (!any(valid_water)) {
    warning(
      "No water rendered--no finite heightmap and water level values overlap."
    )
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }
  flooded = valid_water & heightmap < waterheight
  if (!any(flooded)) {
    warning(format_no_water_warning(heightmap, waterheight, zscale))
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }

  water_mesh = make_water_mesh_cpp(heightmap, waterheight)
  vertices = water_mesh$vertices
  if (length(vertices) > 0) {
    for (component in vertices) {
      if (is.matrix(component) && nrow(component) > 0) {
        rgl::triangles3d(
          x = component,
          indices = seq_len(nrow(component)),
          color = watercolor,
          alpha = wateralpha,
          lit = FALSE,
          front = "filled",
          back = "filled",
          texture = NULL,
          tag = "water"
        )
      }
    }
  }
  invisible(water_mesh)
}

#'@keywords internal
make_spatial_water_surface = function(
  waterheight,
  heightmap = NULL,
  valid_water,
  watercolor = "lightblue",
  wateralpha = 0.5,
  water_render_method = c("contour", "raster", "polygon"),
  water_edge_extension = 0.5,
  water_edge_clamp = FALSE,
  parallel = FALSE
) {
  water_render_method = match.arg(water_render_method)
  water_edge_extension = validate_water_edge_extension(water_edge_extension)
  water_edge_clamp = validate_water_edge_clamp(water_edge_clamp)
  water_surface = waterheight
  water_surface[!valid_water] = NA_real_
  if (!any(is.finite(water_surface))) {
    warning(
      "No water rendered--no finite heightmap and water level values overlap."
    )
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }
  if (isTRUE(water_edge_clamp)) {
    water_surface = adjust_spatial_water_surface_to_edge_terrain(
      water_surface = water_surface,
      heightmap = heightmap,
      water_edge_extension = water_edge_extension
    )
  }
  if (identical(water_render_method, "polygon")) {
    water_mesh = make_spatial_water_polygon_surface(
      water_surface = water_surface,
      heightmap = heightmap,
      parallel = parallel
    )
    triangle_vertices = water_mesh$vertices
    line_vertices = water_mesh$lines
  } else {
    triangle_vertices = make_spatial_water_cell_surface(
      water_surface = water_surface,
      heightmap = heightmap,
      water_edge_extension = water_edge_extension
    )
    line_vertices = matrix(nrow = 0, ncol = 3)
  }
  if (!nrow(triangle_vertices)) {
    warning(
      "No water rendered--spatial `waterdepth` does not cover any renderable heightmap cells."
    )
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }
  rgl::triangles3d(
    x = triangle_vertices,
    indices = seq_len(nrow(triangle_vertices)),
    color = watercolor,
    alpha = wateralpha,
    lit = FALSE,
    front = "filled",
    back = "filled",
    polygon_offset = -1,
    texture = NULL,
    tag = "water"
  )
  invisible(list(
    vertices = list(triangle_vertices),
    lines = line_vertices
  ))
}

#'@keywords internal
validate_water_edge_extension = function(water_edge_extension = 0.5) {
  if (
    !is.numeric(water_edge_extension) ||
      length(water_edge_extension) != 1 ||
      is.na(water_edge_extension) ||
      water_edge_extension < 0 ||
      water_edge_extension > 0.5
  ) {
    stop(
      "`water_edge_extension` must be a single number between 0 and 0.5.",
      call. = FALSE
    )
  }
  water_edge_extension
}

#'@keywords internal
validate_water_edge_clamp = function(water_edge_clamp = FALSE) {
  if (
    !is.logical(water_edge_clamp) ||
      length(water_edge_clamp) != 1 ||
      is.na(water_edge_clamp)
  ) {
    stop("`water_edge_clamp` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  water_edge_clamp
}

#'@keywords internal
make_spatial_water_cell_geometry = function(
  water_surface,
  heightmap = NULL,
  water_edge_extension = 0.5
) {
  water_edge_extension = validate_water_edge_extension(water_edge_extension)
  nr = nrow(water_surface)
  nc = ncol(water_surface)
  finite_cells = which(is.finite(water_surface), arr.ind = TRUE)
  if (!nrow(finite_cells)) {
    return(empty_spatial_water_cell_geometry(nr, nc))
  }
  row_index = finite_cells[, 1]
  col_index = finite_cells[, 2]
  water_height = water_surface[finite_cells]
  water_mask = is.finite(water_surface)

  x_scene_min = -(nr - 1) / 2
  x_scene_max = (nr - 1) / 2
  z_scene_min = -(nc - 1) / 2
  z_scene_max = (nc - 1) / 2
  x_center = row_index - 1 - (nr - 1) / 2
  z_center = col_index - 1 - (nc - 1) / 2
  left_edge = row_index == 1L
  right_edge = row_index == nr
  bottom_edge = col_index == 1L
  top_edge = col_index == nc
  interior_left = row_index > 1L
  interior_right = row_index < nr
  interior_bottom = col_index > 1L
  interior_top = col_index < nc
  left_edge[interior_left] =
    !water_mask[cbind(row_index[interior_left] - 1L, col_index[interior_left])]
  right_edge[interior_right] =
    !water_mask[cbind(
      row_index[interior_right] + 1L,
      col_index[interior_right]
    )]
  bottom_edge[interior_bottom] =
    !water_mask[cbind(
      row_index[interior_bottom],
      col_index[interior_bottom] - 1L
    )]
  top_edge[interior_top] =
    !water_mask[cbind(row_index[interior_top], col_index[interior_top] + 1L)]
  eps = sqrt(.Machine$double.eps)
  left_side = left_edge
  right_side = right_edge
  bottom_side = bottom_edge
  top_side = top_edge
  left_side[interior_left] = left_edge[interior_left] |
    water_surface[
      cbind(row_index[interior_left] - 1L, col_index[interior_left])
    ] <
      water_height[interior_left] - eps
  right_side[interior_right] = right_edge[interior_right] |
    water_surface[
      cbind(row_index[interior_right] + 1L, col_index[interior_right])
    ] <
      water_height[interior_right] - eps
  bottom_side[interior_bottom] = bottom_edge[interior_bottom] |
    water_surface[
      cbind(row_index[interior_bottom], col_index[interior_bottom] - 1L)
    ] <
      water_height[interior_bottom] - eps
  top_side[interior_top] = top_edge[interior_top] |
    water_surface[
      cbind(row_index[interior_top], col_index[interior_top] + 1L)
    ] <
      water_height[interior_top] - eps

  x0_base = pmax(x_center - 0.5, x_scene_min)
  x1_base = pmin(x_center + 0.5, x_scene_max)
  z0_base = pmax(z_center - 0.5, z_scene_min)
  z1_base = pmin(z_center + 0.5, z_scene_max)
  extended_bounds = extend_spatial_water_bounds_to_terrain(
    heightmap = heightmap,
    water_height = water_height,
    x0 = x0_base,
    x1 = x1_base,
    z0 = z0_base,
    z1 = z1_base,
    left_edge = left_edge,
    right_edge = right_edge,
    bottom_edge = bottom_edge,
    top_edge = top_edge,
    water_edge_extension = water_edge_extension,
    x_scene_min = x_scene_min,
    x_scene_max = x_scene_max,
    z_scene_min = z_scene_min,
    z_scene_max = z_scene_max
  )
  x0 = extended_bounds$x0
  x1 = extended_bounds$x1
  z0 = extended_bounds$z0
  z1 = extended_bounds$z1

  renderable = x1 > x0 & z1 > z0
  if (!any(renderable)) {
    return(empty_spatial_water_cell_geometry(nr, nc))
  }

  list(
    nr = nr,
    nc = nc,
    row_index = row_index[renderable],
    col_index = col_index[renderable],
    water_height = water_height[renderable],
    x0 = x0[renderable],
    x1 = x1[renderable],
    z0 = z0[renderable],
    z1 = z1[renderable],
    left_edge = left_edge[renderable],
    right_edge = right_edge[renderable],
    bottom_edge = bottom_edge[renderable],
    top_edge = top_edge[renderable],
    left_side = left_side[renderable],
    right_side = right_side[renderable],
    bottom_side = bottom_side[renderable],
    top_side = top_side[renderable]
  )
}

#'@keywords internal
empty_spatial_water_cell_geometry = function(nr, nc) {
  list(
    nr = nr,
    nc = nc,
    row_index = integer(),
    col_index = integer(),
    water_height = numeric(),
    x0 = numeric(),
    x1 = numeric(),
    z0 = numeric(),
    z1 = numeric(),
    left_edge = logical(),
    right_edge = logical(),
    bottom_edge = logical(),
    top_edge = logical(),
    left_side = logical(),
    right_side = logical(),
    bottom_side = logical(),
    top_side = logical()
  )
}

#'@keywords internal
make_spatial_water_cell_surface = function(
  water_surface,
  heightmap = NULL,
  water_edge_extension = 0.5
) {
  geometry = make_spatial_water_cell_geometry(
    water_surface = water_surface,
    heightmap = heightmap,
    water_edge_extension = water_edge_extension
  )
  if (!length(geometry$water_height)) {
    return(matrix(nrow = 0, ncol = 3))
  }

  top_vertices = make_spatial_water_top_vertices(
    heightmap = heightmap,
    x0 = geometry$x0,
    x1 = geometry$x1,
    z0 = geometry$z0,
    z1 = geometry$z1,
    water_height = geometry$water_height
  )
  top_vertices = clip_spatial_water_top_to_terrain(
    top_vertices = top_vertices,
    heightmap = heightmap
  )

  side_vertices = make_spatial_water_edge_sides(
    heightmap = heightmap,
    water_height = geometry$water_height,
    x0 = geometry$x0,
    x1 = geometry$x1,
    z0 = geometry$z0,
    z1 = geometry$z1,
    left_edge = geometry$left_side,
    right_edge = geometry$right_side,
    bottom_edge = geometry$bottom_side,
    top_edge = geometry$top_side,
    row_index = geometry$row_index,
    col_index = geometry$col_index,
    nr = geometry$nr,
    nc = geometry$nc
  )
  rbind(top_vertices, side_vertices)
}

#'@keywords internal
make_spatial_water_polygon_surface = function(
  water_surface,
  heightmap = NULL,
  parallel = FALSE
) {
  require_spatial_water_polygon_packages()
  if (is.null(heightmap) || !is.matrix(heightmap)) {
    stop(
      "`water_render_method = \"polygon\"` requires a matrix heightmap.",
      call. = FALSE
    )
  }
  component_labels = label_spatial_water_components(is.finite(water_surface))
  component_count = max(component_labels)
  if (!component_count) {
    return(empty_spatial_water_polygon_mesh())
  }

  component_tasks = vector("list", component_count)
  for (component_id in seq_len(component_count)) {
    component_mask = component_labels == component_id
    component_tasks[[component_id]] = list(
      component_mask = component_mask,
      fallback_level = max(water_surface[component_mask], na.rm = TRUE)
    )
  }
  component_meshes = make_spatial_water_polygon_components(
    component_tasks = component_tasks,
    heightmap = heightmap,
    parallel = parallel
  )
  component_meshes = Filter(
    function(mesh) {
      !is.null(mesh) && nrow(mesh$vertices) > 0
    },
    component_meshes
  )
  if (!length(component_meshes)) {
    return(empty_spatial_water_polygon_mesh())
  }
  list(
    vertices = do.call(rbind, lapply(component_meshes, `[[`, "vertices")),
    lines = do.call(rbind, lapply(component_meshes, `[[`, "lines"))
  )
}

#'@keywords internal
require_spatial_water_polygon_packages = function() {
  missing_packages = c(
    if (!requireNamespace("sf", quietly = TRUE)) "sf",
    if (!requireNamespace("isoband", quietly = TRUE)) "isoband",
    if (!requireNamespace("decido", quietly = TRUE)) "decido"
  )
  if (length(missing_packages)) {
    stop(
      paste0(
        "`water_render_method = \"polygon\"` requires the ",
        paste(missing_packages, collapse = ", "),
        " package",
        if (length(missing_packages) == 1) "." else "s."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#'@keywords internal
validate_spatial_water_parallel = function(parallel = FALSE) {
  if (
    is.logical(parallel) &&
      length(parallel) == 1L &&
      !is.na(parallel)
  ) {
    return(parallel)
  }
  if (
    is.numeric(parallel) &&
      length(parallel) == 1L &&
      is.finite(parallel) &&
      parallel >= 1
  ) {
    return(floor(parallel))
  }
  stop(
    "`parallel` must be `TRUE`, `FALSE`, or a positive worker count.",
    call. = FALSE
  )
}

#'@keywords internal
spatial_water_parallel_worker_count = function(parallel, component_count) {
  parallel = validate_spatial_water_parallel(parallel)
  if (isFALSE(parallel) || component_count <= 1L) {
    return(1L)
  }
  requested_workers = if (isTRUE(parallel)) {
    option_cores = getOption("cores")
    if (
      is.numeric(option_cores) &&
        length(option_cores) == 1L &&
        is.finite(option_cores) &&
        option_cores >= 1
    ) {
      floor(option_cores)
    } else {
      detected_cores = parallel::detectCores()
      if (is.finite(detected_cores) && detected_cores >= 1) {
        detected_cores
      } else {
        1L
      }
    }
  } else {
    parallel
  }
  max(1L, min(component_count, floor(requested_workers)))
}

#'@keywords internal
make_spatial_water_polygon_components = function(
  component_tasks,
  heightmap,
  parallel = FALSE
) {
  worker_count = spatial_water_parallel_worker_count(
    parallel = parallel,
    component_count = length(component_tasks)
  )
  if (worker_count <= 1L) {
    return(lapply(
      component_tasks,
      make_spatial_water_polygon_component_from_task,
      heightmap = heightmap
    ))
  }
  make_spatial_water_polygon_components_parallel(
    component_tasks = component_tasks,
    heightmap = heightmap,
    worker_count = worker_count
  )
}

#'@keywords internal
make_spatial_water_polygon_component_from_task = function(task, heightmap) {
  make_spatial_water_polygon_component(
    component_mask = task$component_mask,
    heightmap = heightmap,
    fallback_level = task$fallback_level
  )
}

#'@keywords internal
make_spatial_water_polygon_components_parallel = function(
  component_tasks,
  heightmap,
  worker_count
) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop(
      "`parallel = TRUE` for `water_render_method = \"polygon\"` requires the mirai package.",
      call. = FALSE
    )
  }
  compute_profile = spatial_water_mirai_compute_profile()
  helper_functions = spatial_water_parallel_helper_functions()
  tryCatch(
    mirai::daemons(worker_count, .compute = compute_profile),
    error = function(e) {
      stop(
        "Could not start mirai daemons for parallel spatial polygon water fitting: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  on.exit(mirai::daemons(0, .compute = compute_profile), add = TRUE)
  component_meshes = mirai::mirai_map(
    component_tasks,
    function(task, heightmap, helper_functions) {
      worker_env = new.env(parent = .GlobalEnv)
      for (helper_name in names(helper_functions)) {
        helper_function = helper_functions[[helper_name]]
        if (
          is.function(helper_function) && typeof(helper_function) == "closure"
        ) {
          environment(helper_function) = worker_env
        }
        assign(helper_name, helper_function, envir = worker_env)
      }
      worker_env$make_spatial_water_polygon_component_from_task(
        task = task,
        heightmap = heightmap
      )
    },
    .args = list(
      heightmap = heightmap,
      helper_functions = helper_functions
    ),
    .compute = compute_profile
  )[]
  component_errors = vapply(
    component_meshes,
    spatial_water_mirai_result_is_error,
    logical(1)
  )
  if (any(component_errors)) {
    stop(
      "Parallel spatial polygon water fitting failed: ",
      spatial_water_mirai_error_message(component_meshes[[which(
        component_errors
      )[1L]]]),
      call. = FALSE
    )
  }
  component_meshes
}

#'@keywords internal
spatial_water_parallel_helper_functions = function() {
  helper_names = c(
    "make_spatial_water_polygon_component_from_task",
    "make_spatial_water_polygon_component",
    "empty_spatial_water_polygon_mesh",
    "make_spatial_water_component_footprint",
    "fit_spatial_water_component_polygon",
    "spatial_water_component_area_fit_at_level",
    "spatial_water_polygon_area",
    "spatial_water_polygon_perimeter",
    "make_spatial_water_level_polygon",
    "select_spatial_water_component_polygons",
    "extract_spatial_water_polygon_sfc",
    "spatial_water_sfc_is_empty",
    "triangulate_spatial_water_polygon_sfc",
    "make_spatial_water_polygon_sidewalls",
    "make_spatial_water_polygon_lines",
    "spatial_water_sfg_polygons",
    "clean_spatial_water_polygon_ring",
    "close_spatial_water_polygon_ring",
    "spatial_water_polygon_sidewall_segment",
    "spatial_water_point_inside_polygon",
    "spatial_water_point_has_finite_heightmap_cell",
    "make_spatial_water_polygon_sidewall_vertices",
    "spatial_water_side_segment_breakpoints",
    "spatial_water_axis_breakpoints",
    "clean_spatial_water_breakpoints",
    "interpolate_spatial_water_height",
    "spatial_water_row_col",
    "simplify_spatial_water_sidewall_points"
  )
  mget(
    helper_names,
    envir = environment(spatial_water_parallel_helper_functions),
    inherits = FALSE
  )
}

#'@keywords internal
spatial_water_mirai_compute_profile = function() {
  gsub(
    "[^A-Za-z0-9_]",
    "_",
    basename(tempfile("rayshader_water_"))
  )
}

#'@keywords internal
spatial_water_mirai_result_is_error = function(result) {
  inherits(result, c("error", "miraiError", "errorValue")) ||
    isTRUE(tryCatch(mirai::is_mirai_error(result), error = function(e) {
      FALSE
    })) ||
    isTRUE(tryCatch(mirai::is_error_value(result), error = function(e) FALSE))
}

#'@keywords internal
spatial_water_mirai_error_message = function(result) {
  if (inherits(result, "condition")) {
    return(conditionMessage(result))
  }
  result_text = tryCatch(
    paste(capture.output(print(result)), collapse = "\n"),
    error = function(e) ""
  )
  if (!nzchar(result_text)) {
    result_text = "unknown worker error"
  }
  result_text
}

#'@keywords internal
empty_spatial_water_polygon_mesh = function() {
  list(
    vertices = matrix(nrow = 0, ncol = 3),
    lines = matrix(nrow = 0, ncol = 3)
  )
}

#'@keywords internal
make_spatial_water_polygon_component = function(
  component_mask,
  heightmap,
  fallback_level
) {
  if (!is.finite(fallback_level)) {
    return(empty_spatial_water_polygon_mesh())
  }

  component_footprint = make_spatial_water_component_footprint(component_mask)
  if (spatial_water_sfc_is_empty(component_footprint)) {
    return(empty_spatial_water_polygon_mesh())
  }
  fit = fit_spatial_water_component_polygon(
    component_mask = component_mask,
    heightmap = heightmap,
    component_footprint = component_footprint,
    fallback_level = fallback_level
  )
  if (is.null(fit) || spatial_water_sfc_is_empty(fit$polygon)) {
    return(empty_spatial_water_polygon_mesh())
  }
  water_level = fit$level
  water_polygon = fit$polygon

  top_vertices = triangulate_spatial_water_polygon_sfc(
    water_polygon = water_polygon,
    water_level = water_level
  )
  side_vertices = make_spatial_water_polygon_sidewalls(
    water_polygon = water_polygon,
    heightmap = heightmap,
    water_level = water_level
  )
  list(
    vertices = rbind(top_vertices, side_vertices),
    lines = make_spatial_water_polygon_lines(
      water_polygon = water_polygon,
      water_level = water_level
    )
  )
}

#'@keywords internal
fit_spatial_water_component_polygon = function(
  component_mask,
  heightmap,
  component_footprint,
  fallback_level
) {
  target_area = spatial_water_polygon_area(component_footprint)
  if (!is.finite(target_area) || target_area <= sqrt(.Machine$double.eps)) {
    return(NULL)
  }
  target_perimeter = spatial_water_polygon_perimeter(component_footprint)
  target_area_limit = target_area + target_perimeter

  component_heights = heightmap[component_mask & is.finite(heightmap)]
  start_level = if (length(component_heights)) {
    stats::median(component_heights)
  } else {
    fallback_level
  }
  if (!is.finite(start_level)) {
    start_level = fallback_level
  }

  finite_height = heightmap[is.finite(heightmap)]
  if (!length(finite_height)) {
    return(NULL)
  }
  level_min = min(finite_height, na.rm = TRUE)
  level_max = max(finite_height, na.rm = TRUE)
  if (!is.finite(level_min) || !is.finite(level_max)) {
    return(NULL)
  }
  start_level = min(max(start_level, level_min), level_max)

  evaluation_cache = new.env(parent = emptyenv())
  evaluate_level = function(level) {
    spatial_water_component_area_fit_at_level(
      heightmap = heightmap,
      component_footprint = component_footprint,
      level = level,
      target_area = target_area,
      target_area_limit = target_area_limit,
      cache = evaluation_cache
    )
  }

  best = NULL
  update_best = function(candidate) {
    if (spatial_water_sfc_is_empty(candidate$polygon)) {
      return(best)
    }
    if (
      is.null(best) ||
        candidate$difference < best$difference - sqrt(.Machine$double.eps)
    ) {
      return(candidate)
    }
    best
  }

  start_result = evaluate_level(start_level)
  best = update_best(start_result)

  candidate_levels = c(
    start_level,
    if (
      is.finite(fallback_level) &&
        fallback_level >= level_min &&
        fallback_level <= level_max
    ) {
      fallback_level
    },
    seq(level_min, level_max, length.out = 65)
  )
  candidate_levels = sort(unique(candidate_levels[is.finite(candidate_levels)]))
  candidate_levels = candidate_levels[
    candidate_levels >= level_min & candidate_levels <= level_max
  ]

  for (candidate_level in candidate_levels) {
    best = update_best(evaluate_level(candidate_level))
  }

  if (!is.null(best) && length(candidate_levels) > 1L) {
    best_index = which.min(abs(candidate_levels - best$level))
    lower = candidate_levels[max(1L, best_index - 1L)]
    upper = candidate_levels[min(length(candidate_levels), best_index + 1L)]
    if (is.finite(lower) && is.finite(upper) && upper > lower) {
      optimum = tryCatch(
        stats::optimize(
          f = function(level) evaluate_level(level)$difference,
          interval = c(lower, upper)
        ),
        error = function(e) NULL
      )
      if (!is.null(optimum)) {
        best = update_best(evaluate_level(optimum$minimum))
      }
    }
  }
  best
}

#'@keywords internal
spatial_water_component_area_fit_at_level = function(
  heightmap,
  component_footprint,
  level,
  target_area,
  target_area_limit,
  cache
) {
  cache_key = format(level, digits = 17)
  if (exists(cache_key, envir = cache, inherits = FALSE)) {
    return(get(cache_key, envir = cache, inherits = FALSE))
  }

  terrain_band = make_spatial_water_level_polygon(
    heightmap = heightmap,
    water_level = level
  )
  water_polygon = if (spatial_water_sfc_is_empty(terrain_band)) {
    sf::st_sfc()
  } else {
    select_spatial_water_component_polygons(
      terrain_band = terrain_band,
      component_footprint = component_footprint
    )
  }
  polygon_area = spatial_water_polygon_area(water_polygon)
  if (
    is.finite(target_area_limit) &&
      polygon_area > target_area_limit + sqrt(.Machine$double.eps)
  ) {
    result = list(
      level = level,
      polygon = sf::st_sfc(),
      area = polygon_area,
      difference = Inf,
      rejected = TRUE
    )
    assign(cache_key, result, envir = cache)
    return(result)
  }
  result = list(
    level = level,
    polygon = water_polygon,
    area = polygon_area,
    difference = abs(polygon_area - target_area),
    rejected = FALSE
  )
  assign(cache_key, result, envir = cache)
  result
}

#'@keywords internal
spatial_water_polygon_area = function(geometry) {
  if (spatial_water_sfc_is_empty(geometry)) {
    return(0)
  }
  polygon_area = suppressWarnings(as.numeric(sf::st_area(geometry)))
  polygon_area = polygon_area[is.finite(polygon_area)]
  if (!length(polygon_area)) {
    return(0)
  }
  sum(polygon_area)
}

#'@keywords internal
spatial_water_polygon_perimeter = function(geometry) {
  if (spatial_water_sfc_is_empty(geometry)) {
    return(0)
  }
  polygon_perimeter = suppressWarnings(
    as.numeric(sf::st_length(sf::st_boundary(geometry)))
  )
  polygon_perimeter = polygon_perimeter[is.finite(polygon_perimeter)]
  if (!length(polygon_perimeter)) {
    return(0)
  }
  sum(polygon_perimeter)
}

#'@keywords internal
spatial_water_component_edge_heights = function(component_mask, heightmap) {
  component_cells = which(component_mask, arr.ind = TRUE)
  if (!nrow(component_cells)) {
    return(numeric())
  }

  nr = nrow(component_mask)
  nc = ncol(component_mask)
  edge_sample = rep(FALSE, nrow(component_cells))
  row_offset = c(-1L, 1L, 0L, 0L)
  col_offset = c(0L, 0L, -1L, 1L)
  for (cell_index in seq_len(nrow(component_cells))) {
    row_index = component_cells[cell_index, 1L]
    col_index = component_cells[cell_index, 2L]
    for (neighbor_index in seq_along(row_offset)) {
      neighbor_row = row_index + row_offset[neighbor_index]
      neighbor_col = col_index + col_offset[neighbor_index]
      if (
        neighbor_row < 1L ||
          neighbor_row > nr ||
          neighbor_col < 1L ||
          neighbor_col > nc
      ) {
        next
      }
      if (!is.finite(heightmap[neighbor_row, neighbor_col])) {
        next
      }
      if (!component_mask[neighbor_row, neighbor_col]) {
        edge_sample[cell_index] = TRUE
        break
      }
    }
  }
  if (!any(edge_sample)) {
    return(numeric())
  }

  edge_cells = component_cells[edge_sample, , drop = FALSE]
  edge_height = heightmap[edge_cells]
  edge_height = edge_height[is.finite(edge_height)]
  if (!length(edge_height)) {
    return(numeric())
  }
  edge_height
}

#'@keywords internal
mean_spatial_water_component_edge_height = function(component_mask, heightmap) {
  edge_height = spatial_water_component_edge_heights(
    component_mask = component_mask,
    heightmap = heightmap
  )
  if (!length(edge_height)) {
    return(NA_real_)
  }
  mean(edge_height)
}

#'@keywords internal
make_spatial_water_component_footprint = function(component_mask) {
  component_cells = which(component_mask, arr.ind = TRUE)
  if (!nrow(component_cells)) {
    return(sf::st_sfc())
  }

  nr = nrow(component_mask)
  nc = ncol(component_mask)
  row_index = component_cells[, 1L]
  col_index = component_cells[, 2L]
  x_scene_min = -(nr - 1) / 2
  x_scene_max = (nr - 1) / 2
  z_scene_min = -(nc - 1) / 2
  z_scene_max = (nc - 1) / 2
  x_center = row_index - 1 - (nr - 1) / 2
  z_center = col_index - 1 - (nc - 1) / 2
  x0 = pmax(x_center - 0.5, x_scene_min)
  x1 = pmin(x_center + 0.5, x_scene_max)
  z0 = pmax(z_center - 0.5, z_scene_min)
  z1 = pmin(z_center + 0.5, z_scene_max)

  renderable = x1 > x0 & z1 > z0
  if (!any(renderable)) {
    return(sf::st_sfc())
  }
  x0 = x0[renderable]
  x1 = x1[renderable]
  z0 = z0[renderable]
  z1 = z1[renderable]

  rectangles = vector("list", length(x0))
  for (cell_index in seq_along(x0)) {
    rectangles[[cell_index]] = sf::st_polygon(list(rbind(
      c(x0[cell_index], z0[cell_index]),
      c(x1[cell_index], z0[cell_index]),
      c(x1[cell_index], z1[cell_index]),
      c(x0[cell_index], z1[cell_index]),
      c(x0[cell_index], z0[cell_index])
    )))
  }
  footprint = suppressWarnings(sf::st_union(sf::st_sfc(rectangles)))
  extract_spatial_water_polygon_sfc(footprint)
}

#'@keywords internal
select_spatial_water_component_polygons = function(
  terrain_band,
  component_footprint
) {
  terrain_sf = sf::st_sf(geometry = terrain_band)
  footprint_sf = sf::st_sf(geometry = component_footprint)
  intersects = lengths(
    suppressWarnings(sf::st_intersects(terrain_sf, footprint_sf))
  ) >
    0
  if (!any(intersects)) {
    return(sf::st_sfc())
  }
  extract_spatial_water_polygon_sfc(terrain_band[intersects])
}

#'@keywords internal
make_spatial_water_level_polygon = function(heightmap, water_level) {
  if (!any(is.finite(heightmap) & heightmap <= water_level)) {
    return(sf::st_sfc())
  }
  finite_height = heightmap[is.finite(heightmap)]
  max_height = max(finite_height, na.rm = TRUE)
  max_height_eps = sqrt(.Machine$double.eps) * max(1, abs(max_height))
  if (water_level >= max_height - max_height_eps) {
    return(make_spatial_water_component_footprint(is.finite(heightmap)))
  }
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  x = seq(-(nr - 1) / 2, (nr - 1) / 2, length.out = nr)
  z = seq(-(nc - 1) / 2, (nc - 1) / 2, length.out = nc)
  bands = isoband::isobands(
    x = x,
    y = z,
    z = t(heightmap),
    levels_low = -Inf,
    levels_high = water_level
  )
  polygon_sfg = isoband::iso_to_sfg(bands)[[1]]
  extract_spatial_water_polygon_sfc(sf::st_sfc(polygon_sfg))
}

#'@keywords internal
extract_spatial_water_polygon_sfc = function(geometry) {
  if (!length(geometry)) {
    return(sf::st_sfc())
  }
  polygon_geometry = suppressWarnings(
    sf::st_collection_extract(geometry, "POLYGON")
  )
  if (!length(polygon_geometry)) {
    return(sf::st_sfc())
  }
  polygon_geometry = suppressWarnings(sf::st_cast(polygon_geometry, "POLYGON"))
  if (!length(polygon_geometry)) {
    return(sf::st_sfc())
  }
  polygon_geometry = polygon_geometry[!sf::st_is_empty(polygon_geometry)]
  if (!length(polygon_geometry)) {
    return(sf::st_sfc())
  }
  polygon_area = suppressWarnings(as.numeric(sf::st_area(polygon_geometry)))
  polygon_geometry = polygon_geometry[
    is.finite(polygon_area) & polygon_area > sqrt(.Machine$double.eps)
  ]
  if (!length(polygon_geometry)) {
    return(sf::st_sfc())
  }
  polygon_geometry
}

#'@keywords internal
spatial_water_sfc_is_empty = function(geometry) {
  !length(geometry) || all(sf::st_is_empty(geometry))
}

#'@keywords internal
triangulate_spatial_water_polygon_sfc = function(water_polygon, water_level) {
  polygon_vertices = list()
  vertex_index = 0L
  for (geometry_index in seq_along(water_polygon)) {
    polygons = spatial_water_sfg_polygons(water_polygon[[geometry_index]])
    for (polygon in polygons) {
      rings = lapply(polygon, clean_spatial_water_polygon_ring)
      ring_lengths = vapply(rings, nrow, integer(1))
      rings = rings[ring_lengths >= 3L]
      if (!length(rings)) {
        next
      }
      ring_lengths = vapply(rings, nrow, integer(1))
      xy = do.call(rbind, rings)
      holes = if (length(rings) > 1L) {
        cumsum(ring_lengths)[-length(ring_lengths)] + 1L
      } else {
        0
      }
      indices = tryCatch(
        decido::earcut(xy, holes = holes),
        error = function(e) integer()
      )
      if (length(indices) < 3L) {
        next
      }
      triangle_xy = xy[as.integer(indices), , drop = FALSE]
      vertex_index = vertex_index + 1L
      polygon_vertices[[vertex_index]] = cbind(
        triangle_xy[, 1],
        water_level,
        triangle_xy[, 2]
      )
    }
  }
  if (!length(polygon_vertices)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, polygon_vertices)
}

#'@keywords internal
make_spatial_water_polygon_lines = function(water_polygon, water_level) {
  line_vertices = list()
  line_index = 0L
  for (geometry_index in seq_along(water_polygon)) {
    polygons = spatial_water_sfg_polygons(water_polygon[[geometry_index]])
    for (polygon in polygons) {
      for (ring in polygon) {
        ring = close_spatial_water_polygon_ring(ring)
        if (nrow(ring) < 2L) {
          next
        }
        segment_start = seq_len(nrow(ring) - 1L)
        segment_end = segment_start + 1L
        line_index = line_index + 1L
        line_vertices[[line_index]] = rbind(
          cbind(ring[segment_start, 1], water_level, ring[segment_start, 2]),
          cbind(ring[segment_end, 1], water_level, ring[segment_end, 2])
        )[
          as.vector(rbind(
            seq_along(segment_start),
            seq_along(segment_end) +
              length(segment_start)
          )),
          ,
          drop = FALSE
        ]
      }
    }
  }
  if (!length(line_vertices)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, line_vertices)
}

#'@keywords internal
make_spatial_water_polygon_sidewalls = function(
  water_polygon,
  heightmap,
  water_level
) {
  polygon_union = sf::st_sf(
    geometry = suppressWarnings(sf::st_union(
      water_polygon
    ))
  )
  side_vertices = list()
  side_index = 0L
  eps = 1e-6
  for (geometry_index in seq_along(water_polygon)) {
    polygons = spatial_water_sfg_polygons(water_polygon[[geometry_index]])
    for (polygon in polygons) {
      for (ring in polygon) {
        ring = close_spatial_water_polygon_ring(ring)
        if (nrow(ring) < 2L) {
          next
        }
        for (point_index in seq_len(nrow(ring) - 1L)) {
          segment = spatial_water_polygon_sidewall_segment(
            x_start = ring[point_index, 1],
            z_start = ring[point_index, 2],
            x_end = ring[point_index + 1L, 1],
            z_end = ring[point_index + 1L, 2],
            water_polygon = polygon_union,
            heightmap = heightmap,
            water_level = water_level,
            eps = eps
          )
          if (!nrow(segment)) {
            next
          }
          side_index = side_index + 1L
          side_vertices[[side_index]] = segment
        }
      }
    }
  }
  if (!length(side_vertices)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, side_vertices)
}

#'@keywords internal
spatial_water_polygon_sidewall_segment = function(
  x_start,
  z_start,
  x_end,
  z_end,
  water_polygon,
  heightmap,
  water_level,
  eps = 1e-6
) {
  delta_x = x_end - x_start
  delta_z = z_end - z_start
  segment_length = sqrt(delta_x^2 + delta_z^2)
  if (
    !is.finite(segment_length) || segment_length <= sqrt(.Machine$double.eps)
  ) {
    return(matrix(nrow = 0, ncol = 3))
  }
  normal_one = c(-delta_z, delta_x) / segment_length
  normal_two = -normal_one
  midpoint_x = (x_start + x_end) / 2
  midpoint_z = (z_start + z_end) / 2
  inside_one = spatial_water_point_inside_polygon(
    x = midpoint_x + normal_one[1] * eps,
    z = midpoint_z + normal_one[2] * eps,
    water_polygon = water_polygon
  )
  inside_two = spatial_water_point_inside_polygon(
    x = midpoint_x + normal_two[1] * eps,
    z = midpoint_z + normal_two[2] * eps,
    water_polygon = water_polygon
  )
  if (isTRUE(inside_one) && !isTRUE(inside_two)) {
    inward_normal = normal_one
    outward_normal = normal_two
  } else if (isTRUE(inside_two) && !isTRUE(inside_one)) {
    inward_normal = normal_two
    outward_normal = normal_one
  } else {
    return(matrix(nrow = 0, ncol = 3))
  }

  outside_supported = spatial_water_point_has_finite_heightmap_cell(
    heightmap = heightmap,
    x = midpoint_x + outward_normal[1] * eps,
    z = midpoint_z + outward_normal[2] * eps
  )
  if (isTRUE(outside_supported)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  make_spatial_water_polygon_sidewall_vertices(
    heightmap = heightmap,
    x_start = x_start,
    z_start = z_start,
    x_end = x_end,
    z_end = z_end,
    water_level = water_level,
    inward_normal = inward_normal,
    eps = eps
  )
}

#'@keywords internal
spatial_water_point_inside_polygon = function(x, z, water_polygon) {
  point = sf::st_sfc(sf::st_point(c(x, z)))
  any(sf::st_intersects(point, water_polygon, sparse = FALSE))
}

#'@keywords internal
make_spatial_water_polygon_sidewall_vertices = function(
  heightmap,
  x_start,
  z_start,
  x_end,
  z_end,
  water_level,
  inward_normal,
  eps = 1e-6
) {
  breakpoints = spatial_water_side_segment_breakpoints(
    heightmap = heightmap,
    x_start = x_start,
    z_start = z_start,
    x_end = x_end,
    z_end = z_end
  )
  segment_x = x_start + (x_end - x_start) * breakpoints
  segment_z = z_start + (z_end - z_start) * breakpoints
  segment_terrain = interpolate_spatial_water_height(
    heightmap = heightmap,
    x = segment_x + inward_normal[1] * eps,
    z = segment_z + inward_normal[2] * eps
  )
  segment_bottom = pmin(segment_terrain, water_level)
  keep_points = simplify_spatial_water_sidewall_points(
    segment_x,
    segment_z,
    segment_bottom
  )
  segment_x = segment_x[keep_points]
  segment_z = segment_z[keep_points]
  segment_bottom = segment_bottom[keep_points]
  renderable = is.finite(segment_bottom[-length(segment_bottom)]) &
    is.finite(segment_bottom[-1]) &
    (water_level > segment_bottom[-length(segment_bottom)] |
      water_level > segment_bottom[-1])
  if (!any(renderable)) {
    return(matrix(nrow = 0, ncol = 3))
  }

  start_index = which(renderable)
  end_index = start_index + 1L
  n_edges = length(start_index)
  vertices = matrix(NA_real_, nrow = n_edges * 6L, ncol = 3L)
  first_vertex = seq.int(1L, nrow(vertices), by = 6L)
  vertices[first_vertex, ] = cbind(
    segment_x[start_index],
    water_level,
    segment_z[start_index]
  )
  vertices[first_vertex + 1L, ] = cbind(
    segment_x[end_index],
    water_level,
    segment_z[end_index]
  )
  vertices[first_vertex + 2L, ] = cbind(
    segment_x[start_index],
    segment_bottom[start_index],
    segment_z[start_index]
  )
  vertices[first_vertex + 3L, ] = cbind(
    segment_x[end_index],
    water_level,
    segment_z[end_index]
  )
  vertices[first_vertex + 4L, ] = cbind(
    segment_x[end_index],
    segment_bottom[end_index],
    segment_z[end_index]
  )
  vertices[first_vertex + 5L, ] = cbind(
    segment_x[start_index],
    segment_bottom[start_index],
    segment_z[start_index]
  )
  vertices
}

#'@keywords internal
spatial_water_sfg_polygons = function(geometry) {
  if (inherits(geometry, "POLYGON")) {
    return(list(lapply(seq_along(geometry), function(index) geometry[[index]])))
  }
  if (inherits(geometry, "MULTIPOLYGON")) {
    return(lapply(geometry, function(polygon) {
      lapply(seq_along(polygon), function(index) polygon[[index]])
    }))
  }
  list()
}

#'@keywords internal
clean_spatial_water_polygon_ring = function(ring) {
  ring = as.matrix(ring[, 1:2, drop = FALSE])
  ring = ring[is.finite(ring[, 1]) & is.finite(ring[, 2]), , drop = FALSE]
  if (nrow(ring) < 2L) {
    return(matrix(nrow = 0, ncol = 2))
  }
  duplicate_previous = c(
    FALSE,
    rowSums(abs(diff(ring))) <=
      sqrt(
        .Machine$double.eps
      )
  )
  ring = ring[!duplicate_previous, , drop = FALSE]
  if (
    nrow(ring) > 1L &&
      all(abs(ring[1L, ] - ring[nrow(ring), ]) <= sqrt(.Machine$double.eps))
  ) {
    ring = ring[-nrow(ring), , drop = FALSE]
  }
  if (nrow(ring) < 3L) {
    return(matrix(nrow = 0, ncol = 2))
  }
  ring
}

#'@keywords internal
close_spatial_water_polygon_ring = function(ring) {
  ring = clean_spatial_water_polygon_ring(ring)
  if (!nrow(ring)) {
    return(ring)
  }
  rbind(ring, ring[1L, , drop = FALSE])
}

#'@keywords internal
adjust_spatial_water_surface_to_edge_terrain = function(
  water_surface,
  heightmap = NULL,
  water_edge_extension = 0.5
) {
  if (is.null(heightmap) || !is.matrix(heightmap)) {
    return(water_surface)
  }
  component_labels = label_spatial_water_components(is.finite(water_surface))
  component_count = max(component_labels)
  if (!component_count) {
    return(water_surface)
  }

  adjusted_surface = water_surface
  for (component_id in seq_len(component_count)) {
    component_mask = component_labels == component_id
    component_level = max(water_surface[component_mask], na.rm = TRUE)
    if (!is.finite(component_level)) {
      next
    }
    adjusted_level = component_level
    for (iteration in seq_len(20L)) {
      component_surface = matrix(
        NA_real_,
        nrow = nrow(water_surface),
        ncol = ncol(water_surface)
      )
      component_surface[component_mask] = adjusted_level
      geometry = make_spatial_water_cell_geometry(
        water_surface = component_surface,
        heightmap = heightmap,
        water_edge_extension = water_edge_extension
      )
      if (!length(geometry$water_height)) {
        break
      }
      side_vertices = make_spatial_water_edge_sides(
        heightmap = heightmap,
        water_height = geometry$water_height,
        x0 = geometry$x0,
        x1 = geometry$x1,
        z0 = geometry$z0,
        z1 = geometry$z1,
        left_edge = geometry$left_side,
        right_edge = geometry$right_side,
        bottom_edge = geometry$bottom_side,
        top_edge = geometry$top_side,
        row_index = geometry$row_index,
        col_index = geometry$col_index,
        nr = geometry$nr,
        nc = geometry$nc
      )
      edge_adjustment = max_spatial_water_finite_sidewall_height(
        side_vertices = side_vertices,
        heightmap = heightmap
      )
      if (
        !is.finite(edge_adjustment) ||
          edge_adjustment <= sqrt(.Machine$double.eps)
      ) {
        break
      }
      next_level = adjusted_level - edge_adjustment
      if (
        !is.finite(next_level) ||
          abs(next_level - adjusted_level) <= sqrt(.Machine$double.eps)
      ) {
        break
      }
      adjusted_level = next_level
    }
    adjusted_surface[component_mask] = adjusted_level
  }
  adjusted_surface
}

#'@keywords internal
max_spatial_water_finite_sidewall_height = function(side_vertices, heightmap) {
  if (!nrow(side_vertices)) {
    return(NA_real_)
  }
  finite_sidewall = spatial_water_sidewall_faces_finite_terrain(
    side_vertices = side_vertices,
    heightmap = heightmap
  )
  if (!any(finite_sidewall)) {
    return(NA_real_)
  }
  first_vertex = seq.int(1L, nrow(side_vertices), by = 6L)
  finite_rows = as.vector(vapply(
    first_vertex[finite_sidewall],
    function(first_row) seq.int(first_row, first_row + 5L),
    integer(6)
  ))
  max_spatial_water_sidewall_height(side_vertices[finite_rows, , drop = FALSE])
}

#'@keywords internal
max_spatial_water_sidewall_height = function(side_vertices) {
  if (!nrow(side_vertices)) {
    return(NA_real_)
  }
  first_vertex = seq.int(1L, nrow(side_vertices), by = 6L)
  top_height = side_vertices[first_vertex, 2]
  bottom_height = c(
    side_vertices[first_vertex + 2L, 2],
    side_vertices[first_vertex + 4L, 2]
  )
  side_height = rep(top_height, 2L) - bottom_height
  side_height = side_height[is.finite(side_height)]
  if (!length(side_height)) {
    return(NA_real_)
  }
  max(side_height)
}

#'@keywords internal
spatial_water_sidewall_faces_finite_terrain = function(
  side_vertices,
  heightmap
) {
  if (!nrow(side_vertices)) {
    return(logical())
  }
  first_vertex = seq.int(1L, nrow(side_vertices), by = 6L)
  finite_edge = rep(FALSE, length(first_vertex))
  eps = 1e-6
  for (side_index in seq_along(first_vertex)) {
    rows = seq.int(first_vertex[side_index], first_vertex[side_index] + 5L)
    sidewall = side_vertices[rows, , drop = FALSE]
    first_edge = sidewall[2L, ] - sidewall[1L, ]
    second_edge = sidewall[3L, ] - sidewall[1L, ]
    normal = c(
      first_edge[2L] * second_edge[3L] - first_edge[3L] * second_edge[2L],
      first_edge[3L] * second_edge[1L] - first_edge[1L] * second_edge[3L],
      first_edge[1L] * second_edge[2L] - first_edge[2L] * second_edge[1L]
    )
    horizontal_normal = c(normal[1L], normal[3L])
    normal_length = sqrt(sum(horizontal_normal^2))
    if (
      !is.finite(normal_length) || normal_length <= sqrt(.Machine$double.eps)
    ) {
      next
    }
    outward_normal = -horizontal_normal / normal_length
    sample_x = mean(sidewall[c(1L, 2L), 1L]) + outward_normal[1L] * eps
    sample_z = mean(sidewall[c(1L, 2L), 3L]) + outward_normal[2L] * eps
    finite_edge[side_index] = is.finite(
      interpolate_spatial_water_surface_height_unclamped(
        heightmap = heightmap,
        x = sample_x,
        z = sample_z
      )
    )
  }
  finite_edge
}

#'@keywords internal
label_spatial_water_components = function(water_mask) {
  nr = nrow(water_mask)
  nc = ncol(water_mask)
  component_labels = matrix(0L, nrow = nr, ncol = nc)
  component_id = 0L
  max_queue = sum(water_mask)
  if (!max_queue) {
    return(component_labels)
  }

  water_cells = which(water_mask, arr.ind = TRUE)
  queue_row = integer(max_queue)
  queue_col = integer(max_queue)
  for (cell_index in seq_len(nrow(water_cells))) {
    start_row = water_cells[cell_index, 1]
    start_col = water_cells[cell_index, 2]
    if (component_labels[start_row, start_col] != 0L) {
      next
    }

    component_id = component_id + 1L
    queue_start = 1L
    queue_end = 1L
    queue_row[queue_end] = start_row
    queue_col[queue_end] = start_col
    component_labels[start_row, start_col] = component_id

    while (queue_start <= queue_end) {
      row = queue_row[queue_start]
      col = queue_col[queue_start]
      queue_start = queue_start + 1L

      if (
        row > 1L &&
          water_mask[row - 1L, col] &&
          component_labels[row - 1L, col] == 0L
      ) {
        queue_end = queue_end + 1L
        queue_row[queue_end] = row - 1L
        queue_col[queue_end] = col
        component_labels[row - 1L, col] = component_id
      }
      if (
        row < nr &&
          water_mask[row + 1L, col] &&
          component_labels[row + 1L, col] == 0L
      ) {
        queue_end = queue_end + 1L
        queue_row[queue_end] = row + 1L
        queue_col[queue_end] = col
        component_labels[row + 1L, col] = component_id
      }
      if (
        col > 1L &&
          water_mask[row, col - 1L] &&
          component_labels[row, col - 1L] == 0L
      ) {
        queue_end = queue_end + 1L
        queue_row[queue_end] = row
        queue_col[queue_end] = col - 1L
        component_labels[row, col - 1L] = component_id
      }
      if (
        col < nc &&
          water_mask[row, col + 1L] &&
          component_labels[row, col + 1L] == 0L
      ) {
        queue_end = queue_end + 1L
        queue_row[queue_end] = row
        queue_col[queue_end] = col + 1L
        component_labels[row, col + 1L] = component_id
      }
    }
  }
  component_labels
}

#'@keywords internal
make_spatial_water_top_vertices = function(
  heightmap = NULL,
  x0,
  x1,
  z0,
  z1,
  water_height
) {
  if (
    is.null(heightmap) ||
      !is.matrix(heightmap)
  ) {
    return(make_spatial_water_rect_top_vertices(x0, x1, z0, z1, water_height))
  }

  top_vertices = vector("list", length(water_height))
  for (cell_index in seq_along(water_height)) {
    if (
      spatial_water_rectangle_supported_below_water(
        heightmap = heightmap,
        x0 = x0[cell_index],
        x1 = x1[cell_index],
        z0 = z0[cell_index],
        z1 = z1[cell_index],
        water_height = water_height[cell_index]
      )
    ) {
      top_vertices[[cell_index]] = make_spatial_water_rect_top_vertices(
        x0 = x0[cell_index],
        x1 = x1[cell_index],
        z0 = z0[cell_index],
        z1 = z1[cell_index],
        water_height = water_height[cell_index]
      )
    } else {
      top_vertices[[cell_index]] = make_spatial_water_terrain_top_vertices(
        heightmap = heightmap,
        x0 = x0[cell_index],
        x1 = x1[cell_index],
        z0 = z0[cell_index],
        z1 = z1[cell_index],
        water_height = water_height[cell_index]
      )
    }
  }
  top_vertices = top_vertices[lengths(top_vertices) > 0]
  if (!length(top_vertices)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, top_vertices)
}

#'@keywords internal
make_spatial_water_rect_top_vertices = function(x0, x1, z0, z1, water_height) {
  n_cells = length(water_height)
  top_vertices = matrix(NA_real_, nrow = n_cells * 6L, ncol = 3L)
  first_vertex = seq.int(1L, nrow(top_vertices), by = 6L)
  top_vertices[first_vertex, ] = cbind(x0, water_height, z0)
  top_vertices[first_vertex + 1L, ] = cbind(x1, water_height, z0)
  top_vertices[first_vertex + 2L, ] = cbind(x0, water_height, z1)
  top_vertices[first_vertex + 3L, ] = cbind(x1, water_height, z0)
  top_vertices[first_vertex + 4L, ] = cbind(x1, water_height, z1)
  top_vertices[first_vertex + 5L, ] = cbind(x0, water_height, z1)
  top_vertices
}

#'@keywords internal
spatial_water_rectangle_supported_below_water = function(
  heightmap,
  x0,
  x1,
  z0,
  z1,
  water_height
) {
  terrain_cells = spatial_water_overlapping_terrain_cells(
    heightmap,
    x0,
    x1,
    z0,
    z1
  )
  if (!length(terrain_cells$row) || !length(terrain_cells$col)) {
    return(FALSE)
  }
  row_index = rep(terrain_cells$row, each = length(terrain_cells$col))
  col_index = rep(terrain_cells$col, times = length(terrain_cells$row))
  terrain_values = c(
    heightmap[cbind(row_index, col_index)],
    heightmap[cbind(row_index + 1L, col_index)],
    heightmap[cbind(row_index, col_index + 1L)],
    heightmap[cbind(row_index + 1L, col_index + 1L)]
  )
  all(is.finite(terrain_values)) &&
    all(terrain_values <= water_height + sqrt(.Machine$double.eps))
}

#'@keywords internal
spatial_water_overlapping_terrain_cells = function(heightmap, x0, x1, z0, z1) {
  row_col_min = spatial_water_row_col(heightmap, x0, z0)
  row_col_max = spatial_water_row_col(heightmap, x1, z1)
  eps = sqrt(.Machine$double.eps)
  row_min = min(row_col_min$row, row_col_max$row)
  row_max = max(row_col_min$row, row_col_max$row)
  col_min = min(row_col_min$col, row_col_max$col)
  col_max = max(row_col_min$col, row_col_max$col)
  row_start = max(1L, floor(row_min + eps))
  row_end = min(nrow(heightmap) - 1L, ceiling(row_max - eps) - 1L)
  col_start = max(1L, floor(col_min + eps))
  col_end = min(ncol(heightmap) - 1L, ceiling(col_max - eps) - 1L)
  if (row_start > row_end || col_start > col_end) {
    return(list(row = integer(), col = integer()))
  }
  list(
    row = seq.int(row_start, row_end),
    col = seq.int(col_start, col_end)
  )
}

#'@keywords internal
make_spatial_water_terrain_top_vertices = function(
  heightmap,
  x0,
  x1,
  z0,
  z1,
  water_height
) {
  terrain_cells = spatial_water_overlapping_terrain_cells(
    heightmap,
    x0,
    x1,
    z0,
    z1
  )
  if (!length(terrain_cells$row) || !length(terrain_cells$col)) {
    return(matrix(nrow = 0, ncol = 3))
  }

  nr = nrow(heightmap)
  nc = ncol(heightmap)
  terrain_triangles = vector(
    "list",
    length(terrain_cells$row) * length(terrain_cells$col) * 2L
  )
  triangle_index = 0L
  for (row_index in terrain_cells$row) {
    x_left = row_index - 1 - (nr - 1) / 2
    x_right = row_index - (nr - 1) / 2
    for (col_index in terrain_cells$col) {
      z_bottom = col_index - 1 - (nc - 1) / 2
      z_top = col_index - (nc - 1) / 2
      height00 = heightmap[row_index, col_index]
      height10 = heightmap[row_index + 1L, col_index]
      height01 = heightmap[row_index, col_index + 1L]
      height11 = heightmap[row_index + 1L, col_index + 1L]
      if (!all(is.finite(c(height00, height10, height01, height11)))) {
        next
      }

      terrain_triangle_top = matrix(
        c(
          x_left,
          height00,
          z_bottom,
          x_right,
          height10,
          z_bottom,
          x_left,
          height01,
          z_top
        ),
        ncol = 3,
        byrow = TRUE
      )
      terrain_triangle_bottom = matrix(
        c(
          x_right,
          height10,
          z_bottom,
          x_right,
          height11,
          z_top,
          x_left,
          height01,
          z_top
        ),
        ncol = 3,
        byrow = TRUE
      )
      for (terrain_triangle in list(
        terrain_triangle_top,
        terrain_triangle_bottom
      )) {
        clipped_triangle = clip_spatial_water_polygon_to_bounds(
          polygon = terrain_triangle,
          x0 = x0,
          x1 = x1,
          z0 = z0,
          z1 = z1
        )
        clipped_triangle = clip_spatial_water_polygon_to_water_height(
          polygon = clipped_triangle,
          water_height = water_height
        )
        clipped_triangle = clean_spatial_water_polygon(clipped_triangle)
        if (nrow(clipped_triangle) < 3L) {
          next
        }
        clipped_triangle[, 2] = water_height
        triangle_index = triangle_index + 1L
        terrain_triangles[[triangle_index]] =
          triangulate_spatial_water_polygon(clipped_triangle)
      }
    }
  }
  terrain_triangles = terrain_triangles[lengths(terrain_triangles) > 0]
  if (!length(terrain_triangles)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, terrain_triangles)
}

#'@keywords internal
clip_spatial_water_polygon_to_bounds = function(polygon, x0, x1, z0, z1) {
  polygon = clip_spatial_water_polygon_to_axis(
    polygon = polygon,
    axis = 1L,
    limit = x0,
    keep_greater = TRUE
  )
  polygon = clip_spatial_water_polygon_to_axis(
    polygon = polygon,
    axis = 1L,
    limit = x1,
    keep_greater = FALSE
  )
  polygon = clip_spatial_water_polygon_to_axis(
    polygon = polygon,
    axis = 3L,
    limit = z0,
    keep_greater = TRUE
  )
  clip_spatial_water_polygon_to_axis(
    polygon = polygon,
    axis = 3L,
    limit = z1,
    keep_greater = FALSE
  )
}

#'@keywords internal
clip_spatial_water_polygon_to_water_height = function(polygon, water_height) {
  clip_spatial_water_polygon_to_axis(
    polygon = polygon,
    axis = 2L,
    limit = water_height,
    keep_greater = FALSE
  )
}

#'@keywords internal
clip_spatial_water_polygon_to_axis = function(
  polygon,
  axis,
  limit,
  keep_greater = TRUE
) {
  if (nrow(polygon) < 3L) {
    return(matrix(nrow = 0, ncol = 3))
  }
  eps = sqrt(.Machine$double.eps)
  inside = if (isTRUE(keep_greater)) {
    polygon[, axis] >= limit - eps
  } else {
    polygon[, axis] <= limit + eps
  }
  if (all(inside)) {
    return(polygon)
  }
  if (!any(inside)) {
    return(matrix(nrow = 0, ncol = 3))
  }

  clipped_polygon = matrix(nrow = 0, ncol = 3)
  for (point_index in seq_len(nrow(polygon))) {
    next_index = if (point_index == nrow(polygon)) 1L else point_index + 1L
    current_point = polygon[point_index, ]
    next_point = polygon[next_index, ]
    current_inside = inside[point_index]
    next_inside = inside[next_index]

    if (current_inside && next_inside) {
      clipped_polygon = rbind(clipped_polygon, next_point)
    } else if (current_inside && !next_inside) {
      clipped_polygon = rbind(
        clipped_polygon,
        spatial_water_axis_intersection_point(
          current_point,
          next_point,
          axis,
          limit
        )
      )
    } else if (!current_inside && next_inside) {
      clipped_polygon = rbind(
        clipped_polygon,
        spatial_water_axis_intersection_point(
          current_point,
          next_point,
          axis,
          limit
        ),
        next_point
      )
    }
  }
  clean_spatial_water_polygon(clipped_polygon)
}

#'@keywords internal
spatial_water_axis_intersection_point = function(
  current_point,
  next_point,
  axis,
  limit
) {
  axis_delta = next_point[axis] - current_point[axis]
  if (abs(axis_delta) <= sqrt(.Machine$double.eps)) {
    return(current_point)
  }
  current_point +
    (next_point - current_point) *
      ((limit - current_point[axis]) / axis_delta)
}

#'@keywords internal
clip_spatial_water_top_to_terrain = function(
  top_vertices,
  heightmap = NULL
) {
  if (is.null(heightmap) || !is.matrix(heightmap) || !nrow(top_vertices)) {
    return(top_vertices)
  }
  outside_terrain = !spatial_water_point_not_above_terrain(
    top_vertices,
    heightmap
  )
  if (!any(outside_terrain)) {
    return(top_vertices)
  }

  clipped_triangles = vector("list", nrow(top_vertices) / 3)
  for (triangle_index in seq_along(clipped_triangles)) {
    triangle_rows = seq.int(3 * triangle_index - 2L, 3 * triangle_index)
    clipped_triangles[[
      triangle_index
    ]] = clip_spatial_water_triangle_to_terrain(
      triangle = top_vertices[triangle_rows, , drop = FALSE],
      heightmap = heightmap
    )
  }
  clipped_triangles = clipped_triangles[
    lengths(clipped_triangles) > 0
  ]
  if (!length(clipped_triangles)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, clipped_triangles)
}

#'@keywords internal
clip_spatial_water_triangle_to_terrain = function(
  triangle,
  heightmap
) {
  inside = spatial_water_point_not_above_terrain(triangle, heightmap)
  if (all(inside)) {
    return(triangle)
  }
  if (!any(inside)) {
    return(matrix(nrow = 0, ncol = 3))
  }

  clipped_polygon = matrix(nrow = 0, ncol = 3)
  for (point_index in seq_len(nrow(triangle))) {
    next_index = if (point_index == nrow(triangle)) 1L else point_index + 1L
    current_point = triangle[point_index, ]
    next_point = triangle[next_index, ]
    current_inside = inside[point_index]
    next_inside = inside[next_index]

    if (current_inside && next_inside) {
      clipped_polygon = rbind(clipped_polygon, next_point)
    } else if (current_inside && !next_inside) {
      clipped_polygon = rbind(
        clipped_polygon,
        spatial_water_terrain_intersection_point(
          inside_point = current_point,
          outside_point = next_point,
          heightmap = heightmap
        )
      )
    } else if (!current_inside && next_inside) {
      clipped_polygon = rbind(
        clipped_polygon,
        spatial_water_terrain_intersection_point(
          inside_point = next_point,
          outside_point = current_point,
          heightmap = heightmap
        ),
        next_point
      )
    }
  }
  clipped_polygon = clean_spatial_water_polygon(clipped_polygon)
  if (nrow(clipped_polygon) < 3) {
    return(matrix(nrow = 0, ncol = 3))
  }
  triangulate_spatial_water_polygon(clipped_polygon)
}

#'@keywords internal
spatial_water_point_not_above_terrain = function(points, heightmap) {
  terrain_height = interpolate_spatial_water_surface_height(
    heightmap,
    points[, 1],
    points[, 3]
  )
  is.finite(terrain_height) &
    points[, 2] >= terrain_height - sqrt(.Machine$double.eps)
}

#'@keywords internal
spatial_water_terrain_intersection_point = function(
  inside_point,
  outside_point,
  heightmap
) {
  lower = inside_point
  upper = outside_point
  for (iteration in seq_len(40)) {
    midpoint = (lower + upper) / 2
    if (
      spatial_water_point_not_above_terrain(
        matrix(midpoint, nrow = 1),
        heightmap
      )
    ) {
      lower = midpoint
    } else {
      upper = midpoint
    }
  }
  lower
}

#'@keywords internal
clean_spatial_water_polygon = function(polygon) {
  if (!nrow(polygon)) {
    return(polygon)
  }
  keep = rep(TRUE, nrow(polygon))
  eps = sqrt(.Machine$double.eps)
  for (point_index in seq_len(nrow(polygon))) {
    previous_index = if (point_index == 1L) nrow(polygon) else point_index - 1L
    keep[point_index] = any(
      abs(
        polygon[point_index, ] -
          polygon[previous_index, ]
      ) >
        eps
    )
  }
  polygon[keep, , drop = FALSE]
}

#'@keywords internal
triangulate_spatial_water_polygon = function(polygon) {
  triangle_count = nrow(polygon) - 2L
  triangles = matrix(NA_real_, nrow = triangle_count * 3L, ncol = 3L)
  for (triangle_index in seq_len(triangle_count)) {
    triangle_rows = seq.int(3 * triangle_index - 2L, 3 * triangle_index)
    triangles[triangle_rows, ] = rbind(
      polygon[1, ],
      polygon[triangle_index + 1L, ],
      polygon[triangle_index + 2L, ]
    )
  }
  triangles
}

#'@keywords internal
clip_spatial_water_side_segments_to_terrain = function(
  heightmap,
  x_start,
  z_start,
  x_end,
  z_end,
  water_height
) {
  if (!length(water_height)) {
    return(list(
      x_start = numeric(),
      z_start = numeric(),
      x_end = numeric(),
      z_end = numeric(),
      water_height = numeric()
    ))
  }
  terrain_start = interpolate_spatial_water_surface_height(
    heightmap,
    x_start,
    z_start
  )
  terrain_end = interpolate_spatial_water_surface_height(
    heightmap,
    x_end,
    z_end
  )
  eps = sqrt(.Machine$double.eps)
  start_inside = is.finite(terrain_start) & terrain_start <= water_height + eps
  end_inside = is.finite(terrain_end) & terrain_end <= water_height + eps
  keep = start_inside | end_inside
  if (!any(keep)) {
    return(list(
      x_start = numeric(),
      z_start = numeric(),
      x_end = numeric(),
      z_end = numeric(),
      water_height = numeric()
    ))
  }

  x_start = x_start[keep]
  z_start = z_start[keep]
  x_end = x_end[keep]
  z_end = z_end[keep]
  water_height = water_height[keep]
  start_inside = start_inside[keep]
  end_inside = end_inside[keep]

  for (segment_index in seq_along(water_height)) {
    if (start_inside[segment_index] && end_inside[segment_index]) {
      next
    }
    start_point = c(
      x_start[segment_index],
      water_height[segment_index],
      z_start[segment_index]
    )
    end_point = c(
      x_end[segment_index],
      water_height[segment_index],
      z_end[segment_index]
    )
    if (start_inside[segment_index]) {
      intersection = spatial_water_terrain_intersection_point(
        inside_point = start_point,
        outside_point = end_point,
        heightmap = heightmap
      )
      x_end[segment_index] = intersection[1]
      z_end[segment_index] = intersection[3]
    } else {
      intersection = spatial_water_terrain_intersection_point(
        inside_point = end_point,
        outside_point = start_point,
        heightmap = heightmap
      )
      x_start[segment_index] = intersection[1]
      z_start[segment_index] = intersection[3]
    }
  }

  list(
    x_start = x_start,
    z_start = z_start,
    x_end = x_end,
    z_end = z_end,
    water_height = water_height
  )
}

#'@keywords internal
extend_spatial_water_bounds_to_terrain = function(
  heightmap = NULL,
  water_height,
  x0,
  x1,
  z0,
  z1,
  left_edge,
  right_edge,
  bottom_edge,
  top_edge,
  water_edge_extension = 0.5,
  x_scene_min,
  x_scene_max,
  z_scene_min,
  z_scene_max
) {
  if (
    is.null(heightmap) ||
      !is.matrix(heightmap) ||
      water_edge_extension <= 0
  ) {
    return(list(
      x0 = pmax(x0 - ifelse(left_edge, water_edge_extension, 0), x_scene_min),
      x1 = pmin(x1 + ifelse(right_edge, water_edge_extension, 0), x_scene_max),
      z0 = pmax(z0 - ifelse(bottom_edge, water_edge_extension, 0), z_scene_min),
      z1 = pmin(z1 + ifelse(top_edge, water_edge_extension, 0), z_scene_max)
    ))
  }

  left_extension = spatial_water_side_extensions(
    heightmap = heightmap,
    water_height = water_height,
    edge_mask = left_edge,
    x_start = x0,
    z_start = z0,
    x_end = x0,
    z_end = z1,
    direction_x = -1,
    direction_z = 0,
    max_extension = pmin(water_edge_extension, x0 - x_scene_min)
  )
  right_extension = spatial_water_side_extensions(
    heightmap = heightmap,
    water_height = water_height,
    edge_mask = right_edge,
    x_start = x1,
    z_start = z1,
    x_end = x1,
    z_end = z0,
    direction_x = 1,
    direction_z = 0,
    max_extension = pmin(water_edge_extension, x_scene_max - x1)
  )
  bottom_extension = spatial_water_side_extensions(
    heightmap = heightmap,
    water_height = water_height,
    edge_mask = bottom_edge,
    x_start = x1,
    z_start = z0,
    x_end = x0,
    z_end = z0,
    direction_x = 0,
    direction_z = -1,
    max_extension = pmin(water_edge_extension, z0 - z_scene_min)
  )
  top_extension = spatial_water_side_extensions(
    heightmap = heightmap,
    water_height = water_height,
    edge_mask = top_edge,
    x_start = x0,
    z_start = z1,
    x_end = x1,
    z_end = z1,
    direction_x = 0,
    direction_z = 1,
    max_extension = pmin(water_edge_extension, z_scene_max - z1)
  )

  list(
    x0 = pmax(x0 - left_extension, x_scene_min),
    x1 = pmin(x1 + right_extension, x_scene_max),
    z0 = pmax(z0 - bottom_extension, z_scene_min),
    z1 = pmin(z1 + top_extension, z_scene_max)
  )
}

#'@keywords internal
spatial_water_side_extensions = function(
  heightmap,
  water_height,
  edge_mask,
  x_start,
  z_start,
  x_end,
  z_end,
  direction_x,
  direction_z,
  max_extension
) {
  extension = numeric(length(water_height))
  edge_index = which(edge_mask & max_extension > 0)
  if (!length(edge_index)) {
    return(extension)
  }
  sample_t = seq(0, 1, length.out = 5)
  extension[edge_index] = vapply(
    edge_index,
    function(index) {
      sample_x = x_start[index] +
        (x_end[index] - x_start[index]) * sample_t
      sample_z = z_start[index] +
        (z_end[index] - z_start[index]) * sample_t
      sample_extension = vapply(
        seq_along(sample_t),
        function(sample_index) {
          spatial_water_terrain_contact_distance(
            heightmap = heightmap,
            water_height = water_height[index],
            x = sample_x[sample_index],
            z = sample_z[sample_index],
            direction_x = direction_x,
            direction_z = direction_z,
            max_extension = max_extension[index]
          )
        },
        numeric(1)
      )
      max(sample_extension, na.rm = TRUE)
    },
    numeric(1)
  )
  extension
}

#'@keywords internal
spatial_water_terrain_contact_distance = function(
  heightmap,
  water_height,
  x,
  z,
  direction_x,
  direction_z,
  max_extension
) {
  if (!is.finite(max_extension) || max_extension <= 0) {
    return(0)
  }
  eps = sqrt(.Machine$double.eps)
  distance_grid = seq(0, max_extension, length.out = 9)
  terrain_height = interpolate_spatial_water_surface_height(
    heightmap,
    x + direction_x * distance_grid,
    z + direction_z * distance_grid
  )

  inside = is.finite(terrain_height) & terrain_height <= water_height + eps
  if (!inside[1]) {
    return(0)
  }
  contact_index = which(!inside)[1]
  if (is.na(contact_index)) {
    return(max_extension)
  }
  if (contact_index <= 1) {
    return(0)
  }

  lower = distance_grid[contact_index - 1]
  upper = distance_grid[contact_index]
  contact_is_terrain = is.finite(terrain_height[contact_index])
  for (iteration in seq_len(40)) {
    midpoint = (lower + upper) / 2
    midpoint_height = interpolate_spatial_water_surface_height(
      heightmap,
      x + direction_x * midpoint,
      z + direction_z * midpoint
    )
    midpoint_inside = is.finite(midpoint_height) &
      midpoint_height <= water_height + eps
    if (midpoint_inside) {
      lower = midpoint
    } else {
      upper = midpoint
    }
  }
  if (contact_is_terrain) {
    upper
  } else {
    lower
  }
}

#'@keywords internal
make_spatial_water_edge_sides = function(
  heightmap = NULL,
  water_height,
  x0,
  x1,
  z0,
  z1,
  left_edge,
  right_edge,
  bottom_edge,
  top_edge,
  row_index,
  col_index,
  nr,
  nc
) {
  if (is.null(heightmap) || !is.matrix(heightmap)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  cell_index = matrix(0L, nrow = nr, ncol = nc)
  cell_index[cbind(row_index, col_index)] = seq_along(row_index)
  rbind(
    make_spatial_water_side_vertices(
      side = "left",
      edge_mask = left_edge,
      x_start = x0,
      z_start = z0,
      x_end = x0,
      z_end = z1,
      water_height = water_height,
      heightmap = heightmap,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index,
      all_x0 = x0,
      all_x1 = x1,
      all_z0 = z0,
      all_z1 = z1,
      all_water_height = water_height
    ),
    make_spatial_water_side_vertices(
      side = "right",
      edge_mask = right_edge,
      x_start = x1,
      z_start = z1,
      x_end = x1,
      z_end = z0,
      water_height = water_height,
      heightmap = heightmap,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index,
      all_x0 = x0,
      all_x1 = x1,
      all_z0 = z0,
      all_z1 = z1,
      all_water_height = water_height
    ),
    make_spatial_water_side_vertices(
      side = "bottom",
      edge_mask = bottom_edge,
      x_start = x1,
      z_start = z0,
      x_end = x0,
      z_end = z0,
      water_height = water_height,
      heightmap = heightmap,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index,
      all_x0 = x0,
      all_x1 = x1,
      all_z0 = z0,
      all_z1 = z1,
      all_water_height = water_height
    ),
    make_spatial_water_side_vertices(
      side = "top",
      edge_mask = top_edge,
      x_start = x0,
      z_start = z1,
      x_end = x1,
      z_end = z1,
      water_height = water_height,
      heightmap = heightmap,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index,
      all_x0 = x0,
      all_x1 = x1,
      all_z0 = z0,
      all_z1 = z1,
      all_water_height = water_height
    )
  )
}

#'@keywords internal
make_spatial_water_side_vertices = function(
  side,
  edge_mask,
  x_start,
  z_start,
  x_end,
  z_end,
  water_height,
  heightmap,
  row_index,
  col_index,
  cell_index,
  all_x0,
  all_x1,
  all_z0,
  all_z1,
  all_water_height
) {
  if (!any(edge_mask)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  edge_index = which(edge_mask)
  clipped_edges = clip_spatial_water_side_segments(
    side = side,
    edge_index = edge_index,
    x_start = x_start,
    z_start = z_start,
    x_end = x_end,
    z_end = z_end,
    water_height = water_height,
    row_index = row_index,
    col_index = col_index,
    cell_index = cell_index,
    all_x0 = all_x0,
    all_x1 = all_x1,
    all_z0 = all_z0,
    all_z1 = all_z1,
    all_water_height = all_water_height
  )
  if (!length(clipped_edges$water_height)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  x_start = clipped_edges$x_start
  z_start = clipped_edges$z_start
  x_end = clipped_edges$x_end
  z_end = clipped_edges$z_end
  water_height = clipped_edges$water_height
  clipped_to_terrain = clip_spatial_water_side_segments_to_terrain(
    heightmap = heightmap,
    x_start = x_start,
    z_start = z_start,
    x_end = x_end,
    z_end = z_end,
    water_height = water_height
  )
  if (!length(clipped_to_terrain$water_height)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  x_start = clipped_to_terrain$x_start
  z_start = clipped_to_terrain$z_start
  x_end = clipped_to_terrain$x_end
  z_end = clipped_to_terrain$z_end
  water_height = clipped_to_terrain$water_height
  make_spatial_water_sidewall_vertices(
    heightmap = heightmap,
    x_start = x_start,
    z_start = z_start,
    x_end = x_end,
    z_end = z_end,
    water_height = water_height
  )
}

#'@keywords internal
make_spatial_water_sidewall_vertices = function(
  heightmap,
  x_start,
  z_start,
  x_end,
  z_end,
  water_height
) {
  if (!length(water_height)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  side_vertices = vector("list", length(water_height))
  for (segment_index in seq_along(water_height)) {
    breakpoints = spatial_water_side_segment_breakpoints(
      heightmap = heightmap,
      x_start = x_start[segment_index],
      z_start = z_start[segment_index],
      x_end = x_end[segment_index],
      z_end = z_end[segment_index]
    )
    segment_x = x_start[segment_index] +
      (x_end[segment_index] - x_start[segment_index]) * breakpoints
    segment_z = z_start[segment_index] +
      (z_end[segment_index] - z_start[segment_index]) * breakpoints
    segment_terrain = interpolate_spatial_water_surface_height(
      heightmap,
      segment_x,
      segment_z
    )
    segment_bottom = pmin(segment_terrain, water_height[segment_index])
    keep_points = simplify_spatial_water_sidewall_points(
      segment_x,
      segment_z,
      segment_bottom
    )
    segment_x = segment_x[keep_points]
    segment_z = segment_z[keep_points]
    segment_bottom = segment_bottom[keep_points]

    renderable = is.finite(segment_bottom[-length(segment_bottom)]) &
      is.finite(segment_bottom[-1]) &
      (water_height[segment_index] > segment_bottom[-length(segment_bottom)] |
        water_height[segment_index] > segment_bottom[-1])
    if (!any(renderable)) {
      next
    }

    start_index = which(renderable)
    end_index = start_index + 1L
    subsegment_x_start = segment_x[start_index]
    subsegment_z_start = segment_z[start_index]
    subsegment_x_end = segment_x[end_index]
    subsegment_z_end = segment_z[end_index]
    subsegment_bottom_start = segment_bottom[start_index]
    subsegment_bottom_end = segment_bottom[end_index]

    n_edges = length(start_index)
    vertices = matrix(NA_real_, nrow = n_edges * 6L, ncol = 3L)
    first_vertex = seq.int(1L, nrow(vertices), by = 6L)
    vertices[first_vertex, ] = cbind(
      subsegment_x_start,
      water_height[segment_index],
      subsegment_z_start
    )
    vertices[first_vertex + 1L, ] = cbind(
      subsegment_x_end,
      water_height[segment_index],
      subsegment_z_end
    )
    vertices[first_vertex + 2L, ] = cbind(
      subsegment_x_start,
      subsegment_bottom_start,
      subsegment_z_start
    )
    vertices[first_vertex + 3L, ] = cbind(
      subsegment_x_end,
      water_height[segment_index],
      subsegment_z_end
    )
    vertices[first_vertex + 4L, ] = cbind(
      subsegment_x_end,
      subsegment_bottom_end,
      subsegment_z_end
    )
    vertices[first_vertex + 5L, ] = cbind(
      subsegment_x_start,
      subsegment_bottom_start,
      subsegment_z_start
    )
    side_vertices[[segment_index]] = vertices
  }
  side_vertices = side_vertices[lengths(side_vertices) > 0]
  if (!length(side_vertices)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, side_vertices)
}

#'@keywords internal
simplify_spatial_water_sidewall_points = function(
  segment_x,
  segment_z,
  bottom
) {
  point_count = length(bottom)
  if (point_count <= 2L) {
    return(rep(TRUE, point_count))
  }
  keep = rep(FALSE, point_count)
  keep[c(1L, point_count)] = TRUE
  eps = sqrt(.Machine$double.eps)
  for (point_index in seq.int(2L, point_count - 1L)) {
    point_rows = seq.int(point_index - 1L, point_index + 1L)
    point_matrix = cbind(
      segment_x[point_rows],
      bottom[point_rows],
      segment_z[point_rows]
    )
    if (!all(is.finite(point_matrix))) {
      keep[point_index] = TRUE
      next
    }
    previous_edge = point_matrix[2L, ] - point_matrix[1L, ]
    next_edge = point_matrix[3L, ] - point_matrix[2L, ]
    cross_product = c(
      previous_edge[2L] * next_edge[3L] - previous_edge[3L] * next_edge[2L],
      previous_edge[3L] * next_edge[1L] - previous_edge[1L] * next_edge[3L],
      previous_edge[1L] * next_edge[2L] - previous_edge[2L] * next_edge[1L]
    )
    keep[point_index] = sqrt(sum(cross_product^2)) > eps
  }
  keep
}

#'@keywords internal
spatial_water_side_segment_breakpoints = function(
  heightmap,
  x_start,
  z_start,
  x_end,
  z_end
) {
  row_col_start = spatial_water_row_col(heightmap, x_start, z_start)
  row_col_end = spatial_water_row_col(heightmap, x_end, z_end)
  row_start = row_col_start$row
  row_end = row_col_end$row
  col_start = row_col_start$col
  col_end = row_col_end$col
  delta_row = row_end - row_start
  delta_col = col_end - col_start
  eps = sqrt(.Machine$double.eps)

  breakpoints = c(
    0,
    1,
    spatial_water_axis_breakpoints(row_start, delta_row),
    spatial_water_axis_breakpoints(col_start, delta_col)
  )
  breakpoints = clean_spatial_water_breakpoints(breakpoints)
  diagonal_breakpoints = numeric()
  diagonal_denominator = delta_row + delta_col
  if (abs(diagonal_denominator) > eps) {
    for (breakpoint_index in seq_len(length(breakpoints) - 1L)) {
      lower = breakpoints[breakpoint_index]
      upper = breakpoints[breakpoint_index + 1L]
      if (upper <= lower + eps) {
        next
      }
      midpoint = (lower + upper) / 2
      midpoint_row = row_start + delta_row * midpoint
      midpoint_col = col_start + delta_col * midpoint
      row0 = pmin(pmax(floor(midpoint_row), 1), nrow(heightmap) - 1L)
      col0 = pmin(pmax(floor(midpoint_col), 1), ncol(heightmap) - 1L)
      diagonal = (row0 + col0 + 1 - row_start - col_start) /
        diagonal_denominator
      if (diagonal > lower + eps && diagonal < upper - eps) {
        diagonal_breakpoints = c(diagonal_breakpoints, diagonal)
      }
    }
  }
  clean_spatial_water_breakpoints(c(breakpoints, diagonal_breakpoints))
}

#'@keywords internal
spatial_water_axis_breakpoints = function(axis_start, axis_delta) {
  eps = sqrt(.Machine$double.eps)
  if (abs(axis_delta) <= eps) {
    return(numeric())
  }
  axis_end = axis_start + axis_delta
  lower = min(axis_start, axis_end)
  upper = max(axis_start, axis_end)
  crossings = seq.int(ceiling(lower), floor(upper))
  crossings = crossings[crossings > lower + eps & crossings < upper - eps]
  (crossings - axis_start) / axis_delta
}

#'@keywords internal
clean_spatial_water_breakpoints = function(breakpoints) {
  eps = sqrt(.Machine$double.eps)
  breakpoints = sort(pmin(pmax(breakpoints[is.finite(breakpoints)], 0), 1))
  if (!length(breakpoints)) {
    return(c(0, 1))
  }
  breakpoints = breakpoints[c(TRUE, diff(breakpoints) > eps)]
  if (breakpoints[1] <= eps) {
    breakpoints[1] = 0
  } else {
    breakpoints = c(0, breakpoints)
  }
  if (breakpoints[length(breakpoints)] >= 1 - eps) {
    breakpoints[length(breakpoints)] = 1
  } else {
    breakpoints = c(breakpoints, 1)
  }
  breakpoints
}

#'@keywords internal
clip_spatial_water_side_segments = function(
  side,
  edge_index,
  x_start,
  z_start,
  x_end,
  z_end,
  water_height,
  row_index,
  col_index,
  cell_index,
  all_x0,
  all_x1,
  all_z0,
  all_z1,
  all_water_height
) {
  segment_x_start = numeric()
  segment_z_start = numeric()
  segment_x_end = numeric()
  segment_z_end = numeric()
  segment_height = numeric()

  for (edge in edge_index) {
    covering_cells = get_spatial_water_covering_cells(
      edge = edge,
      row_index = row_index,
      col_index = col_index,
      cell_index = cell_index
    )
    coverage = get_spatial_water_side_coverage(
      side = side,
      edge = edge,
      covering_cells = covering_cells,
      edge_water_height = water_height[edge],
      x_start = x_start,
      z_start = z_start,
      x_end = x_end,
      z_end = z_end,
      all_x0 = all_x0,
      all_x1 = all_x1,
      all_z0 = all_z0,
      all_z1 = all_z1,
      all_water_height = all_water_height
    )
    if (identical(side, "left") || identical(side, "right")) {
      uncovered = subtract_spatial_water_coverage(
        range(c(z_start[edge], z_end[edge])),
        coverage
      )
      if (!nrow(uncovered)) {
        next
      }
      reverse_segment = z_end[edge] < z_start[edge]
      segment_x_start = c(segment_x_start, rep(x_start[edge], nrow(uncovered)))
      segment_x_end = c(segment_x_end, rep(x_end[edge], nrow(uncovered)))
      segment_z_start = c(
        segment_z_start,
        if (reverse_segment) uncovered[, 2] else uncovered[, 1]
      )
      segment_z_end = c(
        segment_z_end,
        if (reverse_segment) uncovered[, 1] else uncovered[, 2]
      )
    } else {
      uncovered = subtract_spatial_water_coverage(
        range(c(x_start[edge], x_end[edge])),
        coverage
      )
      if (!nrow(uncovered)) {
        next
      }
      reverse_segment = x_end[edge] < x_start[edge]
      segment_x_start = c(
        segment_x_start,
        if (reverse_segment) uncovered[, 2] else uncovered[, 1]
      )
      segment_x_end = c(
        segment_x_end,
        if (reverse_segment) uncovered[, 1] else uncovered[, 2]
      )
      segment_z_start = c(segment_z_start, rep(z_start[edge], nrow(uncovered)))
      segment_z_end = c(segment_z_end, rep(z_end[edge], nrow(uncovered)))
    }
    segment_height = c(segment_height, rep(water_height[edge], nrow(uncovered)))
  }

  list(
    x_start = segment_x_start,
    z_start = segment_z_start,
    x_end = segment_x_end,
    z_end = segment_z_end,
    water_height = segment_height
  )
}

#'@keywords internal
get_spatial_water_covering_cells = function(
  edge,
  row_index,
  col_index,
  cell_index
) {
  row_range = seq.int(
    max(1L, row_index[edge] - 2L),
    min(nrow(cell_index), row_index[edge] + 2L)
  )
  col_range = seq.int(
    max(1L, col_index[edge] - 2L),
    min(ncol(cell_index), col_index[edge] + 2L)
  )
  covering_cells = as.integer(cell_index[row_range, col_range])
  unique(covering_cells[covering_cells > 0L & covering_cells != edge])
}

#'@keywords internal
get_spatial_water_side_coverage = function(
  side,
  edge,
  covering_cells,
  edge_water_height,
  x_start,
  z_start,
  x_end,
  z_end,
  all_x0,
  all_x1,
  all_z0,
  all_z1,
  all_water_height
) {
  if (!length(covering_cells)) {
    return(matrix(nrow = 0, ncol = 2))
  }
  eps = sqrt(.Machine$double.eps)
  if (identical(side, "left") || identical(side, "right")) {
    edge_x = x_start[edge]
    edge_range = range(c(z_start[edge], z_end[edge]))
    spans_edge = if (identical(side, "left")) {
      all_x0[covering_cells] < edge_x - eps &
        all_x1[covering_cells] >= edge_x - eps
    } else {
      all_x0[covering_cells] <= edge_x + eps &
        all_x1[covering_cells] > edge_x + eps
    }
    coverage = cbind(
      pmax(all_z0[covering_cells], edge_range[1]),
      pmin(all_z1[covering_cells], edge_range[2])
    )
  } else {
    edge_z = z_start[edge]
    edge_range = range(c(x_start[edge], x_end[edge]))
    spans_edge = if (identical(side, "bottom")) {
      all_z0[covering_cells] < edge_z - eps &
        all_z1[covering_cells] >= edge_z - eps
    } else {
      all_z0[covering_cells] <= edge_z + eps &
        all_z1[covering_cells] > edge_z + eps
    }
    coverage = cbind(
      pmax(all_x0[covering_cells], edge_range[1]),
      pmin(all_x1[covering_cells], edge_range[2])
    )
  }
  coverage = coverage[
    spans_edge & all_water_height[covering_cells] >= edge_water_height - eps,
    ,
    drop = FALSE
  ]
  coverage[coverage[, 2] > coverage[, 1] + eps, , drop = FALSE]
}

#'@keywords internal
subtract_spatial_water_coverage = function(edge_range, coverage) {
  eps = sqrt(.Machine$double.eps)
  if (!nrow(coverage)) {
    return(matrix(edge_range, nrow = 1L))
  }
  coverage = coverage[order(coverage[, 1], coverage[, 2]), , drop = FALSE]
  uncovered = matrix(nrow = 0, ncol = 2)
  current_start = edge_range[1]
  for (coverage_index in seq_len(nrow(coverage))) {
    coverage_start = max(edge_range[1], coverage[coverage_index, 1])
    coverage_end = min(edge_range[2], coverage[coverage_index, 2])
    if (coverage_end <= current_start + eps) {
      next
    }
    if (coverage_start > current_start + eps) {
      uncovered = rbind(uncovered, c(current_start, coverage_start))
    }
    current_start = max(current_start, coverage_end)
    if (current_start >= edge_range[2] - eps) {
      break
    }
  }
  if (current_start < edge_range[2] - eps) {
    uncovered = rbind(uncovered, c(current_start, edge_range[2]))
  }
  uncovered
}

#' Convert spatial water scene coordinates to heightmap row/column coordinates
#'
#' @param heightmap Heightmap matrix.
#' @param x X scene coordinate.
#' @param z Z scene coordinate.
#' @param clamp Default `TRUE`. Whether to clamp coordinates to the heightmap.
#'
#' @return List with `row` and `col` coordinates.
#' @keywords internal
spatial_water_row_col = function(heightmap, x, z, clamp = TRUE) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  row = x + (nr - 1) / 2 + 1
  col = z + (nc - 1) / 2 + 1
  if (isTRUE(clamp)) {
    row = pmin(pmax(row, 1), nr)
    col = pmin(pmax(col, 1), nc)
  }
  list(row = row, col = col)
}

#'@keywords internal
spatial_water_point_has_finite_heightmap_cell = function(heightmap, x, z) {
  row_col = spatial_water_row_col(heightmap, x, z, clamp = FALSE)
  row = row_col$row
  col = row_col$col
  row_index = floor(row + 0.5)
  col_index = floor(col + 0.5)
  in_bounds = row >= 1 &
    row <= nrow(heightmap) &
    col >= 1 &
    col <= ncol(heightmap) &
    row_index >= 1L &
    row_index <= nrow(heightmap) &
    col_index >= 1L &
    col_index <= ncol(heightmap)
  supported = rep(FALSE, length(row))
  supported[in_bounds] = is.finite(heightmap[cbind(
    row_index[in_bounds],
    col_index[in_bounds]
  )])
  supported
}

#'@keywords internal
interpolate_spatial_water_height = function(heightmap, x, z) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  if (nr < 2 || nc < 2) {
    return(rep(heightmap[1, 1], length(x)))
  }
  row_col = spatial_water_row_col(heightmap, x, z)
  row = row_col$row
  col = row_col$col
  row0 = pmin(pmax(floor(row), 1), nr - 1)
  row1 = row0 + 1L
  col0 = pmin(pmax(floor(col), 1), nc - 1)
  col1 = col0 + 1L
  row_weight = row - row0
  col_weight = col - col0

  height00 = heightmap[cbind(row0, col0)]
  height10 = heightmap[cbind(row1, col0)]
  height01 = heightmap[cbind(row0, col1)]
  height11 = heightmap[cbind(row1, col1)]
  top_triangle = row_weight + col_weight <= 1
  interpolated = numeric(length(row))
  interpolated[top_triangle] = height00[top_triangle] +
    row_weight[top_triangle] *
      (height10[top_triangle] - height00[top_triangle]) +
    col_weight[top_triangle] *
      (height01[top_triangle] - height00[top_triangle])
  interpolated[!top_triangle] = height11[!top_triangle] +
    (1 - col_weight[!top_triangle]) *
      (height10[!top_triangle] - height11[!top_triangle]) +
    (1 - row_weight[!top_triangle]) *
      (height01[!top_triangle] - height11[!top_triangle])

  nearest_row = as.integer(round(row))
  nearest_col = as.integer(round(col))
  nearest_height = heightmap[cbind(nearest_row, nearest_col)]
  fallback = !is.finite(interpolated)
  interpolated[fallback] = nearest_height[fallback]
  interpolated
}

#'@keywords internal
interpolate_spatial_water_surface_height = function(heightmap, x, z) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  if (nr < 2 || nc < 2) {
    return(rep(heightmap[1, 1], length(x)))
  }
  row_col = spatial_water_row_col(heightmap, x, z)
  row = row_col$row
  col = row_col$col
  row0 = pmin(pmax(floor(row), 1), nr - 1)
  row1 = row0 + 1L
  col0 = pmin(pmax(floor(col), 1), nc - 1)
  col1 = col0 + 1L
  row_weight = row - row0
  col_weight = col - col0

  height00 = heightmap[cbind(row0, col0)]
  height10 = heightmap[cbind(row1, col0)]
  height01 = heightmap[cbind(row0, col1)]
  height11 = heightmap[cbind(row1, col1)]
  top_triangle = row_weight + col_weight <= 1
  interpolated = rep(NA_real_, length(row))
  interpolated[top_triangle] = height00[top_triangle] +
    row_weight[top_triangle] *
      (height10[top_triangle] - height00[top_triangle]) +
    col_weight[top_triangle] *
      (height01[top_triangle] - height00[top_triangle])
  interpolated[!top_triangle] = height11[!top_triangle] +
    (1 - col_weight[!top_triangle]) *
      (height10[!top_triangle] - height11[!top_triangle]) +
    (1 - row_weight[!top_triangle]) *
      (height01[!top_triangle] - height11[!top_triangle])
  interpolated
}

#'@keywords internal
interpolate_spatial_water_surface_height_unclamped = function(heightmap, x, z) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  row_col = spatial_water_row_col(heightmap, x, z, clamp = FALSE)
  in_bounds = row_col$row >= 1 &
    row_col$row <= nr &
    row_col$col >= 1 &
    row_col$col <= nc
  interpolated = rep(NA_real_, length(x))
  if (!any(in_bounds)) {
    return(interpolated)
  }
  interpolated[in_bounds] = interpolate_spatial_water_surface_height(
    heightmap = heightmap,
    x = x[in_bounds],
    z = z[in_bounds]
  )
  interpolated
}

#'@keywords internal
make_water_legacy = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5
) {
  if (
    is.matrix(waterheight) ||
      is_spatial_heightmap_input(waterheight) ||
      length(waterheight) != 1
  ) {
    stop(
      "`water_render_method = \"legacy\"` only supports a scalar `waterdepth`.",
      call. = FALSE
    )
  }
  heightmap = heightmap / zscale
  na_matrix = is.na(heightmap)
  nr = nrow(heightmap)
  nc = ncol(heightmap)

  waterheight = waterheight / zscale
  if (all(heightmap >= waterheight, na.rm = TRUE)) {
    warning(
      "No water rendered--all elevations above or equal to water level. Range of heights: ",
      min(heightmap, na.rm = TRUE) * zscale,
      "-",
      max(heightmap, na.rm = TRUE) * zscale,
      ". Depth specified: ",
      waterheight * zscale
    )
  } else {
    heightlist = make_water_cpp(heightmap, na_matrix, waterheight)
    if (length(heightlist) > 0) {
      fullsides = do.call(rbind, heightlist)
      fullsides[, 3] = -fullsides[, 3]
      fullsides[, 1] = fullsides[, 1] - 1
      fullsides[, 3] = fullsides[, 3]

      fullsides[, 1] = fullsides[, 1] - (nr - 1) / 2
      fullsides[, 3] = fullsides[, 3] - (nc - 1) / 2
    }
    nr1 = nr - 1
    nc1 = nc - 1

    if (all(!na_matrix)) {
      vertices = rbind(
        matrix(
          c(
            -nr1 / 2,
            nr1 / 2,
            -nr1 / 2,
            waterheight,
            waterheight,
            waterheight,
            nc1 / 2,
            -nc1 / 2,
            -nc1 / 2
          ),
          nrow = 3L,
          ncol = 3L
        ),
        matrix(
          c(
            -nr1 / 2,
            nr1 / 2,
            nr1 / 2,
            waterheight,
            waterheight,
            waterheight,
            nc1 / 2,
            nc1 / 2,
            -nc1 / 2
          ),
          nrow = 3L,
          ncol = 3L
        )
      )
      indices = seq_len(6L)
      rgl::triangles3d(
        x = vertices,
        indices = indices,
        color = watercolor,
        alpha = wateralpha,
        lit = FALSE,
        front = "filled",
        back = "cull",
        texture = NULL,
        tag = "water"
      )
      if (length(heightlist) > 0) {
        indices = rev(seq_len(nrow(fullsides)))
        rgl::triangles3d(
          fullsides,
          indices = indices,
          lit = FALSE,
          color = watercolor,
          alpha = wateralpha,
          front = "filled",
          back = "cull",
          depth_test = "less",
          texture = NULL,
          tag = "water"
        )
      }
    } else {
      if (length(heightlist) > 0) {
        indices = rev(seq_len(nrow(fullsides)))
        rgl::triangles3d(
          fullsides,
          indices = indices,
          lit = FALSE,
          color = watercolor,
          alpha = wateralpha,
          front = "fill",
          back = "culled",
          texture = NULL,
          tag = "water"
        )
      }

      basemat = matrix(waterheight, nr, nc)
      basemat[is.na(heightmap)] = NA
      ray_surface = generate_surface(basemat, zscale = 1)

      rgl::triangles3d(
        x = ray_surface$verts,
        indices = ray_surface$inds,
        texcoords = ray_surface$texcoords,
        color = watercolor,
        alpha = wateralpha,
        back = "culled",
        front = "fill",
        lit = FALSE,
        texture = NULL,
        tag = "water"
      )
    }
  }
  invisible(NULL)
}

#'@keywords internal
normalize_waterheight_matrix = function(
  waterheight,
  nr,
  nc,
  zscale,
  caller,
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  if (is.matrix(waterheight)) {
    if (!is.numeric(waterheight)) {
      stop("`waterdepth` must be numeric.", call. = FALSE)
    }
    if (!all(dim(waterheight) == c(nr, nc))) {
      stop(
        sprintf(
          "`waterdepth` matrix must have dimensions %i x %i to match `heightmap`.",
          nr,
          nc
        ),
        call. = FALSE
      )
    }
    return(waterheight / zscale)
  }
  if (is_spatial_heightmap_input(waterheight)) {
    waterheight = resolve_spatial_waterheight_matrix(
      waterheight = waterheight,
      nr = nr,
      nc = nc,
      heightmap_extent = heightmap_extent,
      heightmap_crs = heightmap_crs,
      caller = caller
    )
    return(waterheight / zscale)
  }
  if (
    !is.numeric(waterheight) || length(waterheight) != 1 || is.na(waterheight)
  ) {
    stop(
      sprintf(
        "`waterdepth` must be a scalar, a matrix, or a spatial raster for %s().",
        caller
      ),
      call. = FALSE
    )
  }
  matrix(waterheight / zscale, nrow = nr, ncol = nc)
}

#'@keywords internal
resolve_spatial_waterheight_matrix = function(
  waterheight,
  nr,
  nc,
  heightmap_extent = NULL,
  heightmap_crs = NULL,
  caller = NULL
) {
  water_raster = coerce_spatial_waterheight_raster(waterheight)
  target_template = build_waterheight_template(
    nr = nr,
    nc = nc,
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs,
    caller = caller
  )
  source_crs = tryCatch(terra::crs(water_raster), error = function(e) "")
  target_crs = tryCatch(terra::crs(target_template), error = function(e) "")
  source_has_crs = is.character(source_crs) &&
    length(source_crs) &&
    nzchar(source_crs[1])
  target_has_crs = is.character(target_crs) &&
    length(target_crs) &&
    nzchar(target_crs[1])

  if (target_has_crs && !source_has_crs) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` inputs must have a CRS when the active heightmap has a CRS."
      ),
      call. = FALSE
    )
  }

  aligned_raster = tryCatch(
    {
      if (target_has_crs) {
        same_crs = isTRUE(tryCatch(
          scene_crs_equal(source_crs, target_crs),
          error = function(e) FALSE
        ))
        if (same_crs) {
          terra::resample(water_raster, target_template, method = "bilinear")
        } else {
          terra::project(water_raster, target_template, method = "bilinear")
        }
      } else {
        terra::resample(water_raster, target_template, method = "bilinear")
      }
    },
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not project/resample spatial `waterdepth` to the active heightmap grid: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  waterheight_matrix = raster_to_matrix(aligned_raster, verbose = FALSE)
  if (!all(dim(waterheight_matrix) == c(nr, nc))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` could not be aligned to the active heightmap grid."
      ),
      call. = FALSE
    )
  }
  waterheight_matrix
}

#'@keywords internal
coerce_spatial_waterheight_raster = function(waterheight) {
  if (is.character(waterheight)) {
    waterheight = terra::rast(waterheight)
  } else if (
    inherits(waterheight, c("RasterLayer", "RasterBrick", "RasterStack"))
  ) {
    waterheight = terra::rast(waterheight)
  }
  if (!inherits(waterheight, "SpatRaster")) {
    stop("`waterdepth` must resolve to a spatial raster.", call. = FALSE)
  }
  if (terra::nlyr(waterheight) > 1) {
    warning("`waterdepth` has multiple layers; using the first layer.")
    waterheight = waterheight[[1]]
  }
  waterheight
}

#'@keywords internal
build_waterheight_template = function(
  nr,
  nc,
  heightmap_extent = NULL,
  heightmap_crs = NULL,
  caller = NULL
) {
  if (is.null(heightmap_extent)) {
    heightmap_extent = get_scene_extent(default = NULL)
  }
  if (is.null(heightmap_extent)) {
    heightmap_extent = get_hillshade_extent(default = NULL)
  }
  if (is.null(heightmap_extent)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` inputs require an active heightmap extent."
      ),
      call. = FALSE
    )
  }
  heightmap_extent = tryCatch(
    get_extent(heightmap_extent),
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not interpret the active heightmap extent for spatial `waterdepth`: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  target_template = terra::rast(
    nrows = nc,
    ncols = nr,
    xmin = heightmap_extent["xmin"],
    xmax = heightmap_extent["xmax"],
    ymin = heightmap_extent["ymin"],
    ymax = heightmap_extent["ymax"]
  )
  if (is.null(heightmap_crs)) {
    heightmap_crs = get_scene_target_crs(
      extent = heightmap_extent,
      caller = caller
    )
  }
  heightmap_crs = waterheight_terra_crs(heightmap_crs)
  if (!is.null(heightmap_crs)) {
    terra::crs(target_template) = heightmap_crs
  }
  target_template
}

#'@keywords internal
waterheight_terra_crs = function(crs) {
  parsed_crs = try_parse_scene_crs(crs)
  if (!is.null(parsed_crs) && !is.na(parsed_crs)) {
    return(parsed_crs$wkt)
  }
  if (is.character(crs) && length(crs) && nzchar(trimws(crs[1]))) {
    return(crs[1])
  }
  NULL
}

#'@keywords internal
format_no_water_warning = function(heightmap, waterheight, zscale) {
  height_range = range(heightmap, na.rm = TRUE) * zscale
  water_range = range(waterheight, na.rm = TRUE) * zscale
  if (diff(water_range) == 0) {
    return(paste0(
      "No water rendered--all elevations above or equal to water level. Range of heights: ",
      height_range[1],
      "-",
      height_range[2],
      ". Depth specified: ",
      water_range[1]
    ))
  }
  paste0(
    "No water rendered--all elevations above or equal to water levels. Range of heights: ",
    height_range[1],
    "-",
    height_range[2],
    ". Water level range specified: ",
    water_range[1],
    "-",
    water_range[2]
  )
}

#'@keywords internal
make_waterlines_from_mesh = function(
  water_mesh,
  linecolor = "grey40",
  alpha = 1,
  linewidth = 2,
  antialias = FALSE
) {
  if (is.null(water_mesh) || is.null(water_mesh$lines)) {
    return(invisible(NULL))
  }
  segmentlist = water_mesh$lines
  if (!is.matrix(segmentlist) || nrow(segmentlist) == 0) {
    return(invisible(NULL))
  }
  rgl::segments3d(
    segmentlist,
    color = linecolor,
    lwd = linewidth,
    alpha = alpha,
    depth_mask = TRUE,
    line_antialias = antialias,
    depth_test = "lequal",
    tag = "waterlines",
    lit = FALSE
  )
  invisible(NULL)
}
