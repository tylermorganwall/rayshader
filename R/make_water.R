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
resolve_polygon_water_render_method_for_terrain = function(
  water_render_method,
  triangulate = FALSE,
  caller = "render_water"
) {
  if (isTRUE(triangulate) && identical(water_render_method, "polygon")) {
    warning(
      "`water_render_method = \"polygon\"` clips the fixed grid terrain mesh ",
      "and cannot exactly conform to `triangulate = TRUE` terrain; falling back ",
      "to `water_render_method = \"raster\"` for this ",
      caller,
      "() call.",
      call. = FALSE
    )
    return("raster")
  }
  water_render_method
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
  terrain_mesh = make_spatial_water_fixed_grid_terrain_mesh(heightmap)
  if (!nrow(terrain_mesh$faces)) {
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
    terrain_mesh = terrain_mesh,
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
  terrain_mesh = make_spatial_water_fixed_grid_terrain_mesh(heightmap),
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
      heightmap = heightmap,
      terrain_mesh = terrain_mesh
    ))
  }
  make_spatial_water_polygon_components_parallel(
    component_tasks = component_tasks,
    heightmap = heightmap,
    terrain_mesh = terrain_mesh,
    worker_count = worker_count
  )
}

#'@keywords internal
make_spatial_water_polygon_component_from_task = function(
  task,
  heightmap,
  terrain_mesh = make_spatial_water_fixed_grid_terrain_mesh(heightmap)
) {
  make_spatial_water_polygon_component(
    component_mask = task$component_mask,
    heightmap = heightmap,
    terrain_mesh = terrain_mesh,
    fallback_level = task$fallback_level
  )
}

#'@keywords internal
make_spatial_water_polygon_components_parallel = function(
  component_tasks,
  heightmap,
  terrain_mesh,
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
  native_dll_path = spatial_water_native_dll_path()
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
    function(task, heightmap, terrain_mesh, helper_functions, native_dll_path) {
      worker_env = new.env(parent = .GlobalEnv)
      if (
        !is.null(native_dll_path) &&
          is.character(native_dll_path) &&
          length(native_dll_path) == 1L &&
          file.exists(native_dll_path)
      ) {
        try(dyn.load(native_dll_path), silent = TRUE)
      }
      for (helper_name in names(helper_functions)) {
        helper_function = helper_functions[[helper_name]]
        if (
          is.function(helper_function) &&
            typeof(helper_function) == "closure" &&
            !grepl("_cpp$", helper_name)
        ) {
          environment(helper_function) = worker_env
        }
        assign(helper_name, helper_function, envir = worker_env)
      }
      worker_env$assign_spatial_water_cpp_worker_helpers(worker_env)
      worker_env$make_spatial_water_polygon_component_from_task(
        task = task,
        heightmap = heightmap,
        terrain_mesh = terrain_mesh
      )
    },
    .args = list(
      heightmap = heightmap,
      terrain_mesh = terrain_mesh,
      helper_functions = helper_functions,
      native_dll_path = native_dll_path
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
spatial_water_native_dll_path = function() {
  dlls = getLoadedDLLs()
  if (!"rayshader" %in% names(dlls)) {
    return(NULL)
  }
  dll_path = dlls[["rayshader"]][["path"]]
  if (is.null(dll_path) || !nzchar(dll_path)) {
    return(NULL)
  }
  dll_path
}

#'@keywords internal
assign_spatial_water_cpp_worker_helpers = function(worker_env) {
  worker_env$make_spatial_water_fixed_grid_terrain_mesh_cpp = function(
    heightmap
  ) {
    .Call(
      "_rayshader_make_spatial_water_fixed_grid_terrain_mesh_cpp",
      heightmap,
      PACKAGE = "rayshader"
    )
  }
  worker_env$spatial_water_face_sublevel_area_cpp = function(
    terrain_mesh,
    face_ids,
    water_level,
    height_tol
  ) {
    .Call(
      "_rayshader_spatial_water_face_sublevel_area_cpp",
      terrain_mesh,
      face_ids,
      water_level,
      height_tol,
      PACKAGE = "rayshader"
    )
  }
  worker_env$spatial_water_traverse_seeded_clipped_faces_cpp = function(
    terrain_mesh,
    component_seed,
    water_level,
    target_area_limit,
    height_tol,
    length_tol,
    area_tol,
    return_face_ids = FALSE
  ) {
    .Call(
      "_rayshader_spatial_water_traverse_seeded_clipped_faces_cpp",
      terrain_mesh,
      component_seed,
      water_level,
      target_area_limit,
      height_tol,
      length_tol,
      area_tol,
      return_face_ids,
      PACKAGE = "rayshader"
    )
  }
  worker_env$build_spatial_water_full_terrain_geometry_cpp = function(
    terrain_mesh,
    water_level,
    surface_area_tol
  ) {
    .Call(
      "_rayshader_build_spatial_water_full_terrain_geometry_cpp",
      terrain_mesh,
      water_level,
      surface_area_tol,
      PACKAGE = "rayshader"
    )
  }
  worker_env$build_spatial_water_triangle_clipped_geometry_cpp = function(
    terrain_mesh,
    selected_face_ids,
    water_level,
    height_tol,
    t_tol,
    length_tol,
    area_tol,
    surface_area_tol
  ) {
    .Call(
      "_rayshader_build_spatial_water_triangle_clipped_geometry_cpp",
      terrain_mesh,
      selected_face_ids,
      water_level,
      height_tol,
      t_tol,
      length_tol,
      area_tol,
      surface_area_tol,
      PACKAGE = "rayshader"
    )
  }
  invisible(worker_env)
}

#'@keywords internal
spatial_water_parallel_helper_functions = function() {
  helper_names = c(
    "make_spatial_water_polygon_component_from_task",
    "make_spatial_water_polygon_component",
    "empty_spatial_water_polygon_mesh",
    "assign_spatial_water_cpp_worker_helpers",
    "make_spatial_water_fixed_grid_terrain_mesh_cpp",
    "make_spatial_water_fixed_grid_terrain_mesh",
    "make_spatial_water_fixed_grid_terrain_mesh_r",
    "spatial_water_fixed_grid_edge_key",
    "spatial_water_original_vertices_edge_id",
    "spatial_water_component_mask_metrics",
    "make_spatial_water_component_seed",
    "spatial_water_component_seed_perimeter",
    "make_spatial_water_triangle_clipped_component",
    "evaluate_spatial_water_triangle_clipped_component",
    "spatial_water_empty_triangle_clip_evaluation",
    "spatial_water_level_tolerance",
    "spatial_water_triangle_clip_tolerances",
    "spatial_water_face_sublevel_area_cpp",
    "spatial_water_face_sublevel_area",
    "spatial_water_face_sublevel_area_r",
    "spatial_water_face_sublevel_area_scalar",
    "spatial_water_evaluate_full_scene_component",
    "spatial_water_traverse_seeded_clipped_faces_cpp",
    "spatial_water_traverse_seeded_clipped_faces",
    "spatial_water_traverse_seeded_clipped_faces_r",
    "spatial_water_seed_face_has_positive_overlap",
    "spatial_water_seed_cell_bounds",
    "spatial_water_face_clipped_xz_polygon",
    "spatial_water_clean_xz_polygon",
    "spatial_water_edge_intersections",
    "clip_spatial_water_terrain_face_to_level",
    "spatial_water_original_vertex_records",
    "spatial_water_edge_plane_intersection_record",
    "clean_spatial_water_clipped_records",
    "spatial_water_projected_records_area",
    "spatial_water_projected_polygon_area",
    "spatial_water_records_overlap_component_seed",
    "spatial_water_records_original_edge_segments",
    "spatial_water_records_shared_original_edge_id",
    "spatial_water_record_edge_t",
    "spatial_water_terrain_edge_projected_length",
    "spatial_water_selected_clipped_faces",
    "build_spatial_water_triangle_clipped_geometry_cpp",
    "build_spatial_water_triangle_clipped_geometry",
    "build_spatial_water_triangle_clipped_geometry_r",
    "build_spatial_water_full_terrain_geometry_cpp",
    "build_spatial_water_full_terrain_geometry",
    "build_spatial_water_full_terrain_geometry_r",
    "spatial_water_triangle_normal_y",
    "spatial_water_register_clipped_boundary_edges",
    "spatial_water_triangle_clipped_boundary_geometry",
    "make_spatial_water_triangle_clipped_sidewall",
    "spatial_water_triangle_area3d",
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
    "clip_spatial_water_polygon_to_bounds",
    "clip_spatial_water_polygon_to_axis",
    "spatial_water_axis_intersection_point",
    "clean_spatial_water_polygon",
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
make_spatial_water_fixed_grid_terrain_mesh = function(heightmap) {
  make_spatial_water_fixed_grid_terrain_mesh_cpp(heightmap)
}

#'@keywords internal
make_spatial_water_fixed_grid_terrain_mesh_r = function(heightmap) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  if (nr < 2L || nc < 2L) {
    vertices = cbind(
      x = numeric(),
      h = numeric(),
      z = numeric()
    )
    return(list(
      vertices = vertices,
      faces = matrix(nrow = 0, ncol = 3L),
      face_edges = matrix(nrow = 0, ncol = 3L),
      face_neighbors = matrix(nrow = 0, ncol = 3L),
      face_cells = matrix(nrow = 0, ncol = 2L),
      face_cell_row = integer(),
      face_cell_col = integer(),
      face_cell_id = integer(),
      face_type = integer(),
      face_heights = matrix(nrow = 0, ncol = 3L),
      face_projected_area = numeric(),
      cell_face_id = matrix(nrow = 0, ncol = 2L),
      edge_vertices = matrix(nrow = 0, ncol = 2L),
      edge_face_count = integer(),
      edge_first_face = integer(),
      edge_first_face_edge = integer(),
      edge_min_height = numeric(),
      nr = nr,
      nc = nc,
      x_edge_count = 0L,
      z_edge_count = 0L,
      diag_edge_count = 0L
    ))
  }
  vertex_id = matrix(seq_len(nr * nc), nrow = nr, ncol = nc)
  vertex_x = matrix(seq_len(nr) - 1, nrow = nr, ncol = nc) - (nr - 1) / 2
  vertex_z = matrix(
    seq_len(nc) - 1,
    nrow = nr,
    ncol = nc,
    byrow = TRUE
  ) -
    (nc - 1) / 2
  vertices = cbind(
    x = c(vertex_x),
    h = c(heightmap),
    z = c(vertex_z)
  )

  # Match generate_surface(): each finite 2x2 heightmap cell emits both
  # fixed-diagonal triangles below, and any cell with an NA corner emits neither.
  cell_row = rep(seq_len(nr - 1L), times = nc - 1L)
  cell_col = rep(seq_len(nc - 1L), each = nr - 1L)
  cell_id = seq_along(cell_row)
  v00 = cell_row + (cell_col - 1L) * nr
  v01 = v00 + nr
  v10 = v00 + 1L
  v11 = v01 + 1L
  finite_cell = is.finite(vertices[v00, "h"]) &
    is.finite(vertices[v01, "h"]) &
    is.finite(vertices[v10, "h"]) &
    is.finite(vertices[v11, "h"])
  render_cell = which(finite_cell)
  face_count = length(render_cell) * 2L

  x_edge_count = (nr - 1L) * nc
  z_edge_count = nr * (nc - 1L)
  diag_edge_count = length(cell_id)
  edge_count = x_edge_count + z_edge_count + diag_edge_count

  x_edge_row = rep(seq_len(nr - 1L), times = nc)
  x_edge_col = rep(seq_len(nc), each = nr - 1L)
  x_edge_v0 = x_edge_row + (x_edge_col - 1L) * nr
  z_edge_row = rep(seq_len(nr), times = nc - 1L)
  z_edge_col = rep(seq_len(nc - 1L), each = nr)
  z_edge_v0 = z_edge_row + (z_edge_col - 1L) * nr
  edge_vertices = rbind(
    cbind(x_edge_v0, x_edge_v0 + 1L),
    cbind(z_edge_v0, z_edge_v0 + nr),
    cbind(v10, v01)
  )

  if (face_count) {
    top_slots = 2L * render_cell - 1L
    bottom_slots = 2L * render_cell
    face_slots = as.vector(t(cbind(top_slots, bottom_slots)))
    slot_to_face = integer(length(cell_id) * 2L)
    slot_to_face[face_slots] = seq_len(face_count)
    face_cell_id = rep(render_cell, each = 2L)
    face_type = rep(c(1L, 2L), length(render_cell))
    face_cell_row = cell_row[face_cell_id]
    face_cell_col = cell_col[face_cell_id]

    faces = matrix(NA_integer_, nrow = face_count, ncol = 3L)
    top_face = seq.int(1L, face_count, by = 2L)
    bottom_face = seq.int(2L, face_count, by = 2L)
    faces[top_face, ] = cbind(
      v00[render_cell],
      v01[render_cell],
      v10[render_cell]
    )
    faces[bottom_face, ] = cbind(
      v10[render_cell],
      v01[render_cell],
      v11[render_cell]
    )

    x_edge_id = function(row_index, col_index) {
      (col_index - 1L) * (nr - 1L) + row_index
    }
    z_edge_id = function(row_index, col_index) {
      x_edge_count + (col_index - 1L) * nr + row_index
    }
    diag_edge_id = function(cell_index) {
      x_edge_count + z_edge_count + cell_index
    }

    face_edges = matrix(NA_integer_, nrow = face_count, ncol = 3L)
    face_edges[top_face, ] = cbind(
      z_edge_id(cell_row[render_cell], cell_col[render_cell]),
      diag_edge_id(render_cell),
      x_edge_id(cell_row[render_cell], cell_col[render_cell])
    )
    face_edges[bottom_face, ] = cbind(
      diag_edge_id(render_cell),
      x_edge_id(cell_row[render_cell], cell_col[render_cell] + 1L),
      z_edge_id(cell_row[render_cell] + 1L, cell_col[render_cell])
    )

    slot_neighbor = function(slots) {
      out = integer(length(slots))
      keep = slots > 0L
      out[keep] = slot_to_face[slots[keep]]
      out
    }
    top_neighbor_1 = ifelse(
      cell_row[render_cell] > 1L,
      2L * (render_cell - 1L),
      0L
    )
    top_neighbor_2 = bottom_slots
    top_neighbor_3 = ifelse(
      cell_col[render_cell] > 1L,
      2L * (render_cell - (nr - 1L)),
      0L
    )
    bottom_neighbor_1 = top_slots
    bottom_neighbor_2 = ifelse(
      cell_col[render_cell] < nc - 1L,
      2L * (render_cell + (nr - 1L)) - 1L,
      0L
    )
    bottom_neighbor_3 = ifelse(
      cell_row[render_cell] < nr - 1L,
      2L * (render_cell + 1L) - 1L,
      0L
    )
    face_neighbors = matrix(0L, nrow = face_count, ncol = 3L)
    face_neighbors[top_face, ] = cbind(
      slot_neighbor(top_neighbor_1),
      slot_neighbor(top_neighbor_2),
      slot_neighbor(top_neighbor_3)
    )
    face_neighbors[bottom_face, ] = cbind(
      slot_neighbor(bottom_neighbor_1),
      slot_neighbor(bottom_neighbor_2),
      slot_neighbor(bottom_neighbor_3)
    )
    face_cells = cbind(face_cell_row, face_cell_col)
    face_heights = matrix(vertices[as.vector(faces), "h"], ncol = 3L)
    face_projected_area = rep(0.5, face_count)
    cell_face_id = matrix(0L, nrow = length(cell_id), ncol = 2L)
    cell_face_id[render_cell, ] = cbind(top_face, bottom_face)
  } else {
    faces = matrix(nrow = 0, ncol = 3L)
    face_edges = matrix(nrow = 0, ncol = 3L)
    face_neighbors = matrix(nrow = 0, ncol = 3L)
    face_cells = matrix(nrow = 0, ncol = 2L)
    face_cell_row = integer()
    face_cell_col = integer()
    face_cell_id = integer()
    face_type = integer()
    face_heights = matrix(nrow = 0, ncol = 3L)
    face_projected_area = numeric()
    cell_face_id = matrix(0L, nrow = length(cell_id), ncol = 2L)
  }

  edge_face_count = if (edge_count) {
    tabulate(as.integer(c(face_edges)), nbins = edge_count)
  } else {
    integer()
  }
  edge_first_face = integer(edge_count)
  edge_first_face_edge = integer(edge_count)
  if (face_count && edge_count) {
    edge_first_face[as.integer(c(t(face_edges)))] =
      rep(seq_len(face_count), each = 3L)
    edge_first_face_edge[as.integer(c(t(face_edges)))] =
      rep(seq_len(3L), times = face_count)
  }

  list(
    vertices = vertices,
    faces = faces,
    face_edges = face_edges,
    face_neighbors = face_neighbors,
    face_cells = face_cells,
    face_cell_row = face_cell_row,
    face_cell_col = face_cell_col,
    face_cell_id = face_cell_id,
    face_type = face_type,
    face_heights = face_heights,
    face_projected_area = face_projected_area,
    cell_face_id = cell_face_id,
    edge_vertices = edge_vertices,
    edge_face_count = edge_face_count,
    edge_first_face = edge_first_face,
    edge_first_face_edge = edge_first_face_edge,
    edge_min_height = if (edge_count) {
      pmin(
        vertices[edge_vertices[, 1L], "h"],
        vertices[edge_vertices[, 2L], "h"]
      )
    } else {
      numeric()
    },
    nr = nr,
    nc = nc,
    x_edge_count = x_edge_count,
    z_edge_count = z_edge_count,
    diag_edge_count = diag_edge_count
  )
}

#'@keywords internal
spatial_water_fixed_grid_edge_key = function(vertex_a, vertex_b) {
  if (vertex_a < vertex_b) {
    paste0(vertex_a, ":", vertex_b)
  } else {
    paste0(vertex_b, ":", vertex_a)
  }
}

#'@keywords internal
spatial_water_original_vertices_edge_id = function(
  terrain_mesh,
  vertex_a,
  vertex_b
) {
  if (!is.finite(vertex_a) || !is.finite(vertex_b) || vertex_a == vertex_b) {
    return(NA_integer_)
  }
  nr = terrain_mesh$nr
  row_a = (vertex_a - 1L) %% nr + 1L
  row_b = (vertex_b - 1L) %% nr + 1L
  col_a = (vertex_a - 1L) %/% nr + 1L
  col_b = (vertex_b - 1L) %/% nr + 1L
  if (col_a == col_b && abs(row_a - row_b) == 1L) {
    return((col_a - 1L) * (nr - 1L) + min(row_a, row_b))
  }
  if (row_a == row_b && abs(col_a - col_b) == 1L) {
    return(
      terrain_mesh$x_edge_count +
        (min(col_a, col_b) - 1L) * nr +
        row_a
    )
  }
  if (abs(row_a - row_b) == 1L && abs(col_a - col_b) == 1L) {
    left_row = if (col_a < col_b) row_a else row_b
    right_row = if (col_a < col_b) row_b else row_a
    if (left_row > right_row) {
      cell_row = right_row
      cell_col = min(col_a, col_b)
      return(
        terrain_mesh$x_edge_count +
          terrain_mesh$z_edge_count +
          cell_row +
          (cell_col - 1L) * (nr - 1L)
      )
    }
  }
  NA_integer_
}

#'@keywords internal
spatial_water_component_mask_metrics = function(component_mask) {
  seed = make_spatial_water_component_seed(component_mask)
  area = sum((seed$x1 - seed$x0) * (seed$z1 - seed$z0))
  perimeter = spatial_water_component_seed_perimeter(
    component_mask = component_mask,
    seed = seed
  )
  list(area = area, perimeter = perimeter)
}

#'@keywords internal
make_spatial_water_component_seed = function(
  component_mask,
  terrain_mesh = NULL
) {
  component_cells = which(component_mask, arr.ind = TRUE)
  if (!nrow(component_cells)) {
    seed = list(
      row = integer(),
      col = integer(),
      x0 = numeric(),
      x1 = numeric(),
      z0 = numeric(),
      z1 = numeric(),
      mask = component_mask,
      seed_face_ids = integer(),
      seed_candidate_count = 0L,
      full_scene = FALSE,
      state = new.env(parent = emptyenv())
    )
    return(seed)
  }
  nr = nrow(component_mask)
  nc = ncol(component_mask)
  row_index = component_cells[, 1L]
  col_index = component_cells[, 2L]
  x_center = row_index - 1 - (nr - 1) / 2
  z_center = col_index - 1 - (nc - 1) / 2
  x0 = pmax(x_center - 0.5, -(nr - 1) / 2)
  x1 = pmin(x_center + 0.5, (nr - 1) / 2)
  z0 = pmax(z_center - 0.5, -(nc - 1) / 2)
  z1 = pmin(z_center + 0.5, (nc - 1) / 2)
  keep = x1 > x0 & z1 > z0
  seed_face_ids = integer()
  seed_candidate_count = 0L
  state = new.env(parent = emptyenv())
  if (!is.null(terrain_mesh) && length(row_index)) {
    candidate_row = c(row_index - 1L, row_index - 1L, row_index, row_index)
    candidate_col = c(col_index - 1L, col_index, col_index - 1L, col_index)
    valid_candidate = candidate_row >= 1L &
      candidate_row < nr &
      candidate_col >= 1L &
      candidate_col < nc
    if (any(valid_candidate)) {
      candidate_cell_id = candidate_row[valid_candidate] +
        (candidate_col[valid_candidate] - 1L) * (nr - 1L)
      candidate_cell_id = unique(candidate_cell_id)
      candidate_faces = as.integer(terrain_mesh$cell_face_id[
        candidate_cell_id,
        ,
        drop = FALSE
      ])
      seed_face_ids = sort(unique(candidate_faces[candidate_faces > 0L]))
    }
    seed_candidate_count = length(seed_face_ids)
    state$visited_generation = integer(nrow(terrain_mesh$faces))
    state$generation = 0L
    state$queue = integer(nrow(terrain_mesh$faces))
  }
  list(
    row = row_index[keep],
    col = col_index[keep],
    x0 = x0[keep],
    x1 = x1[keep],
    z0 = z0[keep],
    z1 = z1[keep],
    mask = component_mask,
    seed_face_ids = seed_face_ids,
    seed_candidate_count = seed_candidate_count,
    full_scene = all(component_mask),
    state = state
  )
}

#'@keywords internal
spatial_water_component_seed_perimeter = function(component_mask, seed) {
  if (!length(seed$row)) {
    return(0)
  }
  perimeter = 0
  for (cell_index in seq_along(seed$row)) {
    row_index = seed$row[cell_index]
    col_index = seed$col[cell_index]
    side_height = seed$z1[cell_index] - seed$z0[cell_index]
    side_width = seed$x1[cell_index] - seed$x0[cell_index]
    if (row_index == 1L || !component_mask[row_index - 1L, col_index]) {
      perimeter = perimeter + side_height
    }
    if (
      row_index == nrow(component_mask) ||
        !component_mask[row_index + 1L, col_index]
    ) {
      perimeter = perimeter + side_height
    }
    if (col_index == 1L || !component_mask[row_index, col_index - 1L]) {
      perimeter = perimeter + side_width
    }
    if (
      col_index == ncol(component_mask) ||
        !component_mask[row_index, col_index + 1L]
    ) {
      perimeter = perimeter + side_width
    }
  }
  perimeter
}

#'@keywords internal
make_spatial_water_triangle_clipped_component = function(
  component_mask,
  terrain_mesh,
  water_level
) {
  component_seed = make_spatial_water_component_seed(
    component_mask,
    terrain_mesh = terrain_mesh
  )
  clipped = evaluate_spatial_water_triangle_clipped_component(
    terrain_mesh = terrain_mesh,
    component_seed = component_seed,
    water_level = water_level,
    target_area_limit = Inf,
    build_geometry = TRUE
  )
  if (!isTRUE(clipped$has_geometry)) {
    return(empty_spatial_water_polygon_mesh())
  }
  list(
    vertices = rbind(clipped$top_vertices, clipped$side_vertices),
    lines = clipped$lines,
    top_vertices = clipped$top_vertices,
    side_vertices = clipped$side_vertices,
    top_vertex_table = clipped$top_vertex_table,
    top_faces = clipped$top_faces,
    boundary_edges = clipped$boundary_edges
  )
}

#'@keywords internal
evaluate_spatial_water_triangle_clipped_component = function(
  terrain_mesh,
  component_seed,
  water_level,
  target_area_limit = Inf,
  build_geometry = FALSE,
  return_face_ids = FALSE,
  diagnostics = FALSE
) {
  face_count = nrow(terrain_mesh$faces)
  if (!face_count || !length(component_seed$row)) {
    return(spatial_water_empty_triangle_clip_evaluation(build_geometry))
  }
  if (
    !length(component_seed$seed_face_ids) &&
      !is.null(component_seed$mask)
  ) {
    component_seed = make_spatial_water_component_seed(
      component_seed$mask,
      terrain_mesh = terrain_mesh
    )
  }
  tolerances = spatial_water_triangle_clip_tolerances(
    water_level = water_level,
    heights = terrain_mesh$vertices[, "h"],
    target_area = target_area_limit
  )
  traversal = spatial_water_traverse_seeded_clipped_faces(
    terrain_mesh = terrain_mesh,
    component_seed = component_seed,
    water_level = water_level,
    target_area_limit = target_area_limit,
    tolerances = tolerances,
    return_face_ids = isTRUE(build_geometry) || isTRUE(return_face_ids)
  )
  result = list(
    area = traversal$area,
    rejected = isTRUE(traversal$rejected),
    has_geometry = FALSE
  )
  if (isTRUE(return_face_ids)) {
    result$face_ids = traversal$face_ids
  }
  if (isTRUE(diagnostics)) {
    result$diagnostics = traversal$diagnostics
  }
  if (!isTRUE(build_geometry)) {
    return(result)
  }
  geometry = build_spatial_water_triangle_clipped_geometry(
    terrain_mesh = terrain_mesh,
    selected_face_ids = traversal$face_ids,
    water_level = water_level,
    tolerances = tolerances
  )
  result$has_geometry = nrow(geometry$top_vertices) > 0
  c(result, geometry)
}

#'@keywords internal
spatial_water_empty_triangle_clip_evaluation = function(
  build_geometry = FALSE
) {
  result = list(area = 0, rejected = FALSE, has_geometry = FALSE)
  if (isTRUE(build_geometry)) {
    result = c(
      result,
      list(
        top_vertices = matrix(nrow = 0, ncol = 3),
        side_vertices = matrix(nrow = 0, ncol = 3),
        lines = matrix(nrow = 0, ncol = 3),
        top_vertex_table = matrix(nrow = 0, ncol = 3),
        top_faces = matrix(nrow = 0, ncol = 3),
        boundary_edges = data.frame()
      )
    )
  }
  result
}

#'@keywords internal
spatial_water_level_tolerance = function(water_level, heights) {
  finite_values = c(water_level, heights[is.finite(heights)])
  sqrt(.Machine$double.eps) * max(1, abs(finite_values), na.rm = TRUE)
}

#'@keywords internal
spatial_water_triangle_clip_tolerances = function(
  water_level,
  heights,
  target_area = 1
) {
  height_tol = spatial_water_level_tolerance(
    water_level = water_level,
    heights = heights
  )
  finite_target_area = target_area[is.finite(target_area)]
  area_scale = if (length(finite_target_area)) {
    max(1, finite_target_area, na.rm = TRUE)
  } else {
    1
  }
  list(
    height_tol = height_tol,
    t_tol = sqrt(.Machine$double.eps),
    length_tol = sqrt(.Machine$double.eps),
    area_tol = sqrt(.Machine$double.eps) * area_scale,
    surface_area_tol = sqrt(.Machine$double.eps) *
      max(
        1,
        abs(c(water_level, heights[is.finite(heights)])),
        na.rm = TRUE
      )
  )
}

#'@keywords internal
spatial_water_face_sublevel_area = function(
  terrain_mesh,
  face_ids,
  water_level,
  tolerances
) {
  spatial_water_face_sublevel_area_cpp(
    terrain_mesh = terrain_mesh,
    face_ids = face_ids,
    water_level = water_level,
    height_tol = tolerances$height_tol
  )
}

#'@keywords internal
spatial_water_face_sublevel_area_r = function(
  terrain_mesh,
  face_ids,
  water_level,
  tolerances
) {
  if (!length(face_ids)) {
    return(numeric())
  }
  heights = terrain_mesh$face_heights[face_ids, , drop = FALSE]
  h0 = pmin(heights[, 1L], heights[, 2L], heights[, 3L])
  h2 = pmax(heights[, 1L], heights[, 2L], heights[, 3L])
  h1 = rowSums(heights) - h0 - h2
  area = terrain_mesh$face_projected_area[face_ids]
  fraction = numeric(length(face_ids))
  height_tol = tolerances$height_tol

  flat = abs(h2 - h0) <= height_tol
  fraction[flat & water_level > h0 + height_tol] = 1

  nonflat = !flat
  below_all = nonflat & water_level >= h2 - height_tol
  above_all = nonflat & water_level <= h0 + height_tol
  fraction[below_all] = 1

  partial = nonflat & !below_all & !above_all
  if (any(partial)) {
    lower_flat = abs(h1 - h0) <= height_tol
    upper_flat = abs(h2 - h1) <= height_tol

    first_band = partial & water_level < h1 & !lower_flat
    if (any(first_band)) {
      fraction[first_band] =
        ((water_level - h0[first_band]) / (h1[first_band] - h0[first_band])) *
        ((water_level - h0[first_band]) / (h2[first_band] - h0[first_band]))
    }

    lower_flat_band = partial & water_level < h1 & lower_flat
    if (any(lower_flat_band)) {
      fraction[lower_flat_band] = 1 -
        ((h2[lower_flat_band] - water_level) /
          (h2[lower_flat_band] - h0[lower_flat_band]))^2
    }

    second_band = partial & water_level >= h1 & !upper_flat
    if (any(second_band)) {
      fraction[second_band] = 1 -
        ((h2[second_band] - water_level) /
          (h2[second_band] - h0[second_band])) *
          ((h2[second_band] - water_level) /
            (h2[second_band] - h1[second_band]))
    }

    upper_flat_band = partial & water_level >= h1 & upper_flat
    if (any(upper_flat_band)) {
      fraction[upper_flat_band] =
        ((water_level - h0[upper_flat_band]) /
          (h2[upper_flat_band] - h0[upper_flat_band]))^2
    }
  }

  pmax(0, pmin(1, fraction)) * area
}

#'@keywords internal
spatial_water_face_sublevel_area_scalar = function(
  terrain_mesh,
  face_id,
  water_level,
  tolerances
) {
  heights = terrain_mesh$face_heights[face_id, ]
  h0 = min(heights)
  h2 = max(heights)
  h1 = sum(heights) - h0 - h2
  face_area = terrain_mesh$face_projected_area[face_id]
  height_tol = tolerances$height_tol

  if (abs(h2 - h0) <= height_tol) {
    return(if (water_level > h0 + height_tol) face_area else 0)
  }
  if (water_level <= h0 + height_tol) {
    return(0)
  }
  if (water_level >= h2 - height_tol) {
    return(face_area)
  }
  if (water_level < h1) {
    if (abs(h1 - h0) <= height_tol) {
      fraction = 1 - ((h2 - water_level) / (h2 - h0))^2
    } else {
      fraction = ((water_level - h0) / (h1 - h0)) *
        ((water_level - h0) / (h2 - h0))
    }
  } else if (abs(h2 - h1) <= height_tol) {
    fraction = ((water_level - h0) / (h2 - h0))^2
  } else {
    fraction = 1 -
      ((h2 - water_level) / (h2 - h0)) *
        ((h2 - water_level) / (h2 - h1))
  }
  max(0, min(1, fraction)) * face_area
}

#'@keywords internal
spatial_water_evaluate_full_scene_component = function(
  terrain_mesh,
  water_level,
  target_area_limit,
  tolerances,
  return_face_ids = FALSE
) {
  face_ids = seq_len(nrow(terrain_mesh$faces))
  face_area = spatial_water_face_sublevel_area(
    terrain_mesh = terrain_mesh,
    face_ids = face_ids,
    water_level = water_level,
    tolerances = tolerances
  )
  flooded = face_area > tolerances$area_tol
  flooded_area = face_area[flooded]
  diagnostics = list(
    seed_candidate_count = length(face_ids),
    seed_face_count = sum(flooded),
    visited_face_count = sum(flooded),
    rejected_early = FALSE,
    geometry_face_count = if (isTRUE(return_face_ids)) sum(flooded) else 0L
  )
  if (is.finite(target_area_limit)) {
    cumulative_area = cumsum(flooded_area)
    rejected_at = which(
      cumulative_area > target_area_limit + tolerances$area_tol
    )
    if (length(rejected_at)) {
      diagnostics$visited_face_count = rejected_at[1L]
      diagnostics$rejected_early = TRUE
      return(list(
        area = cumulative_area[rejected_at[1L]],
        rejected = TRUE,
        face_ids = if (isTRUE(return_face_ids)) {
          face_ids[flooded][seq_len(rejected_at[1L])]
        } else {
          integer()
        },
        diagnostics = diagnostics
      ))
    }
  }
  list(
    area = sum(flooded_area),
    rejected = FALSE,
    face_ids = if (isTRUE(return_face_ids)) face_ids[flooded] else integer(),
    diagnostics = diagnostics
  )
}

#'@keywords internal
spatial_water_traverse_seeded_clipped_faces = function(
  terrain_mesh,
  component_seed,
  water_level,
  target_area_limit,
  tolerances,
  return_face_ids = FALSE
) {
  spatial_water_traverse_seeded_clipped_faces_cpp(
    terrain_mesh = terrain_mesh,
    component_seed = component_seed,
    water_level = water_level,
    target_area_limit = target_area_limit,
    height_tol = tolerances$height_tol,
    length_tol = tolerances$length_tol,
    area_tol = tolerances$area_tol,
    return_face_ids = isTRUE(return_face_ids)
  )
}

#'@keywords internal
spatial_water_traverse_seeded_clipped_faces_r = function(
  terrain_mesh,
  component_seed,
  water_level,
  target_area_limit,
  tolerances,
  return_face_ids = FALSE
) {
  face_count = nrow(terrain_mesh$faces)
  diagnostics = list(
    seed_candidate_count = length(component_seed$seed_face_ids),
    seed_face_count = 0L,
    visited_face_count = 0L,
    rejected_early = FALSE,
    geometry_face_count = 0L
  )
  if (!face_count || !length(component_seed$seed_face_ids)) {
    return(list(
      area = 0,
      rejected = FALSE,
      face_ids = integer(),
      diagnostics = diagnostics
    ))
  }
  if (isTRUE(component_seed$full_scene)) {
    return(spatial_water_evaluate_full_scene_component(
      terrain_mesh = terrain_mesh,
      water_level = water_level,
      target_area_limit = target_area_limit,
      tolerances = tolerances,
      return_face_ids = return_face_ids
    ))
  }

  state = component_seed$state
  if (
    is.null(state$visited_generation) ||
      length(state$visited_generation) != face_count
  ) {
    state$visited_generation = integer(face_count)
    state$generation = 0L
    state$queue = integer(face_count)
  }
  generation = state$generation + 1L
  if (!is.finite(generation) || generation <= 0L) {
    state$visited_generation[] = 0L
    generation = 1L
  }
  state$generation = generation
  visited_generation = state$visited_generation
  queue = state$queue
  queue_head = 1L
  queue_tail = 0L

  seed_ids = component_seed$seed_face_ids
  seed_area = spatial_water_face_sublevel_area(
    terrain_mesh = terrain_mesh,
    face_ids = seed_ids,
    water_level = water_level,
    tolerances = tolerances
  )
  seed_ids = seed_ids[seed_area > tolerances$area_tol]
  if (length(seed_ids)) {
    for (face_id in seed_ids) {
      if (
        visited_generation[face_id] != generation &&
          spatial_water_seed_face_has_positive_overlap(
            terrain_mesh = terrain_mesh,
            component_seed = component_seed,
            face_id = face_id,
            water_level = water_level,
            tolerances = tolerances
          )
      ) {
        queue_tail = queue_tail + 1L
        queue[queue_tail] = face_id
        visited_generation[face_id] = generation
      }
    }
  }
  diagnostics$seed_face_count = queue_tail
  if (!queue_tail) {
    state$visited_generation = visited_generation
    state$queue = queue
    return(list(
      area = 0,
      rejected = FALSE,
      face_ids = integer(),
      diagnostics = diagnostics
    ))
  }

  accumulated_area = 0
  while (queue_head <= queue_tail) {
    face_id = queue[queue_head]
    queue_head = queue_head + 1L
    accumulated_area = accumulated_area +
      spatial_water_face_sublevel_area_scalar(
        terrain_mesh = terrain_mesh,
        face_id = face_id,
        water_level = water_level,
        tolerances = tolerances
      )
    if (
      is.finite(target_area_limit) &&
        accumulated_area > target_area_limit + tolerances$area_tol
    ) {
      diagnostics$visited_face_count = queue_head - 1L
      diagnostics$rejected_early = TRUE
      state$visited_generation = visited_generation
      state$queue = queue
      return(list(
        area = accumulated_area,
        rejected = TRUE,
        face_ids = if (isTRUE(return_face_ids)) {
          queue[seq_len(queue_tail)]
        } else {
          integer()
        },
        diagnostics = diagnostics
      ))
    }

    for (edge_index in seq_len(3L)) {
      neighbor = terrain_mesh$face_neighbors[face_id, edge_index]
      if (!neighbor || visited_generation[neighbor] == generation) {
        next
      }
      edge_id = terrain_mesh$face_edges[face_id, edge_index]
      if (
        terrain_mesh$edge_min_height[edge_id] <
          water_level - tolerances$height_tol
      ) {
        queue_tail = queue_tail + 1L
        queue[queue_tail] = neighbor
        visited_generation[neighbor] = generation
      }
    }
  }

  diagnostics$visited_face_count = queue_tail
  diagnostics$geometry_face_count = if (isTRUE(return_face_ids)) {
    queue_tail
  } else {
    0L
  }
  state$visited_generation = visited_generation
  state$queue = queue
  list(
    area = accumulated_area,
    rejected = FALSE,
    face_ids = if (isTRUE(return_face_ids)) {
      queue[seq_len(queue_tail)]
    } else {
      integer()
    },
    diagnostics = diagnostics
  )
}

#'@keywords internal
spatial_water_seed_face_has_positive_overlap = function(
  terrain_mesh,
  component_seed,
  face_id,
  water_level,
  tolerances
) {
  if (isTRUE(component_seed$full_scene)) {
    return(TRUE)
  }
  face_vertices = terrain_mesh$faces[face_id, ]
  face_rows = (face_vertices - 1L) %% terrain_mesh$nr + 1L
  face_cols = (face_vertices - 1L) %/% terrain_mesh$nr + 1L
  masked_vertices = component_seed$mask[cbind(face_rows, face_cols)]
  if (
    any(
      masked_vertices &
        terrain_mesh$vertices[face_vertices, "h"] <
          water_level - tolerances$height_tol
    )
  ) {
    return(TRUE)
  }
  polygon = spatial_water_face_clipped_xz_polygon(
    terrain_mesh = terrain_mesh,
    face_id = face_id,
    water_level = water_level,
    tolerances = tolerances
  )
  if (nrow(polygon) < 3L) {
    return(FALSE)
  }
  row_index = terrain_mesh$face_cell_row[face_id]
  col_index = terrain_mesh$face_cell_col[face_id]
  candidate_rows = c(row_index, row_index + 1L, row_index, row_index + 1L)
  candidate_cols = c(col_index, col_index, col_index + 1L, col_index + 1L)
  for (candidate_index in seq_along(candidate_rows)) {
    mask_row = candidate_rows[candidate_index]
    mask_col = candidate_cols[candidate_index]
    if (!component_seed$mask[mask_row, mask_col]) {
      next
    }
    bounds = spatial_water_seed_cell_bounds(
      row_index = mask_row,
      col_index = mask_col,
      nr = terrain_mesh$nr,
      nc = terrain_mesh$nc
    )
    clipped = clip_spatial_water_polygon_to_bounds(
      polygon = cbind(polygon[, 1L], 0, polygon[, 2L]),
      x0 = bounds$x0,
      x1 = bounds$x1,
      z0 = bounds$z0,
      z1 = bounds$z1
    )
    if (
      nrow(clipped) >= 3L &&
        spatial_water_projected_polygon_area(clipped[, 1L], clipped[, 3L]) >
          tolerances$area_tol
    ) {
      return(TRUE)
    }
  }
  FALSE
}

#'@keywords internal
spatial_water_seed_cell_bounds = function(row_index, col_index, nr, nc) {
  x_center = row_index - 1 - (nr - 1) / 2
  z_center = col_index - 1 - (nc - 1) / 2
  list(
    x0 = max(x_center - 0.5, -(nr - 1) / 2),
    x1 = min(x_center + 0.5, (nr - 1) / 2),
    z0 = max(z_center - 0.5, -(nc - 1) / 2),
    z1 = min(z_center + 0.5, (nc - 1) / 2)
  )
}

#'@keywords internal
spatial_water_face_clipped_xz_polygon = function(
  terrain_mesh,
  face_id,
  water_level,
  tolerances
) {
  vertices = terrain_mesh$faces[face_id, ]
  points = terrain_mesh$vertices[vertices, , drop = FALSE]
  heights = points[, "h"]
  below = heights < water_level - tolerances$height_tol
  on_plane = abs(heights - water_level) <= tolerances$height_tol
  inside = below | on_plane
  if (!any(below) || all(on_plane)) {
    return(matrix(nrow = 0, ncol = 2L))
  }
  records = cbind(points[, "x"], points[, "z"], heights)
  if (all(inside)) {
    return(records[, 1:2, drop = FALSE])
  }
  clipped = matrix(NA_real_, nrow = 4L, ncol = 2L)
  clipped_count = 0L
  for (point_index in seq_len(3L)) {
    next_index = if (point_index == 3L) 1L else point_index + 1L
    current_inside = inside[point_index]
    next_inside = inside[next_index]
    if (current_inside && next_inside) {
      clipped_count = clipped_count + 1L
      clipped[clipped_count, ] = records[next_index, 1:2]
    } else if (current_inside != next_inside) {
      delta = heights[next_index] - heights[point_index]
      if (abs(delta) <= tolerances$height_tol) {
        t = 0
      } else {
        t = (water_level - heights[point_index]) / delta
      }
      t = min(1, max(0, t))
      clipped_count = clipped_count + 1L
      clipped[clipped_count, ] =
        (1 - t) * records[point_index, 1:2] + t * records[next_index, 1:2]
      if (!current_inside && next_inside) {
        clipped_count = clipped_count + 1L
        clipped[clipped_count, ] = records[next_index, 1:2]
      }
    }
  }
  if (!clipped_count) {
    return(matrix(nrow = 0, ncol = 2L))
  }
  spatial_water_clean_xz_polygon(
    clipped[seq_len(clipped_count), , drop = FALSE],
    tolerances = tolerances
  )
}

#'@keywords internal
spatial_water_clean_xz_polygon = function(polygon, tolerances) {
  if (!nrow(polygon)) {
    return(polygon)
  }
  keep = rep(TRUE, nrow(polygon))
  for (point_index in seq_len(nrow(polygon))) {
    previous_index = if (point_index == 1L) nrow(polygon) else point_index - 1L
    keep[point_index] = sqrt(sum(
      (polygon[point_index, ] - polygon[previous_index, ])^2
    )) >
      tolerances$length_tol
  }
  polygon[keep, , drop = FALSE]
}

#'@keywords internal
spatial_water_edge_intersections = function(
  terrain_mesh,
  water_level,
  tolerances
) {
  edge_count = nrow(terrain_mesh$edge_vertices)
  if (!edge_count) {
    return(list(
      has = logical(),
      t = numeric(),
      x = numeric(),
      z = numeric()
    ))
  }
  edge_vertices = terrain_mesh$edge_vertices
  h0 = terrain_mesh$vertices[edge_vertices[, 1L], "h"]
  h1 = terrain_mesh$vertices[edge_vertices[, 2L], "h"]
  delta = h1 - h0
  finite_edge = is.finite(h0) & is.finite(h1)
  crosses = finite_edge &
    abs(delta) > tolerances$height_tol &
    pmin(h0, h1) < water_level - tolerances$height_tol &
    pmax(h0, h1) > water_level + tolerances$height_tol
  t = numeric(edge_count)
  t[crosses] = (water_level - h0[crosses]) / delta[crosses]
  t = pmax(0, pmin(1, t))
  p0 = terrain_mesh$vertices[edge_vertices[, 1L], , drop = FALSE]
  p1 = terrain_mesh$vertices[edge_vertices[, 2L], , drop = FALSE]
  list(
    has = crosses,
    t = t,
    x = (1 - t) * p0[, "x"] + t * p1[, "x"],
    z = (1 - t) * p0[, "z"] + t * p1[, "z"]
  )
}

#'@keywords internal
clip_spatial_water_terrain_face_to_level = function(
  terrain_mesh,
  face_index,
  water_level,
  tolerances,
  edge_intersections
) {
  face_vertices = terrain_mesh$faces[face_index, ]
  heights = terrain_mesh$vertices[face_vertices, "h"]
  below = heights < water_level - tolerances$height_tol
  on_plane = abs(heights - water_level) <= tolerances$height_tol
  inside = below | on_plane
  if (!any(below) || all(on_plane)) {
    return(NULL)
  }

  records = spatial_water_original_vertex_records(
    terrain_mesh = terrain_mesh,
    vertex_ids = face_vertices
  )
  if (all(inside)) {
    return(clean_spatial_water_clipped_records(
      records,
      tolerances = tolerances
    ))
  }

  clipped = records[FALSE, , drop = FALSE]
  face_edges = terrain_mesh$face_edges[face_index, ]
  for (point_index in seq_len(3L)) {
    next_index = if (point_index == 3L) 1L else point_index + 1L
    current_inside = inside[point_index]
    next_inside = inside[next_index]
    if (current_inside && next_inside) {
      clipped = rbind(clipped, records[next_index, , drop = FALSE])
    } else if (current_inside && !next_inside) {
      clipped = rbind(
        clipped,
        spatial_water_edge_plane_intersection_record(
          terrain_mesh = terrain_mesh,
          edge_id = face_edges[point_index],
          vertex_a = face_vertices[point_index],
          vertex_b = face_vertices[next_index],
          water_level = water_level,
          tolerances = tolerances,
          edge_intersections = edge_intersections
        )
      )
    } else if (!current_inside && next_inside) {
      clipped = rbind(
        clipped,
        spatial_water_edge_plane_intersection_record(
          terrain_mesh = terrain_mesh,
          edge_id = face_edges[point_index],
          vertex_a = face_vertices[point_index],
          vertex_b = face_vertices[next_index],
          water_level = water_level,
          tolerances = tolerances,
          edge_intersections = edge_intersections
        ),
        records[next_index, , drop = FALSE]
      )
    }
  }
  clipped = clean_spatial_water_clipped_records(
    clipped,
    tolerances = tolerances
  )
  if (nrow(clipped) < 3L) {
    return(NULL)
  }
  clipped
}

#'@keywords internal
spatial_water_original_vertex_records = function(terrain_mesh, vertex_ids) {
  vertex_ids = as.integer(vertex_ids)
  vertex_data = terrain_mesh$vertices[vertex_ids, , drop = FALSE]
  data.frame(
    key = vertex_ids,
    kind = "vertex",
    id = vertex_ids,
    x = vertex_data[, "x"],
    h = vertex_data[, "h"],
    z = vertex_data[, "z"],
    edge_id = NA_integer_,
    t = NA_real_
  )
}

#'@keywords internal
spatial_water_edge_plane_intersection_record = function(
  terrain_mesh,
  edge_id,
  vertex_a,
  vertex_b,
  water_level,
  tolerances,
  edge_intersections
) {
  height_a = terrain_mesh$vertices[vertex_a, "h"]
  height_b = terrain_mesh$vertices[vertex_b, "h"]
  delta = height_b - height_a
  t_ab = if (abs(delta) <= tolerances$height_tol) {
    0
  } else {
    (water_level - height_a) / delta
  }
  if (t_ab <= tolerances$t_tol) {
    return(spatial_water_original_vertex_records(terrain_mesh, vertex_a))
  }
  if (t_ab >= 1 - tolerances$t_tol) {
    return(spatial_water_original_vertex_records(terrain_mesh, vertex_b))
  }

  if (edge_intersections$has[edge_id]) {
    t = edge_intersections$t[edge_id]
    x = edge_intersections$x[edge_id]
    z = edge_intersections$z[edge_id]
  } else {
    edge_vertices = terrain_mesh$edge_vertices[edge_id, ]
    height_0 = terrain_mesh$vertices[edge_vertices[1L], "h"]
    height_1 = terrain_mesh$vertices[edge_vertices[2L], "h"]
    edge_delta = height_1 - height_0
    t = if (abs(edge_delta) <= tolerances$height_tol) {
      0.5
    } else {
      (water_level - height_0) / edge_delta
    }
    point_0 = terrain_mesh$vertices[edge_vertices[1L], , drop = FALSE]
    point_1 = terrain_mesh$vertices[edge_vertices[2L], , drop = FALSE]
    point = (1 - t) * point_0 + t * point_1
    x = point[, "x"]
    z = point[, "z"]
  }
  data.frame(
    key = nrow(terrain_mesh$vertices) + edge_id,
    kind = "edge",
    id = edge_id,
    x = x,
    h = water_level,
    z = z,
    edge_id = edge_id,
    t = t
  )
}

#'@keywords internal
clean_spatial_water_clipped_records = function(records, tolerances) {
  if (!nrow(records)) {
    return(records)
  }
  keep = rep(TRUE, nrow(records))
  for (point_index in seq_len(nrow(records))) {
    previous_index = if (point_index == 1L) nrow(records) else point_index - 1L
    same_key = records$key[point_index] == records$key[previous_index]
    same_point =
      sqrt(
        (records$x[point_index] - records$x[previous_index])^2 +
          (records$z[point_index] - records$z[previous_index])^2
      ) <=
        tolerances$length_tol &&
      abs(records$h[point_index] - records$h[previous_index]) <=
        tolerances$height_tol
    keep[point_index] = !(same_key || same_point)
  }
  records[keep, , drop = FALSE]
}

#'@keywords internal
spatial_water_projected_records_area = function(records) {
  spatial_water_projected_polygon_area(records$x, records$z)
}

#'@keywords internal
spatial_water_projected_polygon_area = function(x, z) {
  if (length(x) < 3L) {
    return(0)
  }
  next_index = c(seq.int(2L, length(x)), 1L)
  abs(sum(x * z[next_index] - z * x[next_index])) / 2
}

#'@keywords internal
spatial_water_records_overlap_component_seed = function(
  records,
  component_seed,
  tol
) {
  if (!length(component_seed$row)) {
    return(FALSE)
  }
  x_min = min(records$x)
  x_max = max(records$x)
  z_min = min(records$z)
  z_max = max(records$z)
  candidates = which(
    component_seed$x1 > x_min + tol &
      component_seed$x0 < x_max - tol &
      component_seed$z1 > z_min + tol &
      component_seed$z0 < z_max - tol
  )
  if (!length(candidates)) {
    return(FALSE)
  }
  polygon = cbind(records$x, 0, records$z)
  for (candidate in candidates) {
    clipped = clip_spatial_water_polygon_to_bounds(
      polygon = polygon,
      x0 = component_seed$x0[candidate],
      x1 = component_seed$x1[candidate],
      z0 = component_seed$z0[candidate],
      z1 = component_seed$z1[candidate]
    )
    if (
      nrow(clipped) >= 3L &&
        spatial_water_projected_polygon_area(clipped[, 1], clipped[, 3]) > tol
    ) {
      return(TRUE)
    }
  }
  FALSE
}

#'@keywords internal
spatial_water_records_original_edge_segments = function(
  records,
  terrain_mesh,
  tolerances
) {
  if (nrow(records) < 2L) {
    return(data.frame(edge_id = integer(), t0 = numeric(), t1 = numeric()))
  }
  segments = vector("list", nrow(records))
  segment_count = 0L
  for (point_index in seq_len(nrow(records))) {
    next_index = if (point_index == nrow(records)) 1L else point_index + 1L
    edge_id = spatial_water_records_shared_original_edge_id(
      records[point_index, , drop = FALSE],
      records[next_index, , drop = FALSE],
      terrain_mesh = terrain_mesh
    )
    if (!is.finite(edge_id)) {
      next
    }
    t0 = spatial_water_record_edge_t(
      records[point_index, , drop = FALSE],
      edge_id = edge_id,
      terrain_mesh = terrain_mesh
    )
    t1 = spatial_water_record_edge_t(
      records[next_index, , drop = FALSE],
      edge_id = edge_id,
      terrain_mesh = terrain_mesh
    )
    edge_length = spatial_water_terrain_edge_projected_length(
      terrain_mesh = terrain_mesh,
      edge_id = edge_id
    )
    if (
      !is.finite(t0) ||
        !is.finite(t1) ||
        abs(t1 - t0) * edge_length <= tolerances$length_tol
    ) {
      next
    }
    segment_count = segment_count + 1L
    segments[[segment_count]] = data.frame(
      edge_id = edge_id,
      t0 = t0,
      t1 = t1
    )
  }
  if (!segment_count) {
    return(data.frame(edge_id = integer(), t0 = numeric(), t1 = numeric()))
  }
  do.call(rbind, segments[seq_len(segment_count)])
}

#'@keywords internal
spatial_water_records_shared_original_edge_id = function(
  record_a,
  record_b,
  terrain_mesh
) {
  kind_a = record_a$kind[1L]
  kind_b = record_b$kind[1L]
  if (identical(kind_a, "edge") && identical(kind_b, "edge")) {
    if (record_a$edge_id[1L] == record_b$edge_id[1L]) {
      return(record_a$edge_id[1L])
    }
    return(NA_integer_)
  }
  if (identical(kind_a, "vertex") && identical(kind_b, "vertex")) {
    return(
      spatial_water_original_vertices_edge_id(
        terrain_mesh = terrain_mesh,
        vertex_a = record_a$id[1L],
        vertex_b = record_b$id[1L]
      )
    )
  }
  vertex_record = if (identical(kind_a, "vertex")) record_a else record_b
  edge_record = if (identical(kind_a, "edge")) record_a else record_b
  edge_vertices = terrain_mesh$edge_vertices[edge_record$edge_id[1L], ]
  if (vertex_record$id[1L] %in% edge_vertices) {
    return(edge_record$edge_id[1L])
  }
  NA_integer_
}

#'@keywords internal
spatial_water_record_edge_t = function(record, edge_id, terrain_mesh) {
  if (identical(record$kind[1L], "edge")) {
    if (record$edge_id[1L] == edge_id) {
      return(record$t[1L])
    }
    return(NA_real_)
  }
  edge_vertices = terrain_mesh$edge_vertices[edge_id, ]
  if (record$id[1L] == edge_vertices[1L]) {
    return(0)
  }
  if (record$id[1L] == edge_vertices[2L]) {
    return(1)
  }
  NA_real_
}

#'@keywords internal
spatial_water_terrain_edge_projected_length = function(terrain_mesh, edge_id) {
  edge_vertices = terrain_mesh$edge_vertices[edge_id, ]
  points = terrain_mesh$vertices[edge_vertices, , drop = FALSE]
  sqrt(diff(points[, "x"])^2 + diff(points[, "z"])^2)
}

#'@keywords internal
spatial_water_selected_clipped_faces = function(
  edge_segments,
  edge_face_count,
  clip_seed,
  clip_count
) {
  if (!clip_count || !any(clip_seed)) {
    return(rep(FALSE, clip_count))
  }
  parent = seq_len(clip_count)
  find_root = function(index) {
    while (parent[index] != index) {
      parent[index] <<- parent[parent[index]]
      index = parent[index]
    }
    index
  }
  union_roots = function(a, b) {
    root_a = find_root(a)
    root_b = find_root(b)
    if (root_a != root_b) {
      parent[root_b] <<- root_a
    }
  }
  if (nrow(edge_segments)) {
    edge_groups = split(edge_segments$clip_id, edge_segments$edge_id)
    for (edge_name in names(edge_groups)) {
      edge_id = as.integer(edge_name)
      if (
        !is.na(edge_id) &&
          edge_id <= length(edge_face_count) &&
          edge_face_count[edge_id] >= 2L
      ) {
        clip_ids = unique(edge_groups[[edge_name]])
        if (length(clip_ids) > 1L) {
          for (clip_id in clip_ids[-1L]) {
            union_roots(clip_ids[1L], clip_id)
          }
        }
      }
    }
  }
  roots = vapply(seq_len(clip_count), find_root, integer(1))
  seed_roots = unique(roots[clip_seed])
  roots %in% seed_roots
}

#'@keywords internal
build_spatial_water_triangle_clipped_geometry = function(
  terrain_mesh,
  water_level,
  tolerances,
  selected_face_ids
) {
  if (
    length(selected_face_ids) == nrow(terrain_mesh$faces) &&
      water_level >= max(terrain_mesh$face_heights) - tolerances$height_tol
  ) {
    selected_flags = rep(FALSE, nrow(terrain_mesh$faces))
    selected_flags[selected_face_ids] = TRUE
    if (all(selected_flags)) {
      return(build_spatial_water_full_terrain_geometry(
        terrain_mesh = terrain_mesh,
        water_level = water_level,
        tolerances = tolerances
      ))
    }
  }
  build_spatial_water_triangle_clipped_geometry_cpp(
    terrain_mesh = terrain_mesh,
    selected_face_ids = selected_face_ids,
    water_level = water_level,
    height_tol = tolerances$height_tol,
    t_tol = tolerances$t_tol,
    length_tol = tolerances$length_tol,
    area_tol = tolerances$area_tol,
    surface_area_tol = tolerances$surface_area_tol
  )
}

#'@keywords internal
build_spatial_water_triangle_clipped_geometry_r = function(
  terrain_mesh,
  water_level,
  tolerances,
  selected_face_ids
) {
  if (!length(selected_face_ids)) {
    return(spatial_water_empty_triangle_clip_evaluation(TRUE)[
      c(
        "top_vertices",
        "side_vertices",
        "lines",
        "top_vertex_table",
        "top_faces",
        "boundary_edges"
      )
    ])
  }
  if (
    length(selected_face_ids) == nrow(terrain_mesh$faces) &&
      water_level >= max(terrain_mesh$face_heights) - tolerances$height_tol
  ) {
    selected_flags = rep(FALSE, nrow(terrain_mesh$faces))
    selected_flags[selected_face_ids] = TRUE
    if (all(selected_flags)) {
      return(build_spatial_water_full_terrain_geometry(
        terrain_mesh = terrain_mesh,
        water_level = water_level,
        tolerances = tolerances
      ))
    }
  }
  edge_intersections = spatial_water_edge_intersections(
    terrain_mesh = terrain_mesh,
    water_level = water_level,
    tolerances = tolerances
  )
  clip_records = vector("list", length(selected_face_ids))
  clip_count = 0L
  for (face_id in selected_face_ids) {
    records = clip_spatial_water_terrain_face_to_level(
      terrain_mesh = terrain_mesh,
      face_index = face_id,
      water_level = water_level,
      tolerances = tolerances,
      edge_intersections = edge_intersections
    )
    if (is.null(records)) {
      next
    }
    if (spatial_water_projected_records_area(records) <= tolerances$area_tol) {
      next
    }
    clip_count = clip_count + 1L
    clip_records[[clip_count]] = records
  }
  if (!clip_count) {
    return(spatial_water_empty_triangle_clip_evaluation(TRUE)[
      c(
        "top_vertices",
        "side_vertices",
        "lines",
        "top_vertex_table",
        "top_faces",
        "boundary_edges"
      )
    ])
  }
  clip_records = clip_records[seq_len(clip_count)]
  max_key = nrow(terrain_mesh$vertices) + nrow(terrain_mesh$edge_vertices)
  top_index = integer(max_key)
  top_vertex_table = matrix(NA_real_, nrow = max_key, ncol = 3L)
  top_vertex_count = 0L
  top_faces = vector("list", length(clip_records) * 2L)
  top_face_count = 0L
  boundary_env = new.env(parent = emptyenv())

  add_top_vertex = function(record) {
    key = record$key[1L]
    if (!top_index[key]) {
      top_vertex_count <<- top_vertex_count + 1L
      top_index[key] <<- top_vertex_count
      top_vertex_table[top_vertex_count, ] <<- c(
        record$x[1L],
        water_level,
        record$z[1L]
      )
    }
    top_index[key]
  }

  for (clip_id in seq_along(clip_records)) {
    records = clip_records[[clip_id]]
    vertex_indices = integer(nrow(records))
    for (record_index in seq_len(nrow(records))) {
      vertex_indices[record_index] =
        add_top_vertex(records[record_index, , drop = FALSE])
    }
    if (length(vertex_indices) >= 3L) {
      for (triangle_index in seq_len(length(vertex_indices) - 2L)) {
        face = c(
          vertex_indices[1L],
          vertex_indices[triangle_index + 1L],
          vertex_indices[triangle_index + 2L]
        )
        face_points = top_vertex_table[face, , drop = FALSE]
        normal_y = spatial_water_triangle_normal_y(face_points)
        if (abs(normal_y) <= tolerances$area_tol) {
          next
        }
        if (normal_y < 0) {
          face = face[c(1L, 3L, 2L)]
        }
        top_face_count = top_face_count + 1L
        top_faces[[top_face_count]] = face
      }
    }
    spatial_water_register_clipped_boundary_edges(
      records = records,
      terrain_mesh = terrain_mesh,
      boundary_env = boundary_env
    )
  }

  if (!top_vertex_count || !top_face_count) {
    return(spatial_water_empty_triangle_clip_evaluation(TRUE)[
      c(
        "top_vertices",
        "side_vertices",
        "lines",
        "top_vertex_table",
        "top_faces",
        "boundary_edges"
      )
    ])
  }
  top_vertex_table = top_vertex_table[seq_len(top_vertex_count), , drop = FALSE]
  top_faces = do.call(rbind, top_faces[seq_len(top_face_count)])
  top_vertices = top_vertex_table[as.vector(t(top_faces)), , drop = FALSE]
  boundary = spatial_water_triangle_clipped_boundary_geometry(
    terrain_mesh = terrain_mesh,
    boundary_env = boundary_env,
    top_index = top_index,
    top_vertex_table = top_vertex_table,
    water_level = water_level,
    tolerances = tolerances
  )
  list(
    top_vertices = top_vertices,
    side_vertices = boundary$side_vertices,
    lines = boundary$lines,
    top_vertex_table = top_vertex_table,
    top_faces = top_faces,
    boundary_edges = boundary$boundary_edges
  )
}

#'@keywords internal
build_spatial_water_full_terrain_geometry = function(
  terrain_mesh,
  water_level,
  tolerances
) {
  build_spatial_water_full_terrain_geometry_cpp(
    terrain_mesh = terrain_mesh,
    water_level = water_level,
    surface_area_tol = tolerances$surface_area_tol
  )
}

#'@keywords internal
build_spatial_water_full_terrain_geometry_r = function(
  terrain_mesh,
  water_level,
  tolerances
) {
  top_vertex_table = cbind(
    terrain_mesh$vertices[, "x"],
    water_level,
    terrain_mesh$vertices[, "z"]
  )
  top_faces = terrain_mesh$faces
  top_vertices = top_vertex_table[as.vector(t(top_faces)), , drop = FALSE]
  boundary_edge_ids = which(terrain_mesh$edge_face_count == 1L)
  if (!length(boundary_edge_ids)) {
    return(list(
      top_vertices = top_vertices,
      side_vertices = matrix(nrow = 0, ncol = 3),
      lines = matrix(nrow = 0, ncol = 3),
      top_vertex_table = top_vertex_table,
      top_faces = top_faces,
      boundary_edges = data.frame(
        v1 = integer(),
        v2 = integer(),
        kind = character(),
        edge_id = integer(),
        wall = logical()
      )
    ))
  }

  line_indices = as.vector(t(terrain_mesh$edge_vertices[boundary_edge_ids, ]))
  lines = top_vertex_table[line_indices, , drop = FALSE]
  side_vertices = vector("list", length(boundary_edge_ids))
  boundary_edges = vector("list", length(boundary_edge_ids))
  side_count = 0L

  for (edge_index in seq_along(boundary_edge_ids)) {
    edge_id = boundary_edge_ids[edge_index]
    face_id = terrain_mesh$edge_first_face[edge_id]
    face_edge = terrain_mesh$edge_first_face_edge[edge_id]
    face_vertices = terrain_mesh$faces[face_id, ]
    next_edge = if (face_edge == 3L) 1L else face_edge + 1L
    vertex_a = face_vertices[face_edge]
    vertex_b = face_vertices[next_edge]
    record_a = spatial_water_original_vertex_records(terrain_mesh, vertex_a)
    record_b = spatial_water_original_vertex_records(terrain_mesh, vertex_b)
    wall = make_spatial_water_triangle_clipped_sidewall(
      record_a = record_a,
      record_b = record_b,
      water_level = water_level,
      tolerances = tolerances
    )
    has_wall = nrow(wall) > 0
    if (has_wall) {
      side_count = side_count + 1L
      side_vertices[[side_count]] = wall
    }
    boundary_edges[[edge_index]] = data.frame(
      v1 = vertex_a,
      v2 = vertex_b,
      kind = "original",
      edge_id = edge_id,
      wall = has_wall
    )
  }

  list(
    top_vertices = top_vertices,
    side_vertices = if (side_count) {
      do.call(rbind, side_vertices[seq_len(side_count)])
    } else {
      matrix(nrow = 0, ncol = 3)
    },
    lines = lines,
    top_vertex_table = top_vertex_table,
    top_faces = top_faces,
    boundary_edges = do.call(rbind, boundary_edges)
  )
}

#'@keywords internal
spatial_water_triangle_normal_y = function(triangle) {
  first_edge = triangle[2L, ] - triangle[1L, ]
  second_edge = triangle[3L, ] - triangle[1L, ]
  first_edge[3L] * second_edge[1L] - first_edge[1L] * second_edge[3L]
}

#'@keywords internal
spatial_water_register_clipped_boundary_edges = function(
  records,
  terrain_mesh,
  boundary_env
) {
  if (nrow(records) < 2L) {
    return(invisible(NULL))
  }
  for (point_index in seq_len(nrow(records))) {
    next_index = if (point_index == nrow(records)) 1L else point_index + 1L
    record_a = records[point_index, , drop = FALSE]
    record_b = records[next_index, , drop = FALSE]
    if (record_a$key[1L] == record_b$key[1L]) {
      next
    }
    edge_key = spatial_water_fixed_grid_edge_key(
      record_a$key[1L],
      record_b$key[1L]
    )
    entry = get0(edge_key, envir = boundary_env, inherits = FALSE)
    original_edge_id = spatial_water_records_shared_original_edge_id(
      record_a,
      record_b,
      terrain_mesh = terrain_mesh
    )
    if (is.null(entry)) {
      entry = list(
        count = 0L,
        record_a = record_a,
        record_b = record_b,
        kind = if (is.finite(original_edge_id)) "original" else "contour",
        edge_id = original_edge_id
      )
    }
    entry$count = entry$count + 1L
    assign(edge_key, entry, envir = boundary_env)
  }
  invisible(NULL)
}

#'@keywords internal
spatial_water_triangle_clipped_boundary_geometry = function(
  terrain_mesh,
  boundary_env,
  top_index,
  top_vertex_table,
  water_level,
  tolerances
) {
  edge_names = ls(boundary_env)
  line_vertices = vector("list", length(edge_names))
  side_vertices = vector("list", length(edge_names))
  boundary_edges = vector("list", length(edge_names))
  line_count = 0L
  side_count = 0L
  boundary_count = 0L
  for (edge_name in edge_names) {
    entry = get(edge_name, envir = boundary_env, inherits = FALSE)
    if (entry$count != 1L) {
      next
    }
    record_a = entry$record_a
    record_b = entry$record_b
    index_a = top_index[record_a$key[1L]]
    index_b = top_index[record_b$key[1L]]
    if (!index_a || !index_b) {
      next
    }
    line_count = line_count + 1L
    line_vertices[[line_count]] = rbind(
      top_vertex_table[index_a, ],
      top_vertex_table[index_b, ]
    )
    boundary_count = boundary_count + 1L
    boundary_edges[[boundary_count]] = data.frame(
      v1 = index_a,
      v2 = index_b,
      kind = entry$kind,
      edge_id = ifelse(is.finite(entry$edge_id), entry$edge_id, NA_integer_),
      wall = FALSE
    )
    if (
      identical(entry$kind, "original") &&
        is.finite(entry$edge_id) &&
        terrain_mesh$edge_face_count[entry$edge_id] == 1L
    ) {
      wall = make_spatial_water_triangle_clipped_sidewall(
        record_a = record_a,
        record_b = record_b,
        water_level = water_level,
        tolerances = tolerances
      )
      if (nrow(wall)) {
        side_count = side_count + 1L
        side_vertices[[side_count]] = wall
        boundary_edges[[boundary_count]]$wall = TRUE
      }
    }
  }
  list(
    lines = if (line_count) {
      do.call(rbind, line_vertices[seq_len(line_count)])
    } else {
      matrix(nrow = 0, ncol = 3)
    },
    side_vertices = if (side_count) {
      do.call(rbind, side_vertices[seq_len(side_count)])
    } else {
      matrix(nrow = 0, ncol = 3)
    },
    boundary_edges = if (boundary_count) {
      do.call(rbind, boundary_edges[seq_len(boundary_count)])
    } else {
      data.frame(
        v1 = integer(),
        v2 = integer(),
        kind = character(),
        edge_id = integer(),
        wall = logical()
      )
    }
  )
}

#'@keywords internal
make_spatial_water_triangle_clipped_sidewall = function(
  record_a,
  record_b,
  water_level,
  tolerances
) {
  top_a = c(record_a$x[1L], water_level, record_a$z[1L])
  top_b = c(record_b$x[1L], water_level, record_b$z[1L])
  bot_a = c(record_a$x[1L], record_a$h[1L], record_a$z[1L])
  bot_b = c(record_b$x[1L], record_b$h[1L], record_b$z[1L])
  candidates = list(
    rbind(top_a, bot_a, top_b),
    rbind(top_b, bot_a, bot_b)
  )
  keep = vapply(
    candidates,
    function(triangle) {
      spatial_water_triangle_area3d(triangle) > tolerances$surface_area_tol
    },
    logical(1)
  )
  if (!any(keep)) {
    return(matrix(nrow = 0, ncol = 3))
  }
  do.call(rbind, candidates[keep])
}

#'@keywords internal
spatial_water_triangle_area3d = function(triangle) {
  first_edge = triangle[2L, ] - triangle[1L, ]
  second_edge = triangle[3L, ] - triangle[1L, ]
  cross_product = c(
    first_edge[2L] * second_edge[3L] - first_edge[3L] * second_edge[2L],
    first_edge[3L] * second_edge[1L] - first_edge[1L] * second_edge[3L],
    first_edge[1L] * second_edge[2L] - first_edge[2L] * second_edge[1L]
  )
  sqrt(sum(cross_product^2)) / 2
}

#'@keywords internal
make_spatial_water_polygon_component = function(
  component_mask,
  heightmap,
  terrain_mesh = make_spatial_water_fixed_grid_terrain_mesh(heightmap),
  fallback_level
) {
  if (!is.finite(fallback_level)) {
    return(empty_spatial_water_polygon_mesh())
  }
  component_metrics = spatial_water_component_mask_metrics(component_mask)
  if (
    !is.finite(component_metrics$area) ||
      component_metrics$area <= sqrt(.Machine$double.eps)
  ) {
    return(empty_spatial_water_polygon_mesh())
  }
  fit = fit_spatial_water_component_polygon(
    component_mask = component_mask,
    heightmap = heightmap,
    terrain_mesh = terrain_mesh,
    component_metrics = component_metrics,
    fallback_level = fallback_level
  )
  if (is.null(fit) || !is.finite(fit$area) || fit$area <= 0) {
    return(empty_spatial_water_polygon_mesh())
  }
  make_spatial_water_triangle_clipped_component(
    component_mask = component_mask,
    terrain_mesh = terrain_mesh,
    water_level = fit$level
  )
}

#'@keywords internal
fit_spatial_water_component_polygon = function(
  component_mask,
  heightmap,
  component_footprint = NULL,
  terrain_mesh = make_spatial_water_fixed_grid_terrain_mesh(heightmap),
  component_metrics = spatial_water_component_mask_metrics(component_mask),
  fallback_level
) {
  target_area = component_metrics$area
  if (!is.finite(target_area) || target_area <= sqrt(.Machine$double.eps)) {
    return(NULL)
  }
  target_perimeter = component_metrics$perimeter
  target_area_limit = target_area + target_perimeter
  component_seed = make_spatial_water_component_seed(
    component_mask,
    terrain_mesh = terrain_mesh
  )

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
  level_eps = spatial_water_level_tolerance(
    water_level = level_max,
    heights = finite_height
  )
  level_upper = level_max + 2 * level_eps
  start_level = min(max(start_level, level_min), level_upper)

  evaluation_cache = new.env(parent = emptyenv())
  evaluate_level = function(level) {
    spatial_water_component_area_fit_at_level(
      heightmap = heightmap,
      terrain_mesh = terrain_mesh,
      component_mask = component_mask,
      component_seed = component_seed,
      component_footprint = component_footprint,
      level = level,
      target_area = target_area,
      target_area_limit = target_area_limit,
      cache = evaluation_cache
    )
  }

  best = NULL
  update_best = function(candidate) {
    if (
      is.null(candidate) ||
        !is.finite(candidate$area) ||
        candidate$area <= sqrt(.Machine$double.eps) ||
        isTRUE(candidate$rejected)
    ) {
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

  seed_levels = c(
    level_min,
    start_level,
    level_upper,
    fallback_level
  )
  seed_levels = sort(unique(seed_levels[
    is.finite(seed_levels) &
      seed_levels >= level_min &
      seed_levels <= level_upper
  ]))
  for (candidate_level in seed_levels) {
    best = update_best(evaluate_level(candidate_level))
  }

  lower = level_min
  upper = level_upper
  lower_result = evaluate_level(lower)
  upper_result = evaluate_level(upper)
  best = update_best(lower_result)
  best = update_best(upper_result)

  level_tol = spatial_water_level_tolerance(
    water_level = max(abs(c(level_min, level_max)), na.rm = TRUE),
    heights = c(level_min, level_max)
  )
  area_tol = sqrt(.Machine$double.eps) * max(1, target_area)
  max_iterations = if (
    is.finite(upper - lower) &&
      upper > lower &&
      is.finite(level_tol) &&
      level_tol > 0
  ) {
    max(1L, ceiling(log2((upper - lower) / level_tol)))
  } else {
    1L
  }
  max_iterations = min(64L, max_iterations)
  for (iteration in seq_len(max_iterations)) {
    if (!is.finite(lower) || !is.finite(upper) || upper - lower <= level_tol) {
      break
    }
    if (!is.null(best) && best$difference <= area_tol) {
      break
    }
    if (abs(upper_result$area - lower_result$area) <= area_tol) {
      best = update_best(lower_result)
      best = update_best(upper_result)
      break
    }
    mid = (lower + upper) / 2
    if (mid <= lower || mid >= upper) {
      break
    }
    mid_result = evaluate_level(mid)
    best = update_best(mid_result)
    if (
      isTRUE(mid_result$rejected) ||
        mid_result$area >= target_area - area_tol
    ) {
      upper = mid
      upper_result = mid_result
    } else {
      lower = mid
      lower_result = mid_result
    }
  }
  best = update_best(lower_result)
  best = update_best(upper_result)
  best
}

#'@keywords internal
spatial_water_component_area_fit_at_level = function(
  heightmap,
  component_footprint = NULL,
  terrain_mesh = NULL,
  component_mask = NULL,
  component_seed = NULL,
  level,
  target_area,
  target_area_limit,
  cache
) {
  cache_key = format(level, digits = 17)
  if (exists(cache_key, envir = cache, inherits = FALSE)) {
    return(get(cache_key, envir = cache, inherits = FALSE))
  }

  if (!is.null(component_mask)) {
    if (is.null(terrain_mesh)) {
      terrain_mesh = make_spatial_water_fixed_grid_terrain_mesh(heightmap)
    }
    if (is.null(component_seed)) {
      component_seed = make_spatial_water_component_seed(
        component_mask,
        terrain_mesh = terrain_mesh
      )
    }
    evaluation = evaluate_spatial_water_triangle_clipped_component(
      terrain_mesh = terrain_mesh,
      component_seed = component_seed,
      water_level = level,
      target_area_limit = target_area_limit
    )
    result = list(
      level = level,
      polygon = NULL,
      area = evaluation$area,
      difference = abs(evaluation$area - target_area),
      rejected = evaluation$area > target_area_limit + sqrt(.Machine$double.eps)
    )
    assign(cache_key, result, envir = cache)
    return(result)
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
