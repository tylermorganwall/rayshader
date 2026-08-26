#' Render Trail Paths
#'
#' @description Adds trails that follow the terrain surface. Trails use
#' lightweight rgl line previews while caching solid meshes for
#' [render_highquality()]. Each input line is meshed independently, without
#' road grades, lanes, layer rules, intersection solving, or mesh joining.
#'
#' @param trails Spatial line data used to draw trails. Supports `sf`, `sfc`,
#' `sfg`, `SpatialLines`, and `SpatialLinesDataFrame` line inputs.
#' @param color Default `"grey50"`. Trail color.
#' @param width Default `1`. Trail width in the units selected by `width_units`.
#' @param width_column Default `NULL`. Column name in an `sf` trail object used
#' to set per-feature trail widths. Values must be positive finite numbers and
#' use the same units as `width`.
#' @param width_units Default `"scene"`. Units used by `width` and
#' `width_column`. `"scene"` uses scene grid-cell units; `"meters"` converts
#' physical widths using the scene extent and CRS.
#' @param densify Default `TRUE`. Whether to densify trails and resample them
#' along the terrain before meshing.
#' @param offset Default `NULL`. Vertical trail centerline offset in elevation
#' units. When `NULL`, the mesh is centered on the terrain surface.
#' @param height Default `0.05`. Total solid mesh thickness in scene units.
#' @param clear_previous Default `TRUE`. Whether to remove the existing trail
#' layer before drawing the new one. A clear-only call returns without drawing
#' a replacement.
#' @param preview Default `"line"`. Whether trails are drawn in rgl as
#' lightweight `"line"` previews or as their cached `"mesh"` geometry. Both
#' options use the solid mesh in [render_highquality()].
#' @param verbose Default `FALSE`. Whether to show native meshing progress.
#' @param parallel Default `TRUE`. Whether to build independent trail meshes in
#' parallel with the native job queue.
#' @param zscale Default `1`. Ratio between horizontal spacing and elevation
#' units. If omitted for a spatial raster scene, rayshader uses its resolution.
#' @param vertical_exaggeration Default `1`. Multiplier applied to the effective
#' visual relief. If omitted, rayshader uses the active scene value.
#' @param heightmap Default `NULL`. Height matrix or spatial raster for the
#' current scene. If omitted, rayshader uses the active scene heightmap.
#'
#' @return Invisibly returns the independently meshed trail coordinates.
#' @seealso [render_streams()], [render_roads()]
#' @export
#' @examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' volcano_dem = volcano_spatial()
#' volcano_dem |>
#'   height_shade() |>
#'   plot_3d()
#'
#' render_trails(
#'   volcano_trails,
#'   color = "grey50",
#'   width = 3,
#'   width_units = "meters"
#' )
#' render_camera(theta=-33,phi=40,zoom=0.4)
#' render_highquality()
render_trails = function(
  trails,
  color = "grey50",
  width = 1,
  width_column = NULL,
  width_units = c("scene", "meters"),
  densify = TRUE,
  offset = NULL,
  height = 0.05,
  clear_previous = TRUE,
  preview = c("line", "mesh"),
  verbose = FALSE,
  parallel = TRUE,
  zscale = 1,
  vertical_exaggeration = 1,
  heightmap = NULL
) {
  clear_trail_layer = function() {
    trail_ids = get_ids_with_labels(
      typeval = c("trail_path", "trail_mesh_preview")
    )$id
    for (trail_id in trail_ids) {
      rgl::pop3d(id = trail_id)
    }
    cache_scene_trail_meshes(NULL)
  }
  if (
    is_render_clear_only_call(
      clear_previous,
      match.call(),
      clear_trail_layer
    )
  ) {
    return(invisible(NULL))
  }

  heightmap_missing = missing(heightmap)
  zscale_missing = missing(zscale)
  vertical_exaggeration_missing = missing(vertical_exaggeration)
  width_missing = missing(width)
  width_expr = substitute(width)
  width_column_missing = missing(width_column)
  width_column_expr = substitute(width_column)

  if (!is_render_line_input(trails)) {
    stop(
      "`trails` must be an sf, sfc, sfg, SpatialLines, or SpatialLinesDataFrame line object.",
      call. = FALSE
    )
  }
  heightmap = resolve_scene_render_heightmap(
    heightmap = heightmap,
    heightmap_missing = heightmap_missing,
    caller = "render_trails"
  )
  if (is.null(heightmap)) {
    stop(
      "No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
    )
  }
  zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = zscale_missing,
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = vertical_exaggeration_missing,
    heightmap = heightmap,
    caller = "render_trails"
  )
  width_units = match.arg(width_units)
  densify = resolve_render_scalar(
    densify,
    missing(densify),
    TRUE,
    "densify",
    type = "logical"
  )
  clear_previous = resolve_render_scalar(
    clear_previous,
    missing(clear_previous),
    TRUE,
    "clear_previous",
    type = "logical"
  )
  preview = match.arg(preview)
  offset = if (is.null(offset)) {
    0
  } else {
    resolve_render_scalar(offset, FALSE, 0, "offset", lower = 0)
  }
  height = resolve_render_positive_number(height, "height")
  verbose = resolve_render_scalar(
    verbose,
    missing(verbose),
    FALSE,
    "verbose",
    type = "logical"
  )
  parallel = resolve_render_scalar(
    parallel,
    missing(parallel),
    TRUE,
    "parallel",
    type = "logical"
  )
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }

  width_column_name = resolve_render_column_name(
    value = width_column,
    value_expr = width_column_expr,
    missing = width_column_missing,
    argument = "width_column"
  )
  if (!is.null(width_column_name)) {
    if (inherits(trails, "SpatialLinesDataFrame")) {
      trails = sf::st_as_sf(trails)
    }
    if (!inherits(trails, "sf")) {
      stop(
        "`width_column` can only be used with `sf` or SpatialLinesDataFrame trail inputs.",
        call. = FALSE
      )
    }
    if (!(width_column_name %in% names(trails))) {
      stop(
        sprintf(
          "`width_column` must name a column in `trails`: %s",
          width_column_name
        ),
        call. = FALSE
      )
    }
    width_values = resolve_render_feature_values(
      data = trails,
      value = width_column_name,
      value_expr = as.name(width_column_name),
      missing = FALSE,
      default = 1,
      argument = "width_column",
      type = "double",
      lower = .Machine$double.xmin
    )
    trails$render_line_width = width_values
    width_column = "render_line_width"
  } else {
    width_values = resolve_render_feature_values(
      data = trails,
      value = width,
      value_expr = width_expr,
      missing = width_missing,
      default = 1,
      argument = "width",
      type = "double",
      lower = .Machine$double.xmin
    )
    width_column = NULL
    if (length(unique(width_values)) > 1L) {
      if (inherits(trails, "SpatialLinesDataFrame")) {
        trails = sf::st_as_sf(trails)
      }
      if (!inherits(trails, "sf")) {
        stop(
          "Feature-varying `width` values require an `sf` trail input.",
          call. = FALSE
        )
      }
      trails$render_line_width = width_values
      width_column = "render_line_width"
    }
  }
  width = if (is.null(width_column)) {
    if (length(width_values)) width_values[[1L]] else 1
  } else {
    NULL
  }

  extent = resolve_scene_render_extent(
    heightmap = heightmap,
    caller = "render_trails",
    error_if_missing = FALSE
  )
  trails = prepare_render_line_geometry(
    lines = trails,
    merge = FALSE,
    exclude_polygons = NULL,
    line_argument = "trails",
    polygon_argument = "exclude_polygons"
  )
  if (is_empty_scene_sf(trails)) {
    return(invisible(list()))
  }
  trail_width = if (is.null(width_column)) {
    width
  } else {
    as.numeric(trails[[width_column]])
  }
  trail_width = convert_render_trail_width_to_scene_units(
    width = trail_width,
    width_units = width_units,
    heightmap = heightmap,
    extent = extent
  )

  path_data = render_line_coords_by_width(
    lines = trails,
    heightmap = heightmap,
    extent = extent,
    zscale = zscale,
    color = color,
    width = trail_width,
    force_by_feature = TRUE
  )
  coord_list = path_data$coords
  coord_width = path_data$width
  if (!length(coord_list)) {
    return(invisible(coord_list))
  }

  if (isTRUE(densify)) {
    coord_list = densify_render_line_coords(
      coords = coord_list,
      heightmap = heightmap,
      zscale = zscale,
      offset = offset
    )
  } else if (!identical(offset, 0)) {
    coord_list = offset_render_line_coords(
      coords = coord_list,
      offset = offset / zscale
    )
  }

  path_members = data.frame(
    trail_path_id = seq_along(coord_list),
    render_line_feature_id = as.integer(path_data$feature_id),
    stringsAsFactors = FALSE
  )
  path_members$source_feature_id = I(path_data$source_feature_id)
  attr(coord_list, "path_members") = path_members

  mesh_preview = identical(preview, "mesh")
  trail_id_by_path = rep(NA_integer_, length(coord_list))
  previous_rgl_parameters = rgl::par3d(skipRedraw = TRUE)
  on.exit(rgl::par3d(previous_rgl_parameters), add = TRUE)
  for (coord_index in seq_along(coord_list)) {
    coord = coord_list[[coord_index]]
    if (is.matrix(coord) && nrow(coord) >= 2L) {
      preview_coord = coord
      preview_coord[, 2L] = preview_coord[, 2L] + 0.01
      trail_id = rgl::lines3d(
        preview_coord,
        color = color,
        alpha = if (mesh_preview) 0 else 1,
        tag = "trail_path",
        lwd = coord_width[[coord_index]],
        line_antialias = FALSE
      )
      trail_id_by_path[[coord_index]] = as.integer(trail_id[[1L]])
    }
  }

  trail_mesh_tasks = lapply(
    which(is.finite(trail_id_by_path)),
    function(coord_index) {
      list(
        points = coord_list[[coord_index]],
        bbox_center = c(0, 0, 0),
        width = coord_width[[coord_index]],
        height = height,
        heightmap = heightmap,
        zscale = zscale,
        material = rayrender::diffuse(
          color = convert_color(color, linear = TRUE)
        ),
        return_mesh = TRUE,
        rgl_id = trail_id_by_path[[coord_index]],
        watercolor = color
      )
    }
  )
  trail_meshes = make_render_highquality_water_path_meshes(
    trail_mesh_tasks,
    verbose = verbose,
    parallel = parallel
  )
  trail_meshes = draw_render_trail_mesh_previews(
    trail_meshes,
    color = color,
    preview = preview
  )
  cache_scene_trail_meshes(
    trail_meshes,
    append = !isTRUE(clear_previous)
  )
  invisible(coord_list)
}

#' Convert trail widths to scene units
#'
#' @param width Trail widths.
#' @param width_units Units used by `width`.
#' @param heightmap Scene heightmap matrix.
#' @param extent Scene spatial extent.
#'
#' @return Trail widths in scene grid-cell units.
#' @keywords internal
convert_render_trail_width_to_scene_units = function(
  width,
  width_units,
  heightmap,
  extent
) {
  if (identical(width_units, "scene")) {
    return(width)
  }
  scene_crs = try_parse_scene_crs(attr(heightmap, "crs", exact = TRUE))
  if (is.null(scene_crs)) {
    scene_crs = get_scene_target_crs(
      extent = extent,
      heightmap = heightmap,
      caller = "render_trails"
    )
  }
  if (is.null(scene_crs)) {
    stop(
      paste0(
        "render_trails(): `width_units = \"meters\"` requires a spatial ",
        "scene with a CRS."
      ),
      call. = FALSE
    )
  }
  world_scale = calculate_road_path_world_scale(
    heightmap = heightmap,
    extent = extent,
    crs = scene_crs
  )
  meters_per_scene_unit = mean(world_scale)
  if (!is.finite(meters_per_scene_unit) || meters_per_scene_unit <= 0) {
    stop(
      "render_trails(): Could not convert trail widths from meters to scene units.",
      call. = FALSE
    )
  }
  width / meters_per_scene_unit
}

#' Draw solid trail mesh previews
#'
#' @param meshes Absolute-coordinate trail `mesh3d` objects.
#' @param color Trail color.
#' @param preview Trail preview type.
#'
#' @return The meshes with their rgl identifiers registered.
#' @keywords internal
draw_render_trail_mesh_previews = function(meshes, color, preview) {
  for (index in seq_along(meshes)) {
    mesh = meshes[[index]]
    if (!inherits(mesh, "mesh3d")) {
      next
    }
    stream_specification = attr(mesh, "render_stream_mesh_specification")
    if (identical(preview, "mesh")) {
      rgl::shade3d(
        mesh,
        color = color,
        specular = "black",
        shininess = 0,
        tag = "trail_mesh_preview"
      )
    }
    trail_specification = list(
      rgl_id = stream_specification$rgl_id,
      color = color,
      width = stream_specification$width,
      height = stream_specification$height,
      material = stream_specification$material
    )
    attr(mesh, "render_stream_mesh_specification") = NULL
    attr(mesh, "render_trail_mesh_specification") = trail_specification
    meshes[[index]] = mesh
  }
  meshes
}

#' Convert cached trail meshes into rayrender models
#'
#' @param meshes Absolute-coordinate trail `mesh3d` objects.
#' @param bbox_center Active rayrender scene center.
#' @param rgl_materials Default `list()`. Validated rgl material overrides.
#'
#' @return List of translated rayrender mesh models.
#' @keywords internal
make_render_highquality_cached_trail_meshes = function(
  meshes,
  bbox_center,
  rgl_materials = list()
) {
  bbox_center = suppressWarnings(as.numeric(bbox_center[1:3]))
  if (length(bbox_center) != 3L || any(!is.finite(bbox_center))) {
    stop("Cached trail meshes require a finite scene center.", call. = FALSE)
  }
  models = lapply(meshes, function(mesh) {
    if (!inherits(mesh, "mesh3d")) {
      return(NULL)
    }
    specification = attr(mesh, "render_trail_mesh_specification")
    color = specification$color
    if (
      is.null(color) ||
        !is.character(color) ||
        !length(color) ||
        is.na(color[[1L]])
    ) {
      color = "grey50"
    }
    rgl_id = specification$rgl_id
    if (is.null(rgl_id) || !length(rgl_id)) {
      rgl_id = NA_integer_
    }
    material = resolve_render_highquality_rgl_material(
      rgl_materials = rgl_materials,
      id = rgl_id[[1L]],
      tag = "trail_path",
      color = color[[1L]]
    )
    if (is.null(material)) {
      material = specification$material
    }
    if (is.null(material)) {
      material = rayrender::diffuse(
        color = convert_color(color[[1L]], linear = TRUE)
      )
    }
    rayrender::mesh3d_model(
      mesh,
      x = -bbox_center[[1L]],
      y = -bbox_center[[2L]],
      z = -bbox_center[[3L]],
      override_material = TRUE,
      material = material
    )
  })
  Filter(Negate(is.null), models)
}
