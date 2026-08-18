#' Render Road Paths
#'
#' @description Adds road paths to the scene and eagerly builds their reusable
#' high-quality meshes. Roads can be previewed as rgl lines or as the same mesh
#' geometry consumed by [render_highquality()].
#'
#' @param roads Spatial line data used to draw road paths. Supports `sf`,
#' `sfc`, `sfg`, `SpatialLines`, and `SpatialLinesDataFrame` line inputs.
#' @param heightmap Default `NULL`. Height matrix or spatial raster for the
#' current scene. If omitted, this is taken from the cached scene set by
#' [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#' @param roadcolor Default `"#303030"`. sRGB road surface color.
#' @param zscale Default `1`. The ratio between the x and y spacing and the z
#' axis. If omitted and `heightmap` is a spatial raster, rayshader uses the
#' raster cell resolution.
#' @param vertical_exaggeration Default `1`. Multiplier applied to the effective
#' visual relief. If omitted, rayshader uses the cached scene value from
#' [plot_3d()] or [plot_gg()] when available; pass explicitly to override for
#' this call.
#' @param width Default `NULL`, which derives road width from `lanes` and
#' `lane_width`. If supplied, road width in scene grid-cell units for
#' [render_highquality()]. The rgl preview uses the same value as line width.
#' @param width_column Default `NULL`. Column name in an `sf` road object used
#' to set per-feature road widths. Values must be positive finite numbers and
#' use the same units as `width`. When supplied, road merging is disabled to
#' preserve feature attributes.
#' @param densify Default `TRUE`. Whether to densify road paths and resample
#' them along the terrain before [render_highquality()] meshing. Set to `FALSE`
#' to use the vertices returned by [render_path()] directly.
#' @param offset Default `0`. Additional vertical road base offset in elevation
#' units above the sampled surface.
#' @param offset_transition Default `0`. Length in scene/world units over which
#' each end of a road transitions between the sampled surface and `offset`.
#' Positive values use the terrain height at the path midpoint plus `offset` as
#' a constant deck height, with a quadratic Bezier ramp at each end. If a road
#' is shorter than twice this distance, each ramp is shortened to half the road
#' length.
#' @param layer Default `NULL`. An unquoted or character column name in an `sf`
#' road object containing OpenStreetMap-style layer values. Missing values are
#' treated as implicit layer `0`, except affirmative `bridge` or elevated
#' `location` metadata infer effective layer `1` and affirmative `tunnel` or
#' underground `location` metadata infer effective layer `-1`. Inferred values
#' remain distinguishable from explicit OSM layer tags in diagnostics.
#' Roads are grouped by exact physical events and conservative endpoint
#' continuations. A sparse quadratic profile solve enforces crossing clearance,
#' physical grade and grade-rate limits, junction height continuity, and
#' selected through-road grade compatibility. Surface roads retain a sampled
#' terrain reference, elevated spans use endpoint support chords, and roads with
#' explicit tunnel or underground metadata use a bounded terrain-relative
#' reference. Untagged negative layers are not independently activated as
#' tunnels. Ambiguous endpoint matches that would create a contradictory
#' clearance cycle are excluded. Supplying `layer` disables `merge` and requires
#' the suggested `sf`, `igraph`, `Matrix`, and `osqp` packages.
#' @param layer_height Default `5.5`. Either a single positive spacing in
#' elevation units between locally ordered layers, or an unquoted or character
#' column name containing each feature's positive separation above the lower
#' road at an intersection. Column heights override constant layer spacing.
#' @param maximum_grade Default `0.15`. Maximum absolute longitudinal road
#' grade. Positive infinity removes the grade bound.
#' @param continuation_grade_tolerance Default `0.14`. Maximum absolute grade
#' mismatch at selected road continuations. Continuation height remains exact.
#' @param merge Default `TRUE`. Whether to merge connected road linework before
#' rendering. This reduces visible joins between adjacent line features in
#' [render_highquality()].
#' @param lane_texture Default `FALSE`. If `TRUE`, [render_highquality()] uses a
#' generated repeating texture on the top road surface for lane markings.
#' @param lane_texture_file Default `NULL`. Optional path to a custom road lane
#' texture. When supplied, it is used instead of the generated texture and
#' `lane_texture` is treated as `TRUE`.
#' @param lane_dash_length Default `3`. Painted dash length in scene/world units
#' for generated dashed lane markings. The default approximates a 10 foot dash
#' as 3 meters.
#' @param lane_gap_length Default `10`. Gap length in scene/world units for
#' generated dashed lane markings. The default approximates a 30 foot gap as
#' 10 meters.
#' @param lane_texture_length Default `NULL`, which uses
#' `lane_dash_length + lane_gap_length`. Scene-unit length covered by one
#' repetition of the road lane texture.
#' @param lane_texture_mapping Default `"auto"`. If `"auto"`, each road path is
#' assigned a texture repetition count based on its world-unit length and
#' `lane_texture_length`. If `"fixed"`, all road paths use `lane_texture_length`
#' directly.
#' @param lanes Default `2`. Number of lanes used when generating the default
#' lane texture and, when `width = NULL`, deriving the road width. May also be
#' an unquoted or character column name in an `sf` road object. OSM-style
#' compound values use their first positive integer. Missing values first use
#' `lanes:forward` plus `lanes:backward`, then fall back to one lane for links
#' and one-way roads or two lanes for other roads.
#' @param lane_width Default `3`. Lane width in scene/world units used when
#' `width = NULL`. The derived road width is
#' `lane_width * (lanes + 2)`. The generated lane texture leaves one lane width
#' total outside the edge lines, split between both sides.
#' @param lane_color Default `"#eeeade"`. sRGB color for dashed lane divider
#' markings.
#' @param centerline_color Default `"#eeeade"`. sRGB color for the center
#' divider.
#' @param edge_line_color Default `"#d6ad3d"`. sRGB color for solid edge
#' markings.
#' @param lane_line_width Default `0.035`. Lane marking width as a fraction of
#' the road width in the generated texture.
#' @param lane_dash_fraction Default `NULL`, which uses
#' `lane_dash_length / (lane_dash_length + lane_gap_length)`. Fraction of each
#' texture repetition occupied by a dash for dashed lane markings.
#' @param clear_previous Default `TRUE`. If `TRUE`, removes the existing road
#' layer before drawing the new one.
#' @param preview Default `"line"`. Whether roads are drawn in rgl as lightweight
#' `"line"` previews or as their cached `"mesh"` geometry. The mesh is the exact
#' high-quality geometry and does not include the line-only preview offset.
#' @param verbose Default `FALSE`. Whether to display progress while building
#' and caching the road meshes.
#' @param parallel Default `TRUE`. Whether to use multiple native threads while
#' building the cached road meshes.
#'
#' @return Invisibly returns the rendered road coordinates. When `layer` is
#' supplied, the result has `terrain_following`, `profile_diagnostics`, and
#' `mesh_topology` attributes describing the sparse profile solve and selected
#' physical mesh continuations.
#' @export
render_roads = function(
  roads,
  heightmap = NULL,
  roadcolor = "#303030",
  zscale = 1,
  vertical_exaggeration = 1,
  width = NULL,
  width_column = NULL,
  densify = TRUE,
  offset = 0,
  offset_transition = 0,
  layer = NULL,
  layer_height = 5.5,
  merge = TRUE,
  lane_texture = FALSE,
  lane_texture_file = NULL,
  lane_dash_length = 3,
  lane_gap_length = 10,
  lane_texture_length = NULL,
  lane_texture_mapping = c("auto", "fixed"),
  lanes = 2,
  lane_width = 3,
  lane_color = "#eeeade",
  centerline_color = "#eeeade",
  edge_line_color = "#d6ad3d",
  lane_line_width = 0.035,
  lane_dash_fraction = NULL,
  clear_previous = TRUE,
  preview = c("line", "mesh"),
  verbose = FALSE,
  parallel = TRUE,
  maximum_grade = 0.15,
  continuation_grade_tolerance = 0.14
) {
  # 1. Capture expressions needed to distinguish values from column references.
  heightmap_missing = missing(heightmap)
  zscale_missing = missing(zscale)
  vertical_exaggeration_missing = missing(vertical_exaggeration)
  width_missing = missing(width)
  width_expr = substitute(width)
  width_column_expr = substitute(width_column)
  layer_expr = substitute(layer)
  layer_height_expr = substitute(layer_height)
  lanes_expr = substitute(lanes)

  # 2. Resolve the active scene and validate public scalar arguments once.
  if (!is_render_line_input(roads)) {
    stop(
      "`roads` must be an sf, sfc, sfg, SpatialLines, or SpatialLinesDataFrame line object.",
      call. = FALSE
    )
  }
  heightmap = resolve_scene_render_heightmap(
    heightmap = heightmap,
    heightmap_missing = heightmap_missing,
    caller = "render_roads"
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
    caller = "render_roads"
  )
  densify = resolve_render_scalar(
    densify,
    missing(densify),
    TRUE,
    "densify",
    type = "logical"
  )
  offset = resolve_render_scalar(
    offset,
    missing(offset),
    0,
    "offset",
    lower = 0
  )
  offset_transition = resolve_render_scalar(
    offset_transition,
    missing(offset_transition),
    0,
    "offset_transition",
    lower = 0
  )
  merge = resolve_render_scalar(
    merge,
    missing(merge),
    TRUE,
    "merge",
    type = "logical"
  )
  lane_texture = resolve_render_scalar(
    lane_texture,
    missing(lane_texture),
    FALSE,
    "lane_texture",
    type = "logical"
  )
  lane_dash_length = resolve_render_scalar(
    lane_dash_length,
    missing(lane_dash_length),
    3,
    "lane_dash_length",
    lower = 0,
    lower_inclusive = FALSE
  )
  lane_gap_length = resolve_render_scalar(
    lane_gap_length,
    missing(lane_gap_length),
    10,
    "lane_gap_length",
    lower = 0
  )
  lane_texture_length = if (is.null(lane_texture_length)) {
    lane_dash_length + lane_gap_length
  } else {
    resolve_render_scalar(
      lane_texture_length,
      FALSE,
      lane_dash_length + lane_gap_length,
      "lane_texture_length",
      lower = 0,
      lower_inclusive = FALSE
    )
  }
  lane_texture_mapping = match.arg(lane_texture_mapping)
  lane_width = resolve_render_scalar(
    lane_width,
    missing(lane_width),
    3,
    "lane_width",
    lower = 0,
    lower_inclusive = FALSE
  )
  lane_line_width = resolve_render_scalar(
    lane_line_width,
    missing(lane_line_width),
    0.035,
    "lane_line_width",
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )
  lane_dash_fraction = if (is.null(lane_dash_fraction)) {
    lane_dash_length / (lane_dash_length + lane_gap_length)
  } else {
    resolve_render_scalar(
      lane_dash_fraction,
      FALSE,
      lane_dash_length / (lane_dash_length + lane_gap_length),
      "lane_dash_fraction",
      lower = 0,
      upper = 1,
      lower_inclusive = FALSE,
      upper_inclusive = FALSE
    )
  }
  clear_previous = resolve_render_scalar(
    clear_previous,
    missing(clear_previous),
    TRUE,
    "clear_previous",
    type = "logical"
  )
  preview = match.arg(preview)
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
  maximum_grade = assert_render_road_profile_setting(
    maximum_grade,
    "maximum_grade",
    allow_infinite = TRUE
  )
  continuation_grade_tolerance = assert_render_road_profile_setting(
    continuation_grade_tolerance,
    "continuation_grade_tolerance",
    allow_zero = TRUE
  )
  if (!is.null(lane_texture_file)) {
    if (
      !is.character(lane_texture_file) ||
        length(lane_texture_file) != 1L ||
        is.na(lane_texture_file) ||
        !nzchar(lane_texture_file) ||
        !file.exists(lane_texture_file)
    ) {
      stop("`lane_texture_file` must be a path to an existing image file.")
    }
    lane_texture_file = normalizePath(
      lane_texture_file,
      winslash = "/",
      mustWork = TRUE
    )
  }
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }

  # 3. Resolve columns and normalize every feature-aligned public value.

  width_column = resolve_render_column_name(
    value = width_column,
    value_expr = width_column_expr,
    missing = missing(width_column),
    argument = "width_column"
  )
  layer_column = resolve_render_column_name(
    value = layer,
    value_expr = layer_expr,
    missing = missing(layer),
    argument = "layer"
  )
  layer_height_spec = resolve_render_scalar_or_column(
    value = layer_height,
    value_expr = layer_height_expr,
    missing = missing(layer_height) ||
      identical(layer_height_expr, quote(NULL)),
    default = 5.5,
    argument = "layer_height",
    type = "double",
    lower = 0,
    lower_inclusive = FALSE
  )
  lanes_spec = resolve_render_scalar_or_column(
    value = lanes,
    value_expr = lanes_expr,
    missing = missing(lanes),
    default = 2L,
    argument = "lanes",
    type = "integer",
    lower = 1
  )
  if (is.null(layer_column) && !is.null(layer_height_spec$column)) {
    stop("`layer_height` can only name a column when `layer` is supplied.")
  }
  if (!is.null(layer_column) && (offset != 0 || offset_transition != 0)) {
    stop(
      "`layer` cannot be combined with `offset` or `offset_transition`.",
      call. = FALSE
    )
  }
  column_input = any(c(
    !is.null(width_column),
    !is.null(layer_column),
    !is.null(layer_height_spec$column),
    !is.null(lanes_spec$column)
  ))
  if (column_input && inherits(roads, "SpatialLinesDataFrame")) {
    roads = sf::st_as_sf(roads)
  }
  if (column_input && !inherits(roads, "sf")) {
    stop(
      "Road column arguments require an `sf` or `SpatialLinesDataFrame` road object.",
      call. = FALSE
    )
  }
  if (!is.null(width_column)) {
    roads$render_road_input_width = resolve_render_feature_values(
      data = roads,
      value = width_column,
      value_expr = width_column_expr,
      missing = FALSE,
      default = NA_real_,
      argument = "width_column",
      type = "double",
      lower = .Machine$double.xmin
    )
    width_column = "render_road_input_width"
    width = NULL
    merge = FALSE
  } else if (!isTRUE(width_missing) && !identical(width_expr, quote(NULL))) {
    width_values = resolve_render_feature_values(
      data = roads,
      value = width,
      value_expr = width_expr,
      missing = FALSE,
      default = NA_real_,
      argument = "width",
      type = "double",
      lower = .Machine$double.xmin
    )
    if (length(unique(width_values)) > 1L) {
      if (inherits(roads, "SpatialLinesDataFrame")) {
        roads = sf::st_as_sf(roads)
      }
      if (!inherits(roads, "sf")) {
        stop(
          "Feature-varying `width` values require an `sf` road input.",
          call. = FALSE
        )
      }
      roads$render_road_input_width = width_values
      width_column = "render_road_input_width"
      width = NULL
      merge = FALSE
    } else {
      width = if (length(width_values)) width_values[[1L]] else 1
    }
  }
  if (!is.null(layer_column)) {
    roads$render_road_input_layer = resolve_render_feature_values(
      data = roads,
      value = layer,
      value_expr = layer_expr,
      missing = FALSE,
      default = NA_real_,
      argument = "layer",
      type = "double",
      allow_na = TRUE
    )
    layer_column = "render_road_input_layer"
    merge = FALSE
  }
  if (!is.null(layer_height_spec$column)) {
    roads$render_road_input_layer_height = resolve_render_feature_values(
      data = roads,
      value = layer_height,
      value_expr = layer_height_expr,
      missing = FALSE,
      default = NA_real_,
      argument = "layer_height",
      type = "double",
      lower = .Machine$double.xmin,
      allow_na = TRUE
    )
    layer_height_spec$column = "render_road_input_layer_height"
  }
  layer_spacing = if (is.null(layer_height_spec$value)) {
    5.5
  } else {
    layer_height_spec$value
  }
  if (!is.null(lanes_spec$column)) {
    roads$render_road_input_lanes = resolve_render_feature_values(
      data = roads,
      value = lanes,
      value_expr = lanes_expr,
      missing = FALSE,
      default = NA_character_,
      argument = "lanes",
      type = "character",
      allow_na = TRUE
    )
    lanes_spec$column = "render_road_input_lanes"
    merge = FALSE
  }
  if (isTRUE(clear_previous)) {
    road_scene_ids = get_ids_with_labels(
      typeval = c("road_path", "road_mesh_preview")
    )$id
    for (road_scene_id in road_scene_ids) {
      rgl::pop3d(id = road_scene_id)
    }
    clear_render_road_path_info()
    cache_scene_road_meshes(NULL)
  }

  # 4. Normalize geometry and create terrain-sampled centerline paths.
  extent = resolve_scene_render_extent(
    heightmap = heightmap,
    caller = "render_roads",
    error_if_missing = FALSE
  )
  roads = prepare_render_line_geometry(
    lines = roads,
    merge = merge,
    line_argument = "roads"
  )
  if (is_empty_scene_sf(roads)) {
    return(invisible(list()))
  }
  road_lanes = resolve_render_road_lane_values(
    roads = roads,
    lanes = lanes_spec$value,
    lanes_column = lanes_spec$column
  )
  scene_crs = get_scene_target_crs(
    extent = extent,
    heightmap = heightmap,
    caller = "render_roads"
  )
  texture_world_scale = calculate_road_path_world_scale(
    heightmap = heightmap,
    extent = extent,
    crs = scene_crs
  )
  road_width = resolve_render_road_width(
    road_width = width,
    lanes = road_lanes,
    lane_width = lane_width,
    texture_world_scale = texture_world_scale
  )
  if (length(road_width) > 1L && is.null(width_column)) {
    if (
      length(road_width) != nrow(roads) ||
        any(!is.finite(road_width)) ||
        any(road_width <= 0)
    ) {
      stop(
        "Derived road widths must be positive and match the road features.",
        call. = FALSE
      )
    }
  } else if (!is.null(width_column)) {
    road_width = as.numeric(roads[[width_column]])
  }
  path_data = render_line_coords_by_width(
    lines = roads,
    heightmap = heightmap,
    extent = extent,
    zscale = zscale,
    color = roadcolor,
    width = road_width,
    force_by_feature = TRUE
  )
  coord_list = path_data$coords
  coord_width = path_data$width
  coord_feature = path_data$feature_id
  coord_lanes = if (length(road_lanes) == 1L) {
    rep(road_lanes, length(coord_list))
  } else {
    road_lanes[coord_feature]
  }
  coord_terrain_following = rep(TRUE, length(coord_list))
  if (!length(coord_list)) {
    return(invisible(coord_list))
  }
  # Layered paths are resampled once after the continuous profile solve. Doing
  # the ordinary terrain densification here first expands every source path at
  # triangle boundaries, only for the layered solver to replace all heights and
  # the native mesher to resample the final profile again.
  if (isTRUE(densify) && is.null(layer_column)) {
    coord_list = densify_render_line_coords(
      coords = coord_list,
      heightmap = heightmap,
      zscale = zscale,
      offset = 0
    )
  }

  # 5. Apply sparse layered profiles or the ordinary terrain-relative offset.
  if (!is.null(layer_column)) {
    coord_list = solve_render_road_path_profiles(
      coord_list = coord_list,
      coord_feature = coord_feature,
      coord_width = coord_width,
      roads = roads,
      heightmap = heightmap,
      layer_column = layer_column,
      lane_column = lanes_spec$column,
      lane_values = if (length(road_lanes) == 1L) {
        rep(road_lanes, nrow(roads))
      } else {
        road_lanes
      },
      layer_height_column = layer_height_spec$column,
      layer_spacing = layer_spacing,
      maximum_grade = maximum_grade,
      continuation_grade_tolerance = continuation_grade_tolerance,
      zscale = zscale,
      texture_world_scale = texture_world_scale
    )
    layer_terrain_following = attr(coord_list, "terrain_following")
    if (length(layer_terrain_following) == length(coord_list)) {
      coord_terrain_following = layer_terrain_following
    }
  } else {
    coord_list = offset_render_road_path_coords(
      coord_list = coord_list,
      offset = offset / zscale,
      transition_length = offset_transition,
      texture_world_scale = texture_world_scale
    )
    if (offset > 0 && offset_transition > 0) {
      coord_terrain_following[] = FALSE
    }
  }

  # 6. Resolve lane textures and station-based road-envelope metadata.
  resolve_texture_files = function(coord_lanes) {
    resolve_texture_file = function(lanes) {
      if (!is.null(lane_texture_file)) {
        return(lane_texture_file)
      }
      if (!isTRUE(lane_texture)) {
        return(NULL)
      }
      make_road_lane_texture(
        roadcolor = roadcolor,
        lanes = lanes,
        lane_color = lane_color,
        centerline_color = centerline_color,
        edge_line_color = edge_line_color,
        lane_line_width = lane_line_width,
        lane_dash_fraction = lane_dash_fraction
      )
    }

    if (!length(coord_lanes)) {
      return(list())
    }
    coord_lanes = vapply(
      coord_lanes,
      assert_render_road_lane_count,
      integer(1)
    )
    unique_lanes = unique(coord_lanes)
    unique_files = lapply(unique_lanes, resolve_texture_file)
    unique_files[match(coord_lanes, unique_lanes)]
  }
  resolve_texture_mapping = function(coord_list) {
    path_length = function(points) {
      cumulative_distance = calculate_road_path_cumulative_distance(
        points,
        texture_world_scale = texture_world_scale
      )
      if (!length(cumulative_distance)) {
        return(0)
      }
      cumulative_distance[[length(cumulative_distance)]]
    }

    road_lengths = vapply(coord_list, path_length, numeric(1))
    texture_lengths = rep(lane_texture_length, length(road_lengths))
    texture_repeats = rep(NA_real_, length(road_lengths))
    if (identical(lane_texture_mapping, "auto")) {
      valid_length = is.finite(road_lengths) & road_lengths > 0
      texture_repeats[valid_length] =
        road_lengths[valid_length] / lane_texture_length
    }
    list(
      road_length = road_lengths,
      texture_length = texture_lengths,
      texture_repeats = texture_repeats,
      texture_world_scale = texture_world_scale
    )
  }

  texture_files = resolve_texture_files(coord_lanes)
  texture_mapping = resolve_texture_mapping(coord_list)

  # 7. Build physical mesh chains from accepted exact continuations.
  mesh_topology = attr(coord_list, "mesh_topology")
  path_members = if (is.null(mesh_topology)) {
    source_feature_id = vapply(
      path_data$source_feature_id,
      function(value) {
        value = unique(as.integer(value))
        if (length(value) == 1L && is.finite(value)) {
          return(value)
        }
        NA_integer_
      },
      integer(1)
    )
    data.frame(
      road_path_id = seq_along(coord_list),
      render_road_fragment_id = NA_integer_,
      render_road_feature_id = source_feature_id,
      stringsAsFactors = FALSE
    )
  } else {
    mesh_topology$path_members
  }
  mesh_chain_members = build_render_road_mesh_chain_members(
    coord_list = coord_list,
    path_members = path_members,
    selected_connections = if (is.null(mesh_topology)) {
      NULL
    } else {
      mesh_topology$selected_connections
    },
    lanes = coord_lanes,
    width = coord_width,
    texture_file = texture_files,
    texture_length = texture_mapping$texture_length,
    texture_repeats = texture_mapping$texture_repeats
  )
  attr(coord_list, "mesh_chain_members") = mesh_chain_members

  # 8. Draw source paths and register the compact high-quality mesh contract.
  rgl_preview_offset = 0.01
  mesh_preview = identical(preview, "mesh")
  road_id_by_path = rep(NA_integer_, length(coord_list))
  previous_rgl_parameters = rgl::par3d(skipRedraw = TRUE)
  on.exit(rgl::par3d(previous_rgl_parameters), add = TRUE)
  for (coord_index in seq_along(coord_list)) {
    coord = coord_list[[coord_index]]
    if (is.matrix(coord) && nrow(coord) >= 2) {
      preview_coord = coord
      preview_coord[, 2] = preview_coord[, 2] + rgl_preview_offset
      road_id = rgl::lines3d(
        preview_coord,
        color = roadcolor,
        alpha = if (mesh_preview) 0 else 1,
        tag = "road_path",
        lwd = coord_width[[coord_index]],
        line_antialias = FALSE
      )
      road_id_by_path[[coord_index]] = as.integer(road_id[[1L]])
    }
  }
  for (coord_index in which(is.finite(road_id_by_path))) {
    texture_repeats = texture_mapping$texture_repeats[[coord_index]]
    if (!is.finite(texture_repeats)) {
      texture_repeats = NULL
    }
    path_member = mesh_chain_members[
      mesh_chain_members$road_path_id == coord_index,
      ,
      drop = FALSE
    ]
    register_render_road_path_info(
      id = road_id_by_path[[coord_index]],
      info = list(
        road_path_id = coord_index,
        fragment_id = if (nrow(path_member)) {
          path_member$fragment_id[[1L]]
        } else {
          NA_integer_
        },
        feature_id = if (nrow(path_member)) {
          path_member$feature_id[[1L]]
        } else {
          NA_integer_
        },
        mesh_chain_id = if (nrow(path_member)) {
          path_member$mesh_chain_id[[1L]]
        } else {
          NA_integer_
        },
        member_order = if (nrow(path_member)) {
          path_member$member_order[[1L]]
        } else {
          1L
        },
        orientation = if (nrow(path_member)) {
          path_member$orientation[[1L]]
        } else {
          1L
        },
        closed = nrow(path_member) && isTRUE(path_member$closed[[1L]]),
        lanes = coord_lanes[[coord_index]],
        width = coord_width[[coord_index]],
        texture_file = texture_files[[coord_index]],
        texture_length = texture_mapping$texture_length[[coord_index]],
        texture_repeats = texture_repeats,
        texture_world_scale = texture_mapping$texture_world_scale,
        terrain_following = coord_terrain_following[[coord_index]],
        rgl_preview_offset = rgl_preview_offset,
        roadcolor = roadcolor
      )
    )
  }

  # 9. Build absolute-coordinate meshes once and cache them for every later
  # high-quality render of this scene.
  road_mesh_tasks = lapply(
    which(is.finite(road_id_by_path)),
    function(coord_index) {
      road_info = get_render_road_path_info(
        road_id_by_path[[coord_index]]
      )
      material = if (is.null(road_info$texture_file)) {
        rayrender::diffuse(
          color = convert_color(roadcolor, linear = TRUE)
        )
      } else {
        rayrender::diffuse(
          color = "white",
          image_texture = road_info$texture_file,
          image_repeat = 1
        )
      }
      task = list(
        points = coord_list[[coord_index]],
        bbox_center = c(0, 0, 0),
        width = coord_width[[coord_index]],
        heightmap = heightmap,
        zscale = zscale,
        material = material,
        texture_file = road_info$texture_file,
        texture_length = road_info$texture_length,
        texture_repeats = road_info$texture_repeats,
        texture_world_scale = road_info$texture_world_scale,
        terrain_following = road_info$terrain_following,
        return_mesh = TRUE,
        rgl_id = road_id_by_path[[coord_index]],
        roadcolor = roadcolor
      )
      attr(task, "mesh_topology") = list(
        mesh_chain_id = as.integer(road_info$mesh_chain_id[[1L]]),
        road_path_id = road_info$road_path_id,
        render_road_fragment_id = as.integer(road_info$fragment_id[[1L]]),
        render_road_feature_id = as.integer(road_info$feature_id[[1L]]),
        member_order = as.integer(road_info$member_order[[1L]]),
        orientation = as.integer(road_info$orientation[[1L]]),
        closed = isTRUE(road_info$closed),
        road_lanes = road_info$lanes
      )
      task
    }
  )
  road_meshes = make_render_highquality_road_path_meshes(
    road_mesh_tasks,
    verbose = verbose,
    parallel = parallel
  )
  cache_scene_road_meshes(
    road_meshes,
    append = !isTRUE(clear_previous)
  )
  if (mesh_preview && length(road_meshes)) {
    draw_render_road_mesh_preview(road_meshes, color = roadcolor)
  }

  # 10. Preserve the public coordinate-list return value and diagnostics.
  invisible(coord_list)
}

#' Draw cached road meshes in rgl
#'
#' @param meshes Absolute-coordinate road `mesh3d` objects.
#' @param color Default `"#303030"`. Fallback preview color.
#'
#' @return Invisibly returns the created rgl identifiers.
#' @keywords internal
draw_render_road_mesh_preview = function(meshes, color = "#303030") {
  mesh_ids = integer(0)
  for (mesh in meshes) {
    if (!inherits(mesh, "mesh3d")) {
      next
    }
    specification = attr(mesh, "render_road_mesh_specification")
    mesh_color = if (is.null(specification$texture_file)) {
      specification$roadcolor
    } else {
      "white"
    }
    if (
      is.null(mesh_color) ||
        !is.character(mesh_color) ||
        !length(mesh_color) ||
        is.na(mesh_color[[1L]])
    ) {
      mesh_color = color
    }
    mesh_id = rgl::shade3d(
      mesh,
      color = mesh_color[[1L]],
      tag = "road_mesh_preview"
    )
    mesh_ids = c(mesh_ids, as.integer(mesh_id))
  }
  invisible(mesh_ids)
}

#' Parse an OSM lane-count tag
#'
#' @param value Raw OSM lane-count values.
#'
#' @return Integer lane counts with invalid or missing entries represented by
#' `NA`.
#' @keywords internal
parse_render_road_osm_lane_count = function(value) {
  if (is.factor(value)) {
    value = as.character(value)
  }
  if (is.numeric(value)) {
    parsed = suppressWarnings(as.numeric(value))
  } else {
    value = trimws(as.character(value))
    matched = regexpr(
      "[0-9]+(?:\\.[0-9]+)?",
      value,
      perl = TRUE
    )
    parsed = rep(NA_real_, length(value))
    present = !is.na(value) & matched > 0L
    match_length = attr(matched, "match.length")
    parsed[present] = suppressWarnings(as.numeric(substring(
      value[present],
      matched[present],
      matched[present] + match_length[present] - 1L
    )))
  }
  valid = is.finite(parsed) &
    parsed >= 1 &
    parsed == floor(parsed)
  parsed[!valid] = NA_real_
  as.integer(parsed)
}

#' Normalize an OSM text tag
#'
#' @param value Raw OSM tag values.
#'
#' @return Lowercase character values with missing and blank entries as `NA`.
#' @keywords internal
normalize_render_road_osm_tag = function(value) {
  value = tolower(trimws(as.character(value)))
  value[is.na(value) | !nzchar(value)] = NA_character_
  value
}

#' Identify truthy OSM structure tags
#'
#' @param value Raw OSM tag values.
#'
#' @return Logical values indicating affirmative structure metadata.
#' @keywords internal
is_truthy_render_road_osm_tag = function(value) {
  value = normalize_render_road_osm_tag(value)
  !is.na(value) & !(value %in% c("no", "false", "0"))
}

#' Resolve reliable lane-count evidence
#'
#' @param roads Prepared road features.
#' @param lanes_column Default `NULL`. Optional lane-count column.
#'
#' @return Lane counts suitable for topology evidence and their sources.
#' @keywords internal
resolve_render_road_lane_evidence = function(roads, lanes_column = NULL) {
  lane_count = rep(NA_integer_, nrow(roads))
  source = rep("unavailable", nrow(roads))
  if (is.null(lanes_column)) {
    return(list(lane_count = lane_count, source = source))
  }
  if (!(lanes_column %in% names(roads))) {
    stop(
      sprintf(
        "Prepared road data is missing lane column `%s`.",
        lanes_column
      ),
      call. = FALSE
    )
  }

  lane_count = parse_render_road_osm_lane_count(roads[[lanes_column]])
  source[is.finite(lane_count)] = "selected_column"
  directional_value = function(column) {
    if (!(column %in% names(roads))) {
      return(rep(NA_integer_, nrow(roads)))
    }
    parse_render_road_osm_lane_count(roads[[column]])
  }
  forward = directional_value("lanes:forward")
  backward = directional_value("lanes:backward")
  directional_present = is.finite(forward) | is.finite(backward)
  directional_total = rowSums(
    cbind(forward, backward),
    na.rm = TRUE
  )
  use_directional = !is.finite(lane_count) & directional_present
  lane_count[use_directional] = directional_total[use_directional]
  source[use_directional] = "directional_sum"
  list(lane_count = as.integer(lane_count), source = source)
}

#' Resolve render road lane values
#'
#' @param roads Prepared road features.
#' @param lanes Constant lane count.
#' @param lanes_column Optional lane count column.
#'
#' @return Integer lane count for all road features.
#' @keywords internal
resolve_render_road_lane_values = function(
  roads,
  lanes,
  lanes_column = NULL
) {
  if (is.null(lanes_column)) {
    return(as.integer(lanes))
  }
  lane_evidence = resolve_render_road_lane_evidence(roads, lanes_column)
  lane_values = lane_evidence$lane_count

  highway = if ("highway" %in% names(roads)) {
    normalize_render_road_osm_tag(roads$highway)
  } else {
    rep(NA_character_, nrow(roads))
  }
  oneway = if ("oneway" %in% names(roads)) {
    normalize_render_road_osm_tag(roads$oneway) %in%
      c("yes", "true", "1", "-1")
  } else {
    rep(FALSE, nrow(roads))
  }
  link = !is.na(highway) & grepl("_link$", highway)
  fallback = ifelse(oneway | link, 1L, 2L)
  missing = !is.finite(lane_values) | lane_values < 1L
  if (any(missing)) {
    evidence_group = paste(
      ifelse(is.na(highway), "<missing>", highway),
      oneway,
      sep = ":"
    )
    available_group = unique(evidence_group[
      !missing &
        !is.na(highway)
    ])
    for (group in available_group) {
      target = missing & evidence_group == group
      if (!any(target)) {
        next
      }
      group_lane_values = lane_values[
        !missing &
          evidence_group == group
      ]
      inferred = suppressWarnings(as.integer(round(
        stats::median(group_lane_values, na.rm = TRUE)
      )))
      if (is.finite(inferred) && inferred >= 1L) {
        fallback[target] = inferred
      }
    }
  }
  lane_values[missing] = fallback[missing]
  as.integer(lane_values)
}

#' Resolve effective OSM road layers
#'
#' @param roads Road features containing OSM-style metadata.
#' @param layer_column Column containing explicit OSM layer values.
#'
#' @return Effective layers, explicit and inferred flags, and inference sources.
#' @keywords internal
resolve_render_road_osm_layer_values = function(roads, layer_column) {
  raw_layer = roads[[layer_column]]
  explicit = !is.na(raw_layer)
  layer = as.numeric(raw_layer)
  layer[!explicit] = 0

  metadata_value = function(column) {
    if (!(column %in% names(roads))) {
      return(rep(NA_character_, nrow(roads)))
    }
    roads[[column]]
  }
  bridge = is_truthy_render_road_osm_tag(metadata_value("bridge"))
  tunnel = is_truthy_render_road_osm_tag(metadata_value("tunnel"))
  location = normalize_render_road_osm_tag(metadata_value("location"))
  elevated_location = !is.na(location) &
    location %in% c("elevated", "overground")
  underground_location = !is.na(location) &
    location %in% c("underground", "underwater", "subway")
  upper = bridge | elevated_location
  lower = tunnel | underground_location
  inferred_upper = !explicit & upper & !lower
  inferred_lower = !explicit & lower & !upper
  conflicting = !explicit & upper & lower
  layer[inferred_upper] = 1
  layer[inferred_lower] = -1
  inferred = inferred_upper | inferred_lower
  source = rep("implicit_surface", nrow(roads))
  source[explicit] = "explicit_layer"
  source[inferred_upper & bridge] = "bridge"
  source[inferred_upper & !bridge] = "elevated_location"
  source[inferred_lower & tunnel] = "tunnel"
  source[inferred_lower & !tunnel] = "underground_location"
  source[conflicting] = "conflicting_structure_metadata"
  list(
    layer = layer,
    explicit = explicit,
    inferred = inferred,
    source = source
  )
}


#' Build rendered-path to topology-fragment membership
#'
#' @param topology Prepared road topology.
#' @param coord_feature Source feature index for every rendered path.
#' @param valid_path Logical vector identifying usable rendered paths.
#'
#' @return Schema-stable rendered-path membership table.
#' @keywords internal
build_render_road_path_mesh_members = function(
  topology,
  coord_feature,
  valid_path
) {
  path_count = length(coord_feature)
  members = data.frame(
    road_path_id = seq_len(path_count),
    render_road_fragment_id = rep(NA_integer_, path_count),
    render_road_feature_id = rep(NA_integer_, path_count),
    render_road_path_feature_index = as.integer(coord_feature),
    render_road_layer = rep(NA_real_, path_count),
    mapping_status = ifelse(valid_path, "unmapped", "invalid_path"),
    stringsAsFactors = FALSE
  )
  fragments = topology$fragments
  if (!nrow(fragments) || !path_count) {
    return(members)
  }
  fragment_feature = fragments$render_road_path_feature_index
  fragment_groups = split(
    seq_len(nrow(fragments)),
    as.character(fragment_feature)
  )
  path_groups = split(
    seq_len(path_count)[valid_path],
    as.character(coord_feature[valid_path])
  )
  common_feature = intersect(names(fragment_groups), names(path_groups))
  for (feature_key in common_feature) {
    fragment_rows = fragment_groups[[feature_key]]
    path_rows = path_groups[[feature_key]]
    if (length(fragment_rows) != length(path_rows)) {
      members$mapping_status[path_rows] = "ambiguous_feature_parts"
      next
    }
    fragment_rows = fragment_rows[
      order(fragments$render_road_fragment_id[fragment_rows])
    ]
    path_rows = sort(path_rows)
    members$render_road_fragment_id[path_rows] =
      fragments$render_road_fragment_id[fragment_rows]
    members$render_road_feature_id[path_rows] =
      fragments$render_road_feature_id[fragment_rows]
    members$render_road_layer[path_rows] =
      fragments$render_road_layer[fragment_rows]
    members$mapping_status[path_rows] = "mapped"
  }
  members
}

#' Map selected physical continuations to rendered road paths
#'
#' @param topology Prepared road topology.
#' @param path_members Rendered-path membership table.
#'
#' @return Schema-stable physical mesh-connection table.
#' @keywords internal
build_render_road_path_mesh_connections = function(
  topology,
  path_members
) {
  empty = data.frame(
    mesh_connection_id = integer(0),
    source_connection_id = integer(0),
    connection_type = character(0),
    road_path_id_a = integer(0),
    road_path_id_b = integer(0),
    render_road_fragment_id_a = integer(0),
    render_road_fragment_id_b = integer(0),
    endpoint_id_a = integer(0),
    endpoint_id_b = integer(0),
    endpoint_side_a = character(0),
    endpoint_side_b = character(0),
    layer_a = numeric(0),
    layer_b = numeric(0),
    lane_count_a = integer(0),
    lane_count_b = integer(0),
    exact_endpoint = logical(0),
    evidence_tier = character(0),
    evidence_subclass = character(0),
    direction_score = numeric(0),
    endpoint_distance = numeric(0),
    selected = logical(0),
    ambiguous = logical(0),
    mesh_chain_eligible = logical(0),
    diagnostic_reason = character(0),
    stringsAsFactors = FALSE
  )
  selected = topology$selected_continuations
  if (is.null(selected) || !nrow(selected)) {
    return(empty)
  }
  fragment_path = stats::setNames(
    path_members$road_path_id,
    as.character(path_members$render_road_fragment_id)
  )
  path_a = unname(fragment_path[as.character(selected$fragment_a)])
  path_b = unname(fragment_path[as.character(selected$fragment_b)])
  mapped = is.finite(path_a) & is.finite(path_b) & path_a != path_b
  if (!any(mapped)) {
    return(empty)
  }
  selected = selected[mapped, , drop = FALSE]
  path_a = as.integer(path_a[mapped])
  path_b = as.integer(path_b[mapped])
  layer_a = path_members$render_road_layer[
    match(path_a, path_members$road_path_id)
  ]
  layer_b = path_members$render_road_layer[
    match(path_b, path_members$road_path_id)
  ]
  exact_endpoint = as.logical(selected$exact_endpoint)
  lane_count_a = suppressWarnings(as.integer(selected$lane_count_a))
  lane_count_b = suppressWarnings(as.integer(selected$lane_count_b))
  lane_transition = exact_endpoint &
    is.finite(lane_count_a) &
    is.finite(lane_count_b) &
    lane_count_a != lane_count_b
  layer_transition = exact_endpoint &
    is.finite(layer_a) &
    is.finite(layer_b) &
    layer_a != layer_b
  accepted_layer_transition = layer_transition &
    (selected$same_way |
      selected$same_ref |
      selected$same_name |
      (selected$same_highway & selected$same_lanes))
  unresolved_layer_transition = layer_transition &
    !accepted_layer_transition
  connection_type = ifelse(
    accepted_layer_transition,
    "longitudinal_layer_transition",
    ifelse(
      unresolved_layer_transition,
      "unresolved_layer_transition",
      ifelse(
        lane_transition,
        "lane_count_transition",
        ifelse(
          exact_endpoint,
          "exact_continuation",
          "true_gap_continuation"
        )
      )
    )
  )
  mesh_chain_eligible = exact_endpoint & !unresolved_layer_transition
  diagnostic_reason = ifelse(
    accepted_layer_transition,
    "selected_longitudinal_layer_transition",
    ifelse(
      unresolved_layer_transition,
      "layer_transition_lacks_continuity_evidence",
      ifelse(
        exact_endpoint,
        "selected_physical_continuation",
        "nonexact_continuation_not_mesh_chain_edge"
      )
    )
  )
  source_connection_id = suppressWarnings(as.integer(
    selected$continuation_id
  ))
  source_connection_id[!is.finite(source_connection_id)] =
    seq_len(nrow(selected))[!is.finite(source_connection_id)]
  data.frame(
    mesh_connection_id = seq_len(nrow(selected)),
    source_connection_id = source_connection_id,
    connection_type = connection_type,
    road_path_id_a = path_a,
    road_path_id_b = path_b,
    render_road_fragment_id_a = as.integer(selected$fragment_a),
    render_road_fragment_id_b = as.integer(selected$fragment_b),
    endpoint_id_a = as.integer(selected$endpoint_a),
    endpoint_id_b = as.integer(selected$endpoint_b),
    endpoint_side_a = as.character(selected$side_a),
    endpoint_side_b = as.character(selected$side_b),
    layer_a = layer_a,
    layer_b = layer_b,
    lane_count_a = lane_count_a,
    lane_count_b = lane_count_b,
    exact_endpoint = exact_endpoint,
    evidence_tier = as.character(selected$evidence_tier),
    evidence_subclass = as.character(selected$evidence_subclass),
    direction_score = as.numeric(selected$direction_score),
    endpoint_distance = as.numeric(selected$endpoint_distance),
    selected = TRUE,
    ambiguous = FALSE,
    mesh_chain_eligible = mesh_chain_eligible,
    diagnostic_reason = diagnostic_reason,
    stringsAsFactors = FALSE
  )
}


#' Build physical mesh topology for rendered road paths
#'
#' @param topology Prepared road topology.
#' @param coord_feature Source feature index for every rendered path.
#' @param valid_path Logical vector identifying usable rendered paths.
#'
#' @return Physical mesh-topology tables and summary diagnostics.
#' @keywords internal
build_render_road_path_mesh_topology = function(
  topology,
  coord_feature,
  valid_path
) {
  path_members = build_render_road_path_mesh_members(
    topology = topology,
    coord_feature = coord_feature,
    valid_path = valid_path
  )
  selected_connections = build_render_road_path_mesh_connections(
    topology = topology,
    path_members = path_members
  )
  list(
    path_members = path_members,
    selected_connections = selected_connections,
    diagnostics = list(
      mapped_path_count = sum(path_members$mapping_status == "mapped"),
      unmapped_path_count = sum(path_members$mapping_status != "mapped"),
      selected_connection_count = nrow(selected_connections),
      exact_continuation_count = sum(
        selected_connections$connection_type == "exact_continuation"
      ),
      longitudinal_layer_transition_count = sum(
        selected_connections$connection_type == "longitudinal_layer_transition"
      ),
      unresolved_layer_transition_count = sum(
        selected_connections$connection_type == "unresolved_layer_transition"
      ),
      lane_count_transition_count = sum(
        selected_connections$connection_type == "lane_count_transition"
      ),
      true_gap_continuation_count = sum(
        selected_connections$connection_type == "true_gap_continuation"
      ),
      mesh_chain_edge_count = sum(selected_connections$mesh_chain_eligible)
    )
  )
}

#' Order one precomputed road mesh chain
#'
#' @param road_path_id Rendered path identifiers in one chain component.
#' @param selected_connections Exact selected continuation edges.
#' @param coord_list Solved road coordinate matrices.
#'
#' @return Ordered path identifiers, orientations, closure, and canonical key.
#' @keywords internal
order_render_road_precomputed_mesh_chain = function(
  road_path_id,
  selected_connections,
  coord_list
) {
  road_path_id = sort(unique(as.integer(road_path_id)))
  component_connections = selected_connections[
    selected_connections$road_path_id_a %in%
      road_path_id &
      selected_connections$road_path_id_b %in% road_path_id &
      selected_connections$mesh_chain_eligible,
    ,
    drop = FALSE
  ]
  endpoint_rows = lapply(road_path_id, function(path_id) {
    points = as.matrix(coord_list[[path_id]])
    endpoint = rbind(points[1L, 1:3], points[nrow(points), 1:3])
    neighbor = rbind(points[2L, 1:3], points[nrow(points) - 1L, 1:3])
    direction = neighbor - endpoint
    magnitude = sqrt(rowSums(direction^2))
    valid = is.finite(magnitude) & magnitude > sqrt(.Machine$double.eps)
    direction[valid, ] = direction[valid, , drop = FALSE] / magnitude[valid]
    direction[!valid, ] = 0
    data.frame(
      road_path_id = path_id,
      endpoint_side = c("start", "end"),
      x = endpoint[, 1L],
      y = endpoint[, 2L],
      z = endpoint[, 3L],
      direction_x = direction[, 1L],
      direction_y = direction[, 2L],
      direction_z = direction[, 3L],
      stringsAsFactors = FALSE
    )
  })
  endpoints = do.call(rbind, endpoint_rows)
  endpoint_key = paste0(
    endpoints$road_path_id,
    ":",
    endpoints$endpoint_side
  )
  endpoint_connection = stats::setNames(
    rep(NA_integer_, length(endpoint_key)),
    endpoint_key
  )
  if (nrow(component_connections)) {
    for (connection_row in seq_len(nrow(component_connections))) {
      key_a = paste0(
        component_connections$road_path_id_a[[connection_row]],
        ":",
        component_connections$endpoint_side_a[[connection_row]]
      )
      key_b = paste0(
        component_connections$road_path_id_b[[connection_row]],
        ":",
        component_connections$endpoint_side_b[[connection_row]]
      )
      if (
        !is.na(endpoint_connection[[key_a]]) ||
          !is.na(endpoint_connection[[key_b]])
      ) {
        stop(
          "A selected road continuation endpoint belongs to multiple chains.",
          call. = FALSE
        )
      }
      endpoint_connection[[key_a]] = connection_row
      endpoint_connection[[key_b]] = connection_row
    }
  }
  degree = vapply(
    road_path_id,
    function(path_id) {
      sum(is.finite(endpoint_connection[
        paste0(path_id, ":", c("start", "end"))
      ]))
    },
    integer(1)
  )
  if (any(degree > 2L)) {
    stop("A road mesh chain is not path structured.", call. = FALSE)
  }
  single_closed = FALSE
  if (length(road_path_id) == 1L) {
    points = as.matrix(coord_list[[road_path_id]])
    single_closed = nrow(points) > 2L &&
      sqrt(sum((points[1L, 1:3] - points[nrow(points), 1:3])^2)) <= 1e-8
  }
  closed = single_closed ||
    (length(road_path_id) > 1L && all(degree == 2L))
  candidate_endpoints = if (closed) {
    endpoints
  } else {
    unconnected = !is.finite(endpoint_connection[endpoint_key])
    endpoints[unconnected, , drop = FALSE]
  }
  if (!nrow(candidate_endpoints)) {
    stop("A road mesh chain has no canonical endpoint.", call. = FALSE)
  }
  canonical_order = order(
    candidate_endpoints$x,
    candidate_endpoints$z,
    candidate_endpoints$y,
    candidate_endpoints$direction_x,
    candidate_endpoints$direction_z,
    candidate_endpoints$direction_y,
    candidate_endpoints$endpoint_side
  )
  canonical_endpoint = candidate_endpoints[
    canonical_order[[1L]],
    ,
    drop = FALSE
  ]
  current_path = canonical_endpoint$road_path_id[[1L]]
  entry_side = canonical_endpoint$endpoint_side[[1L]]
  members = list()
  visited = integer(0)
  while (!(current_path %in% visited)) {
    members[[length(members) + 1L]] = data.frame(
      road_path_id = current_path,
      orientation = if (identical(entry_side, "start")) 1L else -1L,
      stringsAsFactors = FALSE
    )
    visited = c(visited, current_path)
    exit_side = if (identical(entry_side, "start")) "end" else "start"
    connection_row = endpoint_connection[[
      paste0(current_path, ":", exit_side)
    ]]
    if (!is.finite(connection_row)) {
      break
    }
    connection = component_connections[connection_row, , drop = FALSE]
    if (connection$road_path_id_a[[1L]] == current_path) {
      current_path = connection$road_path_id_b[[1L]]
      entry_side = connection$endpoint_side_b[[1L]]
    } else {
      current_path = connection$road_path_id_a[[1L]]
      entry_side = connection$endpoint_side_a[[1L]]
    }
  }
  if (!setequal(visited, road_path_id)) {
    stop("A selected road mesh chain could not be ordered.", call. = FALSE)
  }
  list(
    members = do.call(rbind, members),
    closed = closed,
    canonical_key = paste(
      sprintf("%.12f", canonical_endpoint$x[[1L]]),
      sprintf("%.12f", canonical_endpoint$z[[1L]]),
      sprintf("%.12f", canonical_endpoint$y[[1L]]),
      sprintf("%.12f", canonical_endpoint$direction_x[[1L]]),
      sprintf("%.12f", canonical_endpoint$direction_z[[1L]]),
      sep = ":"
    )
  )
}

#' Build precomputed road mesh-chain membership
#'
#' @param coord_list Solved road coordinate matrices.
#' @param path_members Rendered-path fragment membership.
#' @param selected_connections Exact selected continuation edges.
#' @param lanes Effective integer lane counts.
#' @param width Effective road widths.
#' @param texture_file Texture files by rendered path.
#' @param texture_length Texture lengths by rendered path.
#' @param texture_repeats Texture repeats by rendered path.
#'
#' @return Compact, ordered mesh-chain membership table.
#' @keywords internal
build_render_road_mesh_chain_members = function(
  coord_list,
  path_members,
  selected_connections,
  lanes,
  width,
  texture_file,
  texture_length,
  texture_repeats
) {
  path_count = length(coord_list)
  metadata_length = c(
    length(lanes),
    length(width),
    length(texture_file),
    length(texture_length),
    length(texture_repeats)
  )
  if (any(metadata_length != path_count)) {
    stop("Road mesh metadata must match the coordinate paths.", call. = FALSE)
  }
  valid_path = vapply(
    coord_list,
    function(points) {
      is.matrix(points) &&
        nrow(points) >= 2L &&
        ncol(points) >= 3L &&
        all(is.finite(points[, 1:3, drop = FALSE]))
    },
    logical(1)
  )
  valid_path_id = which(valid_path)
  if (!length(valid_path_id)) {
    return(data.frame(
      mesh_chain_id = integer(0),
      road_path_id = integer(0),
      fragment_id = integer(0),
      feature_id = integer(0),
      member_order = integer(0),
      orientation = integer(0),
      closed = logical(0),
      lanes = integer(0),
      width = numeric(0),
      texture_file = character(0),
      texture_length = numeric(0),
      texture_repeats = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  if (is.null(path_members)) {
    path_members = data.frame(
      road_path_id = seq_len(path_count),
      render_road_fragment_id = NA_integer_,
      render_road_feature_id = NA_integer_,
      stringsAsFactors = FALSE
    )
  }
  if (is.null(selected_connections)) {
    selected_connections = data.frame(
      road_path_id_a = integer(0),
      road_path_id_b = integer(0),
      endpoint_side_a = character(0),
      endpoint_side_b = character(0),
      mesh_chain_eligible = logical(0)
    )
  }
  selected_connections = selected_connections[
    selected_connections$mesh_chain_eligible &
      selected_connections$road_path_id_a %in% valid_path_id &
      selected_connections$road_path_id_b %in% valid_path_id,
    ,
    drop = FALSE
  ]
  graph = igraph::make_empty_graph(n = path_count, directed = FALSE)
  if (nrow(selected_connections)) {
    edge_vector = as.vector(t(as.matrix(
      selected_connections[,
        c("road_path_id_a", "road_path_id_b"),
        drop = FALSE
      ]
    )))
    graph = igraph::add_edges(graph, edge_vector)
  }
  component = igraph::components(graph)$membership
  component_paths = split(valid_path_id, component[valid_path_id])
  ordered = lapply(component_paths, function(path_id) {
    order_render_road_precomputed_mesh_chain(
      road_path_id = path_id,
      selected_connections = selected_connections,
      coord_list = coord_list
    )
  })
  component_order = order(vapply(
    ordered,
    function(value) value$canonical_key,
    character(1)
  ))
  ordered = ordered[component_order]
  rows = lapply(seq_along(ordered), function(chain_id) {
    members = ordered[[chain_id]]$members
    member_rows = lapply(seq_len(nrow(members)), function(member_order) {
      path_id = members$road_path_id[[member_order]]
      membership_row = match(path_id, path_members$road_path_id)
      texture = texture_file[[path_id]]
      if (is.null(texture) || !length(texture)) {
        texture = NA_character_
      } else {
        texture = as.character(texture[[1L]])
      }
      data.frame(
        mesh_chain_id = chain_id,
        road_path_id = path_id,
        fragment_id = if (is.finite(membership_row)) {
          as.integer(path_members$render_road_fragment_id[[membership_row]])
        } else {
          NA_integer_
        },
        feature_id = if (is.finite(membership_row)) {
          as.integer(path_members$render_road_feature_id[[membership_row]])
        } else {
          NA_integer_
        },
        member_order = member_order,
        orientation = members$orientation[[member_order]],
        closed = ordered[[chain_id]]$closed,
        lanes = as.integer(lanes[[path_id]]),
        width = as.numeric(width[[path_id]]),
        texture_file = texture,
        texture_length = as.numeric(texture_length[[path_id]]),
        texture_repeats = as.numeric(texture_repeats[[path_id]]),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, member_rows)
  })
  do.call(rbind, rows)
}

#' Select a terrain reference across one road cross-section
#'
#' @param coordinates Scene-coordinate road centerline.
#' @param width Road width in scene units.
#' @param heightmap Scene heightmap in elevation units.
#' @param zscale Effective scene zscale.
#' @param texture_world_scale Two-value scene-to-world horizontal scale.
#' @param lateral_samples Default `5L`. Number of terrain candidates sampled
#'   from one half-width left to one half-width right.
#'
#' @return Conditioned coordinates and terrain-stage diagnostics.
#' @keywords internal
condition_render_road_terrain_path = function(
  coordinates,
  width,
  heightmap,
  zscale,
  texture_world_scale,
  lateral_samples = 5L
) {
  coordinates = as.matrix(coordinates)
  point_count = nrow(coordinates)
  if (
    point_count < 2L ||
      !is.matrix(heightmap) ||
      any(!is.finite(coordinates[, 1:3, drop = FALSE]))
  ) {
    return(list(
      coordinates = coordinates,
      regime = "easy",
      hard_run_count = 0L,
      hard_point_count = 0L,
      maximum_lateral_relief = 0,
      selected_lateral_fraction = rep(0, point_count)
    ))
  }
  texture_world_scale = normalize_render_road_world_scale(
    texture_world_scale
  )
  width = suppressWarnings(as.numeric(width[[1L]]))
  if (!is.finite(width) || width <= 0) {
    width = 1
  }
  lateral_samples = max(3L, as.integer(lateral_samples[[1L]]))
  if (lateral_samples %% 2L == 0L) {
    lateral_samples = lateral_samples + 1L
  }
  world = cbind(
    coordinates[, 1L] * texture_world_scale[[1L]],
    coordinates[, 3L] * texture_world_scale[[2L]]
  )
  previous = c(1L, seq_len(point_count - 1L))
  following = c(seq.int(2L, point_count), point_count)
  tangent = world[following, , drop = FALSE] -
    world[previous, , drop = FALSE]
  magnitude = sqrt(rowSums(tangent^2))
  invalid_tangent = !is.finite(magnitude) |
    magnitude <= sqrt(.Machine$double.eps)
  tangent[!invalid_tangent, ] = tangent[!invalid_tangent, , drop = FALSE] /
    magnitude[!invalid_tangent]
  if (any(invalid_tangent)) {
    tangent[invalid_tangent, ] = matrix(
      c(1, 0),
      nrow = sum(invalid_tangent),
      ncol = 2L,
      byrow = TRUE
    )
  }
  side = cbind(-tangent[, 2L], tangent[, 1L])
  half_width_world = width * mean(texture_world_scale) / 2
  lateral_offset = seq(
    -half_width_world,
    half_width_world,
    length.out = lateral_samples
  )
  sample_x = as.vector(
    outer(
      side[, 1L] / texture_world_scale[[1L]],
      lateral_offset,
      `*`
    ) +
      coordinates[, 1L]
  )
  sample_z = as.vector(
    outer(
      side[, 2L] / texture_world_scale[[2L]],
      lateral_offset,
      `*`
    ) +
      coordinates[, 3L]
  )
  lateral_height = matrix(
    interpolate_render_heightmap_height(heightmap, sample_x, sample_z),
    nrow = point_count,
    ncol = lateral_samples
  )
  center_height = coordinates[, 2L] * zscale
  invalid_height = !is.finite(lateral_height)
  if (any(invalid_height)) {
    lateral_height[invalid_height] = center_height[
      row(lateral_height)[invalid_height]
    ]
  }
  station = calculate_road_path_cumulative_distance(
    coordinates,
    texture_world_scale = texture_world_scale
  )
  lateral_minimum = apply(lateral_height, 1L, min)
  lateral_maximum = apply(lateral_height, 1L, max)
  lateral_median = apply(lateral_height, 1L, stats::median)
  smooth_values = function(value, spar = 0.6) {
    if (length(value) < 4L || length(unique(station)) < 4L) {
      return(value)
    }
    tryCatch(
      stats::predict(
        stats::smooth.spline(
          x = station,
          y = value,
          spar = spar,
          all.knots = TRUE
        ),
        x = station
      )$y,
      error = function(error) value
    )
  }
  preliminary = smooth_values(lateral_median, spar = 0.55)
  target = preliminary + 0.35 * pmax(lateral_maximum - preliminary, 0)
  relief_scale = max(
    0.25,
    stats::median(lateral_maximum - lateral_minimum),
    na.rm = TRUE
  )
  local_cost = (lateral_height - target)^2 +
    0.35 * pmax(lateral_maximum - lateral_height, 0)^2
  cumulative_cost = matrix(Inf, point_count, lateral_samples)
  predecessor = matrix(1L, point_count, lateral_samples)
  cumulative_cost[1L, ] = local_cost[1L, ]
  if (point_count > 1L) {
    offset_scale = max(half_width_world, sqrt(.Machine$double.eps))
    transition_cost = 0.05 *
      relief_scale^2 *
      outer(lateral_offset, lateral_offset, function(first, second) {
        ((first - second) / offset_scale)^2
      })
    for (point in seq.int(2L, point_count)) {
      for (candidate in seq_len(lateral_samples)) {
        candidate_cost = cumulative_cost[point - 1L, ] +
          transition_cost[, candidate]
        best = which.min(candidate_cost)
        predecessor[point, candidate] = best
        cumulative_cost[point, candidate] =
          candidate_cost[[best]] + local_cost[point, candidate]
      }
    }
  }
  selected_index = integer(point_count)
  selected_index[[point_count]] = which.min(
    cumulative_cost[point_count, ]
  )
  if (point_count > 1L) {
    for (point in seq.int(point_count, 2L)) {
      selected_index[[point - 1L]] = predecessor[
        point,
        selected_index[[point]]
      ]
    }
  }
  selected_height = lateral_height[cbind(
    seq_len(point_count),
    selected_index
  )]
  run = diff(station)
  run[!is.finite(run) | run <= 0] = 1
  grade = diff(selected_height) / run
  grade_change = numeric(point_count)
  if (length(grade) > 1L) {
    change = abs(diff(grade))
    grade_change[seq.int(2L, point_count - 1L)] = pmax(
      grade_change[seq.int(2L, point_count - 1L)],
      change
    )
    grade_change[seq_len(point_count - 2L)] = pmax(
      grade_change[seq_len(point_count - 2L)],
      change
    )
  }
  lateral_relief = lateral_maximum - lateral_minimum
  cross_grade = lateral_relief / max(2 * half_width_world, 0.1)
  severity = pmax(
    (cross_grade - 0.08) / 0.24,
    (grade_change - 0.05) / 0.25,
    0
  )
  severity = pmin(severity, 1)
  hard = severity > 0
  if (any(hard) && point_count > 2L) {
    hard = hard |
      c(FALSE, hard[-point_count]) |
      c(hard[-1L], FALSE)
    severity[hard] = pmax(severity[hard], 0.25)
  }
  if (!any(hard)) {
    return(list(
      coordinates = coordinates,
      regime = "easy",
      hard_run_count = 0L,
      hard_point_count = 0L,
      maximum_lateral_relief = max(lateral_relief),
      selected_lateral_fraction = lateral_offset[selected_index] /
        max(half_width_world, sqrt(.Machine$double.eps))
    ))
  }
  smoothed_height = smooth_values(selected_height, spar = 0.65)
  conditioned_height = selected_height +
    severity * (smoothed_height - selected_height)
  conditioned_height = pmin(
    pmax(conditioned_height, lateral_minimum),
    lateral_maximum
  )
  coordinates[, 2L] = conditioned_height / zscale
  hard_start = hard & !c(FALSE, utils::head(hard, -1L))
  list(
    coordinates = coordinates,
    regime = if (any(hard)) "hard" else "easy",
    hard_run_count = sum(hard_start),
    hard_point_count = sum(hard),
    maximum_lateral_relief = max(lateral_relief),
    selected_lateral_fraction = lateral_offset[selected_index] /
      max(half_width_world, sqrt(.Machine$double.eps))
  )
}

#' Apply lateral terrain conditioning to road paths
#'
#' @param coord_list Road coordinate matrices.
#' @param coord_width Road widths by coordinate path.
#' @param heightmap Scene heightmap.
#' @param zscale Effective scene zscale.
#' @param texture_world_scale Two-value scene-to-world horizontal scale.
#'
#' @return Conditioned coordinates, per-path regimes, and compact diagnostics.
#' @keywords internal
condition_render_road_terrain_paths = function(
  coord_list,
  coord_width,
  heightmap,
  zscale,
  texture_world_scale
) {
  result = Map(
    function(coordinates, width) {
      condition_render_road_terrain_path(
        coordinates = coordinates,
        width = width,
        heightmap = heightmap,
        zscale = zscale,
        texture_world_scale = texture_world_scale
      )
    },
    coord_list,
    as.list(coord_width)
  )
  hard_path = vapply(
    result,
    function(value) identical(value$regime, "hard"),
    logical(1)
  )
  list(
    coordinates = lapply(result, `[[`, "coordinates"),
    hard_path = hard_path,
    diagnostics = list(
      easy_path_count = sum(!hard_path),
      hard_path_count = sum(hard_path),
      hard_run_count = sum(vapply(
        result,
        `[[`,
        integer(1),
        "hard_run_count"
      )),
      hard_point_count = sum(vapply(
        result,
        `[[`,
        integer(1),
        "hard_point_count"
      )),
      maximum_lateral_relief = max(vapply(
        result,
        `[[`,
        numeric(1),
        "maximum_lateral_relief"
      ))
    )
  )
}

#' Solve road profiles for rendered path coordinates
#'
#' @param coord_list Terrain-sampled scene coordinate matrices.
#' @param coord_feature Source feature index for every coordinate matrix.
#' @param coord_width Road widths by coordinate path.
#' @param roads Prepared road features corresponding to `coord_feature`.
#' @param heightmap Scene heightmap in elevation units.
#' @param layer_column Column containing OSM-style layer values.
#' @param lane_column Default `NULL`. Optional lane-count column used as
#' continuation evidence.
#' @param lane_values Default `NULL`. Effective positive lane counts used by
#' the rendered paths.
#' @param layer_height_column Default `NULL`. Optional clearance column.
#' @param layer_spacing Default `5.5`. Fallback adjacent-layer clearance in
#' metres.
#' @param maximum_grade Default `0.15`. Maximum absolute longitudinal grade.
#' Positive infinity removes the grade bound.
#' @param continuation_grade_tolerance Default `0.14`. Maximum absolute grade
#' mismatch at selected road continuations. Continuation height remains exact.
#' @param zscale Effective scene zscale.
#' @param texture_world_scale Two-value x-z multiplier from scene units to
#' world units.
#'
#' @return Coordinate matrices with solved heights and profile diagnostics.
#' @keywords internal
# Engineering guardrail: before changing layered profile topology, anchors,
# terrain sampling, or constraints, read tools/README-road-profile-solver.md.
solve_render_road_path_profiles = function(
  coord_list,
  coord_feature,
  coord_width,
  roads,
  heightmap,
  layer_column,
  lane_column = NULL,
  lane_values = NULL,
  layer_height_column = NULL,
  layer_spacing = 5.5,
  maximum_grade = 0.15,
  continuation_grade_tolerance = 0.14,
  zscale = 1,
  texture_world_scale = c(1, 1)
) {
  if (!inherits(roads, "sf")) {
    stop("Road profile solving requires an `sf` road object.", call. = FALSE)
  }
  if (length(coord_feature) != length(coord_list)) {
    stop(
      "Rendered road feature indices must match the coordinate paths.",
      call. = FALSE
    )
  }
  if (length(coord_width) != length(coord_list)) {
    stop(
      "Rendered road widths must match the coordinate paths.",
      call. = FALSE
    )
  }
  texture_world_scale = normalize_render_road_world_scale(
    texture_world_scale
  )
  layer_spacing = if (is.null(layer_spacing)) {
    5.5
  } else {
    as.numeric(layer_spacing)
  }
  coord_list = lapply(
    coord_list,
    collapse_render_highquality_road_path_points,
    texture_world_scale = texture_world_scale
  )
  valid_path = vapply(
    coord_list,
    function(coords) {
      is.matrix(coords) &&
        nrow(coords) >= 2L &&
        ncol(coords) >= 3L &&
        all(is.finite(coords[, 1:3, drop = FALSE]))
    },
    logical(1)
  )
  valid_feature = is.finite(coord_feature) &
    coord_feature >= 1L &
    coord_feature <= nrow(roads)
  if (any(!valid_feature)) {
    stop("Rendered road feature indices are invalid.", call. = FALSE)
  }
  visible_feature = sort(unique(as.integer(coord_feature[valid_path])))
  terrain_following = rep(TRUE, length(coord_list))
  if (!length(visible_feature)) {
    attr(coord_list, "terrain_following") = terrain_following
    attr(coord_list, "profile_diagnostics") = list(
      solver = "sparse_qp",
      active_fragment_count = 0L,
      solve_component_count = 0L,
      refinement_iterations = 0L
    )
    return(coord_list)
  }

  profile_roads = roads[visible_feature, , drop = FALSE]
  profile_roads$render_road_path_feature_index = visible_feature
  if (!is.null(lane_values)) {
    lane_values = suppressWarnings(as.integer(lane_values))
    if (
      length(lane_values) != nrow(roads) ||
        any(!is.finite(lane_values) | lane_values < 1L)
    ) {
      stop(
        "Effective rendered lane counts must match the road features.",
        call. = FALSE
      )
    }
    lane_column = "render_road_effective_lanes"
    profile_roads[[lane_column]] = lane_values[visible_feature]
  }
  prepared = prepare_render_road_layer_features(
    roads = profile_roads,
    layer_column = layer_column,
    lane_column = lane_column,
    layer_height_column = layer_height_column
  )
  topology = build_render_road_layer_topology(prepared)
  terrain_stage = condition_render_road_terrain_paths(
    coord_list = coord_list,
    coord_width = coord_width,
    heightmap = heightmap,
    zscale = zscale,
    texture_world_scale = texture_world_scale
  )
  coord_list = terrain_stage$coordinates
  terrain_following[terrain_stage$hard_path] = FALSE
  mesh_topology = build_render_road_path_mesh_topology(
    topology = topology,
    coord_feature = coord_feature,
    valid_path = valid_path
  )
  attr(coord_list, "mesh_topology") = mesh_topology
  active_fragment_id = topology$prospective_solve_fragment_id
  if (!length(active_fragment_id)) {
    attr(coord_list, "terrain_following") = terrain_following
    attr(coord_list, "profile_diagnostics") = list(
      solver = "sparse_qp",
      active_fragment_count = 0L,
      solve_component_count = 0L,
      refinement_iterations = 0L,
      terrain_stage = terrain_stage$diagnostics
    )
    return(coord_list)
  }

  fragments = topology$fragments
  active_row = fragments$render_road_fragment_id %in% active_fragment_id
  active_fragments = fragments[active_row, , drop = FALSE]
  path_by_feature = split(
    seq_along(coord_list)[valid_path],
    as.character(coord_feature[valid_path])
  )
  terrain_profiles = vector("list", nrow(active_fragments))
  names(terrain_profiles) = as.character(
    active_fragments$render_road_fragment_id
  )
  fragment_path = integer(nrow(active_fragments))
  for (fragment_row in seq_len(nrow(active_fragments))) {
    feature_index = active_fragments$render_road_path_feature_index[[
      fragment_row
    ]]
    path_index = path_by_feature[[as.character(feature_index)]]
    if (length(path_index) != 1L) {
      stop(
        sprintf(
          paste0(
            "Active road fragment %s must map to exactly one rendered ",
            "coordinate path."
          ),
          active_fragments$render_road_fragment_id[[fragment_row]]
        ),
        call. = FALSE
      )
    }
    fragment_path[[fragment_row]] = path_index
    coordinates = coord_list[[path_index]]
    path_distance = calculate_road_path_cumulative_distance(
      coordinates,
      texture_world_scale = texture_world_scale
    )
    path_length = utils::tail(path_distance, 1L)
    geometry_info = calculate_render_road_metric_line_distances(
      sf::st_geometry(active_fragments)[[fragment_row]]
    )
    if (!is.finite(path_length) || path_length <= 0) {
      stop("Rendered road paths must have positive length.", call. = FALSE)
    }
    terrain_profiles[[fragment_row]] = data.frame(
      distance = path_distance * geometry_info$length / path_length,
      elevation = coordinates[, 2L] * zscale
    )
  }

  problem = build_render_road_profile_problem(
    topology = topology,
    terrain_profiles = terrain_profiles,
    layer_spacing = layer_spacing,
    maximum_grade = maximum_grade,
    continuation_grade_tolerance = continuation_grade_tolerance
  )
  solution = solve_render_road_profile_problem(
    problem,
    maximum_iterations = 100000,
    profile_tolerance = 0.05
  )
  problem = solution$problem
  solved_fragment_id = problem$topology$fragments$render_road_fragment_id
  fragment_row = match(
    solved_fragment_id,
    active_fragments$render_road_fragment_id
  )
  for (solved_row in seq_along(solved_fragment_id)) {
    source_row = fragment_row[[solved_row]]
    path_index = fragment_path[[source_row]]
    coordinates = coord_list[[path_index]]
    path_distance = calculate_road_path_cumulative_distance(
      coordinates,
      texture_world_scale = texture_world_scale
    )
    fragment_length = problem$fragment_length[[
      as.character(solved_fragment_id[[solved_row]])
    ]]
    evaluation_distance = path_distance *
      fragment_length /
      utils::tail(path_distance, 1L)
    profile = evaluate_render_road_profile_at(
      problem = problem,
      solution = solution,
      fragment = solved_fragment_id[[solved_row]],
      distance = evaluation_distance
    )
    solved_height = profile$height / zscale
    terrain_following[[path_index]] =
      terrain_following[[path_index]] &&
      max(abs(solved_height - coordinates[, 2L])) <= 1e-3 / zscale
    coordinates[, 2L] = solved_height
    coord_list[[path_index]] = coordinates
  }

  component_id = unique(
    problem$topology$fragments$solve_component_id
  )
  attr(coord_list, "terrain_following") = terrain_following
  attr(coord_list, "profile_diagnostics") = list(
    solver = "sparse_qp",
    active_fragment_count = length(solved_fragment_id),
    solve_component_count = length(component_id),
    refinement_iterations = solution$refinement_iterations,
    maximum_violation = solution$engineering_audit$maximum_violation,
    engineering_tolerance = solution$engineering_audit$tolerance,
    engineering_audit_passed = solution$engineering_audit$passed,
    settings = problem$settings,
    terrain_stage = terrain_stage$diagnostics
  )
  coord_list
}

#' Offset road path coordinates
#'
#' @param coord_list List of scene coordinate matrices.
#' @param offset Vertical offset in scene units.
#' @param transition_length Quadratic transition length at each path end in
#' world units.
#' @param texture_world_scale Two-value x-z multiplier from scene units to
#' world units.
#'
#' @return List of offset road coordinate matrices.
#' @keywords internal
offset_render_road_path_coords = function(
  coord_list,
  offset,
  transition_length = 0,
  texture_world_scale = c(1, 1)
) {
  if (isTRUE(offset == 0)) {
    return(coord_list)
  }
  if (!is.finite(transition_length) || transition_length <= 0) {
    return(offset_render_line_coords(coord_list, offset))
  }
  lapply(coord_list, function(coords) {
    coords = collapse_render_highquality_road_path_points(
      coords,
      texture_world_scale = texture_world_scale
    )
    if (nrow(coords) < 2 || ncol(coords) < 3) {
      return(coords)
    }
    path_distance = calculate_road_path_cumulative_distance(
      coords,
      texture_world_scale = texture_world_scale
    )
    if (length(path_distance) != nrow(coords)) {
      return(coords)
    }
    path_length = path_distance[[length(path_distance)]]
    local_transition = min(transition_length, path_length / 2)
    if (!is.finite(local_transition) || local_transition <= 0) {
      return(coords)
    }
    ramp_fraction = seq(0, 1, length.out = 9)
    profile_distance = sort(unique(c(
      path_distance,
      local_transition * ramp_fraction,
      path_length - local_transition * ramp_fraction
    )))
    coords = vapply(
      seq_len(ncol(coords)),
      function(column) {
        stats::approx(
          x = path_distance,
          y = coords[, column],
          xout = profile_distance,
          ties = "ordered"
        )$y
      },
      numeric(length(profile_distance))
    )
    path_distance = profile_distance
    midpoint_height = stats::approx(
      x = path_distance,
      y = coords[, 2],
      xout = path_length / 2,
      ties = "ordered"
    )$y
    if (!is.finite(midpoint_height)) {
      return(coords)
    }
    deck_height = midpoint_height + offset
    start_height = coords[1, 2]
    end_height = coords[nrow(coords), 2]
    coords[, 2] = deck_height

    rising = path_distance < local_transition
    rise_progress = path_distance[rising] / local_transition
    coords[rising, 2] = start_height +
      (deck_height - start_height) *
        (1 - (1 - rise_progress)^2)

    falling = path_distance > path_length - local_transition
    fall_progress = (path_length - path_distance[falling]) /
      local_transition
    coords[falling, 2] = end_height +
      (deck_height - end_height) *
        (1 - (1 - fall_progress)^2)
    coords
  })
}


# Phase 1 topology pipeline overview:
# 1. Normalize road features into clean metric LINESTRING fragments.
# 2. Detect exact point crossings, shared junctions, and line overlaps.
# 3. Infer conservative through-road continuations at fragment endpoints.
# 4. Build graph objects and visual diagnostics for later profile solving.
#
# The output of this phase does not assign road elevations. It establishes the
# sparse local relationships that the later quadratic profile solver will use.
# Road layer feature preparation -------------------------------------------

#' Resolve a metric CRS for road topology
#'
#' @param roads Road features with a defined CRS.
#'
#' @return An `sf` CRS using metre units near the road data.
#' @keywords internal
resolve_render_road_metric_crs = function(roads) {
  # Validate the spatial input before any metric distance or topology work.
  # Every later tolerance is expressed in metres, so an undefined CRS is fatal.
  if (!inherits(roads, "sf") || is.na(sf::st_crs(roads))) {
    stop(
      "Road layer topology requires an `sf` object with a defined CRS.",
      call. = FALSE
    )
  }
  # Reuse an existing projected metre-based CRS when possible. This avoids an
  # unnecessary transformation and preserves the caller's local coordinate frame.
  source_crs = sf::st_crs(roads)
  source_units = source_crs$units_gdal
  if (is.null(source_units) || is.na(source_units)) {
    source_units = ""
  }
  source_units = tolower(source_units)
  metric_units = source_units %in% c("m", "meter", "metre")
  if (!isTRUE(sf::st_is_longlat(roads)) && metric_units) {
    return(source_crs)
  }

  # Otherwise derive the UTM zone from the center of the road extent. UTM gives
  # locally meaningful distances for intersection tolerances and path lengths.
  # This is appropriate for a city-scale render, not a continental road network.
  center = sf::st_as_sfc(sf::st_bbox(roads)) |>
    sf::st_centroid() |>
    sf::st_transform(4326) |>
    sf::st_coordinates()
  longitude = center[[1L, 1L]]
  latitude = center[[1L, 2L]]
  if (!is.finite(longitude) || !is.finite(latitude)) {
    stop(
      "Could not derive a local metric CRS for road topology.",
      call. = FALSE
    )
  }
  zone = min(max(floor((longitude + 180) / 6) + 1L, 1L), 60L)
  epsg = if (latitude >= 0) 32600L + zone else 32700L + zone
  sf::st_crs(epsg)
}

#' Collapse consecutive duplicate road line coordinates
#'
#' @param original_geometry Road fragment geometry in its source CRS.
#' @param metric_geometry Matching road fragment geometry in a metric CRS.
#' @param minimum_step Default `1e-3`. Minimum retained horizontal separation
#' in metres.
#'
#' @return Original and metric geometries with matching retained coordinates.
#' @keywords internal
collapse_render_road_line_coordinates = function(
  original_geometry,
  metric_geometry,
  minimum_step = 1e-3
) {
  # Extract matching coordinate matrices from the source and metric geometries.
  # Both matrices must retain the same rows so horizontal cleanup does not break
  # the mapping back to the source geometry used by the renderer.
  original_coords = unclass(original_geometry)
  metric_coords = unclass(metric_geometry)
  # Reject malformed or too-short fragments early. Returning NULL lets the caller
  # drop the fragment while preserving diagnostics about the original fragment ID.
  if (
    !is.matrix(original_coords) ||
      !is.matrix(metric_coords) ||
      nrow(original_coords) != nrow(metric_coords) ||
      nrow(metric_coords) < 2L
  ) {
    return(NULL)
  }
  finite_xy = stats::complete.cases(
    original_coords[, 1:2, drop = FALSE],
    metric_coords[, 1:2, drop = FALSE]
  )
  # Never reconnect finite coordinates across an unknown malformed section. Splitting
  # into finite runs would require assigning new fragment lineage, so Phase 1 drops the
  # complete fragment and records the reason instead.
  if (any(!finite_xy)) {
    return(NULL)
  }
  removed_nonfinite_coordinate_count = 0L
  # Normalize the minimum retained step. The 1 mm default removes numerically
  # meaningless vertices that can later create degenerate road mesh triangles.
  minimum_step = suppressWarnings(as.numeric(minimum_step[[1L]]))
  if (!is.finite(minimum_step) || minimum_step <= 0) {
    minimum_step = 1e-3
  }

  # Greedily retain the first point and then only points separated from the last
  # retained point by more than minimum_step. This collapses runs of near-duplicates
  # without changing the broad horizontal centerline.
  keep = rep(FALSE, nrow(metric_coords))
  keep[[1L]] = TRUE
  previous = 1L
  for (index in seq.int(2L, nrow(metric_coords))) {
    separation = sqrt(sum(
      (metric_coords[index, 1:2] - metric_coords[previous, 1:2])^2
    ))
    if (is.finite(separation) && separation > minimum_step) {
      keep[[index]] = TRUE
      previous = index
    }
  }
  # Preserve the original final endpoint. When the final point is too close to the
  # previous retained point, replace that previous point rather than truncating the
  # road. Endpoint identity matters for junction and continuation detection.
  final = nrow(metric_coords)
  if (!keep[[final]] && previous != 1L) {
    keep[[previous]] = FALSE
    keep[[final]] = TRUE
  }
  # A fragment with fewer than two retained points cannot define a road segment.
  if (sum(keep) < 2L) {
    return(NULL)
  }
  retained = which(keep)
  retained_step = sqrt(rowSums(
    diff(metric_coords[retained, 1:2, drop = FALSE])^2
  ))
  if (any(!is.finite(retained_step) | retained_step <= minimum_step)) {
    return(NULL)
  }
  # Rebuild source-CRS and metric geometries from the same logical row mask. The
  # renderer keeps the source path, while topology calculations use metric space.
  list(
    original = sf::st_linestring(original_coords[keep, , drop = FALSE]),
    metric = sf::st_linestring(metric_coords[keep, , drop = FALSE]),
    removed_nonfinite_coordinate_count = removed_nonfinite_coordinate_count
  )
}

#' Clean matching source and metric road fragments
#'
#' @param source_fragments Road fragments in the source CRS.
#' @param metric_fragments Matching road fragments in a metric CRS.
#' @param minimum_step Default `1e-3`. Minimum retained horizontal separation
#' in metres.
#'
#' @return Clean source and metric fragments plus dropped fragment identifiers.
#' @keywords internal
clean_render_road_line_fragments = function(
  source_fragments,
  metric_fragments,
  minimum_step = 1e-3
) {
  # Pull both geometry vectors once so the cleanup operation can be applied in lockstep.
  source_geometry = sf::st_geometry(source_fragments)
  metric_geometry = sf::st_geometry(metric_fragments)
  # Clean every fragment independently. This is deliberately feature-local: no
  # attempt is made here to merge roads or infer network connectivity.
  cleaned = lapply(seq_along(source_geometry), function(index) {
    collapse_render_road_line_coordinates(
      original_geometry = source_geometry[[index]],
      metric_geometry = metric_geometry[[index]],
      minimum_step = minimum_step
    )
  })
  # Drop fragments that collapsed below two usable points and retain their IDs for
  # diagnostics. Stable IDs are more useful than renumbering after cleanup.
  keep = !vapply(cleaned, is.null, logical(1))
  dropped = source_fragments$render_road_fragment_id[!keep]
  dropped_reason = vapply(
    which(!keep),
    function(index) {
      source_coordinates = unclass(source_geometry[[index]])
      metric_coordinates = unclass(metric_geometry[[index]])
      malformed = !is.matrix(source_coordinates) ||
        !is.matrix(metric_coordinates) ||
        nrow(source_coordinates) != nrow(metric_coordinates)
      if (malformed) {
        return("malformed_geometry")
      }
      finite_xy = stats::complete.cases(
        source_coordinates[, 1:2, drop = FALSE],
        metric_coordinates[, 1:2, drop = FALSE]
      )
      if (any(!finite_xy)) {
        return("nonfinite_xy_coordinate")
      }
      "collapsed_below_minimum_step"
    },
    character(1)
  )
  dropped_fragment_diagnostics = data.frame(
    render_road_fragment_id = dropped,
    reason = dropped_reason,
    stringsAsFactors = FALSE
  )
  retained_cleanup = data.frame(
    render_road_fragment_id = source_fragments$render_road_fragment_id[keep],
    removed_nonfinite_coordinate_count = vapply(
      cleaned[keep],
      `[[`,
      integer(1),
      "removed_nonfinite_coordinate_count"
    )
  )
  source_fragments = source_fragments[keep, , drop = FALSE]
  metric_fragments = metric_fragments[keep, , drop = FALSE]
  cleaned = cleaned[keep]
  # Replace the geometry columns with the cleaned source and metric LINESTRINGs.
  sf::st_geometry(source_fragments) = sf::st_sfc(
    lapply(cleaned, `[[`, "original"),
    crs = sf::st_crs(source_fragments)
  )
  sf::st_geometry(metric_fragments) = sf::st_sfc(
    lapply(cleaned, `[[`, "metric"),
    crs = sf::st_crs(metric_fragments)
  )
  # Return both coordinate representations because later topology and rendering
  # stages need different CRSs but must refer to the same fragments.
  list(
    source = source_fragments,
    metric = metric_fragments,
    dropped_fragment_id = dropped,
    dropped_fragment_diagnostics = dropped_fragment_diagnostics,
    retained_cleanup = retained_cleanup
  )
}

#' Calculate a robust inward road-endpoint tangent
#'
#' @param geometry Metric LINESTRING geometry.
#' @param endpoint_side Endpoint side, either `"start"` or `"end"`.
#' @param direction_lookahead Default `8`. Physical look-ahead distance in
#' metres.
#'
#' @return Inward unit direction and the distance used to estimate it.
#' @keywords internal
calculate_render_road_endpoint_direction = function(
  geometry,
  endpoint_side,
  direction_lookahead = 8
) {
  coordinates = unclass(geometry)[, 1:2, drop = FALSE]
  if (
    nrow(coordinates) < 2L ||
      any(!is.finite(coordinates)) ||
      !(endpoint_side %in% c("start", "end"))
  ) {
    return(c(direction_x = NA_real_, direction_y = NA_real_, distance = 0))
  }
  segment = coordinates[-1L, , drop = FALSE] -
    coordinates[-nrow(coordinates), , drop = FALSE]
  segment_length = sqrt(rowSums(segment^2))
  valid_segment = is.finite(segment_length) & segment_length > 0
  if (!any(valid_segment)) {
    return(c(direction_x = NA_real_, direction_y = NA_real_, distance = 0))
  }
  cumulative = c(0, cumsum(segment_length))
  total_length = utils::tail(cumulative, 1L)
  lookahead_distance = min(direction_lookahead, total_length)
  target_distance = if (identical(endpoint_side, "start")) {
    lookahead_distance
  } else {
    total_length - lookahead_distance
  }
  interval = findInterval(
    target_distance,
    cumulative,
    all.inside = TRUE,
    rightmost.closed = TRUE
  )
  interval = min(interval, length(segment_length))
  interval_fraction = if (segment_length[[interval]] > 0) {
    (target_distance - cumulative[[interval]]) / segment_length[[interval]]
  } else {
    0
  }
  lookahead_point = coordinates[interval, ] +
    interval_fraction * segment[interval, ]
  endpoint = if (identical(endpoint_side, "start")) {
    coordinates[1L, ]
  } else {
    coordinates[nrow(coordinates), ]
  }
  direction = lookahead_point - endpoint
  direction_length = sqrt(sum(direction^2))
  if (!is.finite(direction_length) || direction_length <= 0) {
    fallback = if (identical(endpoint_side, "start")) {
      which(valid_segment)[[1L]]
    } else {
      utils::tail(which(valid_segment), 1L)
    }
    direction = if (identical(endpoint_side, "start")) {
      segment[fallback, ]
    } else {
      -segment[fallback, ]
    }
    direction_length = sqrt(sum(direction^2))
    lookahead_distance = segment_length[[fallback]]
  }
  c(
    direction_x = direction[[1L]] / direction_length,
    direction_y = direction[[2L]] / direction_length,
    distance = lookahead_distance
  )
}

#' Build road fragment endpoint diagnostics
#'
#' @param fragments Metric road fragments.
#' @param boundary Default `NULL`. Optional source-data boundary geometry.
#' @param boundary_tolerance Default `1`. Distance in metres used to identify
#' endpoints on the supplied-data boundary.
#' @param direction_lookahead Default `8`. Physical distance in metres used to
#' estimate an endpoint tangent.
#'
#' @return An `sf` endpoint table with inward directions and boundary flags.
#' @keywords internal
build_render_road_endpoint_table = function(
  fragments,
  boundary = NULL,
  boundary_tolerance = 1,
  direction_lookahead = 8
) {
  # Produce a schema-stable empty endpoint table for empty fragment collections.
  fragment_count = nrow(fragments)
  if (!fragment_count) {
    return(sf::st_sf(
      render_road_endpoint_id = integer(0),
      render_road_fragment_id = integer(0),
      endpoint_side = character(0),
      direction_x = numeric(0),
      direction_y = numeric(0),
      direction_lookahead_distance = numeric(0),
      supplied_boundary = logical(0),
      geometry = sf::st_sfc(crs = sf::st_crs(fragments))
    ))
  }
  # Normalize the tolerance used to flag endpoints near the supplied data boundary.
  boundary_tolerance = suppressWarnings(as.numeric(boundary_tolerance[[1L]]))
  if (!is.finite(boundary_tolerance) || boundary_tolerance < 0) {
    boundary_tolerance = 1
  }
  direction_lookahead = suppressWarnings(as.numeric(
    direction_lookahead[[1L]]
  ))
  if (!is.finite(direction_lookahead) || direction_lookahead <= 0) {
    direction_lookahead = 8
  }

  # Preallocate exactly two endpoint records per fragment: start and end.
  endpoint_geometry = vector("list", fragment_count * 2L)
  direction_x = rep(NA_real_, fragment_count * 2L)
  direction_y = rep(NA_real_, fragment_count * 2L)
  direction_lookahead_distance = rep(NA_real_, fragment_count * 2L)
  # Extract endpoint coordinates and estimate an inward unit tangent at each end.
  # The tangent is later used to decide which nearby fragment is the most plausible
  # continuation of the same physical road.
  for (fragment in seq_len(fragment_count)) {
    coordinates = unclass(sf::st_geometry(fragments)[[fragment]])
    endpoint_index = c(2L * fragment - 1L, 2L * fragment)
    endpoint_geometry[[endpoint_index[[1L]]]] = sf::st_point(coordinates[
      1L,
      1:2
    ])
    endpoint_geometry[[endpoint_index[[2L]]]] = sf::st_point(
      coordinates[nrow(coordinates), 1:2]
    )
    # Start tangents point from the start into the line; end tangents point from the
    # end back into the line. Opposite inward tangents therefore indicate a straight
    # through continuation at a shared endpoint.
    inward = rbind(
      calculate_render_road_endpoint_direction(
        sf::st_geometry(fragments)[[fragment]],
        "start",
        direction_lookahead
      ),
      calculate_render_road_endpoint_direction(
        sf::st_geometry(fragments)[[fragment]],
        "end",
        direction_lookahead
      )
    )
    direction_x[endpoint_index] = inward[, "direction_x"]
    direction_y[endpoint_index] = inward[, "direction_y"]
    direction_lookahead_distance[endpoint_index] = inward[, "distance"]
  }
  # Assemble endpoint IDs, parent fragment IDs, sides, tangents, and point geometry.
  endpoints = sf::st_sf(
    render_road_endpoint_id = seq_len(fragment_count * 2L),
    render_road_fragment_id = rep(
      fragments$render_road_fragment_id,
      each = 2L
    ),
    endpoint_side = rep(c("start", "end"), fragment_count),
    direction_x = direction_x,
    direction_y = direction_y,
    direction_lookahead_distance = direction_lookahead_distance,
    supplied_boundary = FALSE,
    geometry = sf::st_sfc(endpoint_geometry, crs = sf::st_crs(fragments))
  )

  # Normalize an optional caller-supplied boundary into the fragment CRS. When no
  # boundary is supplied, all endpoint boundary flags remain FALSE.
  if (is.null(boundary)) {
    return(endpoints)
  } else {
    if (inherits(boundary, "bbox")) {
      boundary = sf::st_as_sfc(boundary)
    } else if (inherits(boundary, "sf")) {
      boundary = sf::st_geometry(boundary)
    } else if (inherits(boundary, "sfg")) {
      boundary = sf::st_sfc(boundary)
    }
    if (!inherits(boundary, "sfc")) {
      stop("`boundary` must be an sf geometry or bounding box.", call. = FALSE)
    }
    if (is.na(sf::st_crs(boundary))) {
      stop("`boundary` must have a defined CRS.", call. = FALSE)
    }
    boundary = sf::st_transform(boundary, sf::st_crs(fragments))
  }
  # Flag endpoints within boundary_tolerance of the boundary line. A later profile
  # solver can avoid incorrectly forcing these clipped endpoints back to terrain.
  boundary_line = sf::st_boundary(sf::st_union(boundary))
  endpoints$supplied_boundary = lengths(sf::st_is_within_distance(
    endpoints,
    boundary_line,
    dist = boundary_tolerance
  )) >
    0L
  endpoints
}

#' Prepare road layer features for local topology
#'
#' @param roads Road line features.
#' @param layer_column Column containing OSM layer values.
#' @param lane_column Default `NULL`. Optional lane-count column used as
#' continuation evidence.
#' @param layer_height_column Default `NULL`. Optional feature clearance column.
#' @param boundary Default `NULL`. Optional supplied-data boundary geometry.
#' @param minimum_step Default `1e-3`. Minimum retained coordinate separation
#' in metres.
#' @param boundary_tolerance Default `1`. Boundary endpoint tolerance in metres.
#' @param direction_lookahead Default `8`. Physical distance in metres used to
#' estimate endpoint tangents.
#'
#' @return Prepared source fragments, metric fragments, endpoints, and metadata.
#' @keywords internal
prepare_render_road_layer_features = function(
  roads,
  layer_column,
  lane_column = NULL,
  layer_height_column = NULL,
  boundary = NULL,
  minimum_step = 1e-3,
  boundary_tolerance = 1,
  direction_lookahead = 8
) {
  # Assert package availability and the normalized sf input contract.
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for road layer topology.", call. = FALSE)
  }
  if (!inherits(roads, "sf")) {
    stop("Road layer topology requires an `sf` road object.", call. = FALSE)
  }
  # Assert the normalized layer column before fragmenting the geometry.
  if (!(layer_column %in% names(roads))) {
    stop(sprintf(
      "Prepared road data is missing layer column `%s`.",
      layer_column
    ))
  }
  # Limit the topology subsystem to line features. Other geometry types require
  # different intersection and path-distance semantics.
  geometry_type = as.character(sf::st_geometry_type(roads))
  supported = geometry_type %in% c("LINESTRING", "MULTILINESTRING")
  if (!all(supported)) {
    stop(
      "Road layer topology supports LINESTRING and MULTILINESTRING features.",
      call. = FALSE
    )
  }

  # Assign a stable source-feature ID before casting MULTILINESTRING rows. The cast
  # can create several fragments from one source feature, and later metadata must
  # still be traceable to the original sf row.
  roads$render_road_feature_id = if (
    "render_line_source_feature_id" %in% names(roads)
  ) {
    vapply(
      roads$render_line_source_feature_id,
      function(value) {
        value = unique(as.integer(value))
        if (length(value) != 1L || !is.finite(value)) {
          stop(
            "Layered road features must resolve to one source feature ID.",
            call. = FALSE
          )
        }
        value
      },
      integer(1)
    )
  } else {
    seq_len(nrow(roads))
  }
  source_crs = sf::st_crs(roads)
  metric_crs = resolve_render_road_metric_crs(roads)
  source_fragments = suppressWarnings(sf::st_cast(roads, "LINESTRING"))
  source_fragments$render_road_fragment_id = seq_len(nrow(source_fragments))
  metric_fragments = sf::st_transform(source_fragments, metric_crs)
  # Remove duplicate and sub-millimetre coordinates in matched source/metric paths.
  cleaned = clean_render_road_line_fragments(
    source_fragments = source_fragments,
    metric_fragments = metric_fragments,
    minimum_step = minimum_step
  )
  source_fragments = cleaned$source
  metric_fragments = cleaned$metric

  # Parse explicit OSM layers and infer only missing bridge/tunnel structure
  # values. The explicit and inferred flags remain separate for diagnostics and
  # later solver policy.
  layer_values = resolve_render_road_osm_layer_values(
    source_fragments,
    layer_column
  )
  layer_explicit = layer_values$explicit
  effective_layer = layer_values$layer
  lane_evidence = resolve_render_road_lane_evidence(
    source_fragments,
    lane_column
  )

  # Optionally read per-feature clearance values. These are physical separations for
  # future constraints; they are not used to turn raw OSM layer numbers into heights.
  clearance = rep(NA_real_, nrow(source_fragments))
  if (!is.null(layer_height_column)) {
    if (!(layer_height_column %in% names(source_fragments))) {
      stop(sprintf(
        "Prepared road data is missing layer-height column `%s`.",
        layer_height_column
      ))
    }
    clearance = as.numeric(source_fragments[[layer_height_column]])
  }

  # Resolve the first available OSM-style metadata column from a list of aliases.
  # Metadata is used as evidence when choosing through-road continuations.
  metadata_value = function(candidates) {
    column = candidates[candidates %in% names(source_fragments)][1L]
    if (!length(column) || is.na(column)) {
      return(rep(NA_character_, nrow(source_fragments)))
    }
    as.character(source_fragments[[column]])
  }
  # Build a normalized internal metadata schema shared by source and metric fragments.
  topology_columns = list(
    render_road_layer = effective_layer,
    render_road_layer_explicit = layer_explicit,
    render_road_layer_inferred = layer_values$inferred,
    render_road_layer_source = layer_values$source,
    render_road_clearance = clearance,
    render_road_lanes = lane_evidence$lane_count,
    render_road_lane_source = lane_evidence$source,
    render_road_way_id = metadata_value(c("osm_id", "way_id")),
    render_road_ref = metadata_value("ref"),
    render_road_name = metadata_value("name"),
    render_road_highway = metadata_value("highway"),
    render_road_bridge = metadata_value("bridge"),
    render_road_tunnel = metadata_value("tunnel"),
    render_road_location = metadata_value("location")
  )
  # Copy normalized columns into both representations so later functions do not need
  # to repeatedly join source metadata back onto metric geometries.
  for (column in names(topology_columns)) {
    source_fragments[[column]] = topology_columns[[column]]
    metric_fragments[[column]] = topology_columns[[column]]
  }

  # Derive endpoint geometry, tangents, and boundary flags from cleaned metric paths.
  endpoints = build_render_road_endpoint_table(
    fragments = metric_fragments,
    boundary = boundary,
    boundary_tolerance = boundary_tolerance,
    direction_lookahead = direction_lookahead
  )
  # Copy the fragment metadata needed by endpoint continuation scoring onto endpoints.
  endpoint_fragment = match(
    endpoints$render_road_fragment_id,
    metric_fragments$render_road_fragment_id
  )
  for (column in c(
    "render_road_feature_id",
    "render_road_layer",
    "render_road_layer_explicit",
    "render_road_layer_inferred",
    "render_road_layer_source",
    "render_road_lanes",
    "render_road_lane_source",
    "render_road_way_id",
    "render_road_ref",
    "render_road_name",
    "render_road_highway",
    "render_road_bridge",
    "render_road_tunnel",
    "render_road_location",
    "render_road_clearance"
  )) {
    endpoints[[column]] = metric_fragments[[column]][endpoint_fragment]
  }

  # Return a self-contained prepared object. This is the stable input contract for
  # exact event detection, endpoint matching, graph construction, and diagnostics.
  list(
    source_fragments = source_fragments,
    fragments = metric_fragments,
    endpoints = endpoints,
    source_crs = source_crs,
    metric_crs = metric_crs,
    layer_column = layer_column,
    lane_column = lane_column,
    layer_height_column = layer_height_column,
    dropped_fragment_id = cleaned$dropped_fragment_id,
    dropped_fragment_diagnostics = cleaned$dropped_fragment_diagnostics,
    retained_cleanup = cleaned$retained_cleanup,
    minimum_step = minimum_step,
    boundary_tolerance = boundary_tolerance,
    direction_lookahead = direction_lookahead
  )
}

# Road layer event topology -----------------------------------------------

#' Project a topology point onto a road fragment
#'
#' @param geometry Metric LINESTRING geometry.
#' @param point Two-value metric point coordinate.
#' @param endpoint_tolerance Default `1e-2`. Endpoint tolerance in metres.
#'
#' @return Projection distance, separation, and endpoint classification.
#' @keywords internal
project_render_road_topology_point = function(
  geometry,
  point,
  endpoint_tolerance = 1e-2
) {
  # Work directly with the metric XY centerline. Elevation is deliberately excluded
  # because Phase 1 is only establishing horizontal topology.
  coordinates = unclass(geometry)[, 1:2, drop = FALSE]
  if (nrow(coordinates) < 2L) {
    return(NULL)
  }
  # Represent the polyline as segment vectors and identify nonzero segments.
  segment = coordinates[-1L, , drop = FALSE] -
    coordinates[-nrow(coordinates), , drop = FALSE]
  segment_length_squared = rowSums(segment^2)
  valid = is.finite(segment_length_squared) & segment_length_squared > 0
  if (!any(valid)) {
    return(NULL)
  }
  # Orthogonally project the point onto every segment, clamping each projection to
  # the finite segment. This handles intersections that land at vertices or interiors.
  difference = sweep(
    coordinates[-nrow(coordinates), , drop = FALSE],
    2,
    as.numeric(point[1:2]),
    FUN = "-"
  )
  fraction = -rowSums(difference * segment) / segment_length_squared
  fraction = pmin(pmax(fraction, 0), 1)
  projection = coordinates[-nrow(coordinates), , drop = FALSE] +
    fraction * segment
  # Measure point-to-projection separation and choose the closest valid segment.
  separation_squared = rowSums(
    sweep(
      projection,
      2,
      as.numeric(point[1:2]),
      FUN = "-"
    )^2
  )
  separation_squared[!valid] = Inf
  closest = which.min(separation_squared)
  if (!length(closest) || !is.finite(separation_squared[[closest]])) {
    return(NULL)
  }
  # Convert the local segment fraction into cumulative distance along the full road.
  # That scalar becomes the control location used by the later vertical profile model.
  segment_length = sqrt(segment_length_squared)
  cumulative = c(0, cumsum(segment_length))
  distance = cumulative[[closest]] +
    fraction[[closest]] * segment_length[[closest]]
  total_length = utils::tail(cumulative, 1L)
  # Classify whether the projected point lies at either road endpoint within a mixed
  # absolute/relative tolerance. Endpoint classification separates junctions from
  # interior grade-separated crossings.
  tolerance = max(endpoint_tolerance, total_length * 1e-10)
  list(
    distance = distance,
    total_length = total_length,
    separation = sqrt(separation_squared[[closest]]),
    segment_index = closest,
    segment_fraction = fraction[[closest]],
    at_start = distance <= tolerance,
    at_end = distance >= total_length - tolerance
  )
}

#' Extract point and line parts from a road intersection
#'
#' @param intersection Exact `sf` intersection geometry.
#'
#' @return Point and LINESTRING simple-feature collections.
#' @keywords internal
extract_render_road_topology_intersection_parts = function(intersection) {
  # Preserve the intersection CRS and prepare schema-stable empty geometry vectors.
  intersection_crs = if (is.null(intersection)) {
    sf::st_crs(NA)
  } else {
    sf::st_crs(intersection)
  }
  empty_points = sf::st_sfc(crs = intersection_crs)
  empty_lines = sf::st_sfc(crs = intersection_crs)
  # Empty or failed intersections contribute no point or overlap events.
  if (
    is.null(intersection) ||
      !length(intersection) ||
      all(sf::st_is_empty(intersection))
  ) {
    return(list(points = empty_points, lines = empty_lines))
  }

  # Extract one geometry type at a time from GEOMETRYCOLLECTION results. Line-line
  # intersections can contain points, line overlaps, or a mixture of both.
  extract_type = function(type) {
    extracted = tryCatch(
      suppressWarnings(sf::st_collection_extract(intersection, type)),
      error = function(error) NULL
    )
    if (is.null(extracted) || !length(extracted)) {
      return(sf::st_sfc(crs = intersection_crs))
    }
    extracted = extracted[!sf::st_is_empty(extracted)]
    if (!length(extracted)) {
      return(sf::st_sfc(crs = intersection_crs))
    }
    suppressWarnings(sf::st_cast(extracted, type))
  }
  # Return point events and overlap sections separately because they produce different
  # constraint structures in the eventual elevation solver.
  list(
    points = extract_type("POINT"),
    lines = extract_type("LINESTRING")
  )
}

#' Group pairwise road point events into physical events
#'
#' @param pair_events Pairwise point-event `sf` rows.
#' @param fragments Prepared metric road fragments.
#' @param event_name Event identifier prefix.
#' @param tolerance Default `1e-2`. Point grouping tolerance in metres.
#'
#' @return Physical events, participants, and pair relationships.
#' @keywords internal
group_render_road_point_events = function(
  pair_events,
  fragments,
  event_name,
  tolerance = 1e-2
) {
  # igraph is used only for proximity-component grouping; no custom graph traversal
  # or union-find implementation is needed.
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The `igraph` package is required for road layer topology.")
  }
  # Construct stable empty event, participant, and pair schemas so downstream code
  # can operate without special-case column checks.
  event_id_column = paste0(event_name, "_id")
  if (!nrow(pair_events)) {
    events = sf::st_sf(
      event_id = integer(0),
      participant_count = integer(0),
      layer_min = numeric(0),
      layer_max = numeric(0),
      event_spread = numeric(0),
      geometry = sf::st_sfc(crs = sf::st_crs(fragments))
    )
    names(events)[names(events) == "event_id"] = event_id_column
    participants = data.frame(
      event_id = integer(0),
      render_road_fragment_id = integer(0),
      render_road_feature_id = integer(0),
      distance = numeric(0),
      render_road_layer = numeric(0),
      render_road_layer_explicit = logical(0)
    )
    names(participants)[names(participants) == "event_id"] = event_id_column
    pairs = sf::st_drop_geometry(pair_events)
    pairs[[event_id_column]] = integer(0)
    return(list(events = events, participants = participants, pairs = pairs))
  }

  # Find pair-event points within tolerance of one another. Pairwise intersections from
  # a three-road stack should become one physical event, not three independent events.
  neighbors = sf::st_is_within_distance(
    pair_events,
    pair_events,
    dist = tolerance
  )
  # Convert the proximity relation into an undirected edge list without self-edges or
  # duplicate reverse edges.
  edge_rows = lapply(seq_along(neighbors), function(from) {
    to = neighbors[[from]]
    to = to[to > from]
    if (!length(to)) {
      return(NULL)
    }
    cbind(from = from, to = to)
  })
  edge_rows = Filter(Negate(is.null), edge_rows)
  edges = if (length(edge_rows)) {
    do.call(rbind, edge_rows)
  } else {
    matrix(integer(0), ncol = 2L)
  }
  # Connected components implement single-linkage grouping of nearby pair events.
  graph = igraph::make_empty_graph(n = nrow(pair_events), directed = FALSE)
  if (nrow(edges)) {
    graph = igraph::add_edges(graph, as.vector(t(edges)))
  }
  membership = igraph::components(graph)$membership
  membership = match(membership, unique(membership))
  pair_events[[event_id_column]] = membership

  # Split pair rows by physical event and choose a representative event point.
  event_groups = split(seq_len(nrow(pair_events)), membership)
  pair_coordinates = sf::st_coordinates(pair_events)[, 1:2, drop = FALSE]
  event_geometry = sf::st_sfc(
    lapply(event_groups, function(rows) {
      sf::st_point(colMeans(pair_coordinates[rows, , drop = FALSE]))
    }),
    crs = sf::st_crs(pair_events)
  )
  # Summarize each event by participant count and observed layer range. This describes
  # the local vertical stack without assigning physical elevations yet.
  event_rows = lapply(seq_along(event_groups), function(event_id) {
    rows = event_groups[[event_id]]
    fragments_present = unique(c(
      pair_events$fragment_a[rows],
      pair_events$fragment_b[rows]
    ))
    event_coordinates = pair_coordinates[rows, , drop = FALSE]
    event_spread = if (nrow(event_coordinates) > 1L) {
      max(stats::dist(event_coordinates))
    } else {
      0
    }
    data.frame(
      event_id = event_id,
      participant_count = length(fragments_present),
      layer_min = min(
        fragments$render_road_layer[match(
          fragments_present,
          fragments$render_road_fragment_id
        )]
      ),
      layer_max = max(
        fragments$render_road_layer[match(
          fragments_present,
          fragments$render_road_fragment_id
        )]
      ),
      event_spread = event_spread
    )
  })
  events = sf::st_sf(
    do.call(rbind, event_rows),
    geometry = event_geometry,
    crs = sf::st_crs(pair_events)
  )
  names(events)[names(events) == "event_id"] = event_id_column

  # Build one participant row per road fragment per event. Averaging repeated pairwise
  # distances reconciles the same road appearing in several pair relationships at a
  # multi-road crossing.
  participant_rows = lapply(seq_along(event_groups), function(event_id) {
    rows = event_groups[[event_id]]
    fragment_id = unique(c(
      pair_events$fragment_a[rows],
      pair_events$fragment_b[rows]
    ))
    data_rows = lapply(fragment_id, function(fragment) {
      distance = c(
        pair_events$distance_a[rows][pair_events$fragment_a[rows] == fragment],
        pair_events$distance_b[rows][pair_events$fragment_b[rows] == fragment]
      )
      fragment_row = match(fragment, fragments$render_road_fragment_id)
      data.frame(
        event_id = event_id,
        render_road_fragment_id = fragment,
        render_road_feature_id = fragments$render_road_feature_id[[
          fragment_row
        ]],
        distance = mean(distance),
        render_road_layer = fragments$render_road_layer[[fragment_row]],
        render_road_layer_explicit = fragments$render_road_layer_explicit[[
          fragment_row
        ]]
      )
    })
    do.call(rbind, data_rows)
  })
  participants = do.call(rbind, participant_rows)
  names(participants)[names(participants) == "event_id"] = event_id_column
  # Retain the original pair relationships with event IDs for detailed diagnostics and
  # later construction of adjacent upper/lower constraints.
  pairs = sf::st_drop_geometry(pair_events)
  list(events = events, participants = participants, pairs = pairs)
}

#' Classify one exact road point pair without deciding solver behavior
#'
#' @param projection_a Projection on fragment A.
#' @param projection_b Projection on fragment B.
#' @param layer_relationship Whether the effective layers differ.
#' @param supplied_boundary_a Whether fragment A meets a supplied boundary.
#' @param supplied_boundary_b Whether fragment B meets a supplied boundary.
#'
#' @return Topology relation, boundary annotation, and conflict flag.
#' @keywords internal
classify_render_road_point_pair = function(
  projection_a,
  projection_b,
  layer_relationship,
  supplied_boundary_a,
  supplied_boundary_b
) {
  endpoint_a = projection_a$at_start || projection_a$at_end
  endpoint_b = projection_b$at_start || projection_b$at_end
  topology_relation = if (endpoint_a && endpoint_b) {
    "shared_endpoint"
  } else if (xor(endpoint_a, endpoint_b)) {
    "endpoint_to_interior"
  } else if (layer_relationship) {
    "interior_crossing"
  } else {
    "equal_layer_interior"
  }
  boundary_endpoint = supplied_boundary_a || supplied_boundary_b
  list(
    endpoint_a = endpoint_a,
    endpoint_b = endpoint_b,
    topology_relation = topology_relation,
    point_relation = if (boundary_endpoint) {
      "boundary_endpoint"
    } else {
      topology_relation
    },
    boundary_endpoint = boundary_endpoint,
    topology_layer_conflict = isTRUE(layer_relationship) &&
      topology_relation %in% c("shared_endpoint", "endpoint_to_interior")
  )
}

#' Test whether a projected road endpoint lies on a supplied boundary
#'
#' @param endpoints Prepared endpoint table.
#' @param fragment_id Fragment identifier.
#' @param projection Point projection on the fragment.
#'
#' @return A logical scalar.
#' @keywords internal
is_render_road_projection_on_supplied_boundary = function(
  endpoints,
  fragment_id,
  projection
) {
  endpoint_rows = endpoints$render_road_fragment_id == fragment_id
  side_matches = (projection$at_start & endpoints$endpoint_side == "start") |
    (projection$at_end & endpoints$endpoint_side == "end")
  any(endpoints$supplied_boundary & endpoint_rows & side_matches)
}

#' Resolve a projected road endpoint to its stable endpoint identity
#'
#' @param endpoints Prepared endpoint table.
#' @param fragment_id Fragment identifier.
#' @param projection Point projection on the fragment.
#'
#' @return Endpoint identifier and side, or missing values for an interior point.
#' @keywords internal
resolve_render_road_projection_endpoint = function(
  endpoints,
  fragment_id,
  projection
) {
  endpoint_side = if (isTRUE(projection$at_start)) {
    "start"
  } else if (isTRUE(projection$at_end)) {
    "end"
  } else {
    NA_character_
  }
  if (is.na(endpoint_side)) {
    return(list(endpoint_id = NA_integer_, endpoint_side = endpoint_side))
  }
  endpoint_row = which(
    endpoints$render_road_fragment_id == fragment_id &
      endpoints$endpoint_side == endpoint_side
  )
  if (length(endpoint_row) != 1L) {
    stop(
      "A projected fragment endpoint did not resolve to one prepared endpoint.",
      call. = FALSE
    )
  }
  list(
    endpoint_id = endpoints$render_road_endpoint_id[[endpoint_row]],
    endpoint_side = endpoint_side
  )
}

#' Build one exact road point-pair record
#'
#' @param prepared Prepared road layer features.
#' @param row_a Fragment row A.
#' @param row_b Fragment row B.
#' @param point One point geometry.
#' @param endpoint_tolerance Endpoint tolerance in metres.
#'
#' @return One point-pair `sf` row or `NULL`.
#' @keywords internal
build_render_road_point_pair = function(
  prepared,
  row_a,
  row_b,
  point,
  endpoint_tolerance
) {
  fragments = prepared$fragments
  point_xy = sf::st_coordinates(point)[1L, 1:2]
  projection_a = project_render_road_topology_point(
    sf::st_geometry(fragments)[[row_a]],
    point_xy,
    endpoint_tolerance
  )
  projection_b = project_render_road_topology_point(
    sf::st_geometry(fragments)[[row_b]],
    point_xy,
    endpoint_tolerance
  )
  if (is.null(projection_a) || is.null(projection_b)) {
    return(NULL)
  }
  fragment_a = fragments$render_road_fragment_id[[row_a]]
  fragment_b = fragments$render_road_fragment_id[[row_b]]
  supplied_boundary_a = is_render_road_projection_on_supplied_boundary(
    prepared$endpoints,
    fragment_a,
    projection_a
  )
  supplied_boundary_b = is_render_road_projection_on_supplied_boundary(
    prepared$endpoints,
    fragment_b,
    projection_b
  )
  layer_relationship = fragments$render_road_layer[[row_a]] !=
    fragments$render_road_layer[[row_b]]
  relation = classify_render_road_point_pair(
    projection_a,
    projection_b,
    layer_relationship,
    supplied_boundary_a,
    supplied_boundary_b
  )
  endpoint_identity_a = resolve_render_road_projection_endpoint(
    prepared$endpoints,
    fragment_a,
    projection_a
  )
  endpoint_identity_b = resolve_render_road_projection_endpoint(
    prepared$endpoints,
    fragment_b,
    projection_b
  )
  sf::st_sf(
    fragment_a = fragment_a,
    fragment_b = fragment_b,
    distance_a = projection_a$distance,
    distance_b = projection_b$distance,
    endpoint_a = relation$endpoint_a,
    endpoint_b = relation$endpoint_b,
    endpoint_id_a = endpoint_identity_a$endpoint_id,
    endpoint_id_b = endpoint_identity_b$endpoint_id,
    endpoint_side_a = endpoint_identity_a$endpoint_side,
    endpoint_side_b = endpoint_identity_b$endpoint_side,
    supplied_boundary_a = supplied_boundary_a,
    supplied_boundary_b = supplied_boundary_b,
    layer_relationship = layer_relationship,
    topology_relation = relation$topology_relation,
    point_relation = relation$point_relation,
    boundary_endpoint = relation$boundary_endpoint,
    topology_layer_conflict = relation$topology_layer_conflict,
    geometry = point
  )
}

#' Build one normalized road overlap-pair record
#'
#' @param fragments Prepared metric fragments.
#' @param row_a Fragment row A.
#' @param row_b Fragment row B.
#' @param line One overlap LINESTRING geometry.
#' @param endpoint_tolerance Endpoint tolerance in metres.
#'
#' @return One overlap `sf` row or `NULL`.
#' @keywords internal
build_render_road_overlap_pair = function(
  fragments,
  row_a,
  row_b,
  line,
  endpoint_tolerance
) {
  line_coordinates = sf::st_coordinates(line)[, 1:2, drop = FALSE]
  endpoint_coordinates = line_coordinates[
    c(1L, nrow(line_coordinates)),
    ,
    drop = FALSE
  ]
  project_endpoints = function(row) {
    lapply(seq_len(2L), function(endpoint) {
      project_render_road_topology_point(
        sf::st_geometry(fragments)[[row]],
        endpoint_coordinates[endpoint, ],
        endpoint_tolerance
      )
    })
  }
  projections_a = project_endpoints(row_a)
  projections_b = project_endpoints(row_b)
  if (any(vapply(c(projections_a, projections_b), is.null, logical(1)))) {
    return(NULL)
  }
  distance_a = vapply(projections_a, `[[`, numeric(1), "distance")
  distance_b = vapply(projections_b, `[[`, numeric(1), "distance")
  sf::st_sf(
    fragment_a = fragments$render_road_fragment_id[[row_a]],
    fragment_b = fragments$render_road_fragment_id[[row_b]],
    layer_relationship = fragments$render_road_layer[[row_a]] !=
      fragments$render_road_layer[[row_b]],
    distance_a_min = min(distance_a),
    distance_a_max = max(distance_a),
    direction_a = as.integer(sign(distance_a[[2L]] - distance_a[[1L]])),
    distance_b_min = min(distance_b),
    distance_b_max = max(distance_b),
    direction_b = as.integer(sign(distance_b[[2L]] - distance_b[[1L]])),
    geometry = line
  )
}

#' Intersect one indexed road-fragment pair
#'
#' @param prepared Prepared road layer features.
#' @param row_a Fragment row A.
#' @param row_b Fragment row B.
#' @param candidate_pair_id Indexed candidate-pair identifier.
#' @param endpoint_tolerance Endpoint tolerance in metres.
#'
#' @return Point rows, overlap rows, and an optional retained failure.
#' @keywords internal
intersect_render_road_fragment_pair = function(
  prepared,
  row_a,
  row_b,
  candidate_pair_id,
  endpoint_tolerance
) {
  fragments = prepared$fragments
  fragment_a = fragments$render_road_fragment_id[[row_a]]
  fragment_b = fragments$render_road_fragment_id[[row_b]]
  intersection_result = tryCatch(
    list(
      value = suppressWarnings(sf::st_intersection(
        sf::st_geometry(fragments)[row_a],
        sf::st_geometry(fragments)[row_b]
      )),
      error_message = NULL
    ),
    error = function(error) {
      list(value = NULL, error_message = conditionMessage(error))
    }
  )
  parts = extract_render_road_topology_intersection_parts(
    intersection_result$value
  )
  point_rows = lapply(parts$points, function(point) {
    build_render_road_point_pair(
      prepared,
      row_a,
      row_b,
      sf::st_sfc(point, crs = sf::st_crs(fragments)),
      endpoint_tolerance
    )
  })
  overlap_rows = lapply(parts$lines, function(line) {
    build_render_road_overlap_pair(
      fragments,
      row_a,
      row_b,
      sf::st_sfc(line, crs = sf::st_crs(fragments)),
      endpoint_tolerance
    )
  })
  failure = if (is.null(intersection_result$error_message)) {
    NULL
  } else {
    data.frame(
      candidate_pair_id = candidate_pair_id,
      fragment_a = fragment_a,
      fragment_b = fragment_b,
      error_message = intersection_result$error_message,
      stringsAsFactors = FALSE
    )
  }
  list(
    points = Filter(Negate(is.null), point_rows),
    overlaps = Filter(Negate(is.null), overlap_rows),
    failure = failure
  )
}

#' Derive relation-specific views from unified physical point events
#'
#' @param point_events Unified grouped point events.
#' @param pair_rows Pair rows selected for the derived relationship.
#' @param relationship_name Output relationship prefix.
#'
#' @return Events, unified participants, and selected pair rows.
#' @keywords internal
derive_render_road_point_event_view = function(
  point_events,
  pair_rows,
  relationship_name
) {
  source_id = "point_event_id"
  target_id = paste0(relationship_name, "_id")
  event_id = unique(pair_rows[[source_id]])
  events = point_events$events[
    point_events$events[[source_id]] %in% event_id,
    ,
    drop = FALSE
  ]
  participants = point_events$participants[
    point_events$participants[[source_id]] %in% event_id,
    ,
    drop = FALSE
  ]
  names(events)[names(events) == source_id] = target_id
  names(participants)[names(participants) == source_id] = target_id
  names(pair_rows)[names(pair_rows) == source_id] = target_id
  list(events = events, participants = participants, pairs = pair_rows)
}

#' Find exact local road layer events
#'
#' @param prepared Prepared road layer features.
#' @param endpoint_tolerance Default `1e-2`. Endpoint and event tolerance in
#' metres.
#'
#' @return Unified point events, derived pair relationships, overlaps, and
#' retained failures.
#' @keywords internal
find_render_road_layer_events = function(
  prepared,
  endpoint_tolerance = 1e-2
) {
  fragments = prepared$fragments
  empty_points = sf::st_sf(
    point_pair_id = integer(0),
    fragment_a = integer(0),
    fragment_b = integer(0),
    distance_a = numeric(0),
    distance_b = numeric(0),
    endpoint_a = logical(0),
    endpoint_b = logical(0),
    endpoint_id_a = integer(0),
    endpoint_id_b = integer(0),
    endpoint_side_a = character(0),
    endpoint_side_b = character(0),
    supplied_boundary_a = logical(0),
    supplied_boundary_b = logical(0),
    layer_relationship = logical(0),
    topology_relation = character(0),
    point_relation = character(0),
    boundary_endpoint = logical(0),
    topology_layer_conflict = logical(0),
    geometry = sf::st_sfc(crs = sf::st_crs(fragments))
  )
  empty_overlaps = sf::st_sf(
    overlap_id = integer(0),
    fragment_a = integer(0),
    fragment_b = integer(0),
    layer_relationship = logical(0),
    distance_a_min = numeric(0),
    distance_a_max = numeric(0),
    direction_a = integer(0),
    distance_b_min = numeric(0),
    distance_b_max = numeric(0),
    direction_b = integer(0),
    geometry = sf::st_sfc(crs = sf::st_crs(fragments))
  )
  empty_failures = data.frame(
    candidate_pair_id = integer(0),
    fragment_a = integer(0),
    fragment_b = integer(0),
    error_message = character(0),
    stringsAsFactors = FALSE
  )

  candidate_index = sf::st_intersects(fragments, sparse = TRUE)
  candidate_pairs = lapply(seq_along(candidate_index), function(first) {
    second = candidate_index[[first]]
    second = second[second > first]
    if (length(second)) cbind(first = first, second = second) else NULL
  })
  candidate_pairs = Filter(Negate(is.null), candidate_pairs)
  candidate_pairs = if (length(candidate_pairs)) {
    do.call(rbind, candidate_pairs)
  } else {
    matrix(integer(0), ncol = 2L)
  }
  pair_results = lapply(seq_len(nrow(candidate_pairs)), function(pair_id) {
    intersect_render_road_fragment_pair(
      prepared,
      candidate_pairs[pair_id, 1L],
      candidate_pairs[pair_id, 2L],
      pair_id,
      endpoint_tolerance
    )
  })
  point_rows = unlist(lapply(pair_results, `[[`, "points"), recursive = FALSE)
  overlap_rows = unlist(
    lapply(pair_results, `[[`, "overlaps"),
    recursive = FALSE
  )
  failure_rows = Filter(
    Negate(is.null),
    lapply(pair_results, `[[`, "failure")
  )
  points = if (length(point_rows)) do.call(rbind, point_rows) else empty_points
  points$point_pair_id = seq_len(nrow(points))
  points = points[, c(
    "point_pair_id",
    setdiff(names(points), c("point_pair_id", attr(points, "sf_column"))),
    attr(points, "sf_column")
  )]
  overlaps = if (length(overlap_rows)) {
    do.call(rbind, overlap_rows)
  } else {
    empty_overlaps[, setdiff(names(empty_overlaps), "overlap_id"), drop = FALSE]
  }
  overlaps$overlap_id = seq_len(nrow(overlaps))
  if (nrow(overlaps)) {
    overlaps = overlaps[, c(
      "overlap_id",
      setdiff(names(overlaps), c("overlap_id", attr(overlaps, "sf_column"))),
      attr(overlaps, "sf_column")
    )]
  }
  failures = if (length(failure_rows)) {
    do.call(rbind, failure_rows)
  } else {
    empty_failures
  }

  # All point pairs are grouped together before any relationship-specific view is
  # derived, so a mixed endpoint/crossing location has one physical event ID.
  point_events = group_render_road_point_events(
    points,
    fragments,
    "point_event",
    endpoint_tolerance
  )
  pair_table = point_events$pairs
  event_id = point_events$events$point_event_id
  summarize_pair_flag = function(column) {
    vapply(
      event_id,
      function(id) {
        any(pair_table[[column]][pair_table$point_event_id == id])
      },
      logical(1)
    )
  }
  point_events$events$has_crossing_order = summarize_pair_flag(
    "layer_relationship"
  ) &
    vapply(
      event_id,
      function(id) {
        any(
          pair_table$topology_relation[pair_table$point_event_id == id] ==
            "interior_crossing"
        )
      },
      logical(1)
    )
  point_events$events$has_junction_equality = vapply(
    event_id,
    function(id) {
      rows = pair_table$point_event_id == id
      any(
        !pair_table$layer_relationship[rows] &
          pair_table$topology_relation[rows] %in%
            c(
              "shared_endpoint",
              "endpoint_to_interior",
              "equal_layer_interior"
            )
      )
    },
    logical(1)
  )
  point_events$events$topology_layer_conflict = summarize_pair_flag(
    "topology_layer_conflict"
  )
  point_events$events$boundary_endpoint = summarize_pair_flag(
    "boundary_endpoint"
  )

  crossing_pairs = pair_table[
    pair_table$topology_relation == "interior_crossing" &
      pair_table$layer_relationship,
    ,
    drop = FALSE
  ]
  junction_pairs = pair_table[
    !pair_table$layer_relationship &
      pair_table$topology_relation %in%
        c(
          "shared_endpoint",
          "endpoint_to_interior",
          "equal_layer_interior"
        ),
    ,
    drop = FALSE
  ]
  equal_layer_pairs = pair_table[
    pair_table$topology_relation == "equal_layer_interior",
    ,
    drop = FALSE
  ]
  conflict_pairs = pair_table[
    pair_table$topology_layer_conflict,
    ,
    drop = FALSE
  ]
  list(
    point_events = point_events,
    crossings = derive_render_road_point_event_view(
      point_events,
      crossing_pairs,
      "crossing"
    ),
    junctions = derive_render_road_point_event_view(
      point_events,
      junction_pairs,
      "junction"
    ),
    equal_layer_intersections = derive_render_road_point_event_view(
      point_events,
      equal_layer_pairs,
      "equal_layer_intersection"
    ),
    conflicts = derive_render_road_point_event_view(
      point_events,
      conflict_pairs,
      "conflict"
    ),
    crossing_order_pairs = crossing_pairs,
    junction_equality_pairs = junction_pairs,
    conflict_pairs = conflict_pairs,
    layer_overlaps = overlaps[overlaps$layer_relationship, , drop = FALSE],
    equal_layer_overlaps = overlaps[
      !overlaps$layer_relationship,
      ,
      drop = FALSE
    ],
    intersection_failures = failures,
    candidate_pair_count = nrow(candidate_pairs)
  )
}

# Endpoint continuations and topology graph -------------------------------

#' Score one road endpoint continuation pair
#'
#' @param endpoints Prepared endpoint table.
#' @param endpoint_coordinates Endpoint XY matrix.
#' @param first First endpoint row.
#' @param second Second endpoint row.
#' @param candidate_id Candidate identifier.
#' @param endpoint_tolerance Exact endpoint tolerance in metres.
#' @param minimum_direction_score Minimum eligible direction score.
#'
#' @return One continuation-candidate `sf` row.
#' @keywords internal
score_render_road_continuation_pair = function(
  endpoints,
  endpoint_coordinates,
  first,
  second,
  candidate_id,
  endpoint_tolerance,
  minimum_direction_score
) {
  clean_metadata = function(value) {
    value = tolower(trimws(as.character(value)))
    if (is.na(value) || !nzchar(value)) NA_character_ else value
  }
  same_value = function(first_value, second_value) {
    !is.na(first_value) &&
      !is.na(second_value) &&
      identical(first_value, second_value)
  }
  gap = endpoint_coordinates[second, ] - endpoint_coordinates[first, ]
  gap_distance = sqrt(sum(gap^2))
  exact_endpoint = gap_distance <= endpoint_tolerance
  inward_a = c(endpoints$direction_x[[first]], endpoints$direction_y[[first]])
  inward_b = c(
    endpoints$direction_x[[second]],
    endpoints$direction_y[[second]]
  )
  through_score = -sum(inward_a * inward_b)
  if (exact_endpoint) {
    alignment_a = 1
    alignment_b = 1
  } else {
    gap_direction = gap / gap_distance
    alignment_a = -sum(gap_direction * inward_a)
    alignment_b = sum(gap_direction * inward_b)
  }
  direction_score = min(through_score, alignment_a, alignment_b)

  same_way = same_value(
    clean_metadata(endpoints$render_road_way_id[[first]]),
    clean_metadata(endpoints$render_road_way_id[[second]])
  ) ||
    endpoints$render_road_feature_id[[first]] ==
      endpoints$render_road_feature_id[[second]]
  same_ref = same_value(
    clean_metadata(endpoints$render_road_ref[[first]]),
    clean_metadata(endpoints$render_road_ref[[second]])
  )
  same_name = same_value(
    clean_metadata(endpoints$render_road_name[[first]]),
    clean_metadata(endpoints$render_road_name[[second]])
  )
  same_highway = same_value(
    clean_metadata(endpoints$render_road_highway[[first]]),
    clean_metadata(endpoints$render_road_highway[[second]])
  )
  lane_count_a = suppressWarnings(as.integer(
    endpoints$render_road_lanes[[first]]
  ))
  lane_count_b = suppressWarnings(as.integer(
    endpoints$render_road_lanes[[second]]
  ))
  lane_evidence = is.finite(lane_count_a) && is.finite(lane_count_b)
  same_lanes = lane_evidence && lane_count_a == lane_count_b
  lane_difference = if (lane_evidence) {
    abs(lane_count_a - lane_count_b)
  } else {
    NA_integer_
  }
  lane_continuity_rank = as.integer(same_lanes)
  lane_subclass = if (same_lanes) {
    "same_lanes"
  } else if (lane_evidence) {
    "different_lanes"
  } else {
    "unknown_lanes"
  }
  metadata_match = c(
    if (same_way) "way",
    if (same_ref) "ref",
    if (same_name) "name",
    if (same_highway) "highway"
  )
  if (!length(metadata_match)) {
    metadata_match = "direction"
  }
  metadata_rank = if (same_way) {
    5L
  } else if (same_ref) {
    4L
  } else if (same_name) {
    3L
  } else if (same_highway) {
    2L
  } else {
    1L
  }
  metadata_tier = c(
    "direction",
    "same_highway",
    "same_name",
    "same_ref",
    "same_way"
  )[[metadata_rank]]
  evidence_rank = as.integer(exact_endpoint) * 10L + metadata_rank
  evidence_tier = paste0(
    if (exact_endpoint) "exact_" else "gap_",
    metadata_tier
  )
  evidence_subclass = paste(evidence_tier, lane_subclass, sep = "_")
  eligible = is.finite(direction_score) &&
    direction_score >= minimum_direction_score
  selection_reason = if (!eligible) {
    "direction_mismatch"
  } else if (exact_endpoint) {
    "exact_shared_endpoint"
  } else if (same_way) {
    "same_way"
  } else if (same_ref) {
    "matching_ref"
  } else if (same_name) {
    "matching_name"
  } else if (same_highway) {
    "compatible_highway"
  } else {
    "unique_direction"
  }
  sf::st_sf(
    continuation_id = candidate_id,
    endpoint_a = endpoints$render_road_endpoint_id[[first]],
    endpoint_b = endpoints$render_road_endpoint_id[[second]],
    fragment_a = endpoints$render_road_fragment_id[[first]],
    fragment_b = endpoints$render_road_fragment_id[[second]],
    side_a = endpoints$endpoint_side[[first]],
    side_b = endpoints$endpoint_side[[second]],
    endpoint_distance = gap_distance,
    direction_score = direction_score,
    same_way = same_way,
    same_ref = same_ref,
    same_name = same_name,
    same_highway = same_highway,
    lane_count_a = lane_count_a,
    lane_count_b = lane_count_b,
    lane_evidence = lane_evidence,
    same_lanes = same_lanes,
    lane_difference = lane_difference,
    lane_continuity_rank = lane_continuity_rank,
    exact_endpoint = exact_endpoint,
    evidence_rank = evidence_rank,
    evidence_tier = evidence_tier,
    evidence_subclass = evidence_subclass,
    metadata_match = paste(metadata_match, collapse = "+"),
    selection_score = evidence_rank *
      1000 +
      lane_continuity_rank * 100 +
      direction_score,
    selection_reason = selection_reason,
    eligible = eligible,
    status = if (eligible) "candidate" else "rejected",
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(
        endpoint_coordinates[first, ],
        endpoint_coordinates[second, ]
      )),
      crs = sf::st_crs(endpoints)
    )
  )
}

#' Select best continuation candidates independently at each endpoint
#'
#' @param candidates Continuation candidate table.
#' @param endpoints Prepared endpoint table.
#' @param ambiguity_direction_margin Direction-score ambiguity margin.
#'
#' @return Best rows and endpoint/candidate ambiguity flags.
#' @keywords internal
select_render_road_endpoint_candidate = function(
  candidates,
  endpoints,
  ambiguity_direction_margin
) {
  eligible_rows = which(candidates$eligible)
  best_candidate = integer(nrow(endpoints))
  ambiguous_endpoint = logical(nrow(endpoints))
  ambiguous_candidate = logical(nrow(candidates))
  for (endpoint_row in seq_len(nrow(endpoints))) {
    endpoint_id = endpoints$render_road_endpoint_id[[endpoint_row]]
    rows = eligible_rows[
      candidates$endpoint_a[eligible_rows] == endpoint_id |
        candidates$endpoint_b[eligible_rows] == endpoint_id
    ]
    if (!length(rows)) {
      next
    }
    highest_evidence = max(candidates$evidence_rank[rows])
    tier_rows = rows[candidates$evidence_rank[rows] == highest_evidence]
    highest_lane_continuity = max(
      candidates$lane_continuity_rank[tier_rows]
    )
    tier_rows = tier_rows[
      candidates$lane_continuity_rank[tier_rows] == highest_lane_continuity
    ]
    best_direction = max(candidates$direction_score[tier_rows])
    competing = tier_rows[
      candidates$direction_score[tier_rows] >=
        best_direction - ambiguity_direction_margin
    ]
    if (length(competing) == 1L) {
      best_candidate[[endpoint_row]] = competing
    } else {
      ambiguous_endpoint[[endpoint_row]] = TRUE
      ambiguous_candidate[competing] = TRUE
    }
  }
  list(
    eligible_rows = eligible_rows,
    best_candidate = best_candidate,
    ambiguous_endpoint = ambiguous_endpoint,
    ambiguous_candidate = ambiguous_candidate
  )
}

#' Select conservative road endpoint continuations
#'
#' @param prepared Prepared road layer features.
#' @param endpoint_tolerance Default `1e-2`. Exact endpoint tolerance in metres.
#' @param continuation_tolerance Default `0.25`. Maximum true geometry gap in
#' metres.
#' @param minimum_direction_score Default `cos(pi / 6)`. Minimum directional
#' agreement for a continuation candidate.
#' @param ambiguity_direction_margin Default `0.04`. Maximum directional-score
#' difference between candidates in the same best evidence tier before the
#' endpoint is considered ambiguous.
#'
#' @return Selected, ambiguous, and rejected continuation diagnostics.
#' @keywords internal
select_render_road_continuations = function(
  prepared,
  endpoint_tolerance = 1e-2,
  continuation_tolerance = 0.25,
  minimum_direction_score = cos(pi / 6),
  ambiguity_direction_margin = 0.04
) {
  # Start from the prepared endpoint table and define a schema-stable diagnostic table.
  endpoints = prepared$endpoints
  empty = sf::st_sf(
    continuation_id = integer(0),
    endpoint_a = integer(0),
    endpoint_b = integer(0),
    fragment_a = integer(0),
    fragment_b = integer(0),
    side_a = character(0),
    side_b = character(0),
    endpoint_distance = numeric(0),
    direction_score = numeric(0),
    same_way = logical(0),
    same_ref = logical(0),
    same_name = logical(0),
    same_highway = logical(0),
    lane_count_a = integer(0),
    lane_count_b = integer(0),
    lane_evidence = logical(0),
    same_lanes = logical(0),
    lane_difference = integer(0),
    lane_continuity_rank = integer(0),
    exact_endpoint = logical(0),
    evidence_rank = integer(0),
    evidence_tier = character(0),
    evidence_subclass = character(0),
    metadata_match = character(0),
    selection_score = numeric(0),
    selection_reason = character(0),
    status = character(0),
    geometry = sf::st_sfc(crs = sf::st_crs(endpoints))
  )
  empty_diagnostics = list(
    candidate_count = 0L,
    eligible_candidate_count = 0L,
    selected_candidate_count = 0L,
    ambiguous_candidate_count = 0L,
    rejected_candidate_count = 0L,
    ambiguous_endpoint_count = 0L,
    selection_reason_count = integer(0)
  )
  if (nrow(endpoints) < 2L) {
    return(list(
      selected = empty,
      ambiguous_endpoint_id = integer(0),
      diagnostics = empty_diagnostics,
      ambiguity_direction_margin = ambiguity_direction_margin
    ))
  }
  # Normalize endpoint and gap tolerances. The continuation tolerance is intentionally
  # small so it repairs coordinate/clipping gaps rather than jumping across intersections.
  continuation_tolerance = suppressWarnings(as.numeric(
    continuation_tolerance[[1L]]
  ))
  if (!is.finite(continuation_tolerance) || continuation_tolerance <= 0) {
    continuation_tolerance = 0.25
  }
  endpoint_tolerance = suppressWarnings(as.numeric(endpoint_tolerance[[1L]]))
  if (!is.finite(endpoint_tolerance) || endpoint_tolerance <= 0) {
    endpoint_tolerance = 1e-2
  }
  minimum_direction_score = suppressWarnings(as.numeric(
    minimum_direction_score[[1L]]
  ))
  if (
    !is.finite(minimum_direction_score) ||
      minimum_direction_score < -1 ||
      minimum_direction_score > 1
  ) {
    minimum_direction_score = cos(pi / 6)
  }
  ambiguity_direction_margin = suppressWarnings(as.numeric(
    ambiguity_direction_margin[[1L]]
  ))
  if (
    !is.finite(ambiguity_direction_margin) || ambiguity_direction_margin < 0
  ) {
    ambiguity_direction_margin = 0.04
  }

  # Use sf's distance index to enumerate nearby endpoint pairs from different fragments.
  nearby = sf::st_is_within_distance(
    endpoints,
    endpoints,
    dist = continuation_tolerance
  )
  pair_index = lapply(seq_along(nearby), function(first) {
    second = nearby[[first]]
    second = second[
      second > first &
        endpoints$render_road_fragment_id[second] !=
          endpoints$render_road_fragment_id[[first]]
    ]
    if (!length(second)) {
      return(NULL)
    }
    cbind(first = first, second = second)
  })
  pair_index = Filter(Negate(is.null), pair_index)
  if (!length(pair_index)) {
    return(list(
      selected = empty,
      ambiguous_endpoint_id = integer(0),
      diagnostics = empty_diagnostics,
      ambiguity_direction_margin = ambiguity_direction_margin
    ))
  }
  pair_index = do.call(rbind, pair_index)

  endpoint_coordinates = sf::st_coordinates(endpoints)[, 1:2, drop = FALSE]
  candidate_rows = lapply(seq_len(nrow(pair_index)), function(candidate_id) {
    score_render_road_continuation_pair(
      endpoints = endpoints,
      endpoint_coordinates = endpoint_coordinates,
      first = pair_index[candidate_id, 1L],
      second = pair_index[candidate_id, 2L],
      candidate_id = candidate_id,
      endpoint_tolerance = endpoint_tolerance,
      minimum_direction_score = minimum_direction_score
    )
  })
  candidates = do.call(rbind, candidate_rows)
  endpoint_selection = select_render_road_endpoint_candidate(
    candidates,
    endpoints,
    ambiguity_direction_margin
  )
  eligible_rows = endpoint_selection$eligible_rows
  best_candidate = endpoint_selection$best_candidate
  ambiguous_endpoint = endpoint_selection$ambiguous_endpoint
  ambiguous_candidate = endpoint_selection$ambiguous_candidate

  # Select an edge only when each endpoint chooses the other as its unique best match.
  # Mutual-best matching limits an endpoint to one through continuation while leaving
  # side branches connected by junction height but not by grade continuity.
  endpoint_row = stats::setNames(
    seq_len(nrow(endpoints)),
    endpoints$render_road_endpoint_id
  )
  for (row in eligible_rows) {
    first = endpoint_row[[as.character(candidates$endpoint_a[[row]])]]
    second = endpoint_row[[as.character(candidates$endpoint_b[[row]])]]
    if (
      best_candidate[[first]] == row &&
        best_candidate[[second]] == row
    ) {
      candidates$status[[row]] = "selected"
    } else if (
      ambiguous_candidate[[row]] &&
        (ambiguous_endpoint[[first]] || ambiguous_endpoint[[second]])
    ) {
      candidates$status[[row]] = "ambiguous"
      candidates$selection_reason[[row]] = "ambiguous_best_match"
    } else {
      candidates$status[[row]] = "rejected"
      candidates$selection_reason[[row]] = "non_mutual_best_match"
    }
  }
  # Retain only selected rows. Candidate and rejected geometries are summarized as
  # compact counts so topology objects do not carry a second diagnostic geometry set.
  candidates$eligible = NULL
  candidates$continuation_id = seq_len(nrow(candidates))
  selection_reason_count = table(candidates$selection_reason)
  list(
    selected = candidates[candidates$status == "selected", , drop = FALSE],
    ambiguous_endpoint_id = sort(unique(
      endpoints$render_road_endpoint_id[ambiguous_endpoint]
    )),
    diagnostics = list(
      candidate_count = nrow(candidates),
      eligible_candidate_count = length(eligible_rows),
      selected_candidate_count = sum(candidates$status == "selected"),
      ambiguous_candidate_count = sum(candidates$status == "ambiguous"),
      rejected_candidate_count = sum(candidates$status == "rejected"),
      ambiguous_endpoint_count = sum(ambiguous_endpoint),
      selection_reason_count = as.integer(selection_reason_count) |>
        stats::setNames(names(selection_reason_count))
    ),
    ambiguity_direction_margin = ambiguity_direction_margin
  )
}


#' Identify endpoint-specific candidate ground anchors
#'
#' @param prepared Prepared road layer features.
#'
#' @return Candidate endpoint and fragment identifiers plus surface policy.
#' @keywords internal
identify_render_road_candidate_anchor_endpoints = function(prepared) {
  fragments = prepared$fragments
  clean_tag = function(value) {
    value = tolower(trimws(as.character(value)))
    value[is.na(value) | !nzchar(value)] = NA_character_
    value
  }
  truthy_structure = function(value) {
    value = clean_tag(value)
    !is.na(value) & !(value %in% c("no", "false", "0"))
  }
  location = clean_tag(fragments$render_road_location)
  non_surface_location = !is.na(location) &
    location %in%
      c(
        "underground",
        "underwater",
        "subway",
        "elevated",
        "overground"
      )
  surface_fragment = fragments$render_road_layer == 0 &
    !truthy_structure(fragments$render_road_bridge) &
    !truthy_structure(fragments$render_road_tunnel) &
    !non_surface_location
  surface_fragment_id = fragments$render_road_fragment_id[surface_fragment]
  endpoint_candidate = prepared$endpoints$supplied_boundary |
    prepared$endpoints$render_road_fragment_id %in% surface_fragment_id
  list(
    candidate_anchor_endpoint_id = prepared$endpoints$render_road_endpoint_id[
      endpoint_candidate
    ],
    candidate_anchor_fragment_id = sort(unique(
      prepared$endpoints$render_road_fragment_id[endpoint_candidate]
    )),
    surface_fragment_id = sort(unique(surface_fragment_id))
  )
}

#' Build graph edges from a fragment-pair table
#'
#' @param pairs Fragment-pair table.
#' @param topology_type Edge relationship type.
#' @param relation_column Pair identifier column.
#'
#' @return A graph edge table, or `NULL` for an empty pair table.
#' @keywords internal
build_render_road_pair_edges = function(
  pairs,
  topology_type,
  relation_column
) {
  if (!nrow(pairs)) {
    return(NULL)
  }
  relation_id = if (relation_column %in% names(pairs)) {
    pairs[[relation_column]]
  } else {
    seq_len(nrow(pairs))
  }
  data.frame(
    from = as.character(pairs$fragment_a),
    to = as.character(pairs$fragment_b),
    topology_type = topology_type,
    relation_id = as.integer(relation_id),
    stringsAsFactors = FALSE
  )
}

#' Build a prospective sparse road-profile solve graph
#'
#' @param prepared Prepared road layer features.
#' @param events Exact road layer events.
#' @param continuations Continuation selection diagnostics.
#'
#' @return Layer-event seeds, selected approach continuations, graph edges, and
#' prospective component membership.
#' @keywords internal
build_render_road_prospective_solve_graph = function(
  prepared,
  events,
  continuations
) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The `igraph` package is required for road layer topology.")
  }
  fragments = prepared$fragments
  fragment_id = fragments$render_road_fragment_id
  all_vertices = data.frame(
    name = as.character(fragment_id),
    render_road_fragment_id = fragment_id,
    render_road_feature_id = fragments$render_road_feature_id,
    render_road_layer = fragments$render_road_layer,
    stringsAsFactors = FALSE
  )

  # Phase 3 admits every fragment participating in a layer event. Explicit
  # underground fragments receive a bounded terrain-relative reference in the
  # profile model, while negative layer values without tunnel metadata remain
  # ordinary terrain-constrained roads.
  underground = identify_render_road_underground_fragments(fragments)
  eligible_fragment_id = fragment_id
  crossing_pairs = events$crossing_order_pairs[
    events$crossing_order_pairs$fragment_a %in%
      eligible_fragment_id &
      events$crossing_order_pairs$fragment_b %in% eligible_fragment_id,
    ,
    drop = FALSE
  ]
  layer_overlaps = events$layer_overlaps[
    events$layer_overlaps$fragment_a %in%
      eligible_fragment_id &
      events$layer_overlaps$fragment_b %in% eligible_fragment_id,
    ,
    drop = FALSE
  ]
  equality_pairs = events$junctions$pairs[
    events$junctions$pairs$fragment_a %in%
      eligible_fragment_id &
      events$junctions$pairs$fragment_b %in% eligible_fragment_id,
    ,
    drop = FALSE
  ]
  crossing_edges = build_render_road_pair_edges(
    crossing_pairs,
    "layer_crossing",
    "point_pair_id"
  )
  overlap_edges = build_render_road_pair_edges(
    layer_overlaps,
    "layer_overlap",
    "overlap_id"
  )
  structure_profile_seed_id = fragment_id[
    fragments$render_road_layer > 0 |
      underground
  ]
  seed_fragment_id = unique(c(
    crossing_pairs$fragment_a,
    crossing_pairs$fragment_b,
    layer_overlaps$fragment_a,
    layer_overlaps$fragment_b,
    structure_profile_seed_id
  ))

  # Follow only selected endpoint continuations from a seed. Ordinary junction and
  # at-grade network edges never enlarge a solve component.
  selected_continuations = continuations$selected[
    continuations$selected$fragment_a %in%
      eligible_fragment_id &
      continuations$selected$fragment_b %in% eligible_fragment_id,
    ,
    drop = FALSE
  ]
  continuation_edges = if (nrow(selected_continuations)) {
    data.frame(
      continuation_id = selected_continuations$continuation_id,
      from = as.character(selected_continuations$fragment_a),
      to = as.character(selected_continuations$fragment_b),
      from_endpoint = selected_continuations$endpoint_a,
      to_endpoint = selected_continuations$endpoint_b,
      topology_type = "approach_continuation",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      continuation_id = integer(0),
      from = character(0),
      to = character(0),
      from_endpoint = integer(0),
      to_endpoint = integer(0),
      topology_type = character(0)
    )
  }
  # Surface endpoints are retained as diagnostics only. Selected continuation
  # edges may cross them in either direction: surface contact is chosen by the
  # profile objective under the terrain floor and physical grade constraints,
  # never by terminating the solve at a candidate endpoint.
  candidate_anchors = identify_render_road_candidate_anchor_endpoints(prepared)
  candidate_anchor_endpoint_id = candidate_anchors$candidate_anchor_endpoint_id
  reverse_continuation_edges = continuation_edges
  if (nrow(reverse_continuation_edges)) {
    reverse_continuation_edges = data.frame(
      continuation_id = reverse_continuation_edges$continuation_id,
      from = reverse_continuation_edges$to,
      to = reverse_continuation_edges$from,
      from_endpoint = reverse_continuation_edges$to_endpoint,
      to_endpoint = reverse_continuation_edges$from_endpoint,
      topology_type = reverse_continuation_edges$topology_type,
      stringsAsFactors = FALSE
    )
  }
  directed_continuation_edges = rbind(
    continuation_edges,
    reverse_continuation_edges
  )

  # Track active solve context separately from fragments allowed to propagate it.
  # Selected continuations always propagate the physical road profile. A surface
  # fragment reached only through an ordinary junction supplies terminal context
  # without opening the rest of its street network.
  active_fragment_id = sort(unique(seed_fragment_id))
  expandable_fragment_id = active_fragment_id
  terminal_ground_fragment_id = integer(0)
  permitted_continuation_id = integer(0)
  permitted_equality_pair_id = integer(0)
  repeat {
    previous_fragment_id = active_fragment_id
    previous_expandable_fragment_id = expandable_fragment_id
    previous_continuation_id = permitted_continuation_id
    if (nrow(directed_continuation_edges)) {
      traversed = directed_continuation_edges$from %in%
        as.character(active_fragment_id)
      permitted_continuation_id = sort(unique(c(
        permitted_continuation_id,
        directed_continuation_edges$continuation_id[traversed]
      )))
      target_fragment_id = as.integer(
        directed_continuation_edges$to[traversed]
      )
      # Continue across every selected physical fragment boundary, including
      # from a road admitted only as terminal junction context. Propagate its
      # terminal status along that through-road so the solve cannot branch into
      # the surrounding at-grade street grid.
      terminal_target = !(directed_continuation_edges$from[traversed] %in%
        as.character(expandable_fragment_id))
      active_fragment_id = sort(unique(c(
        active_fragment_id,
        target_fragment_id
      )))
      expandable_fragment_id = sort(unique(c(
        expandable_fragment_id,
        target_fragment_id[!terminal_target]
      )))
      terminal_ground_fragment_id = sort(unique(c(
        terminal_ground_fragment_id,
        target_fragment_id[terminal_target]
      )))
    }
    if (nrow(equality_pairs)) {
      from_a = equality_pairs$fragment_a %in%
        expandable_fragment_id
      from_b = equality_pairs$fragment_b %in%
        expandable_fragment_id
      traversed_equality = from_a | from_b
      permitted_equality_pair_id = sort(unique(c(
        permitted_equality_pair_id,
        equality_pairs$point_pair_id[traversed_equality]
      )))
      target_fragment_id = c(
        equality_pairs$fragment_b[from_a],
        equality_pairs$fragment_a[from_b]
      )
      # A newly reached ordinary surface fragment supplies only immediate equality
      # context. This is endpoint-independent so interior at-grade intersections do
      # not relay solve membership through the surface street grid.
      terminal_target = target_fragment_id %in%
        candidate_anchors$surface_fragment_id
      active_fragment_id = sort(unique(c(
        active_fragment_id,
        target_fragment_id
      )))
      expandable_fragment_id = sort(unique(c(
        expandable_fragment_id,
        target_fragment_id[!terminal_target]
      )))
      terminal_ground_fragment_id = sort(unique(c(
        terminal_ground_fragment_id,
        target_fragment_id[terminal_target]
      )))
    }
    terminal_ground_fragment_id = setdiff(
      terminal_ground_fragment_id,
      expandable_fragment_id
    )
    if (
      identical(previous_fragment_id, active_fragment_id) &&
        identical(
          previous_expandable_fragment_id,
          expandable_fragment_id
        ) &&
        identical(previous_continuation_id, permitted_continuation_id)
    ) {
      break
    }
  }
  # Retain the established diagnostic name, but report fragments not activated
  # by a seed or permitted graph traversal instead of the now-empty eligibility
  # complement.
  deferred_profile_fragment_id = setdiff(fragment_id, active_fragment_id)
  permitted_continuations = selected_continuations[
    selected_continuations$continuation_id %in% permitted_continuation_id,
    ,
    drop = FALSE
  ]
  permitted_continuation_edges = build_render_road_pair_edges(
    sf::st_drop_geometry(permitted_continuations),
    "approach_continuation",
    "continuation_id"
  )
  active_equality_pairs = equality_pairs[
    equality_pairs$point_pair_id %in%
      permitted_equality_pair_id &
      equality_pairs$fragment_a %in% active_fragment_id &
      equality_pairs$fragment_b %in% active_fragment_id,
    ,
    drop = FALSE
  ]
  equality_edges = build_render_road_pair_edges(
    active_equality_pairs,
    "junction_equality",
    "point_pair_id"
  )

  edge_rows = list(
    crossing_edges,
    overlap_edges,
    equality_edges,
    permitted_continuation_edges
  )
  edge_rows = Filter(function(value) !is.null(value) && nrow(value), edge_rows)
  edges = if (length(edge_rows)) {
    do.call(rbind, edge_rows)
  } else {
    data.frame(
      from = character(0),
      to = character(0),
      topology_type = character(0),
      relation_id = integer(0)
    )
  }
  active_vertices = all_vertices[
    all_vertices$render_road_fragment_id %in% active_fragment_id,
    ,
    drop = FALSE
  ]
  graph = igraph::graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = active_vertices
  )
  component = igraph::components(graph)$membership
  components = data.frame(
    render_road_fragment_id = as.integer(names(component)),
    prospective_solve_component_id = as.integer(component),
    stringsAsFactors = FALSE
  )
  components = components[
    match(active_fragment_id, components$render_road_fragment_id),
    ,
    drop = FALSE
  ]
  permitted_continuations$fragment_a_component = components$prospective_solve_component_id[
    match(
      permitted_continuations$fragment_a,
      components$render_road_fragment_id
    )
  ]
  permitted_continuations$fragment_b_component = components$prospective_solve_component_id[
    match(
      permitted_continuations$fragment_b,
      components$render_road_fragment_id
    )
  ]
  if (
    nrow(permitted_continuations) &&
      any(
        is.na(permitted_continuations$fragment_a_component) |
          is.na(permitted_continuations$fragment_b_component) |
          permitted_continuations$fragment_a_component !=
            permitted_continuations$fragment_b_component
      )
  ) {
    stop(
      "A prospective continuation crosses solve-component boundaries.",
      call. = FALSE
    )
  }
  list(
    graph = graph,
    components = components,
    edges = edges,
    seed_fragment_id = sort(unique(seed_fragment_id)),
    candidate_anchor_endpoint_id = sort(unique(candidate_anchor_endpoint_id)),
    candidate_anchor_fragment_id = candidate_anchors$candidate_anchor_fragment_id,
    active_fragment_id = active_fragment_id,
    expandable_fragment_id = expandable_fragment_id,
    terminal_ground_fragment_id = terminal_ground_fragment_id,
    deferred_profile_fragment_id = deferred_profile_fragment_id,
    permitted_continuations = permitted_continuations,
    active_equality_pairs = active_equality_pairs
  )
}

#' Build complete local road layer topology
#'
#' @param prepared Prepared road layer features.
#' @param endpoint_tolerance Default `1e-2`. Exact topology tolerance in metres.
#' @param continuation_tolerance Default `0.25`. Maximum true continuation gap
#' in metres.
#' @param ambiguity_direction_margin Default `0.04`. Direction-score ambiguity
#' margin within the best continuation evidence tier.
#'
#' @return Prepared features, active topology, continuations, and diagnostics.
#' @keywords internal
build_render_road_layer_topology = function(
  prepared,
  endpoint_tolerance = 1e-2,
  continuation_tolerance = 0.25,
  ambiguity_direction_margin = 0.04
) {
  # Detect exact point and overlap events from the prepared fragment geometries.
  events = find_render_road_layer_events(
    prepared = prepared,
    endpoint_tolerance = endpoint_tolerance
  )
  # Infer conservative through-road relationships from endpoint geometry and metadata.
  continuations = select_render_road_continuations(
    prepared = prepared,
    endpoint_tolerance = endpoint_tolerance,
    continuation_tolerance = continuation_tolerance,
    ambiguity_direction_margin = ambiguity_direction_margin
  )
  prospective_solve_graph = build_render_road_prospective_solve_graph(
    prepared = prepared,
    events = events,
    continuations = continuations
  )
  # Attach prospective solve membership. Inactive fragments deliberately receive no
  # prospective solve component.
  fragments = prepared$fragments
  fragments$prospective_solve_component_id = prospective_solve_graph$components$prospective_solve_component_id[
    match(
      fragments$render_road_fragment_id,
      prospective_solve_graph$components$render_road_fragment_id
    )
  ]

  # Sort participants within each physical crossing to describe the local layer stack.
  crossing_stack = events$crossings$participants
  if (nrow(crossing_stack)) {
    crossing_stack = crossing_stack[
      order(
        crossing_stack$crossing_id,
        crossing_stack$render_road_layer,
        crossing_stack$render_road_fragment_id
      ),
      ,
      drop = FALSE
    ]
    crossing_stack$local_order = stats::ave(
      crossing_stack$render_road_layer,
      crossing_stack$crossing_id,
      FUN = function(value) {
        match(value, sort(unique(value)))
      }
    )
  } else {
    crossing_stack$local_order = integer(0)
  }

  # Return only normalized roads, solve/mesh event tables, selected continuation
  # decisions, and compact reproducibility diagnostics.
  list(
    prepared = prepared,
    fragments = fragments,
    endpoints = prepared$endpoints,
    point_events = events$point_events$events,
    point_event_participants = events$point_events$participants,
    point_pairs = events$point_events$pairs,
    junctions = events$junctions$events,
    junction_participants = events$junctions$participants,
    junction_equality_pairs = events$junctions$pairs,
    crossings = events$crossings$events,
    crossing_participants = crossing_stack,
    crossing_pairs = events$crossings$pairs,
    equal_layer_intersections = events$equal_layer_intersections$events,
    equal_layer_intersection_participants = events$equal_layer_intersections$participants,
    topology_conflicts = events$conflicts$events,
    topology_conflict_participants = events$conflicts$participants,
    topology_conflict_pairs = events$conflicts$pairs,
    layer_overlaps = events$layer_overlaps,
    equal_layer_overlaps = events$equal_layer_overlaps,
    selected_continuations = continuations$selected,
    prospective_solve_continuations = prospective_solve_graph$permitted_continuations,
    ambiguous_endpoint_id = continuations$ambiguous_endpoint_id,
    prospective_solve_graph = prospective_solve_graph$graph,
    prospective_solve_graph_edges = prospective_solve_graph$edges,
    prospective_solve_components = prospective_solve_graph$components,
    prospective_solve_junction_equality_pairs = prospective_solve_graph$active_equality_pairs,
    prospective_solve_seed_fragment_id = prospective_solve_graph$seed_fragment_id,
    candidate_anchor_endpoint_id = prospective_solve_graph$candidate_anchor_endpoint_id,
    candidate_anchor_fragment_id = prospective_solve_graph$candidate_anchor_fragment_id,
    prospective_solve_fragment_id = prospective_solve_graph$active_fragment_id,
    prospective_solve_expandable_fragment_id = prospective_solve_graph$expandable_fragment_id,
    prospective_solve_terminal_ground_fragment_id = prospective_solve_graph$terminal_ground_fragment_id,
    prospective_solve_deferred_profile_fragment_id = prospective_solve_graph$deferred_profile_fragment_id,
    diagnostics = list(
      candidate_pair_count = events$candidate_pair_count,
      dropped_fragment_id = prepared$dropped_fragment_id,
      dropped_fragment_diagnostics = prepared$dropped_fragment_diagnostics,
      retained_cleanup = prepared$retained_cleanup,
      intersection_failures = events$intersection_failures,
      maximum_event_spread = max(c(
        events$point_events$events$event_spread,
        0
      )),
      endpoint_tolerance = endpoint_tolerance,
      continuation_tolerance = continuation_tolerance,
      direction_lookahead = prepared$direction_lookahead,
      ambiguity_direction_margin = continuations$ambiguity_direction_margin,
      continuation = continuations$diagnostics
    )
  )
}


# Sparse quadratic road profiles ------------------------------------------

#' Calculate metric distances along a road fragment
#'
#' @param geometry Metric LINESTRING geometry.
#'
#' @return Coordinates, cumulative distances, and total length.
#' @keywords internal
calculate_render_road_metric_line_distances = function(geometry) {
  coordinates = unclass(geometry)[, 1:2, drop = FALSE]
  if (nrow(coordinates) < 2L) {
    stop("A road profile fragment requires at least two points.", call. = FALSE)
  }
  segment_length = sqrt(rowSums(
    (coordinates[-1L, , drop = FALSE] -
      coordinates[-nrow(coordinates), , drop = FALSE])^2
  ))
  distance = c(0, cumsum(segment_length))
  list(
    coordinates = coordinates,
    distance = distance,
    length = utils::tail(distance, 1L)
  )
}

#' Interpolate values from a road reference profile
#'
#' @param profile Data frame containing `distance` and `elevation`.
#' @param distance Distances to evaluate in metres.
#'
#' @return Interpolated elevations.
#' @keywords internal
interpolate_render_road_profile_reference = function(profile, distance) {
  stats::approx(
    x = profile$distance,
    y = profile$elevation,
    xout = distance,
    rule = 2,
    ties = "ordered"
  )$y
}

#' Calculate a terrain-reference grade
#'
#' @param profile Data frame containing `distance` and `elevation`.
#' @param distance Distance to evaluate in metres.
#'
#' @return Terrain grade as rise divided by run.
#' @keywords internal
calculate_render_road_profile_reference_grade = function(profile, distance) {
  interval = findInterval(
    distance,
    profile$distance,
    all.inside = TRUE
  )
  interval = min(max(interval, 1L), nrow(profile) - 1L)
  run = profile$distance[[interval + 1L]] - profile$distance[[interval]]
  if (!is.finite(run) || run <= 0) {
    return(0)
  }
  rise = profile$elevation[[interval + 1L]] - profile$elevation[[interval]]
  rise / run
}

#' Normalize terrain references for road profile solving
#'
#' @param topology Road topology diagnostics.
#' @param terrain_profiles Default `NULL`. List of terrain reference profiles,
#' one per fragment. Each entry may be elevations at original vertices or a
#' two-column distance/elevation table.
#'
#' @return Named distance/elevation data frames covering every fragment.
#' @keywords internal
normalize_render_road_terrain_profiles = function(
  topology,
  terrain_profiles = NULL
) {
  fragments = topology$fragments
  fragment_id = fragments$render_road_fragment_id
  fragment_geometry = sf::st_geometry(fragments)
  geometry_info = lapply(
    fragment_geometry,
    calculate_render_road_metric_line_distances
  )
  if (is.null(terrain_profiles)) {
    terrain_profiles = lapply(geometry_info, function(info) {
      data.frame(
        distance = info$distance,
        elevation = rep(0, length(info$distance))
      )
    })
  }
  if (!is.list(terrain_profiles)) {
    stop("`terrain_profiles` must be a list.", call. = FALSE)
  }
  if (!is.null(names(terrain_profiles))) {
    profile_index = match(as.character(fragment_id), names(terrain_profiles))
    if (anyNA(profile_index)) {
      stop(
        "Named `terrain_profiles` must include every fragment ID.",
        call. = FALSE
      )
    }
    terrain_profiles = terrain_profiles[profile_index]
  } else if (length(terrain_profiles) != nrow(fragments)) {
    stop(
      "`terrain_profiles` must contain one entry per fragment.",
      call. = FALSE
    )
  }

  normalized = lapply(seq_along(terrain_profiles), function(fragment_row) {
    profile = terrain_profiles[[fragment_row]]
    info = geometry_info[[fragment_row]]
    if (is.numeric(profile) && is.null(dim(profile))) {
      if (length(profile) != length(info$distance)) {
        stop(
          "Numeric terrain profiles must match original fragment vertices.",
          call. = FALSE
        )
      }
      profile = data.frame(
        distance = info$distance,
        elevation = as.numeric(profile)
      )
    } else {
      profile = as.data.frame(profile)
      if (all(c("distance", "elevation") %in% names(profile))) {
        profile = profile[, c("distance", "elevation"), drop = FALSE]
      } else if (ncol(profile) >= 2L) {
        profile = profile[, 1:2, drop = FALSE]
        names(profile) = c("distance", "elevation")
      } else {
        stop(
          "Terrain profile tables require distance and elevation columns.",
          call. = FALSE
        )
      }
    }
    profile$distance = suppressWarnings(as.numeric(profile$distance))
    profile$elevation = suppressWarnings(as.numeric(profile$elevation))
    if (
      nrow(profile) < 2L ||
        any(!is.finite(profile$distance)) ||
        any(!is.finite(profile$elevation))
    ) {
      stop("Terrain profiles require at least two finite rows.", call. = FALSE)
    }
    profile = stats::aggregate(
      elevation ~ distance,
      data = profile,
      FUN = mean
    )
    profile = profile[order(profile$distance), , drop = FALSE]
    tolerance = max(1e-8, info$length * 1e-10)
    if (
      profile$distance[[1L]] > tolerance ||
        utils::tail(profile$distance, 1L) < info$length - tolerance
    ) {
      stop(
        "Each terrain profile must cover both fragment endpoints.",
        call. = FALSE
      )
    }
    profile$distance = pmin(pmax(profile$distance, 0), info$length)
    endpoint_distance = c(0, info$length)
    endpoint_elevation = interpolate_render_road_profile_reference(
      profile,
      endpoint_distance
    )
    profile = rbind(
      profile,
      data.frame(
        distance = endpoint_distance,
        elevation = endpoint_elevation
      )
    )
    profile = stats::aggregate(
      elevation ~ distance,
      data = profile,
      FUN = mean
    )
    profile[order(profile$distance), , drop = FALSE]
  })
  names(normalized) = as.character(fragment_id)
  attr(normalized, "geometry_info") = geometry_info
  normalized
}


#' Resolve an overlap distance at a geometry endpoint
#'
#' @param overlap One normalized overlap row.
#' @param suffix Fragment suffix, either `"a"` or `"b"`.
#' @param geometry_endpoint Overlap geometry endpoint, either `"start"` or
#' `"end"`.
#'
#' @return Distance along the requested source fragment.
#' @keywords internal
resolve_render_road_overlap_endpoint_distance = function(
  overlap,
  suffix,
  geometry_endpoint
) {
  minimum = overlap[[paste0("distance_", suffix, "_min")]][[1L]]
  maximum = overlap[[paste0("distance_", suffix, "_max")]][[1L]]
  direction = overlap[[paste0("direction_", suffix)]][[1L]]
  forward = !is.finite(direction) || direction >= 0
  if (identical(geometry_endpoint, "start")) {
    if (forward) minimum else maximum
  } else {
    if (forward) maximum else minimum
  }
}

#' Assert one road-profile solver setting
#'
#' @param value Setting value.
#' @param argument Argument name used in errors.
#' @param allow_zero Default `FALSE`. Whether zero is valid.
#' @param allow_infinite Default `FALSE`. Whether positive infinity is valid.
#'
#' @return An asserted numeric scalar.
#' @keywords internal
assert_render_road_profile_setting = function(
  value,
  argument,
  allow_zero = FALSE,
  allow_infinite = FALSE
) {
  if (!is.numeric(value) || length(value) != 1L) {
    stop(sprintf("`%s` must be a single number.", argument), call. = FALSE)
  }
  value = as.numeric(value[[1L]])
  invalid = is.na(value) ||
    (!allow_infinite && !is.finite(value)) ||
    if (allow_zero) value < 0 else value <= 0
  if (invalid) {
    qualifier = if (allow_zero) "non-negative" else "positive"
    finite = if (allow_infinite) "" else " and finite"
    stop(
      sprintf("`%s` must be %s%s.", argument, qualifier, finite),
      call. = FALSE
    )
  }
  value
}

#' Build compact active topology for road-profile solving
#'
#' @param topology Road topology diagnostics.
#' @param terrain_profiles Default `NULL`. Terrain profiles before subsetting.
#'
#' @return Compact active topology and aligned terrain profiles.
#' @keywords internal
build_render_road_active_profile_topology = function(
  topology,
  terrain_profiles = NULL
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for road profile solving.")
  }
  fragments = topology$fragments
  required_fragment_columns = c(
    "render_road_fragment_id",
    "render_road_feature_id",
    "render_road_layer",
    "render_road_clearance",
    "render_road_tunnel",
    "render_road_location",
    "prospective_solve_component_id"
  )
  if (
    !inherits(fragments, "sf") ||
      !nrow(fragments) ||
      any(!(required_fragment_columns %in% names(fragments)))
  ) {
    stop(
      "`topology` does not contain prepared profile fragments.",
      call. = FALSE
    )
  }
  active_rows = !is.na(fragments$prospective_solve_component_id)
  if (!any(active_rows)) {
    stop(
      "`topology` does not contain a prospective profile solve graph.",
      call. = FALSE
    )
  }
  if (
    is.list(terrain_profiles) &&
      is.null(names(terrain_profiles)) &&
      length(terrain_profiles) == nrow(fragments)
  ) {
    terrain_profiles = terrain_profiles[active_rows]
  }
  fragments = fragments[active_rows, , drop = FALSE]
  fragments$solve_component_id = as.integer(
    fragments$prospective_solve_component_id
  )
  active_fragment_id = fragments$render_road_fragment_id
  endpoints = topology$endpoints[
    topology$endpoints$render_road_fragment_id %in% active_fragment_id,
    ,
    drop = FALSE
  ]
  point_event_participants = topology$point_event_participants[
    topology$point_event_participants$render_road_fragment_id %in%
      active_fragment_id,
    ,
    drop = FALSE
  ]
  crossing_participants = topology$crossing_participants[
    topology$crossing_participants$render_road_fragment_id %in%
      active_fragment_id,
    ,
    drop = FALSE
  ]
  point_pairs = topology$point_pairs[
    topology$point_pairs$fragment_a %in%
      active_fragment_id &
      topology$point_pairs$fragment_b %in% active_fragment_id,
    ,
    drop = FALSE
  ]
  crossing_pairs = topology$crossing_pairs[
    topology$crossing_pairs$fragment_a %in%
      active_fragment_id &
      topology$crossing_pairs$fragment_b %in% active_fragment_id,
    ,
    drop = FALSE
  ]
  junction_source = topology$prospective_solve_junction_equality_pairs
  if (is.null(junction_source)) {
    junction_source = topology$junction_equality_pairs
  }
  junction_equality_pairs =
    junction_source[
      junction_source$fragment_a %in%
        active_fragment_id &
        junction_source$fragment_b %in%
          active_fragment_id,
      ,
      drop = FALSE
    ]
  topology_conflict_pairs = topology$topology_conflict_pairs[
    topology$topology_conflict_pairs$fragment_a %in%
      active_fragment_id &
      topology$topology_conflict_pairs$fragment_b %in% active_fragment_id,
    ,
    drop = FALSE
  ]
  layer_overlaps = topology$layer_overlaps[
    topology$layer_overlaps$fragment_a %in%
      active_fragment_id &
      topology$layer_overlaps$fragment_b %in% active_fragment_id,
    ,
    drop = FALSE
  ]
  prospective_solve_continuations =
    topology$prospective_solve_continuations[
      topology$prospective_solve_continuations$fragment_a %in%
        active_fragment_id &
        topology$prospective_solve_continuations$fragment_b %in%
          active_fragment_id,
      ,
      drop = FALSE
    ]
  endpoint_id = endpoints$render_road_endpoint_id
  active_topology = list(
    fragments = fragments,
    endpoints = endpoints,
    point_event_participants = point_event_participants,
    crossing_participants = crossing_participants,
    point_pairs = point_pairs,
    crossing_pairs = crossing_pairs,
    junction_equality_pairs = junction_equality_pairs,
    topology_conflict_pairs = topology_conflict_pairs,
    layer_overlaps = layer_overlaps,
    prospective_solve_continuations = prospective_solve_continuations,
    ambiguous_endpoint_id = intersect(
      topology$ambiguous_endpoint_id,
      endpoint_id
    ),
    candidate_anchor_endpoint_id = intersect(
      topology$candidate_anchor_endpoint_id,
      endpoint_id
    )
  )
  list(
    topology = active_topology,
    terrain_profiles = terrain_profiles
  )
}

#' Identify fragments exempt from terrain-floor constraints
#'
#' @param fragments Prepared road fragments.
#'
#' @return Logical vector aligned with `fragments`.
#' @keywords internal
identify_render_road_underground_fragments = function(fragments) {
  tunnel = tolower(trimws(as.character(fragments$render_road_tunnel)))
  location = tolower(trimws(as.character(fragments$render_road_location)))
  explicit_tunnel = !is.na(tunnel) &
    nzchar(tunnel) &
    !(tunnel %in% c("no", "false", "0"))
  underground = !is.na(location) &
    location %in% c("underground", "underwater", "subway")
  explicit_tunnel | underground
}

#' Identify fragments that define elevated profile spans
#'
#' @param topology Active road topology.
#'
#' @return Sorted elevated fragment identifiers.
#' @keywords internal
identify_render_road_elevated_fragments = function(topology) {
  fragments = topology$fragments
  fragment_row = stats::setNames(
    seq_len(nrow(fragments)),
    fragments$render_road_fragment_id
  )
  elevated = fragments$render_road_fragment_id[
    fragments$render_road_layer > 0
  ]
  if (nrow(topology$crossing_pairs)) {
    for (pair in seq_len(nrow(topology$crossing_pairs))) {
      fragment_a = topology$crossing_pairs$fragment_a[[pair]]
      fragment_b = topology$crossing_pairs$fragment_b[[pair]]
      layer_a = fragments$render_road_layer[
        fragment_row[[as.character(fragment_a)]]
      ]
      layer_b = fragments$render_road_layer[
        fragment_row[[as.character(fragment_b)]]
      ]
      elevated = c(
        elevated,
        if (layer_a > layer_b) fragment_a else fragment_b
      )
    }
  }
  if (nrow(topology$layer_overlaps)) {
    for (overlap in seq_len(nrow(topology$layer_overlaps))) {
      fragment_a = topology$layer_overlaps$fragment_a[[overlap]]
      fragment_b = topology$layer_overlaps$fragment_b[[overlap]]
      layer_a = fragments$render_road_layer[
        fragment_row[[as.character(fragment_a)]]
      ]
      layer_b = fragments$render_road_layer[
        fragment_row[[as.character(fragment_b)]]
      ]
      elevated = c(
        elevated,
        if (layer_a > layer_b) fragment_a else fragment_b
      )
    }
  }
  sort(unique(elevated))
}

#' Traverse one path- or cycle-structured continuation component
#'
#' @param members Fragment identifiers in one continuation component.
#' @param continuations Prospective continuation relations without geometry.
#' @param fragment_length Named fragment lengths in metres.
#'
#' @return Ordered members, closure status, and the closing true-gap length.
#' @keywords internal
traverse_render_road_profile_path = function(
  members,
  continuations,
  fragment_length
) {
  members = sort(unique(as.integer(members)))
  component_edge = which(
    continuations$fragment_a %in%
      members &
      continuations$fragment_b %in% members
  )
  degree = stats::setNames(integer(length(members)), as.character(members))
  for (edge in component_edge) {
    degree[[as.character(continuations$fragment_a[[edge]])]] =
      degree[[as.character(continuations$fragment_a[[edge]])]] + 1L
    degree[[as.character(continuations$fragment_b[[edge]])]] =
      degree[[as.character(continuations$fragment_b[[edge]])]] + 1L
  }
  if (any(degree > 2L)) {
    stop(
      "Selected continuation graph is not path/cycle structured.",
      call. = FALSE
    )
  }
  closed = length(members) > 1L && all(degree == 2L)
  frontier = members[degree < 2L]
  current = if (length(frontier)) min(frontier) else min(members)
  previous_edge = NA_integer_
  entering_side = NA_character_
  used_fragment = integer(0)
  path_rows = list()
  repeat {
    if (current %in% used_fragment) {
      break
    }
    incident_edge = component_edge[
      continuations$fragment_a[component_edge] == current |
        continuations$fragment_b[component_edge] == current
    ]
    outgoing_edge = setdiff(incident_edge, previous_edge)
    outgoing_edge = if (length(outgoing_edge)) {
      min(outgoing_edge)
    } else {
      NA_integer_
    }
    if (is.na(entering_side)) {
      if (is.na(outgoing_edge)) {
        orientation = 1L
      } else {
        current_side = if (
          continuations$fragment_a[[outgoing_edge]] == current
        ) {
          continuations$side_a[[outgoing_edge]]
        } else {
          continuations$side_b[[outgoing_edge]]
        }
        orientation = if (current_side == "end") 1L else -1L
      }
    } else {
      orientation = if (entering_side == "start") 1L else -1L
    }
    length_m = fragment_length[[as.character(current)]]
    next_fragment = NA_integer_
    next_side = NA_character_
    gap_after = 0
    if (!is.na(outgoing_edge)) {
      if (continuations$fragment_a[[outgoing_edge]] == current) {
        next_fragment = continuations$fragment_b[[outgoing_edge]]
        next_side = continuations$side_b[[outgoing_edge]]
      } else {
        next_fragment = continuations$fragment_a[[outgoing_edge]]
        next_side = continuations$side_a[[outgoing_edge]]
      }
      gap_after = continuations$endpoint_distance[[outgoing_edge]]
      if (!is.finite(gap_after) || gap_after < 0) {
        gap_after = 0
      }
    }
    path_rows[[length(path_rows) + 1L]] = data.frame(
      path_order = length(path_rows) + 1L,
      render_road_fragment_id = current,
      orientation = orientation,
      fragment_length = length_m,
      gap_after = gap_after,
      stringsAsFactors = FALSE
    )
    used_fragment = c(used_fragment, current)
    if (is.na(outgoing_edge) || next_fragment %in% used_fragment) {
      if (closed && !identical(next_fragment, used_fragment[[1L]])) {
        stop(
          "A continuation cycle did not close at its first fragment.",
          call. = FALSE
        )
      }
      break
    }
    previous_edge = outgoing_edge
    entering_side = next_side
    current = next_fragment
  }
  if (!setequal(used_fragment, members)) {
    stop("Profile span traversal omitted fragments.", call. = FALSE)
  }
  list(
    members = do.call(rbind, path_rows),
    closed = closed,
    closing_gap = if (closed) path_rows[[length(path_rows)]]$gap_after else 0
  )
}

#' Build physical-regime road-profile spans
#'
#' @param topology Active road topology.
#' @param fragment_length Named fragment lengths in metres.
#'
#' @return Span and oriented span-member tables.
#' @keywords internal
build_render_road_profile_spans = function(topology, fragment_length) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The `igraph` package is required for road profile spans.")
  }
  fragment_id = topology$fragments$render_road_fragment_id
  continuations = sf::st_drop_geometry(
    topology$prospective_solve_continuations
  )
  graph_edges = if (nrow(continuations)) {
    data.frame(
      from = as.character(continuations$fragment_a),
      to = as.character(continuations$fragment_b),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(from = character(0), to = character(0))
  }
  graph = igraph::graph_from_data_frame(
    graph_edges,
    directed = FALSE,
    vertices = data.frame(name = as.character(fragment_id))
  )
  membership = igraph::components(graph)$membership
  component_fragment = split(
    as.integer(names(membership)),
    as.integer(membership)
  )
  component_fragment = component_fragment[
    order(vapply(component_fragment, min, integer(1)))
  ]

  elevated_fragment = identify_render_road_elevated_fragments(topology)
  underground_fragment = topology$fragments$render_road_fragment_id[
    identify_render_road_underground_fragments(topology$fragments)
  ]
  fragment_regime = stats::setNames(
    ifelse(
      fragment_id %in% underground_fragment,
      "underground",
      ifelse(fragment_id %in% elevated_fragment, "elevated", "surface")
    ),
    fragment_id
  )

  span_rows = list()
  member_rows = list()
  used_fragment = integer(0)
  for (path_id in seq_along(component_fragment)) {
    path = traverse_render_road_profile_path(
      component_fragment[[path_id]],
      continuations,
      fragment_length
    )
    ordered = path$members
    ordered$regime = unname(
      fragment_regime[as.character(ordered$render_road_fragment_id)]
    )
    if (path$closed && length(unique(ordered$regime)) > 1L) {
      next_regime = c(ordered$regime[-1L], ordered$regime[[1L]])
      boundary = which(ordered$regime != next_regime)
      start = boundary[[1L]] %% nrow(ordered) + 1L
      order_index = c(
        seq.int(start, nrow(ordered)),
        if (start > 1L) seq_len(start - 1L) else integer(0)
      )
      ordered = ordered[order_index, , drop = FALSE]
      rownames(ordered) = NULL
    }
    run_id = cumsum(c(
      TRUE,
      ordered$regime[-1L] != ordered$regime[-nrow(ordered)]
    ))
    for (run in unique(run_id)) {
      run_members = ordered[run_id == run, , drop = FALSE]
      run_closed = path$closed && nrow(run_members) == nrow(ordered)
      span_id = length(span_rows) + 1L
      increments = run_members$fragment_length + run_members$gap_after
      span_offset = c(0, utils::head(cumsum(increments), -1L))
      if (!run_closed) {
        span_length = utils::tail(
          span_offset + run_members$fragment_length,
          1L
        )
      } else {
        span_length = sum(increments)
      }
      first_member = run_members[1L, , drop = FALSE]
      last_member = run_members[nrow(run_members), , drop = FALSE]
      start_side = if (first_member$orientation[[1L]] == 1L) "start" else "end"
      outer_end_side = if (last_member$orientation[[1L]] == 1L) {
        "end"
      } else {
        "start"
      }
      regime = run_members$regime[[1L]]
      span_rows[[span_id]] = data.frame(
        span_id = span_id,
        path_id = path_id,
        reference_regime = regime,
        span_length = span_length,
        start_fragment_id = first_member$render_road_fragment_id[[1L]],
        start_side = start_side,
        end_fragment_id = if (run_closed) {
          first_member$render_road_fragment_id[[1L]]
        } else {
          last_member$render_road_fragment_id[[1L]]
        },
        end_side = if (run_closed) start_side else outer_end_side,
        outer_end_fragment_id = last_member$render_road_fragment_id[[1L]],
        outer_end_side = outer_end_side,
        closing_gap = if (run_closed) last_member$gap_after[[1L]] else 0,
        closed = run_closed,
        elevated = regime == "elevated",
        underground = regime == "underground",
        no_dip = regime == "elevated",
        reference = if (regime == "surface") {
          "terrain"
        } else if (regime == "underground") {
          "underground_terrain"
        } else if (run_closed) {
          "periodic_chord"
        } else {
          "span_chord"
        },
        stringsAsFactors = FALSE
      )
      run_members$span_id = span_id
      run_members$order_in_span = seq_len(nrow(run_members))
      run_members$span_offset = span_offset
      member_rows[[length(member_rows) + 1L]] = run_members[, c(
        "span_id",
        "order_in_span",
        "render_road_fragment_id",
        "orientation",
        "span_offset",
        "fragment_length",
        "gap_after",
        "regime"
      )]
      used_fragment = c(
        used_fragment,
        run_members$render_road_fragment_id
      )
    }
  }
  if (!setequal(used_fragment, fragment_id)) {
    stop("Profile span traversal omitted fragments.", call. = FALSE)
  }
  list(
    spans = do.call(rbind, span_rows),
    members = do.call(rbind, member_rows)
  )
}


#' Normalize adaptive road-profile constraints
#'
#' @param adaptive_constraints Default `NULL`. Constraints requested by a
#' continuous-profile audit.
#'
#' @return Schema-stable adaptive constraint table.
#' @keywords internal
normalize_render_road_adaptive_constraints = function(
  adaptive_constraints = NULL
) {
  empty = data.frame(
    type = character(0),
    fragment_a = integer(0),
    distance_a = numeric(0),
    fragment_b = integer(0),
    distance_b = numeric(0),
    event_id = integer(0),
    clearance = numeric(0),
    source_margin = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(adaptive_constraints) || !nrow(adaptive_constraints)) {
    return(empty)
  }
  adaptive_constraints = as.data.frame(adaptive_constraints)
  missing_columns = setdiff(names(empty), names(adaptive_constraints))
  for (column in missing_columns) {
    adaptive_constraints[[column]] = empty[[column]]
  }
  adaptive_constraints = adaptive_constraints[, names(empty), drop = FALSE]
  valid_type = c(
    "terrain_floor",
    "no_dip_chord",
    "overlap_clearance"
  )
  if (any(!(adaptive_constraints$type %in% valid_type))) {
    stop("Unknown adaptive road-profile constraint type.", call. = FALSE)
  }
  adaptive_constraints$fragment_a = as.integer(
    adaptive_constraints$fragment_a
  )
  adaptive_constraints$fragment_b = as.integer(
    adaptive_constraints$fragment_b
  )
  adaptive_constraints$distance_a = as.numeric(
    adaptive_constraints$distance_a
  )
  adaptive_constraints$distance_b = as.numeric(
    adaptive_constraints$distance_b
  )
  adaptive_constraints$event_id = as.integer(adaptive_constraints$event_id)
  adaptive_constraints$clearance = as.numeric(
    adaptive_constraints$clearance
  )
  adaptive_constraints$source_margin = as.numeric(
    adaptive_constraints$source_margin
  )
  adaptive_constraints
}

#' Resolve one road-profile control with a checked tolerance
#'

#' Classify road-profile solve frontiers
#'
#' @param topology Active road topology.
#' @param fragment_length Named fragment lengths.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Endpoint identifier sets and endpoint classifications. Ground
#' anchors are disabled; surface contact is determined by the terrain floor
#' and terrain-reference objective.
#' @keywords internal
identify_render_road_profile_anchor_sets = function(
  topology,
  fragment_length,
  control_tolerance
) {
  endpoints = sf::st_drop_geometry(topology$endpoints)
  active_endpoint_id = endpoints$render_road_endpoint_id
  boundary_endpoint_id = endpoints$render_road_endpoint_id[
    endpoints$supplied_boundary
  ]
  selected_endpoint_id = unique(c(
    topology$prospective_solve_continuations$endpoint_a,
    topology$prospective_solve_continuations$endpoint_b
  ))
  ambiguous_endpoint_id = topology$ambiguous_endpoint_id
  if (is.null(ambiguous_endpoint_id)) {
    ambiguous_endpoint_id = integer(0)
  }
  conflict_endpoint_id = integer(0)
  conflicts = topology$topology_conflict_pairs
  if (nrow(conflicts)) {
    conflict_endpoint_id = unique(c(
      conflicts$endpoint_id_a[conflicts$endpoint_a],
      conflicts$endpoint_id_b[conflicts$endpoint_b]
    ))
    conflict_endpoint_id = conflict_endpoint_id[!is.na(conflict_endpoint_id)]
  }
  ambiguous_endpoint_id = intersect(
    active_endpoint_id,
    ambiguous_endpoint_id
  )
  conflict_endpoint_id = intersect(
    active_endpoint_id,
    unique(conflict_endpoint_id)
  )
  selected_endpoint_id = intersect(active_endpoint_id, selected_endpoint_id)
  ground_anchor_endpoint_id = integer(0)
  solve_frontier_endpoint_id = setdiff(
    active_endpoint_id,
    unique(c(ground_anchor_endpoint_id, selected_endpoint_id))
  )
  endpoints$endpoint_role = "internal_or_unclassified"
  endpoints$endpoint_role[
    endpoints$render_road_endpoint_id %in% selected_endpoint_id
  ] = "selected_continuation"
  endpoints$endpoint_role[
    endpoints$render_road_endpoint_id %in% solve_frontier_endpoint_id
  ] = "solve_frontier"
  endpoints$endpoint_role[
    endpoints$render_road_endpoint_id %in% boundary_endpoint_id
  ] = "boundary_frontier"
  endpoints$endpoint_role[
    endpoints$render_road_endpoint_id %in% ambiguous_endpoint_id
  ] = "ambiguous_frontier"
  endpoints$endpoint_role[
    endpoints$render_road_endpoint_id %in% conflict_endpoint_id
  ] = "conflict_frontier"
  list(
    ground_anchor_endpoint_id = sort(unique(ground_anchor_endpoint_id)),
    solve_frontier_endpoint_id = sort(unique(solve_frontier_endpoint_id)),
    ambiguous_endpoint_id = sort(unique(ambiguous_endpoint_id)),
    conflict_endpoint_id = sort(unique(conflict_endpoint_id)),
    boundary_endpoint_id = sort(unique(boundary_endpoint_id)),
    selected_continuation_endpoint_id = sort(unique(selected_endpoint_id)),
    endpoints = endpoints
  )
}

#' Prepare immutable numerical road-profile compiler input
#'
#' @param topology Road topology diagnostics.
#' @param terrain_profiles Default `NULL`. Terrain reference profiles accepted
#' by [normalize_render_road_terrain_profiles()].
#' @param layer_spacing Default `5.5`. Fallback adjacent-layer clearance in
#' metres.
#' @param maximum_grade Default `0.15`. Maximum absolute longitudinal grade.
#' Positive infinity removes the grade bound.
#' @param maximum_grade_rate Default `1e-3`. Maximum grade change per metre.
#' @param curvature_weight Default `100`. Objective weight on grade change.
#' @param grade_weight Default `1`. Objective weight on grade magnitude.
#' @param terrain_reference_weight Default `1e-3`. Objective weight toward the
#' sampled terrain reference.
#' @param continuation_grade_tolerance Default `0.14`. Maximum absolute grade
#' mismatch at selected road continuations. Continuation height remains exact.
#' @param underground_reference_depth Default `NULL`, which uses
#' `layer_spacing`. Terrain-relative reference depth in metres.
#' @param underground_reference_weight Default `1e-3`. Underground reference
#' objective weight.
#' @param uplift_weight Default `1e-5`. Linear uplift objective weight.
#' @param control_tolerance Default `1e-7`. Control matching tolerance in
#' metres.
#'
#' @return Immutable numerical compiler input and R-side result context.
#' @keywords internal
prepare_render_road_profile_specification = function(
  topology,
  terrain_profiles = NULL,
  layer_spacing = 5.5,
  maximum_grade = 0.15,
  maximum_grade_rate = 1e-3,
  curvature_weight = 100,
  grade_weight = 1,
  terrain_reference_weight = 1e-3,
  continuation_grade_tolerance = 0.14,
  underground_reference_depth = NULL,
  underground_reference_weight = 1e-3,
  uplift_weight = 1e-5,
  control_tolerance = 1e-7
) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop(
      "The `Matrix` package is required for road profile solving.",
      call. = FALSE
    )
  }
  layer_spacing = assert_render_road_profile_setting(
    layer_spacing,
    "layer_spacing"
  )
  maximum_grade = assert_render_road_profile_setting(
    maximum_grade,
    "maximum_grade",
    allow_infinite = TRUE
  )
  maximum_grade_rate = assert_render_road_profile_setting(
    maximum_grade_rate,
    "maximum_grade_rate"
  )
  curvature_weight = assert_render_road_profile_setting(
    curvature_weight,
    "curvature_weight",
    allow_zero = TRUE
  )
  grade_weight = assert_render_road_profile_setting(
    grade_weight,
    "grade_weight",
    allow_zero = TRUE
  )
  terrain_reference_weight = assert_render_road_profile_setting(
    terrain_reference_weight,
    "terrain_reference_weight",
    allow_zero = TRUE
  )
  continuation_grade_tolerance = assert_render_road_profile_setting(
    continuation_grade_tolerance,
    "continuation_grade_tolerance",
    allow_zero = TRUE
  )
  if (is.null(underground_reference_depth)) {
    underground_reference_depth = layer_spacing
  } else {
    underground_reference_depth = assert_render_road_profile_setting(
      underground_reference_depth,
      "underground_reference_depth"
    )
  }
  underground_reference_weight = assert_render_road_profile_setting(
    underground_reference_weight,
    "underground_reference_weight"
  )
  uplift_weight = assert_render_road_profile_setting(
    uplift_weight,
    "uplift_weight",
    allow_zero = TRUE
  )
  control_tolerance = assert_render_road_profile_setting(
    control_tolerance,
    "control_tolerance"
  )
  settings = list(
    layer_spacing = layer_spacing,
    maximum_grade = maximum_grade,
    maximum_grade_rate = maximum_grade_rate,
    curvature_weight = curvature_weight,
    grade_weight = grade_weight,
    terrain_reference_weight = terrain_reference_weight,
    continuation_grade_tolerance = continuation_grade_tolerance,
    underground_reference_depth = underground_reference_depth,
    underground_reference_weight = underground_reference_weight,
    uplift_weight = uplift_weight,
    control_tolerance = control_tolerance
  )

  subset = build_render_road_active_profile_topology(
    topology,
    terrain_profiles
  )
  topology = subset$topology
  fragments = topology$fragments
  fragment_id = as.integer(fragments$render_road_fragment_id)
  fragment_index = stats::setNames(
    seq_along(fragment_id) - 1L,
    as.character(fragment_id)
  )
  terrain_profiles = normalize_render_road_terrain_profiles(
    topology,
    subset$terrain_profiles
  )
  geometry_info = attr(terrain_profiles, "geometry_info")
  fragment_length = vapply(geometry_info, `[[`, numeric(1), "length")
  names(fragment_length) = as.character(fragment_id)
  profile_spans = build_render_road_profile_spans(
    topology,
    fragment_length
  )
  anchor_sets = identify_render_road_profile_anchor_sets(
    topology,
    fragment_length,
    control_tolerance
  )

  terrain_count = vapply(terrain_profiles, nrow, integer(1))
  terrain_start = cumsum(c(0L, utils::head(terrain_count, -1L)))
  terrain_distance = unlist(
    lapply(terrain_profiles, `[[`, "distance"),
    use.names = FALSE
  )
  terrain_elevation = unlist(
    lapply(terrain_profiles, `[[`, "elevation"),
    use.names = FALSE
  )
  span_member_row = match(
    fragment_id,
    profile_spans$members$render_road_fragment_id
  )
  spans = profile_spans$spans

  participants = topology$point_event_participants
  initial_control = list(
    fragment_index = if (nrow(participants)) {
      unname(fragment_index[
        as.character(participants$render_road_fragment_id)
      ])
    } else {
      integer(0)
    },
    distance = if (nrow(participants)) {
      as.numeric(participants$distance)
    } else {
      numeric(0)
    }
  )

  point_pairs = topology$point_pairs
  point_flag = if (nrow(point_pairs)) {
    ifelse(
      point_pairs$topology_layer_conflict,
      3L,
      ifelse(
        point_pairs$layer_relationship &
          point_pairs$topology_relation == "interior_crossing",
        1L,
        2L
      )
    )
  } else {
    integer(0)
  }
  point_relations = list(
    pair_id = as.integer(point_pairs$point_pair_id),
    fragment_a = unname(
      fragment_index[as.character(point_pairs$fragment_a)]
    ),
    distance_a = as.numeric(point_pairs$distance_a),
    fragment_b = unname(
      fragment_index[as.character(point_pairs$fragment_b)]
    ),
    distance_b = as.numeric(point_pairs$distance_b),
    flag = as.integer(point_flag)
  )

  crossing_pairs = topology$crossing_pairs
  crossing_count = nrow(crossing_pairs)
  crossing = list(
    crossing_id = integer(crossing_count),
    pair_id = integer(crossing_count),
    lower_fragment = integer(crossing_count),
    upper_fragment = integer(crossing_count),
    lower_distance = numeric(crossing_count),
    upper_distance = numeric(crossing_count),
    lower_rank = numeric(crossing_count),
    upper_rank = numeric(crossing_count),
    clearance = numeric(crossing_count)
  )
  if (crossing_count) {
    for (pair in seq_len(crossing_count)) {
      record = crossing_pairs[pair, , drop = FALSE]
      participant = topology$crossing_participants[
        topology$crossing_participants$crossing_id == record$crossing_id[[1L]] &
          topology$crossing_participants$render_road_fragment_id %in%
            c(record$fragment_a[[1L]], record$fragment_b[[1L]]),
        ,
        drop = FALSE
      ]
      rank_a = participant$local_order[
        participant$render_road_fragment_id == record$fragment_a[[1L]]
      ]
      rank_b = participant$local_order[
        participant$render_road_fragment_id == record$fragment_b[[1L]]
      ]
      if (length(rank_a) != 1L || length(rank_b) != 1L) {
        stop(
          sprintf(
            "Crossing pair %s does not map to two dense event ranks.",
            record$point_pair_id[[1L]]
          ),
          call. = FALSE
        )
      }
      lower_is_a = rank_a < rank_b
      lower_fragment = if (lower_is_a) {
        record$fragment_a[[1L]]
      } else {
        record$fragment_b[[1L]]
      }
      upper_fragment = if (lower_is_a) {
        record$fragment_b[[1L]]
      } else {
        record$fragment_a[[1L]]
      }
      upper_row = match(upper_fragment, fragment_id)
      clearance = fragments$render_road_clearance[[upper_row]]
      if (!is.finite(clearance)) {
        clearance = layer_spacing
      }
      crossing$crossing_id[[pair]] = record$crossing_id[[1L]]
      crossing$pair_id[[pair]] = record$point_pair_id[[1L]]
      crossing$lower_fragment[[pair]] = fragment_index[[
        as.character(lower_fragment)
      ]]
      crossing$upper_fragment[[pair]] = fragment_index[[
        as.character(upper_fragment)
      ]]
      crossing$lower_distance[[pair]] = if (lower_is_a) {
        record$distance_a[[1L]]
      } else {
        record$distance_b[[1L]]
      }
      crossing$upper_distance[[pair]] = if (lower_is_a) {
        record$distance_b[[1L]]
      } else {
        record$distance_a[[1L]]
      }
      crossing$lower_rank[[pair]] = if (lower_is_a) rank_a else rank_b
      crossing$upper_rank[[pair]] = if (lower_is_a) rank_b else rank_a
      crossing$clearance[[pair]] = clearance
    }
  }

  junction_pairs = topology$junction_equality_pairs
  junction = list(
    junction_id = as.integer(junction_pairs$junction_id),
    pair_id = as.integer(junction_pairs$point_pair_id),
    fragment_a = unname(
      fragment_index[as.character(junction_pairs$fragment_a)]
    ),
    fragment_b = unname(
      fragment_index[as.character(junction_pairs$fragment_b)]
    ),
    distance_a = as.numeric(junction_pairs$distance_a),
    distance_b = as.numeric(junction_pairs$distance_b)
  )

  layer_overlaps = topology$layer_overlaps
  overlap_count = nrow(layer_overlaps)
  overlap = list(
    overlap_id = integer(overlap_count),
    lower_fragment = integer(overlap_count),
    upper_fragment = integer(overlap_count),
    lower_start = numeric(overlap_count),
    lower_end = numeric(overlap_count),
    upper_start = numeric(overlap_count),
    upper_end = numeric(overlap_count),
    clearance = numeric(overlap_count)
  )
  if (overlap_count) {
    for (relation in seq_len(overlap_count)) {
      record = layer_overlaps[relation, , drop = FALSE]
      row_a = match(record$fragment_a[[1L]], fragment_id)
      row_b = match(record$fragment_b[[1L]], fragment_id)
      lower_is_a = fragments$render_road_layer[[row_a]] <
        fragments$render_road_layer[[row_b]]
      lower_suffix = if (lower_is_a) "a" else "b"
      upper_suffix = if (lower_is_a) "b" else "a"
      lower_fragment = record[[paste0("fragment_", lower_suffix)]][[1L]]
      upper_fragment = record[[paste0("fragment_", upper_suffix)]][[1L]]
      upper_row = if (lower_is_a) row_b else row_a
      clearance = fragments$render_road_clearance[[upper_row]]
      if (!is.finite(clearance)) {
        clearance = layer_spacing
      }
      overlap$overlap_id[[relation]] = record$overlap_id[[1L]]
      overlap$lower_fragment[[relation]] = fragment_index[[
        as.character(lower_fragment)
      ]]
      overlap$upper_fragment[[relation]] = fragment_index[[
        as.character(upper_fragment)
      ]]
      overlap$lower_start[[relation]] =
        resolve_render_road_overlap_endpoint_distance(
          record,
          lower_suffix,
          "start"
        )
      overlap$lower_end[[relation]] =
        resolve_render_road_overlap_endpoint_distance(
          record,
          lower_suffix,
          "end"
        )
      overlap$upper_start[[relation]] =
        resolve_render_road_overlap_endpoint_distance(
          record,
          upper_suffix,
          "start"
        )
      overlap$upper_end[[relation]] =
        resolve_render_road_overlap_endpoint_distance(
          record,
          upper_suffix,
          "end"
        )
      overlap$clearance[[relation]] = clearance
    }
  }

  continuation_source = topology$prospective_solve_continuations
  continuation_count = nrow(continuation_source)
  continuation = list(
    continuation_id = as.integer(continuation_source$continuation_id),
    fragment_a = unname(
      fragment_index[as.character(continuation_source$fragment_a)]
    ),
    fragment_b = unname(
      fragment_index[as.character(continuation_source$fragment_b)]
    ),
    distance_a = numeric(continuation_count),
    distance_b = numeric(continuation_count),
    sign_a = ifelse(continuation_source$side_a == "end", 1, -1),
    sign_b = ifelse(continuation_source$side_b == "start", 1, -1),
    gap = as.numeric(continuation_source$endpoint_distance),
    exact = as.logical(continuation_source$exact_endpoint)
  )
  if (continuation_count) {
    continuation$distance_a = ifelse(
      continuation_source$side_a == "start",
      0,
      fragment_length[as.character(continuation_source$fragment_a)]
    )
    continuation$distance_b = ifelse(
      continuation_source$side_b == "start",
      0,
      fragment_length[as.character(continuation_source$fragment_b)]
    )
  }

  endpoints = sf::st_drop_geometry(topology$endpoints)
  # Endpoint terrain snaps are intentionally absent. This stable empty schema
  # keeps compiler diagnostics compatible while the profile finds surface
  # contact through the terrain objective and engineering constraints.
  anchor = list(
    endpoint_id = integer(0),
    fragment = integer(0),
    side = integer(0),
    distance = numeric(0)
  )
  endpoint_fragment = unname(
    fragment_index[as.character(endpoints$render_road_fragment_id)]
  )
  endpoint_side = ifelse(endpoints$endpoint_side == "start", 0L, 1L)
  endpoint_distance = ifelse(
    endpoint_side == 0L,
    0,
    fragment_length[as.character(endpoints$render_road_fragment_id)]
  )
  span_members = profile_spans$members
  support_point = point_relations$flag == 1L
  support_candidate_fragment = c(
    point_relations$fragment_a[support_point],
    point_relations$fragment_b[support_point],
    overlap$lower_fragment,
    overlap$lower_fragment,
    overlap$upper_fragment,
    overlap$upper_fragment
  )
  support_candidate_distance = c(
    point_relations$distance_a[support_point],
    point_relations$distance_b[support_point],
    overlap$lower_start,
    overlap$lower_end,
    overlap$upper_start,
    overlap$upper_end
  )

  specification = list(
    component = list(
      id = sort(unique(as.integer(fragments$solve_component_id))),
      fragment_component_index = match(
        fragments$solve_component_id,
        sort(unique(fragments$solve_component_id))
      ) -
        1L
    ),
    fragment = list(
      id = fragment_id,
      feature_id = as.integer(fragments$render_road_feature_id),
      component_id = as.integer(fragments$solve_component_id),
      length = unname(as.numeric(fragment_length)),
      layer = as.numeric(fragments$render_road_layer),
      underground = identify_render_road_underground_fragments(fragments),
      span_id = as.integer(
        profile_spans$members$span_id[span_member_row]
      ),
      span_station_offset = as.numeric(
        profile_spans$members$span_offset[span_member_row]
      ),
      span_orientation = as.integer(
        profile_spans$members$orientation[span_member_row]
      )
    ),
    terrain = list(
      start = as.integer(terrain_start),
      count = as.integer(terrain_count),
      distance = as.numeric(terrain_distance),
      elevation = as.numeric(terrain_elevation)
    ),
    initial_control = initial_control,
    point_relation = point_relations,
    endpoint = list(
      id = as.integer(endpoints$render_road_endpoint_id),
      fragment = as.integer(endpoint_fragment),
      side = as.integer(endpoint_side),
      distance = as.numeric(endpoint_distance),
      role = match(
        anchor_sets$endpoints$endpoint_role,
        c(
          "internal_or_unclassified",
          "selected_continuation",
          "solve_frontier",
          "boundary_frontier",
          "ambiguous_frontier",
          "conflict_frontier"
        )
      )
    ),
    span = list(
      id = as.integer(spans$span_id),
      length = as.numeric(spans$span_length),
      start_fragment = unname(
        fragment_index[as.character(spans$start_fragment_id)]
      ),
      start_side = ifelse(spans$start_side == "start", 0L, 1L),
      end_fragment = unname(
        fragment_index[as.character(spans$end_fragment_id)]
      ),
      end_side = ifelse(spans$end_side == "start", 0L, 1L),
      closed = as.logical(spans$closed),
      no_dip = as.logical(spans$no_dip),
      reference = match(
        spans$reference,
        c(
          "terrain",
          "underground_terrain",
          "span_chord",
          "periodic_chord"
        )
      )
    ),
    span_member = list(
      span_id = as.integer(span_members$span_id),
      order = as.integer(span_members$order_in_span),
      fragment = unname(
        fragment_index[
          as.character(span_members$render_road_fragment_id)
        ]
      ),
      orientation = as.integer(span_members$orientation),
      offset = as.numeric(span_members$span_offset),
      fragment_length = as.numeric(span_members$fragment_length),
      gap_after = as.numeric(span_members$gap_after),
      regime = match(
        span_members$regime,
        c("surface", "elevated", "underground")
      )
    ),
    support_arc = list(
      span_id = as.integer(spans$span_id),
      closed = as.logical(spans$closed),
      span_length = as.numeric(spans$span_length),
      start_fragment = unname(
        fragment_index[as.character(spans$start_fragment_id)]
      ),
      start_side = ifelse(spans$start_side == "start", 0L, 1L),
      end_fragment = unname(
        fragment_index[as.character(spans$end_fragment_id)]
      ),
      end_side = ifelse(spans$end_side == "start", 0L, 1L),
      candidate_fragment = as.integer(support_candidate_fragment),
      candidate_distance = as.numeric(support_candidate_distance)
    ),
    anchor = anchor,
    crossing = crossing,
    junction = junction,
    overlap = overlap,
    continuation = continuation,
    validity = list(
      finite_geometry = all(vapply(
        geometry_info,
        function(info) {
          all(is.finite(c(
            info$coordinates,
            info$distance,
            info$length
          )))
        },
        logical(1)
      )),
      finite_control_terrain = all(is.finite(c(
        terrain_distance,
        terrain_elevation
      )))
    ),
    settings = unlist(settings, use.names = TRUE)
  )
  context = list(
    topology = topology,
    terrain_profiles = terrain_profiles,
    fragment_length = fragment_length,
    spans = profile_spans$spans,
    span_members = profile_spans$members,
    anchor_sets = anchor_sets,
    topology_conflict_pairs = topology$topology_conflict_pairs,
    settings = settings
  )
  list(specification = specification, context = context)
}

#' Compile one numerical road-profile problem
#'
#' @param specification Immutable numerical compiler input.
#' @param context Immutable R-side result context.
#' @param adaptive_constraints Default `NULL`. Continuous-audit requests.
#' @param native Default `NULL`. Precompiled native numerical problem.
#'
#' @return A `render_road_profile_problem`.
#' @keywords internal
compile_render_road_profile_problem = function(
  specification,
  context,
  adaptive_constraints = NULL,
  native = NULL
) {
  adaptive_constraints = normalize_render_road_adaptive_constraints(
    adaptive_constraints
  )
  fragment_id = specification$fragment$id
  if (
    nrow(adaptive_constraints) &&
      any(!(adaptive_constraints$fragment_a %in% fragment_id))
  ) {
    stop("Adaptive controls reference an inactive fragment.", call. = FALSE)
  }
  adaptive_fragment_a = match(
    adaptive_constraints$fragment_a,
    fragment_id
  ) -
    1L
  adaptive_fragment_b = match(
    adaptive_constraints$fragment_b,
    fragment_id
  ) -
    1L
  adaptive_fragment_b[!is.finite(adaptive_constraints$fragment_b)] =
    NA_integer_
  if (is.null(native)) {
    native = compile_render_road_profile_problem_cpp(
      specification,
      list(
        type = match(
          adaptive_constraints$type,
          c(
            "terrain_floor",
            "no_dip_chord",
            "overlap_clearance"
          )
        ),
        fragment_a = as.integer(adaptive_fragment_a),
        distance_a = as.numeric(adaptive_constraints$distance_a),
        fragment_b = as.integer(adaptive_fragment_b),
        distance_b = as.numeric(adaptive_constraints$distance_b),
        event_id = as.integer(adaptive_constraints$event_id),
        clearance = as.numeric(adaptive_constraints$clearance),
        source_margin = as.numeric(adaptive_constraints$source_margin)
      )
    )
  }
  control = native$controls
  controls = data.frame(
    control_id = seq_along(control$fragment_id),
    render_road_fragment_id = control$fragment_id,
    render_road_feature_id = control$feature_id,
    solve_component_id = control$component_id,
    distance = control$distance,
    control_tolerance = control$tolerance,
    terrain = control$terrain,
    render_road_layer = control$layer,
    span_id = control$span_id,
    span_station = control$span_station,
    endpoint_control = control$endpoint,
    crossing_control = control$crossing,
    junction_control = control$junction,
    conflict_control = control$conflict,
    overlap_control = control$overlap,
    adaptive_control = control$adaptive,
    station_weight = control$station_weight,
    height_variable = control$height_variable,
    grade_variable = control$grade_variable,
    stringsAsFactors = FALSE
  )
  intervals = as.data.frame(native$interval_metadata)
  spans = context$spans
  spans$start_control_id = native$span_controls$start
  spans$end_control_id = native$span_controls$end
  spans$periodic_support_control_id = native$span_controls$periodic
  support_arcs = as.data.frame(native$support_arcs)
  anchors = as.data.frame(native$anchors)
  anchors$endpoint_side = as.character(ifelse(
    anchors$endpoint_side == 0L,
    "start",
    "end"
  ))
  anchors = anchors[, c(
    "render_road_endpoint_id",
    "render_road_fragment_id",
    "endpoint_side",
    "control_id",
    "terrain",
    "solve_component_id"
  )]
  clearances = as.data.frame(native$clearances)
  clearances$type = c(
    "crossing",
    "overlap_start",
    "overlap_end",
    "overlap_adaptive"
  )[clearances$type]
  clearances = clearances[, c(
    "type",
    "event_id",
    "pair_id",
    "lower_fragment_id",
    "upper_fragment_id",
    "lower_control_id",
    "upper_control_id",
    "lower_distance",
    "upper_distance",
    "lower_rank",
    "upper_rank",
    "clearance",
    "solve_component_id"
  )]
  overlap_relations = as.data.frame(native$overlap_relations)
  junction_equalities = as.data.frame(native$junction_equalities)
  continuation_equalities = as.data.frame(native$continuation_equalities)
  chord_controls = as.data.frame(native$chord_controls)
  curvature_terms = as.data.frame(native$curvature_terms)
  constraint_names = c(
    "quadratic_interval",
    "grade_rate",
    "grade_bound",
    "terrain_floor",
    "ground_anchor",
    "crossing_clearance",
    "junction_height",
    "overlap_clearance",
    "overlap_clearance_adaptive",
    "continuation_height",
    "continuation_grade",
    "continuation_gap_interval",
    "continuation_gap_grade_rate",
    "no_dip_span_chord"
  )
  constraints = as.data.frame(native$constraint_metadata)
  constraints$type = constraint_names[constraints$type]
  constraints = constraints[, c(
    "constraint_id",
    "type",
    "solve_component_id",
    "fragment_a",
    "fragment_b",
    "event_id",
    "clearance",
    "distance_a",
    "distance_b",
    "lower",
    "upper"
  )]
  variable_count = nrow(controls) * 2L
  matrix_p = Matrix::sparseMatrix(
    i = native$P$i,
    j = native$P$j,
    x = native$P$x,
    dims = c(variable_count, variable_count)
  )
  matrix_a = Matrix::sparseMatrix(
    i = native$A$i,
    j = native$A$j,
    x = native$A$x,
    dims = c(nrow(constraints), variable_count)
  )
  result = list(
    topology = context$topology,
    terrain_profiles = context$terrain_profiles,
    adaptive_constraints = adaptive_constraints,
    fragment_length = context$fragment_length,
    controls = controls,
    intervals = intervals,
    spans = spans,
    span_members = context$span_members,
    support_arcs = support_arcs,
    anchors = anchors,
    anchor_sets = context$anchor_sets,
    clearances = clearances,
    overlap_relations = overlap_relations,
    junction_equalities = junction_equalities,
    continuation_equalities = continuation_equalities,
    chord_controls = chord_controls,
    curvature_terms = curvature_terms,
    topology_conflict_pairs = context$topology_conflict_pairs,
    P = Matrix::forceSymmetric(matrix_p, uplo = "U"),
    q = native$q,
    A = matrix_a,
    lower = native$lower,
    upper = native$upper,
    constraints = constraints,
    variable_component = native$variable_component,
    settings = context$settings,
    diagnostics = list(
      control_count = nrow(controls),
      adaptive_control_count = sum(controls$adaptive_control),
      constraint_count = nrow(constraints),
      constraint_counts = table(constraints$type),
      ground_anchor_endpoint_id = context$anchor_sets$ground_anchor_endpoint_id,
      solve_frontier_endpoint_id = context$anchor_sets$solve_frontier_endpoint_id,
      boundary_frontier_endpoint_id = context$anchor_sets$boundary_endpoint_id,
      ambiguous_endpoint_id = context$anchor_sets$ambiguous_endpoint_id,
      conflict_endpoint_id = context$anchor_sets$conflict_endpoint_id
    ),
    profile_specification = specification,
    profile_context = context,
    compiler_diagnostics = native$diagnostics
  )
  result$audit_specification =
    prepare_render_road_profile_audit_specification(result)
  class(result) = c("render_road_profile_problem", class(result))
  result
}

#' Build a sparse quadratic road profile problem
#'
#' @inheritParams prepare_render_road_profile_specification
#' @param adaptive_constraints Default `NULL`. Continuous-audit constraint
#'   requests used when compiling a refined problem.
#'
#' @return Sparse matrices, controls, constraints, and solve diagnostics.
#' @keywords internal
build_render_road_profile_problem = function(
  topology,
  terrain_profiles = NULL,
  layer_spacing = 5.5,
  maximum_grade = 0.15,
  maximum_grade_rate = 1e-3,
  curvature_weight = 100,
  grade_weight = 1,
  terrain_reference_weight = 1e-3,
  continuation_grade_tolerance = 0.14,
  underground_reference_depth = NULL,
  underground_reference_weight = 1e-3,
  uplift_weight = 1e-5,
  control_tolerance = 1e-7,
  adaptive_constraints = NULL
) {
  prepared = prepare_render_road_profile_specification(
    topology = topology,
    terrain_profiles = terrain_profiles,
    layer_spacing = layer_spacing,
    maximum_grade = maximum_grade,
    maximum_grade_rate = maximum_grade_rate,
    curvature_weight = curvature_weight,
    grade_weight = grade_weight,
    terrain_reference_weight = terrain_reference_weight,
    continuation_grade_tolerance = continuation_grade_tolerance,
    underground_reference_depth = underground_reference_depth,
    underground_reference_weight = underground_reference_weight,
    uplift_weight = uplift_weight,
    control_tolerance = control_tolerance
  )
  compile_render_road_profile_problem(
    prepared$specification,
    prepared$context,
    adaptive_constraints
  )
}

#' Prepare a transient continuous-audit specification
#'
#' @param problem Sparse road profile problem.
#'
#' @return Ordinary vectors describing controls, terrain, support chords, and
#' overlap relations for the transient native continuous audit. Dense indices,
#' slice starts, and control rows are zero-based; external relation identifiers
#' remain stable R identifiers. Numerical distances and elevations are metres.
#' @keywords internal
prepare_render_road_profile_audit_specification = function(problem) {
  fragments = problem$topology$fragments
  fragment_id = fragments$render_road_fragment_id
  controls = problem$controls
  control_rows = lapply(fragment_id, function(fragment) {
    rows = which(controls$render_road_fragment_id == fragment)
    rows[order(controls$distance[rows])]
  })
  control_count = lengths(control_rows)
  if (any(control_count < 2L)) {
    fragment = fragment_id[which(control_count < 2L)[[1L]]]
    stop(
      sprintf("Fragment %s does not have two profile controls.", fragment),
      call. = FALSE
    )
  }
  control_row = unlist(control_rows, use.names = FALSE)
  control_start = cumsum(c(0L, utils::head(control_count, -1L)))
  terrain_profiles = problem$terrain_profiles[as.character(fragment_id)]
  terrain_count = vapply(terrain_profiles, nrow, integer(1))
  terrain_start = cumsum(c(0L, utils::head(terrain_count, -1L)))

  active_spans = problem$spans[problem$spans$no_dip, , drop = FALSE]
  active_members = lapply(active_spans$span_id, function(span_id) {
    problem$span_members[
      problem$span_members$span_id == span_id,
      ,
      drop = FALSE
    ]
  })
  active_members = if (length(active_members)) {
    do.call(rbind, active_members)
  } else {
    problem$span_members[0, , drop = FALSE]
  }
  active_arcs = problem$support_arcs[
    problem$support_arcs$span_id %in% active_spans$span_id,
    ,
    drop = FALSE
  ]

  overlaps = problem$overlap_relations
  adaptive_overlap = problem$adaptive_constraints[
    problem$adaptive_constraints$type == "overlap_clearance",
    ,
    drop = FALSE
  ]
  geometry_info = attr(problem$terrain_profiles, "geometry_info")
  if (is.null(geometry_info)) {
    geometry_info = lapply(
      sf::st_geometry(fragments),
      calculate_render_road_metric_line_distances
    )
  } else {
    geometry_info = geometry_info[as.character(fragment_id)]
  }
  finite_geometry = all(vapply(
    geometry_info,
    function(info) {
      all(is.finite(c(info$coordinates, info$distance, info$length)))
    },
    logical(1)
  ))

  list(
    fragment_id = as.integer(fragment_id),
    fragment_component = as.integer(fragments$solve_component_id),
    control_start = as.integer(control_start),
    control_count = as.integer(control_count),
    control_row = as.integer(control_row - 1L),
    control_distance = as.numeric(controls$distance[control_row]),
    control_tolerance = as.numeric(vapply(
      control_rows,
      function(rows) max(controls$control_tolerance[rows]),
      numeric(1)
    )),
    underground = identify_render_road_underground_fragments(fragments),
    terrain_start = as.integer(terrain_start),
    terrain_count = as.integer(terrain_count),
    terrain_distance = as.numeric(unlist(
      lapply(terrain_profiles, `[[`, "distance"),
      use.names = FALSE
    )),
    terrain_elevation = as.numeric(unlist(
      lapply(terrain_profiles, `[[`, "elevation"),
      use.names = FALSE
    )),
    chord_span_id = as.integer(active_members$span_id),
    chord_fragment_index = as.integer(
      match(
        active_members$render_road_fragment_id,
        fragment_id
      ) -
        1L
    ),
    chord_span_offset = as.numeric(active_members$span_offset),
    chord_orientation = as.integer(active_members$orientation),
    chord_fragment_length = as.numeric(active_members$fragment_length),
    arc_span_id = as.integer(active_arcs$span_id),
    arc_start_control = as.integer(active_arcs$start_control_id - 1L),
    arc_end_control = as.integer(active_arcs$end_control_id - 1L),
    arc_start_station = as.numeric(active_arcs$start_station),
    arc_end_station = as.numeric(active_arcs$end_station),
    arc_length = as.numeric(active_arcs$arc_length),
    arc_span_length = as.numeric(active_arcs$span_length),
    arc_closed = as.logical(active_arcs$closed),
    arc_id = as.integer(active_arcs$support_arc_id),
    overlap_id = as.integer(overlaps$overlap_id),
    overlap_lower_fragment_index = as.integer(
      match(
        overlaps$lower_fragment_id,
        fragment_id
      ) -
        1L
    ),
    overlap_upper_fragment_index = as.integer(
      match(
        overlaps$upper_fragment_id,
        fragment_id
      ) -
        1L
    ),
    overlap_lower_start = as.numeric(overlaps$lower_distance_start),
    overlap_lower_end = as.numeric(overlaps$lower_distance_end),
    overlap_upper_start = as.numeric(overlaps$upper_distance_start),
    overlap_upper_end = as.numeric(overlaps$upper_distance_end),
    overlap_clearance = as.numeric(overlaps$clearance),
    prior_overlap_id = as.integer(adaptive_overlap$event_id),
    prior_lower_distance = as.numeric(adaptive_overlap$distance_a),
    prior_upper_distance = as.numeric(adaptive_overlap$distance_b),
    finite_geometry = finite_geometry,
    finite_control_terrain = all(is.finite(c(
      controls$distance,
      controls$terrain,
      unlist(terrain_profiles, use.names = FALSE)
    )))
  )
}

#' Evaluate one quadratic road profile at arbitrary stations
#'
#' @param problem Sparse road profile problem.
#' @param solution Solved road profile object.
#' @param fragment Fragment identifier.
#' @param distance Distances along the fragment in metres.
#'
#' @return Evaluated height, grade, and interval control identifiers.
#' @keywords internal
evaluate_render_road_profile_at = function(
  problem,
  solution,
  fragment,
  distance
) {
  controls = solution$controls
  distance = as.numeric(distance)
  specification = problem$audit_specification
  if (is.null(specification)) {
    specification = prepare_render_road_profile_audit_specification(problem)
  }
  fragment_index = match(as.integer(fragment), specification$fragment_id)
  if (is.na(fragment_index)) {
    stop(
      sprintf("Fragment %s does not have two profile controls.", fragment),
      call. = FALSE
    )
  }
  result = evaluate_render_road_profiles_cpp(
    fragment_index = rep.int(fragment_index - 1L, length(distance)),
    distance = distance,
    control_start = specification$control_start,
    control_count = specification$control_count,
    control_row = specification$control_row,
    control_distance = specification$control_distance,
    height = as.numeric(controls$height),
    grade = as.numeric(controls$grade)
  )
  as.data.frame(result, stringsAsFactors = FALSE)
}


#' Find continuous road-profile engineering violations
#'
#' @param problem Sparse road profile problem.
#' @param solution Solved road profile object.
#' @param tolerance Feasibility tolerance in metres.
#' @param diagnostics Default `TRUE`. Whether to return complete continuous
#' check tables.
#'
#' @return Continuous margins, optional details, and adaptive control requests.
#' @keywords internal
find_render_road_profile_continuous_violations = function(
  problem,
  solution,
  tolerance,
  diagnostics = TRUE
) {
  specification = problem$audit_specification
  if (is.null(specification)) {
    specification =
      prepare_render_road_profile_audit_specification(problem)
  }
  controls = solution$controls
  result = audit_render_road_profiles_cpp(
    specification_list = specification,
    height = as.numeric(controls$height),
    grade = as.numeric(controls$grade),
    tolerance = tolerance,
    diagnostics = diagnostics
  )
  request_type = c(
    "terrain_floor",
    "no_dip_chord",
    "overlap_clearance"
  )
  requests = as.data.frame(result$requests, stringsAsFactors = FALSE)
  requests$type = request_type[requests$type]
  result$requests = normalize_render_road_adaptive_constraints(requests)
  if (diagnostics) {
    result$terrain = as.data.frame(
      result$terrain,
      stringsAsFactors = FALSE
    )
    result$chord = as.data.frame(
      result$chord,
      stringsAsFactors = FALSE
    )
    result$overlap = as.data.frame(
      result$overlap,
      stringsAsFactors = FALSE
    )
  }
  result
}

#' Describe an infeasible road profile component
#'
#' @param problem Sparse road profile problem.
#' @param component_id Solve component identifier.
#' @param status Solver status.
#'
#' @return Structured component diagnostics.
#' @keywords internal
diagnose_render_road_profile_component = function(
  problem,
  component_id,
  status
) {
  controls = problem$controls
  component_controls = controls$solve_component_id == component_id
  fragment_id = sort(unique(
    controls$render_road_fragment_id[component_controls]
  ))
  fragment_rows = match(
    fragment_id,
    problem$topology$fragments$render_road_fragment_id
  )
  component_constraints = problem$constraints[
    problem$constraints$solve_component_id == component_id,
    ,
    drop = FALSE
  ]
  relation_counts = as.data.frame(
    table(component_constraints$type),
    stringsAsFactors = FALSE
  )
  names(relation_counts) = c("relation", "count")
  relation_counts = relation_counts[relation_counts$count > 0, , drop = FALSE]
  list(
    status = status,
    solve_component_id = component_id,
    fragment_id = fragment_id,
    feature_id = sort(unique(
      problem$topology$fragments$render_road_feature_id[fragment_rows]
    )),
    relation_counts = relation_counts
  )
}


#' Restore native continuous road-profile diagnostics
#'
#' @param result Native continuous-audit result.
#'
#' @return Continuous diagnostics with standard request names and tables.
#' @keywords internal
restore_render_road_profile_continuous_diagnostics = function(result) {
  request_type = c(
    "terrain_floor",
    "no_dip_chord",
    "overlap_clearance"
  )
  requests = as.data.frame(result$requests, stringsAsFactors = FALSE)
  requests$type = request_type[requests$type]
  result$requests = normalize_render_road_adaptive_constraints(requests)
  if (!is.null(result$terrain)) {
    result$terrain = as.data.frame(
      result$terrain,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(result$chord)) {
    result$chord = as.data.frame(
      result$chord,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(result$overlap)) {
    result$overlap = as.data.frame(
      result$overlap,
      stringsAsFactors = FALSE
    )
  }
  result
}

#' Restore one native road-profile solution
#'
#' @param result Native adaptive-solver result.
#' @param problem Final sparse road-profile problem.
#'
#' @return A standard `render_road_profile_solution`.
#' @keywords internal
restore_render_road_profile_native_solution = function(result, problem) {
  controls = problem$controls
  height = if (is.null(result$height)) {
    result$controls$height
  } else {
    result$height
  }
  grade = if (is.null(result$grade)) {
    result$controls$grade
  } else {
    result$grade
  }
  controls$height = as.numeric(height)
  controls$grade = as.numeric(grade)
  components = as.data.frame(
    result$components,
    stringsAsFactors = FALSE
  )
  solved = list(
    problem = problem,
    solution = as.numeric(result$solution),
    controls = controls,
    components = components,
    solver_results = result$solver_results
  )
  class(solved) = c("render_road_profile_solution", class(solved))
  solved
}

#' Solve sparse quadratic road profiles with a native adaptive loop
#'
#' @param problem Sparse road profile problem.
#' @param verbose Default `FALSE`. Whether OSQP prints progress.
#' @param absolute_tolerance Default `1e-7`. Absolute solver tolerance.
#' @param relative_tolerance Default `1e-7`. Relative solver tolerance.
#' @param maximum_iterations Default `20000`. Maximum OSQP iterations per
#'   component.
#' @param profile_tolerance Default `0.05`. Accepted geometric profile
#'   tolerance in metres for adaptive refinement and the engineering audit.
#' @param maximum_refinement_iterations Default `20`. Maximum adaptive
#'   continuous-constraint refinement iterations.
#' @param maximum_requests_per_relation Default `4L`. Maximum
#'   number of well-separated refinement requests selected for each fragment
#'   and request type, or each overlap event. Must be between 1 and 4.
#'
#' @return Solved variables, controls, and component diagnostics.
#' @keywords internal
solve_render_road_profile_problem = function(
  problem,
  verbose = FALSE,
  absolute_tolerance = 1e-7,
  relative_tolerance = 1e-7,
  maximum_iterations = 20000,
  profile_tolerance = 0.05,
  maximum_refinement_iterations = 20,
  maximum_requests_per_relation = 4L
) {
  if (!inherits(problem, "render_road_profile_problem")) {
    stop("`problem` must be a road profile problem.", call. = FALSE)
  }
  if (!requireNamespace("osqp", quietly = TRUE)) {
    stop(
      "The `osqp` package is required for road profile solving.",
      call. = FALSE
    )
  }
  absolute_tolerance = assert_render_road_profile_setting(
    absolute_tolerance,
    "absolute_tolerance"
  )
  relative_tolerance = assert_render_road_profile_setting(
    relative_tolerance,
    "relative_tolerance"
  )
  profile_tolerance = assert_render_road_profile_setting(
    profile_tolerance,
    "profile_tolerance",
    allow_zero = TRUE
  )
  if (
    !is.numeric(maximum_iterations) ||
      length(maximum_iterations) != 1L ||
      !is.finite(maximum_iterations) ||
      maximum_iterations < 1 ||
      maximum_iterations != floor(maximum_iterations)
  ) {
    stop("`maximum_iterations` must be a positive integer.", call. = FALSE)
  }
  if (
    !is.numeric(maximum_refinement_iterations) ||
      length(maximum_refinement_iterations) != 1L ||
      !is.finite(maximum_refinement_iterations) ||
      maximum_refinement_iterations < 0 ||
      maximum_refinement_iterations != floor(maximum_refinement_iterations)
  ) {
    stop(
      "`maximum_refinement_iterations` must be a non-negative integer.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(maximum_requests_per_relation) ||
      length(maximum_requests_per_relation) != 1L ||
      !is.finite(maximum_requests_per_relation) ||
      maximum_requests_per_relation < 1 ||
      maximum_requests_per_relation > 4 ||
      maximum_requests_per_relation != floor(maximum_requests_per_relation)
  ) {
    stop(
      "`maximum_requests_per_relation` must be an integer between 1 and 4.",
      call. = FALSE
    )
  }
  if (
    is.null(problem$profile_specification) ||
      is.null(problem$profile_context)
  ) {
    stop(
      paste0(
        "`problem` does not contain the transient native specification; ",
        "rebuild it with `build_render_road_profile_problem()`."
      ),
      call. = FALSE
    )
  }

  native_specification = problem$profile_specification
  adaptive_constraints = normalize_render_road_adaptive_constraints(
    problem$adaptive_constraints
  )
  fragment_id = native_specification$fragment$id
  adaptive_fragment_a = match(
    adaptive_constraints$fragment_a,
    fragment_id
  ) -
    1L
  adaptive_fragment_b = match(
    adaptive_constraints$fragment_b,
    fragment_id
  ) -
    1L
  adaptive_fragment_b[!is.finite(adaptive_constraints$fragment_b)] =
    NA_integer_
  native_specification$adaptive = list(
    type = match(
      adaptive_constraints$type,
      c(
        "terrain_floor",
        "no_dip_chord",
        "overlap_clearance"
      )
    ),
    fragment_a = as.integer(adaptive_fragment_a),
    distance_a = as.numeric(adaptive_constraints$distance_a),
    fragment_b = as.integer(adaptive_fragment_b),
    distance_b = as.numeric(adaptive_constraints$distance_b),
    event_id = as.integer(adaptive_constraints$event_id),
    clearance = as.numeric(adaptive_constraints$clearance),
    source_margin = as.numeric(adaptive_constraints$source_margin)
  )

  solve_component = function(component) {
    tryCatch(
      {
        matrix_p = Matrix::sparseMatrix(
          i = component$P$i,
          j = component$P$j,
          x = component$P$x,
          dims = c(
            component$variable_count,
            component$variable_count
          ),
          symmetric = TRUE
        )
        matrix_a = Matrix::sparseMatrix(
          i = component$A$i,
          j = component$A$j,
          x = component$A$x,
          dims = c(
            component$constraint_count,
            component$variable_count
          )
        )
        result = osqp::solve_osqp(
          P = matrix_p,
          q = component$q,
          A = matrix_a,
          l = component$lower,
          u = component$upper,
          pars = osqp::osqpSettings(
            verbose = verbose,
            eps_abs = absolute_tolerance,
            eps_rel = relative_tolerance,
            max_iter = as.integer(maximum_iterations),
            polishing = TRUE
          )
        )
        list(
          x = as.numeric(result$x),
          status = tolower(result$info$status),
          status_message = result$info$status,
          iterations = result$info$iter,
          objective = result$info$obj_val,
          primal_residual = result$info$prim_res,
          dual_residual = result$info$dual_res,
          elapsed = result$info$run_time
        )
      },
      error = function(error) {
        list(
          x = numeric(0),
          status = "callback error",
          status_message = conditionMessage(error),
          iterations = NA_integer_,
          objective = NA_real_,
          primal_residual = NA_real_,
          dual_residual = NA_real_,
          elapsed = NA_real_
        )
      }
    )
  }

  native = solve_render_road_profiles_cpp(
    specification = native_specification,
    solve_component = solve_component,
    profile_tolerance = profile_tolerance,
    maximum_refinement_iterations = as.integer(maximum_refinement_iterations),
    diagnostics = FALSE,
    maximum_requests_per_relation = as.integer(
      maximum_requests_per_relation
    )
  )
  native_adaptive = as.data.frame(
    native$adaptive,
    stringsAsFactors = FALSE
  )
  native_adaptive$type = c(
    "terrain_floor",
    "no_dip_chord",
    "overlap_clearance"
  )[native_adaptive$type]
  native_adaptive = normalize_render_road_adaptive_constraints(
    native_adaptive
  )
  final_problem = compile_render_road_profile_problem(
    specification = problem$profile_specification,
    context = problem$profile_context,
    adaptive_constraints = native_adaptive,
    native = native$compiled
  )

  if (!native$success && native$failure_type == "solver") {
    status = native$solver_result$status_message
    diagnostics = diagnose_render_road_profile_component(
      final_problem,
      native$component_id,
      status
    )
    condition = structure(
      list(
        message = sprintf(
          "Road profile component %d was not solved: %s.",
          native$component_id,
          status
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

  solved = restore_render_road_profile_native_solution(
    native,
    final_problem
  )
  continuous = restore_render_road_profile_continuous_diagnostics(
    native$continuous_diagnostics
  )
  if (!native$success && native$failure_type == "refinement") {
    message = if (native$failure_reason == "iteration_limit") {
      sprintf(
        paste0(
          "Road-profile continuous refinement did not converge after ",
          "%d iterations."
        ),
        maximum_refinement_iterations
      )
    } else {
      paste0(
        "A continuous road-profile violation remained at an ",
        "already-constrained station."
      )
    }
    condition = structure(
      list(
        message = message,
        call = NULL,
        diagnostics = continuous,
        solution = solved
      ),
      class = c(
        "render_road_profile_refinement_failure",
        "error",
        "condition"
      )
    )
    stop(condition)
  }
  if (!native$success && native$failure_type == "engineering") {
    status = native$failure_status
    condition = structure(
      list(
        message = sprintf(
          "Road profile component %d failed after %s.",
          native$component_id,
          status
        ),
        call = NULL,
        diagnostics = diagnose_render_road_profile_component(
          final_problem,
          native$component_id,
          status
        )
      ),
      class = c(
        "render_road_profile_infeasible",
        "error",
        "condition"
      )
    )
    stop(condition)
  }
  if (!native$success) {
    stop("Native road-profile solving failed unexpectedly.", call. = FALSE)
  }

  engineering_audit = native$engineering_audit
  class(engineering_audit) = c(
    "render_road_profile_audit",
    class(engineering_audit)
  )
  solved$continuous_diagnostics = continuous
  solved$engineering_audit = engineering_audit
  solved$refinement_iterations = native$refinement_iterations
  solved$refinement_requests = native$refinement_requests
  solved$rendered_elevation = native$rendered_elevation
  solved$timing = native$timing
  solved
}


#' Audit solved road profile constraints
#'
#' @param problem Sparse road profile problem.
#' @param solution Solved road profile object.
#' @param tolerance Default `1e-6`. Feasibility tolerance.
#' @param continuous Default `NULL`. An already computed continuous-audit
#' result for the same problem, solution, and tolerance.
#'
#' @return Rendering-critical feasibility and continuous-margin diagnostics.
#' @keywords internal
audit_render_road_profiles = function(
  problem,
  solution,
  tolerance = 1e-6,
  continuous = NULL
) {
  if (!inherits(problem, "render_road_profile_problem")) {
    stop("`problem` must be a road profile problem.", call. = FALSE)
  }
  if (!inherits(solution, "render_road_profile_solution")) {
    stop("`solution` must be a solved road profile.", call. = FALSE)
  }
  if (inherits(solution$problem, "render_road_profile_problem")) {
    problem = solution$problem
  }
  if (
    !is.numeric(tolerance) ||
      length(tolerance) != 1L ||
      !is.finite(tolerance) ||
      tolerance < 0
  ) {
    stop("`tolerance` must be non-negative and finite.", call. = FALSE)
  }
  activity = as.numeric(problem$A %*% solution$solution)
  lower_violation = pmax(problem$lower - activity, 0)
  upper_violation = pmax(activity - problem$upper, 0)
  constraint_violation = pmax(lower_violation, upper_violation)
  if (is.null(continuous)) {
    continuous = find_render_road_profile_continuous_violations(
      problem,
      solution,
      tolerance,
      diagnostics = FALSE
    )
  }
  maximum_constraint_violation = if (length(constraint_violation)) {
    max(constraint_violation)
  } else {
    0
  }
  continuous_violation = max(
    pmax(-continuous$continuous_terrain_margin, 0),
    pmax(-continuous$continuous_chord_margin, 0),
    pmax(-continuous$continuous_overlap_clearance_margin, 0)
  )
  finite_violation = if (continuous$finite_profile_coordinates) 0 else Inf
  maximum_violation = max(
    maximum_constraint_violation,
    continuous_violation,
    finite_violation
  )
  result = list(
    passed = is.finite(maximum_violation) &&
      maximum_violation <= tolerance &&
      continuous$finite_profile_coordinates,
    tolerance = tolerance,
    maximum_violation = maximum_violation,
    maximum_constraint_violation = maximum_constraint_violation,
    continuous_terrain_margin = continuous$continuous_terrain_margin,
    continuous_chord_margin = continuous$continuous_chord_margin,
    continuous_overlap_clearance_margin = continuous$continuous_overlap_clearance_margin,
    finite_profile_coordinates = continuous$finite_profile_coordinates
  )
  if (
    !result$passed &&
      is.null(continuous$terrain)
  ) {
    result$continuous_diagnostics =
      find_render_road_profile_continuous_violations(
        problem,
        solution,
        tolerance,
        diagnostics = TRUE
      )
  }
  class(result) = c("render_road_profile_audit", class(result))
  result
}


#' Normalize road world scale
#'
#' @param texture_world_scale Two-value x-z world scale.
#'
#' @return Normalized world scale.
#' @keywords internal
normalize_render_road_world_scale = function(texture_world_scale) {
  texture_world_scale = suppressWarnings(as.numeric(
    texture_world_scale[1:2]
  ))
  if (
    length(texture_world_scale) != 2L ||
      any(!is.finite(texture_world_scale)) ||
      any(texture_world_scale <= 0)
  ) {
    return(c(1, 1))
  }
  texture_world_scale
}

#' Make default road lane texture
#'
#' @inheritParams render_roads
#' @param lane_color Default `"white"`. sRGB color for dashed lane divider
#' markings.
#' @param centerline_color Default `"#ffd23f"`. sRGB color for the center
#' divider.
#' @param edge_line_color Default `"white"`. sRGB color for solid edge markings.
#' @param size Default `128`. Texture width/height in pixels.
#'
#' @return Texture file path.
#' @keywords internal
make_road_lane_texture = function(
  roadcolor = "#303030",
  lanes = 2,
  lane_color = "white",
  centerline_color = "#ffd23f",
  edge_line_color = "white",
  lane_line_width = 0.035,
  lane_dash_fraction = NULL,
  lane_dash_length = 3,
  lane_gap_length = 10,
  size = 128
) {
  resolve_dash_fraction = function(
    lane_dash_fraction,
    lane_dash_length,
    lane_gap_length
  ) {
    if (is.null(lane_dash_fraction)) {
      cycle_length = lane_dash_length + lane_gap_length
      if (!is.finite(cycle_length) || cycle_length <= 0) {
        stop(
          "`lane_dash_length + lane_gap_length` must be positive.",
          call. = FALSE
        )
      }
      return(lane_dash_length / cycle_length)
    }
    assert_render_road_fraction(
      lane_dash_fraction,
      "lane_dash_fraction"
    )
  }
  calculate_marking_positions = function(lanes) {
    lanes = assert_render_road_lane_count(lanes)
    edge_offset = 0.5 / (lanes + 2)
    lane_edges = seq(
      edge_offset,
      1 - edge_offset,
      length.out = lanes + 1L
    )
    dividers = if (lanes > 1L) {
      lane_edges[-c(1L, length(lane_edges))]
    } else {
      numeric(0)
    }
    list(
      edge_lines = c(edge_offset, 1 - edge_offset),
      dividers = dividers
    )
  }

  lanes = assert_render_road_lane_count(lanes)
  lane_line_width = assert_render_road_fraction(
    lane_line_width,
    "lane_line_width"
  )
  lane_dash_length = resolve_render_positive_number(
    lane_dash_length,
    "lane_dash_length"
  )
  lane_gap_length = resolve_render_positive_number(
    lane_gap_length,
    "lane_gap_length",
    allow_zero = TRUE
  )
  lane_dash_fraction = resolve_dash_fraction(
    lane_dash_fraction,
    lane_dash_length = lane_dash_length,
    lane_gap_length = lane_gap_length
  )
  road_rgb = convert_color(roadcolor, linear = TRUE)
  texture = array(
    rep(road_rgb, each = size * size),
    dim = c(size, size, 3)
  )
  line_half_width = max(1L, round(size * lane_line_width / 2))
  draw_vertical_line = function(texture, u, color, rows = seq_len(size)) {
    col = min(size, max(1L, round(u * (size - 1L)) + 1L))
    cols = seq.int(
      max(1L, col - line_half_width),
      min(size, col + line_half_width)
    )
    texture[rows, cols, ] = array(
      rep(
        convert_color(color, linear = TRUE),
        each = length(rows) * length(cols)
      ),
      dim = c(length(rows), length(cols), 3)
    )
    texture
  }
  marking_positions = calculate_marking_positions(lanes)
  texture = draw_vertical_line(
    texture,
    marking_positions$edge_lines[[1]],
    edge_line_color
  )
  texture = draw_vertical_line(
    texture,
    marking_positions$edge_lines[[2]],
    edge_line_color
  )
  if (length(marking_positions$dividers) > 0) {
    dash_rows = seq_len(max(1L, floor(size * lane_dash_fraction)))
    center_index = which.min(abs(marking_positions$dividers - 0.5))
    for (divider_index in seq_along(marking_positions$dividers)) {
      divider_color = if (divider_index == center_index) {
        centerline_color
      } else {
        lane_color
      }
      texture = draw_vertical_line(
        texture,
        marking_positions$dividers[[divider_index]],
        divider_color,
        dash_rows
      )
    }
  }
  texture_file = tempfile(fileext = ".png")
  rayimage::ray_write_image(
    rayimage::ray_read_image(
      texture,
      source_linear = TRUE,
      assume_colorspace = rayimage::CS_SRGB
    ),
    texture_file,
    write_linear = FALSE
  )
  normalizePath(texture_file, winslash = "/", mustWork = TRUE)
}

#' Assert road lane count
#'
#' @param lanes Number of lanes.
#'
#' @return Positive integer lane count.
#' @keywords internal
assert_render_road_lane_count = function(lanes) {
  lanes = suppressWarnings(as.integer(lanes[1]))
  if (!is.finite(lanes) || lanes < 1L) {
    stop("`lanes` must be a positive integer.")
  }
  lanes
}

#' Evaluate the quintic road-envelope transition curve
#'
#' @param value Transition fraction.
#'
#' @return Quintic smoothstep values on the closed interval from zero to one.
#' @keywords internal
calculate_render_road_transition_fraction = function(value) {
  value = pmin(1, pmax(0, suppressWarnings(as.numeric(value))))
  value^3 * (value * (value * 6 - 15) + 10)
}

#' Calculate a visual road-envelope transition length
#'
#' @param lateral_change Maximum symmetric edge displacement.
#' @param maximum_lateral_rate Default `0.05`. Maximum lateral change per metre
#' of longitudinal distance.
#' @param minimum_length Default `20`. Minimum transition length in metres.
#' @param maximum_length Default `150`. Maximum transition length in metres.
#'
#' @return Transition length in metres.
#' @keywords internal
calculate_render_road_envelope_transition_length = function(
  lateral_change,
  maximum_lateral_rate = 0.05,
  minimum_length = 20,
  maximum_length = 150
) {
  lateral_change = suppressWarnings(as.numeric(lateral_change[[1L]]))
  if (!is.finite(lateral_change) || lateral_change < 0) {
    stop("`lateral_change` must be finite and nonnegative.", call. = FALSE)
  }
  maximum_lateral_rate = resolve_render_positive_number(
    maximum_lateral_rate,
    "maximum_lateral_rate"
  )
  minimum_length = resolve_render_positive_number(
    minimum_length,
    "minimum_transition_length"
  )
  maximum_length = resolve_render_positive_number(
    maximum_length,
    "maximum_transition_length"
  )
  if (maximum_length < minimum_length) {
    stop(
      "Maximum transition length cannot be less than the minimum.",
      call. = FALSE
    )
  }
  min(
    maximum_length,
    max(
      minimum_length,
      1.875 * lateral_change / maximum_lateral_rate
    )
  )
}

#' Build station-based envelope sections for one physical mesh chain
#'
#' @param mesh_chain_members Ordered member table for one physical mesh chain.
#' @param maximum_lateral_rate Default `0.05`. Maximum lateral envelope change
#' per metre.
#' @param minimum_transition_length Default `20`. Minimum transition length.
#' @param maximum_transition_length Default `150`. Maximum transition length.
#'
#' @return Schema-stable centered-envelope section table.
#' @keywords internal
build_render_road_envelope_sections = function(
  mesh_chain_members,
  maximum_lateral_rate = 0.05,
  minimum_transition_length = 20,
  maximum_transition_length = 150
) {
  empty = data.frame(
    station_start = numeric(0),
    station_end = numeric(0),
    half_width_start = numeric(0),
    half_width_end = numeric(0),
    transition_type = character(0),
    stringsAsFactors = FALSE
  )
  if (!nrow(mesh_chain_members)) {
    return(empty)
  }
  mesh_chain_members = mesh_chain_members[
    order(mesh_chain_members$member_order),
    ,
    drop = FALSE
  ]
  required_columns = c(
    "member_order",
    "chain_station_start",
    "chain_station_end",
    "road_lanes",
    "road_width"
  )
  if (!all(required_columns %in% names(mesh_chain_members))) {
    stop(
      "Mesh chain members do not contain lane-envelope metadata.",
      call. = FALSE
    )
  }
  lane_count = suppressWarnings(as.integer(mesh_chain_members$road_lanes))
  road_width = suppressWarnings(as.numeric(mesh_chain_members$road_width))
  if (
    any(!is.finite(lane_count) | lane_count < 1L) ||
      any(!is.finite(road_width) | road_width <= 0)
  ) {
    stop(
      "Road envelope members require positive lane counts and widths.",
      call. = FALSE
    )
  }
  member_count = nrow(mesh_chain_members)
  half_width = road_width / 2
  changed_boundary = if (member_count > 1L) {
    which(abs(road_width[-member_count] - road_width[-1L]) > 1e-10)
  } else {
    integer(0)
  }
  chain_start = mesh_chain_members$chain_station_start[[1L]]
  chain_end = mesh_chain_members$chain_station_end[[member_count]]
  if (!length(changed_boundary)) {
    return(data.frame(
      station_start = chain_start,
      station_end = chain_end,
      half_width_start = half_width[[1L]],
      half_width_end = half_width[[1L]],
      transition_type = "uniform",
      stringsAsFactors = FALSE
    ))
  }
  transition = lapply(changed_boundary, function(member_index) {
    boundary_station =
      mesh_chain_members$chain_station_end[[member_index]]
    transition_length = calculate_render_road_envelope_transition_length(
      lateral_change = abs(
        half_width[[member_index + 1L]] -
          half_width[[member_index]]
      ),
      maximum_lateral_rate = maximum_lateral_rate,
      minimum_length = minimum_transition_length,
      maximum_length = maximum_transition_length
    )
    data.frame(
      member_order = member_index,
      boundary_station = boundary_station,
      station_start = max(
        chain_start,
        boundary_station - transition_length / 2
      ),
      station_end = min(
        chain_end,
        boundary_station + transition_length / 2
      ),
      half_width_start = half_width[[member_index]],
      half_width_end = half_width[[member_index + 1L]],
      transition_type = if (
        lane_count[[member_index]] != lane_count[[member_index + 1L]]
      ) {
        "lane_count_transition"
      } else {
        "width_transition"
      },
      stringsAsFactors = FALSE
    )
  })
  transition = do.call(rbind, transition)
  if (nrow(transition) > 1L) {
    for (transition_index in seq_len(nrow(transition) - 1L)) {
      if (
        transition$station_end[[transition_index]] >
          transition$station_start[[transition_index + 1L]]
      ) {
        split_station = (transition$boundary_station[[transition_index]] +
          transition$boundary_station[[transition_index + 1L]]) /
          2
        transition$station_end[[transition_index]] = split_station
        transition$station_start[[transition_index + 1L]] = split_station
      }
    }
  }
  section_rows = list()
  section_index = 0L
  current_station = chain_start
  current_transition = NULL
  for (transition_index in seq_len(nrow(transition))) {
    record = transition[transition_index, , drop = FALSE]
    if (record$station_start[[1L]] > current_station) {
      if (is.null(current_transition)) {
        uniform_half_width = record$half_width_start[[1L]]
      } else {
        uniform_half_width = current_transition$half_width_end[[1L]]
      }
      section_index = section_index + 1L
      section_rows[[section_index]] = data.frame(
        station_start = current_station,
        station_end = record$station_start[[1L]],
        half_width_start = uniform_half_width,
        half_width_end = uniform_half_width,
        transition_type = "uniform",
        stringsAsFactors = FALSE
      )
    }
    section_index = section_index + 1L
    section_rows[[section_index]] = data.frame(
      station_start = record$station_start[[1L]],
      station_end = record$station_end[[1L]],
      half_width_start = record$half_width_start[[1L]],
      half_width_end = record$half_width_end[[1L]],
      transition_type = record$transition_type[[1L]],
      stringsAsFactors = FALSE
    )
    current_station = record$station_end[[1L]]
    current_transition = record
  }
  if (current_station < chain_end) {
    section_index = section_index + 1L
    section_rows[[section_index]] = data.frame(
      station_start = current_station,
      station_end = chain_end,
      half_width_start = current_transition$half_width_end[[1L]],
      half_width_end = current_transition$half_width_end[[1L]],
      transition_type = "uniform",
      stringsAsFactors = FALSE
    )
  }
  sections = do.call(rbind, section_rows)
  if (any(sections$station_end <= sections$station_start)) {
    stop("Road envelope sections have nonpositive length.", call. = FALSE)
  }
  rownames(sections) = NULL
  sections
}

#' Evaluate station-based centered road envelope sections
#'
#' @param envelope_sections Centered-envelope section table.
#' @param station Chain stations to evaluate.
#'
#' @return Evaluated symmetric half-width and transition type.
#' @keywords internal
evaluate_render_road_envelope_sections = function(
  envelope_sections,
  station
) {
  station = suppressWarnings(as.numeric(station))
  if (!nrow(envelope_sections) || any(!is.finite(station))) {
    stop("Envelope evaluation requires finite sections and stations.")
  }
  section_index = findInterval(
    station,
    envelope_sections$station_end,
    left.open = TRUE
  ) +
    1L
  section_index = pmin(nrow(envelope_sections), pmax(1L, section_index))
  section = envelope_sections[section_index, , drop = FALSE]
  section_length = section$station_end - section$station_start
  fraction = ifelse(
    section_length > 0,
    (station - section$station_start) / section_length,
    0
  )
  transition_fraction = calculate_render_road_transition_fraction(fraction)
  interpolate = function(start, end) {
    start + (end - start) * transition_fraction
  }
  data.frame(
    station = station,
    half_width = interpolate(
      section$half_width_start,
      section$half_width_end
    ),
    transition_type = section$transition_type,
    stringsAsFactors = FALSE
  )
}

#' Calculate road path world scale
#'
#' @param heightmap Heightmap matrix.
#' @param extent Scene extent.
#' @param crs Default `NULL`. Scene CRS used to convert axis spacing to metres.
#'
#' @return Two-value x-z multiplier from scene units to world units.
#' @keywords internal
calculate_road_path_world_scale = function(heightmap, extent, crs = NULL) {
  extent = tryCatch(get_extent(extent), error = function(e) NULL)
  if (
    is.null(extent) ||
      !is.matrix(heightmap) ||
      nrow(heightmap) < 2 ||
      ncol(heightmap) < 2
  ) {
    return(c(1, 1))
  }
  x_range = extent[["xmax"]] - extent[["xmin"]]
  z_range = extent[["ymax"]] - extent[["ymin"]]
  if (!is.finite(x_range) || !is.finite(z_range)) {
    return(c(1, 1))
  }
  axis_scale = c(
    abs(x_range) / (nrow(heightmap) - 1),
    abs(z_range) / (ncol(heightmap) - 1)
  )
  parsed_crs = try_parse_scene_crs(crs)
  if (is.null(parsed_crs) || !requireNamespace("sf", quietly = TRUE)) {
    return(axis_scale)
  }
  center = c(
    mean(extent[c("xmin", "xmax")]),
    mean(extent[c("ymin", "ymax")])
  )
  metric_scale = tryCatch(
    {
      axis_points = sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_point(center),
          sf::st_point(center + c(axis_scale[[1L]], 0)),
          sf::st_point(center + c(0, axis_scale[[2L]])),
          crs = parsed_crs
        )
      )
      metric_crs = resolve_render_road_metric_crs(axis_points)
      metric_coordinates = sf::st_coordinates(
        sf::st_transform(axis_points, metric_crs)
      )
      c(
        sqrt(sum(
          (metric_coordinates[2L, ] -
            metric_coordinates[1L, ])^2
        )),
        sqrt(sum(
          (metric_coordinates[3L, ] -
            metric_coordinates[1L, ])^2
        ))
      )
    },
    error = function(error) c(NA_real_, NA_real_)
  )
  if (any(!is.finite(metric_scale)) || any(metric_scale <= 0)) {
    return(axis_scale)
  }
  metric_scale
}

#' Resolve render road width
#'
#' @param road_width Requested road width.
#' @param lanes Number of lanes.
#' @param lane_width Lane width in world units.
#' @param texture_world_scale Two-value x-z multiplier from scene units to
#' world units.
#'
#' @return Road width in scene units.
#' @keywords internal
resolve_render_road_width = function(
  road_width,
  lanes,
  lane_width,
  texture_world_scale
) {
  if (!is.null(road_width)) {
    return(road_width)
  }
  texture_world_scale = suppressWarnings(as.numeric(texture_world_scale[1:2]))
  if (
    length(texture_world_scale) != 2 ||
      any(!is.finite(texture_world_scale)) ||
      any(texture_world_scale <= 0)
  ) {
    texture_world_scale = c(1, 1)
  }
  road_width_world = lane_width * (lanes + 2)
  road_width_world / mean(texture_world_scale)
}

#' Calculate road path cumulative distance
#'
#' @param points Road path points.
#' @param texture_world_scale Default `c(1, 1)`. Multipliers converting scene
#' x-z distances to world distances.
#'
#' @return Cumulative road path distance in world x-z units.
#' @keywords internal
calculate_road_path_cumulative_distance = function(
  points,
  texture_world_scale = c(1, 1)
) {
  points = as.matrix(points)
  if (nrow(points) < 2 || ncol(points) < 3) {
    return(rep(0, nrow(points)))
  }
  texture_world_scale = suppressWarnings(as.numeric(texture_world_scale[1:2]))
  if (
    length(texture_world_scale) != 2 ||
      any(!is.finite(texture_world_scale)) ||
      any(texture_world_scale <= 0)
  ) {
    texture_world_scale = c(1, 1)
  }
  path_points = points[, c(1, 3), drop = FALSE]
  complete_rows = stats::complete.cases(path_points)
  if (!all(complete_rows)) {
    path_points = path_points[complete_rows, , drop = FALSE]
  }
  if (nrow(path_points) < 2) {
    return(rep(0, nrow(path_points)))
  }
  c(
    0,
    cumsum(sqrt(rowSums(
      sweep(
        path_points[-1, , drop = FALSE] -
          path_points[-nrow(path_points), , drop = FALSE],
        2,
        texture_world_scale,
        FUN = "*"
      )^2
    )))
  )
}

#' Assert road fraction
#'
#' @param value Value to validate.
#' @param name Argument name.
#'
#' @return Numeric fraction.
#' @keywords internal
assert_render_road_fraction = function(value, name) {
  value = suppressWarnings(as.numeric(value[1]))
  if (!is.finite(value) || value <= 0 || value >= 1) {
    stop(sprintf("`%s` must be a single number between 0 and 1.", name))
  }
  value
}

#' Register road path info
#'
#' @param id rgl id.
#' @param info Road path info.
#'
#' @return Invisibly returns `id`.
#' @keywords internal
register_render_road_path_info = function(id, info) {
  id = as.integer(id)
  for (id_single in id) {
    assign(as.character(id_single), info, envir = ray_road_path_envir)
  }
  invisible(id)
}

#' Get road path info
#'
#' @param id rgl id.
#'
#' @return Road path info.
#' @keywords internal
get_render_road_path_info = function(id) {
  get0(
    as.character(as.integer(id)),
    envir = ray_road_path_envir,
    inherits = FALSE
  )
}

#' Get source vertices for a rendered rgl object
#'
#' @param id rgl object identifier.
#'
#' @return Vertex matrix with any display-only road height offset removed.
#' @keywords internal
get_render_source_vertices = function(id) {
  vertices = rgl::rgl.attrib(id, "vertices")
  road_path_info = get_render_road_path_info(id)
  preview_offset = road_path_info$rgl_preview_offset
  if (is.null(preview_offset)) {
    return(vertices)
  }
  preview_offset = tryCatch(
    suppressWarnings(as.numeric(preview_offset[[1L]])),
    error = function(error) NA_real_
  )
  if (
    length(preview_offset) == 1L &&
      is.finite(preview_offset) &&
      preview_offset != 0 &&
      is.matrix(vertices) &&
      ncol(vertices) >= 2L
  ) {
    vertices[, 2] = vertices[, 2] - preview_offset
  }
  vertices
}

#' Clear road path info
#'
#' @param id Default `NULL`. Optional id to clear.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
clear_render_road_path_info = function(id = NULL) {
  if (is.null(id)) {
    ids = ls(envir = ray_road_path_envir, all.names = TRUE)
  } else {
    ids = as.character(as.integer(id))
  }
  if (length(ids)) {
    rm(list = ids, envir = ray_road_path_envir)
  }
  invisible(NULL)
}

#' Attach registered physical topology to high-quality road tasks
#'
#' @param tasks High-quality road mesh task list.
#'
#' @return Road tasks carrying path-local mesh-topology attributes.
#' @keywords internal
attach_render_road_mesh_task_metadata = function(tasks) {
  if (!length(tasks)) {
    return(tasks)
  }
  existing_topology = vapply(
    tasks,
    function(task) {
      topology = attr(task, "mesh_topology")
      !is.null(topology) &&
        !is.null(topology$mesh_chain_id) &&
        !is.null(topology$member_order) &&
        !is.null(topology$orientation) &&
        is.finite(suppressWarnings(as.numeric(
          topology$mesh_chain_id[[1L]]
        ))) &&
        is.finite(suppressWarnings(as.numeric(
          topology$member_order[[1L]]
        ))) &&
        suppressWarnings(as.integer(topology$orientation[[1L]])) %in%
          c(-1L, 1L)
    },
    logical(1)
  )
  if (all(existing_topology)) {
    return(tasks)
  }
  road_path_info = get_ids_with_labels(typeval = "road_path")
  road_path_id = road_path_info$id
  registered = vector("list", length(tasks))
  if (length(road_path_id) == length(tasks)) {
    registered = lapply(road_path_id, get_render_road_path_info)
  }
  registered_chain_id = suppressWarnings(as.integer(vapply(
    registered,
    function(info) {
      if (is.null(info) || is.null(info$mesh_chain_id)) {
        return(NA_real_)
      }
      as.numeric(info$mesh_chain_id[[1L]])
    },
    numeric(1)
  )))
  next_chain_id = max(c(registered_chain_id, 0L), na.rm = TRUE)
  for (task_index in seq_along(tasks)) {
    if (existing_topology[[task_index]]) {
      next
    }
    path_info = registered[[task_index]]
    explicit_chain = !is.null(path_info) &&
      is.finite(registered_chain_id[[task_index]])
    if (!explicit_chain) {
      next_chain_id = next_chain_id + 1L
      path_info = list(
        mesh_chain_id = next_chain_id,
        road_path_id = task_index,
        fragment_id = NA_integer_,
        feature_id = NA_integer_,
        member_order = 1L,
        orientation = 1L,
        closed = FALSE,
        lanes = NA_integer_
      )
    }
    attr(tasks[[task_index]], "mesh_topology") = list(
      mesh_chain_id = as.integer(path_info$mesh_chain_id[[1L]]),
      road_path_id = path_info$road_path_id,
      render_road_fragment_id = if (is.null(path_info$fragment_id)) {
        NA_integer_
      } else {
        as.integer(path_info$fragment_id[[1L]])
      },
      render_road_feature_id = if (is.null(path_info$feature_id)) {
        NA_integer_
      } else {
        as.integer(path_info$feature_id[[1L]])
      },
      member_order = as.integer(path_info$member_order[[1L]]),
      orientation = as.integer(path_info$orientation[[1L]]),
      closed = isTRUE(path_info$closed),
      road_lanes = path_info$lanes
    )
  }
  tasks
}

#' Assemble precomputed road mesh chains
#'
#' @param tasks High-quality road mesh task list.
#' @param endpoint_tolerance Default `1e-6`. Maximum closing endpoint gap in
#' scene units.
#'
#' @return High-quality road mesh-chain tasks with compact diagnostics.
#' @keywords internal
assemble_render_road_mesh_chain_tasks = function(
  tasks,
  endpoint_tolerance = 1e-6
) {
  if (!length(tasks)) {
    return(tasks)
  }
  topology = lapply(tasks, function(task) attr(task, "mesh_topology"))
  chain_id = suppressWarnings(as.integer(vapply(
    topology,
    function(value) {
      if (is.null(value) || is.null(value$mesh_chain_id)) {
        return(NA_real_)
      }
      as.numeric(value$mesh_chain_id[[1L]])
    },
    numeric(1)
  )))
  if (any(!is.finite(chain_id) | chain_id < 1L)) {
    stop(
      "Every high-quality road task requires precomputed mesh-chain metadata.",
      call. = FALSE
    )
  }
  chain_levels = sort(unique(chain_id))
  chain_tasks = vector("list", length(chain_levels))
  member_rows = vector("list", length(chain_levels))
  suppressed_internal_cap_count = 0L
  for (chain_index in seq_along(chain_levels)) {
    current_chain_id = chain_levels[[chain_index]]
    task_id = which(chain_id == current_chain_id)
    member_order = suppressWarnings(as.integer(vapply(
      topology[task_id],
      function(value) as.numeric(value$member_order[[1L]]),
      numeric(1)
    )))
    orientation = suppressWarnings(as.integer(vapply(
      topology[task_id],
      function(value) as.numeric(value$orientation[[1L]]),
      numeric(1)
    )))
    if (
      any(!is.finite(member_order) | member_order < 1L) ||
        any(!(orientation %in% c(-1L, 1L))) ||
        anyDuplicated(member_order)
    ) {
      stop(
        sprintf(
          "Road mesh chain %i has invalid member metadata.",
          current_chain_id
        ),
        call. = FALSE
      )
    }
    order_index = order(member_order)
    task_id = task_id[order_index]
    member_order = member_order[order_index]
    orientation = orientation[order_index]
    if (!identical(member_order, seq_along(member_order))) {
      stop(
        sprintf(
          "Road mesh chain %i member order is not contiguous.",
          current_chain_id
        ),
        call. = FALSE
      )
    }
    closed_value = unique(vapply(
      topology[task_id],
      function(value) isTRUE(value$closed),
      logical(1)
    ))
    if (length(closed_value) != 1L) {
      stop(
        sprintf(
          "Road mesh chain %i has inconsistent closure.",
          current_chain_id
        ),
        call. = FALSE
      )
    }
    closed = closed_value[[1L]]
    chain_points = NULL
    chain_station = 0
    chain_member_rows = vector("list", length(task_id))
    for (member_index in seq_along(task_id)) {
      current_task_id = task_id[[member_index]]
      member_points = as.matrix(tasks[[current_task_id]]$points)
      if (orientation[[member_index]] < 0L) {
        member_points = member_points[
          nrow(member_points):1L,
          ,
          drop = FALSE
        ]
      }
      if (!is.null(chain_points)) {
        shared_endpoint = (member_points[1L, ] +
          chain_points[nrow(chain_points), ]) /
          2
        chain_points[nrow(chain_points), ] = shared_endpoint
        member_points[1L, ] = shared_endpoint
      }
      member_scale = normalize_render_road_world_scale(
        tasks[[current_task_id]]$texture_world_scale
      )
      member_station = calculate_road_path_cumulative_distance(
        member_points,
        texture_world_scale = member_scale
      )
      station_start = chain_station
      station_end = chain_station + utils::tail(member_station, 1L)
      member_topology = topology[[current_task_id]]
      fragment_id = suppressWarnings(as.integer(
        member_topology$render_road_fragment_id[[1L]]
      ))
      if (!length(fragment_id) || !is.finite(fragment_id)) {
        fragment_id = suppressWarnings(as.integer(
          member_topology$road_path_id[[1L]]
        ))
      }
      if (!length(fragment_id) || !is.finite(fragment_id)) {
        fragment_id = current_task_id
      }
      feature_id = suppressWarnings(as.integer(
        member_topology$render_road_feature_id[[1L]]
      ))
      if (!length(feature_id) || !is.finite(feature_id)) {
        feature_id = NA_integer_
      }
      road_lanes = suppressWarnings(as.integer(
        member_topology$road_lanes[[1L]]
      ))
      if (!length(road_lanes) || !is.finite(road_lanes)) {
        road_lanes = NA_integer_
      }
      road_width = suppressWarnings(as.numeric(
        tasks[[current_task_id]]$width[[1L]]
      ))
      if (!length(road_width) || !is.finite(road_width)) {
        road_width = NA_real_
      }
      chain_member_rows[[member_index]] = data.frame(
        mesh_chain_id = current_chain_id,
        road_path_task_id = current_task_id,
        render_road_fragment_id = fragment_id,
        render_road_feature_id = feature_id,
        road_lanes = road_lanes,
        road_width = road_width,
        member_order = member_index,
        orientation = orientation[[member_index]],
        chain_station_start = station_start,
        chain_station_end = station_end,
        cap_start = !closed && member_index == 1L,
        cap_end = !closed && member_index == length(task_id),
        closed = closed,
        stringsAsFactors = FALSE
      )
      chain_station = station_end
      if (is.null(chain_points)) {
        chain_points = member_points
      } else {
        chain_points = rbind(
          chain_points,
          member_points[-1L, , drop = FALSE]
        )
        suppressed_internal_cap_count =
          suppressed_internal_cap_count + 2L
      }
    }
    if (closed) {
      closing_gap = sqrt(sum(
        (chain_points[nrow(chain_points), 1:3] -
          chain_points[1L, 1:3])^2
      ))
      if (is.finite(closing_gap) && closing_gap <= endpoint_tolerance) {
        shared_endpoint = (chain_points[nrow(chain_points), ] +
          chain_points[1L, ]) /
          2
        chain_points[1L, ] = shared_endpoint
        chain_points = chain_points[-nrow(chain_points), , drop = FALSE]
      }
      suppressed_internal_cap_count =
        suppressed_internal_cap_count + 2L
    }
    base_task_id = task_id[[1L]]
    chain_task = tasks[[base_task_id]]
    chain_members = do.call(rbind, chain_member_rows)
    envelope_sections = if (
      all(is.finite(chain_members$road_lanes)) &&
        all(is.finite(chain_members$road_width))
    ) {
      build_render_road_envelope_sections(
        mesh_chain_members = chain_members
      )
    } else {
      NULL
    }
    material_sections = lapply(
      seq_along(task_id),
      function(member_index) {
        current_task_id = task_id[[member_index]]
        list(
          mesh_chain_id = current_chain_id,
          member_order = member_index,
          road_path_task_id = current_task_id,
          rgl_id = tasks[[current_task_id]]$rgl_id,
          roadcolor = tasks[[current_task_id]]$roadcolor,
          station_start = chain_members$chain_station_start[[member_index]],
          station_end = chain_members$chain_station_end[[member_index]],
          road_lanes = chain_members$road_lanes[[member_index]],
          material = tasks[[current_task_id]]$material,
          texture_file = tasks[[current_task_id]]$texture_file,
          texture_length = tasks[[current_task_id]]$texture_length,
          texture_repeats = tasks[[current_task_id]]$texture_repeats
        )
      }
    )
    chain_task$points = chain_points
    chain_task$width = max(chain_members$road_width, na.rm = TRUE)
    if (!is.finite(chain_task$width)) {
      chain_task$width = tasks[[base_task_id]]$width
    }
    chain_task$envelope_sections = envelope_sections
    chain_task$material_sections = material_sections
    chain_task$cap_start = !closed
    chain_task$cap_end = !closed
    chain_task$closed = closed
    chain_task$terrain_following = all(vapply(
      task_id,
      function(current_task_id) {
        isTRUE(tasks[[current_task_id]]$terrain_following)
      },
      logical(1)
    ))
    attr(chain_task, "mesh_topology") = list(
      mesh_chain_id = current_chain_id,
      mesh_chain_members = chain_members,
      envelope_sections = envelope_sections,
      material_sections = material_sections
    )
    chain_tasks[[chain_index]] = chain_task
    member_rows[[chain_index]] = chain_members
  }
  mesh_chain_members = do.call(rbind, member_rows)
  envelope_rows = lapply(
    chain_tasks,
    function(task) task$envelope_sections
  )
  envelope_rows = Filter(Negate(is.null), envelope_rows)
  envelope_sections = if (length(envelope_rows)) {
    do.call(rbind, envelope_rows)
  } else {
    NULL
  }
  chain_member_count = table(mesh_chain_members$mesh_chain_id)
  closed_chain = vapply(
    chain_tasks,
    function(task) isTRUE(task$closed),
    logical(1)
  )
  selected_continuation_count =
    sum(as.integer(chain_member_count) - 1L) +
    sum(closed_chain & as.integer(chain_member_count) > 1L)
  attr(chain_tasks, "mesh_chain_members") = mesh_chain_members
  attr(chain_tasks, "envelope_sections") = envelope_sections
  attr(chain_tasks, "mesh_chain_diagnostics") = list(
    source_task_count = length(tasks),
    mesh_chain_count = length(chain_tasks),
    selected_continuation_count = selected_continuation_count,
    explicit_continuation_count = selected_continuation_count,
    geometry_fallback_continuation_count = 0L,
    ambiguous_continuation_count = 0L,
    material_section_count = sum(vapply(
      chain_tasks,
      function(task) length(task$material_sections),
      integer(1)
    )),
    lane_count_transition_count = if (is.null(envelope_sections)) {
      0L
    } else {
      sum(
        envelope_sections$transition_type == "lane_count_transition"
      )
    },
    suppressed_internal_cap_count = suppressed_internal_cap_count,
    retained_physical_cap_count = sum(!closed_chain) * 2L,
    closed_loop_count = sum(closed_chain)
  )
  chain_tasks
}

#' Make high-quality road path meshes
#'
#' @param tasks Road path task list.
#' @param verbose Default `FALSE`. Whether to display mesh-building progress.
#' @param parallel Default `FALSE`. Whether to use multiple native threads.
#'
#' @return List of rayrender mesh objects.
#' @keywords internal
make_render_highquality_road_path_meshes = function(
  tasks,
  verbose = FALSE,
  parallel = FALSE
) {
  parallel = resolve_render_logical(parallel, "parallel")
  tasks = attach_render_road_mesh_task_metadata(tasks)
  if (!length(tasks)) {
    return(tasks)
  }
  chain_tasks = assemble_render_road_mesh_chain_tasks(tasks)
  chain_members = attr(chain_tasks, "mesh_chain_members")
  envelope_sections = attr(chain_tasks, "envelope_sections")
  chain_diagnostics = attr(chain_tasks, "mesh_chain_diagnostics")
  prepared_results = prepare_render_highquality_road_chain_meshes(
    tasks = chain_tasks,
    verbose = verbose,
    parallel = parallel
  )
  prepared_index = which(vapply(
    prepared_results,
    function(result) is.null(result$error) && !is.null(result$prepared),
    logical(1)
  ))
  native_results = if (length(prepared_index)) {
    build_render_highquality_road_mesh_batch_cpp(
      input_jobs = lapply(
        prepared_results[prepared_index],
        function(result) result$prepared$job
      ),
      parallel = parallel,
      verbose = verbose
    )
  } else {
    list()
  }
  native_result_by_chain = vector("list", length(chain_tasks))
  native_result_by_chain[prepared_index] = native_results
  mesh_results = vector("list", length(chain_tasks))
  for (chain_index in seq_along(chain_tasks)) {
    preparation_result = prepared_results[[chain_index]]
    if (!is.null(preparation_result$error)) {
      mesh_results[[chain_index]] =
        make_render_highquality_road_chain_fallback_result(
          task = chain_tasks[[chain_index]],
          sweep_error = preparation_result$error
        )
      next
    }
    if (is.null(preparation_result$prepared)) {
      mesh_results[[chain_index]] = list(
        mesh = NULL,
        failed = FALSE,
        sweep_error = NULL,
        fallback_error = NULL
      )
      next
    }
    native_result = native_result_by_chain[[chain_index]]
    if (!isTRUE(native_result$success)) {
      mesh_results[[chain_index]] =
        make_render_highquality_road_chain_fallback_result(
          task = chain_tasks[[chain_index]],
          sweep_error = native_result$error
        )
      next
    }
    finalization_result = tryCatch(
      list(
        mesh = finalize_render_highquality_road_chain_mesh(
          preparation_result$prepared,
          native_result
        ),
        error = NULL
      ),
      error = function(error) {
        list(mesh = NULL, error = conditionMessage(error))
      }
    )
    if (is.null(finalization_result$error)) {
      mesh_results[[chain_index]] = list(
        mesh = finalization_result$mesh,
        failed = FALSE,
        sweep_error = NULL,
        fallback_error = NULL
      )
    } else {
      mesh_results[[chain_index]] =
        make_render_highquality_road_chain_fallback_result(
          task = chain_tasks[[chain_index]],
          sweep_error = finalization_result$error
        )
    }
  }
  for (chain_index in seq_along(mesh_results)) {
    mesh_result = mesh_results[[chain_index]]
    if (!isTRUE(mesh_result$failed)) {
      next
    }
    task = chain_tasks[[chain_index]]
    mesh_chain_id = attr(task, "mesh_topology")$mesh_chain_id
    source_task = chain_members$render_road_fragment_id[
      chain_members$mesh_chain_id == mesh_chain_id
    ]
    stop(
      sprintf(
        paste0(
          "High-quality road mesh chain %i (source tasks %s) ",
          "failed: %s"
        ),
        chain_index,
        paste(source_task, collapse = ", "),
        paste0(
          mesh_result$sweep_error,
          " Buffered fallback failed: ",
          mesh_result$fallback_error,
          "."
        )
      ),
      call. = FALSE
    )
  }
  chain_meshes = lapply(mesh_results, `[[`, "mesh")
  buffered_fallback = vapply(
    chain_meshes,
    function(mesh) {
      isTRUE(attr(mesh, "render_road_buffered_fallback")$used)
    },
    logical(1)
  )
  buffered_fallback_count = sum(buffered_fallback)
  if (buffered_fallback_count) {
    fallback_label = if (buffered_fallback_count == 1L) {
      "chain"
    } else {
      "chains"
    }
    warning(
      sprintf(
        paste0(
          "Used the terrain-buffered road mesh fallback for %i %s with ",
          "self-overlapping sweeps."
        ),
        buffered_fallback_count,
        fallback_label
      ),
      call. = FALSE
    )
  }
  chain_meshes = Filter(Negate(is.null), chain_meshes)
  mesh_groups = lapply(chain_meshes, function(mesh) {
    if (inherits(mesh, "render_road_mesh_group")) {
      return(lapply(seq_along(mesh), function(index) mesh[[index]]))
    }
    list(mesh)
  })
  meshes = do.call(c, mesh_groups)
  attr(meshes, "mesh_chain_members") = chain_members
  attr(meshes, "envelope_sections") = envelope_sections
  chain_diagnostics$buffered_fallback_count = buffered_fallback_count
  chain_diagnostics$buffered_fallback_chain_id = vapply(
    chain_tasks[buffered_fallback],
    function(task) attr(task, "mesh_topology")$mesh_chain_id,
    integer(1)
  )
  attr(meshes, "mesh_chain_diagnostics") = chain_diagnostics
  meshes
}

#' Convert cached road meshes into translated rayrender models
#'
#' @param meshes Absolute-coordinate cached road `mesh3d` objects.
#' @param bbox_center Active rayrender scene center.
#' @param rgl_materials Default `list()`. Validated rgl material overrides.
#'
#' @return List of translated rayrender mesh models.
#' @keywords internal
make_render_highquality_cached_road_meshes = function(
  meshes,
  bbox_center,
  rgl_materials = list()
) {
  bbox_center = suppressWarnings(as.numeric(bbox_center[1:3]))
  if (length(bbox_center) != 3L || any(!is.finite(bbox_center))) {
    stop("Cached road meshes require a finite scene center.", call. = FALSE)
  }
  models = lapply(meshes, function(mesh) {
    if (!inherits(mesh, "mesh3d")) {
      return(NULL)
    }
    specification = attr(mesh, "render_road_mesh_specification")
    roadcolor = specification$roadcolor
    if (
      is.null(roadcolor) ||
        !is.character(roadcolor) ||
        !length(roadcolor) ||
        is.na(roadcolor[[1L]])
    ) {
      roadcolor = "#303030"
    }
    rgl_id = specification$rgl_id
    if (is.null(rgl_id) || !length(rgl_id)) {
      rgl_id = NA_integer_
    }
    material_override = resolve_render_highquality_rgl_material(
      rgl_materials = rgl_materials,
      id = rgl_id[[1L]],
      tag = "road_path",
      color = roadcolor[[1L]]
    )
    material = material_override
    if (is.null(material)) {
      material = specification$material
    }
    if (is.null(material)) {
      material = rayrender::diffuse(
        color = convert_color(roadcolor[[1L]], linear = TRUE)
      )
    }
    texture_file = specification$texture_file
    if (!is.null(material_override)) {
      mesh$material$texture = NULL
      texture_file = NULL
    }
    rayrender::mesh3d_model(
      mesh,
      x = -bbox_center[[1L]],
      y = -bbox_center[[2L]],
      z = -bbox_center[[3L]],
      override_material = is.null(texture_file),
      material = material
    )
  })
  Filter(Negate(is.null), models)
}

#' Build one high-quality road chain mesh with fallback diagnostics
#'
#' @param task Assembled road mesh-chain task.
#' @param parallel Default `FALSE`. Whether to use multiple native threads.
#'
#' @return List containing the mesh and captured errors.
#' @keywords internal
make_render_highquality_road_chain_mesh_result = function(
  task,
  parallel = FALSE
) {
  parallel = resolve_render_logical(parallel, "parallel")
  sweep_result = tryCatch(
    {
      preparation_result = prepare_render_highquality_road_chain_meshes(
        tasks = list(task),
        verbose = FALSE,
        parallel = parallel
      )[[1L]]
      if (!is.null(preparation_result$error)) {
        stop(preparation_result$error, call. = FALSE)
      }
      prepared = preparation_result$prepared
      native_result = if (is.null(prepared)) {
        NULL
      } else {
        build_render_highquality_road_mesh_batch_cpp(
          input_jobs = list(prepared$job),
          parallel = parallel,
          verbose = FALSE
        )[[1L]]
      }
      if (!is.null(native_result) && !isTRUE(native_result$success)) {
        stop(native_result$error, call. = FALSE)
      }
      list(
        mesh = if (is.null(prepared)) {
          NULL
        } else {
          finalize_render_highquality_road_chain_mesh(prepared, native_result)
        },
        error = NULL
      )
    },
    error = function(error) {
      list(mesh = NULL, error = conditionMessage(error))
    }
  )
  if (is.null(sweep_result$error)) {
    return(list(
      mesh = sweep_result$mesh,
      failed = FALSE,
      sweep_error = NULL,
      fallback_error = NULL
    ))
  }
  make_render_highquality_road_chain_fallback_result(
    task = task,
    sweep_error = sweep_result$error
  )
}

#' Build a road-chain fallback result
#'
#' @param task Assembled road mesh-chain task.
#' @param sweep_error Ordinary road-sweep error message.
#'
#' @return List containing the fallback mesh and captured errors.
#' @keywords internal
make_render_highquality_road_chain_fallback_result = function(
  task,
  sweep_error
) {
  fallback_result = tryCatch(
    list(
      mesh = make_render_highquality_buffered_road_chain_mesh(
        task = task,
        sweep_error = simpleError(sweep_error)
      ),
      error = NULL
    ),
    error = function(error) {
      list(mesh = NULL, error = conditionMessage(error))
    }
  )
  list(
    mesh = fallback_result$mesh,
    failed = !is.null(fallback_result$error),
    sweep_error = sweep_error,
    fallback_error = fallback_result$error
  )
}

#' Make a terrain-buffered fallback for a self-overlapping road sweep
#'
#' @param task Assembled road mesh-chain task.
#' @param sweep_error Error raised by the ordinary road sweep.
#'
#' @return Rayrender mesh object with fallback diagnostics.
#' @keywords internal
make_render_highquality_buffered_road_chain_mesh = function(
  task,
  sweep_error
) {
  if (!isTRUE(task$terrain_following)) {
    stop(
      "the chain has an absolute or layered elevation profile",
      call. = FALSE
    )
  }
  texture_file = task$texture_file
  has_texture = length(texture_file) &&
    !all(is.na(texture_file)) &&
    any(nzchar(texture_file))
  if (has_texture) {
    stop("the chain uses a road texture", call. = FALSE)
  }
  if (
    !is.null(task$material_sections) &&
      length(task$material_sections) > 1L
  ) {
    stop("the chain has multiple material sections", call. = FALSE)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("the `sf` package is unavailable", call. = FALSE)
  }
  fallback_task = task
  if (isTRUE(fallback_task$closed)) {
    fallback_task$points = rbind(
      fallback_task$points,
      fallback_task$points[1L, , drop = FALSE]
    )
  }
  road_surface_clearance = 0.055
  fallback_task$points[, 2] =
    fallback_task$points[, 2] + road_surface_clearance
  mesh = make_render_highquality_joined_water_path_mesh(
    tasks = list(fallback_task),
    seal_epsilon = 0,
    bottom_cap = TRUE,
    height = 0.11,
    extrusion_alignment = "below",
    return_mesh = isTRUE(task$return_mesh)
  )
  if (inherits(mesh, "mesh3d")) {
    attr(mesh, "render_road_mesh_specification") = list(
      rgl_id = task$rgl_id,
      roadcolor = task$roadcolor,
      texture_file = NULL,
      material = task$material,
      cap_start = TRUE,
      cap_end = TRUE,
      closed = isTRUE(task$closed)
    )
  }
  attr(mesh, "render_road_buffered_fallback") = list(
    used = TRUE,
    sweep_error = conditionMessage(sweep_error)
  )
  mesh
}

#' Resolve a road vertex join style
#'
#' @param incoming_tangent Incoming horizontal unit tangent.
#' @param outgoing_tangent Outgoing horizontal unit tangent.
#' @param miter_limit Maximum permitted miter scale.
#'
#' @return Join style, side direction, scale, and turn diagnostics.
#' @keywords internal
resolve_render_road_join_style = function(
  incoming_tangent,
  outgoing_tangent,
  miter_limit = 4
) {
  incoming_tangent = suppressWarnings(as.numeric(incoming_tangent[1:2]))
  outgoing_tangent = suppressWarnings(as.numeric(outgoing_tangent[1:2]))
  normalize = function(value) {
    magnitude = sqrt(sum(value^2))
    if (!is.finite(magnitude) || magnitude <= sqrt(.Machine$double.eps)) {
      return(c(NA_real_, NA_real_))
    }
    value / magnitude
  }
  incoming_tangent = normalize(incoming_tangent)
  outgoing_tangent = normalize(outgoing_tangent)
  incoming_side = c(-incoming_tangent[[2]], incoming_tangent[[1]])
  outgoing_side = c(-outgoing_tangent[[2]], outgoing_tangent[[1]])
  miter_side = normalize(incoming_side + outgoing_side)
  denominator = sum(miter_side * outgoing_side)
  miter_scale = 1 / denominator
  stable = all(is.finite(c(
    incoming_tangent,
    outgoing_tangent,
    miter_side,
    denominator,
    miter_scale
  ))) &&
    denominator > sqrt(.Machine$double.eps) &&
    miter_scale <= miter_limit
  list(
    style = if (stable) "miter" else "round",
    side_x = if (stable) miter_side[[1]] else outgoing_side[[1]],
    side_z = if (stable) miter_side[[2]] else outgoing_side[[2]],
    miter_scale = if (stable) miter_scale else NA_real_,
    turn_cross = incoming_tangent[[1]] *
      outgoing_tangent[[2]] -
      incoming_tangent[[2]] * outgoing_tangent[[1]],
    turn_dot = sum(incoming_tangent * outgoing_tangent)
  )
}

#' Calculate road path vertex frames
#'
#' @param points Road centerline points.
#' @param closed Whether the path is periodic.
#' @param miter_limit Maximum permitted miter scale.
#'
#' @return Per-vertex tangents, sides, scales, and join styles.
#' @keywords internal
calculate_render_road_vertex_frames = function(
  points,
  closed = FALSE,
  miter_limit = 4
) {
  points = as.matrix(points)
  point_count = nrow(points)
  closed = resolve_render_logical(closed, "closed")
  if (ncol(points) < 3L || point_count < if (closed) 3L else 2L) {
    stop("Road vertex frames require a valid path.", call. = FALSE)
  }
  calculate_render_road_vertex_frames_cpp(
    points = points,
    closed = closed,
    miter_limit = miter_limit
  )
}

#' Expand unstable road joins with short rounded sections
#'
#' @param points Road centerline points.
#' @param left_distance Left section distance at each point.
#' @param right_distance Right section distance at each point.
#' @param closed Whether the path is periodic.
#' @param miter_limit Maximum permitted miter scale.
#' @param round_join_segments Number of segments in each rounded fallback.
#'
#' @return Expanded points, section distances, and join diagnostics.
#' @keywords internal
expand_render_road_unstable_joins = function(
  points,
  left_distance,
  right_distance,
  closed = FALSE,
  miter_limit = 4,
  round_join_segments = 5L
) {
  points = as.matrix(points)
  point_count = nrow(points)
  frames = calculate_render_road_vertex_frames(
    points,
    closed = closed,
    miter_limit = miter_limit
  )
  round_join_segments = suppressWarnings(as.integer(round_join_segments[[1L]]))
  if (!is.finite(round_join_segments) || round_join_segments < 2L) {
    round_join_segments = 5L
  }
  point_rows = vector("list", point_count)
  left_rows = vector("list", point_count)
  right_rows = vector("list", point_count)
  inserted_count = integer(point_count)
  for (index in seq_len(point_count)) {
    if (frames$join_style[[index]] != "round") {
      point_rows[[index]] = matrix(points[index, ], nrow = 1L)
      left_rows[[index]] = left_distance[[index]]
      right_rows[[index]] = right_distance[[index]]
      inserted_count[[index]] = 1L
      next
    }
    previous_index = if (index == 1L) point_count else index - 1L
    next_index = if (index == point_count) 1L else index + 1L
    incoming_length = sqrt(sum(
      (points[index, c(1, 3)] - points[previous_index, c(1, 3)])^2
    ))
    outgoing_length = sqrt(sum(
      (points[next_index, c(1, 3)] - points[index, c(1, 3)])^2
    ))
    maximum_distance = max(
      left_distance[[index]],
      right_distance[[index]]
    )
    turn_angle = acos(max(-1, min(1, frames$turn_dot[[index]])))
    turn_sign = sign(frames$turn_cross[[index]])
    target_radius = maximum_distance * 1.25
    required_setback = target_radius * tan(turn_angle / 2)
    setback = min(
      required_setback,
      incoming_length * 0.8,
      outgoing_length * 0.8
    )
    if (!is.finite(setback) || setback <= sqrt(.Machine$double.eps)) {
      stop("An unstable road join cannot be rounded safely.", call. = FALSE)
    }
    radius = setback / tan(turn_angle / 2)
    if (
      !is.finite(radius) ||
        radius <= maximum_distance * (1 + sqrt(.Machine$double.eps)) ||
        turn_sign == 0
    ) {
      stop(
        "An unstable road join has insufficient length for a safe round join.",
        call. = FALSE
      )
    }
    incoming = frames$incoming_tangent[index, ]
    outgoing = frames$outgoing_tangent[index, ]
    start = points[index, ]
    start[c(1, 3)] = start[c(1, 3)] - incoming * setback
    start[[2L]] = points[index, 2L] +
      (points[previous_index, 2L] - points[index, 2L]) *
        setback /
        incoming_length
    end = points[index, ]
    end[c(1, 3)] = end[c(1, 3)] + outgoing * setback
    end[[2L]] = points[index, 2L] +
      (points[next_index, 2L] - points[index, 2L]) *
        setback /
        outgoing_length
    fraction = seq(0, 1, length.out = round_join_segments + 1L)
    incoming_side = c(-incoming[[2]], incoming[[1]])
    center_xz = start[c(1, 3)] +
      incoming_side * turn_sign * radius
    start_angle = atan2(
      start[[3L]] - center_xz[[2L]],
      start[[1L]] - center_xz[[1L]]
    )
    curve_angle = start_angle + fraction * turn_sign * turn_angle
    curve = cbind(
      center_xz[[1L]] + radius * cos(curve_angle),
      start[[2L]] + (end[[2L]] - start[[2L]]) * fraction,
      center_xz[[2L]] + radius * sin(curve_angle)
    )
    point_rows[[index]] = curve
    left_rows[[index]] = rep(
      left_distance[[index]],
      nrow(curve)
    )
    right_rows[[index]] = rep(
      right_distance[[index]],
      nrow(curve)
    )
    inserted_count[[index]] = nrow(curve)
  }
  list(
    points = do.call(rbind, point_rows),
    left_distance = unlist(left_rows, use.names = FALSE),
    right_distance = unlist(right_rows, use.names = FALSE),
    diagnostics = data.frame(
      source_vertex = seq_len(point_count),
      join_style = frames$join_style,
      miter_scale = frames$miter_scale,
      inserted_section_count = inserted_count,
      stringsAsFactors = FALSE
    )
  )
}

#' Calculate road vertex cross-sections
#'
#' @param points Road centerline points.
#' @param left_distance Left distance from the centerline.
#' @param right_distance Right distance from the centerline.
#' @param heightmap Cached heightmap.
#' @param zscale Effective zscale.
#' @param closed Whether the path is periodic.
#' @param miter_limit Maximum permitted miter scale.
#' @param frames Default `NULL`. Optional precomputed vertex frames.
#'
#' @return Bottom and top edge positions, normals, and vertex frames.
#' @keywords internal
calculate_render_road_vertex_sections = function(
  points,
  left_distance,
  right_distance,
  heightmap = NULL,
  zscale = 1,
  closed = FALSE,
  miter_limit = 4,
  frames = NULL
) {
  points = as.matrix(points)
  if (is.null(frames)) {
    frames = calculate_render_road_vertex_frames(
      points,
      closed = closed,
      miter_limit = miter_limit
    )
  }
  if (
    !is.list(frames) ||
      !is.matrix(frames$side) ||
      nrow(frames$side) != nrow(points) ||
      length(frames$miter_scale) != nrow(points)
  ) {
    stop("Road vertex frames do not match the centerline.", call. = FALSE)
  }
  if (any(frames$join_style == "round")) {
    stop(
      "Road join expansion left an unresolved unstable vertex.",
      call. = FALSE
    )
  }
  terrain_following = !is.null(heightmap) && is.matrix(heightmap)
  terrain_heightmap = if (terrain_following) {
    heightmap
  } else {
    matrix(numeric(), nrow = 0L, ncol = 0L)
  }
  section_data = sample_render_road_sections_batch_cpp(
    input_jobs = list(list(
      points = points,
      left_distance = left_distance,
      right_distance = right_distance,
      side = frames$side,
      miter_scale = frames$miter_scale,
      terrain_following = terrain_following
    )),
    heightmap = terrain_heightmap,
    zscale = zscale,
    parallel = FALSE,
    verbose = FALSE
  )[[1L]]
  c(
    list(
      points = points,
      frames = frames
    ),
    section_data
  )
}

#' Resolve one shared road-surface normal
#'
#' @param face_normals Incident area-weighted face normals.
#' @param fallback Fallback outward normal.
#' @param minimum_dot Default `1e-8`. Minimum positive dot product with each
#' incident face.
#'
#' @return A unit normal in the geometric hemisphere of every incident face.
#' @keywords internal
resolve_render_road_shared_surface_normal = function(
  face_normals,
  fallback,
  minimum_dot = 1e-8
) {
  face_normals = as.matrix(face_normals)
  fallback = suppressWarnings(as.numeric(fallback[1:3]))
  if (!nrow(face_normals) || ncol(face_normals) != 3L) {
    return(fallback)
  }
  face_length = sqrt(rowSums(face_normals^2))
  valid = stats::complete.cases(face_normals) &
    is.finite(face_length) &
    face_length > sqrt(.Machine$double.eps)
  face_normals = face_normals[valid, , drop = FALSE]
  face_length = face_length[valid]
  if (!nrow(face_normals)) {
    return(fallback)
  }
  unit_face = face_normals / face_length
  candidate = colSums(face_normals)
  candidate_length = sqrt(sum(candidate^2))
  if (
    !is.finite(candidate_length) ||
      candidate_length <= sqrt(.Machine$double.eps)
  ) {
    candidate = colSums(unit_face)
    candidate_length = sqrt(sum(candidate^2))
  }
  if (
    !is.finite(candidate_length) ||
      candidate_length <= sqrt(.Machine$double.eps)
  ) {
    candidate = fallback
  } else {
    candidate = candidate / candidate_length
  }
  for (iteration in seq_len(32L)) {
    dot = as.vector(unit_face %*% candidate)
    violation = which(!is.finite(dot) | dot < minimum_dot)
    if (!length(violation)) {
      break
    }
    for (face_index in violation) {
      face_dot = sum(unit_face[face_index, ] * candidate)
      if (!is.finite(face_dot)) {
        next
      }
      candidate = candidate +
        (minimum_dot - face_dot) * unit_face[face_index, ]
    }
  }
  candidate_length = sqrt(sum(candidate^2))
  if (
    !is.finite(candidate_length) ||
      candidate_length <= sqrt(.Machine$double.eps)
  ) {
    candidate = fallback
  } else {
    candidate = candidate / candidate_length
  }
  final_dot = as.vector(unit_face %*% candidate)
  if (any(final_dot <= 0)) {
    stop(
      sprintf(
        paste0(
          "A road surface vertex has no common outward shading hemisphere ",
          "(minimum face dot %.6g)."
        ),
        min(final_dot)
      ),
      call. = FALSE
    )
  }
  candidate
}

#' Calculate smooth normals for a ruled road surface in R
#'
#' @param left_vertices Left boundary vertices ordered by road station.
#' @param right_vertices Right boundary vertices ordered by road station.
#' @param closed Default `FALSE`. Whether the surface is periodic.
#' @param outward_sign Default `1`. Direction relative to the top-surface
#' winding.
#'
#' @return Left and right area-weighted vertex normals derived from the final
#' surface geometry.
#' @keywords internal
calculate_render_road_surface_normals_reference = function(
  left_vertices,
  right_vertices,
  closed = FALSE,
  outward_sign = 1
) {
  left_vertices = as.matrix(left_vertices)
  right_vertices = as.matrix(right_vertices)
  closed = resolve_render_logical(closed, "closed")
  outward_sign = suppressWarnings(as.numeric(outward_sign[[1L]]))
  if (
    nrow(left_vertices) != nrow(right_vertices) ||
      ncol(left_vertices) != 3L ||
      ncol(right_vertices) != 3L ||
      nrow(left_vertices) < if (closed) 3L else 2L
  ) {
    stop("Road surface boundaries do not define a valid strip.", call. = FALSE)
  }
  if (!is.finite(outward_sign) || outward_sign == 0) {
    stop("`outward_sign` must be finite and nonzero.", call. = FALSE)
  }
  point_count = nrow(left_vertices)
  segment_index = if (closed) {
    seq_len(point_count)
  } else {
    seq_len(point_count - 1L)
  }
  next_index = if (closed) {
    c(seq.int(2L, point_count), 1L)
  } else {
    seq.int(2L, point_count)
  }
  first_face = row_cross(
    left_vertices[next_index, , drop = FALSE] -
      left_vertices[segment_index, , drop = FALSE],
    right_vertices[next_index, , drop = FALSE] -
      left_vertices[segment_index, , drop = FALSE]
  )
  second_face = row_cross(
    right_vertices[next_index, , drop = FALSE] -
      left_vertices[segment_index, , drop = FALSE],
    right_vertices[segment_index, , drop = FALSE] -
      left_vertices[segment_index, , drop = FALSE]
  )
  first_face = first_face * outward_sign
  second_face = second_face * outward_sign
  left_face = vector("list", point_count)
  right_face = vector("list", point_count)
  for (segment_row in seq_along(segment_index)) {
    current = segment_index[[segment_row]]
    following = next_index[[segment_row]]
    first = matrix(first_face[segment_row, ], nrow = 1L)
    second = matrix(second_face[segment_row, ], nrow = 1L)
    left_face[[current]] = rbind(left_face[[current]], first, second)
    left_face[[following]] = rbind(left_face[[following]], first)
    right_face[[following]] = rbind(right_face[[following]], first, second)
    right_face[[current]] = rbind(right_face[[current]], second)
  }
  fallback = c(0, sign(outward_sign), 0)
  left_normal = t(vapply(
    left_face,
    resolve_render_road_shared_surface_normal,
    numeric(3),
    fallback = fallback
  ))
  right_normal = t(vapply(
    right_face,
    resolve_render_road_shared_surface_normal,
    numeric(3),
    fallback = fallback
  ))
  list(
    left = left_normal,
    right = right_normal,
    first_face = first_face,
    second_face = second_face
  )
}

#' Calculate smooth normals for a ruled road surface
#'
#' @param left_vertices Left boundary vertices ordered by road station.
#' @param right_vertices Right boundary vertices ordered by road station.
#' @param closed Default `FALSE`. Whether the surface is periodic.
#' @param outward_sign Default `1`. Direction relative to the top-surface
#' winding.
#' @param parallel Default `FALSE`. Whether to use multiple native threads.
#'
#' @return Left and right area-weighted vertex normals derived from the final
#' surface geometry.
#' @keywords internal
calculate_render_road_surface_normals = function(
  left_vertices,
  right_vertices,
  closed = FALSE,
  outward_sign = 1,
  parallel = FALSE
) {
  left_vertices = as.matrix(left_vertices)
  right_vertices = as.matrix(right_vertices)
  closed = resolve_render_logical(closed, "closed")
  parallel = resolve_render_logical(parallel, "parallel")
  outward_sign = suppressWarnings(as.numeric(outward_sign[[1L]]))
  if (
    nrow(left_vertices) != nrow(right_vertices) ||
      ncol(left_vertices) != 3L ||
      ncol(right_vertices) != 3L ||
      nrow(left_vertices) < if (closed) 3L else 2L
  ) {
    stop("Road surface boundaries do not define a valid strip.", call. = FALSE)
  }
  if (!is.finite(outward_sign) || outward_sign == 0) {
    stop("`outward_sign` must be finite and nonzero.", call. = FALSE)
  }
  calculate_render_road_surface_normals_cpp(
    left_vertices = left_vertices,
    right_vertices = right_vertices,
    closed = closed,
    outward_sign = outward_sign,
    parallel = parallel
  )
}

#' Identify inverted road-strip segments
#'
#' @param left_vertices Left surface boundary.
#' @param right_vertices Right surface boundary.
#' @param closed Whether the strip is periodic.
#' @param tolerance Default `1e-12`. Minimum upward projected triangle area.
#'
#' @return Segment indices containing an inverted or degenerate triangle.
#' @keywords internal
identify_render_road_inverted_surface_segments = function(
  left_vertices,
  right_vertices,
  closed = FALSE,
  tolerance = 1e-12
) {
  left_vertices = as.matrix(left_vertices)
  right_vertices = as.matrix(right_vertices)
  closed = resolve_render_logical(closed, "closed")
  point_count = nrow(left_vertices)
  segment_index = if (closed) {
    seq_len(point_count)
  } else {
    seq_len(point_count - 1L)
  }
  next_index = if (closed) {
    c(seq.int(2L, point_count), 1L)
  } else {
    seq.int(2L, point_count)
  }
  first_face = row_cross(
    left_vertices[next_index, , drop = FALSE] -
      left_vertices[segment_index, , drop = FALSE],
    right_vertices[next_index, , drop = FALSE] -
      left_vertices[segment_index, , drop = FALSE]
  )
  second_face = row_cross(
    right_vertices[next_index, , drop = FALSE] -
      left_vertices[segment_index, , drop = FALSE],
    right_vertices[segment_index, , drop = FALSE] -
      left_vertices[segment_index, , drop = FALSE]
  )
  invalid = !stats::complete.cases(first_face) |
    !stats::complete.cases(second_face) |
    first_face[, 2] <= tolerance |
    second_face[, 2] <= tolerance
  segment_index[invalid]
}

#' Calculate stabilized road sweep frames
#'
#' @param points Dense road centerline points.
#' @param left_distance Left section distance at each point.
#' @param right_distance Right section distance at each point.
#' @param texture_world_scale Scene-to-world horizontal scale.
#' @param closed Whether the path is periodic.
#' @param miter_limit Maximum permitted miter scale.
#' @param guide_step_fraction Default `0.2`. Guide spacing relative to the
#' maximum half-width.
#'
#' @return Dense vertex frames interpolated from a width-scale guide path.
#' @keywords internal
calculate_render_road_stabilized_vertex_frames = function(
  points,
  left_distance,
  right_distance,
  texture_world_scale,
  closed = FALSE,
  miter_limit = 4,
  guide_step_fraction = 0.2
) {
  points = as.matrix(points)
  point_count = nrow(points)
  guide_step_fraction = resolve_render_positive_number(
    guide_step_fraction,
    "guide_step_fraction"
  )
  station = calculate_road_path_cumulative_distance(
    points,
    texture_world_scale = texture_world_scale
  )
  total_length = utils::tail(station, 1L)
  if (closed) {
    closing_delta = (points[1L, c(1, 3)] -
      points[point_count, c(1, 3)]) *
      texture_world_scale
    total_length = total_length + sqrt(sum(closing_delta^2))
  }
  minimum_guide_step = max(
    1e-3,
    guide_step_fraction *
      max(c(left_distance, right_distance)) *
      mean(texture_world_scale)
  )
  keep = rep(FALSE, point_count)
  keep[[1L]] = TRUE
  previous = 1L
  if (point_count > 2L) {
    for (index in seq.int(2L, point_count - 1L)) {
      if (station[[index]] - station[[previous]] >= minimum_guide_step) {
        keep[[index]] = TRUE
        previous = index
      }
    }
  }
  keep[[point_count]] = TRUE
  guide_index = which(keep)
  if (
    closed &&
      length(guide_index) > 3L &&
      total_length - station[[utils::tail(guide_index, 1L)]] <
        minimum_guide_step
  ) {
    guide_index = guide_index[-length(guide_index)]
  }
  if (length(guide_index) < if (closed) 3L else 2L) {
    stop(
      "Road sweep stabilization has insufficient guide points.",
      call. = FALSE
    )
  }
  guide_frames = calculate_render_road_vertex_frames(
    points[guide_index, , drop = FALSE],
    closed = closed,
    miter_limit = miter_limit
  )
  if (any(guide_frames$join_style == "round")) {
    stop(
      "Road sweep stabilization encountered an unresolved sharp guide join.",
      call. = FALSE
    )
  }
  guide_station = station[guide_index]
  guide_angle = atan2(
    guide_frames$side[, 2],
    guide_frames$side[, 1]
  )
  if (length(guide_angle) > 1L) {
    for (index in seq.int(2L, length(guide_angle))) {
      angle_delta = guide_angle[[index]] - guide_angle[[index - 1L]]
      while (angle_delta > pi) {
        guide_angle[[index]] = guide_angle[[index]] - 2 * pi
        angle_delta = guide_angle[[index]] - guide_angle[[index - 1L]]
      }
      while (angle_delta < -pi) {
        guide_angle[[index]] = guide_angle[[index]] + 2 * pi
        angle_delta = guide_angle[[index]] - guide_angle[[index - 1L]]
      }
    }
  }
  if (closed) {
    closing_angle = guide_angle[[1L]]
    while (closing_angle - utils::tail(guide_angle, 1L) > pi) {
      closing_angle = closing_angle - 2 * pi
    }
    while (closing_angle - utils::tail(guide_angle, 1L) < -pi) {
      closing_angle = closing_angle + 2 * pi
    }
    guide_station = c(guide_station, total_length)
    guide_angle = c(guide_angle, closing_angle)
  }
  dense_angle = stats::approx(
    guide_station,
    guide_angle,
    xout = station,
    rule = 2
  )$y
  frames = calculate_render_road_vertex_frames(
    points,
    closed = closed,
    miter_limit = miter_limit
  )
  frames$side = cbind(cos(dense_angle), sin(dense_angle))
  # The interpolated side is a smooth unit cross-section normal, not the
  # exact bisector of either adjacent segment. Applying the guide-path miter
  # scale to it overextends tight bends and can invert an otherwise valid
  # strip. Unit scale preserves the requested width while the interpolated
  # direction supplies the stabilization.
  frames$miter_scale = rep(1, point_count)
  stabilize = frames$join_style != "endpoint"
  frames$join_style[stabilize] = "stabilized"
  attr(frames, "render_road_stabilization") = list(
    guide_index = guide_index,
    minimum_guide_step = minimum_guide_step
  )
  frames
}

#' Sanitize a road section mesh
#'
#' @param vertices Quad vertices.
#' @param vertex_normals Quad shading normals.
#' @param texcoords Quad texture coordinates.
#' @param geometry_tolerance Minimum triangle area.
#' @param uv_tolerance Minimum texture triangle area.
#'
#' @return Sanitized quad arrays and diagnostics.
#' @keywords internal
sanitize_render_road_section_mesh = function(
  vertices,
  vertex_normals,
  texcoords,
  geometry_tolerance = 1e-12,
  uv_tolerance = 1e-14
) {
  quad_start = seq(1L, nrow(vertices), by = 4L)
  first_area = sqrt(rowSums(
    row_cross(
      vertices[quad_start + 1L, , drop = FALSE] -
        vertices[quad_start, , drop = FALSE],
      vertices[quad_start + 2L, , drop = FALSE] -
        vertices[quad_start, , drop = FALSE]
    )^2
  )) /
    2
  second_area = sqrt(rowSums(
    row_cross(
      vertices[quad_start + 2L, , drop = FALSE] -
        vertices[quad_start, , drop = FALSE],
      vertices[quad_start + 3L, , drop = FALSE] -
        vertices[quad_start, , drop = FALSE]
    )^2
  )) /
    2
  uv_cross = function(first, second) {
    abs(first[, 1] * second[, 2] - first[, 2] * second[, 1]) / 2
  }
  first_uv_area = uv_cross(
    texcoords[quad_start + 1L, , drop = FALSE] -
      texcoords[quad_start, , drop = FALSE],
    texcoords[quad_start + 2L, , drop = FALSE] -
      texcoords[quad_start, , drop = FALSE]
  )
  second_uv_area = uv_cross(
    texcoords[quad_start + 2L, , drop = FALSE] -
      texcoords[quad_start, , drop = FALSE],
    texcoords[quad_start + 3L, , drop = FALSE] -
      texcoords[quad_start, , drop = FALSE]
  )
  finite_quad = vapply(
    seq_along(quad_start),
    function(index) {
      rows = seq.int(quad_start[[index]], quad_start[[index]] + 3L)
      all(is.finite(vertices[rows, ])) &&
        all(is.finite(vertex_normals[rows, ])) &&
        all(is.finite(texcoords[rows, ]))
    },
    logical(1)
  )
  keep_quad = finite_quad &
    first_area > geometry_tolerance &
    second_area > geometry_tolerance &
    first_uv_area > uv_tolerance &
    second_uv_area > uv_tolerance
  keep_rows = unlist(lapply(
    quad_start[keep_quad],
    function(start) seq.int(start, start + 3L)
  ))
  geometry_area = c(first_area[keep_quad], second_area[keep_quad])
  uv_area = c(first_uv_area[keep_quad], second_uv_area[keep_quad])
  list(
    vertices = vertices[keep_rows, , drop = FALSE],
    vertex_normals = vertex_normals[keep_rows, , drop = FALSE],
    texcoords = texcoords[keep_rows, , drop = FALSE],
    diagnostics = list(
      input_quad_count = length(quad_start),
      retained_quad_count = sum(keep_quad),
      removed_quad_count = sum(!keep_quad),
      non_finite_quad_count = sum(!finite_quad),
      minimum_triangle_area = if (length(geometry_area)) {
        min(geometry_area)
      } else {
        NA_real_
      },
      minimum_uv_triangle_area = if (length(uv_area)) {
        min(uv_area)
      } else {
        NA_real_
      }
    )
  )
}

#' Build a road mesh from shared vertex sections in R
#'
#' @param sections Road vertex sections.
#' @param station Global station at each section.
#' @param total_length Total open or periodic path length.
#' @param bbox_center Scene center.
#' @param texture_file Default `NULL`. Road texture file.
#' @param texture_length Default `20`. Texture repeat length.
#' @param texture_repeats Default `NULL`. Number of texture repeats.
#' @param surface_normals Default `NULL`. Optional top and bottom surface
#' normals calculated over the complete physical mesh chain.
#' @param cap_start Default `TRUE`. Whether to cap the first section.
#' @param cap_end Default `TRUE`. Whether to cap the final section.
#' @param closed Default `FALSE`. Whether the path is periodic.
#'
#' @return A raw `mesh3d` object with mesh diagnostics.
#' @keywords internal
build_render_road_section_mesh_reference = function(
  sections,
  station,
  total_length,
  bbox_center,
  texture_file = NULL,
  texture_length = 20,
  texture_repeats = NULL,
  surface_normals = NULL,
  cap_start = TRUE,
  cap_end = TRUE,
  closed = FALSE
) {
  point_count = nrow(sections$points)
  segment_indices = if (closed) {
    seq_len(point_count)
  } else {
    seq_len(point_count - 1L)
  }
  next_indices = if (closed) {
    c(seq.int(2L, point_count), 1L)
  } else {
    seq.int(2L, point_count)
  }
  texture_length = resolve_render_positive_number(
    texture_length,
    "lane_texture_length"
  )
  texture_repeats = suppressWarnings(as.numeric(texture_repeats[1]))
  if (
    length(texture_repeats) &&
      is.finite(texture_repeats) &&
      texture_repeats > 0 &&
      is.finite(total_length) &&
      total_length > 0
  ) {
    texture_v = station / total_length * texture_repeats
    closing_v = texture_repeats
  } else if (closed) {
    closed_texture_repeats = max(
      1,
      round(total_length / texture_length)
    )
    texture_v = station / total_length * closed_texture_repeats
    closing_v = closed_texture_repeats
  } else {
    texture_v = station / texture_length
    closing_v = total_length / texture_length
  }
  v0 = texture_v[segment_indices]
  v1 = texture_v[next_indices]
  if (closed) {
    v1[[length(v1)]] = closing_v
  }
  top_vertices = make_render_highquality_quad_rows(
    sections$left_top[segment_indices, , drop = FALSE],
    sections$left_top[next_indices, , drop = FALSE],
    sections$right_top[next_indices, , drop = FALSE],
    sections$right_top[segment_indices, , drop = FALSE]
  )
  bottom_vertices = make_render_highquality_quad_rows(
    sections$left_bottom[segment_indices, , drop = FALSE],
    sections$right_bottom[segment_indices, , drop = FALSE],
    sections$right_bottom[next_indices, , drop = FALSE],
    sections$left_bottom[next_indices, , drop = FALSE]
  )
  left_vertices = make_render_highquality_quad_rows(
    sections$left_bottom[segment_indices, , drop = FALSE],
    sections$left_bottom[next_indices, , drop = FALSE],
    sections$left_top[next_indices, , drop = FALSE],
    sections$left_top[segment_indices, , drop = FALSE]
  )
  right_vertices = make_render_highquality_quad_rows(
    sections$right_bottom[segment_indices, , drop = FALSE],
    sections$right_top[segment_indices, , drop = FALSE],
    sections$right_top[next_indices, , drop = FALSE],
    sections$right_bottom[next_indices, , drop = FALSE]
  )
  if (is.null(surface_normals)) {
    top_surface_normals = calculate_render_road_surface_normals_reference(
      sections$left_top,
      sections$right_top,
      closed = closed
    )
    bottom_surface_normals = calculate_render_road_surface_normals_reference(
      sections$left_bottom,
      sections$right_bottom,
      closed = closed,
      outward_sign = -1
    )
  } else {
    top_surface_normals = surface_normals$top
    bottom_surface_normals = surface_normals$bottom
    valid_surface_normals =
      is.list(top_surface_normals) &&
      is.list(bottom_surface_normals) &&
      is.matrix(top_surface_normals$left) &&
      is.matrix(top_surface_normals$right) &&
      is.matrix(bottom_surface_normals$left) &&
      is.matrix(bottom_surface_normals$right) &&
      nrow(top_surface_normals$left) == point_count &&
      nrow(top_surface_normals$right) == point_count &&
      nrow(bottom_surface_normals$left) == point_count &&
      nrow(bottom_surface_normals$right) == point_count
    if (!valid_surface_normals) {
      stop(
        "Shared road-surface normals do not match the material section.",
        call. = FALSE
      )
    }
  }
  top_normals = make_render_highquality_quad_rows(
    top_surface_normals$left[segment_indices, , drop = FALSE],
    top_surface_normals$left[next_indices, , drop = FALSE],
    top_surface_normals$right[next_indices, , drop = FALSE],
    top_surface_normals$right[segment_indices, , drop = FALSE]
  )
  bottom_normals = make_render_highquality_quad_rows(
    bottom_surface_normals$left[segment_indices, , drop = FALSE],
    bottom_surface_normals$right[segment_indices, , drop = FALSE],
    bottom_surface_normals$right[next_indices, , drop = FALSE],
    bottom_surface_normals$left[next_indices, , drop = FALSE]
  )
  left_forward = sections$left_bottom[next_indices, , drop = FALSE] -
    sections$left_bottom[segment_indices, , drop = FALSE]
  left_up = sections$left_top[segment_indices, , drop = FALSE] -
    sections$left_bottom[segment_indices, , drop = FALSE]
  left_wall_normal = replace_invalid_render_highquality_vectors(
    normalize_render_highquality_rows(row_cross(left_forward, left_up)),
    fallback = c(0, 0, 1)
  )
  right_forward = sections$right_bottom[next_indices, , drop = FALSE] -
    sections$right_bottom[segment_indices, , drop = FALSE]
  right_up = sections$right_top[segment_indices, , drop = FALSE] -
    sections$right_bottom[segment_indices, , drop = FALSE]
  right_wall_normal = replace_invalid_render_highquality_vectors(
    normalize_render_highquality_rows(row_cross(right_up, right_forward)),
    fallback = c(0, 0, -1)
  )
  left_normals = make_render_highquality_quad_rows(
    left_wall_normal,
    left_wall_normal,
    left_wall_normal,
    left_wall_normal
  )
  right_normals = make_render_highquality_quad_rows(
    right_wall_normal,
    right_wall_normal,
    right_wall_normal,
    right_wall_normal
  )
  top_texcoords = make_render_highquality_quad_rows(
    cbind(0, v0),
    cbind(0, v1),
    cbind(1, v1),
    cbind(1, v0)
  )
  bottom_texcoords = make_render_highquality_quad_rows(
    cbind(0, v0),
    cbind(1, v0),
    cbind(1, v1),
    cbind(0, v1)
  )
  side_texture_u = c(0.01, 0.02)
  left_texcoords = make_render_highquality_quad_rows(
    cbind(side_texture_u[[1]], v0),
    cbind(side_texture_u[[1]], v1),
    cbind(side_texture_u[[2]], v1),
    cbind(side_texture_u[[2]], v0)
  )
  right_texcoords = make_render_highquality_quad_rows(
    cbind(side_texture_u[[1]], v0),
    cbind(side_texture_u[[2]], v0),
    cbind(side_texture_u[[2]], v1),
    cbind(side_texture_u[[1]], v1)
  )
  vertices = rbind(
    top_vertices,
    bottom_vertices,
    left_vertices,
    right_vertices
  )
  vertex_normals = rbind(
    top_normals,
    bottom_normals,
    left_normals,
    right_normals
  )
  texcoords = rbind(
    top_texcoords,
    bottom_texcoords,
    left_texcoords,
    right_texcoords
  )
  cap_texture_v_span = 1e-4
  if (!closed && isTRUE(cap_start)) {
    start_tangent = sections$frames$outgoing_tangent[1L, ]
    cap_normal = matrix(
      c(-start_tangent[[1]], 0, -start_tangent[[2]]),
      nrow = 1L
    )
    vertices = rbind(
      vertices,
      make_render_highquality_quad_rows(
        matrix(sections$left_bottom[1L, ], nrow = 1L),
        matrix(sections$left_top[1L, ], nrow = 1L),
        matrix(sections$right_top[1L, ], nrow = 1L),
        matrix(sections$right_bottom[1L, ], nrow = 1L)
      )
    )
    vertex_normals = rbind(
      vertex_normals,
      make_render_highquality_quad_rows(
        cap_normal,
        cap_normal,
        cap_normal,
        cap_normal
      )
    )
    start_v = texture_v[[1L]]
    texcoords = rbind(
      texcoords,
      matrix(
        c(
          side_texture_u[[1]],
          start_v,
          side_texture_u[[1]],
          start_v + cap_texture_v_span,
          side_texture_u[[2]],
          start_v + cap_texture_v_span,
          side_texture_u[[2]],
          start_v
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  }
  if (!closed && isTRUE(cap_end)) {
    end_tangent = sections$frames$incoming_tangent[point_count, ]
    cap_normal = matrix(
      c(end_tangent[[1]], 0, end_tangent[[2]]),
      nrow = 1L
    )
    vertices = rbind(
      vertices,
      make_render_highquality_quad_rows(
        matrix(sections$left_bottom[point_count, ], nrow = 1L),
        matrix(sections$right_bottom[point_count, ], nrow = 1L),
        matrix(sections$right_top[point_count, ], nrow = 1L),
        matrix(sections$left_top[point_count, ], nrow = 1L)
      )
    )
    vertex_normals = rbind(
      vertex_normals,
      make_render_highquality_quad_rows(
        cap_normal,
        cap_normal,
        cap_normal,
        cap_normal
      )
    )
    end_v = texture_v[[point_count]]
    texcoords = rbind(
      texcoords,
      matrix(
        c(
          side_texture_u[[1]],
          end_v,
          side_texture_u[[2]],
          end_v,
          side_texture_u[[2]],
          end_v - cap_texture_v_span,
          side_texture_u[[1]],
          end_v - cap_texture_v_span
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  }
  vertex_normals = sanitize_render_highquality_road_quad_normals(
    vertices,
    vertex_normals
  )
  sanitized = sanitize_render_road_section_mesh(
    vertices,
    vertex_normals,
    texcoords
  )
  if (!nrow(sanitized$vertices)) {
    return(NULL)
  }
  vertices = sweep(
    sanitized$vertices,
    2,
    bbox_center,
    FUN = "-"
  )
  quad_starts = seq(1L, nrow(vertices), by = 4L)
  indices = rbind(
    cbind(quad_starts, quad_starts + 1L, quad_starts + 2L),
    cbind(quad_starts, quad_starts + 2L, quad_starts + 3L)
  )
  mesh = list(
    vb = t(cbind(vertices, 1)),
    it = t(indices),
    normals = t(sanitized$vertex_normals),
    texcoords = t(sanitized$texcoords),
    material = list(texture = texture_file, bump_texture = NULL, color = NULL)
  )
  class(mesh) = "mesh3d"
  attr(mesh, "render_road_mesh_diagnostics") = sanitized$diagnostics
  mesh
}

#' Build a road mesh from shared vertex sections
#'
#' @param sections Road vertex sections.
#' @param station Global station at each section.
#' @param total_length Total open or periodic path length.
#' @param bbox_center Scene center.
#' @param texture_file Default `NULL`. Road texture file.
#' @param texture_length Default `20`. Texture repeat length.
#' @param texture_repeats Default `NULL`. Number of texture repeats.
#' @param surface_normals Default `NULL`. Optional top and bottom surface
#' normals calculated over the complete physical mesh chain.
#' @param cap_start Default `TRUE`. Whether to cap the first section.
#' @param cap_end Default `TRUE`. Whether to cap the final section.
#' @param closed Default `FALSE`. Whether the path is periodic.
#' @param parallel Default `FALSE`. Whether to use multiple native threads.
#'
#' @return A raw `mesh3d` object with mesh diagnostics.
#' @keywords internal
build_render_road_section_mesh = function(
  sections,
  station,
  total_length,
  bbox_center,
  texture_file = NULL,
  texture_length = 20,
  texture_repeats = NULL,
  surface_normals = NULL,
  cap_start = TRUE,
  cap_end = TRUE,
  closed = FALSE,
  parallel = FALSE
) {
  point_count = nrow(sections$points)
  closed = resolve_render_logical(closed, "closed")
  cap_start = resolve_render_logical(cap_start, "cap_start")
  cap_end = resolve_render_logical(cap_end, "cap_end")
  parallel = resolve_render_logical(parallel, "parallel")
  texture_length = resolve_render_positive_number(
    texture_length,
    "lane_texture_length"
  )
  texture_repeats = suppressWarnings(as.numeric(texture_repeats[1]))
  if (
    length(texture_repeats) &&
      is.finite(texture_repeats) &&
      texture_repeats > 0 &&
      is.finite(total_length) &&
      total_length > 0
  ) {
    texture_v = station / total_length * texture_repeats
    closing_v = texture_repeats
  } else if (closed) {
    closed_texture_repeats = max(
      1,
      round(total_length / texture_length)
    )
    texture_v = station / total_length * closed_texture_repeats
    closing_v = closed_texture_repeats
  } else {
    texture_v = station / texture_length
    closing_v = total_length / texture_length
  }
  if (is.null(surface_normals)) {
    top_surface_normals = calculate_render_road_surface_normals(
      sections$left_top,
      sections$right_top,
      closed = closed,
      parallel = parallel
    )
    bottom_surface_normals = calculate_render_road_surface_normals(
      sections$left_bottom,
      sections$right_bottom,
      closed = closed,
      outward_sign = -1,
      parallel = parallel
    )
  } else {
    top_surface_normals = surface_normals$top
    bottom_surface_normals = surface_normals$bottom
    valid_surface_normals =
      is.list(top_surface_normals) &&
      is.list(bottom_surface_normals) &&
      is.matrix(top_surface_normals$left) &&
      is.matrix(top_surface_normals$right) &&
      is.matrix(bottom_surface_normals$left) &&
      is.matrix(bottom_surface_normals$right) &&
      nrow(top_surface_normals$left) == point_count &&
      nrow(top_surface_normals$right) == point_count &&
      nrow(bottom_surface_normals$left) == point_count &&
      nrow(bottom_surface_normals$right) == point_count
    if (!valid_surface_normals) {
      stop(
        "Shared road-surface normals do not match the material section.",
        call. = FALSE
      )
    }
  }
  mesh_data = build_render_road_section_mesh_cpp(
    left_bottom_matrix = sections$left_bottom,
    right_bottom_matrix = sections$right_bottom,
    left_top_matrix = sections$left_top,
    right_top_matrix = sections$right_top,
    incoming_tangent = sections$frames$incoming_tangent,
    outgoing_tangent = sections$frames$outgoing_tangent,
    texture_v = texture_v,
    closing_v = closing_v,
    bbox_center = bbox_center,
    top_left_normal_matrix = top_surface_normals$left,
    top_right_normal_matrix = top_surface_normals$right,
    bottom_left_normal_matrix = bottom_surface_normals$left,
    bottom_right_normal_matrix = bottom_surface_normals$right,
    cap_start = cap_start,
    cap_end = cap_end,
    closed = closed,
    parallel = parallel
  )
  if (!nrow(mesh_data$vertices)) {
    return(NULL)
  }
  colnames(mesh_data$indices) = c("quad_starts", "", "")
  mesh = list(
    vb = t(cbind(mesh_data$vertices, 1)),
    it = t(mesh_data$indices),
    normals = t(mesh_data$vertex_normals),
    texcoords = t(mesh_data$texcoords),
    material = list(texture = texture_file, bump_texture = NULL, color = NULL)
  )
  class(mesh) = "mesh3d"
  attr(mesh, "render_road_mesh_diagnostics") = mesh_data$diagnostics
  mesh
}

#' Subset shared road-chain sections
#'
#' @param sections Complete physical-chain vertex sections.
#' @param section_index Ordered section indices to retain.
#'
#' @return Vertex sections for one material section.
#' @keywords internal
subset_render_road_vertex_sections = function(sections, section_index) {
  section_index = suppressWarnings(as.integer(section_index))
  if (
    length(section_index) < 2L ||
      any(!is.finite(section_index)) ||
      any(section_index < 1L) ||
      any(section_index > nrow(sections$points))
  ) {
    stop(
      "A road material section requires at least two valid rings.",
      call. = FALSE
    )
  }
  result = sections
  matrix_names = c(
    "points",
    "left_bottom",
    "right_bottom",
    "left_top",
    "right_top",
    "left_normal",
    "right_normal"
  )
  for (name in matrix_names) {
    result[[name]] = sections[[name]][section_index, , drop = FALSE]
  }
  frame_matrix_names = c(
    "incoming_tangent",
    "outgoing_tangent",
    "side"
  )
  for (name in frame_matrix_names) {
    result$frames[[name]] =
      sections$frames[[name]][section_index, , drop = FALSE]
  }
  frame_vector_names = c(
    "miter_scale",
    "turn_cross",
    "turn_dot",
    "join_style"
  )
  for (name in frame_vector_names) {
    result$frames[[name]] = sections$frames[[name]][section_index]
  }
  result
}

#' Subset shared road-chain surface normals
#'
#' @param surface_normals Complete physical-chain surface normals.
#' @param section_index Ordered section indices to retain.
#'
#' @return Surface normals for one material section.
#' @keywords internal
subset_render_road_surface_normals = function(
  surface_normals,
  section_index
) {
  list(
    top = list(
      left = surface_normals$top$left[
        section_index,
        ,
        drop = FALSE
      ],
      right = surface_normals$top$right[
        section_index,
        ,
        drop = FALSE
      ]
    ),
    bottom = list(
      left = surface_normals$bottom$left[
        section_index,
        ,
        drop = FALSE
      ],
      right = surface_normals$bottom$right[
        section_index,
        ,
        drop = FALSE
      ]
    )
  )
}

#' Calculate road texture coordinates
#'
#' @param station Road station at every ring.
#' @param total_length Total open or periodic road length.
#' @param texture_length Texture repeat length in scene units.
#' @param texture_repeats Default `NULL`. Explicit number of texture repeats.
#' @param closed Whether the road is periodic.
#'
#' @return Texture coordinates and the closing texture coordinate.
#' @keywords internal
calculate_render_road_texture_coordinates = function(
  station,
  total_length,
  texture_length,
  texture_repeats = NULL,
  closed
) {
  texture_length = resolve_render_positive_number(
    texture_length,
    "lane_texture_length"
  )
  texture_repeats = suppressWarnings(as.numeric(texture_repeats[1]))
  if (
    length(texture_repeats) &&
      is.finite(texture_repeats) &&
      texture_repeats > 0 &&
      is.finite(total_length) &&
      total_length > 0
  ) {
    texture_v = station / total_length * texture_repeats
    closing_v = texture_repeats
  } else if (closed) {
    closed_texture_repeats = max(
      1,
      round(total_length / texture_length)
    )
    texture_v = station / total_length * closed_texture_repeats
    closing_v = closed_texture_repeats
  } else {
    texture_v = station / texture_length
    closing_v = total_length / texture_length
  }
  list(texture_v = texture_v, closing_v = closing_v)
}

#' Initialize batched road-chain preparation
#'
#' @param task Assembled road mesh-chain task.
#'
#' @return Normalized task and native densification job, or `NULL`.
#' @keywords internal
initialize_render_highquality_road_chain_preparation = function(task) {
  defaults = list(
    heightmap = NULL,
    zscale = 1,
    texture_file = NULL,
    texture_length = 20,
    texture_repeats = NULL,
    texture_world_scale = c(1, 1),
    terrain_following = TRUE,
    left_width = NULL,
    right_width = NULL,
    envelope_sections = NULL,
    material_sections = NULL,
    cap_start = TRUE,
    cap_end = TRUE,
    closed = FALSE,
    miter_limit = 4,
    round_join_segments = 5L,
    return_mesh = FALSE,
    rgl_id = NULL,
    roadcolor = NULL
  )
  task = utils::modifyList(defaults, task, keep.null = TRUE)
  required = c("points", "bbox_center", "width", "material")
  if (!all(required %in% names(task))) {
    stop("Road mesh-chain task inputs do not match.", call. = FALSE)
  }
  task$terrain_following = resolve_render_logical(
    task$terrain_following,
    "terrain_following"
  )
  task$closed = resolve_render_logical(task$closed, "closed")
  task$cap_start = resolve_render_logical(task$cap_start, "cap_start")
  task$cap_end = resolve_render_logical(task$cap_end, "cap_end")
  task$return_mesh = resolve_render_logical(task$return_mesh, "return_mesh")
  task$miter_limit = resolve_render_positive_number(
    task$miter_limit,
    "miter_limit"
  )
  if (task$miter_limit <= 1) {
    stop("`miter_limit` must be greater than one.", call. = FALSE)
  }
  task$width = resolve_render_positive_number(task$width, "width")
  task$zscale = suppressWarnings(as.numeric(task$zscale[[1L]]))
  if (!is.finite(task$zscale) || task$zscale <= 0) {
    task$zscale = 1
  }
  task$texture_world_scale = suppressWarnings(as.numeric(
    task$texture_world_scale[1:2]
  ))
  if (
    length(task$texture_world_scale) != 2L ||
      any(!is.finite(task$texture_world_scale)) ||
      any(task$texture_world_scale <= 0)
  ) {
    task$texture_world_scale = c(1, 1)
  }
  task$points = collapse_render_highquality_road_path_points(
    task$points,
    texture_world_scale = task$texture_world_scale
  )
  if (task$closed && nrow(task$points) >= 2L) {
    closing_delta = (task$points[nrow(task$points), c(1, 3)] -
      task$points[1L, c(1, 3)]) *
      task$texture_world_scale
    if (sqrt(sum(closing_delta^2)) <= 1e-3) {
      task$points = task$points[-nrow(task$points), , drop = FALSE]
    }
  }
  if (nrow(task$points) < if (task$closed) 3L else 2L) {
    return(NULL)
  }
  densify_points = if (task$closed) {
    rbind(task$points, task$points[1L, , drop = FALSE])
  } else {
    task$points
  }
  list(
    task = task,
    densify_job = list(
      points = densify_points,
      width = task$width,
      terrain_following = task$terrain_following &&
        is.matrix(task$heightmap)
    )
  )
}

#' Prepare a road-chain terrain-section job
#'
#' @param initialized Initialized road-chain preparation.
#' @param densified_points Native densified path points.
#'
#' @return Geometry metadata and native terrain-section job, or `NULL`.
#' @keywords internal
prepare_render_highquality_road_chain_section_job = function(
  initialized,
  densified_points
) {
  task = initialized$task
  points = as.matrix(densified_points)
  if (task$closed && nrow(points)) {
    points = points[-nrow(points), , drop = FALSE]
  }
  points = collapse_render_highquality_road_path_points(
    points,
    texture_world_scale = task$texture_world_scale
  )
  if (nrow(points) < if (task$closed) 3L else 2L) {
    return(NULL)
  }
  normalize_distance = function(value, name) {
    value = suppressWarnings(as.numeric(value))
    if (length(value) == 1L) {
      value = rep(value, nrow(points))
    }
    if (
      length(value) != nrow(points) ||
        any(!is.finite(value)) ||
        any(value <= 0)
    ) {
      stop(
        sprintf("`%s` must contain positive section distances.", name),
        call. = FALSE
      )
    }
    value
  }
  if (!is.null(task$envelope_sections)) {
    envelope_station = calculate_road_path_cumulative_distance(
      points,
      texture_world_scale = task$texture_world_scale
    )
    envelope = evaluate_render_road_envelope_sections(
      envelope_sections = task$envelope_sections,
      station = envelope_station
    )
    left_distance = normalize_distance(envelope$half_width, "left_width")
    right_distance = normalize_distance(envelope$half_width, "right_width")
  } else {
    left_width = if (is.null(task$left_width)) {
      task$width / 2
    } else {
      task$left_width
    }
    right_width = if (is.null(task$right_width)) {
      task$width / 2
    } else {
      task$right_width
    }
    left_distance = normalize_distance(left_width, "left_width")
    right_distance = normalize_distance(right_width, "right_width")
  }
  join_diagnostics = list()
  for (iteration in seq_len(3L)) {
    frames = calculate_render_road_vertex_frames(
      points,
      closed = task$closed,
      miter_limit = task$miter_limit
    )
    if (!any(frames$join_style == "round")) {
      break
    }
    expanded = expand_render_road_unstable_joins(
      points = points,
      left_distance = left_distance,
      right_distance = right_distance,
      closed = task$closed,
      miter_limit = task$miter_limit,
      round_join_segments = task$round_join_segments
    )
    join_diagnostics[[length(join_diagnostics) + 1L]] = expanded$diagnostics
    points = expanded$points
    left_distance = expanded$left_distance
    right_distance = expanded$right_distance
  }
  frames = calculate_render_road_vertex_frames(
    points,
    closed = task$closed,
    miter_limit = task$miter_limit
  )
  if (any(frames$join_style == "round")) {
    stop("Road join expansion did not converge.", call. = FALSE)
  }
  geometry = task
  geometry$points = points
  geometry["mesh_heightmap"] = list(
    if (task$terrain_following) {
      task$heightmap
    } else {
      NULL
    }
  )
  geometry$left_distance = left_distance
  geometry$right_distance = right_distance
  geometry$join_diagnostics = join_diagnostics
  geometry$frames = frames
  geometry$section_job = list(
    points = points,
    left_distance = left_distance,
    right_distance = right_distance,
    side = frames$side,
    miter_scale = frames$miter_scale,
    terrain_following = task$terrain_following && is.matrix(task$heightmap)
  )
  geometry
}

#' Group road preparations by shared terrain
#'
#' @param preparations Initialized road-chain preparations.
#' @param indices Preparation indices to group.
#'
#' @return Terrain groups with original preparation indices.
#' @keywords internal
group_render_highquality_road_preparations_by_terrain = function(
  preparations,
  indices
) {
  groups = list()
  for (index in indices) {
    preparation = preparations[[index]]
    task = preparation$task
    has_terrain = task$terrain_following && is.matrix(task$heightmap)
    matched = FALSE
    for (group_index in seq_along(groups)) {
      group = groups[[group_index]]
      same_terrain = if (!has_terrain && !group$has_terrain) {
        TRUE
      } else {
        has_terrain &&
          group$has_terrain &&
          identical(task$zscale, group$zscale) &&
          identical(task$heightmap, group$heightmap)
      }
      if (same_terrain) {
        groups[[group_index]]$indices = c(group$indices, index)
        matched = TRUE
        break
      }
    }
    if (!matched) {
      groups[[length(groups) + 1L]] = list(
        indices = index,
        has_terrain = has_terrain,
        heightmap = if (has_terrain) {
          task$heightmap
        } else {
          matrix(numeric(), nrow = 0L, ncol = 0L)
        },
        zscale = if (has_terrain) task$zscale else 1
      )
    }
  }
  groups
}

#' Prepare all high-quality road chains with native terrain queues
#'
#' @param tasks Assembled road mesh-chain tasks.
#' @param verbose Whether to display preparation progress.
#' @param parallel Whether to use native worker threads.
#'
#' @return Per-chain prepared results and captured errors.
#' @keywords internal
prepare_render_highquality_road_chain_meshes = function(
  tasks,
  verbose = FALSE,
  parallel = FALSE
) {
  preparation_progress = new_render_highquality_progress_bar(
    verbose = verbose,
    label = "Preparing road mesh jobs",
    total = length(tasks)
  )
  initialized_results = lapply(tasks, function(task) {
    tryCatch(
      list(
        initialized = initialize_render_highquality_road_chain_preparation(
          task
        ),
        error = NULL
      ),
      error = function(error) {
        list(initialized = NULL, error = conditionMessage(error))
      }
    )
  })
  initialized = lapply(initialized_results, `[[`, "initialized")
  initialized_index = which(vapply(
    initialized_results,
    function(result) is.null(result$error) && !is.null(result$initialized),
    logical(1)
  ))
  densified = vector("list", length(tasks))
  terrain_groups = group_render_highquality_road_preparations_by_terrain(
    initialized,
    initialized_index
  )
  for (group in terrain_groups) {
    densified[group$indices] = densify_render_road_paths_batch_cpp(
      input_jobs = lapply(
        initialized[group$indices],
        `[[`,
        "densify_job"
      ),
      heightmap = group$heightmap,
      zscale = group$zscale,
      parallel = parallel,
      verbose = verbose
    )
  }
  geometry_results = vector("list", length(tasks))
  for (index in initialized_index) {
    geometry_results[[index]] = tryCatch(
      list(
        geometry = prepare_render_highquality_road_chain_section_job(
          initialized[[index]],
          densified[[index]]
        ),
        error = NULL
      ),
      error = function(error) {
        list(geometry = NULL, error = conditionMessage(error))
      }
    )
  }
  geometry_index = which(vapply(
    geometry_results,
    function(result) {
      !is.null(result) &&
        is.null(result$error) &&
        !is.null(result$geometry)
    },
    logical(1)
  ))
  geometry = lapply(geometry_results, function(result) {
    if (is.null(result)) NULL else result$geometry
  })
  section_data = vector("list", length(tasks))
  section_groups = group_render_highquality_road_preparations_by_terrain(
    initialized,
    geometry_index
  )
  for (group in section_groups) {
    section_data[group$indices] = sample_render_road_sections_batch_cpp(
      input_jobs = lapply(
        geometry[group$indices],
        `[[`,
        "section_job"
      ),
      heightmap = group$heightmap,
      zscale = group$zscale,
      parallel = parallel,
      verbose = verbose
    )
  }
  prepared_results = vector("list", length(tasks))
  for (index in seq_along(tasks)) {
    preparation_error = initialized_results[[index]]$error
    if (is.null(preparation_error) && !is.null(geometry_results[[index]])) {
      preparation_error = geometry_results[[index]]$error
    }
    if (!is.null(preparation_error)) {
      prepared_results[[index]] = list(
        prepared = NULL,
        error = preparation_error
      )
    } else if (
      is.null(initialized[[index]]) ||
        is.null(geometry[[index]])
    ) {
      prepared_results[[index]] = list(prepared = NULL, error = NULL)
    } else {
      sections = c(
        list(
          points = geometry[[index]]$points,
          frames = geometry[[index]]$frames
        ),
        section_data[[index]]
      )
      prepared_results[[index]] = tryCatch(
        list(
          prepared = do.call(
            prepare_render_highquality_road_chain_mesh,
            c(
              tasks[[index]],
              list(
                preparation_geometry = geometry[[index]],
                precomputed_sections = sections
              )
            )
          ),
          error = NULL
        ),
        error = function(error) {
          list(prepared = NULL, error = conditionMessage(error))
        }
      )
    }
    if (!is.null(preparation_progress)) {
      preparation_progress$tick()
    }
  }
  prepared_results
}

#' Prepare a high-quality continuous road chain mesh
#'
#' @param points Path points.
#' @param bbox_center Scene center.
#' @param width Road width.
#' @param heightmap Cached heightmap.
#' @param zscale Effective zscale.
#' @param material Rayrender material.
#' @param texture_file Default `NULL`. Road texture file.
#' @param texture_length Texture repeat length in scene units.
#' @param texture_repeats Default `NULL`. Number of texture repeats.
#' @param texture_world_scale Default `c(1, 1)`. Scene-to-world scale.
#' @param terrain_following Whether the mesh follows terrain.
#' @param left_width Default `NULL`. Left distance from the centerline.
#' @param right_width Default `NULL`. Right distance from the centerline.
#' @param envelope_sections Default `NULL`. Station-based centered road
#' envelope sections.
#' @param material_sections Default `NULL`. Source material and texture
#' sections along the physical mesh chain.
#' @param cap_start Whether to cap the first section.
#' @param cap_end Whether to cap the final section.
#' @param closed Whether the path is periodic.
#' @param miter_limit Maximum permitted miter scale.
#' @param round_join_segments Number of rounded fallback segments.
#' @param return_mesh Whether to return raw mesh data instead of a rayrender model.
#' @param rgl_id Default `NULL`. Source rgl path identifier used for later
#' material overrides.
#' @param roadcolor Default `NULL`. Source road color used for mesh previews and
#' material functions.
#' @param preparation_geometry Default `NULL`. Precomputed road geometry used
#' by the collection preparation queue.
#' @param precomputed_sections Default `NULL`. Precomputed terrain-following
#' road sections used by the collection preparation queue.
#'
#' @return Prepared native mesh job and R-only finalization metadata, or `NULL`.
#' @keywords internal
prepare_render_highquality_road_chain_mesh = function(
  points,
  bbox_center,
  width,
  heightmap = NULL,
  zscale = 1,
  material,
  texture_file = NULL,
  texture_length = 20,
  texture_repeats = NULL,
  texture_world_scale = c(1, 1),
  terrain_following = TRUE,
  left_width = NULL,
  right_width = NULL,
  envelope_sections = NULL,
  material_sections = NULL,
  cap_start = TRUE,
  cap_end = TRUE,
  closed = FALSE,
  miter_limit = 4,
  round_join_segments = 5L,
  return_mesh = FALSE,
  rgl_id = NULL,
  roadcolor = NULL,
  preparation_geometry = NULL,
  precomputed_sections = NULL
) {
  if (is.null(preparation_geometry)) {
    terrain_following = resolve_render_logical(
      terrain_following,
      "terrain_following"
    )
    closed = resolve_render_logical(closed, "closed")
    cap_start = resolve_render_logical(cap_start, "cap_start")
    cap_end = resolve_render_logical(cap_end, "cap_end")
    return_mesh = resolve_render_logical(return_mesh, "return_mesh")
    miter_limit = resolve_render_positive_number(
      miter_limit,
      "miter_limit"
    )
    if (miter_limit <= 1) {
      stop("`miter_limit` must be greater than one.", call. = FALSE)
    }
    texture_world_scale = suppressWarnings(as.numeric(texture_world_scale[1:2]))
    if (
      length(texture_world_scale) != 2L ||
        any(!is.finite(texture_world_scale)) ||
        any(texture_world_scale <= 0)
    ) {
      texture_world_scale = c(1, 1)
    }
    points = collapse_render_highquality_road_path_points(
      points,
      texture_world_scale = texture_world_scale
    )
    if (closed && nrow(points) >= 2L) {
      closing_delta = (points[nrow(points), c(1, 3)] -
        points[1L, c(1, 3)]) *
        texture_world_scale
      if (sqrt(sum(closing_delta^2)) <= 1e-3) {
        points = points[-nrow(points), , drop = FALSE]
      }
    }
    if (nrow(points) < if (closed) 3L else 2L) {
      return(NULL)
    }
    mesh_heightmap = if (terrain_following) heightmap else NULL
    densify_points = if (closed) {
      rbind(points, points[1L, , drop = FALSE])
    } else {
      points
    }
    densify_points = densify_render_highquality_path_points(
      points = densify_points,
      width = width,
      heightmap = mesh_heightmap,
      zscale = zscale
    )
    if (closed) {
      densify_points = densify_points[-nrow(densify_points), , drop = FALSE]
    }
    points = collapse_render_highquality_road_path_points(
      densify_points,
      texture_world_scale = texture_world_scale
    )
    if (nrow(points) < if (closed) 3L else 2L) {
      return(NULL)
    }
    half_width = width / 2
    normalize_distance = function(value, name) {
      value = suppressWarnings(as.numeric(value))
      if (length(value) == 1L) {
        value = rep(value, nrow(points))
      }
      if (
        length(value) != nrow(points) ||
          any(!is.finite(value)) ||
          any(value <= 0)
      ) {
        stop(
          sprintf("`%s` must contain positive section distances.", name),
          call. = FALSE
        )
      }
      value
    }
    if (!is.null(envelope_sections)) {
      envelope_station = calculate_road_path_cumulative_distance(
        points,
        texture_world_scale = texture_world_scale
      )
      envelope = evaluate_render_road_envelope_sections(
        envelope_sections = envelope_sections,
        station = envelope_station
      )
      left_distance = normalize_distance(
        envelope$half_width,
        "left_width"
      )
      right_distance = normalize_distance(
        envelope$half_width,
        "right_width"
      )
    } else {
      left_width = if (is.null(left_width)) half_width else left_width
      right_width = if (is.null(right_width)) half_width else right_width
      left_distance = normalize_distance(left_width, "left_width")
      right_distance = normalize_distance(right_width, "right_width")
    }
    join_diagnostics = list()
    for (iteration in seq_len(3L)) {
      frames = calculate_render_road_vertex_frames(
        points,
        closed = closed,
        miter_limit = miter_limit
      )
      if (!any(frames$join_style == "round")) {
        break
      }
      expanded = expand_render_road_unstable_joins(
        points = points,
        left_distance = left_distance,
        right_distance = right_distance,
        closed = closed,
        miter_limit = miter_limit,
        round_join_segments = round_join_segments
      )
      join_diagnostics[[length(join_diagnostics) + 1L]] =
        expanded$diagnostics
      points = expanded$points
      left_distance = expanded$left_distance
      right_distance = expanded$right_distance
    }
    frames = calculate_render_road_vertex_frames(
      points,
      closed = closed,
      miter_limit = miter_limit
    )
    if (any(frames$join_style == "round")) {
      stop("Road join expansion did not converge.", call. = FALSE)
    }
    sections = calculate_render_road_vertex_sections(
      points = points,
      left_distance = left_distance,
      right_distance = right_distance,
      heightmap = mesh_heightmap,
      zscale = zscale,
      closed = closed,
      miter_limit = miter_limit
    )
  } else {
    list2env(preparation_geometry, envir = environment())
    mesh_heightmap = if (terrain_following) heightmap else NULL
    sections = precomputed_sections
  }
  inverted_segment = sort(unique(c(
    identify_render_road_inverted_surface_segments(
      sections$left_bottom,
      sections$right_bottom,
      closed = closed
    ),
    identify_render_road_inverted_surface_segments(
      sections$left_top,
      sections$right_top,
      closed = closed
    )
  )))
  stabilization = NULL
  if (length(inverted_segment)) {
    stabilization_fraction = c(
      0.2,
      0.3,
      0.4,
      0.6,
      0.8,
      1,
      1.5,
      2,
      3,
      4,
      6,
      8
    )
    stabilization_attempt = vector(
      "list",
      length(stabilization_fraction)
    )
    stabilized_sections = NULL
    for (attempt in seq_along(stabilization_fraction)) {
      fraction = stabilization_fraction[[attempt]]
      attempt_result = tryCatch(
        {
          stabilized_frames = calculate_render_road_stabilized_vertex_frames(
            points = points,
            left_distance = left_distance,
            right_distance = right_distance,
            texture_world_scale = texture_world_scale,
            closed = closed,
            miter_limit = miter_limit,
            guide_step_fraction = fraction
          )
          attempt_sections = calculate_render_road_vertex_sections(
            points = points,
            left_distance = left_distance,
            right_distance = right_distance,
            heightmap = mesh_heightmap,
            zscale = zscale,
            closed = closed,
            miter_limit = miter_limit,
            frames = stabilized_frames
          )
          remaining_inverted_segment = sort(unique(c(
            identify_render_road_inverted_surface_segments(
              attempt_sections$left_bottom,
              attempt_sections$right_bottom,
              closed = closed
            ),
            identify_render_road_inverted_surface_segments(
              attempt_sections$left_top,
              attempt_sections$right_top,
              closed = closed
            )
          )))
          list(
            frames = stabilized_frames,
            sections = attempt_sections,
            remaining_inverted_segment = remaining_inverted_segment,
            error = NA_character_
          )
        },
        error = function(error) {
          list(
            frames = NULL,
            sections = NULL,
            remaining_inverted_segment = integer(0),
            error = conditionMessage(error)
          )
        }
      )
      stabilization_attempt[[attempt]] = data.frame(
        guide_step_fraction = fraction,
        remaining_inverted_segment = paste(
          attempt_result$remaining_inverted_segment,
          collapse = ","
        ),
        error = attempt_result$error,
        stringsAsFactors = FALSE
      )
      if (
        is.na(attempt_result$error) &&
          !length(attempt_result$remaining_inverted_segment)
      ) {
        stabilized_sections = attempt_result$sections
        stabilization = attr(
          attempt_result$frames,
          "render_road_stabilization"
        )
        stabilization$guide_step_fraction = fraction
        break
      }
    }
    stabilization_attempt = Filter(
      Negate(is.null),
      stabilization_attempt
    )
    stabilization_attempt = do.call(rbind, stabilization_attempt)
    if (is.null(stabilized_sections)) {
      stop(
        paste0(
          "Road sweep stabilization could not resolve inverted surface ",
          "segments: ",
          paste(inverted_segment, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
    stabilization$initial_inverted_segment = inverted_segment
    stabilization$attempts = stabilization_attempt
    sections = stabilized_sections
  }
  station = calculate_road_path_cumulative_distance(
    points,
    texture_world_scale = texture_world_scale
  )
  total_length = utils::tail(station, 1L)
  if (closed) {
    closing_delta = (points[1L, c(1, 3)] -
      points[nrow(points), c(1, 3)]) *
      texture_world_scale
    total_length = total_length + sqrt(sum(closing_delta^2))
  }
  diagnostics = list(
    section_count = nrow(points),
    join_expansion = join_diagnostics,
    sweep_stabilization = stabilization,
    envelope_section_count = if (is.null(envelope_sections)) {
      0L
    } else {
      nrow(envelope_sections)
    },
    minimum_left_width = min(left_distance),
    maximum_left_width = max(left_distance),
    minimum_right_width = min(right_distance),
    maximum_right_width = max(right_distance)
  )
  if (is.null(material_sections) || length(material_sections) <= 1L) {
    texture_coordinates = calculate_render_road_texture_coordinates(
      station = station,
      total_length = total_length,
      texture_length = texture_length,
      texture_repeats = texture_repeats,
      closed = closed
    )
    return(list(
      job = list(
        sections = sections,
        bbox_center = bbox_center,
        closed = closed,
        mesh_sections = list(list(
          section_index = seq_len(nrow(points)),
          texture_v = texture_coordinates$texture_v,
          closing_v = texture_coordinates$closing_v,
          cap_start = cap_start,
          cap_end = cap_end,
          closed = closed
        ))
      ),
      specifications = list(list(
        rgl_id = rgl_id,
        roadcolor = roadcolor,
        texture_file = texture_file,
        material = material,
        cap_start = !closed && cap_start,
        cap_end = !closed && cap_end,
        closed = closed
      )),
      diagnostics = diagnostics,
      boundary_index = NULL,
      return_mesh = return_mesh,
      grouped = FALSE
    ))
  }
  material_sections = material_sections[
    order(vapply(
      material_sections,
      function(section) as.numeric(section$station_start[[1L]]),
      numeric(1)
    ))
  ]
  source_total_length = max(vapply(
    material_sections,
    function(section) as.numeric(section$station_end[[1L]]),
    numeric(1)
  ))
  if (
    !is.finite(source_total_length) ||
      source_total_length <= 0 ||
      !is.finite(total_length) ||
      total_length <= 0
  ) {
    stop("Road material sections have invalid stations.", call. = FALSE)
  }
  boundary_station = c(
    vapply(
      material_sections,
      function(section) as.numeric(section$station_start[[1L]]),
      numeric(1)
    ),
    source_total_length
  )
  boundary_station = boundary_station / source_total_length * total_length
  boundary_index = integer(length(boundary_station))
  boundary_index[[1L]] = 1L
  boundary_index[[length(boundary_index)]] = if (closed) {
    nrow(points) + 1L
  } else {
    nrow(points)
  }
  if (length(boundary_index) > 2L) {
    for (boundary in seq.int(2L, length(boundary_index) - 1L)) {
      available_start = boundary_index[[boundary - 1L]] + 1L
      available_end = nrow(points) -
        (length(boundary_index) - boundary - 1L)
      if (available_end < available_start) {
        stop(
          "Road material sections contain insufficient shared rings.",
          call. = FALSE
        )
      }
      candidate = seq.int(available_start, available_end)
      boundary_index[[boundary]] = candidate[which.min(abs(
        station[candidate] - boundary_station[[boundary]]
      ))]
    }
  }
  mesh_sections = vector("list", length(material_sections))
  specifications = vector("list", length(material_sections))
  for (material_index in seq_along(material_sections)) {
    start_index = boundary_index[[material_index]]
    end_index = boundary_index[[material_index + 1L]]
    if (closed && end_index == nrow(points) + 1L) {
      section_index = c(seq.int(start_index, nrow(points)), 1L)
      section_station = c(
        station[seq.int(start_index, nrow(points))] -
          station[[start_index]],
        total_length - station[[start_index]]
      )
    } else {
      section_index = seq.int(start_index, end_index)
      section_station = station[section_index] -
        station[[start_index]]
    }
    section_length = utils::tail(section_station, 1L)
    if (!is.finite(section_length) || section_length <= 0) {
      stop(
        "A road material section has nonpositive rendered length.",
        call. = FALSE
      )
    }
    specification = material_sections[[material_index]]
    texture_coordinates = calculate_render_road_texture_coordinates(
      station = section_station,
      total_length = section_length,
      texture_length = specification$texture_length,
      texture_repeats = specification$texture_repeats,
      closed = FALSE
    )
    rendered_cap_start = !closed &&
      material_index == 1L &&
      cap_start
    rendered_cap_end = !closed &&
      material_index == length(material_sections) &&
      cap_end
    mesh_sections[[material_index]] = list(
      section_index = section_index,
      texture_v = texture_coordinates$texture_v,
      closing_v = texture_coordinates$closing_v,
      cap_start = rendered_cap_start,
      cap_end = rendered_cap_end,
      closed = FALSE
    )
    specifications[[material_index]] = list(
      rgl_id = specification$rgl_id,
      roadcolor = specification$roadcolor,
      texture_file = specification$texture_file,
      material = specification$material,
      cap_start = rendered_cap_start,
      cap_end = rendered_cap_end,
      closed = FALSE
    )
  }
  list(
    job = list(
      sections = sections,
      bbox_center = bbox_center,
      closed = closed,
      mesh_sections = mesh_sections
    ),
    specifications = specifications,
    diagnostics = diagnostics,
    boundary_index = boundary_index,
    return_mesh = return_mesh,
    grouped = TRUE
  )
}

#' Finalize a prepared high-quality road chain mesh
#'
#' @param prepared Prepared road-chain job and R-only metadata.
#' @param native_result Native mesh arrays for every material section.
#'
#' @return A rayrender mesh model, raw `mesh3d`, mesh group, or `NULL`.
#' @keywords internal
finalize_render_highquality_road_chain_mesh = function(
  prepared,
  native_result
) {
  native_meshes = native_result$meshes
  if (length(native_meshes) != length(prepared$specifications)) {
    stop("Native road mesh result sections do not match.", call. = FALSE)
  }
  section_meshes = vector("list", length(native_meshes))
  for (material_index in seq_along(native_meshes)) {
    mesh_data = native_meshes[[material_index]]
    if (!nrow(mesh_data$vertices)) {
      next
    }
    colnames(mesh_data$indices) = c("quad_starts", "", "")
    specification = prepared$specifications[[material_index]]
    mesh = list(
      vb = t(cbind(mesh_data$vertices, 1)),
      it = t(mesh_data$indices),
      normals = t(mesh_data$vertex_normals),
      texcoords = t(mesh_data$texcoords),
      material = list(
        texture = specification$texture_file,
        bump_texture = NULL,
        color = NULL
      )
    )
    class(mesh) = "mesh3d"
    diagnostics = mesh_data$diagnostics
    diagnostics$closed = specification$closed
    diagnostics$cap_start = specification$cap_start
    diagnostics$cap_end = specification$cap_end
    diagnostics$section_count = prepared$diagnostics$section_count
    diagnostics$join_expansion = prepared$diagnostics$join_expansion
    diagnostics$sweep_stabilization =
      prepared$diagnostics$sweep_stabilization
    diagnostics$envelope_section_count =
      prepared$diagnostics$envelope_section_count
    diagnostics$material_section_index = material_index
    diagnostics$material_section_count = length(native_meshes)
    diagnostics$minimum_left_width =
      prepared$diagnostics$minimum_left_width
    diagnostics$maximum_left_width =
      prepared$diagnostics$maximum_left_width
    diagnostics$minimum_right_width =
      prepared$diagnostics$minimum_right_width
    diagnostics$maximum_right_width =
      prepared$diagnostics$maximum_right_width
    attr(mesh, "render_road_mesh_diagnostics") = diagnostics
    attr(mesh, "render_road_mesh_specification") = specification
    section_meshes[[material_index]] = if (prepared$return_mesh) {
      mesh
    } else {
      rayrender::mesh3d_model(
        mesh,
        override_material = is.null(specification$texture_file),
        material = specification$material
      )
    }
  }
  section_meshes = Filter(Negate(is.null), section_meshes)
  if (!length(section_meshes)) {
    return(NULL)
  }
  if (!prepared$grouped) {
    return(section_meshes[[1L]])
  }
  class(section_meshes) = c("render_road_mesh_group", "list")
  attr(section_meshes, "boundary_index") = prepared$boundary_index
  section_meshes
}

#' Make a high-quality continuous road chain mesh
#'
#' @inheritParams prepare_render_highquality_road_chain_mesh
#' @param parallel Default `FALSE`. Whether to use multiple native threads.
#'
#' @return A rayrender mesh model, raw `mesh3d`, or `NULL`.
#' @keywords internal
make_render_highquality_road_chain_mesh = function(
  points,
  bbox_center,
  width,
  heightmap = NULL,
  zscale = 1,
  material,
  texture_file = NULL,
  texture_length = 20,
  texture_repeats = NULL,
  texture_world_scale = c(1, 1),
  terrain_following = TRUE,
  left_width = NULL,
  right_width = NULL,
  envelope_sections = NULL,
  material_sections = NULL,
  cap_start = TRUE,
  cap_end = TRUE,
  closed = FALSE,
  miter_limit = 4,
  round_join_segments = 5L,
  return_mesh = FALSE,
  rgl_id = NULL,
  roadcolor = NULL,
  parallel = FALSE
) {
  parallel = resolve_render_logical(parallel, "parallel")
  preparation_arguments = as.list(environment())
  preparation_arguments$parallel = NULL
  preparation_result = prepare_render_highquality_road_chain_meshes(
    tasks = list(preparation_arguments),
    verbose = FALSE,
    parallel = parallel
  )[[1L]]
  if (!is.null(preparation_result$error)) {
    stop(preparation_result$error, call. = FALSE)
  }
  prepared = preparation_result$prepared
  if (is.null(prepared)) {
    return(NULL)
  }
  native_result = build_render_highquality_road_mesh_batch_cpp(
    input_jobs = list(prepared$job),
    parallel = parallel,
    verbose = FALSE
  )[[1L]]
  if (!isTRUE(native_result$success)) {
    stop(native_result$error, call. = FALSE)
  }
  finalize_render_highquality_road_chain_mesh(prepared, native_result)
}

#' Collapse effectively duplicated road path points
#'
#' @param points Path points.
#' @param texture_world_scale Default `c(1, 1)`. Multipliers converting scene
#' x-z distances to world distances.
#' @param minimum_world_step Default `1e-3`. Minimum horizontal distance between
#' retained points in world units.
#'
#' @return Filtered path point matrix.
#' @keywords internal
collapse_render_highquality_road_path_points = function(
  points,
  texture_world_scale = c(1, 1),
  minimum_world_step = 1e-3
) {
  points = as.matrix(points)
  points = points[stats::complete.cases(points), , drop = FALSE]
  if (nrow(points) < 2) {
    return(points)
  }
  texture_world_scale = suppressWarnings(as.numeric(texture_world_scale[1:2]))
  if (
    length(texture_world_scale) != 2 ||
      any(!is.finite(texture_world_scale)) ||
      any(texture_world_scale <= 0)
  ) {
    texture_world_scale = c(1, 1)
  }
  minimum_world_step = suppressWarnings(as.numeric(minimum_world_step[1]))
  if (
    !length(minimum_world_step) ||
      !is.finite(minimum_world_step) ||
      minimum_world_step <= 0
  ) {
    minimum_world_step = 1e-3
  }
  keep = rep(FALSE, nrow(points))
  keep[[1L]] = TRUE
  previous_index = 1L
  for (point_index in seq.int(2L, nrow(points))) {
    world_delta = (points[point_index, c(1, 3)] -
      points[previous_index, c(1, 3)]) *
      texture_world_scale
    if (sqrt(sum(world_delta^2)) > minimum_world_step) {
      keep[[point_index]] = TRUE
      previous_index = point_index
    }
  }
  final_index = nrow(points)
  if (!keep[[final_index]] && previous_index != 1L) {
    keep[[previous_index]] = FALSE
    keep[[final_index]] = TRUE
  }
  points[keep, , drop = FALSE]
}

#' Sanitize road quad vertex normals
#'
#' @param vertices Road mesh vertices, with four consecutive rows per quad.
#' @param vertex_normals Proposed vertex normals matching `vertices`.
#'
#' @return Vertex normals that remain in the geometric hemisphere of both
#' triangles in each quad.
#' @keywords internal
sanitize_render_highquality_road_quad_normals = function(
  vertices,
  vertex_normals
) {
  vertices = as.matrix(vertices)
  vertex_normals = as.matrix(vertex_normals)
  if (
    nrow(vertices) != nrow(vertex_normals) ||
      nrow(vertices) %% 4L != 0L
  ) {
    return(vertex_normals)
  }
  quad_starts = seq(1L, nrow(vertices), by = 4L)
  first_face = row_cross(
    vertices[quad_starts + 1L, , drop = FALSE] -
      vertices[quad_starts, , drop = FALSE],
    vertices[quad_starts + 2L, , drop = FALSE] -
      vertices[quad_starts, , drop = FALSE]
  )
  second_face = row_cross(
    vertices[quad_starts + 2L, , drop = FALSE] -
      vertices[quad_starts, , drop = FALSE],
    vertices[quad_starts + 3L, , drop = FALSE] -
      vertices[quad_starts, , drop = FALSE]
  )
  first_face = normalize_render_highquality_rows(first_face)
  second_face = normalize_render_highquality_rows(second_face)
  fallback_normals = normalize_render_highquality_rows(first_face + second_face)
  fallback_normals = replace_invalid_render_highquality_vectors(
    fallback_normals,
    fallback = c(0, 1, 0)
  )
  invalid_quad = !stats::complete.cases(first_face) |
    !stats::complete.cases(second_face)
  first_rows = cbind(
    quad_starts,
    quad_starts + 1L,
    quad_starts + 2L
  )
  second_rows = cbind(
    quad_starts,
    quad_starts + 2L,
    quad_starts + 3L
  )
  for (corner_index in seq_len(ncol(first_rows))) {
    invalid_quad = invalid_quad |
      rowSums(
        vertex_normals[first_rows[, corner_index], , drop = FALSE] *
          first_face
      ) <=
        0 |
      rowSums(
        vertex_normals[second_rows[, corner_index], , drop = FALSE] *
          second_face
      ) <=
        0
  }
  if (any(invalid_quad)) {
    invalid_rows = unlist(lapply(
      quad_starts[invalid_quad],
      function(quad_start) seq.int(quad_start, quad_start + 3L)
    ))
    vertex_normals[invalid_rows, ] = fallback_normals[
      rep(which(invalid_quad), each = 4L),
      ,
      drop = FALSE
    ]
  }
  vertex_normals
}
