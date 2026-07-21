#' Render Road Paths
#'
#' @description Adds road paths to the scene. Roads are previewed as rgl lines
#' and rendered by [render_highquality()] as textured rectangular meshes.
#'
#' @param roads Spatial line data used to draw road paths. Supports `sf`,
#' `sfc`, `sfg`, `SpatialLines`, and `SpatialLinesDataFrame` line inputs.
#' @param heightmap Default `NULL`. Height matrix or spatial raster for the
#' current scene. If omitted, this is taken from the cached scene set by
#' [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#' @param roadcolor Default `"#303030"`. Road surface color.
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
#' treated as the implicit layer `0` when determining local crossing order.
#' Roads are grouped by actual intersections and a branched graph of coincident
#' feature endpoints. Each higher road is constrained above the already-solved
#' height of the road below at every exact intersection. Positive layers use a
#' linear interpolation between their sampled endpoint elevations as the deck
#' baseline instead of following interior terrain. The resulting branched
#' profile is low-pass filtered across both individual features and accepted
#' endpoint continuations, while retaining the requested clearance at every
#' crossing. Non-tunnel roads are constrained to remain at or above their
#' sampled terrain everywhere; negative layers retain their below-terrain
#' tunnel profile. Ambiguous endpoint matches that would create a contradictory
#' clearance cycle are excluded. Supplying `layer` disables `merge`.
#' @param layer_height Default `5.5`. Either a single positive spacing in
#' elevation units between locally ordered layers, or an unquoted or character
#' column name containing each feature's positive separation above the lower
#' road at an intersection. Column heights override constant layer spacing.
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
#' an unquoted or character column name in an `sf` road object; this generates
#' the matching texture and width for each feature.
#' @param lane_width Default `3`. Lane width in scene/world units used when
#' `width = NULL`. The derived road width is
#' `lane_width * (lanes + 2)`. The generated lane texture leaves one lane width
#' total outside the edge lines, split between both sides.
#' @param lane_color Default `"white"`. Color for dashed lane divider markings.
#' @param centerline_color Default `"#ffd23f"`. Color for the center divider.
#' @param edge_line_color Default `"white"`. Color for solid edge markings.
#' @param lane_line_width Default `0.035`. Lane marking width as a fraction of
#' the road width in the generated texture.
#' @param lane_dash_fraction Default `NULL`, which uses
#' `lane_dash_length / (lane_dash_length + lane_gap_length)`. Fraction of each
#' texture repetition occupied by a dash for dashed lane markings.
#' @param clear_previous Default `TRUE`. If `TRUE`, removes the existing road
#' layer before drawing the new one.
#'
#' @return Invisibly returns the rendered road coordinates.
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
  lane_color = "white",
  centerline_color = "#ffd23f",
  edge_line_color = "white",
  lane_line_width = 0.035,
  lane_dash_fraction = NULL,
  clear_previous = TRUE
) {
  heightmap = resolve_render_water_heightmap(
    heightmap,
    heightmap_missing = missing(heightmap),
    caller = "render_roads"
  )
  if (is.null(heightmap)) {
    stop(
      "No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
    )
  }
  zscale = resolve_render_water_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    heightmap = heightmap,
    caller = "render_roads"
  )
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  width_column = resolve_waterpath_width_column(
    width_column = width_column,
    width_column_expr = substitute(width_column),
    width_column_missing = missing(width_column)
  )
  layer_column = resolve_render_road_column(
    value = layer,
    value_expr = substitute(layer),
    value_missing = missing(layer),
    argument = "layer"
  )
  layer_height_spec = resolve_render_road_layer_height(
    value = layer_height,
    value_expr = substitute(layer_height),
    value_missing = missing(layer_height)
  )
  lanes_spec = resolve_render_road_lanes(
    value = lanes,
    value_expr = substitute(lanes),
    value_missing = missing(lanes)
  )
  if (is.null(layer_column) && !is.null(layer_height_spec$column)) {
    stop("`layer_height` can only name a column when `layer` is supplied.")
  }
  if (isTRUE(clear_previous)) {
    rgl::pop3d(tag = "road_path")
    clear_render_road_path_info()
  }
  render_road_paths(
    roads = roads,
    heightmap = heightmap,
    extent = resolve_scene_render_extent(
      heightmap = heightmap,
      caller = "render_roads",
      error_if_missing = FALSE
    ),
    zscale = zscale,
    roadcolor = roadcolor,
    road_width = width,
    road_width_column = width_column,
    road_densify = densify,
    road_offset = offset,
    road_offset_transition = offset_transition,
    road_layer_column = layer_column,
    road_layer_spacing = layer_height_spec$spacing,
    road_layer_height_column = layer_height_spec$column,
    road_lanes_column = lanes_spec$column,
    road_merge = merge,
    lane_texture = lane_texture,
    lane_texture_file = lane_texture_file,
    lane_dash_length = lane_dash_length,
    lane_gap_length = lane_gap_length,
    lane_texture_length = lane_texture_length,
    lane_texture_mapping = lane_texture_mapping,
    lanes = lanes_spec$value,
    lane_width = lane_width,
    lane_color = lane_color,
    centerline_color = centerline_color,
    edge_line_color = edge_line_color,
    lane_line_width = lane_line_width,
    lane_dash_fraction = lane_dash_fraction
  )
}

#' Resolve a render road column
#'
#' @param value Column argument value.
#' @param value_expr Captured column argument expression.
#' @param value_missing Whether the argument was omitted.
#' @param argument Argument name used in errors.
#'
#' @return Column name or `NULL`.
#' @keywords internal
resolve_render_road_column = function(
  value = NULL,
  value_expr = NULL,
  value_missing = FALSE,
  argument
) {
  if (isTRUE(value_missing) || identical(value_expr, quote(NULL))) {
    return(NULL)
  }
  if (is.character(value_expr)) {
    return(validate_render_road_column_name(value_expr, argument))
  }
  if (is.name(value_expr)) {
    evaluated = tryCatch(value, error = function(error) NULL)
    if (is.character(evaluated) && length(evaluated) == 1L) {
      return(validate_render_road_column_name(evaluated, argument))
    }
    return(validate_render_road_column_name(
      as.character(value_expr),
      argument
    ))
  }
  validate_render_road_column_name(value, argument)
}

#' Validate a render road column name
#'
#' @param value Column name.
#' @param argument Argument name used in errors.
#'
#' @return Validated column name.
#' @keywords internal
validate_render_road_column_name = function(value, argument) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop(
      sprintf("`%s` must be a single column name.", argument),
      call. = FALSE
    )
  }
  value
}

#' Resolve render road layer height input
#'
#' @param value Layer height argument value.
#' @param value_expr Captured layer height expression.
#' @param value_missing Whether the argument was omitted.
#'
#' @return List containing either a spacing or column name.
#' @keywords internal
resolve_render_road_layer_height = function(
  value = 5.5,
  value_expr = NULL,
  value_missing = FALSE
) {
  if (isTRUE(value_missing) || identical(value_expr, quote(NULL))) {
    return(list(spacing = 5.5, column = NULL))
  }
  if (is.character(value_expr)) {
    return(list(
      spacing = NULL,
      column = validate_render_road_column_name(
        value_expr,
        "layer_height"
      )
    ))
  }
  if (is.name(value_expr)) {
    evaluated = tryCatch(value, error = function(error) NULL)
    if (is.numeric(evaluated) && length(evaluated) == 1L) {
      return(list(spacing = evaluated, column = NULL))
    }
    if (is.character(evaluated) && length(evaluated) == 1L) {
      return(list(
        spacing = NULL,
        column = validate_render_road_column_name(
          evaluated,
          "layer_height"
        )
      ))
    }
    return(list(
      spacing = NULL,
      column = validate_render_road_column_name(
        as.character(value_expr),
        "layer_height"
      )
    ))
  }
  if (is.numeric(value) && length(value) == 1L) {
    return(list(spacing = value, column = NULL))
  }
  stop(
    paste0(
      "`layer_height` must be a single positive number or a single ",
      "column name."
    ),
    call. = FALSE
  )
}

#' Resolve render road lanes input
#'
#' @param value Lanes argument value.
#' @param value_expr Captured lanes expression.
#' @param value_missing Whether the argument was omitted.
#'
#' @return List containing either a lane value or column name.
#' @keywords internal
resolve_render_road_lanes = function(
  value = 2,
  value_expr = NULL,
  value_missing = FALSE
) {
  if (isTRUE(value_missing)) {
    return(list(value = 2L, column = NULL))
  }
  if (is.character(value_expr)) {
    return(list(
      value = NULL,
      column = validate_render_road_column_name(value_expr, "lanes")
    ))
  }
  if (is.name(value_expr)) {
    evaluated = tryCatch(value, error = function(error) NULL)
    if (is.numeric(evaluated) && length(evaluated) == 1L) {
      return(list(value = evaluated, column = NULL))
    }
    if (is.character(evaluated) && length(evaluated) == 1L) {
      return(list(
        value = NULL,
        column = validate_render_road_column_name(evaluated, "lanes")
      ))
    }
    return(list(
      value = NULL,
      column = validate_render_road_column_name(
        as.character(value_expr),
        "lanes"
      )
    ))
  }
  if (is.numeric(value) && length(value) == 1L) {
    return(list(value = value, column = NULL))
  }
  stop(
    "`lanes` must be a single positive integer or a single column name.",
    call. = FALSE
  )
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
    return(validate_road_lanes(lanes))
  }
  if (!inherits(roads, "sf")) {
    stop("A `lanes` column can only be used with an `sf` road object.")
  }
  if (!(lanes_column %in% names(roads))) {
    stop(
      sprintf("`lanes` must name a column in `roads`: %s", lanes_column),
      call. = FALSE
    )
  }
  raw_lanes = roads[[lanes_column]]
  if (is.factor(raw_lanes)) {
    raw_lanes = as.character(raw_lanes)
  }
  lane_values = suppressWarnings(as.numeric(raw_lanes))
  valid = length(lane_values) == nrow(roads) &&
    all(is.finite(lane_values)) &&
    all(lane_values >= 1) &&
    all(lane_values == floor(lane_values))
  if (!valid) {
    stop(
      sprintf(
        "`lanes` column `%s` must contain positive finite integers.",
        lanes_column
      ),
      call. = FALSE
    )
  }
  as.integer(lane_values)
}

#' Resolve render road layer values
#'
#' @param roads Prepared road features.
#' @param layer_column Layer column name.
#' @param layer_height_column Optional height column name.
#'
#' @return Layer values, explicit-value flags, and optional feature heights.
#' @keywords internal
resolve_render_road_layer_values = function(
  roads,
  layer_column = NULL,
  layer_height_column = NULL
) {
  if (is.null(layer_column)) {
    return(list(layer = NULL, explicit = NULL, height = NULL))
  }
  if (!inherits(roads, "sf")) {
    stop("`layer` can only be used with an `sf` road object.", call. = FALSE)
  }
  if (!(layer_column %in% names(roads))) {
    stop(
      sprintf("`layer` must name a column in `roads`: %s", layer_column),
      call. = FALSE
    )
  }
  raw_layer = roads[[layer_column]]
  if (is.factor(raw_layer)) {
    raw_layer = as.character(raw_layer)
  }
  explicit = !is.na(raw_layer)
  if (is.character(raw_layer)) {
    explicit = explicit & nzchar(trimws(raw_layer))
  }
  layer = suppressWarnings(as.numeric(raw_layer))
  if (any(explicit & !is.finite(layer))) {
    stop(
      sprintf(
        "`layer` column `%s` must contain finite numeric values or NA.",
        layer_column
      ),
      call. = FALSE
    )
  }
  layer[!explicit] = 0

  height = NULL
  if (!is.null(layer_height_column)) {
    if (!(layer_height_column %in% names(roads))) {
      stop(
        sprintf(
          "`layer_height` must name a column in `roads`: %s",
          layer_height_column
        ),
        call. = FALSE
      )
    }
    raw_height = roads[[layer_height_column]]
    if (is.factor(raw_height)) {
      raw_height = as.character(raw_height)
    }
    height_present = !is.na(raw_height)
    if (is.character(raw_height)) {
      height_present = height_present & nzchar(trimws(raw_height))
    }
    height = suppressWarnings(as.numeric(raw_height))
    if (any(height_present & (!is.finite(height) | height < 0))) {
      stop(
        sprintf(
          paste0(
            "`layer_height` column `%s` must contain non-negative finite ",
            "numeric values or NA."
          ),
          layer_height_column
        ),
        call. = FALSE
      )
    }
    height[!height_present] = NA_real_
  }
  list(layer = layer, explicit = explicit, height = height)
}

#' Render road paths
#'
#' @param roads Spatial line input.
#' @param heightmap Heightmap matrix.
#' @param extent Scene extent.
#' @param zscale Effective zscale.
#' @param roadcolor Road color.
#' @param road_width Road width.
#' @param road_width_column Column name containing road widths.
#' @param road_densify Whether to densify paths.
#' @param road_offset Centerline offset in elevation units.
#' @param road_offset_transition Quadratic transition length at each path end.
#' @param road_layer_column Column containing OSM-style layer values.
#' @param road_layer_spacing Constant vertical spacing between ordered layers.
#' @param road_layer_height_column Column containing feature-specific heights.
#' @param road_lanes_column Column containing feature lane counts.
#' @param road_merge Whether to merge connected linework.
#' @param lane_texture Whether to use a lane texture.
#' @param lane_texture_file Optional lane texture file.
#' @param lane_dash_length Painted dash length.
#' @param lane_gap_length Gap length.
#' @param lane_texture_length Scene-unit length per texture repeat.
#' @param lane_texture_mapping Texture mapping mode.
#' @param lanes Number of lanes.
#' @param lane_width Lane width.
#' @param lane_color Lane marking color.
#' @param centerline_color Center line color.
#' @param edge_line_color Edge line color.
#' @param lane_line_width Lane line width fraction.
#' @param lane_dash_fraction Dash fraction.
#'
#' @return Invisibly returns the rendered road coordinates.
#' @keywords internal
render_road_paths = function(
  roads,
  heightmap,
  extent,
  zscale,
  roadcolor,
  road_width = NULL,
  road_width_column = NULL,
  road_densify = TRUE,
  road_offset = 0,
  road_offset_transition = 0,
  road_layer_column = NULL,
  road_layer_spacing = 5.5,
  road_layer_height_column = NULL,
  road_lanes_column = NULL,
  road_merge = TRUE,
  lane_texture = FALSE,
  lane_texture_file = NULL,
  lane_dash_length = 3,
  lane_gap_length = 10,
  lane_texture_length = NULL,
  lane_texture_mapping = c("auto", "fixed"),
  lanes = 2,
  lane_width = 3,
  lane_color = "white",
  centerline_color = "#ffd23f",
  edge_line_color = "white",
  lane_line_width = 0.035,
  lane_dash_fraction = NULL
) {
  if (!is_waterpath_input(roads)) {
    stop(
      "`roads` must be an sf, sfc, sfg, SpatialLines, or SpatialLinesDataFrame line object.",
      call. = FALSE
    )
  }
  road_densify = validate_waterpath_logical(road_densify, "densify")
  road_merge = validate_waterpath_logical(road_merge, "merge")
  road_offset = resolve_waterpath_offset(road_offset, name = "offset")
  road_offset_transition = validate_waterpath_positive_number(
    road_offset_transition,
    "offset_transition",
    allow_zero = TRUE
  )
  if (!is.null(road_layer_column)) {
    road_layer_column = validate_render_road_column_name(
      road_layer_column,
      "layer"
    )
    if (road_offset != 0 || road_offset_transition != 0) {
      stop(
        "`layer` cannot be combined with `offset` or `offset_transition`.",
        call. = FALSE
      )
    }
    road_merge = FALSE
  }
  if (!is.null(road_layer_height_column)) {
    road_layer_height_column = validate_render_road_column_name(
      road_layer_height_column,
      "layer_height"
    )
  } else {
    road_layer_spacing = validate_waterpath_positive_number(
      road_layer_spacing,
      "layer_height"
    )
  }
  lane_texture = validate_waterpath_logical(lane_texture, "lane_texture")
  lane_texture_mapping = match.arg(lane_texture_mapping)
  if (!is.null(road_lanes_column)) {
    road_lanes_column = validate_render_road_column_name(
      road_lanes_column,
      "lanes"
    )
    road_merge = FALSE
  } else {
    lanes = validate_road_lanes(lanes)
  }
  lane_width = validate_waterpath_positive_number(
    lane_width,
    "lane_width"
  )
  lane_dash_length = validate_waterpath_positive_number(
    lane_dash_length,
    "lane_dash_length"
  )
  lane_gap_length = validate_waterpath_positive_number(
    lane_gap_length,
    "lane_gap_length",
    allow_zero = TRUE
  )
  lane_texture_length = resolve_road_lane_texture_length(
    lane_texture_length,
    lane_dash_length = lane_dash_length,
    lane_gap_length = lane_gap_length
  )
  lane_dash_fraction = resolve_road_lane_dash_fraction(
    lane_dash_fraction,
    lane_dash_length = lane_dash_length,
    lane_gap_length = lane_gap_length
  )
  if (!is.null(road_width_column)) {
    road_width_column = validate_waterpath_width_column_name(road_width_column)
    road_merge = FALSE
  }
  roads = prepare_render_water_path_geometry(
    waterpaths = roads,
    waterpath_merge = road_merge
  )
  if (is_empty_scene_sf(roads)) {
    return(invisible(list()))
  }
  road_layer_info = resolve_render_road_layer_values(
    roads = roads,
    layer_column = road_layer_column,
    layer_height_column = road_layer_height_column
  )
  road_lanes = resolve_render_road_lane_values(
    roads = roads,
    lanes = lanes,
    lanes_column = road_lanes_column
  )
  texture_world_scale = calculate_road_path_world_scale(
    heightmap = heightmap,
    extent = extent
  )
  road_width = resolve_render_road_width(
    road_width = road_width,
    lanes = road_lanes,
    lane_width = lane_width,
    texture_world_scale = texture_world_scale
  )
  if (length(road_width) > 1L && is.null(road_width_column)) {
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
  } else {
    road_width = resolve_waterpath_widths(
      waterpaths = roads,
      waterpath_width = road_width,
      waterpath_width_column = road_width_column
    )
  }
  path_render = render_water_path_coords_by_width(
    waterpaths = roads,
    heightmap = heightmap,
    extent = extent,
    zscale = zscale,
    watercolor = roadcolor,
    waterpath_width = road_width,
    force_by_feature = !is.null(road_layer_column) ||
      !is.null(road_lanes_column)
  )
  coord_list = path_render$coord_list
  coord_width = path_render$width
  coord_feature = path_render$feature
  coord_lanes = if (length(road_lanes) == 1L) {
    rep(road_lanes, length(coord_list))
  } else {
    road_lanes[coord_feature]
  }
  coord_terrain_following = rep(TRUE, length(coord_list))
  if (!length(coord_list)) {
    return(invisible(coord_list))
  }
  if (isTRUE(road_densify)) {
    coord_list = densify_water_path_coords(
      coord_list = coord_list,
      heightmap = heightmap,
      zscale = zscale,
      offset = 0
    )
  }
  if (!is.null(road_layer_column)) {
    coord_list = elevate_render_road_layer_coords(
      coord_list = coord_list,
      layer = road_layer_info$layer[coord_feature],
      layer_explicit = road_layer_info$explicit[coord_feature],
      layer_spacing = road_layer_spacing,
      layer_height = if (is.null(road_layer_info$height)) {
        NULL
      } else {
        road_layer_info$height[coord_feature]
      },
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
      offset = road_offset / zscale,
      transition_length = road_offset_transition,
      texture_world_scale = texture_world_scale
    )
    if (road_offset > 0 && road_offset_transition > 0) {
      coord_terrain_following[] = FALSE
    }
  }
  texture_files = resolve_road_lane_texture_files(
    coord_lanes = coord_lanes,
    lane_texture = lane_texture,
    lane_texture_file = lane_texture_file,
    roadcolor = roadcolor,
    lane_color = lane_color,
    centerline_color = centerline_color,
    edge_line_color = edge_line_color,
    lane_line_width = lane_line_width,
    lane_dash_fraction = lane_dash_fraction
  )
  texture_mapping = resolve_road_lane_texture_mapping(
    coord_list = coord_list,
    lane_texture_length = lane_texture_length,
    lane_texture_mapping = lane_texture_mapping,
    texture_world_scale = texture_world_scale
  )
  for (coord_index in seq_along(coord_list)) {
    coord = coord_list[[coord_index]]
    if (is.matrix(coord) && nrow(coord) >= 2) {
      texture_repeats = texture_mapping$texture_repeats[[coord_index]]
      if (!is.finite(texture_repeats)) {
        texture_repeats = NULL
      }
      road_id = rgl::lines3d(
        coord,
        color = roadcolor,
        tag = "road_path",
        lwd = coord_width[[coord_index]],
        line_antialias = FALSE
      )
      register_render_road_path_info(
        id = road_id,
        info = list(
          texture_file = texture_files[[coord_index]],
          texture_length = texture_mapping$texture_length[[coord_index]],
          texture_repeats = texture_repeats,
          texture_mapping = lane_texture_mapping,
          road_length = texture_mapping$road_length[[coord_index]],
          texture_world_scale = texture_mapping$texture_world_scale,
          terrain_following = coord_terrain_following[[coord_index]],
          road_width = coord_width[[coord_index]],
          roadcolor = roadcolor
        )
      )
    }
  }
  invisible(coord_list)
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
    return(offset_water_path_coords(coord_list, offset))
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

# Road layer feature preparation -------------------------------------------

#' Resolve a metric CRS for road topology
#'
#' @param roads Road features with a defined CRS.
#'
#' @return An `sf` CRS using metre units near the road data.
#' @keywords internal
resolve_render_road_metric_crs = function(roads) {
  if (!inherits(roads, "sf") || is.na(sf::st_crs(roads))) {
    stop(
      "Road layer topology requires an `sf` object with a defined CRS.",
      call. = FALSE
    )
  }
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
  original_coords = unclass(original_geometry)
  metric_coords = unclass(metric_geometry)
  if (
    !is.matrix(original_coords) ||
      !is.matrix(metric_coords) ||
      nrow(original_coords) != nrow(metric_coords) ||
      nrow(metric_coords) < 2L
  ) {
    return(NULL)
  }
  minimum_step = suppressWarnings(as.numeric(minimum_step[[1L]]))
  if (!is.finite(minimum_step) || minimum_step <= 0) {
    minimum_step = 1e-3
  }

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
  final = nrow(metric_coords)
  if (!keep[[final]] && previous != 1L) {
    keep[[previous]] = FALSE
    keep[[final]] = TRUE
  }
  if (sum(keep) < 2L) {
    return(NULL)
  }
  list(
    original = sf::st_linestring(original_coords[keep, , drop = FALSE]),
    metric = sf::st_linestring(metric_coords[keep, , drop = FALSE])
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
  source_geometry = sf::st_geometry(source_fragments)
  metric_geometry = sf::st_geometry(metric_fragments)
  cleaned = lapply(seq_along(source_geometry), function(index) {
    collapse_render_road_line_coordinates(
      original_geometry = source_geometry[[index]],
      metric_geometry = metric_geometry[[index]],
      minimum_step = minimum_step
    )
  })
  keep = !vapply(cleaned, is.null, logical(1))
  dropped = source_fragments$render_road_fragment_id[!keep]
  source_fragments = source_fragments[keep, , drop = FALSE]
  metric_fragments = metric_fragments[keep, , drop = FALSE]
  cleaned = cleaned[keep]
  sf::st_geometry(source_fragments) = sf::st_sfc(
    lapply(cleaned, `[[`, "original"),
    crs = sf::st_crs(source_fragments)
  )
  sf::st_geometry(metric_fragments) = sf::st_sfc(
    lapply(cleaned, `[[`, "metric"),
    crs = sf::st_crs(metric_fragments)
  )
  list(
    source = source_fragments,
    metric = metric_fragments,
    dropped_fragment_id = dropped
  )
}

#' Build road fragment endpoint diagnostics
#'
#' @param fragments Metric road fragments.
#' @param boundary Default `NULL`. Optional source-data boundary geometry.
#' @param boundary_tolerance Default `1`. Distance in metres used to identify
#' endpoints on the supplied-data boundary.
#'
#' @return An `sf` endpoint table with inward directions and boundary flags.
#' @keywords internal
build_render_road_endpoint_table = function(
  fragments,
  boundary = NULL,
  boundary_tolerance = 1
) {
  fragment_count = nrow(fragments)
  if (!fragment_count) {
    return(sf::st_sf(
      render_road_endpoint_id = integer(0),
      render_road_fragment_id = integer(0),
      endpoint_side = character(0),
      direction_x = numeric(0),
      direction_y = numeric(0),
      supplied_boundary = logical(0),
      geometry = sf::st_sfc(crs = sf::st_crs(fragments))
    ))
  }
  boundary_tolerance = suppressWarnings(as.numeric(boundary_tolerance[[1L]]))
  if (!is.finite(boundary_tolerance) || boundary_tolerance < 0) {
    boundary_tolerance = 1
  }

  endpoint_geometry = vector("list", fragment_count * 2L)
  direction_x = rep(NA_real_, fragment_count * 2L)
  direction_y = rep(NA_real_, fragment_count * 2L)
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
    inward = rbind(
      coordinates[2L, 1:2] - coordinates[1L, 1:2],
      coordinates[nrow(coordinates) - 1L, 1:2] -
        coordinates[nrow(coordinates), 1:2]
    )
    inward_length = sqrt(rowSums(inward^2))
    valid = is.finite(inward_length) & inward_length > 0
    inward[valid, ] = inward[valid, , drop = FALSE] / inward_length[valid]
    inward[!valid, ] = NA_real_
    direction_x[endpoint_index] = inward[, 1L]
    direction_y[endpoint_index] = inward[, 2L]
  }
  endpoints = sf::st_sf(
    render_road_endpoint_id = seq_len(fragment_count * 2L),
    render_road_fragment_id = rep(
      fragments$render_road_fragment_id,
      each = 2L
    ),
    endpoint_side = rep(c("start", "end"), fragment_count),
    direction_x = direction_x,
    direction_y = direction_y,
    supplied_boundary = FALSE,
    geometry = sf::st_sfc(endpoint_geometry, crs = sf::st_crs(fragments))
  )

  if (is.null(boundary)) {
    boundary = sf::st_as_sfc(sf::st_bbox(fragments))
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
#' @param layer_height_column Default `NULL`. Optional feature clearance column.
#' @param boundary Default `NULL`. Optional supplied-data boundary geometry.
#' @param minimum_step Default `1e-3`. Minimum retained coordinate separation
#' in metres.
#' @param boundary_tolerance Default `1`. Boundary endpoint tolerance in metres.
#'
#' @return Prepared source fragments, metric fragments, endpoints, and metadata.
#' @keywords internal
prepare_render_road_layer_features = function(
  roads,
  layer_column,
  layer_height_column = NULL,
  boundary = NULL,
  minimum_step = 1e-3,
  boundary_tolerance = 1
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for road layer topology.", call. = FALSE)
  }
  if (!inherits(roads, "sf")) {
    stop("Road layer topology requires an `sf` road object.", call. = FALSE)
  }
  layer_column = validate_render_road_column_name(layer_column, "layer")
  if (!(layer_column %in% names(roads))) {
    stop(sprintf("`layer` must name a column in `roads`: %s", layer_column))
  }
  geometry_type = as.character(sf::st_geometry_type(roads))
  supported = geometry_type %in% c("LINESTRING", "MULTILINESTRING")
  if (!all(supported)) {
    stop(
      "Road layer topology supports LINESTRING and MULTILINESTRING features.",
      call. = FALSE
    )
  }

  roads$render_road_feature_id = seq_len(nrow(roads))
  source_crs = sf::st_crs(roads)
  metric_crs = resolve_render_road_metric_crs(roads)
  source_fragments = suppressWarnings(sf::st_cast(roads, "LINESTRING"))
  source_fragments$render_road_fragment_id = seq_len(nrow(source_fragments))
  metric_fragments = sf::st_transform(source_fragments, metric_crs)
  cleaned = clean_render_road_line_fragments(
    source_fragments = source_fragments,
    metric_fragments = metric_fragments,
    minimum_step = minimum_step
  )
  source_fragments = cleaned$source
  metric_fragments = cleaned$metric

  raw_layer = source_fragments[[layer_column]]
  if (is.factor(raw_layer)) {
    raw_layer = as.character(raw_layer)
  }
  layer_explicit = !is.na(raw_layer)
  if (is.character(raw_layer)) {
    layer_explicit = layer_explicit & nzchar(trimws(raw_layer))
  }
  effective_layer = suppressWarnings(as.numeric(raw_layer))
  if (any(layer_explicit & !is.finite(effective_layer))) {
    stop("Explicit road layer values must be finite numbers.", call. = FALSE)
  }
  effective_layer[!layer_explicit] = 0

  clearance = rep(NA_real_, nrow(source_fragments))
  if (!is.null(layer_height_column)) {
    layer_height_column = validate_render_road_column_name(
      layer_height_column,
      "layer_height"
    )
    if (!(layer_height_column %in% names(source_fragments))) {
      stop(sprintf(
        "`layer_height` must name a column in `roads`: %s",
        layer_height_column
      ))
    }
    clearance = suppressWarnings(as.numeric(source_fragments[[
      layer_height_column
    ]]))
    present = !is.na(source_fragments[[layer_height_column]])
    if (any(present & (!is.finite(clearance) | clearance <= 0))) {
      stop("Explicit road layer heights must be positive finite numbers.")
    }
  }

  metadata_value = function(candidates) {
    column = candidates[candidates %in% names(source_fragments)][1L]
    if (!length(column) || is.na(column)) {
      return(rep(NA_character_, nrow(source_fragments)))
    }
    as.character(source_fragments[[column]])
  }
  topology_columns = list(
    render_road_layer = effective_layer,
    render_road_layer_explicit = layer_explicit,
    render_road_clearance = clearance,
    render_road_way_id = metadata_value(c("osm_id", "way_id", "id")),
    render_road_ref = metadata_value("ref"),
    render_road_name = metadata_value("name"),
    render_road_highway = metadata_value("highway"),
    render_road_bridge = metadata_value("bridge"),
    render_road_tunnel = metadata_value("tunnel"),
    render_road_location = metadata_value("location")
  )
  for (column in names(topology_columns)) {
    source_fragments[[column]] = topology_columns[[column]]
    metric_fragments[[column]] = topology_columns[[column]]
  }

  endpoints = build_render_road_endpoint_table(
    fragments = metric_fragments,
    boundary = boundary,
    boundary_tolerance = boundary_tolerance
  )
  endpoint_fragment = match(
    endpoints$render_road_fragment_id,
    metric_fragments$render_road_fragment_id
  )
  for (column in c(
    "render_road_feature_id",
    "render_road_layer",
    "render_road_layer_explicit",
    "render_road_way_id",
    "render_road_ref",
    "render_road_name",
    "render_road_highway"
  )) {
    endpoints[[column]] = metric_fragments[[column]][endpoint_fragment]
  }

  list(
    source_fragments = source_fragments,
    fragments = metric_fragments,
    endpoints = endpoints,
    source_crs = source_crs,
    metric_crs = metric_crs,
    layer_column = layer_column,
    layer_height_column = layer_height_column,
    dropped_fragment_id = cleaned$dropped_fragment_id,
    minimum_step = minimum_step,
    boundary_tolerance = boundary_tolerance
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
  coordinates = unclass(geometry)[, 1:2, drop = FALSE]
  if (nrow(coordinates) < 2L) {
    return(NULL)
  }
  segment = coordinates[-1L, , drop = FALSE] -
    coordinates[-nrow(coordinates), , drop = FALSE]
  segment_length_squared = rowSums(segment^2)
  valid = is.finite(segment_length_squared) & segment_length_squared > 0
  if (!any(valid)) {
    return(NULL)
  }
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
  segment_length = sqrt(segment_length_squared)
  cumulative = c(0, cumsum(segment_length))
  distance = cumulative[[closest]] +
    fraction[[closest]] * segment_length[[closest]]
  total_length = tail(cumulative, 1L)
  tolerance = max(endpoint_tolerance, total_length * 1e-10)
  list(
    distance = distance,
    total_length = total_length,
    separation = sqrt(separation_squared[[closest]]),
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
  intersection_crs = sf::st_crs(intersection)
  empty_points = sf::st_sfc(crs = intersection_crs)
  empty_lines = sf::st_sfc(crs = intersection_crs)
  if (
    is.null(intersection) ||
      !length(intersection) ||
      all(sf::st_is_empty(intersection))
  ) {
    return(list(points = empty_points, lines = empty_lines))
  }

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
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The `igraph` package is required for road layer topology.")
  }
  event_id_column = paste0(event_name, "_id")
  if (!nrow(pair_events)) {
    events = sf::st_sf(
      event_id = integer(0),
      participant_count = integer(0),
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

  neighbors = sf::st_is_within_distance(
    pair_events,
    pair_events,
    dist = tolerance
  )
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
  graph = igraph::make_empty_graph(n = nrow(pair_events), directed = FALSE)
  if (nrow(edges)) {
    graph = igraph::add_edges(graph, as.vector(t(edges)))
  }
  membership = igraph::components(graph)$membership
  membership = match(membership, unique(membership))
  pair_events[[event_id_column]] = membership

  event_groups = split(seq_len(nrow(pair_events)), membership)
  event_geometry = sf::st_geometry(pair_events)[vapply(
    event_groups,
    `[[`,
    integer(1),
    1L
  )]
  event_rows = lapply(seq_along(event_groups), function(event_id) {
    rows = event_groups[[event_id]]
    fragments_present = unique(c(
      pair_events$fragment_a[rows],
      pair_events$fragment_b[rows]
    ))
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
      )
    )
  })
  events = sf::st_sf(
    do.call(rbind, event_rows),
    geometry = event_geometry,
    crs = sf::st_crs(pair_events)
  )
  names(events)[names(events) == "event_id"] = event_id_column

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
  pairs = sf::st_drop_geometry(pair_events)
  list(events = events, participants = participants, pairs = pairs)
}

#' Find exact local road layer events
#'
#' @param prepared Prepared road layer features.
#' @param endpoint_tolerance Default `1e-2`. Endpoint and event tolerance in
#' metres.
#'
#' @return Crossing, junction, no-layer-relationship, and overlap event tables.
#' @keywords internal
find_render_road_layer_events = function(
  prepared,
  endpoint_tolerance = 1e-2
) {
  fragments = prepared$fragments
  fragment_count = nrow(fragments)
  empty_pair_events = sf::st_sf(
    fragment_a = integer(0),
    fragment_b = integer(0),
    distance_a = numeric(0),
    distance_b = numeric(0),
    endpoint_a = logical(0),
    endpoint_b = logical(0),
    geometry = sf::st_sfc(crs = sf::st_crs(fragments))
  )
  empty_overlaps = sf::st_sf(
    overlap_id = integer(0),
    fragment_a = integer(0),
    fragment_b = integer(0),
    layer_relationship = logical(0),
    distance_a_start = numeric(0),
    distance_a_end = numeric(0),
    distance_b_start = numeric(0),
    distance_b_end = numeric(0),
    geometry = sf::st_sfc(crs = sf::st_crs(fragments))
  )
  if (fragment_count < 2L) {
    crossings = group_render_road_point_events(
      empty_pair_events,
      fragments,
      "crossing",
      endpoint_tolerance
    )
    junctions = group_render_road_point_events(
      empty_pair_events,
      fragments,
      "junction",
      endpoint_tolerance
    )
    no_relationship = group_render_road_point_events(
      empty_pair_events,
      fragments,
      "no_relationship",
      endpoint_tolerance
    )
    return(list(
      crossings = crossings,
      junctions = junctions,
      no_relationship = no_relationship,
      overlaps = empty_overlaps,
      candidate_pair_count = 0L
    ))
  }

  candidate_index = sf::st_intersects(fragments, sparse = TRUE)
  candidate_pairs = lapply(seq_along(candidate_index), function(first) {
    second = candidate_index[[first]]
    second = second[second > first]
    if (!length(second)) {
      return(NULL)
    }
    cbind(first = first, second = second)
  })
  candidate_pairs = Filter(Negate(is.null), candidate_pairs)
  candidate_pairs = if (length(candidate_pairs)) {
    do.call(rbind, candidate_pairs)
  } else {
    matrix(integer(0), ncol = 2L)
  }

  pair_results = lapply(seq_len(nrow(candidate_pairs)), function(pair_index) {
    row_a = candidate_pairs[pair_index, 1L]
    row_b = candidate_pairs[pair_index, 2L]
    fragment_a = fragments$render_road_fragment_id[[row_a]]
    fragment_b = fragments$render_road_fragment_id[[row_b]]
    intersection = tryCatch(
      suppressWarnings(sf::st_intersection(
        sf::st_geometry(fragments)[row_a],
        sf::st_geometry(fragments)[row_b]
      )),
      error = function(error) NULL
    )
    parts = extract_render_road_topology_intersection_parts(intersection)

    point_rows = lapply(seq_along(parts$points), function(point_index) {
      point = sf::st_coordinates(parts$points[point_index])[1L, 1:2]
      projection_a = project_render_road_topology_point(
        sf::st_geometry(fragments)[[row_a]],
        point,
        endpoint_tolerance
      )
      projection_b = project_render_road_topology_point(
        sf::st_geometry(fragments)[[row_b]],
        point,
        endpoint_tolerance
      )
      if (is.null(projection_a) || is.null(projection_b)) {
        return(NULL)
      }
      endpoint_a = projection_a$at_start || projection_a$at_end
      endpoint_b = projection_b$at_start || projection_b$at_end
      classification = if (endpoint_a || endpoint_b) {
        "junction"
      } else if (
        fragments$render_road_layer[[row_a]] !=
          fragments$render_road_layer[[row_b]]
      ) {
        "crossing"
      } else {
        "no_relationship"
      }
      sf::st_sf(
        fragment_a = fragment_a,
        fragment_b = fragment_b,
        distance_a = projection_a$distance,
        distance_b = projection_b$distance,
        endpoint_a = endpoint_a,
        endpoint_b = endpoint_b,
        classification = classification,
        geometry = parts$points[point_index]
      )
    })
    point_rows = Filter(Negate(is.null), point_rows)

    overlap_rows = lapply(seq_along(parts$lines), function(line_index) {
      line = parts$lines[line_index]
      line_coordinates = sf::st_coordinates(line)[, 1:2, drop = FALSE]
      endpoint_coordinates = line_coordinates[
        c(1L, nrow(line_coordinates)),
        ,
        drop = FALSE
      ]
      projections_a = lapply(seq_len(2L), function(endpoint) {
        project_render_road_topology_point(
          sf::st_geometry(fragments)[[row_a]],
          endpoint_coordinates[endpoint, ],
          endpoint_tolerance
        )
      })
      projections_b = lapply(seq_len(2L), function(endpoint) {
        project_render_road_topology_point(
          sf::st_geometry(fragments)[[row_b]],
          endpoint_coordinates[endpoint, ],
          endpoint_tolerance
        )
      })
      if (any(vapply(c(projections_a, projections_b), is.null, logical(1)))) {
        return(NULL)
      }
      sf::st_sf(
        fragment_a = fragment_a,
        fragment_b = fragment_b,
        layer_relationship = fragments$render_road_layer[[row_a]] !=
          fragments$render_road_layer[[row_b]],
        distance_a_start = projections_a[[1L]]$distance,
        distance_a_end = projections_a[[2L]]$distance,
        distance_b_start = projections_b[[1L]]$distance,
        distance_b_end = projections_b[[2L]]$distance,
        geometry = line
      )
    })
    overlap_rows = Filter(Negate(is.null), overlap_rows)
    list(points = point_rows, overlaps = overlap_rows)
  })
  point_rows = unlist(lapply(pair_results, `[[`, "points"), recursive = FALSE)
  overlap_rows = unlist(
    lapply(pair_results, `[[`, "overlaps"),
    recursive = FALSE
  )
  all_points = if (length(point_rows)) {
    do.call(rbind, point_rows)
  } else {
    empty_pair_events
  }
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

  event_group = function(classification, event_name) {
    selected = if (nrow(all_points)) {
      all_points[all_points$classification == classification, , drop = FALSE]
    } else {
      empty_pair_events
    }
    selected$classification = NULL
    group_render_road_point_events(
      selected,
      fragments,
      event_name,
      endpoint_tolerance
    )
  }
  list(
    crossings = event_group("crossing", "crossing"),
    junctions = event_group("junction", "junction"),
    no_relationship = event_group("no_relationship", "no_relationship"),
    overlaps = overlaps,
    candidate_pair_count = nrow(candidate_pairs)
  )
}

# Endpoint continuations and topology graph -------------------------------

#' Select conservative road endpoint continuations
#'
#' @param prepared Prepared road layer features.
#' @param endpoint_tolerance Default `1e-2`. Exact endpoint tolerance in metres.
#' @param continuation_tolerance Default `0.25`. Maximum true geometry gap in
#' metres.
#' @param minimum_direction_score Default `cos(pi / 6)`. Minimum directional
#' agreement for a continuation candidate.
#'
#' @return Selected, ambiguous, and rejected continuation diagnostics.
#' @keywords internal
select_render_road_continuations = function(
  prepared,
  endpoint_tolerance = 1e-2,
  continuation_tolerance = 0.25,
  minimum_direction_score = cos(pi / 6)
) {
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
    exact_endpoint = logical(0),
    metadata_match = character(0),
    selection_score = numeric(0),
    selection_reason = character(0),
    status = character(0),
    geometry = sf::st_sfc(crs = sf::st_crs(endpoints))
  )
  if (nrow(endpoints) < 2L) {
    return(list(
      selected = empty,
      ambiguous = empty,
      rejected = empty,
      candidates = empty
    ))
  }
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
      ambiguous = empty,
      rejected = empty,
      candidates = empty
    ))
  }
  pair_index = do.call(rbind, pair_index)

  clean_metadata = function(value) {
    value = trimws(as.character(value))
    value[is.na(value) | !nzchar(value)] = NA_character_
    tolower(value)
  }
  endpoint_coordinates = sf::st_coordinates(endpoints)[, 1:2, drop = FALSE]
  candidate_rows = lapply(seq_len(nrow(pair_index)), function(candidate_id) {
    first = pair_index[candidate_id, 1L]
    second = pair_index[candidate_id, 2L]
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

    way_a = clean_metadata(endpoints$render_road_way_id[[first]])
    way_b = clean_metadata(endpoints$render_road_way_id[[second]])
    ref_a = clean_metadata(endpoints$render_road_ref[[first]])
    ref_b = clean_metadata(endpoints$render_road_ref[[second]])
    name_a = clean_metadata(endpoints$render_road_name[[first]])
    name_b = clean_metadata(endpoints$render_road_name[[second]])
    highway_a = clean_metadata(endpoints$render_road_highway[[first]])
    highway_b = clean_metadata(endpoints$render_road_highway[[second]])
    same_value = function(first_value, second_value) {
      !is.na(first_value) &&
        !is.na(second_value) &&
        identical(first_value, second_value)
    }
    same_way = same_value(way_a, way_b) ||
      endpoints$render_road_feature_id[[first]] ==
        endpoints$render_road_feature_id[[second]]
    same_ref = same_value(ref_a, ref_b)
    same_name = same_value(name_a, name_b)
    same_highway = same_value(highway_a, highway_b)
    metadata_match = c(
      if (same_way) "way",
      if (same_ref) "ref",
      if (same_name) "name",
      if (same_highway) "highway"
    )
    if (!length(metadata_match)) {
      metadata_match = "direction"
    }
    selection_score = 10000 *
      exact_endpoint +
      1000 *
        same_way +
      100 * same_ref +
      50 * same_name +
      10 * same_highway +
      direction_score
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
      exact_endpoint = exact_endpoint,
      metadata_match = paste(metadata_match, collapse = "+"),
      selection_score = selection_score,
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
  })
  candidates = do.call(rbind, candidate_rows)
  eligible_rows = which(candidates$eligible)
  best_candidate = integer(nrow(endpoints))
  ambiguous_endpoint = logical(nrow(endpoints))
  for (endpoint_row in seq_len(nrow(endpoints))) {
    endpoint_id = endpoints$render_road_endpoint_id[[endpoint_row]]
    rows = eligible_rows[
      candidates$endpoint_a[eligible_rows] == endpoint_id |
        candidates$endpoint_b[eligible_rows] == endpoint_id
    ]
    if (!length(rows)) {
      next
    }
    score = candidates$selection_score[rows]
    best = rows[score >= max(score) - 1e-8]
    if (length(best) == 1L) {
      best_candidate[[endpoint_row]] = best
    } else {
      ambiguous_endpoint[[endpoint_row]] = TRUE
    }
  }

  endpoint_row = setNames(
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
      ambiguous_endpoint[[first]] ||
        ambiguous_endpoint[[second]]
    ) {
      candidates$status[[row]] = "ambiguous"
      candidates$selection_reason[[row]] = "ambiguous_best_match"
    } else {
      candidates$status[[row]] = "rejected"
      candidates$selection_reason[[row]] = "non_mutual_best_match"
    }
  }
  candidates$eligible = NULL
  candidates$continuation_id = seq_len(nrow(candidates))
  list(
    selected = candidates[candidates$status == "selected", , drop = FALSE],
    ambiguous = candidates[candidates$status == "ambiguous", , drop = FALSE],
    rejected = candidates[candidates$status == "rejected", , drop = FALSE],
    candidates = candidates
  )
}

#' Build an igraph road topology graph
#'
#' @param prepared Prepared road layer features.
#' @param events Exact road layer events.
#' @param continuations Continuation selection diagnostics.
#'
#' @return Fragment graph and solve-component membership.
#' @keywords internal
build_render_road_topology_graph = function(prepared, events, continuations) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The `igraph` package is required for road layer topology.")
  }
  fragments = prepared$fragments
  fragment_id = fragments$render_road_fragment_id
  vertices = data.frame(
    name = as.character(fragment_id),
    render_road_fragment_id = fragment_id,
    render_road_feature_id = fragments$render_road_feature_id,
    render_road_layer = fragments$render_road_layer,
    stringsAsFactors = FALSE
  )

  participant_edges = function(participants, event_id_column, edge_type) {
    if (!nrow(participants)) {
      return(NULL)
    }
    groups = split(
      participants$render_road_fragment_id,
      participants[[event_id_column]]
    )
    rows = lapply(groups, function(group) {
      group = unique(group)
      if (length(group) < 2L) {
        return(NULL)
      }
      pairs = utils::combn(group, 2L)
      data.frame(
        from = as.character(pairs[1L, ]),
        to = as.character(pairs[2L, ]),
        topology_type = edge_type,
        stringsAsFactors = FALSE
      )
    })
    rows = Filter(Negate(is.null), rows)
    if (length(rows)) do.call(rbind, rows) else NULL
  }
  edge_rows = list(
    participant_edges(
      events$crossings$participants,
      "crossing_id",
      "crossing"
    ),
    participant_edges(
      events$junctions$participants,
      "junction_id",
      "junction"
    )
  )
  if (nrow(events$overlaps)) {
    edge_rows[[length(edge_rows) + 1L]] = data.frame(
      from = as.character(events$overlaps$fragment_a),
      to = as.character(events$overlaps$fragment_b),
      topology_type = "overlap",
      stringsAsFactors = FALSE
    )
  }
  if (nrow(continuations$selected)) {
    edge_rows[[length(edge_rows) + 1L]] = data.frame(
      from = as.character(continuations$selected$fragment_a),
      to = as.character(continuations$selected$fragment_b),
      topology_type = "continuation",
      stringsAsFactors = FALSE
    )
  }
  edge_rows = Filter(function(value) !is.null(value) && nrow(value), edge_rows)
  edges = if (length(edge_rows)) {
    do.call(rbind, edge_rows)
  } else {
    data.frame(
      from = character(0),
      to = character(0),
      topology_type = character(0)
    )
  }
  graph = igraph::graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = vertices
  )
  component = igraph::components(graph)$membership
  components = data.frame(
    render_road_fragment_id = as.integer(names(component)),
    solve_component_id = as.integer(component),
    stringsAsFactors = FALSE
  )
  components = components[
    match(fragment_id, components$render_road_fragment_id),
  ]
  list(graph = graph, components = components, edges = edges)
}

#' Build complete local road layer topology
#'
#' @param prepared Prepared road layer features.
#' @param endpoint_tolerance Default `1e-2`. Exact topology tolerance in metres.
#' @param continuation_tolerance Default `0.25`. Maximum true continuation gap
#' in metres.
#'
#' @return Prepared features, events, continuations, graph, and diagnostics.
#' @keywords internal
build_render_road_layer_topology = function(
  prepared,
  endpoint_tolerance = 1e-2,
  continuation_tolerance = 0.25
) {
  events = find_render_road_layer_events(
    prepared = prepared,
    endpoint_tolerance = endpoint_tolerance
  )
  continuations = select_render_road_continuations(
    prepared = prepared,
    endpoint_tolerance = endpoint_tolerance,
    continuation_tolerance = continuation_tolerance
  )
  topology_graph = build_render_road_topology_graph(
    prepared = prepared,
    events = events,
    continuations = continuations
  )
  fragments = prepared$fragments
  fragments$solve_component_id = topology_graph$components$solve_component_id

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
    crossing_stack$local_order = ave(
      crossing_stack$render_road_layer,
      crossing_stack$crossing_id,
      FUN = seq_along
    )
  } else {
    crossing_stack$local_order = integer(0)
  }

  list(
    prepared = prepared,
    fragments = fragments,
    endpoints = prepared$endpoints,
    junctions = events$junctions$events,
    junction_participants = events$junctions$participants,
    crossings = events$crossings$events,
    crossing_participants = crossing_stack,
    crossing_pairs = events$crossings$pairs,
    no_layer_relationships = events$no_relationship$events,
    no_layer_relationship_participants = events$no_relationship$participants,
    overlaps = events$overlaps,
    selected_continuations = continuations$selected,
    ambiguous_continuations = continuations$ambiguous,
    rejected_continuations = continuations$rejected,
    continuation_candidates = continuations$candidates,
    graph = topology_graph$graph,
    graph_edges = topology_graph$edges,
    components = topology_graph$components,
    diagnostics = list(
      candidate_pair_count = events$candidate_pair_count,
      dropped_fragment_id = prepared$dropped_fragment_id,
      endpoint_tolerance = endpoint_tolerance,
      continuation_tolerance = continuation_tolerance
    )
  )
}

# Road topology diagnostics -----------------------------------------------

#' Resolve road topology layer colors
#'
#' @param layer Effective numeric road layers.
#' @param layer_colors Default `NULL`. Optional named or layer-ordered colors.
#'
#' @return A named character vector mapping layers to colors.
#' @keywords internal
resolve_render_road_topology_layer_colors = function(
  layer,
  layer_colors = NULL
) {
  layer_levels = as.character(sort(unique(layer)))
  default_colors = c(
    `-2` = "#2d004b",
    `-1` = "#7b3294",
    `0` = "#5f6368",
    `1` = "#e66101",
    `2` = "#d73027",
    `3` = "#a50026",
    `4` = "#67000d"
  )
  resolved = unname(default_colors[layer_levels])
  missing_color = is.na(resolved)
  if (any(missing_color)) {
    resolved[missing_color] = grDevices::hcl.colors(
      sum(missing_color),
      palette = "Dark 3"
    )
  }
  names(resolved) = layer_levels

  if (!is.null(layer_colors)) {
    layer_colors = as.character(layer_colors)
    color_names = names(layer_colors)
    if (is.null(color_names)) {
      if (length(layer_colors) != length(layer_levels)) {
        stop(
          "Unnamed `layer_colors` must supply one color per effective layer.",
          call. = FALSE
        )
      }
      names(layer_colors) = layer_levels
    } else if (any(is.na(color_names) | !nzchar(color_names))) {
      stop("Named `layer_colors` cannot contain empty names.", call. = FALSE)
    }
    matched = intersect(names(layer_colors), layer_levels)
    resolved[matched] = layer_colors[matched]
  }
  valid_colors = tryCatch(
    {
      grDevices::col2rgb(unname(resolved))
      TRUE
    },
    error = function(error) FALSE
  )
  if (!valid_colors) {
    stop("`layer_colors` contains an invalid color.", call. = FALSE)
  }
  resolved
}

#' Calculate a point along a road topology line
#'
#' @param geometry LINESTRING geometry.
#' @param fraction Default `0.5`. Fraction of line length used for the point.
#'
#' @return Two-value point coordinate.
#' @keywords internal
calculate_render_road_topology_line_point = function(
  geometry,
  fraction = 0.5
) {
  coordinates = unclass(geometry)[, 1:2, drop = FALSE]
  if (nrow(coordinates) < 2L) {
    return(rep(NA_real_, 2L))
  }
  fraction = suppressWarnings(as.numeric(fraction[[1L]]))
  if (!is.finite(fraction)) {
    fraction = 0.5
  }
  fraction = min(max(fraction, 0), 1)
  segment = coordinates[-1L, , drop = FALSE] -
    coordinates[-nrow(coordinates), , drop = FALSE]
  segment_length = sqrt(rowSums(segment^2))
  total_length = sum(segment_length)
  if (!is.finite(total_length) || total_length <= 0) {
    return(colMeans(coordinates[c(1L, nrow(coordinates)), , drop = FALSE]))
  }
  cumulative = c(0, cumsum(segment_length))
  target_distance = total_length * fraction
  segment_index = which(cumulative[-1L] >= target_distance)[1L]
  if (!length(segment_index) || is.na(segment_index)) {
    segment_index = length(segment_length)
  }
  local_fraction = if (segment_length[[segment_index]] > 0) {
    (target_distance - cumulative[[segment_index]]) /
      segment_length[[segment_index]]
  } else {
    0
  }
  coordinates[segment_index, ] + local_fraction * segment[segment_index, ]
}

#' Resolve the detailed road topology plot bounds
#'
#' @param topology Road topology diagnostics.
#' @param focus Default `NULL`. Optional metric focus bounds or center.
#'
#' @return Four named metric plot bounds.
#' @keywords internal
resolve_render_road_topology_focus = function(topology, focus = NULL) {
  fragments = topology$fragments
  complete_bounds = sf::st_bbox(fragments)
  span_x = complete_bounds[["xmax"]] - complete_bounds[["xmin"]]
  span_y = complete_bounds[["ymax"]] - complete_bounds[["ymin"]]
  detail_width = max(
    span_x * 0.36,
    topology$diagnostics$endpoint_tolerance * 20
  )
  detail_height = max(
    span_y * 0.36,
    topology$diagnostics$endpoint_tolerance * 20
  )

  if (!is.null(focus)) {
    if (inherits(focus, "bbox")) {
      focus = unclass(focus)
    }
    if (!is.numeric(focus) || !(length(focus) %in% c(2L, 4L))) {
      stop(
        "`focus` must be a two-value center or four-value metric bounding box.",
        call. = FALSE
      )
    }
    focus = as.numeric(focus)
    if (any(!is.finite(focus))) {
      stop("`focus` values must be finite.", call. = FALSE)
    }
    if (length(focus) == 4L) {
      bounds = setNames(focus, c("xmin", "ymin", "xmax", "ymax"))
      if (
        bounds[["xmax"]] <= bounds[["xmin"]] ||
          bounds[["ymax"]] <= bounds[["ymin"]]
      ) {
        stop("`focus` bounding-box maxima must exceed minima.", call. = FALSE)
      }
      return(bounds)
    }
    center = focus
  } else if (nrow(topology$crossings)) {
    crossing_geometry = sf::st_geometry(topology$crossings)
    density_radius = max(
      min(span_x, span_y) * 0.12,
      topology$diagnostics$endpoint_tolerance * 20
    )
    crossing_density = lengths(sf::st_is_within_distance(
      crossing_geometry,
      crossing_geometry,
      dist = density_radius
    ))
    center = sf::st_coordinates(
      crossing_geometry[which.max(crossing_density)]
    )[1L, 1:2]
  } else {
    center = c(
      mean(complete_bounds[c("xmin", "xmax")]),
      mean(complete_bounds[c("ymin", "ymax")])
    )
  }
  c(
    xmin = center[[1L]] - detail_width / 2,
    ymin = center[[2L]] - detail_height / 2,
    xmax = center[[1L]] + detail_width / 2,
    ymax = center[[2L]] + detail_height / 2
  )
}

#' Draw one road topology diagnostic panel
#'
#' @param topology Road topology diagnostics.
#' @param bounds Four named metric plot bounds.
#' @param title Panel title.
#' @param label_fragment_id Fragment identifiers to label.
#' @param layer_colors Named effective-layer colors.
#' @param label_components Whether to draw component identifiers.
#' @param show_legend Whether to draw the symbol legend.
#' @param road_width Road line width.
#' @param label_size Label size.
#'
#' @return Invisibly returns `topology`.
#' @keywords internal
draw_render_road_topology_panel = function(
  topology,
  bounds,
  title,
  label_fragment_id,
  layer_colors,
  label_components = TRUE,
  show_legend = TRUE,
  road_width = 2,
  label_size = 0.55
) {
  fragments = topology$fragments
  line_points = function(object, fraction = 0.5) {
    if (!nrow(object)) {
      return(matrix(numeric(0), ncol = 2L))
    }
    t(vapply(
      sf::st_geometry(object),
      calculate_render_road_topology_line_point,
      numeric(2),
      fraction = fraction
    ))
  }

  graphics::plot(
    sf::st_geometry(fragments),
    col = unname(layer_colors[as.character(fragments$render_road_layer)]),
    lwd = road_width,
    xlim = bounds[c("xmin", "xmax")],
    ylim = bounds[c("ymin", "ymax")],
    axes = TRUE,
    reset = FALSE,
    main = title
  )
  graphics::box(col = "#222222")

  if (nrow(topology$overlaps)) {
    graphics::plot(
      sf::st_geometry(topology$overlaps),
      add = TRUE,
      col = "#00a6a6",
      lwd = road_width + 4
    )
    graphics::plot(
      sf::st_geometry(topology$overlaps),
      add = TRUE,
      col = "#bdf2f2",
      lwd = max(2, road_width - 1)
    )
  }
  if (nrow(topology$junctions)) {
    graphics::plot(
      sf::st_geometry(topology$junctions),
      add = TRUE,
      pch = 21,
      bg = "#40c463",
      col = "#005a24",
      cex = max(0.45, min(1.1, road_width / 2.5))
    )
  }
  if (nrow(topology$crossings)) {
    graphics::plot(
      sf::st_geometry(topology$crossings),
      add = TRUE,
      pch = 4,
      col = "#d50000",
      lwd = 2,
      cex = max(0.55, min(1.25, road_width / 2))
    )
  }
  continuation_size = max(0.38, min(0.9, road_width / 5))
  if (nrow(topology$selected_continuations)) {
    graphics::plot(
      sf::st_geometry(topology$selected_continuations),
      add = TRUE,
      col = "#0878d1",
      lwd = max(1.2, road_width * 0.65)
    )
    graphics::points(
      line_points(topology$selected_continuations),
      pch = 23,
      bg = grDevices::adjustcolor("#37a8ff", alpha.f = 0.82),
      col = "#005a9c",
      cex = continuation_size
    )
  }
  if (nrow(topology$ambiguous_continuations)) {
    graphics::plot(
      sf::st_geometry(topology$ambiguous_continuations),
      add = TRUE,
      col = "#ed8b00",
      lwd = max(1.2, road_width * 0.5),
      lty = 2
    )
    graphics::points(
      line_points(topology$ambiguous_continuations),
      pch = 24,
      bg = "#ffb000",
      col = "#8c510a",
      cex = continuation_size * 1.1
    )
  }

  label_fragment_id = intersect(
    as.integer(label_fragment_id),
    fragments$render_road_fragment_id
  )
  if (length(label_fragment_id)) {
    label_rows = match(
      label_fragment_id,
      fragments$render_road_fragment_id
    )
    label_fraction = ifelse(seq_along(label_rows) %% 2L, 0.67, 0.36)
    label_points = t(vapply(
      seq_along(label_rows),
      function(label_index) {
        calculate_render_road_topology_line_point(
          sf::st_geometry(fragments)[[label_rows[[label_index]]]],
          label_fraction[[label_index]]
        )
      },
      numeric(2)
    ))
    graphics::text(
      label_points,
      labels = sprintf(
        "S%d/F%d L%s C%d",
        fragments$render_road_feature_id[label_rows],
        fragments$render_road_fragment_id[label_rows],
        fragments$render_road_layer[label_rows],
        fragments$solve_component_id[label_rows]
      ),
      pos = 3,
      cex = label_size,
      col = "#151515",
      xpd = NA
    )
  }

  if (isTRUE(label_components)) {
    fragment_midpoints = line_points(fragments)
    component_groups = split(
      seq_len(nrow(fragments)),
      fragments$solve_component_id
    )
    for (component in names(component_groups)) {
      component_point = colMeans(fragment_midpoints[
        component_groups[[component]],
        ,
        drop = FALSE
      ])
      graphics::text(
        component_point[[1L]],
        component_point[[2L]],
        labels = paste0("C", component),
        cex = 0.8,
        font = 2,
        col = grDevices::adjustcolor("#005a9c", alpha.f = 0.8),
        pos = 4
      )
    }
  }

  if (isTRUE(show_legend)) {
    layers_present = sort(unique(fragments$render_road_layer))
    layer_count = length(layers_present)
    graphics::legend(
      "bottomleft",
      legend = c(
        paste0("layer ", layers_present),
        "shared junction",
        "interior crossing",
        "overlap section",
        "selected continuation",
        "ambiguous continuation"
      ),
      col = c(
        unname(layer_colors[as.character(layers_present)]),
        "#005a24",
        "#d50000",
        "#00a6a6",
        "#0878d1",
        "#ed8b00"
      ),
      lwd = c(rep(4, layer_count), NA, 2, 6, 3, 2),
      lty = c(rep(1, layer_count), NA, NA, 1, 1, 2),
      pch = c(rep(NA, layer_count), 21, 4, NA, 23, 24),
      pt.bg = c(
        rep(NA, layer_count),
        "#40c463",
        NA,
        NA,
        "#37a8ff",
        "#ffb000"
      ),
      bg = grDevices::adjustcolor("white", alpha.f = 0.9),
      cex = 0.72
    )
  }
  graphics::mtext(
    sprintf(
      paste0(
        "%d fragments | %d junctions | %d crossings | %d overlaps | ",
        "%d selected | %d ambiguous"
      ),
      nrow(fragments),
      nrow(topology$junctions),
      nrow(topology$crossings),
      nrow(topology$overlaps),
      nrow(topology$selected_continuations),
      nrow(topology$ambiguous_continuations)
    ),
    side = 1,
    line = 3,
    cex = 0.75
  )
  invisible(topology)
}

#' Plot Road Layer Topology
#'
#' @description Processes road features into the local layer-event topology
#' used by [render_roads()] and draws reproducible two-dimensional diagnostics.
#' The plot distinguishes effective layers, source and fragment IDs, shared
#' junctions, interior crossings, overlaps, selected and ambiguous
#' continuations, and solve components.
#'
#' @param roads Road `sf` LINESTRING or MULTILINESTRING features.
#' @param layer An unquoted or character column name containing
#' OpenStreetMap-style layer values. Missing values are treated as implicit
#' layer `0` while retaining their implicit status.
#' @param layer_height Default `5.5`. A positive constant clearance or an
#' unquoted or character column name containing positive feature clearances.
#' @param boundary Default `NULL`. Optional supplied-data boundary geometry.
#' Endpoints near this boundary are marked in the returned diagnostics.
#' @param boundary_tolerance Default `1`. Distance in metres used to identify
#' endpoints on the supplied-data boundary.
#' @param endpoint_tolerance Default `1e-2`. Exact topology tolerance in metres.
#' @param continuation_tolerance Default `0.25`. Maximum true continuation gap
#' in metres.
#' @param views Default `c("overview", "crossing_detail")`. Panels to draw.
#' Supported values are `"overview"` and `"crossing_detail"`.
#' @param focus Default `NULL`. Optional two-value metric center or four-value
#' metric bounding box for the crossing-detail panel. By default, the function
#' locates the densest crossing neighborhood.
#' @param labels Default `TRUE`. Whether to label source, fragment, layer, and
#' component IDs.
#' @param max_labels Default `40`. Maximum number of fragment labels per panel.
#' @param label_components Default `TRUE`. Whether to draw component IDs.
#' @param layer_colors Default `NULL`. Optional named or layer-ordered colors.
#' @param main Default `NULL`. Optional title or one title per requested view.
#' @param filename Default `NULL`. Optional PNG output path. When omitted, the
#' diagnostic is drawn on the active graphics device.
#' @param width Default `2400`. PNG width in pixels.
#' @param height Default `1400`. PNG height in pixels.
#' @param res Default `150`. PNG resolution in pixels per inch.
#'
#' @return Invisibly returns a `render_road_topology` list containing prepared
#' fragments, exact events, continuation diagnostics, the `igraph` graph,
#' component membership, and plot metadata.
#' @export
plot_render_road_topology = function(
  roads,
  layer,
  layer_height = 5.5,
  boundary = NULL,
  boundary_tolerance = 1,
  endpoint_tolerance = 1e-2,
  continuation_tolerance = 0.25,
  views = c("overview", "crossing_detail"),
  focus = NULL,
  labels = TRUE,
  max_labels = 40,
  label_components = TRUE,
  layer_colors = NULL,
  main = NULL,
  filename = NULL,
  width = 2400,
  height = 1400,
  res = 150
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for road topology plots.", call. = FALSE)
  }
  if (!inherits(roads, "sf") || !nrow(roads)) {
    stop("`roads` must be a non-empty `sf` road object.", call. = FALSE)
  }
  layer_column = resolve_render_road_column(
    value = layer,
    value_expr = substitute(layer),
    value_missing = missing(layer),
    argument = "layer"
  )
  if (is.null(layer_column)) {
    stop("`layer` must name a column in `roads`.", call. = FALSE)
  }
  layer_height_spec = resolve_render_road_layer_height(
    value = layer_height,
    value_expr = substitute(layer_height),
    value_missing = missing(layer_height)
  )
  if (!is.null(layer_height_spec$spacing)) {
    spacing = suppressWarnings(as.numeric(layer_height_spec$spacing[[1L]]))
    if (!is.finite(spacing) || spacing <= 0) {
      stop("Numeric `layer_height` must be positive and finite.", call. = FALSE)
    }
    layer_height_spec$spacing = spacing
  }
  validate_tolerance = function(value, argument, allow_zero = FALSE) {
    if (!is.numeric(value) || length(value) != 1L) {
      stop(sprintf("`%s` must be a single number.", argument), call. = FALSE)
    }
    value = suppressWarnings(as.numeric(value[[1L]]))
    invalid = !is.finite(value) || if (allow_zero) value < 0 else value <= 0
    if (invalid) {
      qualifier = if (allow_zero) "non-negative" else "positive"
      stop(
        sprintf("`%s` must be %s and finite.", argument, qualifier),
        call. = FALSE
      )
    }
    value
  }
  boundary_tolerance = validate_tolerance(
    boundary_tolerance,
    "boundary_tolerance",
    allow_zero = TRUE
  )
  endpoint_tolerance = validate_tolerance(
    endpoint_tolerance,
    "endpoint_tolerance"
  )
  continuation_tolerance = validate_tolerance(
    continuation_tolerance,
    "continuation_tolerance"
  )

  supported_views = c("overview", "crossing_detail")
  if (
    !is.character(views) ||
      !length(views) ||
      any(is.na(views) | !(views %in% supported_views))
  ) {
    stop(
      "`views` must contain `\"overview\"` or `\"crossing_detail\"`.",
      call. = FALSE
    )
  }
  views = unique(views)
  if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) {
    stop("`labels` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (
    !is.logical(label_components) ||
      length(label_components) != 1L ||
      is.na(label_components)
  ) {
    stop("`label_components` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (
    !is.numeric(max_labels) ||
      length(max_labels) != 1L ||
      !is.finite(max_labels) ||
      max_labels < 0 ||
      max_labels != floor(max_labels)
  ) {
    stop("`max_labels` must be a non-negative integer.", call. = FALSE)
  }
  max_labels = as.integer(max_labels)

  prepared = prepare_render_road_layer_features(
    roads = roads,
    layer_column = layer_column,
    layer_height_column = layer_height_spec$column,
    boundary = boundary,
    boundary_tolerance = boundary_tolerance
  )
  topology = build_render_road_layer_topology(
    prepared = prepared,
    endpoint_tolerance = endpoint_tolerance,
    continuation_tolerance = continuation_tolerance
  )
  topology$layer_spacing = layer_height_spec$spacing
  topology$layer_height_column = layer_height_spec$column
  class(topology) = c("render_road_topology", class(topology))

  complete_bounds = sf::st_bbox(topology$fragments)
  span_x = complete_bounds[["xmax"]] - complete_bounds[["xmin"]]
  span_y = complete_bounds[["ymax"]] - complete_bounds[["ymin"]]
  padding_reference = max(
    span_x,
    span_y,
    topology$diagnostics$endpoint_tolerance * 20
  )
  overview_bounds = c(
    xmin = complete_bounds[["xmin"]] -
      max(span_x * 0.08, padding_reference * 0.01),
    ymin = complete_bounds[["ymin"]] -
      max(span_y * 0.08, padding_reference * 0.01),
    xmax = complete_bounds[["xmax"]] +
      max(span_x * 0.08, padding_reference * 0.01),
    ymax = complete_bounds[["ymax"]] +
      max(span_y * 0.08, padding_reference * 0.01)
  )
  detail_bounds = resolve_render_road_topology_focus(topology, focus)
  panel_bounds = list(
    overview = overview_bounds,
    crossing_detail = detail_bounds
  )
  resolved_colors = resolve_render_road_topology_layer_colors(
    topology$fragments$render_road_layer,
    layer_colors
  )

  sample_fragment_ids = function(fragment_id) {
    fragment_id = unique(as.integer(fragment_id))
    if (!isTRUE(labels) || !max_labels || !length(fragment_id)) {
      return(integer(0))
    }
    if (length(fragment_id) <= max_labels) {
      return(fragment_id)
    }
    fragment_id[unique(round(seq(
      1,
      length(fragment_id),
      length.out = max_labels
    )))]
  }
  fragments = topology$fragments
  overview_candidates = fragments$render_road_fragment_id[
    fragments$render_road_layer_explicit
  ]
  if (!length(overview_candidates)) {
    overview_candidates = fragments$render_road_fragment_id
  }
  detail_polygon = sf::st_as_sfc(sf::st_bbox(
    detail_bounds,
    crs = sf::st_crs(fragments)
  ))
  detail_rows = which(
    lengths(sf::st_intersects(
      fragments,
      detail_polygon
    )) >
      0L
  )
  event_fragments = unique(c(
    topology$crossing_participants$render_road_fragment_id,
    topology$junction_participants$render_road_fragment_id
  ))
  detail_candidates = intersect(
    fragments$render_road_fragment_id[detail_rows],
    event_fragments
  )
  if (!length(detail_candidates)) {
    detail_candidates = fragments$render_road_fragment_id[detail_rows]
  }
  panel_labels = list(
    overview = sample_fragment_ids(overview_candidates),
    crossing_detail = sample_fragment_ids(detail_candidates)
  )

  default_title = c(
    overview = "Road layer topology overview",
    crossing_detail = "Densest crossing neighborhood"
  )
  if (is.null(main)) {
    panel_title = unname(default_title[views])
  } else {
    main = as.character(main)
    if (!(length(main) %in% c(1L, length(views))) || any(is.na(main))) {
      stop(
        "`main` must contain one title or one title per view.",
        call. = FALSE
      )
    }
    panel_title = rep(main, length.out = length(views))
  }

  opened_device = FALSE
  if (!is.null(filename)) {
    if (
      !is.character(filename) ||
        length(filename) != 1L ||
        is.na(filename) ||
        !nzchar(filename)
    ) {
      stop("`filename` must be a single non-empty path.", call. = FALSE)
    }
    dimensions = suppressWarnings(as.numeric(c(width, height, res)))
    if (
      length(dimensions) != 3L || any(!is.finite(dimensions) | dimensions <= 0)
    ) {
      stop(
        "`width`, `height`, and `res` must be positive numbers.",
        call. = FALSE
      )
    }
    if (!dir.exists(dirname(filename))) {
      stop("The directory containing `filename` does not exist.", call. = FALSE)
    }
    grDevices::png(
      filename = filename,
      width = dimensions[[1L]],
      height = dimensions[[2L]],
      res = dimensions[[3L]]
    )
    opened_device = TRUE
  }
  previous_parameters = graphics::par(no.readonly = TRUE)
  on.exit(
    {
      try(graphics::par(previous_parameters), silent = TRUE)
      if (opened_device) {
        grDevices::dev.off()
      }
    },
    add = TRUE
  )
  graphics::par(
    mfrow = c(1L, length(views)),
    mar = c(5.2, 4.2, 4.2, 1.2)
  )
  for (panel_index in seq_along(views)) {
    view = views[[panel_index]]
    draw_render_road_topology_panel(
      topology = topology,
      bounds = panel_bounds[[view]],
      title = panel_title[[panel_index]],
      label_fragment_id = panel_labels[[view]],
      layer_colors = resolved_colors,
      label_components = label_components && identical(view, "overview"),
      show_legend = identical(panel_index, 1L),
      road_width = if (identical(view, "overview")) 2 else 3,
      label_size = if (identical(view, "overview")) 0.5 else 0.55
    )
  }

  topology$plot = list(
    views = views,
    bounds = panel_bounds[views],
    layer_colors = resolved_colors,
    filename = filename
  )
  invisible(topology)
}

#' Elevate road paths from local layer ordering
#'
#' @param coord_list List of terrain-sampled scene coordinate matrices.
#' @param layer Effective numeric layer for each path.
#' @param layer_explicit Whether each path had a non-missing layer value.
#' @param layer_spacing Constant spacing between locally ordered layers.
#' @param layer_height Default `NULL`. Optional feature separation above the
#' lower road at each intersection.
#' @param zscale Effective scene zscale.
#' @param texture_world_scale Two-value x-z multiplier from scene units to
#' world units.
#'
#' @return List of locally elevated road coordinate matrices.
#' @keywords internal
elevate_render_road_layer_coords = function(
  coord_list,
  layer,
  layer_explicit,
  layer_spacing = 5.5,
  layer_height = NULL,
  zscale = 1,
  texture_world_scale = c(1, 1)
) {
  path_count = length(coord_list)
  if (!path_count) {
    return(coord_list)
  }
  if (
    length(layer) != path_count ||
      length(layer_explicit) != path_count
  ) {
    stop(
      "Layer values must have one entry for each rendered road path.",
      call. = FALSE
    )
  }
  layer = suppressWarnings(as.numeric(layer))
  layer[!is.finite(layer)] = 0
  layer_explicit = as.logical(layer_explicit)
  layer_explicit[is.na(layer_explicit)] = FALSE
  zscale = validate_waterpath_positive_number(zscale, "zscale")
  if (is.null(layer_height)) {
    layer_spacing = validate_waterpath_positive_number(
      layer_spacing,
      "layer_height"
    )
  } else {
    layer_height = suppressWarnings(as.numeric(layer_height))
    if (length(layer_height) != path_count) {
      stop(
        "Layer heights must have one entry for each rendered road path.",
        call. = FALSE
      )
    }
  }
  if (!any(layer_explicit)) {
    attr(coord_list, "terrain_following") = rep(TRUE, path_count)
    return(coord_list)
  }

  coord_list = lapply(
    coord_list,
    collapse_render_highquality_road_path_points,
    texture_world_scale = texture_world_scale
  )
  terrain_coord_list = coord_list
  intersections = find_render_road_layer_intersections(
    coord_list = coord_list,
    layer_explicit = layer_explicit,
    texture_world_scale = texture_world_scale
  )
  endpoint_graph = build_render_road_endpoint_graph(
    coord_list = coord_list,
    layer = layer,
    layer_explicit = layer_explicit,
    texture_world_scale = texture_world_scale
  )
  layer_chains = build_render_road_layer_chains(
    coord_list = coord_list,
    layer = layer,
    layer_explicit = layer_explicit,
    texture_world_scale = texture_world_scale
  )
  edges = unique(intersections[, c("path_a", "path_b"), drop = FALSE])
  if (nrow(endpoint_graph$same_layer_edges)) {
    edges = unique(rbind(edges, endpoint_graph$same_layer_edges))
  }
  if (nrow(layer_chains$same_layer_edges)) {
    edges = unique(rbind(edges, layer_chains$same_layer_edges))
  }
  component = calculate_render_road_layer_components(
    path_count = path_count,
    edges = edges
  )
  local_rank = integer(path_count)
  for (component_id in unique(component[component > 0L])) {
    component_paths = which(component == component_id)
    ordered_layers = sort(unique(layer[component_paths]))
    local_rank[component_paths] =
      match(layer[component_paths], ordered_layers) - 1L
  }

  terrain_following = rep(TRUE, path_count)
  maximum_rank = max(local_rank)
  if (maximum_rank > 0L) {
    for (current_rank in seq_len(maximum_rank)) {
      anchor_distance = vector("list", path_count)
      anchor_height = vector("list", path_count)
      for (event_index in seq_len(nrow(intersections))) {
        path_a = intersections$path_a[[event_index]]
        path_b = intersections$path_b[[event_index]]
        rank_a = local_rank[[path_a]]
        rank_b = local_rank[[path_b]]
        if (rank_a == current_rank && rank_b < current_rank) {
          upper_path = path_a
          upper_distance = intersections$distance_a[[event_index]]
          lower_path = path_b
          lower_distance = intersections$distance_b[[event_index]]
          lower_rank = rank_b
        } else if (rank_b == current_rank && rank_a < current_rank) {
          upper_path = path_b
          upper_distance = intersections$distance_b[[event_index]]
          lower_path = path_a
          lower_distance = intersections$distance_a[[event_index]]
          lower_rank = rank_a
        } else {
          next
        }
        lower_height = interpolate_render_road_path_height(
          coords = coord_list[[lower_path]],
          distance = lower_distance,
          texture_world_scale = texture_world_scale
        )
        if (!is.finite(lower_height)) {
          next
        }
        height_offset = if (is.null(layer_height)) {
          (current_rank - lower_rank) * layer_spacing
        } else {
          layer_height[[upper_path]]
        }
        if (!is.finite(height_offset) || height_offset <= 0) {
          stop(
            paste0(
              "Every road above a lower local layer must have a positive ",
              "finite `layer_height` value."
            ),
            call. = FALSE
          )
        }
        anchor_distance[[upper_path]] = c(
          anchor_distance[[upper_path]],
          upper_distance
        )
        anchor_height[[upper_path]] = c(
          anchor_height[[upper_path]],
          lower_height + height_offset / zscale
        )
      }
      profiled_path = rep(FALSE, path_count)
      for (chain in layer_chains$chains) {
        chain_rank = unique(local_rank[chain$layer_paths])
        if (
          length(chain_rank) != 1L ||
            chain_rank[[1]] != current_rank ||
            !any(lengths(anchor_distance[chain$layer_paths]) > 0L)
        ) {
          next
        }
        chain_result = apply_render_road_layer_chain_profile(
          coord_list = coord_list,
          chain_paths = chain$paths,
          chain_reverse = chain$reverse,
          layer_paths = chain$layer_paths,
          anchor_distance = anchor_distance,
          anchor_height = anchor_height,
          allow_below_endpoint_minimum = chain$layer < 0,
          texture_world_scale = texture_world_scale
        )
        coord_list = chain_result$coord_list
        profiled_path[chain_result$profiled_paths] = TRUE
        terrain_following[chain_result$profiled_paths] = FALSE
      }
      for (path in which(local_rank == current_rank & !profiled_path)) {
        if (!length(anchor_distance[[path]])) {
          next
        }
        coord_list[[path]] = apply_render_road_layer_profile(
          coords = coord_list[[path]],
          anchor_distance = anchor_distance[[path]],
          anchor_height = anchor_height[[path]],
          allow_below_endpoint_minimum = layer[[path]] < 0,
          texture_world_scale = texture_world_scale
        )
        terrain_following[[path]] = FALSE
      }
    }
  }
  stabilized = stabilize_render_road_layer_graph_profiles(
    terrain_coord_list = terrain_coord_list,
    preliminary_coord_list = coord_list,
    intersections = intersections,
    endpoint_graph = endpoint_graph,
    layer = layer,
    layer_explicit = layer_explicit,
    local_rank = local_rank,
    layer_spacing = layer_spacing,
    layer_height = layer_height,
    zscale = zscale,
    texture_world_scale = texture_world_scale
  )
  stabilized
}

#' Build a branched road endpoint graph
#'
#' @param coord_list List of scene coordinate matrices.
#' @param layer Numeric layer for every path.
#' @param layer_explicit Whether every path has an explicit layer.
#' @param texture_world_scale Two-value x-z world scale.
#' @param endpoint_tolerance Default `1e-3`. Endpoint matching tolerance in
#' world units.
#' @param continuation_tolerance Default `20`. Maximum gap between
#' directionally continuous endpoints in world units.
#'
#' @return Endpoint nodes, membership, and all same-layer endpoint edges.
#' @keywords internal
build_render_road_endpoint_graph = function(
  coord_list,
  layer,
  layer_explicit,
  texture_world_scale = c(1, 1),
  endpoint_tolerance = 1e-3,
  continuation_tolerance = 20
) {
  path_count = length(coord_list)
  endpoint_count = path_count * 2L
  endpoint_path = rep(seq_len(path_count), each = 2L)
  endpoint_side = rep(c("start", "end"), path_count)
  endpoint_x = rep(NA_real_, endpoint_count)
  endpoint_z = rep(NA_real_, endpoint_count)
  endpoint_height = rep(NA_real_, endpoint_count)
  endpoint_dx = rep(NA_real_, endpoint_count)
  endpoint_dz = rep(NA_real_, endpoint_count)
  texture_world_scale = validate_render_road_world_scale(texture_world_scale)
  endpoint_tolerance = suppressWarnings(as.numeric(endpoint_tolerance[[1]]))
  if (!is.finite(endpoint_tolerance) || endpoint_tolerance <= 0) {
    endpoint_tolerance = 1e-3
  }
  continuation_tolerance = suppressWarnings(as.numeric(
    continuation_tolerance[[1]]
  ))
  if (!is.finite(continuation_tolerance) || continuation_tolerance <= 0) {
    continuation_tolerance = 20
  }
  for (path in seq_len(path_count)) {
    coords = as.matrix(coord_list[[path]])
    if (
      nrow(coords) < 2L ||
        ncol(coords) < 3L ||
        any(!is.finite(coords[, c(1, 3), drop = FALSE]))
    ) {
      next
    }
    endpoints = c(2L * path - 1L, 2L * path)
    endpoint_x[endpoints] = coords[c(1L, nrow(coords)), 1] *
      texture_world_scale[[1]]
    endpoint_z[endpoints] = coords[c(1L, nrow(coords)), 3] *
      texture_world_scale[[2]]
    endpoint_height[endpoints] = coords[c(1L, nrow(coords)), 2]
    world_coords = sweep(
      coords[, c(1, 3), drop = FALSE],
      2,
      texture_world_scale,
      FUN = "*"
    )
    inward = rbind(
      world_coords[2L, ] - world_coords[1L, ],
      world_coords[nrow(world_coords) - 1L, ] -
        world_coords[nrow(world_coords), ]
    )
    inward_length = sqrt(rowSums(inward^2))
    valid_inward = is.finite(inward_length) & inward_length > 0
    inward[valid_inward, ] = inward[valid_inward, , drop = FALSE] /
      inward_length[valid_inward]
    inward[!valid_inward, ] = NA_real_
    endpoint_dx[endpoints] = inward[, 1]
    endpoint_dz[endpoints] = inward[, 2]
  }
  valid_endpoint = which(is.finite(endpoint_x) & is.finite(endpoint_z))
  negative_endpoint = layer_explicit[endpoint_path] &
    layer[endpoint_path] < 0
  graph_endpoint = valid_endpoint[!negative_endpoint[valid_endpoint]]
  endpoint_node = integer(endpoint_count)
  parent = seq_len(endpoint_count)
  find_root = function(endpoint) {
    while (parent[[endpoint]] != endpoint) {
      parent[[endpoint]] <<- parent[[parent[[endpoint]]]]
      endpoint = parent[[endpoint]]
    }
    endpoint
  }
  join_roots = function(first, second) {
    first_root = find_root(first)
    second_root = find_root(second)
    if (first_root != second_root) {
      parent[[second_root]] <<- first_root
    }
    invisible(NULL)
  }
  spatial_cell = new.env(hash = TRUE, parent = emptyenv())
  for (endpoint in graph_endpoint) {
    cell_x = floor(endpoint_x[[endpoint]] / endpoint_tolerance)
    cell_z = floor(endpoint_z[[endpoint]] / endpoint_tolerance)
    candidate = integer(0)
    for (x_offset in -1:1) {
      for (z_offset in -1:1) {
        key = paste(cell_x + x_offset, cell_z + z_offset, sep = ":")
        candidate = c(
          candidate,
          get0(key, envir = spatial_cell, ifnotfound = integer(0))
        )
      }
    }
    if (length(candidate)) {
      endpoint_feature = endpoint_path[[endpoint]]
      candidate_feature = endpoint_path[candidate]
      compatible_layer = layer[[endpoint_feature]] == layer[candidate_feature]
      candidate = candidate[compatible_layer]
    }
    if (length(candidate)) {
      separation = sqrt(
        (endpoint_x[candidate] - endpoint_x[[endpoint]])^2 +
          (endpoint_z[candidate] - endpoint_z[[endpoint]])^2
      )
      for (matching_endpoint in candidate[separation <= endpoint_tolerance]) {
        join_roots(endpoint, matching_endpoint)
      }
    }
    own_key = paste(cell_x, cell_z, sep = ":")
    assign(
      own_key,
      c(
        get0(own_key, envir = spatial_cell, ifnotfound = integer(0)),
        endpoint
      ),
      envir = spatial_cell
    )
  }
  endpoint_root = vapply(graph_endpoint, find_root, integer(1))
  node_root = unique(endpoint_root)
  endpoint_node[graph_endpoint] = match(endpoint_root, node_root)
  node_members = split(graph_endpoint, endpoint_node[graph_endpoint])
  same_layer_edges = list()
  edge_index = 0L
  for (members in node_members) {
    paths = unique(endpoint_path[members])
    paths = paths[layer_explicit[paths]]
    if (length(paths) < 2L) {
      next
    }
    for (current_layer in unique(layer[paths])) {
      layer_paths = paths[layer[paths] == current_layer]
      if (length(layer_paths) < 2L) {
        next
      }
      pairs = utils::combn(layer_paths, 2L)
      for (pair in seq_len(ncol(pairs))) {
        edge_index = edge_index + 1L
        same_layer_edges[[edge_index]] = data.frame(
          path_a = pairs[1L, pair],
          path_b = pairs[2L, pair]
        )
      }
    }
  }
  same_layer_edges = if (length(same_layer_edges)) {
    unique(do.call(rbind, same_layer_edges))
  } else {
    data.frame(path_a = integer(0), path_b = integer(0))
  }
  continuation_edges = list()
  continuation_index = 0L
  if (length(graph_endpoint) >= 2L) {
    for (position in seq_len(length(graph_endpoint) - 1L)) {
      endpoint_a = graph_endpoint[[position]]
      candidate = graph_endpoint[
        seq.int(position + 1L, length(graph_endpoint))
      ]
      candidate = candidate[
        endpoint_path[candidate] != endpoint_path[[endpoint_a]] &
          endpoint_node[candidate] != endpoint_node[[endpoint_a]] &
          abs(endpoint_x[candidate] - endpoint_x[[endpoint_a]]) <=
            continuation_tolerance &
          abs(endpoint_z[candidate] - endpoint_z[[endpoint_a]]) <=
            continuation_tolerance
      ]
      if (!length(candidate)) {
        next
      }
      gap_x = endpoint_x[candidate] - endpoint_x[[endpoint_a]]
      gap_z = endpoint_z[candidate] - endpoint_z[[endpoint_a]]
      gap_distance = sqrt(gap_x^2 + gap_z^2)
      valid_gap = is.finite(gap_distance) &
        gap_distance <= continuation_tolerance
      candidate = candidate[valid_gap]
      gap_x = gap_x[valid_gap]
      gap_z = gap_z[valid_gap]
      gap_distance = gap_distance[valid_gap]
      if (!length(candidate)) {
        next
      }
      exact_endpoint = gap_distance <= endpoint_tolerance
      nonzero_gap = !exact_endpoint
      gap_x[nonzero_gap] = gap_x[nonzero_gap] / gap_distance[nonzero_gap]
      gap_z[nonzero_gap] = gap_z[nonzero_gap] / gap_distance[nonzero_gap]
      continuity = -(endpoint_dx[[endpoint_a]] *
        endpoint_dx[candidate] +
        endpoint_dz[[endpoint_a]] * endpoint_dz[candidate])
      alignment_a = rep(1, length(candidate))
      alignment_b = rep(1, length(candidate))
      alignment_a[nonzero_gap] = -(gap_x[nonzero_gap] *
        endpoint_dx[[endpoint_a]] +
        gap_z[nonzero_gap] * endpoint_dz[[endpoint_a]])
      alignment_b[nonzero_gap] = gap_x[nonzero_gap] *
        endpoint_dx[candidate[nonzero_gap]] +
        gap_z[nonzero_gap] * endpoint_dz[candidate[nonzero_gap]]
      is_continuation = is.finite(continuity) &
        is.finite(alignment_a) &
        is.finite(alignment_b) &
        continuity >= 0.9 &
        alignment_a >= 0.9 &
        alignment_b >= 0.9
      if (!any(is_continuation)) {
        next
      }
      match = which(is_continuation)
      match_order = order(gap_distance[match], -continuity[match])
      match = match[[match_order[[1L]]]]
      endpoint_b = candidate[[match]]
      continuation_index = continuation_index + 1L
      continuation_edges[[continuation_index]] = data.frame(
        endpoint_a = endpoint_a,
        endpoint_b = endpoint_b,
        path_a = endpoint_path[[endpoint_a]],
        path_b = endpoint_path[[endpoint_b]],
        distance = gap_distance[[match]],
        continuity = continuity[[match]]
      )
    }
  }
  continuation_edges = if (length(continuation_edges)) {
    do.call(rbind, continuation_edges)
  } else {
    data.frame(
      endpoint_a = integer(0),
      endpoint_b = integer(0),
      path_a = integer(0),
      path_b = integer(0),
      distance = numeric(0),
      continuity = numeric(0)
    )
  }
  same_layer_continuation = continuation_edges[
    layer_explicit[continuation_edges$path_a] &
      layer_explicit[continuation_edges$path_b] &
      layer[continuation_edges$path_a] == layer[continuation_edges$path_b],
    c("path_a", "path_b"),
    drop = FALSE
  ]
  if (nrow(same_layer_continuation)) {
    same_layer_edges = unique(rbind(
      same_layer_edges,
      same_layer_continuation
    ))
  }
  list(
    endpoint_path = endpoint_path,
    endpoint_side = endpoint_side,
    endpoint_x = endpoint_x,
    endpoint_z = endpoint_z,
    endpoint_height = endpoint_height,
    endpoint_dx = endpoint_dx,
    endpoint_dz = endpoint_dz,
    endpoint_node = endpoint_node,
    node_members = node_members,
    node_count = length(node_root),
    same_layer_edges = same_layer_edges,
    continuation_edges = continuation_edges
  )
}

#' Stabilize locally layered road profiles over an endpoint graph
#'
#' @param terrain_coord_list Terrain-sampled path coordinates.
#' @param preliminary_coord_list Paths after the legacy local layer pass.
#' @param intersections Road intersection events.
#' @param endpoint_graph Branched endpoint graph.
#' @param layer Effective layer for every path.
#' @param layer_explicit Whether every path has an explicit layer.
#' @param local_rank Local crossing rank for every path.
#' @param layer_spacing Constant vertical separation in world units.
#' @param layer_height Default `NULL`. Optional per-path vertical separation.
#' @param zscale Effective scene zscale.
#' @param texture_world_scale Two-value x-z world scale.
#' @param maximum_grade Default `0.07`. Maximum generated road grade.
#' @param maximum_grade_change Default `5e-4`. Maximum generated change in
#' physical grade per world unit.
#' @param profile_step Default `5`. Maximum spacing between longitudinal
#' smoothing controls in world units.
#'
#' @return Stabilized path coordinates with terrain-following metadata.
#' @keywords internal
stabilize_render_road_layer_graph_profiles = function(
  terrain_coord_list,
  preliminary_coord_list,
  intersections,
  endpoint_graph,
  layer,
  layer_explicit,
  local_rank,
  layer_spacing,
  layer_height = NULL,
  zscale = 1,
  texture_world_scale = c(1, 1),
  maximum_grade = 0.07,
  maximum_grade_change = 5e-4,
  profile_step = 5
) {
  path_count = length(terrain_coord_list)
  if (!path_count) {
    return(terrain_coord_list)
  }
  texture_world_scale = validate_render_road_world_scale(
    texture_world_scale
  )
  maximum_grade = suppressWarnings(as.numeric(maximum_grade[[1]]))
  if (!is.finite(maximum_grade) || maximum_grade <= 0) {
    maximum_grade = 0.07
  }
  maximum_grade_change = suppressWarnings(as.numeric(
    maximum_grade_change[[1]]
  ))
  if (!is.finite(maximum_grade_change) || maximum_grade_change <= 0) {
    maximum_grade_change = 5e-4
  }
  profile_step = suppressWarnings(as.numeric(profile_step[[1]]))
  if (!is.finite(profile_step) || profile_step <= 0) {
    profile_step = 5
  }
  path_distance = lapply(
    terrain_coord_list,
    calculate_road_path_cumulative_distance,
    texture_world_scale = texture_world_scale
  )
  path_length = vapply(
    path_distance,
    function(distance) {
      if (length(distance)) tail(distance, 1L) else 0
    },
    numeric(1)
  )
  valid_path = vapply(
    terrain_coord_list,
    function(coords) {
      is.matrix(coords) &&
        nrow(coords) >= 2L &&
        ncol(coords) >= 3L &&
        all(is.finite(coords))
    },
    logical(1)
  ) &
    is.finite(path_length) &
    path_length > 0
  positive_path = layer_explicit & layer > 0 & valid_path
  negative_path = layer_explicit & layer < 0 & valid_path
  transition_path = !positive_path & !negative_path & valid_path
  if (!any(positive_path)) {
    terrain_following = attr(preliminary_coord_list, "terrain_following")
    if (is.null(terrain_following)) {
      terrain_following = rep(TRUE, path_count)
    }
    attr(preliminary_coord_list, "terrain_following") = terrain_following
    return(preliminary_coord_list)
  }
  crossing_distance = vector("list", path_count)
  if (nrow(intersections)) {
    for (event_index in seq_len(nrow(intersections))) {
      path_a = intersections$path_a[[event_index]]
      path_b = intersections$path_b[[event_index]]
      crossing_distance[[path_a]] = c(
        crossing_distance[[path_a]],
        intersections$distance_a[[event_index]]
      )
      crossing_distance[[path_b]] = c(
        crossing_distance[[path_b]],
        intersections$distance_b[[event_index]]
      )
    }
  }

  control_distance = vector("list", path_count)
  for (path in seq_len(path_count)) {
    if (!valid_path[[path]]) {
      control_distance[[path]] = numeric(0)
      next
    }
    control_distance[[path]] = merge_render_road_control_distances(
      distance = c(
        path_distance[[path]],
        seq(0, path_length[[path]], by = profile_step)
      ),
      protected_distance = c(
        0,
        crossing_distance[[path]],
        path_length[[path]]
      ),
      path_length = path_length[[path]]
    )
  }

  endpoint_node = endpoint_graph$endpoint_node
  next_vertex = endpoint_graph$node_count
  if (length(endpoint_node) != path_count * 2L) {
    endpoint_node = integer(path_count * 2L)
    next_vertex = 0L
  }
  for (endpoint in which(endpoint_node == 0L)) {
    next_vertex = next_vertex + 1L
    endpoint_node[[endpoint]] = next_vertex
  }
  control_vertex = vector("list", path_count)
  for (path in seq_len(path_count)) {
    control_count = length(control_distance[[path]])
    if (!control_count) {
      control_vertex[[path]] = integer(0)
      next
    }
    vertex = integer(control_count)
    vertex[[1L]] = endpoint_node[[2L * path - 1L]]
    vertex[[control_count]] = endpoint_node[[2L * path]]
    if (control_count > 2L) {
      internal = seq.int(2L, control_count - 1L)
      vertex[internal] = seq.int(
        next_vertex + 1L,
        next_vertex + length(internal)
      )
      next_vertex = next_vertex + length(internal)
    }
    control_vertex[[path]] = vertex
  }

  vertex_count = next_vertex
  required_height = rep(-Inf, vertex_count)
  terrain_control_height = vector("list", path_count)
  for (path in seq_len(path_count)) {
    if (!valid_path[[path]]) {
      terrain_control_height[[path]] = numeric(0)
      next
    }
    terrain_height = stats::approx(
      x = path_distance[[path]],
      y = terrain_coord_list[[path]][, 2],
      xout = control_distance[[path]],
      ties = "ordered",
      rule = 2
    )$y
    terrain_control_height[[path]] = terrain_height
    required_control_height = terrain_height
    if (positive_path[[path]]) {
      endpoint_height = terrain_coord_list[[path]][
        c(1L, nrow(terrain_coord_list[[path]])),
        2
      ]
      required_control_height = endpoint_height[[1L]] +
        (endpoint_height[[2L]] - endpoint_height[[1L]]) *
          control_distance[[path]] /
          path_length[[path]]
      required_control_height = pmax(
        required_control_height,
        terrain_height
      )
    }
    vertex = control_vertex[[path]]
    for (control in seq_along(vertex)) {
      if (is.finite(required_control_height[[control]])) {
        required_height[[vertex[[control]]]] = max(
          required_height[[vertex[[control]]]],
          required_control_height[[control]]
        )
      }
    }
  }
  required_height[!is.finite(required_height)] = 0

  edge_from = integer(0)
  edge_to = integer(0)
  edge_delta = numeric(0)
  append_constraint = function(from, to, delta) {
    edge_from <<- c(edge_from, as.integer(from))
    edge_to <<- c(edge_to, as.integer(to))
    edge_delta <<- c(edge_delta, as.numeric(delta))
    invisible(NULL)
  }
  continuation_edges = endpoint_graph$continuation_edges
  if (is.null(continuation_edges)) {
    continuation_edges = data.frame(
      endpoint_a = integer(0),
      endpoint_b = integer(0),
      path_a = integer(0),
      path_b = integer(0),
      distance = numeric(0)
    )
  }
  path_adjacency = vector("list", path_count)
  add_path_edge = function(from, to) {
    path_adjacency[[from]] <<- unique(c(path_adjacency[[from]], to))
    invisible(NULL)
  }
  for (members in endpoint_graph$node_members) {
    member_path = unique(endpoint_graph$endpoint_path[members])
    if (length(member_path) < 2L) {
      next
    }
    pairs = utils::combn(member_path, 2L)
    for (pair in seq_len(ncol(pairs))) {
      add_path_edge(pairs[1L, pair], pairs[2L, pair])
      add_path_edge(pairs[2L, pair], pairs[1L, pair])
    }
  }
  if (nrow(intersections)) {
    for (event_index in seq_len(nrow(intersections))) {
      path_a = intersections$path_a[[event_index]]
      path_b = intersections$path_b[[event_index]]
      rank_a = local_rank[[path_a]]
      rank_b = local_rank[[path_b]]
      if (rank_a > rank_b && positive_path[[path_a]]) {
        add_path_edge(path_b, path_a)
      } else if (rank_b > rank_a && positive_path[[path_b]]) {
        add_path_edge(path_a, path_b)
      }
    }
  }
  path_is_reachable = function(from, to) {
    if (from == to) {
      return(TRUE)
    }
    seen = rep(FALSE, path_count)
    seen[[from]] = TRUE
    queue = from
    queue_index = 1L
    while (queue_index <= length(queue)) {
      current = queue[[queue_index]]
      queue_index = queue_index + 1L
      neighbor = path_adjacency[[current]]
      if (to %in% neighbor) {
        return(TRUE)
      }
      neighbor = neighbor[!seen[neighbor]]
      if (length(neighbor)) {
        seen[neighbor] = TRUE
        queue = c(queue, neighbor)
      }
    }
    FALSE
  }
  if (nrow(continuation_edges)) {
    continuation_order = order(
      continuation_edges$distance,
      -continuation_edges$continuity
    )
    keep_continuation = rep(FALSE, nrow(continuation_edges))
    for (edge in continuation_order) {
      path_a = continuation_edges$path_a[[edge]]
      path_b = continuation_edges$path_b[[edge]]
      closes_clearance_cycle = local_rank[[path_a]] != local_rank[[path_b]] &&
        (path_is_reachable(path_a, path_b) ||
          path_is_reachable(path_b, path_a))
      if (closes_clearance_cycle) {
        next
      }
      keep_continuation[[edge]] = TRUE
      add_path_edge(path_a, path_b)
      add_path_edge(path_b, path_a)
    }
    continuation_edges = continuation_edges[keep_continuation, , drop = FALSE]
  }
  continuation_constraint_added = rep(FALSE, nrow(continuation_edges))
  append_continuation_constraints = function(edge_index) {
    edge_index = edge_index[!continuation_constraint_added[edge_index]]
    if (!length(edge_index)) {
      return(invisible(NULL))
    }
    for (edge in edge_index) {
      endpoint_a = continuation_edges$endpoint_a[[edge]]
      endpoint_b = continuation_edges$endpoint_b[[edge]]
      maximum_scene_change = maximum_grade *
        continuation_edges$distance[[edge]] /
        zscale
      append_constraint(
        endpoint_node[[endpoint_a]],
        endpoint_node[[endpoint_b]],
        -maximum_scene_change
      )
      append_constraint(
        endpoint_node[[endpoint_b]],
        endpoint_node[[endpoint_a]],
        -maximum_scene_change
      )
    }
    continuation_constraint_added[edge_index] <<- TRUE
    invisible(NULL)
  }
  initial_continuation = which(
    positive_path[continuation_edges$path_a] |
      positive_path[continuation_edges$path_b]
  )
  append_continuation_constraints(initial_continuation)

  if (nrow(intersections)) {
    for (event_index in seq_len(nrow(intersections))) {
      path_a = intersections$path_a[[event_index]]
      path_b = intersections$path_b[[event_index]]
      rank_a = local_rank[[path_a]]
      rank_b = local_rank[[path_b]]
      if (rank_a == rank_b) {
        next
      }
      if (rank_a > rank_b) {
        upper_path = path_a
        upper_distance = intersections$distance_a[[event_index]]
        lower_path = path_b
        lower_distance = intersections$distance_b[[event_index]]
        rank_difference = rank_a - rank_b
      } else {
        upper_path = path_b
        upper_distance = intersections$distance_b[[event_index]]
        lower_path = path_a
        lower_distance = intersections$distance_a[[event_index]]
        rank_difference = rank_b - rank_a
      }
      if (!positive_path[[upper_path]]) {
        next
      }
      upper_control = which.min(abs(
        control_distance[[upper_path]] - upper_distance
      ))
      lower_control = which.min(abs(
        control_distance[[lower_path]] - lower_distance
      ))
      height_offset = if (is.null(layer_height)) {
        rank_difference * layer_spacing
      } else {
        layer_height[[upper_path]]
      }
      if (!is.finite(height_offset) || height_offset <= 0) {
        stop(
          paste0(
            "Every road above a lower local layer must have a positive ",
            "finite `layer_height` value."
          ),
          call. = FALSE
        )
      }
      append_constraint(
        control_vertex[[lower_path]][[lower_control]],
        control_vertex[[upper_path]][[upper_control]],
        height_offset / zscale
      )
    }
  }

  solved_height = solve_render_road_height_constraints(
    required_height = required_height,
    edge_from = edge_from,
    edge_to = edge_to,
    edge_delta = edge_delta
  )
  active_transition_path = rep(FALSE, path_count)
  positive_endpoint_node = unique(unlist(lapply(
    which(positive_path),
    function(path) endpoint_node[c(2L * path - 1L, 2L * path)]
  )))
  adjacent_to_positive = vapply(
    seq_len(path_count),
    function(path) {
      any(
        endpoint_node[c(2L * path - 1L, 2L * path)] %in%
          positive_endpoint_node
      )
    },
    logical(1)
  )
  if (nrow(continuation_edges)) {
    for (edge in seq_len(nrow(continuation_edges))) {
      path_a = continuation_edges$path_a[[edge]]
      path_b = continuation_edges$path_b[[edge]]
      if (positive_path[[path_a]]) {
        adjacent_to_positive[[path_b]] = TRUE
      }
      if (positive_path[[path_b]]) {
        adjacent_to_positive[[path_a]] = TRUE
      }
    }
  }
  for (transition_iteration in seq_len(path_count)) {
    newly_active = which(
      transition_path &
        !active_transition_path &
        (adjacent_to_positive |
          vapply(
            seq_len(path_count),
            function(path) {
              any(
                solved_height[control_vertex[[path]]] >
                  terrain_control_height[[path]] + 1e-8,
                na.rm = TRUE
              )
            },
            logical(1)
          ))
    )
    if (!length(newly_active)) {
      break
    }
    active_transition_path[newly_active] = TRUE
    new_continuation = which(
      !continuation_constraint_added &
        (positive_path[continuation_edges$path_a] |
          active_transition_path[continuation_edges$path_a] |
          positive_path[continuation_edges$path_b] |
          active_transition_path[continuation_edges$path_b])
    )
    append_continuation_constraints(new_continuation)
    solved_height = solve_render_road_height_constraints(
      required_height = required_height,
      edge_from = edge_from,
      edge_to = edge_to,
      edge_delta = edge_delta,
      initial_height = solved_height
    )
  }
  for (iteration in seq_len(10L)) {
    changed = FALSE
    for (path in which(positive_path)) {
      vertex = control_vertex[[path]]
      if (length(vertex) <= 2L) {
        next
      }
      endpoint_minimum = min(solved_height[vertex[c(1L, length(vertex))]])
      internal_vertex = vertex[seq.int(2L, length(vertex) - 1L)]
      below_minimum = required_height[internal_vertex] < endpoint_minimum
      if (any(below_minimum)) {
        required_height[internal_vertex[below_minimum]] = endpoint_minimum
        changed = TRUE
      }
    }
    if (!changed) {
      break
    }
    solved_height = solve_render_road_height_constraints(
      required_height = required_height,
      edge_from = edge_from,
      edge_to = edge_to,
      edge_delta = edge_delta,
      initial_height = solved_height
    )
  }
  smoothed_path = valid_path & !negative_path
  for (smoothing_iteration in seq_len(1L)) {
    changed = FALSE
    for (path in which(smoothed_path)) {
      vertex = control_vertex[[path]]
      if (length(vertex) < 3L) {
        next
      }
      smoothed_height = raise_render_road_profile_for_smoothness(
        height = solved_height[vertex],
        distance = control_distance[[path]],
        maximum_scene_grade = maximum_grade / zscale,
        maximum_scene_grade_change = maximum_grade_change * 0.4 / zscale
      )
      for (control in seq_along(vertex)) {
        target_vertex = vertex[[control]]
        if (
          smoothed_height[[control]] > required_height[[target_vertex]] + 1e-8
        ) {
          required_height[[target_vertex]] = smoothed_height[[control]]
          changed = TRUE
        }
      }
    }
    if (!changed) {
      break
    }
    solved_height = solve_render_road_height_constraints(
      required_height = required_height,
      edge_from = edge_from,
      edge_to = edge_to,
      edge_delta = edge_delta,
      initial_height = solved_height
    )
  }
  continuation_profile = continuation_edges[,
    c(
      "endpoint_a",
      "endpoint_b",
      "distance"
    ),
    drop = FALSE
  ]
  exact_profile = list()
  exact_profile_index = 0L
  for (members in endpoint_graph$node_members) {
    if (length(members) < 2L) {
      next
    }
    pairs = utils::combn(members, 2L)
    continuity = -(endpoint_graph$endpoint_dx[pairs[1L, ]] *
      endpoint_graph$endpoint_dx[pairs[2L, ]] +
      endpoint_graph$endpoint_dz[pairs[1L, ]] *
        endpoint_graph$endpoint_dz[pairs[2L, ]])
    matching_pair = which(is.finite(continuity) & continuity >= 0.9)
    for (pair in matching_pair) {
      exact_profile_index = exact_profile_index + 1L
      exact_profile[[exact_profile_index]] = data.frame(
        endpoint_a = pairs[1L, pair],
        endpoint_b = pairs[2L, pair],
        distance = 0
      )
    }
  }
  if (length(exact_profile)) {
    continuation_profile = unique(rbind(
      continuation_profile,
      do.call(rbind, exact_profile)
    ))
  }
  endpoint_control = function(endpoint, maximum_distance) {
    path = endpoint_graph$endpoint_path[[endpoint]]
    vertex = control_vertex[[path]]
    distance = control_distance[[path]]
    if (endpoint_graph$endpoint_side[[endpoint]] == "start") {
      outward_distance = distance
      keep = outward_distance <= maximum_distance
      list(
        vertex = vertex[keep],
        distance = outward_distance[keep]
      )
    } else {
      outward_distance = rev(path_length[[path]] - distance)
      keep = outward_distance <= maximum_distance
      list(
        vertex = rev(vertex)[keep],
        distance = outward_distance[keep]
      )
    }
  }
  smooth_continuation_profiles = function(height) {
    if (!nrow(continuation_profile)) {
      return(height)
    }
    maximum_distance = max(profile_step * 6, 30)
    for (profile in seq_len(nrow(continuation_profile))) {
      first = endpoint_control(
        continuation_profile$endpoint_a[[profile]],
        maximum_distance = maximum_distance
      )
      second = endpoint_control(
        continuation_profile$endpoint_b[[profile]],
        maximum_distance = maximum_distance
      )
      first_order = rev(seq_along(first$vertex))
      profile_vertex = c(
        first$vertex[first_order],
        second$vertex
      )
      profile_distance = c(
        -first$distance[first_order],
        continuation_profile$distance[[profile]] + second$distance
      )
      unique_control = !duplicated(profile_distance)
      profile_vertex = profile_vertex[unique_control]
      profile_distance = profile_distance[unique_control]
      if (length(profile_vertex) < 3L) {
        next
      }
      smoothed_height = raise_render_road_profile_for_smoothness(
        height = height[profile_vertex],
        distance = profile_distance,
        maximum_scene_grade = maximum_grade / zscale,
        maximum_scene_grade_change = maximum_grade_change * 0.4 / zscale
      )
      height[profile_vertex] = pmax(
        height[profile_vertex],
        smoothed_height
      )
    }
    height
  }
  for (final_smoothing_iteration in seq_len(25L)) {
    previous_height = solved_height
    for (path in which(smoothed_path)) {
      vertex = control_vertex[[path]]
      if (length(vertex) < 3L) {
        next
      }
      endpoint_baseline = solved_height[vertex[[1L]]] +
        (solved_height[vertex[[length(vertex)]]] -
          solved_height[vertex[[1L]]]) *
          control_distance[[path]] /
          path_length[[path]]
      solved_height[vertex] = pmax(
        solved_height[vertex],
        endpoint_baseline
      )
      smoothed_height = raise_render_road_profile_for_smoothness(
        height = solved_height[vertex],
        distance = control_distance[[path]],
        maximum_scene_grade = maximum_grade / zscale,
        maximum_scene_grade_change = maximum_grade_change * 0.4 / zscale
      )
      solved_height[vertex] = pmax(
        solved_height[vertex],
        smoothed_height
      )
    }
    solved_height = smooth_continuation_profiles(solved_height)
    solved_height = solve_render_road_height_constraints(
      required_height = required_height,
      edge_from = edge_from,
      edge_to = edge_to,
      edge_delta = edge_delta,
      initial_height = solved_height
    )
    if (max(solved_height - previous_height, na.rm = TRUE) <= 1e-8) {
      break
    }
  }

  result = terrain_coord_list
  terrain_following = rep(TRUE, path_count)
  preliminary_terrain_following = attr(
    preliminary_coord_list,
    "terrain_following"
  )
  for (path in seq_len(path_count)) {
    if (!valid_path[[path]]) {
      next
    }
    if (negative_path[[path]]) {
      result[[path]] = preliminary_coord_list[[path]]
      if (!is.null(preliminary_terrain_following)) {
        terrain_following[[path]] = preliminary_terrain_following[[path]]
      }
      next
    }
    control_height = solved_height[control_vertex[[path]]]
    raised = any(
      control_height > terrain_control_height[[path]] + 1e-8,
      na.rm = TRUE
    )
    if (!positive_path[[path]] && !raised) {
      next
    }
    result[[path]] = generate_render_road_graph_profile(
      coords = terrain_coord_list[[path]],
      control_distance = control_distance[[path]],
      control_height = control_height,
      quadratic = FALSE,
      texture_world_scale = texture_world_scale
    )
    result[[path]] = collapse_render_highquality_road_path_points(
      result[[path]],
      texture_world_scale = texture_world_scale
    )
    result_distance = calculate_road_path_cumulative_distance(
      result[[path]],
      texture_world_scale = texture_world_scale
    )
    terrain_height = stats::approx(
      x = path_distance[[path]],
      y = terrain_coord_list[[path]][, 2],
      xout = result_distance,
      ties = "ordered",
      rule = 2
    )$y
    result[[path]][, 2] = pmax(result[[path]][, 2], terrain_height)
    if (positive_path[[path]]) {
      endpoint_baseline = result[[path]][1L, 2] +
        (result[[path]][nrow(result[[path]]), 2] -
          result[[path]][1L, 2]) *
          result_distance /
          tail(result_distance, 1L)
      result[[path]][, 2] = pmax(
        result[[path]][, 2],
        endpoint_baseline
      )
    }
    terrain_following[[path]] = FALSE
  }
  result_endpoint_row = function(endpoint) {
    path = endpoint_graph$endpoint_path[[endpoint]]
    if (endpoint_graph$endpoint_side[[endpoint]] == "start") {
      1L
    } else {
      nrow(result[[path]])
    }
  }
  result_endpoint_height = function(endpoint) {
    path = endpoint_graph$endpoint_path[[endpoint]]
    if (
      !is.matrix(result[[path]]) ||
        nrow(result[[path]]) < 1L ||
        ncol(result[[path]]) < 2L
    ) {
      return(NA_real_)
    }
    result[[path]][result_endpoint_row(endpoint), 2]
  }
  smooth_result_path = function(path) {
    if (!valid_path[[path]] || negative_path[[path]]) {
      return(FALSE)
    }
    distance = calculate_road_path_cumulative_distance(
      result[[path]],
      texture_world_scale = texture_world_scale
    )
    smoothed_height = raise_render_road_profile_for_smoothness(
      height = result[[path]][, 2],
      distance = distance,
      maximum_scene_grade = maximum_grade / zscale,
      maximum_scene_grade_change = maximum_grade_change * 0.4 / zscale,
      maximum_iterations = 500L
    )
    changed = any(smoothed_height > result[[path]][, 2] + 1e-10)
    result[[path]][, 2] <<- pmax(
      result[[path]][, 2],
      smoothed_height
    )
    if (changed) {
      terrain_following[[path]] <<- FALSE
    }
    changed
  }
  for (path in which(valid_path & !negative_path)) {
    smooth_result_path(path)
  }
  result_endpoint_control = function(endpoint, maximum_distance) {
    path = endpoint_graph$endpoint_path[[endpoint]]
    distance = calculate_road_path_cumulative_distance(
      result[[path]],
      texture_world_scale = texture_world_scale
    )
    if (endpoint_graph$endpoint_side[[endpoint]] == "start") {
      keep = distance <= maximum_distance
      list(path = path, row = which(keep), distance = distance[keep])
    } else {
      outward_distance = rev(tail(distance, 1L) - distance)
      keep = outward_distance <= maximum_distance
      list(
        path = path,
        row = rev(seq_along(distance))[keep],
        distance = outward_distance[keep]
      )
    }
  }
  for (graph_smoothing_iteration in seq_len(20L)) {
    previous_endpoint_height = vapply(
      seq_len(path_count * 2L),
      result_endpoint_height,
      numeric(1)
    )
    affected_path = integer(0)
    for (members in endpoint_graph$node_members) {
      target_height = max(vapply(
        members,
        result_endpoint_height,
        numeric(1)
      ))
      for (endpoint in members) {
        path = endpoint_graph$endpoint_path[[endpoint]]
        row = result_endpoint_row(endpoint)
        if (result[[path]][row, 2] < target_height - 1e-10) {
          result[[path]][row, 2] = target_height
          affected_path = c(affected_path, path)
        }
      }
    }
    if (nrow(continuation_edges)) {
      for (edge in seq_len(nrow(continuation_edges))) {
        endpoint_a = continuation_edges$endpoint_a[[edge]]
        endpoint_b = continuation_edges$endpoint_b[[edge]]
        path_a = continuation_edges$path_a[[edge]]
        path_b = continuation_edges$path_b[[edge]]
        row_a = result_endpoint_row(endpoint_a)
        row_b = result_endpoint_row(endpoint_b)
        height_a = result[[path_a]][row_a, 2]
        height_b = result[[path_b]][row_b, 2]
        maximum_change = maximum_grade *
          continuation_edges$distance[[edge]] /
          zscale
        if (height_a < height_b - maximum_change) {
          result[[path_a]][row_a, 2] = height_b - maximum_change
          affected_path = c(affected_path, path_a)
        } else if (height_b < height_a - maximum_change) {
          result[[path_b]][row_b, 2] = height_a - maximum_change
          affected_path = c(affected_path, path_b)
        }
      }
    }
    for (profile in seq_len(nrow(continuation_profile))) {
      first = result_endpoint_control(
        continuation_profile$endpoint_a[[profile]],
        maximum_distance = max(profile_step * 6, 30)
      )
      second = result_endpoint_control(
        continuation_profile$endpoint_b[[profile]],
        maximum_distance = max(profile_step * 6, 30)
      )
      first_order = rev(seq_along(first$row))
      if (continuation_profile$distance[[profile]] <= 1e-8) {
        second_keep = seq_along(second$row) > 1L
      } else {
        second_keep = rep(TRUE, length(second$row))
      }
      profile_row = c(
        first$row[first_order],
        second$row[second_keep]
      )
      profile_path = c(
        rep(first$path, length(first_order)),
        rep(second$path, sum(second_keep))
      )
      profile_distance = c(
        -first$distance[first_order],
        continuation_profile$distance[[profile]] +
          second$distance[second_keep]
      )
      if (length(profile_distance) < 3L) {
        next
      }
      profile_height = vapply(
        seq_along(profile_row),
        function(control) {
          result[[profile_path[[control]]]][profile_row[[control]], 2]
        },
        numeric(1)
      )
      smoothed_height = raise_render_road_profile_for_smoothness(
        height = profile_height,
        distance = profile_distance,
        maximum_scene_grade = maximum_grade / zscale,
        maximum_scene_grade_change = maximum_grade_change * 0.4 / zscale,
        maximum_iterations = 500L
      )
      for (control in seq_along(profile_row)) {
        path = profile_path[[control]]
        row = profile_row[[control]]
        if (result[[path]][row, 2] < smoothed_height[[control]] - 1e-10) {
          result[[path]][row, 2] = smoothed_height[[control]]
          affected_path = c(affected_path, path)
        }
      }
    }
    for (path in unique(affected_path)) {
      smooth_result_path(path)
    }
    current_endpoint_height = vapply(
      seq_len(path_count * 2L),
      result_endpoint_height,
      numeric(1)
    )
    if (
      max(current_endpoint_height - previous_endpoint_height, na.rm = TRUE) <=
        1e-8 &&
        !length(affected_path)
    ) {
      break
    }
  }
  component_parent = seq_len(path_count)
  find_component = function(path) {
    while (component_parent[[path]] != path) {
      component_parent[[path]] <<-
        component_parent[[component_parent[[path]]]]
      path = component_parent[[path]]
    }
    path
  }
  join_component = function(path_a, path_b) {
    root_a = find_component(path_a)
    root_b = find_component(path_b)
    if (root_a != root_b) {
      component_parent[[root_b]] <<- root_a
    }
    invisible(NULL)
  }
  for (members in endpoint_graph$node_members) {
    member_path = unique(endpoint_graph$endpoint_path[members])
    if (length(member_path) >= 2L) {
      for (path in member_path[-1L]) {
        join_component(member_path[[1L]], path)
      }
    }
  }
  if (nrow(continuation_edges)) {
    for (edge in seq_len(nrow(continuation_edges))) {
      path_a = continuation_edges$path_a[[edge]]
      path_b = continuation_edges$path_b[[edge]]
      if (local_rank[[path_a]] == local_rank[[path_b]]) {
        join_component(path_a, path_b)
      }
    }
  }
  component_root = vapply(seq_len(path_count), find_component, integer(1))
  component_id = match(component_root, unique(component_root))
  component_count = max(component_id)
  shift_from = integer(0)
  shift_to = integer(0)
  shift_delta = numeric(0)
  if (nrow(intersections)) {
    for (event_index in seq_len(nrow(intersections))) {
      path_a = intersections$path_a[[event_index]]
      path_b = intersections$path_b[[event_index]]
      rank_a = local_rank[[path_a]]
      rank_b = local_rank[[path_b]]
      if (rank_a == rank_b) {
        next
      }
      if (rank_a > rank_b) {
        upper_path = path_a
        upper_distance = intersections$distance_a[[event_index]]
        lower_path = path_b
        lower_distance = intersections$distance_b[[event_index]]
        rank_difference = rank_a - rank_b
      } else {
        upper_path = path_b
        upper_distance = intersections$distance_b[[event_index]]
        lower_path = path_a
        lower_distance = intersections$distance_a[[event_index]]
        rank_difference = rank_b - rank_a
      }
      if (!positive_path[[upper_path]]) {
        next
      }
      height_offset = if (is.null(layer_height)) {
        rank_difference * layer_spacing
      } else {
        layer_height[[upper_path]]
      }
      lower_height = interpolate_render_road_path_height(
        coords = result[[lower_path]],
        distance = lower_distance,
        texture_world_scale = texture_world_scale
      )
      upper_height = interpolate_render_road_path_height(
        coords = result[[upper_path]],
        distance = upper_distance,
        texture_world_scale = texture_world_scale
      )
      lower_component = component_id[[lower_path]]
      upper_component = component_id[[upper_path]]
      required_shift = lower_height + height_offset / zscale - upper_height
      if (lower_component == upper_component) {
        next
      }
      shift_from = c(shift_from, lower_component)
      shift_to = c(shift_to, upper_component)
      shift_delta = c(shift_delta, required_shift)
    }
  }
  component_shift = solve_render_road_height_constraints(
    required_height = rep(0, component_count),
    edge_from = shift_from,
    edge_to = shift_to,
    edge_delta = shift_delta
  )
  keep_shift_continuation = rep(TRUE, nrow(continuation_edges))
  if (nrow(continuation_edges)) {
    continuation_order = order(
      continuation_edges$distance,
      -continuation_edges$continuity
    )
    for (edge in continuation_order) {
      endpoint_a = continuation_edges$endpoint_a[[edge]]
      endpoint_b = continuation_edges$endpoint_b[[edge]]
      path_a = continuation_edges$path_a[[edge]]
      path_b = continuation_edges$path_b[[edge]]
      component_a = component_id[[path_a]]
      component_b = component_id[[path_b]]
      height_a = result_endpoint_height(endpoint_a)
      height_b = result_endpoint_height(endpoint_b)
      maximum_change = maximum_grade *
        continuation_edges$distance[[edge]] /
        zscale
      if (component_a == component_b) {
        if (abs(height_a - height_b) > maximum_change + 1e-8) {
          keep_shift_continuation[[edge]] = FALSE
        }
        next
      }
      candidate_from = c(shift_from, component_a, component_b)
      candidate_to = c(shift_to, component_b, component_a)
      candidate_delta = c(
        shift_delta,
        height_a - height_b - maximum_change,
        height_b - height_a - maximum_change
      )
      candidate_shift = tryCatch(
        solve_render_road_height_constraints(
          required_height = rep(0, component_count),
          edge_from = candidate_from,
          edge_to = candidate_to,
          edge_delta = candidate_delta,
          initial_height = component_shift
        ),
        error = function(error) NULL
      )
      if (is.null(candidate_shift)) {
        keep_shift_continuation[[edge]] = FALSE
        next
      }
      shift_from = candidate_from
      shift_to = candidate_to
      shift_delta = candidate_delta
      component_shift = candidate_shift
    }
  }
  continuation_edges = continuation_edges[
    keep_shift_continuation,
    ,
    drop = FALSE
  ]
  for (path in which(valid_path & !negative_path)) {
    shift = component_shift[[component_id[[path]]]]
    if (shift > 1e-10) {
      result[[path]][, 2] = result[[path]][, 2] + shift
      terrain_following[[path]] = FALSE
    }
  }
  attr(result, "terrain_following") = terrain_following
  attr(result, "continuation_edges") = continuation_edges
  result
}

#' Solve lower-bound road height constraints
#'
#' @param required_height Minimum height for every graph vertex.
#' @param edge_from Constraint source vertices.
#' @param edge_to Constraint destination vertices.
#' @param edge_delta Required destination height relative to the source.
#' @param initial_height Default `NULL`. Optional warm-start heights.
#'
#' @return Minimum graph heights satisfying every constraint.
#' @keywords internal
solve_render_road_height_constraints = function(
  required_height,
  edge_from,
  edge_to,
  edge_delta,
  initial_height = NULL
) {
  vertex_count = length(required_height)
  if (!vertex_count) {
    return(numeric(0))
  }
  height = required_height
  if (!is.null(initial_height) && length(initial_height) == vertex_count) {
    height = pmax(height, initial_height)
  }
  if (!length(edge_from)) {
    return(height)
  }
  valid_edge = edge_from >= 1L &
    edge_from <= vertex_count &
    edge_to >= 1L &
    edge_to <= vertex_count &
    is.finite(edge_delta)
  edge_from = edge_from[valid_edge]
  edge_to = edge_to[valid_edge]
  edge_delta = edge_delta[valid_edge]
  adjacency = split(seq_along(edge_from), edge_from)
  queue_capacity = max(vertex_count * 2L, 1024L)
  queue = integer(queue_capacity)
  queue[seq_len(vertex_count)] = seq_len(vertex_count)
  queue_length = vertex_count
  in_queue = rep(TRUE, vertex_count)
  constraint_depth = integer(vertex_count)
  queue_index = 1L
  tolerance = 1e-10
  while (queue_index <= queue_length) {
    from = queue[[queue_index]]
    queue_index = queue_index + 1L
    in_queue[[from]] = FALSE
    outgoing = adjacency[[as.character(from)]]
    if (!length(outgoing)) {
      next
    }
    for (edge in outgoing) {
      to = edge_to[[edge]]
      candidate = height[[from]] + edge_delta[[edge]]
      if (candidate <= height[[to]] + tolerance) {
        next
      }
      height[[to]] = candidate
      constraint_depth[[to]] = constraint_depth[[from]] + 1L
      if (constraint_depth[[to]] >= vertex_count) {
        stop(
          paste0(
            "Road layer constraints contain an infeasible positive cycle; ",
            "check intersecting `layer` values and `layer_height`."
          ),
          call. = FALSE
        )
      }
      if (!in_queue[[to]]) {
        queue_length = queue_length + 1L
        if (queue_length > length(queue)) {
          length(queue) = length(queue) * 2L
        }
        queue[[queue_length]] = to
        in_queue[[to]] = TRUE
      }
    }
  }
  height
}

#' Raise a road profile to satisfy grade and vertical-curvature limits
#'
#' @param height Minimum control heights in scene units.
#' @param distance Strictly increasing control distances in world units.
#' @param maximum_scene_grade Maximum absolute scene-height grade.
#' @param maximum_scene_grade_change Maximum scene-grade change per world unit.
#' @param maximum_iterations Default `75`. Maximum monotone projection passes.
#'
#' @return A smooth majorant of `height`.
#' @keywords internal
raise_render_road_profile_for_smoothness = function(
  height,
  distance,
  maximum_scene_grade,
  maximum_scene_grade_change,
  maximum_iterations = 75L
) {
  height = suppressWarnings(as.numeric(height))
  distance = suppressWarnings(as.numeric(distance))
  if (
    length(height) < 2L ||
      length(distance) != length(height) ||
      any(!is.finite(height)) ||
      any(!is.finite(distance)) ||
      any(diff(distance) <= 0)
  ) {
    return(height)
  }
  maximum_scene_grade = suppressWarnings(as.numeric(
    maximum_scene_grade[[1]]
  ))
  maximum_scene_grade_change = suppressWarnings(as.numeric(
    maximum_scene_grade_change[[1]]
  ))
  if (
    !is.finite(maximum_scene_grade) ||
      maximum_scene_grade <= 0 ||
      !is.finite(maximum_scene_grade_change) ||
      maximum_scene_grade_change <= 0
  ) {
    return(height)
  }
  maximum_iterations = suppressWarnings(as.integer(maximum_iterations[[1]]))
  if (!is.finite(maximum_iterations) || maximum_iterations < 1L) {
    maximum_iterations = 75L
  }
  tolerance = 1e-10
  control_count = length(height)
  interval = diff(distance)
  enforce_grade = function(value) {
    forward = cummax(value + maximum_scene_grade * distance) -
      maximum_scene_grade * distance
    backward = rev(cummax(rev(
      value - maximum_scene_grade * distance
    ))) +
      maximum_scene_grade * distance
    pmax(value, forward, backward)
  }
  height = enforce_grade(height)
  if (length(height) == 2L) {
    return(height)
  }
  internal_index = seq.int(2L, control_count - 1L)
  left_distance = head(interval, -1L)
  right_distance = tail(interval, -1L)
  change_limit = maximum_scene_grade_change *
    (left_distance + right_distance) /
    2
  inverse_distance = 1 / left_distance + 1 / right_distance
  for (iteration in seq_len(maximum_iterations)) {
    previous_height = height
    center_height = height[internal_index]
    left_height = height[internal_index - 1L]
    right_height = height[internal_index + 1L]
    left_grade = (center_height - left_height) / left_distance
    right_grade = (right_height - center_height) / right_distance
    grade_change = right_grade - left_grade
    next_height = height

    valley = grade_change > change_limit + tolerance
    if (any(valley)) {
      valley_index = internal_index[valley]
      next_height[valley_index] = pmax(
        next_height[valley_index],
        center_height[valley] +
          (grade_change[valley] - change_limit[valley]) /
            inverse_distance[valley]
      )
    }

    peak = grade_change < -change_limit - tolerance
    if (any(peak)) {
      peak_index = internal_index[peak]
      left_headroom = pmax(center_height[peak] - left_height[peak], 0)
      right_headroom = pmax(center_height[peak] - right_height[peak], 0)
      weighted_headroom = left_headroom /
        left_distance[peak] +
        right_headroom / right_distance[peak]
      valid_peak = weighted_headroom > tolerance
      if (any(valid_peak)) {
        peak_index = peak_index[valid_peak]
        uplift_fraction = pmin(
          1,
          (-change_limit[peak][valid_peak] -
            grade_change[peak][valid_peak]) /
            weighted_headroom[valid_peak]
        )
        next_height[peak_index - 1L] = pmax(
          next_height[peak_index - 1L],
          left_height[peak][valid_peak] +
            uplift_fraction * left_headroom[valid_peak]
        )
        next_height[peak_index + 1L] = pmax(
          next_height[peak_index + 1L],
          right_height[peak][valid_peak] +
            uplift_fraction * right_headroom[valid_peak]
        )
      }
    }
    height = enforce_grade(next_height)
    grade = diff(height) / interval
    grade_change = diff(grade)
    if (
      max(abs(grade), na.rm = TRUE) <= maximum_scene_grade + 1e-8 &&
        max(abs(grade_change) - change_limit, na.rm = TRUE) <= 1e-8 &&
        max(height - previous_height, na.rm = TRUE) <= 1e-8
    ) {
      break
    }
  }
  height
}

#' Normalize road graph control distances
#'
#' @param distance Proposed path distances.
#' @param path_length Complete path length.
#' @param minimum_separation Default `0.25`. Minimum separation between
#' adjacent controls in world units.
#'
#' @return Sorted, tolerant, endpoint-snapped distances.
#' @keywords internal
normalize_render_road_control_distances = function(
  distance,
  path_length,
  minimum_separation = 0.25
) {
  path_length = suppressWarnings(as.numeric(path_length[[1]]))
  if (!is.finite(path_length) || path_length <= 0) {
    return(c(0, 0))
  }
  minimum_separation = suppressWarnings(as.numeric(minimum_separation[[1]]))
  if (!is.finite(minimum_separation) || minimum_separation <= 0) {
    minimum_separation = 0.25
  }
  tolerance = max(minimum_separation, path_length * 1e-9)
  distance = sort(pmin(pmax(distance[is.finite(distance)], 0), path_length))
  if (!length(distance)) {
    return(c(0, path_length))
  }
  group = cumsum(c(TRUE, diff(distance) > tolerance))
  distance = unname(vapply(split(distance, group), mean, numeric(1)))
  distance[distance <= tolerance] = 0
  distance[distance >= path_length - tolerance] = path_length
  distance = sort(unique(c(0, distance, path_length)))
  if (length(distance) < 2L) {
    c(0, path_length)
  } else {
    distance
  }
}

#' Merge protected road controls with regularly spaced controls
#'
#' @param distance Proposed unprotected path distances.
#' @param protected_distance Exact intersection and endpoint distances.
#' @param path_length Complete path length.
#' @param minimum_separation Default `0.25`. Minimum separation around an
#' exact protected control in world units.
#'
#' @return Sorted control distances retaining exact protected locations.
#' @keywords internal
merge_render_road_control_distances = function(
  distance,
  protected_distance,
  path_length,
  minimum_separation = 0.25
) {
  regular = normalize_render_road_control_distances(
    distance = distance,
    path_length = path_length,
    minimum_separation = minimum_separation
  )
  protected = normalize_render_road_control_distances(
    distance = protected_distance,
    path_length = path_length,
    minimum_separation = 1e-7
  )
  near_protected = vapply(
    regular,
    function(value) {
      any(abs(protected - value) < minimum_separation)
    },
    logical(1)
  )
  normalize_render_road_control_distances(
    distance = c(regular[!near_protected], protected),
    path_length = path_length,
    minimum_separation = 1e-7
  )
}

#' Generate a graph-constrained road profile
#'
#' @param coords Terrain-sampled path coordinates.
#' @param control_distance Distances of graph controls.
#' @param control_height Solved heights of graph controls.
#' @param quadratic Whether to ease quadratically between controls.
#' @param linear_baseline Default `FALSE`. Whether quadratic interpolation
#' applies only to uplift above the linear endpoint baseline.
#' @param maximum_scene_grade Default `Inf`. Maximum permitted height change
#' per world unit before falling back to an absolute quadratic profile.
#' @param texture_world_scale Two-value x-z world scale.
#'
#' @return Densified road coordinates with solved heights.
#' @keywords internal
generate_render_road_graph_profile = function(
  coords,
  control_distance,
  control_height,
  quadratic,
  linear_baseline = FALSE,
  maximum_scene_grade = Inf,
  texture_world_scale = c(1, 1)
) {
  coords = as.matrix(coords)
  if (nrow(coords) < 2L || ncol(coords) < 3L) {
    return(coords)
  }
  path_distance = calculate_road_path_cumulative_distance(
    coords,
    texture_world_scale = texture_world_scale
  )
  path_length = tail(path_distance, 1L)
  control_distance = normalize_render_road_control_distances(
    control_distance,
    path_length = path_length,
    minimum_separation = 1e-7
  )
  if (length(control_height) != length(control_distance)) {
    return(coords)
  }
  tween_distance = if (isTRUE(quadratic)) {
    tween_fraction = seq(0, 1, length.out = 9L)
    unlist(lapply(
      seq_len(length(control_distance) - 1L),
      function(index) {
        control_distance[[index]] +
          (control_distance[[index + 1L]] - control_distance[[index]]) *
            tween_fraction
      }
    ))
  } else {
    control_distance
  }
  profile_distance = merge_render_road_control_distances(
    distance = c(path_distance, tween_distance),
    protected_distance = control_distance,
    path_length = path_length
  )
  original_coords = coords
  coords = vapply(
    seq_len(ncol(coords)),
    function(column) {
      stats::approx(
        x = path_distance,
        y = original_coords[, column],
        xout = profile_distance,
        ties = "ordered",
        rule = 2
      )$y
    },
    numeric(length(profile_distance))
  )
  if (!isTRUE(quadratic)) {
    coords[, 2] = stats::approx(
      x = control_distance,
      y = control_height,
      xout = profile_distance,
      ties = "ordered",
      rule = 2
    )$y
    return(coords)
  }
  baseline_height = control_height[[1L]] +
    (control_height[[length(control_height)]] - control_height[[1L]]) *
      profile_distance /
      path_length
  baseline_control_height = control_height[[1L]] +
    (control_height[[length(control_height)]] - control_height[[1L]]) *
      control_distance /
      path_length
  interpolated_control_height = if (isTRUE(linear_baseline)) {
    pmax(control_height - baseline_control_height, 0)
  } else {
    control_height
  }
  profile_height = rep(NA_real_, length(profile_distance))
  tolerance = max(1e-7, path_length * 1e-9)
  for (index in seq_len(length(control_distance) - 1L)) {
    interval_start = control_distance[[index]]
    interval_end = control_distance[[index + 1L]]
    if (interval_end - interval_start <= tolerance) {
      next
    }
    in_interval = profile_distance >= interval_start - tolerance &
      profile_distance <= interval_end + tolerance
    progress = pmin(
      pmax(
        (profile_distance[in_interval] - interval_start) /
          (interval_end - interval_start),
        0
      ),
      1
    )
    quadratic_progress = ifelse(
      progress < 0.5,
      2 * progress^2,
      1 - (-2 * progress + 2)^2 / 2
    )
    profile_height[in_interval] = interpolated_control_height[[index]] +
      (interpolated_control_height[[index + 1L]] -
        interpolated_control_height[[index]]) *
        quadratic_progress
  }
  missing_height = !is.finite(profile_height)
  if (any(missing_height)) {
    profile_height[missing_height] = stats::approx(
      x = control_distance,
      y = interpolated_control_height,
      xout = profile_distance[missing_height],
      ties = "ordered",
      rule = 2
    )$y
  }
  if (isTRUE(linear_baseline)) {
    profile_height = baseline_height + profile_height
    horizontal_step = diff(profile_distance)
    profile_grade = abs(diff(profile_height) / horizontal_step)
    if (
      is.finite(maximum_scene_grade) &&
        any(profile_grade > maximum_scene_grade + 1e-8, na.rm = TRUE)
    ) {
      return(generate_render_road_graph_profile(
        coords = original_coords,
        control_distance = control_distance,
        control_height = control_height,
        quadratic = TRUE,
        linear_baseline = FALSE,
        maximum_scene_grade = maximum_scene_grade,
        texture_world_scale = texture_world_scale
      ))
    }
  }
  coords[, 2] = profile_height
  coords
}

#' Build directionally continuous road layer chains
#'
#' @param coord_list List of scene coordinate matrices.
#' @param layer Effective numeric layer for each path.
#' @param layer_explicit Whether each path had a non-missing layer value.
#' @param texture_world_scale Two-value x-z multiplier from scene units to
#' world units.
#'
#' @return Layer chains and same-layer endpoint edges.
#' @keywords internal
build_render_road_layer_chains = function(
  coord_list,
  layer,
  layer_explicit,
  texture_world_scale = c(1, 1)
) {
  path_count = length(coord_list)
  empty_edges = data.frame(path_a = integer(0), path_b = integer(0))
  if (!path_count || !any(layer_explicit)) {
    return(list(chains = list(), same_layer_edges = empty_edges))
  }
  texture_world_scale = validate_render_road_world_scale(texture_world_scale)
  endpoint_count = path_count * 2L
  endpoint_path = rep(seq_len(path_count), each = 2L)
  endpoint_side = rep(c("start", "end"), path_count)
  endpoint_x = rep(NA_real_, endpoint_count)
  endpoint_z = rep(NA_real_, endpoint_count)
  endpoint_dx = rep(NA_real_, endpoint_count)
  endpoint_dz = rep(NA_real_, endpoint_count)
  valid_path = logical(path_count)
  for (path in seq_len(path_count)) {
    coords = as.matrix(coord_list[[path]])
    if (
      nrow(coords) < 2L ||
        ncol(coords) < 3L ||
        any(!is.finite(coords[, c(1, 3), drop = FALSE]))
    ) {
      next
    }
    world = sweep(
      coords[, c(1, 3), drop = FALSE],
      2,
      texture_world_scale,
      FUN = "*"
    )
    start_delta = sweep(
      world[-1L, , drop = FALSE],
      2,
      world[1L, ],
      FUN = "-"
    )
    start_neighbor = which(rowSums(start_delta^2) > 0)
    end_delta = sweep(
      world[-nrow(world), , drop = FALSE],
      2,
      world[nrow(world), ],
      FUN = "-"
    )
    end_neighbor = tail(which(rowSums(end_delta^2) > 0), 1L)
    if (!length(start_neighbor) || !length(end_neighbor)) {
      next
    }
    start_neighbor = start_neighbor[[1]]
    start_direction = world[1L, ] - world[start_neighbor + 1L, ]
    end_direction = world[nrow(world), ] - world[end_neighbor, ]
    start_direction = start_direction / sqrt(sum(start_direction^2))
    end_direction = end_direction / sqrt(sum(end_direction^2))
    start_endpoint = 2L * path - 1L
    end_endpoint = 2L * path
    endpoint_x[c(start_endpoint, end_endpoint)] = world[c(1L, nrow(world)), 1]
    endpoint_z[c(start_endpoint, end_endpoint)] = world[c(1L, nrow(world)), 2]
    endpoint_dx[c(start_endpoint, end_endpoint)] = c(
      start_direction[[1]],
      end_direction[[1]]
    )
    endpoint_dz[c(start_endpoint, end_endpoint)] = c(
      start_direction[[2]],
      end_direction[[2]]
    )
    valid_path[[path]] = TRUE
  }
  rounded_x = endpoint_x
  rounded_z = endpoint_z
  rounded_x[abs(rounded_x) < 5e-8] = 0
  rounded_z[abs(rounded_z) < 5e-8] = 0
  endpoint_key = ifelse(
    is.finite(rounded_x) & is.finite(rounded_z),
    sprintf("%.7f:%.7f", rounded_x, rounded_z),
    NA_character_
  )
  endpoint_groups = split(
    which(!is.na(endpoint_key)),
    endpoint_key[!is.na(endpoint_key)]
  )
  endpoint_link = integer(endpoint_count)
  best_endpoint = integer(endpoint_count)
  for (group in endpoint_groups) {
    if (length(group) < 2L) {
      next
    }
    for (endpoint in group) {
      path = endpoint_path[[endpoint]]
      if (!layer_explicit[[path]] || !valid_path[[path]]) {
        next
      }
      candidates = group[
        endpoint_path[group] != path &
          layer_explicit[endpoint_path[group]] &
          layer[endpoint_path[group]] == layer[[path]]
      ]
      if (!length(candidates)) {
        next
      }
      score = -(endpoint_dx[[endpoint]] *
        endpoint_dx[candidates] +
        endpoint_dz[[endpoint]] * endpoint_dz[candidates])
      score[!is.finite(score)] = -Inf
      best = which.max(score)
      if (length(best) && score[[best]] >= 0.5) {
        best_endpoint[[endpoint]] = candidates[[best]]
      }
    }
  }
  for (endpoint in which(best_endpoint > 0L)) {
    candidate = best_endpoint[[endpoint]]
    if (best_endpoint[[candidate]] == endpoint) {
      endpoint_link[[endpoint]] = candidate
    }
  }
  linked_endpoint = which(endpoint_link > 0L)
  same_layer_edges = if (length(linked_endpoint)) {
    edge_path_a = endpoint_path[linked_endpoint]
    edge_path_b = endpoint_path[endpoint_link[linked_endpoint]]
    edge_key = paste(
      pmin(edge_path_a, edge_path_b),
      pmax(edge_path_a, edge_path_b),
      sep = ":"
    )
    keep = !duplicated(edge_key)
    data.frame(
      path_a = edge_path_a[keep],
      path_b = edge_path_b[keep]
    )
  } else {
    empty_edges
  }

  find_ground_extension = function(endpoint, excluded_paths) {
    if (!length(endpoint) || endpoint_link[[endpoint]] != 0L) {
      return(0L)
    }
    candidates = endpoint_groups[[endpoint_key[[endpoint]]]]
    candidates = candidates[
      !endpoint_path[candidates] %in% excluded_paths &
        (!layer_explicit[endpoint_path[candidates]] |
          layer[endpoint_path[candidates]] == 0)
    ]
    if (!length(candidates)) {
      return(0L)
    }
    score = -(endpoint_dx[[endpoint]] *
      endpoint_dx[candidates] +
      endpoint_dz[[endpoint]] * endpoint_dz[candidates])
    score[!is.finite(score)] = -Inf
    best = which.max(score)
    if (!length(best) || score[[best]] < 0.5) {
      return(0L)
    }
    candidates[[best]]
  }

  chains = list()
  chain_index = 0L
  visited_path = logical(path_count)
  claimed_ground_path = integer(0)
  for (initial_path in which(layer_explicit & valid_path)) {
    if (visited_path[[initial_path]]) {
      next
    }
    component_paths = integer(0)
    pending_paths = initial_path
    while (length(pending_paths)) {
      path = pending_paths[[length(pending_paths)]]
      pending_paths = pending_paths[-length(pending_paths)]
      if (path %in% component_paths) {
        next
      }
      component_paths = c(component_paths, path)
      path_endpoints = c(2L * path - 1L, 2L * path)
      linked = endpoint_link[path_endpoints]
      linked = linked[linked > 0L]
      if (length(linked)) {
        pending_paths = c(pending_paths, endpoint_path[linked])
      }
    }
    component_endpoints = as.vector(rbind(
      2L * component_paths - 1L,
      2L * component_paths
    ))
    open_endpoint = component_endpoints[
      endpoint_link[component_endpoints] == 0L
    ]
    entry_endpoint = if (length(open_endpoint)) {
      open_endpoint[[1]]
    } else {
      2L * initial_path - 1L
    }
    layer_paths = integer(0)
    layer_reverse = logical(0)
    left_endpoint = entry_endpoint
    current_endpoint = entry_endpoint
    right_endpoint = entry_endpoint
    repeat {
      path = endpoint_path[[current_endpoint]]
      if (visited_path[[path]]) {
        break
      }
      visited_path[[path]] = TRUE
      layer_paths = c(layer_paths, path)
      reverse_path = endpoint_side[[current_endpoint]] == "end"
      layer_reverse = c(layer_reverse, reverse_path)
      exit_endpoint = if (reverse_path) 2L * path - 1L else 2L * path
      right_endpoint = exit_endpoint
      next_endpoint = endpoint_link[[exit_endpoint]]
      if (!next_endpoint || visited_path[[endpoint_path[[next_endpoint]]]]) {
        break
      }
      current_endpoint = next_endpoint
    }
    chain_paths = layer_paths
    chain_reverse = layer_reverse
    chain_layer = layer[[layer_paths[[1]]]]
    if (chain_layer != 0) {
      left_extension = find_ground_extension(
        endpoint = left_endpoint,
        excluded_paths = c(layer_paths, claimed_ground_path)
      )
      if (left_extension) {
        left_path = endpoint_path[[left_extension]]
        chain_paths = c(left_path, chain_paths)
        chain_reverse = c(
          endpoint_side[[left_extension]] == "start",
          chain_reverse
        )
        claimed_ground_path = c(claimed_ground_path, left_path)
      }
      right_extension = find_ground_extension(
        endpoint = right_endpoint,
        excluded_paths = c(layer_paths, claimed_ground_path)
      )
      if (right_extension) {
        right_path = endpoint_path[[right_extension]]
        chain_paths = c(chain_paths, right_path)
        chain_reverse = c(
          chain_reverse,
          endpoint_side[[right_extension]] == "end"
        )
        claimed_ground_path = c(claimed_ground_path, right_path)
      }
    }
    chain_index = chain_index + 1L
    chains[[chain_index]] = list(
      paths = chain_paths,
      reverse = chain_reverse,
      layer_paths = layer_paths,
      layer = chain_layer
    )
  }
  list(chains = chains, same_layer_edges = same_layer_edges)
}

#' Apply one height profile across a road layer chain
#'
#' @param coord_list List of scene coordinate matrices.
#' @param chain_paths Ordered path indices in the chain.
#' @param chain_reverse Whether each path must be reversed to follow the chain.
#' @param layer_paths Path indices belonging to the layered portion.
#' @param anchor_distance Per-path crossing distances.
#' @param anchor_height Per-path crossing heights.
#' @param allow_below_endpoint_minimum Whether the profile may fall below the
#' lower sampled endpoint of the layered portion.
#' @param texture_world_scale Two-value x-z multiplier from scene units to
#' world units.
#'
#' @return Updated coordinates and the paths covered by the profile.
#' @keywords internal
apply_render_road_layer_chain_profile = function(
  coord_list,
  chain_paths,
  chain_reverse,
  layer_paths,
  anchor_distance,
  anchor_height,
  allow_below_endpoint_minimum = FALSE,
  texture_world_scale = c(1, 1)
) {
  oriented_coords = Map(
    function(path, reverse_path) {
      coords = coord_list[[path]]
      if (reverse_path) coords[nrow(coords):1L, , drop = FALSE] else coords
    },
    chain_paths,
    chain_reverse
  )
  path_length = vapply(
    oriented_coords,
    function(coords) {
      distance = calculate_road_path_cumulative_distance(
        coords,
        texture_world_scale = texture_world_scale
      )
      if (length(distance)) tail(distance, 1L) else 0
    },
    numeric(1)
  )
  path_offset = c(0, head(cumsum(path_length), -1L))
  path_end = cumsum(path_length)
  combined_coords = oriented_coords[[1]]
  if (length(oriented_coords) > 1L) {
    for (index in 2:length(oriented_coords)) {
      combined_coords = rbind(
        combined_coords,
        oriented_coords[[index]][-1L, , drop = FALSE]
      )
    }
  }
  combined_anchor_distance = numeric(0)
  combined_anchor_height = numeric(0)
  for (index in seq_along(chain_paths)) {
    path = chain_paths[[index]]
    local_distance = anchor_distance[[path]]
    local_height = anchor_height[[path]]
    if (!length(local_distance)) {
      next
    }
    if (chain_reverse[[index]]) {
      local_distance = path_length[[index]] - local_distance
    }
    combined_anchor_distance = c(
      combined_anchor_distance,
      path_offset[[index]] + local_distance
    )
    combined_anchor_height = c(combined_anchor_height, local_height)
  }
  layer_index = which(chain_paths %in% layer_paths)
  if (length(layer_index)) {
    first_layer = min(layer_index)
    last_layer = max(layer_index)
    if (isTRUE(allow_below_endpoint_minimum)) {
      layer_endpoint_height = c(
        oriented_coords[[first_layer]][1L, 2],
        oriented_coords[[last_layer]][
          nrow(oriented_coords[[last_layer]]),
          2
        ]
      )
      combined_anchor_distance = c(
        combined_anchor_distance,
        path_offset[[first_layer]],
        path_end[[last_layer]]
      )
      combined_anchor_height = c(
        combined_anchor_height,
        layer_endpoint_height
      )
    }
  }
  combined_coords = apply_render_road_layer_profile(
    coords = combined_coords,
    anchor_distance = combined_anchor_distance,
    anchor_height = combined_anchor_height,
    allow_below_endpoint_minimum = allow_below_endpoint_minimum,
    profile_start_distance = if (length(layer_index)) {
      path_offset[[first_layer]]
    } else {
      0
    },
    profile_end_distance = if (length(layer_index)) {
      path_end[[last_layer]]
    } else {
      tail(path_end, 1L)
    },
    texture_world_scale = texture_world_scale
  )
  combined_distance = calculate_road_path_cumulative_distance(
    combined_coords,
    texture_world_scale = texture_world_scale
  )
  tolerance = max(1e-7, tail(path_end, 1L) * 1e-9)
  for (index in seq_along(chain_paths)) {
    in_path = combined_distance >= path_offset[[index]] - tolerance &
      combined_distance <= path_end[[index]] + tolerance
    coords = combined_coords[in_path, , drop = FALSE]
    if (chain_reverse[[index]]) {
      coords = coords[nrow(coords):1L, , drop = FALSE]
    }
    coord_list[[chain_paths[[index]]]] = coords
  }
  list(coord_list = coord_list, profiled_paths = chain_paths)
}

#' Find local road layer intersections
#'
#' @param coord_list List of scene coordinate matrices.
#' @param layer_explicit Whether each path has an explicit layer.
#' @param texture_world_scale Two-value x-z world scale.
#'
#' @return Data frame of intersecting paths and distances along each path.
#' @keywords internal
find_render_road_layer_intersections = function(
  coord_list,
  layer_explicit,
  texture_world_scale = c(1, 1)
) {
  empty_result = data.frame(
    path_a = integer(0),
    path_b = integer(0),
    distance_a = numeric(0),
    distance_b = numeric(0)
  )
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required when `layer` is supplied.")
  }
  valid_path = vapply(
    coord_list,
    function(coords) {
      is.matrix(coords) &&
        nrow(coords) >= 2L &&
        ncol(coords) >= 3L &&
        all(is.finite(coords[, c(1, 3), drop = FALSE]))
    },
    logical(1)
  )
  valid_index = which(valid_path)
  if (length(valid_index) < 2L) {
    return(empty_result)
  }
  texture_world_scale = validate_render_road_world_scale(texture_world_scale)
  line_geometry = sf::st_sfc(lapply(valid_index, function(path) {
    world_points = sweep(
      coord_list[[path]][, c(1, 3), drop = FALSE],
      2,
      texture_world_scale,
      FUN = "*"
    )
    sf::st_linestring(unname(world_points))
  }))
  candidates = sf::st_intersects(line_geometry, sparse = TRUE)
  events = list()
  event_index = 0L
  for (local_a in seq_along(valid_index)) {
    local_b_values = candidates[[local_a]]
    local_b_values = local_b_values[local_b_values > local_a]
    if (!length(local_b_values)) {
      next
    }
    path_a = valid_index[[local_a]]
    for (local_b in local_b_values) {
      path_b = valid_index[[local_b]]
      if (!layer_explicit[[path_a]] && !layer_explicit[[path_b]]) {
        next
      }
      intersection = tryCatch(
        suppressWarnings(sf::st_intersection(
          line_geometry[local_a],
          line_geometry[local_b]
        )),
        error = function(error) NULL
      )
      points = extract_render_road_intersection_points(intersection)
      if (!nrow(points)) {
        next
      }
      for (point_index in seq_len(nrow(points))) {
        projection_a = project_render_road_point(
          coords = coord_list[[path_a]],
          point = points[point_index, ],
          texture_world_scale = texture_world_scale
        )
        projection_b = project_render_road_point(
          coords = coord_list[[path_b]],
          point = points[point_index, ],
          texture_world_scale = texture_world_scale
        )
        if (is.null(projection_a) || is.null(projection_b)) {
          next
        }
        if (projection_a$at_endpoint && projection_b$at_endpoint) {
          next
        }
        event_index = event_index + 1L
        events[[event_index]] = data.frame(
          path_a = path_a,
          path_b = path_b,
          distance_a = projection_a$distance,
          distance_b = projection_b$distance
        )
      }
    }
  }
  if (!length(events)) {
    return(empty_result)
  }
  result = do.call(rbind, events)
  key = paste(
    result$path_a,
    result$path_b,
    round(result$distance_a, 8),
    round(result$distance_b, 8),
    sep = ":"
  )
  result[!duplicated(key), , drop = FALSE]
}

#' Extract point locations from a line intersection
#'
#' @param intersection Intersection geometry.
#'
#' @return Two-column matrix of point locations.
#' @keywords internal
extract_render_road_intersection_points = function(intersection) {
  empty_points = matrix(numeric(0), ncol = 2L)
  if (
    is.null(intersection) ||
      !length(intersection) ||
      all(sf::st_is_empty(intersection))
  ) {
    return(empty_points)
  }
  points = list()
  point_index = 0L
  append_geometry = function(geometry) {
    for (geometry_index in seq_along(geometry)) {
      single = sf::st_sfc(geometry[[geometry_index]])
      geometry_type = as.character(sf::st_geometry_type(single))
      if (geometry_type == "GEOMETRYCOLLECTION") {
        point_part = tryCatch(
          suppressWarnings(sf::st_collection_extract(single, "POINT")),
          error = function(error) NULL
        )
        line_part = tryCatch(
          suppressWarnings(sf::st_collection_extract(single, "LINESTRING")),
          error = function(error) NULL
        )
        if (!is.null(point_part) && length(point_part)) {
          append_geometry(point_part)
        }
        if (!is.null(line_part) && length(line_part)) {
          append_geometry(line_part)
        }
        next
      }
      dimension = suppressWarnings(sf::st_dimension(single))[[1]]
      coordinates = tryCatch(
        sf::st_coordinates(single),
        error = function(error) NULL
      )
      if (is.null(coordinates) || !nrow(coordinates)) {
        next
      }
      if (dimension == 0L) {
        point_index <<- point_index + 1L
        points[[point_index]] <<- coordinates[, 1:2, drop = FALSE]
      } else if (dimension == 1L) {
        line_midpoints = calculate_render_road_line_midpoints(coordinates)
        if (nrow(line_midpoints)) {
          point_index <<- point_index + 1L
          points[[point_index]] <<- line_midpoints
        }
      }
    }
    invisible(NULL)
  }
  append_geometry(intersection)
  if (!length(points)) {
    return(empty_points)
  }
  points = do.call(rbind, points)
  points = points[stats::complete.cases(points), , drop = FALSE]
  if (!nrow(points)) {
    return(empty_points)
  }
  points[!duplicated(round(points, 10)), , drop = FALSE]
}

#' Calculate midpoint locations for line geometries
#'
#' @param coordinates Matrix returned by `sf::st_coordinates()`.
#'
#' @return Two-column matrix containing one midpoint per line part.
#' @keywords internal
calculate_render_road_line_midpoints = function(coordinates) {
  coordinates = as.matrix(coordinates)
  if (nrow(coordinates) < 2L || ncol(coordinates) < 2L) {
    return(matrix(numeric(0), ncol = 2L))
  }
  level_columns = grep("^L[0-9]+$", colnames(coordinates))
  groups = if (length(level_columns)) {
    interaction(
      as.data.frame(coordinates[, level_columns, drop = FALSE]),
      drop = TRUE
    )
  } else {
    rep(1L, nrow(coordinates))
  }
  line_parts = split(seq_len(nrow(coordinates)), groups)
  midpoints = lapply(line_parts, function(index) {
    line = coordinates[index, 1:2, drop = FALSE]
    if (nrow(line) < 2L) {
      return(NULL)
    }
    segment_length = sqrt(rowSums(
      (line[-1, , drop = FALSE] - line[-nrow(line), , drop = FALSE])^2
    ))
    total_length = sum(segment_length)
    if (!is.finite(total_length) || total_length <= 0) {
      return(line[1, , drop = FALSE])
    }
    cumulative = c(0, cumsum(segment_length))
    target = total_length / 2
    segment = min(which(cumulative[-1L] >= target))
    fraction = (target - cumulative[[segment]]) / segment_length[[segment]]
    line[segment, , drop = FALSE] +
      fraction *
        (line[segment + 1L, , drop = FALSE] -
          line[segment, , drop = FALSE])
  })
  midpoints = Filter(Negate(is.null), midpoints)
  if (!length(midpoints)) {
    return(matrix(numeric(0), ncol = 2L))
  }
  do.call(rbind, midpoints)
}

#' Project a point onto a rendered road path
#'
#' @param coords Scene coordinate matrix.
#' @param point Two-value point in world x-z coordinates.
#' @param texture_world_scale Two-value x-z world scale.
#'
#' @return Projection distance information or `NULL`.
#' @keywords internal
project_render_road_point = function(
  coords,
  point,
  texture_world_scale = c(1, 1)
) {
  coords = as.matrix(coords)
  if (nrow(coords) < 2L || ncol(coords) < 3L) {
    return(NULL)
  }
  texture_world_scale = validate_render_road_world_scale(texture_world_scale)
  path = sweep(
    coords[, c(1, 3), drop = FALSE],
    2,
    texture_world_scale,
    FUN = "*"
  )
  segment = path[-1, , drop = FALSE] -
    path[-nrow(path), , drop = FALSE]
  segment_length_squared = rowSums(segment^2)
  valid_segment = is.finite(segment_length_squared) &
    segment_length_squared > 0
  if (!any(valid_segment)) {
    return(NULL)
  }
  difference = sweep(
    path[-nrow(path), , drop = FALSE],
    2,
    as.numeric(point[1:2]),
    FUN = "-"
  )
  fraction = -rowSums(difference * segment) / segment_length_squared
  fraction = pmin(pmax(fraction, 0), 1)
  projection = path[-nrow(path), , drop = FALSE] + fraction * segment
  distance_squared = rowSums(
    sweep(
      projection,
      2,
      as.numeric(point[1:2]),
      FUN = "-"
    )^2
  )
  distance_squared[!valid_segment] = Inf
  closest = which.min(distance_squared)
  if (!length(closest) || !is.finite(distance_squared[[closest]])) {
    return(NULL)
  }
  segment_length = sqrt(segment_length_squared)
  cumulative = c(0, cumsum(segment_length))
  distance = cumulative[[closest]] +
    fraction[[closest]] * segment_length[[closest]]
  total_length = cumulative[[length(cumulative)]]
  endpoint_tolerance = max(1e-7, total_length * 1e-9)
  list(
    distance = distance,
    total_length = total_length,
    at_endpoint = distance <= endpoint_tolerance ||
      distance >= total_length - endpoint_tolerance
  )
}

#' Calculate connected road layer groups
#'
#' @param path_count Number of paths.
#' @param edges Two-column data frame of intersecting path indices.
#'
#' @return Integer component label for every path.
#' @keywords internal
calculate_render_road_layer_components = function(path_count, edges) {
  component = integer(path_count)
  if (!nrow(edges)) {
    return(component)
  }
  adjacency = vector("list", path_count)
  for (edge_index in seq_len(nrow(edges))) {
    path_a = edges$path_a[[edge_index]]
    path_b = edges$path_b[[edge_index]]
    adjacency[[path_a]] = c(adjacency[[path_a]], path_b)
    adjacency[[path_b]] = c(adjacency[[path_b]], path_a)
  }
  component_id = 0L
  for (start in unique(c(edges$path_a, edges$path_b))) {
    if (component[[start]] != 0L) {
      next
    }
    component_id = component_id + 1L
    pending = start
    while (length(pending)) {
      path = pending[[length(pending)]]
      pending = pending[-length(pending)]
      if (component[[path]] != 0L) {
        next
      }
      component[[path]] = component_id
      neighbors = adjacency[[path]]
      pending = c(pending, neighbors[component[neighbors] == 0L])
    }
  }
  component
}

#' Interpolate road height at a path distance
#'
#' @param coords Scene coordinate matrix.
#' @param distance Distance along the path in world x-z units.
#' @param texture_world_scale Two-value x-z world scale.
#'
#' @return Interpolated road height in scene units.
#' @keywords internal
interpolate_render_road_path_height = function(
  coords,
  distance,
  texture_world_scale = c(1, 1)
) {
  path_distance = calculate_road_path_cumulative_distance(
    coords,
    texture_world_scale = texture_world_scale
  )
  if (length(path_distance) < 2L || !all(is.finite(path_distance))) {
    return(NA_real_)
  }
  distance = suppressWarnings(as.numeric(distance[[1]]))
  if (!is.finite(distance)) {
    return(NA_real_)
  }
  stats::approx(
    x = path_distance,
    y = coords[, 2],
    xout = distance,
    ties = "ordered",
    rule = 2
  )$y
}

#' Apply an intersection-anchored road layer profile
#'
#' @param coords Terrain-sampled scene coordinate matrix.
#' @param anchor_distance Distances of layer crossings along the path.
#' @param anchor_height Target scene heights at layer crossings.
#' @param allow_below_endpoint_minimum Default `FALSE`. Whether the profile may
#' fall below the lower of its two endpoint elevations.
#' @param profile_start_distance Default `0`. Start of the positive-layer deck
#' within the complete path.
#' @param profile_end_distance Default `NULL`, which uses the path end. End of
#' the positive-layer deck within the complete path.
#' @param texture_world_scale Two-value x-z world scale.
#'
#' @return Road coordinates with quadratic layer transitions.
#' @keywords internal
apply_render_road_layer_profile = function(
  coords,
  anchor_distance,
  anchor_height,
  allow_below_endpoint_minimum = FALSE,
  profile_start_distance = 0,
  profile_end_distance = NULL,
  texture_world_scale = c(1, 1)
) {
  coords = as.matrix(coords)
  if (nrow(coords) < 2L || ncol(coords) < 3L) {
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
  endpoint_tolerance = max(1e-7, path_length * 1e-9)
  if (is.null(profile_end_distance)) {
    profile_end_distance = path_length
  }
  profile_start_distance = suppressWarnings(as.numeric(
    profile_start_distance[[1]]
  ))
  profile_end_distance = suppressWarnings(as.numeric(
    profile_end_distance[[1]]
  ))
  if (
    !is.finite(profile_start_distance) ||
      !is.finite(profile_end_distance) ||
      profile_start_distance < -endpoint_tolerance ||
      profile_end_distance > path_length + endpoint_tolerance ||
      profile_end_distance - profile_start_distance <= endpoint_tolerance
  ) {
    return(coords)
  }
  profile_start_distance = max(profile_start_distance, 0)
  profile_end_distance = min(profile_end_distance, path_length)
  anchor_lower_bound = if (isTRUE(allow_below_endpoint_minimum)) {
    0
  } else {
    profile_start_distance
  }
  anchor_upper_bound = if (isTRUE(allow_below_endpoint_minimum)) {
    path_length
  } else {
    profile_end_distance
  }
  valid_anchor = is.finite(anchor_distance) &
    is.finite(anchor_height) &
    anchor_distance > anchor_lower_bound + endpoint_tolerance &
    anchor_distance < anchor_upper_bound - endpoint_tolerance
  anchor_distance = anchor_distance[valid_anchor]
  anchor_height = anchor_height[valid_anchor]
  if (!length(anchor_distance) && isTRUE(allow_below_endpoint_minimum)) {
    return(coords)
  }
  if (length(anchor_distance)) {
    anchor_order = order(anchor_distance)
    anchor_distance = anchor_distance[anchor_order]
    anchor_height = anchor_height[anchor_order]
    anchor_key = round(anchor_distance, 8)
    anchor_distance = vapply(
      split(anchor_distance, anchor_key),
      mean,
      numeric(1)
    )
    anchor_height = vapply(split(anchor_height, anchor_key), max, numeric(1))
    anchor_order = order(anchor_distance)
    anchor_distance = unname(anchor_distance[anchor_order])
    anchor_height = unname(anchor_height[anchor_order])
  }
  control_distance = if (isTRUE(allow_below_endpoint_minimum)) {
    c(0, anchor_distance, path_length)
  } else {
    c(profile_start_distance, anchor_distance, profile_end_distance)
  }
  tween_fraction = seq(0, 1, length.out = 9)
  tween_distance = unlist(lapply(
    seq_len(length(control_distance) - 1L),
    function(index) {
      control_distance[[index]] +
        (control_distance[[index + 1L]] - control_distance[[index]]) *
          tween_fraction
    }
  ))
  if (
    !isTRUE(allow_below_endpoint_minimum) &&
      profile_start_distance > endpoint_tolerance
  ) {
    tween_distance = c(
      tween_distance,
      profile_start_distance * tween_fraction
    )
  }
  if (
    !isTRUE(allow_below_endpoint_minimum) &&
      profile_end_distance < path_length - endpoint_tolerance
  ) {
    tween_distance = c(
      tween_distance,
      profile_end_distance +
        (path_length - profile_end_distance) * tween_fraction
    )
  }
  profile_distance = sort(unique(c(path_distance, tween_distance)))
  original_coords = coords
  coords = vapply(
    seq_len(ncol(coords)),
    function(column) {
      stats::approx(
        x = path_distance,
        y = original_coords[, column],
        xout = profile_distance,
        ties = "ordered"
      )$y
    },
    numeric(length(profile_distance))
  )
  coords[, 2] = NA_real_
  if (!isTRUE(allow_below_endpoint_minimum)) {
    deck_endpoint_height = stats::approx(
      x = path_distance,
      y = original_coords[, 2],
      xout = c(profile_start_distance, profile_end_distance),
      ties = "ordered"
    )$y
    baseline_at_anchor = deck_endpoint_height[[1]] +
      (deck_endpoint_height[[2]] - deck_endpoint_height[[1]]) *
        (anchor_distance - profile_start_distance) /
        (profile_end_distance - profile_start_distance)
    control_uplift = c(
      0,
      pmax(anchor_height - baseline_at_anchor, 0),
      0
    )
    in_deck = profile_distance >= profile_start_distance &
      profile_distance <= profile_end_distance
    coords[in_deck, 2] = deck_endpoint_height[[1]] +
      (deck_endpoint_height[[2]] - deck_endpoint_height[[1]]) *
        (profile_distance[in_deck] - profile_start_distance) /
        (profile_end_distance - profile_start_distance)
    profile_uplift = numeric(length(profile_distance))
    for (index in seq_len(length(control_distance) - 1L)) {
      interval_start = control_distance[[index]]
      interval_end = control_distance[[index + 1L]]
      in_interval = profile_distance >= interval_start &
        profile_distance <= interval_end
      progress = (profile_distance[in_interval] - interval_start) /
        (interval_end - interval_start)
      quadratic_progress = ifelse(
        progress < 0.5,
        2 * progress^2,
        1 - (-2 * progress + 2)^2 / 2
      )
      profile_uplift[in_interval] = control_uplift[[index]] +
        (control_uplift[[index + 1L]] - control_uplift[[index]]) *
          quadratic_progress
    }
    coords[in_deck, 2] = coords[in_deck, 2] + profile_uplift[in_deck]
    if (profile_start_distance > endpoint_tolerance) {
      before_deck = profile_distance <= profile_start_distance
      progress = profile_distance[before_deck] / profile_start_distance
      quadratic_progress = ifelse(
        progress < 0.5,
        2 * progress^2,
        1 - (-2 * progress + 2)^2 / 2
      )
      coords[before_deck, 2] = original_coords[1L, 2] +
        (deck_endpoint_height[[1]] - original_coords[1L, 2]) *
          quadratic_progress
    }
    if (profile_end_distance < path_length - endpoint_tolerance) {
      after_deck = profile_distance >= profile_end_distance
      progress = (profile_distance[after_deck] - profile_end_distance) /
        (path_length - profile_end_distance)
      quadratic_progress = ifelse(
        progress < 0.5,
        2 * progress^2,
        1 - (-2 * progress + 2)^2 / 2
      )
      coords[after_deck, 2] = deck_endpoint_height[[2]] +
        (original_coords[nrow(original_coords), 2] -
          deck_endpoint_height[[2]]) *
          quadratic_progress
    }
    return(coords)
  }
  control_height = c(
    original_coords[1, 2],
    anchor_height,
    original_coords[nrow(original_coords), 2]
  )
  for (index in seq_len(length(control_distance) - 1L)) {
    interval_start = control_distance[[index]]
    interval_end = control_distance[[index + 1L]]
    in_interval = profile_distance >= interval_start &
      profile_distance <= interval_end
    progress = (profile_distance[in_interval] - interval_start) /
      (interval_end - interval_start)
    quadratic_progress = ifelse(
      progress < 0.5,
      2 * progress^2,
      1 - (-2 * progress + 2)^2 / 2
    )
    coords[in_interval, 2] = control_height[[index]] +
      (control_height[[index + 1L]] - control_height[[index]]) *
        quadratic_progress
  }
  coords
}

#' Validate road world scale
#'
#' @param texture_world_scale Two-value x-z world scale.
#'
#' @return Validated world scale.
#' @keywords internal
validate_render_road_world_scale = function(texture_world_scale) {
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

#' Resolve road lane texture files by lane count
#'
#' @param coord_lanes Lane count for each rendered coordinate path.
#' @inheritParams render_road_paths
#'
#' @return List of texture paths or `NULL` entries for each path.
#' @keywords internal
resolve_road_lane_texture_files = function(
  coord_lanes,
  lane_texture,
  lane_texture_file,
  roadcolor,
  lane_color,
  centerline_color,
  edge_line_color,
  lane_line_width,
  lane_dash_fraction
) {
  if (!length(coord_lanes)) {
    return(list())
  }
  coord_lanes = vapply(coord_lanes, validate_road_lanes, integer(1))
  unique_lanes = unique(coord_lanes)
  unique_files = lapply(unique_lanes, function(lanes) {
    resolve_road_lane_texture_file(
      lane_texture = lane_texture,
      lane_texture_file = lane_texture_file,
      roadcolor = roadcolor,
      lanes = lanes,
      lane_color = lane_color,
      centerline_color = centerline_color,
      edge_line_color = edge_line_color,
      lane_line_width = lane_line_width,
      lane_dash_fraction = lane_dash_fraction
    )
  })
  unique_files[match(coord_lanes, unique_lanes)]
}

#' Resolve road lane texture file
#'
#' @inheritParams render_road_paths
#'
#' @return Texture file path or `NULL`.
#' @keywords internal
resolve_road_lane_texture_file = function(
  lane_texture,
  lane_texture_file,
  roadcolor,
  lanes,
  lane_color,
  centerline_color,
  edge_line_color,
  lane_line_width,
  lane_dash_fraction
) {
  if (!is.null(lane_texture_file)) {
    if (
      !is.character(lane_texture_file) ||
        length(lane_texture_file) != 1 ||
        is.na(lane_texture_file) ||
        !nzchar(lane_texture_file) ||
        !file.exists(lane_texture_file)
    ) {
      stop("`lane_texture_file` must be a path to an existing image file.")
    }
    return(normalizePath(lane_texture_file, winslash = "/", mustWork = TRUE))
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

#' Make default road lane texture
#'
#' @inheritParams render_road_paths
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
  lanes = validate_road_lanes(lanes)
  lane_line_width = validate_road_fraction(
    lane_line_width,
    "lane_line_width"
  )
  lane_dash_length = validate_waterpath_positive_number(
    lane_dash_length,
    "lane_dash_length"
  )
  lane_gap_length = validate_waterpath_positive_number(
    lane_gap_length,
    "lane_gap_length",
    allow_zero = TRUE
  )
  lane_dash_fraction = resolve_road_lane_dash_fraction(
    lane_dash_fraction,
    lane_dash_length = lane_dash_length,
    lane_gap_length = lane_gap_length
  )
  road_rgb = convert_color(roadcolor)
  texture = array(
    rep(road_rgb, each = size * size),
    dim = c(size, size, 3)
  )
  line_half_width = max(1L, round(size * lane_line_width / 2))
  draw_vertical_line = function(u, color, rows = seq_len(size)) {
    col = min(size, max(1L, round(u * (size - 1L)) + 1L))
    cols = seq.int(
      max(1L, col - line_half_width),
      min(size, col + line_half_width)
    )
    texture[rows, cols, ] <<- array(
      rep(convert_color(color), each = length(rows) * length(cols)),
      dim = c(length(rows), length(cols), 3)
    )
  }
  marking_positions = calculate_road_lane_marking_positions(lanes)
  draw_vertical_line(marking_positions$edge_lines[[1]], edge_line_color)
  draw_vertical_line(marking_positions$edge_lines[[2]], edge_line_color)
  if (length(marking_positions$dividers) > 0) {
    dash_rows = seq_len(max(1L, floor(size * lane_dash_fraction)))
    center_index = which.min(abs(marking_positions$dividers - 0.5))
    for (divider_index in seq_along(marking_positions$dividers)) {
      divider_color = if (divider_index == center_index) {
        centerline_color
      } else {
        lane_color
      }
      draw_vertical_line(
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
    write_linear = TRUE
  )
  normalizePath(texture_file, winslash = "/", mustWork = TRUE)
}

#' Validate road lanes
#'
#' @param lanes Number of lanes.
#'
#' @return Positive integer lane count.
#' @keywords internal
validate_road_lanes = function(lanes) {
  lanes = suppressWarnings(as.integer(lanes[1]))
  if (!is.finite(lanes) || lanes < 1L) {
    stop("`lanes` must be a positive integer.")
  }
  lanes
}

#' Calculate road lane marking positions
#'
#' @param lanes Number of lanes.
#'
#' @return List of edge line and lane divider positions in texture u
#' coordinates.
#' @keywords internal
calculate_road_lane_marking_positions = function(lanes) {
  lanes = validate_road_lanes(lanes)
  edge_offset = 0.5 / (lanes + 2)
  lane_edges = seq(edge_offset, 1 - edge_offset, length.out = lanes + 1L)
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

#' Resolve road lane texture mapping
#'
#' @param coord_list List of road coordinate matrices.
#' @param lane_texture_length Target or fixed texture repetition length.
#' @param lane_texture_mapping Texture mapping mode.
#' @param texture_world_scale Default `c(1, 1)`. Multipliers converting scene
#' x-z distances to world distances.
#'
#' @return List containing road lengths, texture lengths, and texture repeats.
#' @keywords internal
resolve_road_lane_texture_mapping = function(
  coord_list,
  lane_texture_length,
  lane_texture_mapping,
  texture_world_scale = c(1, 1)
) {
  road_lengths = vapply(
    coord_list,
    calculate_road_path_length,
    numeric(1),
    texture_world_scale = texture_world_scale
  )
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

#' Calculate road path world scale
#'
#' @param heightmap Heightmap matrix.
#' @param extent Scene extent.
#'
#' @return Two-value x-z multiplier from scene units to world units.
#' @keywords internal
calculate_road_path_world_scale = function(heightmap, extent) {
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
  c(
    abs(x_range) / (nrow(heightmap) - 1),
    abs(z_range) / (ncol(heightmap) - 1)
  )
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

#' Resolve road lane texture length
#'
#' @param lane_texture_length Requested texture length.
#' @param lane_dash_length Painted dash length.
#' @param lane_gap_length Gap length.
#'
#' @return Texture length in scene/world units.
#' @keywords internal
resolve_road_lane_texture_length = function(
  lane_texture_length,
  lane_dash_length,
  lane_gap_length
) {
  if (is.null(lane_texture_length)) {
    return(lane_dash_length + lane_gap_length)
  }
  validate_waterpath_positive_number(
    lane_texture_length,
    "lane_texture_length"
  )
}

#' Resolve road lane dash fraction
#'
#' @param lane_dash_fraction Requested dash fraction.
#' @param lane_dash_length Painted dash length.
#' @param lane_gap_length Gap length.
#'
#' @return Dash fraction.
#' @keywords internal
resolve_road_lane_dash_fraction = function(
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
  validate_road_fraction(
    lane_dash_fraction,
    "lane_dash_fraction"
  )
}

#' Calculate road path length
#'
#' @param points Road path points.
#' @param texture_world_scale Default `c(1, 1)`. Multipliers converting scene
#' x-z distances to world distances.
#'
#' @return Road path length in world x-z units.
#' @keywords internal
calculate_road_path_length = function(points, texture_world_scale = c(1, 1)) {
  cumulative_distance = calculate_road_path_cumulative_distance(
    points,
    texture_world_scale = texture_world_scale
  )
  if (!length(cumulative_distance)) {
    return(0)
  }
  cumulative_distance[[length(cumulative_distance)]]
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

#' Validate road fraction
#'
#' @param value Value to validate.
#' @param name Argument name.
#'
#' @return Numeric fraction.
#' @keywords internal
validate_road_fraction = function(value, name) {
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

#' Road path profile height
#'
#' @return Road slab height in scene units.
#' @keywords internal
roadpath_profile_height = function() {
  0.11
}

#' Make high-quality road path profile polygon
#'
#' @return Road profile polygon.
#' @keywords internal
make_render_highquality_road_path_polygon = function() {
  height = roadpath_profile_height()
  matrix(
    c(
      -0.5,
      0,
      0.5,
      0,
      0.5,
      height,
      -0.5,
      height
    ),
    ncol = 2,
    byrow = TRUE
  )
}

#' Make high-quality road path meshes
#'
#' @param tasks Road path task list.
#'
#' @return List of rayrender mesh objects.
#' @keywords internal
make_render_highquality_road_path_meshes = function(tasks) {
  meshes = lapply(tasks, function(task) {
    do.call(make_render_highquality_road_path_mesh, task)
  })
  Filter(Negate(is.null), meshes)
}

#' Make high-quality road path mesh
#'
#' @param points Path points.
#' @param bbox_center Scene center.
#' @param width Road width.
#' @param heightmap Cached heightmap.
#' @param zscale Effective zscale.
#' @param material Rayrender material.
#' @param texture_file Default `NULL`. Road texture file.
#' @param texture_length Texture repeat length in scene units.
#' @param texture_repeats Default `NULL`. Number of texture repeats over the
#' full road path.
#' @param texture_world_scale Default `c(1, 1)`. Multipliers converting scene
#' x-z distances to world distances.
#' @param terrain_following Default `TRUE`. Whether high-quality mesh
#' densification and road edges should follow the terrain.
#'
#' @return A rayrender mesh object or `NULL`.
#' @keywords internal
make_render_highquality_road_path_mesh = function(
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
  terrain_following = TRUE
) {
  terrain_following = validate_waterpath_logical(
    terrain_following,
    "terrain_following"
  )
  mesh_heightmap = if (isTRUE(terrain_following)) heightmap else NULL
  points = collapse_render_highquality_road_path_points(
    points,
    texture_world_scale = texture_world_scale
  )
  if (nrow(points) < 2) {
    return(NULL)
  }
  points = densify_render_highquality_water_path_points(
    points = points,
    width = width,
    heightmap = mesh_heightmap,
    zscale = zscale
  )
  points = collapse_render_highquality_road_path_points(
    points,
    texture_world_scale = texture_world_scale
  )
  if (nrow(points) < 2) {
    return(NULL)
  }
  half_width = width / 2
  road_height = diff(range(make_render_highquality_road_path_polygon()[, 2]))
  normals = interpolate_render_highquality_water_path_normals(
    points = points,
    heightmap = mesh_heightmap,
    zscale = zscale
  )
  segment_indices = seq_len(nrow(points) - 1L)
  next_indices = segment_indices + 1L
  segment_start = points[segment_indices, , drop = FALSE]
  segment_end = points[next_indices, , drop = FALSE]
  segment_normals = normalize_render_highquality_rows(
    normals[segment_indices, , drop = FALSE] +
      normals[next_indices, , drop = FALSE]
  )
  segment_normals = replace_invalid_render_highquality_vectors(
    segment_normals,
    fallback = c(0, 1, 0)
  )
  segment_tangents = segment_end - segment_start
  segment_tangents = segment_tangents -
    segment_normals * rowSums(segment_tangents * segment_normals)
  segment_tangents = replace_invalid_render_highquality_vectors(
    normalize_render_highquality_rows(segment_tangents),
    fallback = c(1, 0, 0)
  )
  horizontal_delta = segment_end[, c(1, 3), drop = FALSE] -
    segment_start[, c(1, 3), drop = FALSE]
  segment_side_vectors = normalize_render_highquality_rows(cbind(
    -horizontal_delta[, 2],
    0,
    horizontal_delta[, 1]
  ))
  segment_side_vectors = replace_invalid_render_highquality_vectors(
    segment_side_vectors,
    fallback = c(0, 0, 1)
  )
  edge_centers = make_render_highquality_water_path_edge_centers(
    points = rbind(segment_start, segment_end),
    side_vectors = rbind(segment_side_vectors, segment_side_vectors),
    half_width = half_width,
    heightmap = mesh_heightmap,
    zscale = zscale
  )
  segment_count = length(segment_indices)
  start_rows = seq_len(segment_count)
  end_rows = segment_count + start_rows
  left_start_bottom = edge_centers$left[start_rows, , drop = FALSE]
  left_end_bottom = edge_centers$left[end_rows, , drop = FALSE]
  right_start_bottom = edge_centers$right[start_rows, , drop = FALSE]
  right_end_bottom = edge_centers$right[end_rows, , drop = FALSE]
  edge_normals = interpolate_render_highquality_water_path_normals(
    points = rbind(
      left_start_bottom,
      left_end_bottom,
      right_start_bottom,
      right_end_bottom
    ),
    heightmap = mesh_heightmap,
    zscale = zscale
  )
  left_start_normals = edge_normals[start_rows, , drop = FALSE]
  left_end_normals = edge_normals[end_rows, , drop = FALSE]
  right_start_normals = edge_normals[
    2L * segment_count + start_rows,
    ,
    drop = FALSE
  ]
  right_end_normals = edge_normals[
    3L * segment_count + start_rows,
    ,
    drop = FALSE
  ]
  left_start_top = left_start_bottom + left_start_normals * road_height
  left_end_top = left_end_bottom + left_end_normals * road_height
  right_start_top = right_start_bottom + right_start_normals * road_height
  right_end_top = right_end_bottom + right_end_normals * road_height
  texture_length = validate_waterpath_positive_number(
    texture_length,
    "lane_texture_length"
  )
  center_dist = calculate_road_path_cumulative_distance(
    points,
    texture_world_scale = texture_world_scale
  )
  texture_repeats = suppressWarnings(as.numeric(texture_repeats[1]))
  if (
    length(texture_repeats) &&
      is.finite(texture_repeats) &&
      texture_repeats > 0
  ) {
    road_length = center_dist[[length(center_dist)]]
    texture_v = if (is.finite(road_length) && road_length > 0) {
      center_dist / road_length * texture_repeats
    } else {
      center_dist
    }
  } else {
    texture_v = center_dist / texture_length
  }
  v0 = texture_v[segment_indices]
  v1 = texture_v[next_indices]
  texcoords = make_render_highquality_water_path_quad_rows(
    cbind(0, v0),
    cbind(0, v1),
    cbind(1, v1),
    cbind(1, v0)
  )
  bottom_texcoords = make_render_highquality_water_path_quad_rows(
    cbind(0, v0),
    cbind(1, v0),
    cbind(1, v1),
    cbind(0, v1)
  )
  side_texture_u = c(0.01, 0.02)
  left_side_texcoords = make_render_highquality_water_path_quad_rows(
    cbind(side_texture_u[[1]], v0),
    cbind(side_texture_u[[1]], v1),
    cbind(side_texture_u[[2]], v1),
    cbind(side_texture_u[[2]], v0)
  )
  right_side_texcoords = make_render_highquality_water_path_quad_rows(
    cbind(side_texture_u[[1]], v0),
    cbind(side_texture_u[[2]], v0),
    cbind(side_texture_u[[2]], v1),
    cbind(side_texture_u[[1]], v1)
  )
  start_v = texture_v[[1L]]
  end_v = texture_v[[length(texture_v)]]
  cap_texture_v_span = 1e-4
  cap_texcoords = rbind(
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
    ),
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
  vertices = rbind(
    make_render_highquality_water_path_quad_rows(
      left_start_top,
      left_end_top,
      right_end_top,
      right_start_top
    ),
    make_render_highquality_water_path_quad_rows(
      left_start_bottom,
      right_start_bottom,
      right_end_bottom,
      left_end_bottom
    ),
    make_render_highquality_water_path_quad_rows(
      left_start_bottom,
      left_end_bottom,
      left_end_top,
      left_start_top
    ),
    make_render_highquality_water_path_quad_rows(
      right_start_bottom,
      right_start_top,
      right_end_top,
      right_end_bottom
    ),
    make_render_highquality_water_path_quad_rows(
      matrix(left_start_bottom[1L, ], nrow = 1L),
      matrix(left_start_top[1L, ], nrow = 1L),
      matrix(right_start_top[1L, ], nrow = 1L),
      matrix(right_start_bottom[1L, ], nrow = 1L)
    ),
    make_render_highquality_water_path_quad_rows(
      matrix(left_end_bottom[segment_count, ], nrow = 1L),
      matrix(right_end_bottom[segment_count, ], nrow = 1L),
      matrix(right_end_top[segment_count, ], nrow = 1L),
      matrix(left_end_top[segment_count, ], nrow = 1L)
    )
  )
  vertex_normals = rbind(
    make_render_highquality_water_path_quad_rows(
      normals[segment_indices, , drop = FALSE],
      normals[next_indices, , drop = FALSE],
      normals[next_indices, , drop = FALSE],
      normals[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      -normals[segment_indices, , drop = FALSE],
      -normals[segment_indices, , drop = FALSE],
      -normals[next_indices, , drop = FALSE],
      -normals[next_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      segment_side_vectors,
      segment_side_vectors,
      segment_side_vectors,
      segment_side_vectors
    ),
    make_render_highquality_water_path_quad_rows(
      -segment_side_vectors,
      -segment_side_vectors,
      -segment_side_vectors,
      -segment_side_vectors
    ),
    make_render_highquality_water_path_quad_rows(
      matrix(-segment_tangents[1L, ], nrow = 1L),
      matrix(-segment_tangents[1L, ], nrow = 1L),
      matrix(-segment_tangents[1L, ], nrow = 1L),
      matrix(-segment_tangents[1L, ], nrow = 1L)
    ),
    make_render_highquality_water_path_quad_rows(
      matrix(segment_tangents[segment_count, ], nrow = 1L),
      matrix(segment_tangents[segment_count, ], nrow = 1L),
      matrix(segment_tangents[segment_count, ], nrow = 1L),
      matrix(segment_tangents[segment_count, ], nrow = 1L)
    )
  )
  texcoords = rbind(
    texcoords,
    bottom_texcoords,
    left_side_texcoords,
    right_side_texcoords,
    cap_texcoords
  )
  quad_starts = seq(1L, nrow(vertices), by = 4L)
  indices = rbind(
    cbind(quad_starts, quad_starts + 1L, quad_starts + 2L),
    cbind(quad_starts, quad_starts + 2L, quad_starts + 3L)
  )
  vertex_normals = sanitize_render_highquality_road_quad_normals(
    vertices,
    vertex_normals
  )
  vertices = sweep(vertices, 2, bbox_center, FUN = "-")
  mesh = list(
    vb = t(cbind(vertices, 1)),
    it = t(indices),
    normals = t(vertex_normals),
    texcoords = t(texcoords),
    material = list(texture = texture_file, bump_texture = NULL, color = NULL)
  )
  class(mesh) = "mesh3d"
  rayrender::mesh3d_model(
    mesh,
    override_material = is.null(texture_file),
    material = material
  )
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
