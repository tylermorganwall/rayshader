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
#' treated as implicit layer `0`, except affirmative `bridge` or elevated
#' `location` metadata infer effective layer `1` and affirmative `tunnel` or
#' underground `location` metadata infer effective layer `-1`. Inferred values
#' remain distinguishable from explicit OSM layer tags in diagnostics.
#' Roads are grouped by exact physical events and conservative endpoint
#' continuations. A sparse quadratic profile solve enforces crossing clearance,
#' longitudinal grade and grade-rate limits, junction height continuity, and
#' selected through-road grade continuity. Surface roads retain a sampled
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
#' @return Invisibly returns the rendered road coordinates. When `layer` is
#' supplied, the result has `terrain_following` and `profile_diagnostics`
#' attributes describing the sparse profile solve.
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
      sprintf("`lanes` must name a column in `roads`: %s", lanes_column),
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
  layer_values = resolve_render_road_osm_layer_values(roads, layer_column)

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
  list(
    layer = layer_values$layer,
    explicit = layer_values$explicit,
    inferred = layer_values$inferred,
    source = layer_values$source,
    height = height
  )
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
    coord_list = solve_render_road_path_profiles(
      coord_list = coord_list,
      coord_feature = coord_feature,
      roads = roads,
      layer_column = road_layer_column,
      lane_column = road_lanes_column,
      layer_height_column = road_layer_height_column,
      layer_spacing = road_layer_spacing,
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

#' Solve road profiles for rendered path coordinates
#'
#' @param coord_list Terrain-sampled scene coordinate matrices.
#' @param coord_feature Source feature index for every coordinate matrix.
#' @param roads Prepared road features corresponding to `coord_feature`.
#' @param layer_column Column containing OSM-style layer values.
#' @param lane_column Default `NULL`. Optional lane-count column used as
#' continuation evidence.
#' @param layer_height_column Default `NULL`. Optional clearance column.
#' @param layer_spacing Default `5.5`. Fallback adjacent-layer clearance in
#' metres.
#' @param zscale Effective scene zscale.
#' @param texture_world_scale Two-value x-z multiplier from scene units to
#' world units.
#'
#' @return Coordinate matrices with solved heights and profile diagnostics.
#' @keywords internal
solve_render_road_path_profiles = function(
  coord_list,
  coord_feature,
  roads,
  layer_column,
  lane_column = NULL,
  layer_height_column = NULL,
  layer_spacing = 5.5,
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
  zscale = validate_waterpath_positive_number(zscale, "zscale")
  texture_world_scale = validate_render_road_world_scale(
    texture_world_scale
  )
  layer_spacing = if (is.null(layer_spacing)) {
    5.5
  } else {
    validate_waterpath_positive_number(layer_spacing, "layer_height")
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
  prepared = prepare_render_road_layer_features(
    roads = profile_roads,
    layer_column = layer_column,
    lane_column = lane_column,
    layer_height_column = layer_height_column
  )
  topology = build_render_road_layer_topology(prepared)
  active_fragment_id = topology$prospective_solve_fragment_id
  if (!length(active_fragment_id)) {
    attr(coord_list, "terrain_following") = terrain_following
    attr(coord_list, "profile_diagnostics") = list(
      solver = "sparse_qp",
      active_fragment_count = 0L,
      solve_component_count = 0L,
      refinement_iterations = 0L
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
    path_length = tail(path_distance, 1L)
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
    layer_spacing = layer_spacing
  )
  solution = solve_render_road_profile_problem(
    problem,
    maximum_iterations = 100000,
    profile_tolerance = 1e-3
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
      tail(path_distance, 1L)
    profile = evaluate_render_road_profile_at(
      problem = problem,
      solution = solution,
      fragment = solved_fragment_id[[solved_row]],
      distance = evaluation_distance
    )
    solved_height = profile$height / zscale
    terrain_following[[path_index]] = max(
      abs(solved_height - coordinates[, 2L])
    ) <=
      1e-3 / zscale
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
    engineering_audit_passed = solution$engineering_audit$passed
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
  total_length = tail(cumulative, 1L)
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
      tail(which(valid_segment), 1L)
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
  # Validate package availability and the basic sf input contract.
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for road layer topology.", call. = FALSE)
  }
  if (!inherits(roads, "sf")) {
    stop("Road layer topology requires an `sf` road object.", call. = FALSE)
  }
  # Resolve and validate the layer column before fragmenting the geometry.
  layer_column = validate_render_road_column_name(layer_column, "layer")
  if (!(layer_column %in% names(roads))) {
    stop(sprintf("`layer` must name a column in `roads`: %s", layer_column))
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
  roads$render_road_feature_id = seq_len(nrow(roads))
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
    raw_clearance = source_fragments[[layer_height_column]]
    if (is.factor(raw_clearance)) {
      raw_clearance = as.character(raw_clearance)
    }
    clearance = suppressWarnings(as.numeric(raw_clearance))
    present = !is.na(raw_clearance)
    if (is.character(raw_clearance)) {
      present = present & nzchar(trimws(raw_clearance))
    }
    if (any(present & (!is.finite(clearance) | clearance <= 0))) {
      stop("Explicit road layer heights must be positive finite numbers.")
    }
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
  total_length = tail(cumulative, 1L)
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
  if (nrow(endpoints) < 2L) {
    return(list(
      selected = empty,
      ambiguous = empty,
      rejected = empty,
      candidates = empty,
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
      ambiguous = empty,
      rejected = empty,
      candidates = empty,
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
  # Remove the temporary eligibility field and return selected, ambiguous, rejected,
  # and complete candidate tables for inspection.
  candidates$eligible = NULL
  candidates$continuation_id = seq_len(nrow(candidates))
  list(
    selected = candidates[candidates$status == "selected", , drop = FALSE],
    ambiguous = candidates[candidates$status == "ambiguous", , drop = FALSE],
    rejected = candidates[candidates$status == "rejected", , drop = FALSE],
    candidates = candidates,
    ambiguity_direction_margin = ambiguity_direction_margin
  )
}

#' Build fragment-pair edges from physical event participants
#'
#' @param participants Event participant table.
#' @param event_id_column Event identifier column.
#' @param edge_type Edge classification.
#'
#' @return Fragment-pair edge table or `NULL`.
#' @keywords internal
build_render_road_participant_edges = function(
  participants,
  event_id_column,
  edge_type
) {
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

#' Build the complete physical road-network graph
#'
#' @param prepared Prepared road layer features.
#' @param events Exact road layer events.
#' @param continuations Continuation selection diagnostics.
#'
#' @return Fragment graph and network-component membership.
#' @keywords internal
build_render_road_topology_graph = function(prepared, events, continuations) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The `igraph` package is required for road layer topology.")
  }
  # Represent every road fragment as one graph vertex with stable feature/layer metadata.
  fragments = prepared$fragments
  fragment_id = fragments$render_road_fragment_id
  vertices = data.frame(
    name = as.character(fragment_id),
    render_road_fragment_id = fragment_id,
    render_road_feature_id = fragments$render_road_feature_id,
    render_road_layer = fragments$render_road_layer,
    stringsAsFactors = FALSE
  )

  # Add every physical relationship to this descriptive road-network graph. It is
  # intentionally broader than the graph used to isolate sparse profile solves.
  edge_rows = list(
    build_render_road_participant_edges(
      events$point_events$participants,
      "point_event_id",
      "physical_point_event"
    )
  )
  if (nrow(events$layer_overlaps)) {
    edge_rows[[length(edge_rows) + 1L]] = data.frame(
      from = as.character(events$layer_overlaps$fragment_a),
      to = as.character(events$layer_overlaps$fragment_b),
      topology_type = "layer_overlap",
      stringsAsFactors = FALSE
    )
  }
  if (nrow(events$equal_layer_overlaps)) {
    edge_rows[[length(edge_rows) + 1L]] = data.frame(
      from = as.character(events$equal_layer_overlaps$fragment_a),
      to = as.character(events$equal_layer_overlaps$fragment_b),
      topology_type = "equal_layer_overlap",
      stringsAsFactors = FALSE
    )
  }
  # Selected continuation edges connect fragmented pieces of the same through road.
  if (nrow(continuations$selected)) {
    edge_rows[[length(edge_rows) + 1L]] = data.frame(
      from = as.character(continuations$selected$fragment_a),
      to = as.character(continuations$selected$fragment_b),
      topology_type = "continuation",
      stringsAsFactors = FALSE
    )
  }
  # Combine all edge sources into a single fragment-level topology graph.
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
  # Let igraph construct the undirected graph, including isolated fragment vertices.
  graph = igraph::graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = vertices
  )
  # These connected components describe only physical network connectivity. They are
  # never used directly as optimization components.
  component = igraph::components(graph)$membership
  components = data.frame(
    render_road_fragment_id = as.integer(names(component)),
    network_component_id = as.integer(component),
    stringsAsFactors = FALSE
  )
  components = components[
    match(fragment_id, components$render_road_fragment_id),
  ]
  # Return the graph, edge table, and fragment-aligned component membership.
  list(graph = graph, components = components, edges = edges)
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
  # Candidate anchors are endpoint-specific and use physical surface tags, not whether
  # a zero-valued layer happened to be explicit. They remain candidates until the
  # profile solver applies its final anchor policy.
  candidate_anchors = identify_render_road_candidate_anchor_endpoints(prepared)
  candidate_anchor_endpoint_id = candidate_anchors$candidate_anchor_endpoint_id
  reverse_continuation_edges = continuation_edges[
    !(continuation_edges$to_endpoint %in% candidate_anchor_endpoint_id),
    ,
    drop = FALSE
  ]
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
    continuation_edges[
      !(continuation_edges$from_endpoint %in% candidate_anchor_endpoint_id),
      ,
      drop = FALSE
    ],
    reverse_continuation_edges
  )

  # Track active solve context separately from fragments allowed to propagate it.
  # A surface fragment reached through an equality supplies terminal context without
  # opening the rest of its street network. Active fragments may still emit their
  # immediate junction equalities at candidate anchors so every incident approach
  # shares the junction height.
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
        as.character(expandable_fragment_id)
      permitted_continuation_id = sort(unique(c(
        permitted_continuation_id,
        directed_continuation_edges$continuation_id[traversed]
      )))
      target_fragment_id = as.integer(
        directed_continuation_edges$to[traversed]
      )
      target_endpoint_id = directed_continuation_edges$to_endpoint[traversed]
      terminal_target = target_fragment_id %in%
        candidate_anchors$surface_fragment_id &
        target_endpoint_id %in% candidate_anchor_endpoint_id
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
#' @return Prepared features, events, continuations, graph, and diagnostics.
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
  # Combine events and continuations into the fragment-level network graph.
  topology_graph = build_render_road_topology_graph(
    prepared = prepared,
    events = events,
    continuations = continuations
  )
  prospective_solve_graph = build_render_road_prospective_solve_graph(
    prepared = prepared,
    events = events,
    continuations = continuations
  )
  # Attach network and prospective solve membership independently. Inactive fragments
  # deliberately receive no prospective solve component.
  fragments = prepared$fragments
  fragments$network_component_id = topology_graph$components$network_component_id
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
    crossing_stack$local_order = ave(
      crossing_stack$render_road_layer,
      crossing_stack$crossing_id,
      FUN = function(value) {
        match(value, sort(unique(value)))
      }
    )
  } else {
    crossing_stack$local_order = integer(0)
  }

  # Return a single topology object containing normalized roads, event tables, continuation
  # decisions, graph structures, and reproducibility diagnostics for the next phase.
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
    ambiguous_continuations = continuations$ambiguous,
    rejected_continuations = continuations$rejected,
    continuation_candidates = continuations$candidates,
    network_graph = topology_graph$graph,
    network_graph_edges = topology_graph$edges,
    network_components = topology_graph$components,
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
      ambiguity_direction_margin = continuations$ambiguity_direction_margin
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
  # Resolve the set of effective layers and seed a readable diverging layer palette.
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
  # Generate deterministic fallback colors for layer values outside the common range.
  resolved = unname(default_colors[layer_levels])
  missing_color = is.na(resolved)
  if (any(missing_color)) {
    resolved[missing_color] = grDevices::hcl.colors(
      sum(missing_color),
      palette = "Dark 3"
    )
  }
  names(resolved) = layer_levels

  # Apply caller overrides, either positionally for all layers or by explicit layer name.
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
  # Validate the complete palette before plotting so graphics errors are reported clearly.
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
  # Return a named layer-to-color mapping used consistently across all panels.
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
  # Extract metric XY coordinates for one diagnostic line.
  coordinates = unclass(geometry)[, 1:2, drop = FALSE]
  if (nrow(coordinates) < 2L) {
    return(rep(NA_real_, 2L))
  }
  # Normalize the requested fractional position to the closed interval [0, 1].
  fraction = suppressWarnings(as.numeric(fraction[[1L]]))
  if (!is.finite(fraction)) {
    fraction = 0.5
  }
  fraction = min(max(fraction, 0), 1)
  # Compute segment lengths and total path length so the label point is based on distance
  # along the road rather than raw vertex index.
  segment = coordinates[-1L, , drop = FALSE] -
    coordinates[-nrow(coordinates), , drop = FALSE]
  segment_length = sqrt(rowSums(segment^2))
  total_length = sum(segment_length)
  if (!is.finite(total_length) || total_length <= 0) {
    return(colMeans(coordinates[c(1L, nrow(coordinates)), , drop = FALSE]))
  }
  # Locate the segment containing the target cumulative distance.
  cumulative = c(0, cumsum(segment_length))
  target_distance = total_length * fraction
  segment_index = which(cumulative[-1L] >= target_distance)[1L]
  if (!length(segment_index) || is.na(segment_index)) {
    segment_index = length(segment_length)
  }
  # Linearly interpolate within that segment to obtain the diagnostic point coordinate.
  local_fraction = if (segment_length[[segment_index]] > 0) {
    (target_distance - cumulative[[segment_index]]) /
      segment_length[[segment_index]]
  } else {
    0
  }
  coordinates[segment_index, ] + local_fraction * segment[segment_index, ]
}

#' Build pair-specific continuation diagnostic geometry
#'
#' @param topology Road topology diagnostics.
#' @param continuations Selected or ambiguous continuation table.
#' @param stub_length Default `8`. Inward stub length in metres.
#'
#' @return Continuation attributes with two inward stubs and a connecting chord.
#' @keywords internal
build_render_road_continuation_diagnostic_geometry = function(
  topology,
  continuations,
  stub_length = 8
) {
  empty = sf::st_sf(
    continuation_id = integer(0),
    fragment_a = integer(0),
    fragment_b = integer(0),
    pair_label = character(0),
    label_x = numeric(0),
    label_y = numeric(0),
    geometry = sf::st_sfc(crs = sf::st_crs(topology$fragments))
  )
  if (!nrow(continuations)) {
    return(empty)
  }
  stub_length = suppressWarnings(as.numeric(stub_length[[1L]]))
  if (!is.finite(stub_length) || stub_length <= 0) {
    stub_length = 8
  }
  endpoints = topology$endpoints
  fragments = topology$fragments
  endpoint_coordinates = sf::st_coordinates(endpoints)[, 1:2, drop = FALSE]

  # Interpolate a point a physical distance inward from a specific endpoint.
  inward_stub = function(fragment_id, endpoint_side) {
    fragment_row = match(fragment_id, fragments$render_road_fragment_id)
    geometry = sf::st_geometry(fragments)[[fragment_row]]
    coordinates = unclass(geometry)[, 1:2, drop = FALSE]
    segment_length = sqrt(rowSums(
      (coordinates[-1L, , drop = FALSE] -
        coordinates[-nrow(coordinates), , drop = FALSE])^2
    ))
    total_length = sum(segment_length)
    used_length = min(stub_length, total_length * 0.3)
    fraction = used_length / total_length
    if (identical(endpoint_side, "end")) {
      fraction = 1 - fraction
    }
    calculate_render_road_topology_line_point(geometry, fraction)
  }

  rows = lapply(seq_len(nrow(continuations)), function(row) {
    endpoint_a_row = match(
      continuations$endpoint_a[[row]],
      endpoints$render_road_endpoint_id
    )
    endpoint_b_row = match(
      continuations$endpoint_b[[row]],
      endpoints$render_road_endpoint_id
    )
    endpoint_a = endpoint_coordinates[endpoint_a_row, ]
    endpoint_b = endpoint_coordinates[endpoint_b_row, ]
    stub_a = inward_stub(
      continuations$fragment_a[[row]],
      continuations$side_a[[row]]
    )
    stub_b = inward_stub(
      continuations$fragment_b[[row]],
      continuations$side_b[[row]]
    )
    geometry = sf::st_multilinestring(list(
      rbind(endpoint_a, stub_a),
      rbind(stub_a, stub_b),
      rbind(stub_b, endpoint_b)
    ))
    sf::st_sf(
      continuation_id = continuations$continuation_id[[row]],
      fragment_a = continuations$fragment_a[[row]],
      fragment_b = continuations$fragment_b[[row]],
      pair_label = sprintf(
        "F%d%sF%d",
        continuations$fragment_a[[row]],
        "<->",
        continuations$fragment_b[[row]]
      ),
      label_x = mean(c(stub_a[[1L]], stub_b[[1L]])),
      label_y = mean(c(stub_a[[2L]], stub_b[[2L]])),
      geometry = sf::st_sfc(geometry, crs = sf::st_crs(fragments))
    )
  })
  do.call(rbind, rows)
}

#' Draw dense local layer ranks at crossing events
#'
#' @param topology Road topology diagnostics.
#' @param bounds Four named metric plot bounds.
#' @param label_size Text size multiplier.
#' @param maximum_events Default `8`. Maximum crossing events labeled in one
#' panel.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
draw_render_road_crossing_order_labels = function(
  topology,
  bounds,
  label_size = 0.55,
  maximum_events = 8
) {
  if (!nrow(topology$crossings) || !nrow(topology$crossing_participants)) {
    return(invisible(NULL))
  }
  crossing_coordinates = sf::st_coordinates(topology$crossings)[,
    1:2,
    drop = FALSE
  ]
  visible = crossing_coordinates[, 1L] >= bounds[["xmin"]] &
    crossing_coordinates[, 1L] <= bounds[["xmax"]] &
    crossing_coordinates[, 2L] >= bounds[["ymin"]] &
    crossing_coordinates[, 2L] <= bounds[["ymax"]]
  visible_id = topology$crossings$crossing_id[visible]
  if (!length(visible_id)) {
    return(invisible(NULL))
  }
  if (length(visible_id) > maximum_events) {
    center = c(
      mean(bounds[c("xmin", "xmax")]),
      mean(bounds[c("ymin", "ymax")])
    )
    visible_rows = which(visible)
    separation = rowSums(
      sweep(
        crossing_coordinates[visible_rows, , drop = FALSE],
        2,
        center,
        FUN = "-"
      )^2
    )
    visible_id = topology$crossings$crossing_id[
      visible_rows[order(separation)[seq_len(maximum_events)]]
    ]
  }
  offset = max(
    bounds[["xmax"]] - bounds[["xmin"]],
    bounds[["ymax"]] - bounds[["ymin"]]
  ) *
    0.055
  for (crossing_id in visible_id) {
    participant = topology$crossing_participants[
      topology$crossing_participants$crossing_id == crossing_id,
      ,
      drop = FALSE
    ]
    crossing_row = match(crossing_id, topology$crossings$crossing_id)
    center = crossing_coordinates[crossing_row, ]
    angle = seq(0, 2 * pi, length.out = nrow(participant) + 1L)[
      -(nrow(participant) + 1L)
    ]
    horizontal = abs(cos(angle)) >= abs(sin(angle))
    text_position = ifelse(
      horizontal,
      ifelse(cos(angle) >= 0, 4, 2),
      ifelse(sin(angle) >= 0, 3, 1)
    )
    graphics::text(
      center[[1L]] + cos(angle) * offset,
      center[[2L]] + sin(angle) * offset,
      labels = sprintf(
        "F%d  L%s  R%d",
        participant$render_road_fragment_id,
        participant$render_road_layer,
        participant$local_order
      ),
      cex = label_size,
      col = "#8b0000",
      font = 2,
      pos = text_position,
      xpd = NA
    )
  }
  invisible(NULL)
}

#' Select spatial diagnostic label rows
#'
#' @param x Label x coordinates.
#' @param y Label y coordinates.
#' @param bounds Four named metric plot bounds.
#' @param maximum_labels Maximum labels returned.
#'
#' @return Integer row indices inside the plot bounds.
#' @keywords internal
select_render_road_diagnostic_label_rows = function(
  x,
  y,
  bounds,
  maximum_labels
) {
  rows = which(
    x >= bounds[["xmin"]] &
      x <= bounds[["xmax"]] &
      y >= bounds[["ymin"]] &
      y <= bounds[["ymax"]]
  )
  if (length(rows) <= maximum_labels) {
    return(rows)
  }
  rows[unique(round(seq(1, length(rows), length.out = maximum_labels)))]
}

#' Resolve the detailed road topology plot bounds
#'
#' @param topology Road topology diagnostics.
#' @param focus Default `NULL`. Optional metric focus bounds or center.
#'
#' @return Four named metric plot bounds.
#' @keywords internal
resolve_render_road_topology_focus = function(topology, focus = NULL) {
  # Establish full-network metric bounds and a default detail-panel footprint.
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

  # Honor an explicit focus center or bounding box after strict numeric validation.
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
    # Without an explicit focus, locate the crossing with the densest nearby cluster of
    # other layer crossings. This automatically centers the diagnostic on an interchange.
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
    # If no layer crossings exist, fall back to the center of the complete road extent.
  } else {
    center = c(
      mean(complete_bounds[c("xmin", "xmax")]),
      mean(complete_bounds[c("ymin", "ymax")])
    )
  }
  # Convert the selected center into named plot bounds used by the drawing function.
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
#' @param continuation_stub_length Inward continuation stub length in metres.
#' @param label_event_order Whether to label local dense crossing ranks.
#' @param label_continuation_pairs Whether to label continuation fragment pairs.
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
  label_size = 0.55,
  continuation_stub_length = 8,
  label_event_order = FALSE,
  label_continuation_pairs = TRUE
) {
  # Cache fragments and define a distance-based helper for line labels and continuation
  # symbols.
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

  # Draw the cleaned road fragments first, colored only by effective OSM layer.
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

  # Highlight only fragments reached by the prospective solve graph. The original
  # layer color is redrawn over a pale halo so network and solve membership remain
  # simultaneously legible.
  solve_rows = fragments$render_road_fragment_id %in%
    topology$prospective_solve_fragment_id
  if (any(solve_rows)) {
    graphics::plot(
      sf::st_geometry(fragments[solve_rows, , drop = FALSE]),
      add = TRUE,
      col = "#7a4bb7",
      lwd = road_width + 5
    )
    graphics::plot(
      sf::st_geometry(fragments[solve_rows, , drop = FALSE]),
      add = TRUE,
      col = unname(layer_colors[as.character(
        fragments$render_road_layer[solve_rows]
      )]),
      lwd = road_width + 0.8
    )
  }

  # Overlay line-overlap events with a wide halo so they remain visible above road colors.
  if (nrow(topology$layer_overlaps)) {
    graphics::plot(
      sf::st_geometry(topology$layer_overlaps),
      add = TRUE,
      col = "#00a6a6",
      lwd = road_width + 4
    )
    graphics::plot(
      sf::st_geometry(topology$layer_overlaps),
      add = TRUE,
      col = "#bdf2f2",
      lwd = max(2, road_width - 1)
    )
  }
  if (nrow(topology$equal_layer_overlaps)) {
    graphics::plot(
      sf::st_geometry(topology$equal_layer_overlaps),
      add = TRUE,
      col = "#8e6c8a",
      lwd = road_width + 2,
      lty = 3
    )
  }
  # Mark derived junction-equality candidate events in green.
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
  # Mark interior unequal-layer crossings in red. These are the sparse points that will
  # eventually receive vertical ordering and clearance constraints.
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
  # Unequal-layer endpoint relationships are retained as explicit conflicts instead
  # of being silently interpreted as either junction equality or vertical clearance.
  if (nrow(topology$topology_conflicts)) {
    graphics::plot(
      sf::st_geometry(topology$topology_conflicts),
      add = TRUE,
      pch = 8,
      col = "#cc5500",
      lwd = 2,
      cex = max(0.8, min(1.4, road_width / 1.8))
    )
    if (isTRUE(label_event_order)) {
      conflict_coordinates = sf::st_coordinates(topology$topology_conflicts)[,
        1:2,
        drop = FALSE
      ]
      label_rows = select_render_road_diagnostic_label_rows(
        conflict_coordinates[, 1L],
        conflict_coordinates[, 2L],
        bounds,
        8L
      )
      conflict_labels = vapply(
        topology$topology_conflicts$conflict_id[label_rows],
        function(conflict_id) {
          rows = topology$topology_conflict_pairs$conflict_id == conflict_id
          paste(
            sprintf(
              "F%d-F%d %s",
              topology$topology_conflict_pairs$fragment_a[rows],
              topology$topology_conflict_pairs$fragment_b[rows],
              topology$topology_conflict_pairs$topology_relation[rows]
            ),
            collapse = "\n"
          )
        },
        character(1)
      )
      graphics::text(
        conflict_coordinates[label_rows, , drop = FALSE],
        labels = conflict_labels,
        pos = 1,
        offset = 1.2,
        cex = label_size,
        col = "#9c3d00",
        font = 2,
        xpd = NA
      )
    }
  }

  # Mark only endpoints near a boundary explicitly supplied by the caller.
  boundary_endpoints = topology$endpoints[
    topology$endpoints$supplied_boundary,
    ,
    drop = FALSE
  ]
  if (nrow(boundary_endpoints)) {
    graphics::plot(
      sf::st_geometry(boundary_endpoints),
      add = TRUE,
      pch = 22,
      bg = "#cab2d6",
      col = "#54278f",
      cex = max(0.55, min(1, road_width / 3)),
      lwd = 1.4
    )
  }

  # Exact endpoint connectors collapse to a point, so draw each candidate as two
  # physical inward stubs joined by a chord and label the fragment pair.
  selected_geometry = build_render_road_continuation_diagnostic_geometry(
    topology,
    topology$selected_continuations,
    continuation_stub_length
  )
  ambiguous_geometry = build_render_road_continuation_diagnostic_geometry(
    topology,
    topology$ambiguous_continuations,
    continuation_stub_length
  )
  if (nrow(topology$selected_continuations)) {
    graphics::plot(
      sf::st_geometry(selected_geometry),
      add = TRUE,
      col = "#0878d1",
      lwd = max(1.2, road_width * 0.65)
    )
    if (isTRUE(label_continuation_pairs)) {
      label_rows = select_render_road_diagnostic_label_rows(
        selected_geometry$label_x,
        selected_geometry$label_y,
        bounds,
        24L
      )
      graphics::text(
        selected_geometry$label_x[label_rows],
        selected_geometry$label_y[label_rows],
        labels = selected_geometry$pair_label[label_rows],
        cex = label_size,
        col = "#005a9c",
        font = 2,
        pos = 3,
        xpd = NA
      )
    }
  }
  # Draw ambiguous continuation candidates with dashed orange connectors and triangles.
  if (nrow(topology$ambiguous_continuations)) {
    graphics::plot(
      sf::st_geometry(ambiguous_geometry),
      add = TRUE,
      col = "#ed8b00",
      lwd = max(1.2, road_width * 0.5),
      lty = 2
    )
    if (isTRUE(label_continuation_pairs)) {
      label_rows = select_render_road_diagnostic_label_rows(
        ambiguous_geometry$label_x,
        ambiguous_geometry$label_y,
        bounds,
        24L
      )
      graphics::text(
        ambiguous_geometry$label_x[label_rows],
        ambiguous_geometry$label_y[label_rows],
        labels = ambiguous_geometry$pair_label[label_rows],
        cex = label_size,
        col = "#8c510a",
        font = 2,
        pos = 1,
        xpd = NA
      )
    }
  }

  if (isTRUE(label_event_order)) {
    draw_render_road_crossing_order_labels(topology, bounds, label_size)
  }

  # Select valid fragment IDs, place labels at alternating along-road fractions, and show
  # source feature, fragment, layer, and component IDs without moving the road geometry.
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
        "S%d/F%d L%s N%d P%s",
        fragments$render_road_feature_id[label_rows],
        fragments$render_road_fragment_id[label_rows],
        fragments$render_road_layer[label_rows],
        fragments$network_component_id[label_rows],
        ifelse(
          is.na(fragments$prospective_solve_component_id[label_rows]),
          "-",
          fragments$prospective_solve_component_id[label_rows]
        )
      ),
      pos = 3,
      cex = label_size,
      col = "#151515",
      xpd = NA
    )
  }

  # Place independent labels for physical-network and prospective-solve components.
  if (isTRUE(label_components)) {
    fragment_midpoints = line_points(fragments)
    network_groups = split(
      seq_len(nrow(fragments)),
      fragments$network_component_id
    )
    for (component in names(network_groups)) {
      component_point = colMeans(fragment_midpoints[
        network_groups[[component]],
        ,
        drop = FALSE
      ])
      graphics::text(
        component_point[[1L]],
        component_point[[2L]],
        labels = paste0("N", component),
        cex = 0.8,
        font = 2,
        col = grDevices::adjustcolor("#005a9c", alpha.f = 0.8),
        pos = 4
      )
    }
    solve_groups = split(
      which(!is.na(fragments$prospective_solve_component_id)),
      fragments$prospective_solve_component_id[
        !is.na(fragments$prospective_solve_component_id)
      ]
    )
    for (component in names(solve_groups)) {
      component_point = colMeans(fragment_midpoints[
        solve_groups[[component]],
        ,
        drop = FALSE
      ])
      graphics::text(
        component_point[[1L]],
        component_point[[2L]],
        labels = paste0("P", component),
        cex = 0.8,
        font = 2,
        col = "#54278f",
        pos = 2
      )
    }
  }

  # Build a combined legend for layer colors and topology-event symbols.
  if (isTRUE(show_legend)) {
    layers_present = sort(unique(fragments$render_road_layer))
    layer_count = length(layers_present)
    graphics::legend(
      "bottomleft",
      legend = c(
        paste0("layer ", layers_present),
        "junction/equality candidate",
        "interior crossing",
        "topology/layer conflict",
        "layer overlap",
        "equal-layer overlap",
        "supplied boundary",
        "prospective solve road",
        "selected continuation",
        "ambiguous continuation"
      ),
      col = c(
        unname(layer_colors[as.character(layers_present)]),
        "#005a24",
        "#d50000",
        "#cc5500",
        "#00a6a6",
        "#8e6c8a",
        "#54278f",
        "#7a4bb7",
        "#0878d1",
        "#ed8b00"
      ),
      lwd = c(rep(4, layer_count), NA, 2, 2, 6, 4, NA, 7, 3, 2),
      lty = c(rep(1, layer_count), NA, NA, NA, 1, 3, NA, 1, 1, 2),
      pch = c(rep(NA, layer_count), 21, 4, 8, NA, NA, 22, NA, NA, NA),
      pt.bg = c(
        rep(NA, layer_count),
        "#40c463",
        NA,
        NA,
        NA,
        NA,
        "#cab2d6",
        NA,
        NA,
        NA
      ),
      bg = grDevices::adjustcolor("white", alpha.f = 0.9),
      cex = 0.72
    )
  }
  # Add a compact count summary for visual checkpoint reporting.
  graphics::mtext(
    sprintf(
      paste0(
        "%d fragments | %d junctions | %d crossings | %d layer overlaps | ",
        "%d conflicts | %d selected | %d ambiguous | network max %d | solve %d"
      ),
      nrow(fragments),
      nrow(topology$junctions),
      nrow(topology$crossings),
      nrow(topology$layer_overlaps),
      nrow(topology$topology_conflicts),
      nrow(topology$selected_continuations),
      nrow(topology$ambiguous_continuations),
      max(table(fragments$network_component_id)),
      length(topology$prospective_solve_fragment_id)
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
#' Point pairs are grouped into unified physical events before the plot
#' distinguishes junction-equality candidates, interior crossings,
#' topology/layer conflicts, overlaps, selected and ambiguous continuations,
#' physical network components, and the smaller prospective profile-solve
#' graph.
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
#' @param direction_lookahead Default `8`. Physical distance in metres used to
#' estimate inward endpoint tangents.
#' @param continuation_ambiguity_margin Default `0.04`. Direction-score margin
#' used to identify near-ties within the best continuation evidence tier.
#' @param continuation_stub_length Default `8`. Physical inward-stub length in
#' metres used to draw pair-specific continuation diagnostics.
#' @param views Default `c("overview", "crossing_detail")`. Panels to draw.
#' Supported values are `"overview"` and `"crossing_detail"`.
#' @param focus Default `NULL`. Optional two-value metric center or four-value
#' metric bounding box for the crossing-detail panel. By default, the function
#' locates the densest crossing neighborhood.
#' @param labels Default `TRUE`. Whether to label source, fragment, layer, and
#' component IDs.
#' @param max_labels Default `40`. Maximum number of fragment labels per panel.
#' @param label_components Default `TRUE`. Whether to draw component IDs.
#' @param label_event_order Default `TRUE`. Whether the detail panel labels each
#' crossing participant with its layer and dense local rank.
#' @param label_continuation_pairs Default `TRUE`. Whether to label selected and
#' ambiguous continuation fragment pairs.
#' @param layer_colors Default `NULL`. Optional named or layer-ordered colors.
#' @param main Default `NULL`. Optional title or one title per requested view.
#' @param filename Default `NULL`. Optional PNG output path. When omitted, the
#' diagnostic is drawn on the active graphics device.
#' @param width Default `2400`. PNG width in pixels.
#' @param height Default `1400`. PNG height in pixels.
#' @param res Default `150`. PNG resolution in pixels per inch.
#'
#' @return Invisibly returns a `render_road_topology` list containing prepared
#' fragments, exact events, continuation diagnostics, network and prospective
#' solve `igraph` objects, component membership, and plot metadata.
#' @export
plot_render_road_topology = function(
  roads,
  layer,
  layer_height = 5.5,
  boundary = NULL,
  boundary_tolerance = 1,
  endpoint_tolerance = 1e-2,
  continuation_tolerance = 0.25,
  direction_lookahead = 8,
  continuation_ambiguity_margin = 0.04,
  continuation_stub_length = 8,
  views = c("overview", "crossing_detail"),
  focus = NULL,
  labels = TRUE,
  max_labels = 40,
  label_components = TRUE,
  label_event_order = TRUE,
  label_continuation_pairs = TRUE,
  layer_colors = NULL,
  main = NULL,
  filename = NULL,
  width = 2400,
  height = 1400,
  res = 150
) {
  # Validate the public diagnostic entry point and its required sf input.
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for road topology plots.", call. = FALSE)
  }
  if (!inherits(roads, "sf") || !nrow(roads)) {
    stop("`roads` must be a non-empty `sf` road object.", call. = FALSE)
  }
  # Resolve tidy-evaluation or character-column input for the OSM layer field.
  layer_column = resolve_render_road_column(
    value = layer,
    value_expr = substitute(layer),
    value_missing = missing(layer),
    argument = "layer"
  )
  if (is.null(layer_column)) {
    stop("`layer` must name a column in `roads`.", call. = FALSE)
  }
  # Resolve layer_height as either a constant spacing or a per-feature column. Phase 1
  # stores this specification for later constraint construction but does not solve heights.
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
  # Centralize scalar tolerance validation for the three metric tolerances exposed here.
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
  direction_lookahead = validate_tolerance(
    direction_lookahead,
    "direction_lookahead"
  )
  continuation_ambiguity_margin = validate_tolerance(
    continuation_ambiguity_margin,
    "continuation_ambiguity_margin",
    allow_zero = TRUE
  )
  continuation_stub_length = validate_tolerance(
    continuation_stub_length,
    "continuation_stub_length"
  )

  # Validate and de-duplicate the requested diagnostic views while preserving order.
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
  # Validate logical display switches separately so NA cannot silently alter plotting.
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
  for (argument in c("label_event_order", "label_continuation_pairs")) {
    value = get(argument)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("`%s` must be `TRUE` or `FALSE`.", argument), call. = FALSE)
    }
  }
  # Limit labels to a non-negative integer count to prevent unreadable dense panels.
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

  # Execute the Phase 1 pipeline: normalize fragments, detect local events, select
  # continuations, and construct graph diagnostics.
  prepared = prepare_render_road_layer_features(
    roads = roads,
    layer_column = layer_column,
    layer_height_column = layer_height_spec$column,
    boundary = boundary,
    boundary_tolerance = boundary_tolerance,
    direction_lookahead = direction_lookahead
  )
  topology = build_render_road_layer_topology(
    prepared = prepared,
    endpoint_tolerance = endpoint_tolerance,
    continuation_tolerance = continuation_tolerance,
    ambiguity_direction_margin = continuation_ambiguity_margin
  )
  # Attach the unresolved height specification and a class marker to the returned object.
  topology$layer_spacing = layer_height_spec$spacing
  topology$layer_height_column = layer_height_spec$column
  class(topology) = c("render_road_topology", class(topology))

  # Build padded overview bounds and a data-driven crossing-detail view.
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
  # Resolve one consistent layer palette for all requested panels.
  resolved_colors = resolve_render_road_topology_layer_colors(
    topology$fragments$render_road_layer,
    layer_colors
  )

  # Evenly subsample candidate fragment IDs when labels exceed max_labels.
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
  # Prefer explicit or metadata-inferred layer roads for overview labels because
  # they are the roads that drive vertical ordering. Fall back to all fragments
  # when no vertical structure is present.
  fragments = topology$fragments
  overview_candidates = fragments$render_road_fragment_id[
    fragments$render_road_layer_explicit |
      fragments$render_road_layer_inferred
  ]
  if (!length(overview_candidates)) {
    overview_candidates = fragments$render_road_fragment_id
  }
  # Identify fragments intersecting the detail window using sf's spatial index.
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
  # Within the detail window, prioritize roads that participate in crossings or junctions.
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
  # Store the final label selections independently for overview and detail panels.
  panel_labels = list(
    overview = sample_fragment_ids(overview_candidates),
    crossing_detail = sample_fragment_ids(detail_candidates)
  )

  # Resolve default or caller-supplied panel titles.
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

  # Optionally open a PNG device after validating the output path and dimensions.
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
  # Capture and restore graphics parameters, and close only the device opened here.
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
  # Arrange one row of panels and delegate all topology drawing to the panel helper.
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
      label_size = if (identical(view, "overview")) 0.5 else 0.55,
      continuation_stub_length = continuation_stub_length,
      label_event_order = label_event_order &&
        identical(view, "crossing_detail"),
      label_continuation_pairs = label_continuation_pairs
    )
  }

  # Record plot metadata in the returned topology object for reproducibility and return
  # invisibly so callers can inspect all event and graph tables after drawing.
  topology$plot = list(
    views = views,
    bounds = panel_bounds[views],
    layer_colors = resolved_colors,
    direction_lookahead = direction_lookahead,
    continuation_ambiguity_margin = continuation_ambiguity_margin,
    continuation_stub_length = continuation_stub_length,
    filename = filename
  )
  invisible(topology)
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
    length = tail(distance, 1L)
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
        tail(profile$distance, 1L) < info$length - tolerance
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

#' Interpolate metric coordinates along a road fragment
#'
#' @param geometry Metric LINESTRING geometry.
#' @param distance Distances to evaluate in metres.
#'
#' @return Matrix of interpolated metric x/y coordinates.
#' @keywords internal
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

#' Validate one road-profile solver setting
#'
#' @param value Setting value.
#' @param argument Argument name used in errors.
#' @param allow_zero Default `FALSE`. Whether zero is valid.
#'
#' @return A validated numeric scalar.
#' @keywords internal
validate_render_road_profile_setting = function(
  value,
  argument,
  allow_zero = FALSE
) {
  if (!is.numeric(value) || length(value) != 1L) {
    stop(sprintf("`%s` must be a single number.", argument), call. = FALSE)
  }
  value = as.numeric(value[[1L]])
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

#' Subset topology inputs to prospective road-profile fragments
#'
#' @param topology Road topology diagnostics.
#' @param terrain_profiles Default `NULL`. Terrain profiles before subsetting.
#' @param explicit_controls Default `NULL`. Explicit controls before subsetting.
#'
#' @return Active topology and aligned caller inputs.
#' @keywords internal
subset_render_road_profile_topology = function(
  topology,
  terrain_profiles = NULL,
  explicit_controls = NULL
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
  if (
    is.list(explicit_controls) &&
      is.null(names(explicit_controls)) &&
      length(explicit_controls) == nrow(fragments)
  ) {
    explicit_controls = explicit_controls[active_rows]
  }
  fragments = fragments[active_rows, , drop = FALSE]
  fragments$solve_component_id = as.integer(
    fragments$prospective_solve_component_id
  )
  active_fragment_id = fragments$render_road_fragment_id
  topology$fragments = fragments
  topology$endpoints = topology$endpoints[
    topology$endpoints$render_road_fragment_id %in% active_fragment_id,
    ,
    drop = FALSE
  ]
  participant_tables = c(
    "point_event_participants",
    "crossing_participants",
    "junction_participants",
    "topology_conflict_participants"
  )
  for (table_name in participant_tables) {
    table = topology[[table_name]]
    if (!is.null(table)) {
      topology[[table_name]] = table[
        table$render_road_fragment_id %in% active_fragment_id,
        ,
        drop = FALSE
      ]
    }
  }
  pair_tables = c(
    "point_pairs",
    "crossing_pairs",
    "junction_equality_pairs",
    "topology_conflict_pairs"
  )
  for (table_name in pair_tables) {
    table = topology[[table_name]]
    if (!is.null(table)) {
      topology[[table_name]] = table[
        table$fragment_a %in%
          active_fragment_id &
          table$fragment_b %in% active_fragment_id,
        ,
        drop = FALSE
      ]
    }
  }
  topology$junction_equality_pairs =
    topology$prospective_solve_junction_equality_pairs[
      topology$prospective_solve_junction_equality_pairs$fragment_a %in%
        active_fragment_id &
        topology$prospective_solve_junction_equality_pairs$fragment_b %in%
          active_fragment_id,
      ,
      drop = FALSE
    ]
  continuation_tables = c(
    "selected_continuations",
    "prospective_solve_continuations",
    "ambiguous_continuations",
    "rejected_continuations",
    "continuation_candidates"
  )
  for (table_name in continuation_tables) {
    table = topology[[table_name]]
    if (!is.null(table)) {
      topology[[table_name]] = table[
        table$fragment_a %in%
          active_fragment_id &
          table$fragment_b %in% active_fragment_id,
        ,
        drop = FALSE
      ]
    }
  }
  overlap_tables = c("layer_overlaps", "equal_layer_overlaps")
  for (table_name in overlap_tables) {
    table = topology[[table_name]]
    if (!is.null(table)) {
      topology[[table_name]] = table[
        table$fragment_a %in%
          active_fragment_id &
          table$fragment_b %in% active_fragment_id,
        ,
        drop = FALSE
      ]
    }
  }
  event_specs = list(
    point_events = c("point_event_participants", "point_event_id"),
    crossings = c("crossing_participants", "crossing_id"),
    junctions = c("junction_participants", "junction_id"),
    topology_conflicts = c("topology_conflict_participants", "conflict_id")
  )
  for (event_name in names(event_specs)) {
    event_table = topology[[event_name]]
    participant_table = topology[[event_specs[[event_name]][[1L]]]]
    id_column = event_specs[[event_name]][[2L]]
    if (!is.null(event_table) && !is.null(participant_table)) {
      topology[[event_name]] = event_table[
        event_table[[id_column]] %in% participant_table[[id_column]],
        ,
        drop = FALSE
      ]
    }
  }
  topology$candidate_anchor_endpoint_id = intersect(
    topology$candidate_anchor_endpoint_id,
    topology$endpoints$render_road_endpoint_id
  )
  list(
    topology = topology,
    terrain_profiles = terrain_profiles,
    explicit_controls = explicit_controls
  )
}

#' Normalize explicit road-profile controls
#'
#' @param explicit_controls Default `NULL`. Caller-supplied control distances.
#' @param fragment_id Active fragment identifiers.
#'
#' @return Named numeric vectors, one per fragment.
#' @keywords internal
normalize_render_road_explicit_controls = function(
  explicit_controls = NULL,
  fragment_id
) {
  if (is.null(explicit_controls)) {
    explicit_controls = rep(list(numeric(0)), length(fragment_id))
  } else if (!is.list(explicit_controls)) {
    stop("`explicit_controls` must be a list.", call. = FALSE)
  } else if (!is.null(names(explicit_controls))) {
    explicit_index = match(as.character(fragment_id), names(explicit_controls))
    if (anyNA(explicit_index)) {
      stop(
        "Named `explicit_controls` must include every fragment ID.",
        call. = FALSE
      )
    }
    explicit_controls = explicit_controls[explicit_index]
  } else if (length(explicit_controls) != length(fragment_id)) {
    stop(
      "`explicit_controls` must contain one entry per fragment.",
      call. = FALSE
    )
  }
  explicit_controls = lapply(explicit_controls, function(value) {
    value = suppressWarnings(as.numeric(value))
    value[is.finite(value)]
  })
  names(explicit_controls) = as.character(fragment_id)
  explicit_controls
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
  fragment_row = setNames(
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
  degree = setNames(integer(length(members)), as.character(members))
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
  fragment_regime = setNames(
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
      span_offset = c(0, head(cumsum(increments), -1L))
      if (!run_closed) {
        span_length = tail(span_offset + run_members$fragment_length, 1L)
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

#' Map fragment distances to an oriented profile-span station
#'
#' @param span_members Oriented span-member table.
#' @param fragment Fragment identifier.
#' @param distance Distance along the source fragment.
#'
#' @return Span stations in metres.
#' @keywords internal
map_render_road_profile_span_station = function(
  span_members,
  fragment,
  distance
) {
  member = span_members[
    span_members$render_road_fragment_id == fragment,
    ,
    drop = FALSE
  ]
  if (nrow(member) != 1L) {
    stop(
      sprintf(
        "Fragment %s does not have one profile-span membership.",
        fragment
      ),
      call. = FALSE
    )
  }
  distance = as.numeric(distance)
  member$span_offset[[1L]] +
    if (member$orientation[[1L]] == 1L) {
      distance
    } else {
      member$fragment_length[[1L]] - distance
    }
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
#' @param controls Road-profile controls.
#' @param fragment Fragment identifier.
#' @param distance Requested distance in metres.
#' @param tolerance Maximum permitted separation in metres.
#' @param context Diagnostic context included in failures.
#'
#' @return Matching control row identifier.
#' @keywords internal
match_render_road_profile_control_id = function(
  controls,
  fragment,
  distance,
  tolerance,
  context
) {
  rows = which(controls$render_road_fragment_id == fragment)
  if (!length(rows)) {
    stop(
      sprintf(
        "No profile controls exist for fragment %s while resolving %s.",
        fragment,
        context
      ),
      call. = FALSE
    )
  }
  if ("control_tolerance" %in% names(controls)) {
    fragment_tolerance = unique(controls$control_tolerance[rows])
    fragment_tolerance = fragment_tolerance[
      is.finite(fragment_tolerance) & fragment_tolerance >= 0
    ]
    if (length(fragment_tolerance)) {
      tolerance = max(tolerance, fragment_tolerance)
    }
  }
  separation = abs(controls$distance[rows] - distance)
  match_index = which.min(separation)
  if (
    !length(match_index) ||
      !is.finite(separation[[match_index]]) ||
      separation[[match_index]] > tolerance
  ) {
    stop(
      sprintf(
        paste0(
          "No profile control matched fragment %s at %.12g m for %s ",
          "within %.3g m; nearest separation was %.3g m."
        ),
        fragment,
        distance,
        context,
        tolerance,
        min(separation, na.rm = TRUE)
      ),
      call. = FALSE
    )
  }
  rows[[match_index]]
}

#' Build sparse road-profile controls
#'
#' @param topology Active road topology.
#' @param terrain_profiles Normalized terrain profiles.
#' @param explicit_controls Named caller-supplied control distances.
#' @param adaptive_constraints Adaptive constraint requests.
#' @param profile_spans Span and member tables.
#' @param fragment_length Named fragment lengths.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Control table with span stations and integration weights.
#' @keywords internal
build_render_road_profile_controls = function(
  topology,
  terrain_profiles,
  explicit_controls,
  adaptive_constraints,
  profile_spans,
  fragment_length,
  control_tolerance
) {
  fragments = topology$fragments
  fragment_id = fragments$render_road_fragment_id
  fragment_row = setNames(seq_len(nrow(fragments)), fragment_id)
  control_distance = lapply(fragment_id, function(fragment) {
    c(
      0,
      fragment_length[[as.character(fragment)]],
      explicit_controls[[as.character(fragment)]]
    )
  })
  names(control_distance) = as.character(fragment_id)
  participants = topology$point_event_participants
  if (!is.null(participants) && nrow(participants)) {
    for (participant in seq_len(nrow(participants))) {
      fragment = as.character(
        participants$render_road_fragment_id[[participant]]
      )
      control_distance[[fragment]] = c(
        control_distance[[fragment]],
        participants$distance[[participant]]
      )
    }
  }
  if (nrow(topology$layer_overlaps)) {
    for (overlap in seq_len(nrow(topology$layer_overlaps))) {
      overlap_row = topology$layer_overlaps[overlap, , drop = FALSE]
      for (suffix in c("a", "b")) {
        fragment = as.character(
          overlap_row[[paste0("fragment_", suffix)]][[1L]]
        )
        control_distance[[fragment]] = c(
          control_distance[[fragment]],
          resolve_render_road_overlap_endpoint_distance(
            overlap_row,
            suffix,
            "start"
          ),
          resolve_render_road_overlap_endpoint_distance(
            overlap_row,
            suffix,
            "end"
          )
        )
      }
    }
  }
  if (nrow(adaptive_constraints)) {
    for (adaptive in seq_len(nrow(adaptive_constraints))) {
      fragment_a = as.character(adaptive_constraints$fragment_a[[adaptive]])
      control_distance[[fragment_a]] = c(
        control_distance[[fragment_a]],
        adaptive_constraints$distance_a[[adaptive]]
      )
      fragment_b = adaptive_constraints$fragment_b[[adaptive]]
      if (is.finite(fragment_b) && fragment_b %in% fragment_id) {
        fragment_b = as.character(fragment_b)
        control_distance[[fragment_b]] = c(
          control_distance[[fragment_b]],
          adaptive_constraints$distance_b[[adaptive]]
        )
      }
    }
  }
  control_rows = lapply(seq_along(fragment_id), function(row) {
    fragment = fragment_id[[row]]
    length_m = fragment_length[[as.character(fragment)]]
    distance = sort(pmin(
      pmax(
        control_distance[[as.character(fragment)]][
          is.finite(control_distance[[as.character(fragment)]])
        ],
        0
      ),
      length_m
    ))
    tolerance = max(control_tolerance, length_m * 1e-10)
    distance = distance[c(TRUE, diff(distance) > tolerance)]
    span_member = profile_spans$members[
      profile_spans$members$render_road_fragment_id == fragment,
      ,
      drop = FALSE
    ]
    span_station = map_render_road_profile_span_station(
      profile_spans$members,
      fragment,
      distance
    )
    data.frame(
      render_road_fragment_id = fragment,
      render_road_feature_id = fragments$render_road_feature_id[[row]],
      solve_component_id = fragments$solve_component_id[[row]],
      distance = distance,
      control_tolerance = tolerance,
      terrain = interpolate_render_road_profile_reference(
        terrain_profiles[[as.character(fragment)]],
        distance
      ),
      render_road_layer = fragments$render_road_layer[[row]],
      span_id = span_member$span_id[[1L]],
      span_station = span_station,
      endpoint_control = distance <= tolerance |
        distance >= length_m - tolerance,
      explicit_control = vapply(
        distance,
        function(value) {
          any(
            abs(explicit_controls[[as.character(fragment)]] - value) <=
              tolerance,
            na.rm = TRUE
          )
        },
        logical(1)
      ),
      stringsAsFactors = FALSE
    )
  })
  controls = do.call(rbind, control_rows)
  rownames(controls) = NULL
  controls$control_id = seq_len(nrow(controls))
  controls = controls[, c("control_id", setdiff(names(controls), "control_id"))]
  controls$crossing_control = FALSE
  controls$junction_control = FALSE
  controls$conflict_control = FALSE
  controls$overlap_control = FALSE
  controls$adaptive_control = FALSE
  point_pairs = topology$point_pairs
  if (!is.null(point_pairs) && nrow(point_pairs)) {
    for (pair in seq_len(nrow(point_pairs))) {
      relation_column = if (point_pairs$topology_layer_conflict[[pair]]) {
        "conflict_control"
      } else if (
        point_pairs$layer_relationship[[pair]] &&
          point_pairs$topology_relation[[pair]] == "interior_crossing"
      ) {
        "crossing_control"
      } else {
        "junction_control"
      }
      for (suffix in c("a", "b")) {
        control = match_render_road_profile_control_id(
          controls,
          point_pairs[[paste0("fragment_", suffix)]][[pair]],
          point_pairs[[paste0("distance_", suffix)]][[pair]],
          control_tolerance,
          sprintf("point pair %s", point_pairs$point_pair_id[[pair]])
        )
        controls[[relation_column]][[control]] = TRUE
      }
    }
  }
  if (nrow(topology$layer_overlaps)) {
    for (overlap in seq_len(nrow(topology$layer_overlaps))) {
      overlap_row = topology$layer_overlaps[overlap, , drop = FALSE]
      for (suffix in c("a", "b")) {
        for (endpoint in c("start", "end")) {
          control = match_render_road_profile_control_id(
            controls,
            overlap_row[[paste0("fragment_", suffix)]][[1L]],
            resolve_render_road_overlap_endpoint_distance(
              overlap_row,
              suffix,
              endpoint
            ),
            control_tolerance,
            sprintf(
              "overlap %s %s",
              overlap_row$overlap_id[[1L]],
              endpoint
            )
          )
          controls$overlap_control[[control]] = TRUE
        }
      }
    }
  }
  if (nrow(adaptive_constraints)) {
    for (adaptive in seq_len(nrow(adaptive_constraints))) {
      control_a = match_render_road_profile_control_id(
        controls,
        adaptive_constraints$fragment_a[[adaptive]],
        adaptive_constraints$distance_a[[adaptive]],
        control_tolerance,
        sprintf("adaptive %s", adaptive_constraints$type[[adaptive]])
      )
      controls$adaptive_control[[control_a]] = TRUE
      if (is.finite(adaptive_constraints$fragment_b[[adaptive]])) {
        control_b = match_render_road_profile_control_id(
          controls,
          adaptive_constraints$fragment_b[[adaptive]],
          adaptive_constraints$distance_b[[adaptive]],
          control_tolerance,
          sprintf("adaptive %s", adaptive_constraints$type[[adaptive]])
        )
        controls$adaptive_control[[control_b]] = TRUE
      }
    }
  }
  controls$station_weight = 0
  for (fragment in fragment_id) {
    rows = which(controls$render_road_fragment_id == fragment)
    rows = rows[order(controls$distance[rows])]
    if (length(rows) > 1L) {
      interval_length = diff(controls$distance[rows])
      controls$station_weight[rows[-length(rows)]] =
        controls$station_weight[rows[-length(rows)]] + interval_length / 2
      controls$station_weight[rows[-1L]] =
        controls$station_weight[rows[-1L]] + interval_length / 2
    }
  }
  if (nrow(topology$prospective_solve_continuations)) {
    for (continuation in seq_len(nrow(
      topology$prospective_solve_continuations
    ))) {
      record = topology$prospective_solve_continuations[
        continuation,
        ,
        drop = FALSE
      ]
      gap = record$endpoint_distance[[1L]]
      if (!is.finite(gap) || gap <= 0) {
        next
      }
      distance_a = if (record$side_a[[1L]] == "start") {
        0
      } else {
        fragment_length[[as.character(record$fragment_a[[1L]])]]
      }
      distance_b = if (record$side_b[[1L]] == "start") {
        0
      } else {
        fragment_length[[as.character(record$fragment_b[[1L]])]]
      }
      control_a = match_render_road_profile_control_id(
        controls,
        record$fragment_a[[1L]],
        distance_a,
        control_tolerance,
        sprintf("continuation %s", record$continuation_id[[1L]])
      )
      control_b = match_render_road_profile_control_id(
        controls,
        record$fragment_b[[1L]],
        distance_b,
        control_tolerance,
        sprintf("continuation %s", record$continuation_id[[1L]])
      )
      controls$station_weight[c(control_a, control_b)] =
        controls$station_weight[c(control_a, control_b)] + gap / 2
    }
  }
  if (any(!is.finite(controls$station_weight) | controls$station_weight <= 0)) {
    stop("Every road-profile control requires a positive station weight.")
  }
  control_count = nrow(controls)
  controls$height_variable = seq_len(control_count)
  controls$grade_variable = control_count + seq_len(control_count)
  controls
}

#' Attach outer control identifiers to road-profile spans
#'
#' @param profile_spans Span and member tables.
#' @param controls Road-profile controls.
#' @param fragment_length Named fragment lengths.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Updated profile span object.
#' @keywords internal
attach_render_road_profile_span_controls = function(
  profile_spans,
  controls,
  fragment_length,
  control_tolerance
) {
  spans = profile_spans$spans
  spans$start_control_id = integer(nrow(spans))
  spans$end_control_id = integer(nrow(spans))
  spans$periodic_support_control_id = rep(NA_integer_, nrow(spans))
  support_arc_rows = list()
  for (span in seq_len(nrow(spans))) {
    if (isTRUE(spans$closed[[span]])) {
      support = which(
        controls$span_id == spans$span_id[[span]] &
          (controls$crossing_control | controls$overlap_control)
      )
      if (!length(support)) {
        support = which(
          controls$span_id == spans$span_id[[span]] &
            controls$endpoint_control
        )
      }
      if (!length(support)) {
        stop(
          "A closed profile span has no periodic support control.",
          call. = FALSE
        )
      }
      support = support[
        order(
          controls$span_station[support],
          controls$control_id[support]
        )
      ]
      support_station = controls$span_station[support]
      keep = !duplicated(round(support_station / control_tolerance))
      support = support[keep]
      support_station = support_station[keep]
      spans$start_control_id[[span]] = support[[1L]]
      spans$end_control_id[[span]] = support[[1L]]
      if (length(support) == 1L) {
        spans$periodic_support_control_id[[span]] = support[[1L]]
        support_arc_rows[[length(support_arc_rows) + 1L]] = data.frame(
          span_id = spans$span_id[[span]],
          start_control_id = support[[1L]],
          end_control_id = support[[1L]],
          start_station = support_station[[1L]],
          end_station = support_station[[1L]] + spans$span_length[[span]],
          arc_length = spans$span_length[[span]],
          span_length = spans$span_length[[span]],
          closed = TRUE,
          stringsAsFactors = FALSE
        )
      } else {
        next_support = c(support[-1L], support[[1L]])
        next_station = c(
          support_station[-1L],
          support_station[[1L]] + spans$span_length[[span]]
        )
        for (support_index in seq_along(support)) {
          support_arc_rows[[length(support_arc_rows) + 1L]] = data.frame(
            span_id = spans$span_id[[span]],
            start_control_id = support[[support_index]],
            end_control_id = next_support[[support_index]],
            start_station = support_station[[support_index]],
            end_station = next_station[[support_index]],
            arc_length = next_station[[support_index]] -
              support_station[[support_index]],
            span_length = spans$span_length[[span]],
            closed = TRUE,
            stringsAsFactors = FALSE
          )
        }
      }
      next
    }
    start_distance = if (spans$start_side[[span]] == "start") {
      0
    } else {
      fragment_length[[as.character(spans$start_fragment_id[[span]])]]
    }
    end_distance = if (spans$end_side[[span]] == "start") {
      0
    } else {
      fragment_length[[as.character(spans$end_fragment_id[[span]])]]
    }
    spans$start_control_id[[span]] = match_render_road_profile_control_id(
      controls,
      spans$start_fragment_id[[span]],
      start_distance,
      control_tolerance,
      sprintf("span %s start", spans$span_id[[span]])
    )
    spans$end_control_id[[span]] = match_render_road_profile_control_id(
      controls,
      spans$end_fragment_id[[span]],
      end_distance,
      control_tolerance,
      sprintf("span %s end", spans$span_id[[span]])
    )
    support_arc_rows[[length(support_arc_rows) + 1L]] = data.frame(
      span_id = spans$span_id[[span]],
      start_control_id = spans$start_control_id[[span]],
      end_control_id = spans$end_control_id[[span]],
      start_station = 0,
      end_station = spans$span_length[[span]],
      arc_length = spans$span_length[[span]],
      span_length = spans$span_length[[span]],
      closed = FALSE,
      stringsAsFactors = FALSE
    )
  }
  profile_spans$spans = spans
  profile_spans$support_arcs = do.call(rbind, support_arc_rows)
  profile_spans$support_arcs$support_arc_id = seq_len(
    nrow(profile_spans$support_arcs)
  )
  profile_spans
}

#' Resolve support-chord arcs at profile-span stations
#'
#' @param support_arcs Ordered support arcs.
#' @param span_id Profile span identifier.
#' @param station Span stations to resolve.
#'
#' @return One support-arc row and interpolation fraction per station.
#' @keywords internal
resolve_render_road_profile_support_arcs = function(
  support_arcs,
  span_id,
  station
) {
  arcs = support_arcs[support_arcs$span_id == span_id, , drop = FALSE]
  if (!nrow(arcs)) {
    stop(
      sprintf("Profile span %s has no support chord.", span_id),
      call. = FALSE
    )
  }
  resolved = vector("list", length(station))
  tolerance = sqrt(.Machine$double.eps)
  for (station_index in seq_along(station)) {
    value = station[[station_index]]
    candidate_value = if (isTRUE(arcs$closed[[1L]])) {
      normalized = value %% arcs$span_length[[1L]]
      c(normalized, normalized + arcs$span_length[[1L]])
    } else {
      value
    }
    arc_index = integer(0)
    adjusted_station = NA_real_
    for (candidate in candidate_value) {
      matches = which(
        candidate >= arcs$start_station - tolerance &
          candidate < arcs$end_station - tolerance
      )
      if (length(matches)) {
        arc_index = matches[[1L]]
        adjusted_station = candidate
        break
      }
    }
    if (!length(arc_index)) {
      terminal_match = which(
        abs(candidate_value[[length(candidate_value)]] - arcs$end_station) <=
          tolerance
      )
      if (length(terminal_match)) {
        arc_index = tail(terminal_match, 1L)
        adjusted_station = arcs$end_station[[arc_index]]
      } else {
        stop(
          sprintf("Station %.6f is outside profile span %s.", value, span_id),
          call. = FALSE
        )
      }
    }
    row = arcs[arc_index, , drop = FALSE]
    row$station = value
    row$adjusted_station = adjusted_station
    row$fraction = (adjusted_station - row$start_station) / row$arc_length
    resolved[[station_index]] = row
  }
  do.call(rbind, resolved)
}

#' Construct one sparse road-profile constraint record
#'
#' @param index Variable indices.
#' @param value Constraint coefficients.
#' @param lower Lower bound.
#' @param upper Upper bound.
#' @param type Constraint family.
#' @param component_id Solve component identifier.
#' @param fragment_a Default `NA_integer_`. First fragment identifier.
#' @param fragment_b Default `NA_integer_`. Second fragment identifier.
#' @param event_id Default `NA_integer_`. Source event identifier.
#' @param clearance Default `NA_real_`. Required clearance.
#' @param distance_a Default `NA_real_`. First source distance.
#' @param distance_b Default `NA_real_`. Second source distance.
#'
#' @return One constraint record.
#' @keywords internal
new_render_road_profile_constraint = function(
  index,
  value,
  lower,
  upper,
  type,
  component_id,
  fragment_a = NA_integer_,
  fragment_b = NA_integer_,
  event_id = NA_integer_,
  clearance = NA_real_,
  distance_a = NA_real_,
  distance_b = NA_real_
) {
  list(
    index = as.integer(index),
    value = as.numeric(value),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    type = as.character(type),
    component_id = as.integer(component_id),
    fragment_a = as.integer(fragment_a),
    fragment_b = as.integer(fragment_b),
    event_id = as.integer(event_id),
    clearance = as.numeric(clearance),
    distance_a = as.numeric(distance_a),
    distance_b = as.numeric(distance_b)
  )
}

#' Build interval, grade, and terrain constraints
#'
#' @param topology Active road topology.
#' @param controls Road-profile controls.
#' @param maximum_grade Maximum absolute grade.
#' @param maximum_grade_rate Maximum grade change per metre.
#'
#' @return Constraints, intervals, and curvature terms.
#' @keywords internal
build_render_road_interval_constraints = function(
  topology,
  controls,
  maximum_grade,
  maximum_grade_rate
) {
  constraints = list()
  interval_rows = list()
  curvature_rows = list()
  interval_index = 0L
  fragment_id = topology$fragments$render_road_fragment_id
  for (fragment in fragment_id) {
    rows = which(controls$render_road_fragment_id == fragment)
    rows = rows[order(controls$distance[rows])]
    for (interval in seq_len(length(rows) - 1L)) {
      first = rows[[interval]]
      second = rows[[interval + 1L]]
      length_m = controls$distance[[second]] - controls$distance[[first]]
      if (!is.finite(length_m) || length_m <= 0) {
        stop(
          sprintf(
            "Fragment %s contains a non-positive control interval.",
            fragment
          ),
          call. = FALSE
        )
      }
      component_id = controls$solve_component_id[[first]]
      interval_index = interval_index + 1L
      interval_rows[[interval_index]] = data.frame(
        interval_id = interval_index,
        solve_component_id = component_id,
        render_road_fragment_id = fragment,
        control_a = first,
        control_b = second,
        length = length_m,
        stringsAsFactors = FALSE
      )
      constraints[[length(constraints) + 1L]] =
        new_render_road_profile_constraint(
          index = c(
            controls$height_variable[[first]],
            controls$height_variable[[second]],
            controls$grade_variable[[first]],
            controls$grade_variable[[second]]
          ),
          value = c(-1, 1, -length_m / 2, -length_m / 2),
          lower = 0,
          upper = 0,
          type = "quadratic_interval",
          component_id = component_id,
          fragment_a = fragment,
          distance_a = controls$distance[[first]],
          distance_b = controls$distance[[second]]
        )
      constraints[[length(constraints) + 1L]] =
        new_render_road_profile_constraint(
          index = c(
            controls$grade_variable[[first]],
            controls$grade_variable[[second]]
          ),
          value = c(-1, 1),
          lower = -maximum_grade_rate * length_m,
          upper = maximum_grade_rate * length_m,
          type = "grade_rate",
          component_id = component_id,
          fragment_a = fragment,
          distance_a = controls$distance[[first]],
          distance_b = controls$distance[[second]]
        )
      curvature_rows[[length(curvature_rows) + 1L]] = data.frame(
        grade_a = controls$grade_variable[[first]],
        grade_b = controls$grade_variable[[second]],
        sign_a = 1,
        sign_b = 1,
        length = length_m,
        stringsAsFactors = FALSE
      )
    }
  }
  underground = identify_render_road_underground_fragments(topology$fragments)
  underground_fragment = topology$fragments$render_road_fragment_id[
    underground
  ]
  for (control in seq_len(nrow(controls))) {
    fragment = controls$render_road_fragment_id[[control]]
    component_id = controls$solve_component_id[[control]]
    constraints[[length(constraints) + 1L]] =
      new_render_road_profile_constraint(
        index = controls$grade_variable[[control]],
        value = 1,
        lower = -maximum_grade,
        upper = maximum_grade,
        type = "grade_bound",
        component_id = component_id,
        fragment_a = fragment,
        distance_a = controls$distance[[control]]
      )
    if (!(fragment %in% underground_fragment)) {
      constraints[[length(constraints) + 1L]] =
        new_render_road_profile_constraint(
          index = controls$height_variable[[control]],
          value = 1,
          lower = controls$terrain[[control]],
          upper = Inf,
          type = "terrain_floor",
          component_id = component_id,
          fragment_a = fragment,
          distance_a = controls$distance[[control]]
        )
    }
  }
  list(
    constraints = constraints,
    intervals = do.call(rbind, interval_rows),
    curvature_terms = do.call(rbind, curvature_rows)
  )
}

#' Classify ground anchors and free solve frontiers
#'
#' @param topology Active road topology.
#' @param fragment_length Named fragment lengths.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Endpoint identifier sets and endpoint classifications.
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
  ambiguous_endpoint_id = unique(c(
    topology$ambiguous_continuations$endpoint_a,
    topology$ambiguous_continuations$endpoint_b
  ))
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
  candidate_endpoint_id = intersect(
    active_endpoint_id,
    topology$candidate_anchor_endpoint_id
  )
  ground_anchor_endpoint_id = setdiff(
    candidate_endpoint_id,
    unique(c(
      boundary_endpoint_id,
      ambiguous_endpoint_id,
      conflict_endpoint_id,
      selected_endpoint_id
    ))
  )
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
  endpoints$endpoint_role[
    endpoints$render_road_endpoint_id %in% ground_anchor_endpoint_id
  ] = "ground_anchor"
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

#' Calculate a smoothed terrain-reference grade
#'
#' @param profile Data frame containing `distance` and `elevation`.
#' @param distance Distance to evaluate in metres.
#' @param window Physical regression window in metres.
#'
#' @return Local terrain grade as rise divided by run.
#' @keywords internal
calculate_render_road_smoothed_reference_grade = function(
  profile,
  distance,
  window
) {
  profile_length = max(profile$distance)
  lower = max(0, distance - window / 2)
  upper = min(profile_length, distance + window / 2)
  if (distance <= window / 2) {
    upper = min(profile_length, max(window, distance + window / 2))
  }
  if (distance >= profile_length - window / 2) {
    lower = max(0, min(profile_length - window, distance - window / 2))
  }
  sample_distance = sort(unique(c(
    lower,
    upper,
    distance,
    profile$distance[
      profile$distance >= lower & profile$distance <= upper
    ]
  )))
  if (length(sample_distance) < 2L || upper <= lower) {
    return(calculate_render_road_profile_reference_grade(profile, distance))
  }
  sample_elevation = interpolate_render_road_profile_reference(
    profile,
    sample_distance
  )
  fit = stats::lm(sample_elevation ~ sample_distance)
  grade = unname(stats::coef(fit)[[2L]])
  if (!is.finite(grade)) 0 else grade
}

#' Build confirmed ground-anchor constraints
#'
#' @param topology Active road topology.
#' @param terrain_profiles Normalized terrain profiles.
#' @param controls Road-profile controls.
#' @param anchor_sets Endpoint anchor classifications.
#' @param fragment_length Named fragment lengths.
#' @param control_tolerance Control matching tolerance in metres.
#' @param anchor_grade_window Terrain-grade smoothing window in metres.
#'
#' @return Anchor constraints and diagnostic rows.
#' @keywords internal
build_render_road_anchor_constraints = function(
  topology,
  terrain_profiles,
  controls,
  anchor_sets,
  fragment_length,
  control_tolerance,
  anchor_grade_window
) {
  constraints = list()
  anchor_rows = list()
  endpoints = sf::st_drop_geometry(topology$endpoints)
  anchor_endpoint = endpoints[
    endpoints$render_road_endpoint_id %in%
      anchor_sets$ground_anchor_endpoint_id,
    ,
    drop = FALSE
  ]
  if (nrow(anchor_endpoint)) {
    for (anchor in seq_len(nrow(anchor_endpoint))) {
      fragment = anchor_endpoint$render_road_fragment_id[[anchor]]
      side = anchor_endpoint$endpoint_side[[anchor]]
      distance = if (side == "start") {
        0
      } else {
        fragment_length[[as.character(fragment)]]
      }
      control = match_render_road_profile_control_id(
        controls,
        fragment,
        distance,
        control_tolerance,
        sprintf(
          "ground anchor endpoint %s",
          anchor_endpoint$render_road_endpoint_id[[anchor]]
        )
      )
      component_id = controls$solve_component_id[[control]]
      constraints[[length(constraints) + 1L]] =
        new_render_road_profile_constraint(
          index = controls$height_variable[[control]],
          value = 1,
          lower = controls$terrain[[control]],
          # Treat the sampled terrain as a one-sided contact. The uplift
          # objective keeps feasible anchors on terrain, while allowing the
          # endpoint to rise when fixing noisy DEM samples would contradict
          # the physical grade or grade-rate limits.
          upper = Inf,
          type = "ground_anchor",
          component_id = component_id,
          fragment_a = fragment,
          distance_a = distance
        )
      terrain_profile = terrain_profiles[[as.character(fragment)]]
      anchor_rows[[length(anchor_rows) + 1L]] = data.frame(
        render_road_endpoint_id = anchor_endpoint$render_road_endpoint_id[[
          anchor
        ]],
        render_road_fragment_id = fragment,
        endpoint_side = side,
        control_id = control,
        terrain = controls$terrain[[control]],
        terrain_grade = calculate_render_road_smoothed_reference_grade(
          terrain_profile,
          distance,
          anchor_grade_window
        ),
        solve_component_id = component_id,
        stringsAsFactors = FALSE
      )
    }
  }
  anchors = if (length(anchor_rows)) {
    do.call(rbind, anchor_rows)
  } else {
    data.frame(
      render_road_endpoint_id = integer(0),
      render_road_fragment_id = integer(0),
      endpoint_side = character(0),
      control_id = integer(0),
      terrain = numeric(0),
      terrain_grade = numeric(0),
      solve_component_id = integer(0),
      stringsAsFactors = FALSE
    )
  }
  list(constraints = constraints, anchors = anchors)
}

#' Build pair-specific crossing-clearance constraints
#'
#' @param topology Active road topology.
#' @param controls Road-profile controls.
#' @param layer_spacing Fallback adjacent-layer clearance in metres.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Crossing constraints and clearance diagnostics.
#' @keywords internal
build_render_road_crossing_constraints = function(
  topology,
  controls,
  layer_spacing,
  control_tolerance
) {
  fragments = topology$fragments
  fragment_row = setNames(
    seq_len(nrow(fragments)),
    fragments$render_road_fragment_id
  )
  constraints = list()
  clearance_rows = list()
  crossing_pairs = topology$crossing_pairs
  for (pair in seq_len(nrow(crossing_pairs))) {
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
    if (rank_a < rank_b) {
      lower_fragment = record$fragment_a[[1L]]
      lower_distance = record$distance_a[[1L]]
      upper_fragment = record$fragment_b[[1L]]
      upper_distance = record$distance_b[[1L]]
      lower_rank = rank_a
      upper_rank = rank_b
    } else {
      lower_fragment = record$fragment_b[[1L]]
      lower_distance = record$distance_b[[1L]]
      upper_fragment = record$fragment_a[[1L]]
      upper_distance = record$distance_a[[1L]]
      lower_rank = rank_b
      upper_rank = rank_a
    }
    lower_control = match_render_road_profile_control_id(
      controls,
      lower_fragment,
      lower_distance,
      control_tolerance,
      sprintf("crossing pair %s lower", record$point_pair_id[[1L]])
    )
    upper_control = match_render_road_profile_control_id(
      controls,
      upper_fragment,
      upper_distance,
      control_tolerance,
      sprintf("crossing pair %s upper", record$point_pair_id[[1L]])
    )
    clearance = fragments$render_road_clearance[
      fragment_row[[as.character(upper_fragment)]]
    ]
    if (!is.finite(clearance)) {
      clearance = layer_spacing
    }
    component_id = controls$solve_component_id[[upper_control]]
    constraints[[length(constraints) + 1L]] =
      new_render_road_profile_constraint(
        index = c(
          controls$height_variable[[lower_control]],
          controls$height_variable[[upper_control]]
        ),
        value = c(-1, 1),
        lower = clearance,
        upper = Inf,
        type = "crossing_clearance",
        component_id = component_id,
        fragment_a = lower_fragment,
        fragment_b = upper_fragment,
        event_id = record$crossing_id[[1L]],
        clearance = clearance,
        distance_a = lower_distance,
        distance_b = upper_distance
      )
    clearance_rows[[length(clearance_rows) + 1L]] = data.frame(
      type = "crossing",
      event_id = record$crossing_id[[1L]],
      pair_id = record$point_pair_id[[1L]],
      lower_fragment_id = lower_fragment,
      upper_fragment_id = upper_fragment,
      lower_control_id = lower_control,
      upper_control_id = upper_control,
      lower_distance = lower_distance,
      upper_distance = upper_distance,
      lower_rank = lower_rank,
      upper_rank = upper_rank,
      clearance = clearance,
      solve_component_id = component_id,
      stringsAsFactors = FALSE
    )
  }
  clearances = if (length(clearance_rows)) {
    do.call(rbind, clearance_rows)
  } else {
    data.frame(
      type = character(0),
      event_id = integer(0),
      pair_id = integer(0),
      lower_fragment_id = integer(0),
      upper_fragment_id = integer(0),
      lower_control_id = integer(0),
      upper_control_id = integer(0),
      lower_distance = numeric(0),
      upper_distance = numeric(0),
      lower_rank = integer(0),
      upper_rank = integer(0),
      clearance = numeric(0),
      solve_component_id = integer(0),
      stringsAsFactors = FALSE
    )
  }
  list(constraints = constraints, clearances = clearances)
}

#' Build pair-specific junction-height constraints
#'
#' @param topology Active road topology.
#' @param controls Road-profile controls.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Junction constraints and equality diagnostics.
#' @keywords internal
build_render_road_junction_constraints = function(
  topology,
  controls,
  control_tolerance
) {
  constraints = list()
  junction_rows = list()
  junction_pairs = topology$junction_equality_pairs
  for (pair in seq_len(nrow(junction_pairs))) {
    record = junction_pairs[pair, , drop = FALSE]
    control_a = match_render_road_profile_control_id(
      controls,
      record$fragment_a[[1L]],
      record$distance_a[[1L]],
      control_tolerance,
      sprintf("junction pair %s side a", record$point_pair_id[[1L]])
    )
    control_b = match_render_road_profile_control_id(
      controls,
      record$fragment_b[[1L]],
      record$distance_b[[1L]],
      control_tolerance,
      sprintf("junction pair %s side b", record$point_pair_id[[1L]])
    )
    component_id = controls$solve_component_id[[control_a]]
    constraints[[length(constraints) + 1L]] =
      new_render_road_profile_constraint(
        index = c(
          controls$height_variable[[control_a]],
          controls$height_variable[[control_b]]
        ),
        value = c(-1, 1),
        lower = 0,
        upper = 0,
        type = "junction_height",
        component_id = component_id,
        fragment_a = record$fragment_a[[1L]],
        fragment_b = record$fragment_b[[1L]],
        event_id = record$junction_id[[1L]],
        distance_a = record$distance_a[[1L]],
        distance_b = record$distance_b[[1L]]
      )
    junction_rows[[length(junction_rows) + 1L]] = data.frame(
      junction_id = record$junction_id[[1L]],
      pair_id = record$point_pair_id[[1L]],
      fragment_a = record$fragment_a[[1L]],
      fragment_b = record$fragment_b[[1L]],
      control_a = control_a,
      control_b = control_b,
      solve_component_id = component_id,
      stringsAsFactors = FALSE
    )
  }
  equalities = if (length(junction_rows)) {
    do.call(rbind, junction_rows)
  } else {
    data.frame(
      junction_id = integer(0),
      pair_id = integer(0),
      fragment_a = integer(0),
      fragment_b = integer(0),
      control_a = integer(0),
      control_b = integer(0),
      solve_component_id = integer(0),
      stringsAsFactors = FALSE
    )
  }
  list(constraints = constraints, equalities = equalities)
}

#' Build pair-specific overlap-clearance constraints
#'
#' @param topology Active road topology.
#' @param controls Road-profile controls.
#' @param adaptive_constraints Adaptive constraint requests.
#' @param layer_spacing Fallback adjacent-layer clearance in metres.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Overlap constraints and relation-specific diagnostics.
#' @keywords internal
build_render_road_overlap_constraints = function(
  topology,
  controls,
  adaptive_constraints,
  layer_spacing,
  control_tolerance
) {
  fragments = topology$fragments
  fragment_row = setNames(
    seq_len(nrow(fragments)),
    fragments$render_road_fragment_id
  )
  constraints = list()
  clearance_rows = list()
  overlap_relation_rows = list()
  if (nrow(topology$layer_overlaps)) {
    for (overlap in seq_len(nrow(topology$layer_overlaps))) {
      record = topology$layer_overlaps[overlap, , drop = FALSE]
      fragment_a = record$fragment_a[[1L]]
      fragment_b = record$fragment_b[[1L]]
      row_a = fragment_row[[as.character(fragment_a)]]
      row_b = fragment_row[[as.character(fragment_b)]]
      if (
        fragments$render_road_layer[[row_a]] <
          fragments$render_road_layer[[row_b]]
      ) {
        lower_fragment = fragment_a
        upper_fragment = fragment_b
        lower_suffix = "a"
        upper_suffix = "b"
        upper_row = row_b
      } else {
        lower_fragment = fragment_b
        upper_fragment = fragment_a
        lower_suffix = "b"
        upper_suffix = "a"
        upper_row = row_a
      }
      clearance = fragments$render_road_clearance[[upper_row]]
      if (!is.finite(clearance)) {
        clearance = layer_spacing
      }
      lower_start = resolve_render_road_overlap_endpoint_distance(
        record,
        lower_suffix,
        "start"
      )
      lower_end = resolve_render_road_overlap_endpoint_distance(
        record,
        lower_suffix,
        "end"
      )
      upper_start = resolve_render_road_overlap_endpoint_distance(
        record,
        upper_suffix,
        "start"
      )
      upper_end = resolve_render_road_overlap_endpoint_distance(
        record,
        upper_suffix,
        "end"
      )
      overlap_relation_rows[[length(overlap_relation_rows) + 1L]] = data.frame(
        overlap_id = record$overlap_id[[1L]],
        lower_fragment_id = lower_fragment,
        upper_fragment_id = upper_fragment,
        lower_distance_start = lower_start,
        lower_distance_end = lower_end,
        upper_distance_start = upper_start,
        upper_distance_end = upper_end,
        clearance = clearance,
        stringsAsFactors = FALSE
      )
      for (endpoint in c("start", "end")) {
        lower_distance = if (endpoint == "start") lower_start else lower_end
        upper_distance = if (endpoint == "start") upper_start else upper_end
        lower_control = match_render_road_profile_control_id(
          controls,
          lower_fragment,
          lower_distance,
          control_tolerance,
          sprintf("overlap %s lower %s", record$overlap_id[[1L]], endpoint)
        )
        upper_control = match_render_road_profile_control_id(
          controls,
          upper_fragment,
          upper_distance,
          control_tolerance,
          sprintf("overlap %s upper %s", record$overlap_id[[1L]], endpoint)
        )
        component_id = controls$solve_component_id[[upper_control]]
        constraints[[length(constraints) + 1L]] =
          new_render_road_profile_constraint(
            index = c(
              controls$height_variable[[lower_control]],
              controls$height_variable[[upper_control]]
            ),
            value = c(-1, 1),
            lower = clearance,
            upper = Inf,
            type = "overlap_clearance",
            component_id = component_id,
            fragment_a = lower_fragment,
            fragment_b = upper_fragment,
            event_id = record$overlap_id[[1L]],
            clearance = clearance,
            distance_a = lower_distance,
            distance_b = upper_distance
          )
        clearance_rows[[length(clearance_rows) + 1L]] = data.frame(
          type = paste0("overlap_", endpoint),
          event_id = record$overlap_id[[1L]],
          pair_id = NA_integer_,
          lower_fragment_id = lower_fragment,
          upper_fragment_id = upper_fragment,
          lower_control_id = lower_control,
          upper_control_id = upper_control,
          lower_distance = lower_distance,
          upper_distance = upper_distance,
          lower_rank = NA_integer_,
          upper_rank = NA_integer_,
          clearance = clearance,
          solve_component_id = component_id,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  adaptive_overlap = adaptive_constraints[
    adaptive_constraints$type == "overlap_clearance",
    ,
    drop = FALSE
  ]
  if (nrow(adaptive_overlap)) {
    for (adaptive in seq_len(nrow(adaptive_overlap))) {
      record = adaptive_overlap[adaptive, , drop = FALSE]
      lower_control = match_render_road_profile_control_id(
        controls,
        record$fragment_a[[1L]],
        record$distance_a[[1L]],
        control_tolerance,
        sprintf("adaptive overlap %s lower", record$event_id[[1L]])
      )
      upper_control = match_render_road_profile_control_id(
        controls,
        record$fragment_b[[1L]],
        record$distance_b[[1L]],
        control_tolerance,
        sprintf("adaptive overlap %s upper", record$event_id[[1L]])
      )
      component_id = controls$solve_component_id[[upper_control]]
      constraints[[length(constraints) + 1L]] =
        new_render_road_profile_constraint(
          index = c(
            controls$height_variable[[lower_control]],
            controls$height_variable[[upper_control]]
          ),
          value = c(-1, 1),
          lower = record$clearance[[1L]],
          upper = Inf,
          type = "overlap_clearance_adaptive",
          component_id = component_id,
          fragment_a = record$fragment_a[[1L]],
          fragment_b = record$fragment_b[[1L]],
          event_id = record$event_id[[1L]],
          clearance = record$clearance[[1L]],
          distance_a = record$distance_a[[1L]],
          distance_b = record$distance_b[[1L]]
        )
      clearance_rows[[length(clearance_rows) + 1L]] = data.frame(
        type = "overlap_adaptive",
        event_id = record$event_id[[1L]],
        pair_id = NA_integer_,
        lower_fragment_id = record$fragment_a[[1L]],
        upper_fragment_id = record$fragment_b[[1L]],
        lower_control_id = lower_control,
        upper_control_id = upper_control,
        lower_distance = record$distance_a[[1L]],
        upper_distance = record$distance_b[[1L]],
        lower_rank = NA_integer_,
        upper_rank = NA_integer_,
        clearance = record$clearance[[1L]],
        solve_component_id = component_id,
        stringsAsFactors = FALSE
      )
    }
  }
  clearances = if (length(clearance_rows)) {
    do.call(rbind, clearance_rows)
  } else {
    data.frame(
      type = character(0),
      event_id = integer(0),
      pair_id = integer(0),
      lower_fragment_id = integer(0),
      upper_fragment_id = integer(0),
      lower_control_id = integer(0),
      upper_control_id = integer(0),
      lower_distance = numeric(0),
      upper_distance = numeric(0),
      lower_rank = integer(0),
      upper_rank = integer(0),
      clearance = numeric(0),
      solve_component_id = integer(0),
      stringsAsFactors = FALSE
    )
  }
  overlap_relations = if (length(overlap_relation_rows)) {
    do.call(rbind, overlap_relation_rows)
  } else {
    data.frame(
      overlap_id = integer(0),
      lower_fragment_id = integer(0),
      upper_fragment_id = integer(0),
      lower_distance_start = numeric(0),
      lower_distance_end = numeric(0),
      upper_distance_start = numeric(0),
      upper_distance_end = numeric(0),
      clearance = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  list(
    constraints = constraints,
    clearances = clearances,
    overlap_relations = overlap_relations
  )
}

#' Build pair-specific road event constraints
#'
#' @param topology Active road topology.
#' @param controls Road-profile controls.
#' @param adaptive_constraints Adaptive constraint requests.
#' @param layer_spacing Fallback adjacent-layer clearance in metres.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Event constraints and relation-specific diagnostics.
#' @keywords internal
build_render_road_event_constraints = function(
  topology,
  controls,
  adaptive_constraints,
  layer_spacing,
  control_tolerance
) {
  crossing = build_render_road_crossing_constraints(
    topology,
    controls,
    layer_spacing,
    control_tolerance
  )
  junction = build_render_road_junction_constraints(
    topology,
    controls,
    control_tolerance
  )
  overlap = build_render_road_overlap_constraints(
    topology,
    controls,
    adaptive_constraints,
    layer_spacing,
    control_tolerance
  )
  list(
    constraints = c(
      crossing$constraints,
      junction$constraints,
      overlap$constraints
    ),
    clearances = rbind(crossing$clearances, overlap$clearances),
    junction_equalities = junction$equalities,
    overlap_relations = overlap$overlap_relations,
    topology_conflict_pairs = topology$topology_conflict_pairs
  )
}

#' Build selected-continuation profile constraints
#'
#' @param topology Active road topology.
#' @param controls Road-profile controls.
#' @param fragment_length Named fragment lengths.
#' @param maximum_grade_rate Maximum grade change per metre.
#' @param control_tolerance Control matching tolerance in metres.
#'
#' @return Constraints, continuation rows, and gap curvature terms.
#' @keywords internal
build_render_road_continuation_constraints = function(
  topology,
  controls,
  fragment_length,
  maximum_grade_rate,
  control_tolerance
) {
  constraints = list()
  continuation_rows = list()
  curvature_rows = list()
  continuations = topology$prospective_solve_continuations
  if (nrow(continuations)) {
    for (continuation in seq_len(nrow(continuations))) {
      record = continuations[continuation, , drop = FALSE]
      fragment_a = record$fragment_a[[1L]]
      fragment_b = record$fragment_b[[1L]]
      distance_a = if (record$side_a[[1L]] == "start") {
        0
      } else {
        fragment_length[[as.character(fragment_a)]]
      }
      distance_b = if (record$side_b[[1L]] == "start") {
        0
      } else {
        fragment_length[[as.character(fragment_b)]]
      }
      control_a = match_render_road_profile_control_id(
        controls,
        fragment_a,
        distance_a,
        control_tolerance,
        sprintf("continuation %s side a", record$continuation_id[[1L]])
      )
      control_b = match_render_road_profile_control_id(
        controls,
        fragment_b,
        distance_b,
        control_tolerance,
        sprintf("continuation %s side b", record$continuation_id[[1L]])
      )
      sign_a = if (record$side_a[[1L]] == "end") 1 else -1
      sign_b = if (record$side_b[[1L]] == "start") 1 else -1
      gap = record$endpoint_distance[[1L]]
      component_id = controls$solve_component_id[[control_a]]
      exact = isTRUE(record$exact_endpoint[[1L]])
      if (exact) {
        constraints[[length(constraints) + 1L]] =
          new_render_road_profile_constraint(
            index = c(
              controls$height_variable[[control_a]],
              controls$height_variable[[control_b]]
            ),
            value = c(-1, 1),
            lower = 0,
            upper = 0,
            type = "continuation_height",
            component_id = component_id,
            fragment_a = fragment_a,
            fragment_b = fragment_b,
            event_id = record$continuation_id[[1L]],
            distance_a = distance_a,
            distance_b = distance_b
          )
        constraints[[length(constraints) + 1L]] =
          new_render_road_profile_constraint(
            index = c(
              controls$grade_variable[[control_a]],
              controls$grade_variable[[control_b]]
            ),
            value = c(sign_a, -sign_b),
            lower = 0,
            upper = 0,
            type = "continuation_grade",
            component_id = component_id,
            fragment_a = fragment_a,
            fragment_b = fragment_b,
            event_id = record$continuation_id[[1L]],
            distance_a = distance_a,
            distance_b = distance_b
          )
      } else {
        constraints[[length(constraints) + 1L]] =
          new_render_road_profile_constraint(
            index = c(
              controls$height_variable[[control_a]],
              controls$height_variable[[control_b]],
              controls$grade_variable[[control_a]],
              controls$grade_variable[[control_b]]
            ),
            value = c(-1, 1, -gap * sign_a / 2, -gap * sign_b / 2),
            lower = 0,
            upper = 0,
            type = "continuation_gap_interval",
            component_id = component_id,
            fragment_a = fragment_a,
            fragment_b = fragment_b,
            event_id = record$continuation_id[[1L]],
            distance_a = distance_a,
            distance_b = distance_b
          )
        constraints[[length(constraints) + 1L]] =
          new_render_road_profile_constraint(
            index = c(
              controls$grade_variable[[control_a]],
              controls$grade_variable[[control_b]]
            ),
            value = c(-sign_a, sign_b),
            lower = -maximum_grade_rate * gap,
            upper = maximum_grade_rate * gap,
            type = "continuation_gap_grade_rate",
            component_id = component_id,
            fragment_a = fragment_a,
            fragment_b = fragment_b,
            event_id = record$continuation_id[[1L]],
            distance_a = distance_a,
            distance_b = distance_b
          )
        curvature_rows[[length(curvature_rows) + 1L]] = data.frame(
          grade_a = controls$grade_variable[[control_a]],
          grade_b = controls$grade_variable[[control_b]],
          sign_a = sign_a,
          sign_b = sign_b,
          length = gap,
          stringsAsFactors = FALSE
        )
      }
      continuation_rows[[length(continuation_rows) + 1L]] = data.frame(
        continuation_id = record$continuation_id[[1L]],
        fragment_a = fragment_a,
        fragment_b = fragment_b,
        control_a = control_a,
        control_b = control_b,
        sign_a = sign_a,
        sign_b = sign_b,
        gap = gap,
        exact_endpoint = exact,
        solve_component_id = component_id,
        stringsAsFactors = FALSE
      )
    }
  }
  continuation_equalities = if (length(continuation_rows)) {
    do.call(rbind, continuation_rows)
  } else {
    data.frame(
      continuation_id = integer(0),
      fragment_a = integer(0),
      fragment_b = integer(0),
      control_a = integer(0),
      control_b = integer(0),
      sign_a = numeric(0),
      sign_b = numeric(0),
      gap = numeric(0),
      exact_endpoint = logical(0),
      solve_component_id = integer(0),
      stringsAsFactors = FALSE
    )
  }
  curvature_terms = if (length(curvature_rows)) {
    do.call(rbind, curvature_rows)
  } else {
    data.frame(
      grade_a = integer(0),
      grade_b = integer(0),
      sign_a = numeric(0),
      sign_b = numeric(0),
      length = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  list(
    constraints = constraints,
    continuation_equalities = continuation_equalities,
    curvature_terms = curvature_terms
  )
}

#' Build span-wide no-dip chord constraints
#'
#' @param profile_spans Spans with attached outer controls.
#' @param controls Road-profile controls.
#'
#' @return Chord constraints and control diagnostics.
#' @keywords internal
build_render_road_chord_constraints = function(profile_spans, controls) {
  constraints = list()
  chord_rows = list()
  spans = profile_spans$spans
  active_span = spans[spans$no_dip, , drop = FALSE]
  if (nrow(active_span)) {
    for (span_row in seq_len(nrow(active_span))) {
      span = active_span[span_row, , drop = FALSE]
      rows = which(controls$span_id == span$span_id[[1L]])
      for (control in rows) {
        support_arc = resolve_render_road_profile_support_arcs(
          profile_spans$support_arcs,
          span$span_id[[1L]],
          controls$span_station[[control]]
        )
        start_control = support_arc$start_control_id[[1L]]
        end_control = support_arc$end_control_id[[1L]]
        if (control %in% c(start_control, end_control)) {
          next
        }
        fraction = support_arc$fraction[[1L]]
        constraints[[length(constraints) + 1L]] =
          new_render_road_profile_constraint(
            index = c(
              controls$height_variable[[start_control]],
              controls$height_variable[[control]],
              controls$height_variable[[end_control]]
            ),
            value = c(-(1 - fraction), 1, -fraction),
            lower = 0,
            upper = Inf,
            type = "no_dip_span_chord",
            component_id = controls$solve_component_id[[control]],
            fragment_a = controls$render_road_fragment_id[[control]],
            event_id = span$span_id[[1L]],
            distance_a = controls$distance[[control]]
          )
        chord_rows[[length(chord_rows) + 1L]] = data.frame(
          span_id = span$span_id[[1L]],
          support_arc_id = support_arc$support_arc_id[[1L]],
          control_id = control,
          start_control_id = start_control,
          end_control_id = end_control,
          fraction = fraction,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  chord_controls = if (length(chord_rows)) {
    do.call(rbind, chord_rows)
  } else {
    data.frame(
      span_id = integer(0),
      support_arc_id = integer(0),
      control_id = integer(0),
      start_control_id = integer(0),
      end_control_id = integer(0),
      fraction = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  list(constraints = constraints, chord_controls = chord_controls)
}

#' Build a length-weighted sparse road-profile objective
#'
#' @param controls Road-profile controls.
#' @param profile_spans Spans with attached outer controls.
#' @param anchors Ground-anchor diagnostics.
#' @param curvature_terms Interval and continuation curvature terms.
#' @param curvature_weight Curvature objective weight.
#' @param grade_weight Grade objective weight.
#' @param terrain_reference_weight Height-reference objective weight.
#' @param underground_reference_depth Terrain-relative reference depth for
#' explicit underground fragments.
#' @param underground_reference_weight Underground height-reference objective
#' weight.
#' @param anchor_grade_weight Anchor-grade objective weight.
#' @param uplift_weight Linear uplift objective weight.
#'
#' @return Sparse quadratic matrix and linear objective vector.
#' @keywords internal
build_render_road_profile_objective = function(
  controls,
  profile_spans,
  anchors,
  curvature_terms,
  curvature_weight,
  grade_weight,
  terrain_reference_weight,
  underground_reference_depth,
  underground_reference_weight,
  anchor_grade_weight,
  uplift_weight
) {
  variable_count = nrow(controls) * 2L
  objective_q = numeric(variable_count)
  p_i = integer(0)
  p_j = integer(0)
  p_x = numeric(0)
  spans = profile_spans$spans
  for (control in seq_len(nrow(controls))) {
    station_weight = controls$station_weight[[control]]
    height_variable = controls$height_variable[[control]]
    grade_variable = controls$grade_variable[[control]]
    span = spans[spans$span_id == controls$span_id[[control]], , drop = FALSE]
    underground_reference = identical(
      span$reference[[1L]],
      "underground_terrain"
    )
    if (!underground_reference) {
      objective_q[[height_variable]] =
        objective_q[[height_variable]] + uplift_weight * station_weight
    }
    if (grade_weight > 0) {
      p_i = c(p_i, grade_variable)
      p_j = c(p_j, grade_variable)
      p_x = c(p_x, 2 * grade_weight * station_weight)
    }
    if (underground_reference) {
      weight = underground_reference_weight * station_weight
      reference_height = controls$terrain[[control]] -
        underground_reference_depth
      p_i = c(p_i, height_variable)
      p_j = c(p_j, height_variable)
      p_x = c(p_x, 2 * weight)
      objective_q[[height_variable]] = objective_q[[height_variable]] -
        2 * weight * reference_height
    } else if (
      terrain_reference_weight > 0 &&
        span$reference[[1L]] %in% c("span_chord", "periodic_chord")
    ) {
      support_arc = resolve_render_road_profile_support_arcs(
        profile_spans$support_arcs,
        span$span_id[[1L]],
        controls$span_station[[control]]
      )
      fraction = support_arc$fraction[[1L]]
      coefficient = c(1, -(1 - fraction), -fraction)
      variable = c(
        height_variable,
        controls$height_variable[[support_arc$start_control_id[[1L]]]],
        controls$height_variable[[support_arc$end_control_id[[1L]]]]
      )
      coefficient_by_variable = tapply(coefficient, variable, sum)
      coefficient_by_variable = coefficient_by_variable[
        abs(coefficient_by_variable) > 1e-14
      ]
      if (length(coefficient_by_variable)) {
        variable = as.integer(names(coefficient_by_variable))
        coefficient = as.numeric(coefficient_by_variable)
        weight = terrain_reference_weight * station_weight
        for (first in seq_along(variable)) {
          for (second in seq_along(variable)) {
            p_i = c(p_i, variable[[first]])
            p_j = c(p_j, variable[[second]])
            p_x = c(
              p_x,
              2 * weight * coefficient[[first]] * coefficient[[second]]
            )
          }
        }
      }
    } else if (terrain_reference_weight > 0) {
      weight = terrain_reference_weight * station_weight
      p_i = c(p_i, height_variable)
      p_j = c(p_j, height_variable)
      p_x = c(p_x, 2 * weight)
      objective_q[[height_variable]] = objective_q[[height_variable]] -
        2 * weight * controls$terrain[[control]]
    }
  }
  if (nrow(anchors) && anchor_grade_weight > 0) {
    for (anchor in seq_len(nrow(anchors))) {
      variable = controls$grade_variable[anchors$control_id[[anchor]]]
      p_i = c(p_i, variable)
      p_j = c(p_j, variable)
      p_x = c(p_x, 2 * anchor_grade_weight)
      objective_q[[variable]] = objective_q[[variable]] -
        2 * anchor_grade_weight * anchors$terrain_grade[[anchor]]
    }
  }
  if (nrow(curvature_terms) && curvature_weight > 0) {
    for (term in seq_len(nrow(curvature_terms))) {
      variable = c(
        curvature_terms$grade_a[[term]],
        curvature_terms$grade_b[[term]]
      )
      coefficient = c(
        -curvature_terms$sign_a[[term]],
        curvature_terms$sign_b[[term]]
      )
      weight = curvature_weight / curvature_terms$length[[term]]
      for (first in seq_len(2L)) {
        for (second in seq_len(2L)) {
          p_i = c(p_i, variable[[first]])
          p_j = c(p_j, variable[[second]])
          p_x = c(
            p_x,
            2 * weight * coefficient[[first]] * coefficient[[second]]
          )
        }
      }
    }
  }
  matrix_p = Matrix::sparseMatrix(
    i = p_i,
    j = p_j,
    x = p_x,
    dims = c(variable_count, variable_count)
  )
  list(
    P = Matrix::forceSymmetric(matrix_p, uplo = "U"),
    q = objective_q
  )
}

#' Compile road-profile constraints into sparse matrices
#'
#' @param constraints Constraint records.
#' @param variable_count Number of optimization variables.
#'
#' @return Sparse constraint matrix, bounds, and diagnostic table.
#' @keywords internal
compile_render_road_profile_matrices = function(
  constraints,
  variable_count
) {
  constraint_count = length(constraints)
  if (!constraint_count) {
    stop("A road-profile problem requires at least one constraint.")
  }
  constraint_length = vapply(
    constraints,
    function(value) length(value$index),
    integer(1)
  )
  matrix_a = Matrix::sparseMatrix(
    i = rep(seq_len(constraint_count), constraint_length),
    j = unlist(lapply(constraints, `[[`, "index"), use.names = FALSE),
    x = unlist(lapply(constraints, `[[`, "value"), use.names = FALSE),
    dims = c(constraint_count, variable_count)
  )
  lower = vapply(constraints, `[[`, numeric(1), "lower")
  upper = vapply(constraints, `[[`, numeric(1), "upper")
  constraint_table = data.frame(
    constraint_id = seq_len(constraint_count),
    type = vapply(constraints, `[[`, character(1), "type"),
    solve_component_id = vapply(
      constraints,
      `[[`,
      integer(1),
      "component_id"
    ),
    fragment_a = vapply(constraints, `[[`, integer(1), "fragment_a"),
    fragment_b = vapply(constraints, `[[`, integer(1), "fragment_b"),
    event_id = vapply(constraints, `[[`, integer(1), "event_id"),
    clearance = vapply(constraints, `[[`, numeric(1), "clearance"),
    distance_a = vapply(constraints, `[[`, numeric(1), "distance_a"),
    distance_b = vapply(constraints, `[[`, numeric(1), "distance_b"),
    lower = lower,
    upper = upper,
    stringsAsFactors = FALSE
  )
  list(
    A = matrix_a,
    lower = lower,
    upper = upper,
    constraints = constraint_table
  )
}

#' Validate solve-component separation before optimization
#'
#' @param topology Active road topology.
#' @param controls Road-profile controls.
#' @param constraints Constraint records before matrix compilation.
#' @param matrix_a Sparse constraint matrix.
#' @param matrix_p Sparse quadratic objective matrix.
#'
#' @return `TRUE`, invisibly, or an error describing a cross-component coupling.
#' @keywords internal
validate_render_road_profile_component_blocks = function(
  topology,
  controls,
  constraints,
  matrix_a,
  matrix_p
) {
  fragment_component = setNames(
    topology$fragments$solve_component_id,
    topology$fragments$render_road_fragment_id
  )
  hard_pair = vapply(
    constraints,
    function(record) {
      is.finite(record$fragment_a) && is.finite(record$fragment_b)
    },
    logical(1)
  )
  if (any(hard_pair)) {
    pair_component = vapply(
      constraints[hard_pair],
      function(record) {
        component_a = fragment_component[[as.character(record$fragment_a)]]
        component_b = fragment_component[[as.character(record$fragment_b)]]
        length(component_a) == 1L &&
          length(component_b) == 1L &&
          is.finite(component_a) &&
          is.finite(component_b) &&
          component_a == component_b
      },
      logical(1)
    )
    if (!all(pair_component)) {
      bad = which(hard_pair)[which(!pair_component)[[1L]]]
      stop(
        sprintf(
          "Hard constraint `%s` couples fragments from different solve components.",
          constraints[[bad]]$type
        ),
        call. = FALSE
      )
    }
  }

  variable_component = c(
    controls$solve_component_id,
    controls$solve_component_id
  )
  summary_a = Matrix::summary(matrix_a)
  if (nrow(summary_a)) {
    component_by_row = split(variable_component[summary_a$j], summary_a$i)
    invalid_a = vapply(
      component_by_row,
      function(value) length(unique(value)) != 1L,
      logical(1)
    )
    if (any(invalid_a)) {
      stop(
        "The hard-constraint matrix is not solve-component block diagonal.",
        call. = FALSE
      )
    }
  }
  summary_p = Matrix::summary(matrix_p)
  off_block = nrow(summary_p) &&
    any(
      abs(summary_p$x) > 1e-14 &
        variable_component[summary_p$i] != variable_component[summary_p$j]
    )
  if (off_block) {
    stop(
      "The quadratic objective is not solve-component block diagonal.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Build a sparse quadratic road profile problem
#'
#' @param topology Road topology diagnostics.
#' @param terrain_profiles Default `NULL`. Terrain reference profiles accepted
#' by [normalize_render_road_terrain_profiles()].
#' @param explicit_controls Default `NULL`. Optional list of additional control
#' distances by fragment.
#' @param layer_spacing Default `5.5`. Fallback adjacent-layer clearance in
#' metres.
#' @param maximum_grade Default `0.07`. Maximum absolute longitudinal grade.
#' @param maximum_grade_rate Default `1e-3`. Maximum grade change per metre.
#' @param curvature_weight Default `100`. Objective weight on grade change.
#' @param grade_weight Default `1`. Objective weight on grade magnitude.
#' @param terrain_reference_weight Default `1e-3`. Objective weight toward the
#' sampled terrain reference.
#' @param underground_reference_depth Default `NULL`, which uses
#' `layer_spacing`. Terrain-relative reference depth in metres for explicit
#' underground fragments.
#' @param underground_reference_weight Default `1e-3`. Positive objective
#' weight that bounds explicit underground profiles around their reference
#' depth.
#' @param anchor_grade_weight Default `10`. Objective weight toward terrain
#' grade at fixed outer anchors.
#' @param uplift_weight Default `1e-5`. Linear objective weight discouraging
#' unnecessary elevation.
#' @param anchor_grade_window Default `10`. Physical terrain-regression window
#' in metres at confirmed ground anchors.
#' @param control_tolerance Default `1e-7`. Maximum distance in metres when
#' resolving an event or endpoint to a compiled control.
#' @param adaptive_constraints Default `NULL`. Continuous-audit constraint
#' requests used when rebuilding a refined problem.
#'
#' @return Sparse matrices, controls, constraints, and solve diagnostics.
#' @keywords internal
build_render_road_profile_problem = function(
  topology,
  terrain_profiles = NULL,
  explicit_controls = NULL,
  layer_spacing = 5.5,
  maximum_grade = 0.07,
  maximum_grade_rate = 1e-3,
  curvature_weight = 100,
  grade_weight = 1,
  terrain_reference_weight = 1e-3,
  underground_reference_depth = NULL,
  underground_reference_weight = 1e-3,
  anchor_grade_weight = 10,
  uplift_weight = 1e-5,
  anchor_grade_window = 10,
  control_tolerance = 1e-7,
  adaptive_constraints = NULL
) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop(
      "The `Matrix` package is required for road profile solving.",
      call. = FALSE
    )
  }
  layer_spacing = validate_render_road_profile_setting(
    layer_spacing,
    "layer_spacing"
  )
  maximum_grade = validate_render_road_profile_setting(
    maximum_grade,
    "maximum_grade"
  )
  maximum_grade_rate = validate_render_road_profile_setting(
    maximum_grade_rate,
    "maximum_grade_rate"
  )
  curvature_weight = validate_render_road_profile_setting(
    curvature_weight,
    "curvature_weight",
    allow_zero = TRUE
  )
  grade_weight = validate_render_road_profile_setting(
    grade_weight,
    "grade_weight",
    allow_zero = TRUE
  )
  terrain_reference_weight = validate_render_road_profile_setting(
    terrain_reference_weight,
    "terrain_reference_weight",
    allow_zero = TRUE
  )
  if (is.null(underground_reference_depth)) {
    underground_reference_depth = layer_spacing
  } else {
    underground_reference_depth = validate_render_road_profile_setting(
      underground_reference_depth,
      "underground_reference_depth"
    )
  }
  underground_reference_weight = validate_render_road_profile_setting(
    underground_reference_weight,
    "underground_reference_weight"
  )
  anchor_grade_weight = validate_render_road_profile_setting(
    anchor_grade_weight,
    "anchor_grade_weight",
    allow_zero = TRUE
  )
  uplift_weight = validate_render_road_profile_setting(
    uplift_weight,
    "uplift_weight",
    allow_zero = TRUE
  )
  anchor_grade_window = validate_render_road_profile_setting(
    anchor_grade_window,
    "anchor_grade_window"
  )
  control_tolerance = validate_render_road_profile_setting(
    control_tolerance,
    "control_tolerance"
  )

  subset = subset_render_road_profile_topology(
    topology,
    terrain_profiles,
    explicit_controls
  )
  topology = subset$topology
  fragments = topology$fragments
  fragment_id = fragments$render_road_fragment_id
  terrain_profiles = normalize_render_road_terrain_profiles(
    topology,
    subset$terrain_profiles
  )
  geometry_info = attr(terrain_profiles, "geometry_info")
  fragment_length = vapply(geometry_info, `[[`, numeric(1), "length")
  names(fragment_length) = as.character(fragment_id)
  explicit_controls = normalize_render_road_explicit_controls(
    subset$explicit_controls,
    fragment_id
  )
  adaptive_constraints = normalize_render_road_adaptive_constraints(
    adaptive_constraints
  )
  if (
    nrow(adaptive_constraints) &&
      any(!(adaptive_constraints$fragment_a %in% fragment_id))
  ) {
    stop("Adaptive controls reference an inactive fragment.", call. = FALSE)
  }
  profile_spans = build_render_road_profile_spans(
    topology,
    fragment_length
  )
  controls = build_render_road_profile_controls(
    topology = topology,
    terrain_profiles = terrain_profiles,
    explicit_controls = explicit_controls,
    adaptive_constraints = adaptive_constraints,
    profile_spans = profile_spans,
    fragment_length = fragment_length,
    control_tolerance = control_tolerance
  )
  profile_spans = attach_render_road_profile_span_controls(
    profile_spans,
    controls,
    fragment_length,
    control_tolerance
  )
  anchor_sets = identify_render_road_profile_anchor_sets(
    topology,
    fragment_length,
    control_tolerance
  )
  interval_result = build_render_road_interval_constraints(
    topology,
    controls,
    maximum_grade,
    maximum_grade_rate
  )
  anchor_result = build_render_road_anchor_constraints(
    topology = topology,
    terrain_profiles = terrain_profiles,
    controls = controls,
    anchor_sets = anchor_sets,
    fragment_length = fragment_length,
    control_tolerance = control_tolerance,
    anchor_grade_window = anchor_grade_window
  )
  event_result = build_render_road_event_constraints(
    topology = topology,
    controls = controls,
    adaptive_constraints = adaptive_constraints,
    layer_spacing = layer_spacing,
    control_tolerance = control_tolerance
  )
  continuation_result = build_render_road_continuation_constraints(
    topology = topology,
    controls = controls,
    fragment_length = fragment_length,
    maximum_grade_rate = maximum_grade_rate,
    control_tolerance = control_tolerance
  )
  chord_result = build_render_road_chord_constraints(
    profile_spans,
    controls
  )
  constraints = c(
    interval_result$constraints,
    anchor_result$constraints,
    event_result$constraints,
    continuation_result$constraints,
    chord_result$constraints
  )
  curvature_terms = rbind(
    interval_result$curvature_terms,
    continuation_result$curvature_terms
  )
  objective = build_render_road_profile_objective(
    controls = controls,
    profile_spans = profile_spans,
    anchors = anchor_result$anchors,
    curvature_terms = curvature_terms,
    curvature_weight = curvature_weight,
    grade_weight = grade_weight,
    terrain_reference_weight = terrain_reference_weight,
    underground_reference_depth = underground_reference_depth,
    underground_reference_weight = underground_reference_weight,
    anchor_grade_weight = anchor_grade_weight,
    uplift_weight = uplift_weight
  )
  matrices = compile_render_road_profile_matrices(
    constraints,
    nrow(controls) * 2L
  )
  validate_render_road_profile_component_blocks(
    topology = topology,
    controls = controls,
    constraints = constraints,
    matrix_a = matrices$A,
    matrix_p = objective$P
  )
  result = list(
    topology = topology,
    terrain_profiles = terrain_profiles,
    explicit_controls = explicit_controls,
    adaptive_constraints = adaptive_constraints,
    fragment_length = fragment_length,
    controls = controls,
    intervals = interval_result$intervals,
    spans = profile_spans$spans,
    span_members = profile_spans$members,
    support_arcs = profile_spans$support_arcs,
    anchors = anchor_result$anchors,
    anchor_sets = anchor_sets,
    clearances = event_result$clearances,
    overlap_relations = event_result$overlap_relations,
    junction_equalities = event_result$junction_equalities,
    continuation_equalities = continuation_result$continuation_equalities,
    chord_controls = chord_result$chord_controls,
    curvature_terms = curvature_terms,
    topology_conflict_pairs = event_result$topology_conflict_pairs,
    P = objective$P,
    q = objective$q,
    A = matrices$A,
    lower = matrices$lower,
    upper = matrices$upper,
    constraints = matrices$constraints,
    variable_component = c(
      controls$solve_component_id,
      controls$solve_component_id
    ),
    settings = list(
      layer_spacing = layer_spacing,
      maximum_grade = maximum_grade,
      maximum_grade_rate = maximum_grade_rate,
      curvature_weight = curvature_weight,
      grade_weight = grade_weight,
      terrain_reference_weight = terrain_reference_weight,
      underground_reference_depth = underground_reference_depth,
      underground_reference_weight = underground_reference_weight,
      anchor_grade_weight = anchor_grade_weight,
      uplift_weight = uplift_weight,
      anchor_grade_window = anchor_grade_window,
      control_tolerance = control_tolerance
    ),
    diagnostics = list(
      control_count = nrow(controls),
      adaptive_control_count = sum(controls$adaptive_control),
      constraint_count = nrow(matrices$constraints),
      constraint_counts = table(matrices$constraints$type),
      ground_anchor_endpoint_id = anchor_sets$ground_anchor_endpoint_id,
      solve_frontier_endpoint_id = anchor_sets$solve_frontier_endpoint_id,
      boundary_frontier_endpoint_id = anchor_sets$boundary_endpoint_id,
      ambiguous_endpoint_id = anchor_sets$ambiguous_endpoint_id,
      conflict_endpoint_id = anchor_sets$conflict_endpoint_id
    )
  )
  class(result) = c("render_road_profile_problem", class(result))
  result
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
  rows = which(controls$render_road_fragment_id == fragment)
  rows = rows[order(controls$distance[rows])]
  if (length(rows) < 2L) {
    stop(
      sprintf("Fragment %s does not have two profile controls.", fragment),
      call. = FALSE
    )
  }
  distance = as.numeric(distance)
  profile_length = tail(controls$distance[rows], 1L)
  distance = pmin(pmax(distance, 0), profile_length)
  interval = findInterval(
    distance,
    controls$distance[rows],
    all.inside = TRUE,
    rightmost.closed = TRUE
  )
  interval = pmin(interval, length(rows) - 1L)
  first = rows[interval]
  second = rows[interval + 1L]
  interval_length = controls$distance[second] - controls$distance[first]
  local_distance = distance - controls$distance[first]
  grade_change = controls$grade[second] - controls$grade[first]
  height = controls$height[first] +
    controls$grade[first] * local_distance +
    grade_change * local_distance^2 / (2 * interval_length)
  grade = controls$grade[first] +
    grade_change * local_distance / interval_length
  data.frame(
    distance = distance,
    height = height,
    grade = grade,
    control_a = first,
    control_b = second,
    stringsAsFactors = FALSE
  )
}

#' Collapse continuous-audit requests by constraint family and relation
#'
#' @param request_rows Candidate adaptive-request rows.
#'
#' @return Deduplicated adaptive constraint requests.
#' @keywords internal
collapse_render_road_profile_adaptive_requests = function(request_rows) {
  if (!length(request_rows)) {
    return(normalize_render_road_adaptive_constraints())
  }
  requests = do.call(rbind, request_rows)
  single_fragment = requests$type != "overlap_clearance"
  selected_request = integer(0)
  if (any(single_fragment)) {
    request_group = split(
      which(single_fragment),
      interaction(
        requests$type[single_fragment],
        requests$fragment_a[single_fragment],
        drop = TRUE
      )
    )
    selected_request = c(
      selected_request,
      vapply(
        request_group,
        function(rows) rows[[which.min(requests$source_margin[rows])]],
        integer(1)
      )
    )
  }
  if (any(!single_fragment)) {
    overlap_group = split(
      which(!single_fragment),
      requests$event_id[!single_fragment]
    )
    selected_request = c(
      selected_request,
      vapply(
        overlap_group,
        function(rows) rows[[which.min(requests$source_margin[rows])]],
        integer(1)
      )
    )
  }
  requests = requests[sort(unique(selected_request)), , drop = FALSE]
  request_key = paste(
    requests$type,
    requests$fragment_a,
    signif(requests$distance_a, 12),
    requests$fragment_b,
    signif(requests$distance_b, 12),
    requests$event_id,
    sep = ":"
  )
  requests[!duplicated(request_key), , drop = FALSE]
}

#' Find continuous road-profile engineering violations
#'
#' @param problem Sparse road profile problem.
#' @param solution Solved road profile object.
#' @param tolerance Feasibility tolerance in metres.
#'
#' @return Continuous margins, details, and adaptive control requests.
#' @keywords internal
find_render_road_profile_continuous_violations = function(
  problem,
  solution,
  tolerance
) {
  controls = solution$controls
  fragments = problem$topology$fragments
  underground = identify_render_road_underground_fragments(fragments)
  underground_fragment = fragments$render_road_fragment_id[underground]
  terrain_rows = list()
  chord_rows = list()
  overlap_rows = list()
  request_rows = list()
  finite_profile_coordinates = all(is.finite(c(
    controls$height,
    controls$grade,
    controls$distance,
    controls$terrain
  )))
  for (fragment in fragments$render_road_fragment_id) {
    fragment_controls = which(controls$render_road_fragment_id == fragment)
    fragment_controls = fragment_controls[
      order(controls$distance[fragment_controls])
    ]
    terrain_profile = problem$terrain_profiles[[as.character(fragment)]]
    evaluation_distance = sort(unique(c(
      controls$distance[fragment_controls],
      terrain_profile$distance
    )))
    evaluation = evaluate_render_road_profile_at(
      problem,
      solution,
      fragment,
      evaluation_distance
    )
    fragment_row = match(
      fragment,
      fragments$render_road_fragment_id
    )
    xy = interpolate_render_road_metric_line(
      sf::st_geometry(fragments)[[fragment_row]],
      evaluation_distance
    )
    finite_profile_coordinates = finite_profile_coordinates &&
      all(is.finite(c(evaluation$height, evaluation$grade, xy)))
    if (fragment %in% underground_fragment) {
      next
    }
    for (interval in seq_len(length(fragment_controls) - 1L)) {
      control_a = fragment_controls[[interval]]
      control_b = fragment_controls[[interval + 1L]]
      interval_start = controls$distance[[control_a]]
      interval_end = controls$distance[[control_b]]
      interval_length = interval_end - interval_start
      quadratic_coefficient = (controls$grade[[control_b]] -
        controls$grade[[control_a]]) /
        (2 * interval_length)
      for (terrain_interval in seq_len(nrow(terrain_profile) - 1L)) {
        terrain_start = terrain_profile$distance[[terrain_interval]]
        terrain_end = terrain_profile$distance[[terrain_interval + 1L]]
        check_start = max(interval_start, terrain_start)
        check_end = min(interval_end, terrain_end)
        if (check_end < check_start) {
          next
        }
        terrain_slope = (terrain_profile$elevation[[terrain_interval + 1L]] -
          terrain_profile$elevation[[terrain_interval]]) /
          (terrain_end - terrain_start)
        candidate = c(check_start, check_end)
        if (abs(quadratic_coefficient) > 1e-14) {
          stationary_local = (terrain_slope - controls$grade[[control_a]]) /
            (2 * quadratic_coefficient)
          stationary = interval_start + stationary_local
          if (stationary > check_start && stationary < check_end) {
            candidate = c(candidate, stationary)
          }
        }
        candidate = sort(unique(candidate))
        profile_value = evaluate_render_road_profile_at(
          problem,
          solution,
          fragment,
          candidate
        )
        terrain_value = interpolate_render_road_profile_reference(
          terrain_profile,
          candidate
        )
        margin = profile_value$height - terrain_value
        worst = which.min(margin)
        terrain_rows[[length(terrain_rows) + 1L]] = data.frame(
          render_road_fragment_id = fragment,
          distance = candidate[[worst]],
          height = profile_value$height[[worst]],
          terrain = terrain_value[[worst]],
          margin = margin[[worst]],
          stringsAsFactors = FALSE
        )
        if (margin[[worst]] < -tolerance) {
          existing_distance = controls$distance[fragment_controls]
          fragment_tolerance = max(
            controls$control_tolerance[fragment_controls]
          )
          if (
            all(
              abs(existing_distance - candidate[[worst]]) > fragment_tolerance
            )
          ) {
            request_rows[[length(request_rows) + 1L]] = data.frame(
              type = "terrain_floor",
              fragment_a = fragment,
              distance_a = candidate[[worst]],
              fragment_b = NA_integer_,
              distance_b = NA_real_,
              event_id = NA_integer_,
              clearance = NA_real_,
              source_margin = margin[[worst]],
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
  active_spans = problem$spans[problem$spans$no_dip, , drop = FALSE]
  if (nrow(active_spans)) {
    for (span_row in seq_len(nrow(active_spans))) {
      span = active_spans[span_row, , drop = FALSE]
      members = problem$span_members[
        problem$span_members$span_id == span$span_id[[1L]],
        ,
        drop = FALSE
      ]
      for (member_row in seq_len(nrow(members))) {
        member = members[member_row, , drop = FALSE]
        fragment = member$render_road_fragment_id[[1L]]
        fragment_controls = which(
          controls$render_road_fragment_id == fragment
        )
        fragment_controls = fragment_controls[
          order(controls$distance[fragment_controls])
        ]
        for (interval in seq_len(length(fragment_controls) - 1L)) {
          control_a = fragment_controls[[interval]]
          control_b = fragment_controls[[interval + 1L]]
          interval_start = controls$distance[[control_a]]
          interval_end = controls$distance[[control_b]]
          interval_length = interval_end - interval_start
          quadratic_coefficient = (controls$grade[[control_b]] -
            controls$grade[[control_a]]) /
            (2 * interval_length)
          midpoint_station = map_render_road_profile_span_station(
            problem$span_members,
            fragment,
            (interval_start + interval_end) / 2
          )
          support_arc = resolve_render_road_profile_support_arcs(
            problem$support_arcs,
            span$span_id[[1L]],
            midpoint_station
          )
          start_height = controls$height[
            support_arc$start_control_id[[1L]]
          ]
          end_height = controls$height[
            support_arc$end_control_id[[1L]]
          ]
          span_slope = (end_height - start_height) /
            support_arc$arc_length[[1L]]
          chord_local_slope = span_slope * member$orientation[[1L]]
          candidate = c(interval_start, interval_end)
          if (abs(quadratic_coefficient) > 1e-14) {
            stationary_local = (chord_local_slope -
              controls$grade[[control_a]]) /
              (2 * quadratic_coefficient)
            stationary = interval_start + stationary_local
            if (stationary > interval_start && stationary < interval_end) {
              candidate = c(candidate, stationary)
            }
          }
          candidate = sort(unique(candidate))
          profile_value = evaluate_render_road_profile_at(
            problem,
            solution,
            fragment,
            candidate
          )
          span_station = map_render_road_profile_span_station(
            problem$span_members,
            fragment,
            candidate
          )
          candidate_arcs = resolve_render_road_profile_support_arcs(
            problem$support_arcs,
            span$span_id[[1L]],
            span_station
          )
          chord = controls$height[candidate_arcs$start_control_id] +
            candidate_arcs$fraction *
              (controls$height[candidate_arcs$end_control_id] -
                controls$height[candidate_arcs$start_control_id])
          margin = profile_value$height - chord
          worst = which.min(margin)
          chord_rows[[length(chord_rows) + 1L]] = data.frame(
            span_id = span$span_id[[1L]],
            support_arc_id = candidate_arcs$support_arc_id[[worst]],
            render_road_fragment_id = fragment,
            distance = candidate[[worst]],
            span_station = span_station[[worst]],
            height = profile_value$height[[worst]],
            chord = chord[[worst]],
            margin = margin[[worst]],
            stringsAsFactors = FALSE
          )
          if (margin[[worst]] < -tolerance) {
            existing_distance = controls$distance[fragment_controls]
            fragment_tolerance = max(
              controls$control_tolerance[fragment_controls]
            )
            if (
              all(
                abs(existing_distance - candidate[[worst]]) > fragment_tolerance
              )
            ) {
              request_rows[[length(request_rows) + 1L]] = data.frame(
                type = "no_dip_chord",
                fragment_a = fragment,
                distance_a = candidate[[worst]],
                fragment_b = NA_integer_,
                distance_b = NA_real_,
                event_id = span$span_id[[1L]],
                clearance = NA_real_,
                source_margin = margin[[worst]],
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
    }
  }
  if (nrow(problem$overlap_relations)) {
    for (overlap in seq_len(nrow(problem$overlap_relations))) {
      relation = problem$overlap_relations[overlap, , drop = FALSE]
      lower_fragment = relation$lower_fragment_id[[1L]]
      upper_fragment = relation$upper_fragment_id[[1L]]
      lower_delta = relation$lower_distance_end[[1L]] -
        relation$lower_distance_start[[1L]]
      upper_delta = relation$upper_distance_end[[1L]] -
        relation$upper_distance_start[[1L]]
      parameter_break = c(0, 1)
      lower_controls = controls$distance[
        controls$render_road_fragment_id == lower_fragment
      ]
      upper_controls = controls$distance[
        controls$render_road_fragment_id == upper_fragment
      ]
      if (abs(lower_delta) > 0) {
        parameter_break = c(
          parameter_break,
          (lower_controls - relation$lower_distance_start[[1L]]) /
            lower_delta
        )
      }
      if (abs(upper_delta) > 0) {
        parameter_break = c(
          parameter_break,
          (upper_controls - relation$upper_distance_start[[1L]]) /
            upper_delta
        )
      }
      parameter_break = sort(unique(parameter_break[
        is.finite(parameter_break) &
          parameter_break >= 0 &
          parameter_break <= 1
      ]))
      for (interval in seq_len(length(parameter_break) - 1L)) {
        parameter_start = parameter_break[[interval]]
        parameter_end = parameter_break[[interval + 1L]]
        parameter_mid = (parameter_start + parameter_end) / 2
        interpolation_parameter = c(
          parameter_start,
          parameter_mid,
          parameter_end
        )
        lower_distance = relation$lower_distance_start[[1L]] +
          lower_delta * interpolation_parameter
        upper_distance = relation$upper_distance_start[[1L]] +
          upper_delta * interpolation_parameter
        lower_profile = evaluate_render_road_profile_at(
          problem,
          solution,
          lower_fragment,
          lower_distance
        )
        upper_profile = evaluate_render_road_profile_at(
          problem,
          solution,
          upper_fragment,
          upper_distance
        )
        margin_value = upper_profile$height -
          lower_profile$height -
          relation$clearance[[1L]]
        coefficient_c = margin_value[[1L]]
        coefficient_b = 4 *
          margin_value[[2L]] -
          3 * margin_value[[1L]] -
          margin_value[[3L]]
        coefficient_a = 2 *
          (margin_value[[1L]] + margin_value[[3L]] - 2 * margin_value[[2L]])
        local_candidate = c(0, 1)
        if (abs(coefficient_a) > 1e-14) {
          stationary = -coefficient_b / (2 * coefficient_a)
          if (stationary > 0 && stationary < 1) {
            local_candidate = c(local_candidate, stationary)
          }
        }
        local_candidate = sort(unique(local_candidate))
        parameter = parameter_start +
          (parameter_end - parameter_start) * local_candidate
        margin = coefficient_a *
          local_candidate^2 +
          coefficient_b * local_candidate +
          coefficient_c
        worst = which.min(margin)
        lower_distance_worst = relation$lower_distance_start[[1L]] +
          lower_delta * parameter[[worst]]
        upper_distance_worst = relation$upper_distance_start[[1L]] +
          upper_delta * parameter[[worst]]
        overlap_rows[[length(overlap_rows) + 1L]] = data.frame(
          overlap_id = relation$overlap_id[[1L]],
          parameter = parameter[[worst]],
          lower_fragment_id = lower_fragment,
          upper_fragment_id = upper_fragment,
          lower_distance = lower_distance_worst,
          upper_distance = upper_distance_worst,
          margin = margin[[worst]],
          stringsAsFactors = FALSE
        )
        if (margin[[worst]] < -tolerance) {
          prior = problem$adaptive_constraints[
            problem$adaptive_constraints$type == "overlap_clearance" &
              problem$adaptive_constraints$event_id ==
                relation$overlap_id[[1L]],
            ,
            drop = FALSE
          ]
          already_requested = nrow(prior) &&
            any(
              abs(prior$distance_a - lower_distance_worst) <=
                max(controls$control_tolerance[
                  controls$render_road_fragment_id == lower_fragment
                ]) &
                abs(prior$distance_b - upper_distance_worst) <=
                  max(controls$control_tolerance[
                    controls$render_road_fragment_id == upper_fragment
                  ])
            )
          if (!already_requested) {
            request_rows[[length(request_rows) + 1L]] = data.frame(
              type = "overlap_clearance",
              fragment_a = lower_fragment,
              distance_a = lower_distance_worst,
              fragment_b = upper_fragment,
              distance_b = upper_distance_worst,
              event_id = relation$overlap_id[[1L]],
              clearance = relation$clearance[[1L]],
              source_margin = margin[[worst]],
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
  terrain_checks = if (length(terrain_rows)) {
    do.call(rbind, terrain_rows)
  } else {
    data.frame(
      render_road_fragment_id = integer(0),
      distance = numeric(0),
      height = numeric(0),
      terrain = numeric(0),
      margin = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  chord_checks = if (length(chord_rows)) {
    do.call(rbind, chord_rows)
  } else {
    data.frame(
      span_id = integer(0),
      support_arc_id = integer(0),
      render_road_fragment_id = integer(0),
      distance = numeric(0),
      span_station = numeric(0),
      height = numeric(0),
      chord = numeric(0),
      margin = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  overlap_checks = if (length(overlap_rows)) {
    do.call(rbind, overlap_rows)
  } else {
    data.frame(
      overlap_id = integer(0),
      parameter = numeric(0),
      lower_fragment_id = integer(0),
      upper_fragment_id = integer(0),
      lower_distance = numeric(0),
      upper_distance = numeric(0),
      margin = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  requests = collapse_render_road_profile_adaptive_requests(request_rows)
  list(
    continuous_terrain_margin = if (nrow(terrain_checks)) {
      min(terrain_checks$margin)
    } else {
      Inf
    },
    continuous_chord_margin = if (nrow(chord_checks)) {
      min(chord_checks$margin)
    } else {
      Inf
    },
    continuous_overlap_clearance_margin = if (nrow(overlap_checks)) {
      min(overlap_checks$margin)
    } else {
      Inf
    },
    finite_profile_coordinates = finite_profile_coordinates,
    terrain = terrain_checks,
    chord = chord_checks,
    overlap = overlap_checks,
    requests = requests
  )
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
  fragment_lengths = vapply(
    problem$terrain_profiles[as.character(fragment_id)],
    function(profile) max(profile$distance),
    numeric(1)
  )
  names(fragment_lengths) = as.character(fragment_id)
  clearance = problem$clearances[
    problem$clearances$solve_component_id == component_id,
    ,
    drop = FALSE
  ]
  if (nrow(clearance)) {
    clearance$lower_distance = controls$distance[
      clearance$lower_control_id
    ]
    clearance$upper_distance = controls$distance[
      clearance$upper_control_id
    ]
    clearance$lower_approach_before = clearance$lower_distance
    clearance$lower_approach_after =
      fragment_lengths[as.character(clearance$lower_fragment_id)] -
      clearance$lower_distance
    clearance$upper_approach_before = clearance$upper_distance
    clearance$upper_approach_after =
      fragment_lengths[as.character(clearance$upper_fragment_id)] -
      clearance$upper_distance
  }
  fragments = problem$topology$fragments[fragment_rows, , drop = FALSE]
  metadata_columns = intersect(
    c(
      "render_road_fragment_id",
      "render_road_feature_id",
      "render_road_way_id",
      "render_road_ref",
      "render_road_name",
      "render_road_highway",
      "render_road_layer",
      "render_road_bridge",
      "render_road_tunnel",
      "render_road_location"
    ),
    names(fragments)
  )
  conflict_pairs = problem$topology_conflict_pairs
  conflict_pairs = conflict_pairs[
    conflict_pairs$fragment_a %in%
      fragment_id |
      conflict_pairs$fragment_b %in% fragment_id,
    ,
    drop = FALSE
  ]
  crossing_id = unique(clearance$event_id[clearance$type == "crossing"])
  junction_id = unique(
    problem$junction_equalities$junction_id[
      problem$junction_equalities$solve_component_id == component_id
    ]
  )
  list(
    status = status,
    solve_component_id = component_id,
    fragment_id = fragment_id,
    feature_id = sort(unique(
      problem$topology$fragments$render_road_feature_id[fragment_rows]
    )),
    fragment_metadata = sf::st_drop_geometry(
      fragments[, metadata_columns, drop = FALSE]
    ),
    clearances = clearance,
    anchors = problem$anchors[
      problem$anchors$solve_component_id == component_id,
      ,
      drop = FALSE
    ],
    constraints = problem$constraints[
      problem$constraints$solve_component_id == component_id,
      ,
      drop = FALSE
    ],
    event_geometry = list(
      crossings = problem$topology$crossings[
        problem$topology$crossings$crossing_id %in% crossing_id,
        ,
        drop = FALSE
      ],
      junctions = problem$topology$junctions[
        problem$topology$junctions$junction_id %in% junction_id,
        ,
        drop = FALSE
      ],
      conflicts = problem$topology$topology_conflicts[
        problem$topology$topology_conflicts$conflict_id %in%
          conflict_pairs$conflict_id,
        ,
        drop = FALSE
      ]
    ),
    ambiguous_endpoints = problem$anchor_sets$endpoints[
      problem$anchor_sets$endpoints$render_road_endpoint_id %in%
        problem$anchor_sets$ambiguous_endpoint_id,
      ,
      drop = FALSE
    ],
    conflict_endpoints = problem$anchor_sets$endpoints[
      problem$anchor_sets$endpoints$render_road_endpoint_id %in%
        problem$anchor_sets$conflict_endpoint_id,
      ,
      drop = FALSE
    ]
  )
}

#' Solve each independent road-profile component once
#'
#' @param problem Sparse road profile problem.
#' @param verbose Whether OSQP prints progress.
#' @param absolute_tolerance Absolute solver tolerance.
#' @param relative_tolerance Relative solver tolerance.
#' @param maximum_iterations Maximum OSQP iterations per component.
#'
#' @return One assembled road-profile solution.
#' @keywords internal
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
          diagnostics = diagnostics,
          solver_result = result
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

#' Rebuild a road-profile problem with adaptive constraints
#'
#' @param problem Existing road-profile problem.
#' @param adaptive_constraints Accumulated adaptive constraints.
#'
#' @return Rebuilt sparse road-profile problem.
#' @keywords internal
rebuild_render_road_profile_problem = function(
  problem,
  adaptive_constraints
) {
  do.call(
    build_render_road_profile_problem,
    c(
      list(
        topology = problem$topology,
        terrain_profiles = problem$terrain_profiles,
        explicit_controls = problem$explicit_controls,
        adaptive_constraints = adaptive_constraints
      ),
      problem$settings
    )
  )
}

#' Solve sparse quadratic road profiles by component
#'
#' @param problem Sparse road profile problem.
#' @param verbose Default `FALSE`. Whether OSQP prints progress.
#' @param absolute_tolerance Default `1e-7`. Absolute solver tolerance.
#' @param relative_tolerance Default `1e-7`. Relative solver tolerance.
#' @param maximum_iterations Default `20000`. Maximum OSQP iterations per
#' component.
#' @param profile_tolerance Default `1e-3`. Accepted geometric profile
#' tolerance in metres for adaptive refinement and the engineering audit.
#' @param maximum_refinement_iterations Default `20`. Maximum adaptive
#' continuous-constraint refinement iterations.
#'
#' @return Solved variables, controls, and component diagnostics.
#' @keywords internal
solve_render_road_profile_problem = function(
  problem,
  verbose = FALSE,
  absolute_tolerance = 1e-7,
  relative_tolerance = 1e-7,
  maximum_iterations = 20000,
  profile_tolerance = 1e-3,
  maximum_refinement_iterations = 20
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
  absolute_tolerance = validate_render_road_profile_setting(
    absolute_tolerance,
    "absolute_tolerance"
  )
  relative_tolerance = validate_render_road_profile_setting(
    relative_tolerance,
    "relative_tolerance"
  )
  profile_tolerance = validate_render_road_profile_setting(
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
  current_problem = problem
  for (refinement_iteration in 0:as.integer(maximum_refinement_iterations)) {
    solved = solve_render_road_profile_components_once(
      problem = current_problem,
      verbose = verbose,
      absolute_tolerance = absolute_tolerance,
      relative_tolerance = relative_tolerance,
      maximum_iterations = maximum_iterations
    )
    continuous = find_render_road_profile_continuous_violations(
      current_problem,
      solved,
      profile_tolerance
    )
    if (nrow(continuous$requests)) {
      if (refinement_iteration >= maximum_refinement_iterations) {
        condition = structure(
          list(
            message = sprintf(
              paste0(
                "Road-profile continuous refinement did not converge after ",
                "%d iterations."
              ),
              maximum_refinement_iterations
            ),
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
      adaptive_constraints = rbind(
        current_problem$adaptive_constraints,
        continuous$requests
      )
      request_key = paste(
        adaptive_constraints$type,
        adaptive_constraints$fragment_a,
        signif(adaptive_constraints$distance_a, 12),
        adaptive_constraints$fragment_b,
        signif(adaptive_constraints$distance_b, 12),
        adaptive_constraints$event_id,
        sep = ":"
      )
      adaptive_constraints = adaptive_constraints[
        !duplicated(request_key),
        ,
        drop = FALSE
      ]
      if (
        nrow(adaptive_constraints) <= nrow(current_problem$adaptive_constraints)
      ) {
        condition = structure(
          list(
            message = paste0(
              "A continuous road-profile violation remained at an ",
              "already-constrained station."
            ),
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
      current_problem = rebuild_render_road_profile_problem(
        current_problem,
        adaptive_constraints
      )
      next
    }
    engineering_audit = audit_render_road_profiles(
      current_problem,
      solved,
      tolerance = profile_tolerance
    )
    inaccurate = any(
      tolower(solved$components$status) == "solved inaccurate"
    )
    if (!engineering_audit$passed) {
      status = if (inaccurate) {
        "solved inaccurate; engineering audit failed"
      } else {
        "engineering audit failed"
      }
      component_id = solved$components$solve_component_id[
        which.max(solved$components$primal_residual)
      ]
      condition = structure(
        list(
          message = sprintf(
            "Road profile component %d failed after %s.",
            component_id,
            status
          ),
          call = NULL,
          diagnostics = list(
            component = diagnose_render_road_profile_component(
              current_problem,
              component_id,
              status
            ),
            audit = engineering_audit
          ),
          solution = solved
        ),
        class = c(
          "render_road_profile_infeasible",
          "error",
          "condition"
        )
      )
      stop(condition)
    }
    solved$problem = current_problem
    solved$continuous_diagnostics = continuous
    solved$engineering_audit = engineering_audit
    solved$refinement_iterations = refinement_iteration
    return(solved)
  }
  stop("Road-profile refinement ended unexpectedly.", call. = FALSE)
}

#' Evaluate solved quadratic road profiles
#'
#' @param problem Sparse road profile problem.
#' @param solution Solved road profile object.
#' @param sample_distances Default `NULL`. Optional list of evaluation distances
#' by fragment. Terrain-reference distances are used by default.
#'
#' @return Named data frames containing exact quadratic profile evaluations.
#' @keywords internal
evaluate_render_road_profiles = function(
  problem,
  solution,
  sample_distances = NULL
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
  fragments = problem$topology$fragments
  fragment_id = fragments$render_road_fragment_id
  if (is.null(sample_distances)) {
    sample_distances = lapply(problem$terrain_profiles, `[[`, "distance")
  } else if (!is.list(sample_distances)) {
    stop("`sample_distances` must be a list.", call. = FALSE)
  } else if (!is.null(names(sample_distances))) {
    sample_index = match(as.character(fragment_id), names(sample_distances))
    if (anyNA(sample_index)) {
      stop(
        "Named `sample_distances` must include every fragment ID.",
        call. = FALSE
      )
    }
    sample_distances = sample_distances[sample_index]
  } else if (length(sample_distances) != nrow(fragments)) {
    stop(
      "`sample_distances` must contain one entry per fragment.",
      call. = FALSE
    )
  }

  controls = solution$controls
  evaluated = lapply(seq_len(nrow(fragments)), function(fragment_row) {
    fragment = fragment_id[[fragment_row]]
    control_rows = which(controls$render_road_fragment_id == fragment)
    control_rows = control_rows[order(controls$distance[control_rows])]
    control_distance = controls$distance[control_rows]
    profile_length = tail(control_distance, 1L)
    distance = sort(unique(c(
      0,
      profile_length,
      control_distance,
      as.numeric(sample_distances[[fragment_row]])
    )))
    distance = distance[is.finite(distance)]
    distance = pmin(pmax(distance, 0), profile_length)
    distance = sort(unique(distance))
    interval = findInterval(
      distance,
      control_distance,
      all.inside = TRUE,
      rightmost.closed = TRUE
    )
    interval = pmin(interval, length(control_rows) - 1L)
    first = control_rows[interval]
    second = control_rows[interval + 1L]
    interval_length = controls$distance[second] - controls$distance[first]
    local_distance = distance - controls$distance[first]
    grade_change = controls$grade[second] - controls$grade[first]
    height = controls$height[first] +
      controls$grade[first] * local_distance +
      grade_change * local_distance^2 / (2 * interval_length)
    grade = controls$grade[first] +
      grade_change * local_distance / interval_length
    xy = interpolate_render_road_metric_line(
      sf::st_geometry(fragments)[[fragment_row]],
      distance
    )
    member = problem$span_members[
      problem$span_members$render_road_fragment_id == fragment,
      ,
      drop = FALSE
    ]
    span = problem$spans[
      problem$spans$span_id == member$span_id[[1L]],
      ,
      drop = FALSE
    ]
    span_station = map_render_road_profile_span_station(
      problem$span_members,
      fragment,
      distance
    )
    support_arcs = resolve_render_road_profile_support_arcs(
      problem$support_arcs,
      span$span_id[[1L]],
      span_station
    )
    endpoint_chord = controls$height[support_arcs$start_control_id] +
      support_arcs$fraction *
        (controls$height[support_arcs$end_control_id] -
          controls$height[support_arcs$start_control_id])
    data.frame(
      render_road_fragment_id = fragment,
      render_road_feature_id = fragments$render_road_feature_id[[fragment_row]],
      solve_component_id = fragments$solve_component_id[[fragment_row]],
      render_road_layer = fragments$render_road_layer[[fragment_row]],
      distance = distance,
      span_id = member$span_id[[1L]],
      span_station = span_station,
      x = xy[, 1L],
      y = xy[, 2L],
      terrain = interpolate_render_road_profile_reference(
        problem$terrain_profiles[[fragment_row]],
        distance
      ),
      endpoint_chord = endpoint_chord,
      height = height,
      grade = grade,
      stringsAsFactors = FALSE
    )
  })
  names(evaluated) = as.character(fragment_id)
  result = list(
    profiles = evaluated,
    controls = controls,
    problem = problem,
    solution = solution
  )
  class(result) = c("render_road_profile_evaluation", class(result))
  result
}

#' Audit solved road profile constraints
#'
#' @param problem Sparse road profile problem.
#' @param solution Solved road profile object.
#' @param tolerance Default `1e-6`. Feasibility tolerance.
#'
#' @return Constraint-family and engineering-margin diagnostics.
#' @keywords internal
audit_render_road_profiles = function(
  problem,
  solution,
  tolerance = 1e-6
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
  constraint_audit = problem$constraints
  constraint_audit$activity = activity
  constraint_audit$violation = pmax(lower_violation, upper_violation)
  family_rows = split(
    seq_len(nrow(constraint_audit)),
    constraint_audit$type
  )
  family = do.call(
    rbind,
    lapply(names(family_rows), function(type) {
      rows = family_rows[[type]]
      data.frame(
        type = type,
        constraint_count = length(rows),
        maximum_violation = max(constraint_audit$violation[rows]),
        passed = max(constraint_audit$violation[rows]) <= tolerance,
        row.names = NULL
      )
    })
  )

  controls = solution$controls
  clearances = problem$clearances
  if (nrow(clearances)) {
    clearances$lower_height = controls$height[clearances$lower_control_id]
    clearances$upper_height = controls$height[clearances$upper_control_id]
    clearances$actual_clearance =
      clearances$upper_height - clearances$lower_height
    clearances$margin = clearances$actual_clearance - clearances$clearance
  }
  intervals = problem$intervals
  if (nrow(intervals)) {
    intervals$grade_rate = abs(
      controls$grade[intervals$control_b] -
        controls$grade[intervals$control_a]
    ) /
      intervals$length
    intervals$grade_rate_margin =
      problem$settings$maximum_grade_rate - intervals$grade_rate
  }
  grade = data.frame(
    control_id = controls$control_id,
    grade = controls$grade,
    absolute_grade = abs(controls$grade),
    margin = problem$settings$maximum_grade - abs(controls$grade)
  )
  terrain = controls[, c(
    "control_id",
    "render_road_fragment_id",
    "distance",
    "terrain",
    "height"
  )]
  terrain$margin = terrain$height - terrain$terrain

  continuation = problem$continuation_equalities
  if (nrow(continuation)) {
    continuation$height_residual =
      controls$height[continuation$control_b] -
      controls$height[continuation$control_a]
    continuation$oriented_grade_residual =
      continuation$sign_a *
      controls$grade[continuation$control_a] -
      continuation$sign_b * controls$grade[continuation$control_b]
    continuation$height_margin =
      tolerance - abs(continuation$height_residual)
    continuation$oriented_grade_margin =
      tolerance - abs(continuation$oriented_grade_residual)
  }
  junction = problem$junction_equalities
  if (nrow(junction)) {
    junction$height_residual =
      controls$height[junction$control_b] -
      controls$height[junction$control_a]
    junction$height_margin = tolerance - abs(junction$height_residual)
  }
  chord = problem$chord_controls
  if (nrow(chord)) {
    chord$height = controls$height[chord$control_id]
    chord$chord =
      (1 - chord$fraction) *
      controls$height[chord$start_control_id] +
      chord$fraction * controls$height[chord$end_control_id]
    chord$margin = chord$height - chord$chord
  }
  continuous = find_render_road_profile_continuous_violations(
    problem,
    solution,
    tolerance
  )
  discrete_maximum_violation = if (nrow(constraint_audit)) {
    max(constraint_audit$violation)
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
    discrete_maximum_violation,
    continuous_violation,
    finite_violation
  )
  result = list(
    passed = is.finite(maximum_violation) &&
      maximum_violation <= tolerance &&
      continuous$finite_profile_coordinates,
    tolerance = tolerance,
    maximum_violation = maximum_violation,
    constraints = constraint_audit,
    families = family,
    clearances = clearances,
    grade = grade,
    grade_rate = intervals,
    terrain = terrain,
    continuations = continuation,
    junctions = junction,
    chord = chord,
    continuous_terrain_margin = continuous$continuous_terrain_margin,
    continuous_chord_margin = continuous$continuous_chord_margin,
    continuous_overlap_clearance_margin = continuous$continuous_overlap_clearance_margin,
    finite_profile_coordinates = continuous$finite_profile_coordinates,
    continuous = continuous,
    ground_anchor_endpoint_id = problem$anchor_sets$ground_anchor_endpoint_id,
    solve_frontier_endpoint_id = problem$anchor_sets$solve_frontier_endpoint_id,
    ambiguous_endpoint_id = problem$anchor_sets$ambiguous_endpoint_id,
    conflict_endpoint_id = problem$anchor_sets$conflict_endpoint_id
  )
  class(result) = c("render_road_profile_audit", class(result))
  result
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
