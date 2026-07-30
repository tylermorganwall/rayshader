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
  lane_color = "white",
  centerline_color = "#ffd23f",
  edge_line_color = "white",
  lane_line_width = 0.035,
  lane_dash_fraction = NULL,
  clear_previous = TRUE
) {
  heightmap_missing = missing(heightmap)
  zscale_missing = missing(zscale)
  vertical_exaggeration_missing = missing(vertical_exaggeration)
  width_missing = missing(width)
  width_expr = substitute(width)
  width_column_expr = substitute(width_column)
  layer_expr = substitute(layer)
  layer_height_expr = substitute(layer_height)
  lanes_expr = substitute(lanes)

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
  width_column = resolve_render_line_width_column(
    width_column = width_column,
    width_column_expr = width_column_expr,
    width_column_missing = missing(width_column)
  )
  layer_column = resolve_render_road_column(
    value = layer,
    value_expr = layer_expr,
    value_missing = missing(layer),
    argument = "layer"
  )
  layer_height_spec = resolve_render_road_layer_height(
    value = layer_height,
    value_expr = layer_height_expr,
    value_missing = missing(layer_height)
  )
  lanes_spec = resolve_render_road_lanes(
    value = lanes,
    value_expr = lanes_expr,
    value_missing = missing(lanes)
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
  } else {
    layer_height_spec$spacing = resolve_render_scalar(
      layer_height_spec$spacing,
      FALSE,
      5.5,
      "layer_height",
      lower = 0,
      lower_inclusive = FALSE
    )
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
  } else {
    lanes_spec$value = resolve_render_scalar(
      lanes_spec$value,
      FALSE,
      2L,
      "lanes",
      type = "integer",
      lower = 1
    )
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
  if (!is.null(road_layer_column)) {
    road_merge = FALSE
  }
  if (!is.null(road_lanes_column)) {
    road_merge = FALSE
  }
  if (!is.null(road_width_column)) {
    road_merge = FALSE
  }
  roads = prepare_render_line_geometry(
    lines = roads,
    merge = road_merge,
    line_argument = "roads"
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
  } else if (!is.null(road_width_column)) {
    road_width = as.numeric(roads[[road_width_column]])
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
  if (isTRUE(road_densify)) {
    coord_list = densify_render_line_coords(
      coords = coord_list,
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
      lane_values = if (length(road_lanes) == 1L) {
        rep(road_lanes, nrow(roads))
      } else {
        road_lanes
      },
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
  road_id_by_path = rep(NA_integer_, length(coord_list))
  for (coord_index in seq_along(coord_list)) {
    coord = coord_list[[coord_index]]
    if (is.matrix(coord) && nrow(coord) >= 2) {
      road_id = rgl::lines3d(
        coord,
        color = roadcolor,
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
        terrain_following = coord_terrain_following[[coord_index]]
      )
    )
  }
  invisible(coord_list)
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
  fragment_path = setNames(
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
  endpoint_connection = setNames(
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

#' Solve road profiles for rendered path coordinates
#'
#' @param coord_list Terrain-sampled scene coordinate matrices.
#' @param coord_feature Source feature index for every coordinate matrix.
#' @param roads Prepared road features corresponding to `coord_feature`.
#' @param layer_column Column containing OSM-style layer values.
#' @param lane_column Default `NULL`. Optional lane-count column used as
#' continuation evidence.
#' @param lane_values Default `NULL`. Effective positive lane counts used by
#' the rendered paths.
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
  lane_values = NULL,
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

#' Assert one road-profile solver setting
#'
#' @param value Setting value.
#' @param argument Argument name used in errors.
#' @param allow_zero Default `FALSE`. Whether zero is valid.
#'
#' @return An asserted numeric scalar.
#' @keywords internal
assert_render_road_profile_setting = function(
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
  adaptive_constraints,
  profile_spans,
  fragment_length,
  control_tolerance
) {
  fragments = topology$fragments
  fragment_id = fragments$render_road_fragment_id
  fragment_row = setNames(seq_len(nrow(fragments)), fragment_id)
  control_distance = lapply(fragment_id, function(fragment) {
    c(0, fragment_length[[as.character(fragment)]])
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

#' Assert solve-component separation before optimization
#'
#' @param topology Active road topology.
#' @param controls Road-profile controls.
#' @param constraints Constraint records before matrix compilation.
#' @param matrix_a Sparse constraint matrix.
#' @param matrix_p Sparse quadratic objective matrix.
#'
#' @return `TRUE`, invisibly, or an error describing a cross-component coupling.
#' @keywords internal
assert_render_road_profile_component_blocks = function(
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
  layer_spacing = assert_render_road_profile_setting(
    layer_spacing,
    "layer_spacing"
  )
  maximum_grade = assert_render_road_profile_setting(
    maximum_grade,
    "maximum_grade"
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
  anchor_grade_weight = assert_render_road_profile_setting(
    anchor_grade_weight,
    "anchor_grade_weight",
    allow_zero = TRUE
  )
  uplift_weight = assert_render_road_profile_setting(
    uplift_weight,
    "uplift_weight",
    allow_zero = TRUE
  )
  anchor_grade_window = assert_render_road_profile_setting(
    anchor_grade_window,
    "anchor_grade_window"
  )
  control_tolerance = assert_render_road_profile_setting(
    control_tolerance,
    "control_tolerance"
  )

  subset = build_render_road_active_profile_topology(
    topology,
    terrain_profiles
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
  assert_render_road_profile_component_blocks(
    topology = topology,
    controls = controls,
    constraints = constraints,
    matrix_a = matrices$A,
    matrix_p = objective$P
  )
  result = list(
    topology = topology,
    terrain_profiles = terrain_profiles,
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
          diagnostics = diagnose_render_road_profile_component(
            current_problem,
            component_id,
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
    solved$problem = current_problem
    solved$continuous_diagnostics = continuous
    solved$engineering_audit = engineering_audit
    solved$refinement_iterations = refinement_iteration
    return(solved)
  }
  stop("Road-profile refinement ended unexpectedly.", call. = FALSE)
}


#' Audit solved road profile constraints
#'
#' @param problem Sparse road profile problem.
#' @param solution Solved road profile object.
#' @param tolerance Default `1e-6`. Feasibility tolerance.
#'
#' @return Rendering-critical feasibility and continuous-margin diagnostics.
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
  constraint_violation = pmax(lower_violation, upper_violation)
  continuous = find_render_road_profile_continuous_violations(
    problem,
    solution,
    tolerance
  )
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
  coord_lanes = vapply(coord_lanes, assert_render_road_lane_count, integer(1))
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

#' Calculate road lane marking positions
#'
#' @param lanes Number of lanes.
#'
#' @return List of edge line and lane divider positions in texture u
#' coordinates.
#' @keywords internal
calculate_road_lane_marking_positions = function(lanes) {
  lanes = assert_render_road_lane_count(lanes)
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
  resolve_render_positive_number(
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
  assert_render_road_fraction(
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
      station_end = chain_station + tail(member_station, 1L)
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
#'
#' @return List of rayrender mesh objects.
#' @keywords internal
make_render_highquality_road_path_meshes = function(tasks) {
  tasks = attach_render_road_mesh_task_metadata(tasks)
  chain_tasks = assemble_render_road_mesh_chain_tasks(tasks)
  chain_members = attr(chain_tasks, "mesh_chain_members")
  envelope_sections = attr(chain_tasks, "envelope_sections")
  chain_diagnostics = attr(chain_tasks, "mesh_chain_diagnostics")
  chain_meshes = lapply(seq_along(chain_tasks), function(chain_index) {
    task = chain_tasks[[chain_index]]
    tryCatch(
      do.call(make_render_highquality_road_chain_mesh, task),
      error = function(error) {
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
            conditionMessage(error)
          ),
          call. = FALSE
        )
      }
    )
  })
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
  attr(meshes, "mesh_chain_diagnostics") = chain_diagnostics
  meshes
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
  if (point_count < if (closed) 3L else 2L) {
    stop("Road vertex frames require a valid path.", call. = FALSE)
  }
  segment_start = if (closed) {
    seq_len(point_count)
  } else {
    seq_len(point_count - 1L)
  }
  segment_end = if (closed) {
    c(seq.int(2L, point_count), 1L)
  } else {
    seq.int(2L, point_count)
  }
  segment_delta = points[segment_end, c(1, 3), drop = FALSE] -
    points[segment_start, c(1, 3), drop = FALSE]
  segment_length = sqrt(rowSums(segment_delta^2))
  if (any(!is.finite(segment_length) | segment_length <= 0)) {
    stop("Road vertex frames contain a zero-length segment.", call. = FALSE)
  }
  segment_tangent = segment_delta / segment_length
  incoming_tangent = matrix(NA_real_, nrow = point_count, ncol = 2L)
  outgoing_tangent = matrix(NA_real_, nrow = point_count, ncol = 2L)
  if (closed) {
    incoming_tangent = segment_tangent[
      c(point_count, seq_len(point_count - 1L)),
      ,
      drop = FALSE
    ]
    outgoing_tangent = segment_tangent
  } else {
    incoming_tangent[1L, ] = segment_tangent[1L, ]
    incoming_tangent[-1L, ] = segment_tangent
    outgoing_tangent[-point_count, ] = segment_tangent
    outgoing_tangent[point_count, ] = segment_tangent[nrow(segment_tangent), ]
  }
  endpoint = !closed & seq_len(point_count) %in% c(1L, point_count)
  join_rows = lapply(seq_len(point_count), function(index) {
    if (endpoint[[index]]) {
      tangent = if (index == 1L) {
        outgoing_tangent[index, ]
      } else {
        incoming_tangent[index, ]
      }
      side = c(-tangent[[2]], tangent[[1]])
      return(data.frame(
        join_style = "endpoint",
        side_x = side[[1]],
        side_z = side[[2]],
        miter_scale = 1,
        turn_cross = 0,
        turn_dot = 1,
        stringsAsFactors = FALSE
      ))
    }
    join = resolve_render_road_join_style(
      incoming_tangent[index, ],
      outgoing_tangent[index, ],
      miter_limit = miter_limit
    )
    data.frame(
      join_style = join$style,
      side_x = join$side_x,
      side_z = join$side_z,
      miter_scale = join$miter_scale,
      turn_cross = join$turn_cross,
      turn_dot = join$turn_dot,
      stringsAsFactors = FALSE
    )
  })
  joins = do.call(rbind, join_rows)
  list(
    incoming_tangent = incoming_tangent,
    outgoing_tangent = outgoing_tangent,
    side = as.matrix(joins[, c("side_x", "side_z"), drop = FALSE]),
    miter_scale = joins$miter_scale,
    join_style = joins$join_style,
    turn_cross = joins$turn_cross,
    turn_dot = joins$turn_dot,
    segment_length = segment_length
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
  side = cbind(frames$side[, 1], 0, frames$side[, 2])
  scale = frames$miter_scale
  left_bottom = points + side * (left_distance * scale)
  right_bottom = points - side * (right_distance * scale)
  heightmap_scene = scale_render_highquality_heightmap(
    heightmap = heightmap,
    zscale = zscale
  )$heightmap
  if (!is.null(heightmap_scene) && is.matrix(heightmap_scene)) {
    center_height = interpolate_render_heightmap_height(
      heightmap_scene,
      points[, 1],
      points[, 3]
    )
    center_offset = points[, 2] - center_height
    left_bottom[, 2] = interpolate_render_heightmap_height(
      heightmap_scene,
      left_bottom[, 1],
      left_bottom[, 3]
    ) +
      center_offset
    right_bottom[, 2] = interpolate_render_heightmap_height(
      heightmap_scene,
      right_bottom[, 1],
      right_bottom[, 3]
    ) +
      center_offset
  }
  left_normal = interpolate_render_highquality_normals(
    points = left_bottom,
    heightmap = heightmap,
    zscale = zscale
  )
  right_normal = interpolate_render_highquality_normals(
    points = right_bottom,
    heightmap = heightmap,
    zscale = zscale
  )
  road_height = 0.11
  list(
    points = points,
    frames = frames,
    left_bottom = left_bottom,
    right_bottom = right_bottom,
    left_top = left_bottom + left_normal * road_height,
    right_top = right_bottom + right_normal * road_height,
    left_normal = left_normal,
    right_normal = right_normal
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

#' Calculate smooth normals for a ruled road surface
#'
#' @param left_vertices Left boundary vertices ordered by road station.
#' @param right_vertices Right boundary vertices ordered by road station.
#' @param closed Whether the surface is periodic.
#' @param outward_sign Default `1`. Direction relative to the top-surface
#' winding.
#'
#' @return Left and right area-weighted vertex normals derived from the final
#' surface geometry.
#' @keywords internal
calculate_render_road_surface_normals = function(
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
  total_length = tail(station, 1L)
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
      total_length - station[[tail(guide_index, 1L)]] < minimum_guide_step
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
  guide_scale = guide_frames$miter_scale
  if (closed) {
    closing_angle = guide_angle[[1L]]
    while (closing_angle - tail(guide_angle, 1L) > pi) {
      closing_angle = closing_angle - 2 * pi
    }
    while (closing_angle - tail(guide_angle, 1L) < -pi) {
      closing_angle = closing_angle + 2 * pi
    }
    guide_station = c(guide_station, total_length)
    guide_angle = c(guide_angle, closing_angle)
    guide_scale = c(guide_scale, guide_scale[[1L]])
  }
  dense_angle = stats::approx(
    guide_station,
    guide_angle,
    xout = station,
    rule = 2
  )$y
  dense_scale = stats::approx(
    guide_station,
    guide_scale,
    xout = station,
    rule = 2
  )$y
  frames = calculate_render_road_vertex_frames(
    points,
    closed = closed,
    miter_limit = miter_limit
  )
  frames$side = cbind(cos(dense_angle), sin(dense_angle))
  frames$miter_scale = dense_scale
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

#' Build a road mesh from shared vertex sections
#'
#' @param sections Road vertex sections.
#' @param station Global station at each section.
#' @param total_length Total open or periodic path length.
#' @param bbox_center Scene center.
#' @param texture_file Default `NULL`. Road texture file.
#' @param texture_length Texture repeat length.
#' @param texture_repeats Default `NULL`. Number of texture repeats.
#' @param surface_normals Default `NULL`. Optional top and bottom surface
#' normals calculated over the complete physical mesh chain.
#' @param cap_start Whether to cap the first section.
#' @param cap_end Whether to cap the final section.
#' @param closed Whether the path is periodic.
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
    top_surface_normals = calculate_render_road_surface_normals(
      sections$left_top,
      sections$right_top,
      closed = closed
    )
    bottom_surface_normals = calculate_render_road_surface_normals(
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

#' Make a high-quality continuous road chain mesh
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
  return_mesh = FALSE
) {
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
    stabilization_fraction = c(0.2, 0.3, 0.4, 0.6, 0.8, 1)
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
  total_length = tail(station, 1L)
  if (closed) {
    closing_delta = (points[1L, c(1, 3)] -
      points[nrow(points), c(1, 3)]) *
      texture_world_scale
    total_length = total_length + sqrt(sum(closing_delta^2))
  }
  add_diagnostics = function(
    mesh,
    material_section_index = 1L,
    material_section_count = 1L,
    rendered_cap_start = !closed && cap_start,
    rendered_cap_end = !closed && cap_end,
    rendered_closed = closed
  ) {
    diagnostics = attr(mesh, "render_road_mesh_diagnostics")
    diagnostics$closed = rendered_closed
    diagnostics$cap_start = rendered_cap_start
    diagnostics$cap_end = rendered_cap_end
    diagnostics$section_count = nrow(points)
    diagnostics$join_expansion = join_diagnostics
    diagnostics$sweep_stabilization = stabilization
    diagnostics$envelope_section_count = if (is.null(envelope_sections)) {
      0L
    } else {
      nrow(envelope_sections)
    }
    diagnostics$material_section_index = material_section_index
    diagnostics$material_section_count = material_section_count
    diagnostics$minimum_left_width = min(left_distance)
    diagnostics$maximum_left_width = max(left_distance)
    diagnostics$minimum_right_width = min(right_distance)
    diagnostics$maximum_right_width = max(right_distance)
    attr(mesh, "render_road_mesh_diagnostics") = diagnostics
    mesh
  }
  if (is.null(material_sections) || length(material_sections) <= 1L) {
    mesh = build_render_road_section_mesh(
      sections = sections,
      station = station,
      total_length = total_length,
      bbox_center = bbox_center,
      texture_file = texture_file,
      texture_length = texture_length,
      texture_repeats = texture_repeats,
      cap_start = cap_start,
      cap_end = cap_end,
      closed = closed
    )
    if (is.null(mesh)) {
      return(NULL)
    }
    mesh = add_diagnostics(mesh)
    if (return_mesh) {
      return(mesh)
    }
    return(rayrender::mesh3d_model(
      mesh,
      override_material = is.null(texture_file),
      material = material
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
  shared_surface_normals = list(
    top = calculate_render_road_surface_normals(
      sections$left_top,
      sections$right_top,
      closed = closed
    ),
    bottom = calculate_render_road_surface_normals(
      sections$left_bottom,
      sections$right_bottom,
      closed = closed,
      outward_sign = -1
    )
  )
  section_meshes = vector("list", length(material_sections))
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
    section_length = tail(section_station, 1L)
    if (!is.finite(section_length) || section_length <= 0) {
      stop(
        "A road material section has nonpositive rendered length.",
        call. = FALSE
      )
    }
    specification = material_sections[[material_index]]
    section_mesh = build_render_road_section_mesh(
      sections = subset_render_road_vertex_sections(
        sections,
        section_index
      ),
      station = section_station,
      total_length = section_length,
      bbox_center = bbox_center,
      texture_file = specification$texture_file,
      texture_length = specification$texture_length,
      texture_repeats = specification$texture_repeats,
      surface_normals = subset_render_road_surface_normals(
        shared_surface_normals,
        section_index
      ),
      cap_start = !closed &&
        material_index == 1L &&
        cap_start,
      cap_end = !closed &&
        material_index == length(material_sections) &&
        cap_end,
      closed = FALSE
    )
    if (is.null(section_mesh)) {
      next
    }
    section_mesh = add_diagnostics(
      section_mesh,
      material_section_index = material_index,
      material_section_count = length(material_sections),
      rendered_cap_start = !closed &&
        material_index == 1L &&
        cap_start,
      rendered_cap_end = !closed &&
        material_index == length(material_sections) &&
        cap_end,
      rendered_closed = FALSE
    )
    section_meshes[[material_index]] = if (return_mesh) {
      section_mesh
    } else {
      rayrender::mesh3d_model(
        section_mesh,
        override_material = is.null(specification$texture_file),
        material = specification$material
      )
    }
  }
  section_meshes = Filter(Negate(is.null), section_meshes)
  if (!length(section_meshes)) {
    return(NULL)
  }
  class(section_meshes) = c("render_road_mesh_group", "list")
  attr(section_meshes, "boundary_index") = boundary_index
  section_meshes
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
