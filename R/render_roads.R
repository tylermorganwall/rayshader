#' Render Road Paths
#'
#' @description Adds road paths to the scene. Roads are previewed as rgl lines
#' and rendered by [render_highquality()] as flat terrain-following rectangles.
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
#' lane texture and, when `width = NULL`, deriving the road width.
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
    road_merge = merge,
    lane_texture = lane_texture,
    lane_texture_file = lane_texture_file,
    lane_dash_length = lane_dash_length,
    lane_gap_length = lane_gap_length,
    lane_texture_length = lane_texture_length,
    lane_texture_mapping = lane_texture_mapping,
    lanes = lanes,
    lane_width = lane_width,
    lane_color = lane_color,
    centerline_color = centerline_color,
    edge_line_color = edge_line_color,
    lane_line_width = lane_line_width,
    lane_dash_fraction = lane_dash_fraction
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
  lane_texture = validate_waterpath_logical(lane_texture, "lane_texture")
  lane_texture_mapping = match.arg(lane_texture_mapping)
  lanes = validate_road_lanes(lanes)
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
  texture_world_scale = calculate_road_path_world_scale(
    heightmap = heightmap,
    extent = extent
  )
  road_width = resolve_render_road_width(
    road_width = road_width,
    lanes = lanes,
    lane_width = lane_width,
    texture_world_scale = texture_world_scale
  )
  road_width = resolve_waterpath_widths(
    waterpaths = roads,
    waterpath_width = road_width,
    waterpath_width_column = road_width_column
  )
  path_render = render_water_path_coords_by_width(
    waterpaths = roads,
    heightmap = heightmap,
    extent = extent,
    zscale = zscale,
    watercolor = roadcolor,
    waterpath_width = road_width
  )
  coord_list = path_render$coord_list
  coord_width = path_render$width
  if (!length(coord_list)) {
    return(invisible(coord_list))
  }
  if (isTRUE(road_densify)) {
    coord_list = densify_water_path_coords(
      coord_list = coord_list,
      heightmap = heightmap,
      zscale = zscale,
      offset = road_offset
    )
  } else if (!identical(road_offset, 0)) {
    coord_list = offset_water_path_coords(
      coord_list = coord_list,
      offset = road_offset / zscale
    )
  }
  texture_file = resolve_road_lane_texture_file(
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
          texture_file = texture_file,
          texture_length = texture_mapping$texture_length[[coord_index]],
          texture_repeats = texture_repeats,
          texture_mapping = lane_texture_mapping,
          road_length = texture_mapping$road_length[[coord_index]],
          texture_world_scale = texture_mapping$texture_world_scale,
          road_width = coord_width[[coord_index]],
          roadcolor = roadcolor
        )
      )
    }
  }
  invisible(coord_list)
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
  png::writePNG(texture, target = texture_file)
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
  texture_world_scale = c(1, 1)
) {
  points = as.matrix(points)
  points = points[stats::complete.cases(points), , drop = FALSE]
  if (nrow(points) >= 2) {
    point_delta = points[-1L, c(1, 3), drop = FALSE] -
      points[-nrow(points), c(1, 3), drop = FALSE]
    points = points[
      c(TRUE, rowSums(point_delta^2) > .Machine$double.eps),
      ,
      drop = FALSE
    ]
  }
  if (nrow(points) < 2) {
    return(NULL)
  }
  points = densify_render_highquality_water_path_points(
    points = points,
    width = width,
    heightmap = heightmap,
    zscale = zscale
  )
  if (nrow(points) >= 2) {
    point_delta = points[-1L, c(1, 3), drop = FALSE] -
      points[-nrow(points), c(1, 3), drop = FALSE]
    points = points[
      c(TRUE, rowSums(point_delta^2) > .Machine$double.eps),
      ,
      drop = FALSE
    ]
  }
  if (nrow(points) < 2) {
    return(NULL)
  }
  half_width = width / 2
  road_height = diff(range(make_render_highquality_road_path_polygon()[, 2]))
  normals = interpolate_render_highquality_water_path_normals(
    points = points,
    heightmap = heightmap,
    zscale = zscale
  )
  tangents = calculate_render_highquality_water_path_tangents(
    points = points,
    normals = normals
  )
  side_vectors = normalize_render_highquality_rows(row_cross(tangents, normals))
  side_vectors = replace_invalid_render_highquality_vectors(
    side_vectors,
    fallback = c(0, 0, 1)
  )
  edge_centers = make_render_highquality_water_path_edge_centers(
    points = points,
    side_vectors = side_vectors,
    half_width = half_width,
    heightmap = heightmap,
    zscale = zscale
  )
  left_bottom = edge_centers$left
  right_bottom = edge_centers$right
  left_normals = interpolate_render_highquality_water_path_normals(
    points = left_bottom,
    heightmap = heightmap,
    zscale = zscale
  )
  right_normals = interpolate_render_highquality_water_path_normals(
    points = right_bottom,
    heightmap = heightmap,
    zscale = zscale
  )
  left_top = left_bottom + left_normals * road_height
  right_top = right_bottom + right_normals * road_height
  segment_indices = seq_len(nrow(points) - 1L)
  next_indices = segment_indices + 1L
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
      left_top[segment_indices, , drop = FALSE],
      left_top[next_indices, , drop = FALSE],
      right_top[next_indices, , drop = FALSE],
      right_top[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      left_bottom[segment_indices, , drop = FALSE],
      right_bottom[segment_indices, , drop = FALSE],
      right_bottom[next_indices, , drop = FALSE],
      left_bottom[next_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      left_bottom[segment_indices, , drop = FALSE],
      left_bottom[next_indices, , drop = FALSE],
      left_top[next_indices, , drop = FALSE],
      left_top[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      right_bottom[segment_indices, , drop = FALSE],
      right_top[segment_indices, , drop = FALSE],
      right_top[next_indices, , drop = FALSE],
      right_bottom[next_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      matrix(left_bottom[1L, ], nrow = 1L),
      matrix(left_top[1L, ], nrow = 1L),
      matrix(right_top[1L, ], nrow = 1L),
      matrix(right_bottom[1L, ], nrow = 1L)
    ),
    make_render_highquality_water_path_quad_rows(
      matrix(left_bottom[nrow(points), ], nrow = 1L),
      matrix(right_bottom[nrow(points), ], nrow = 1L),
      matrix(right_top[nrow(points), ], nrow = 1L),
      matrix(left_top[nrow(points), ], nrow = 1L)
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
      side_vectors[segment_indices, , drop = FALSE],
      side_vectors[next_indices, , drop = FALSE],
      side_vectors[next_indices, , drop = FALSE],
      side_vectors[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      -side_vectors[segment_indices, , drop = FALSE],
      -side_vectors[segment_indices, , drop = FALSE],
      -side_vectors[next_indices, , drop = FALSE],
      -side_vectors[next_indices, , drop = FALSE]
    ),
    make_render_highquality_water_path_quad_rows(
      matrix(-tangents[1L, ], nrow = 1L),
      matrix(-tangents[1L, ], nrow = 1L),
      matrix(-tangents[1L, ], nrow = 1L),
      matrix(-tangents[1L, ], nrow = 1L)
    ),
    make_render_highquality_water_path_quad_rows(
      matrix(tangents[nrow(points), ], nrow = 1L),
      matrix(tangents[nrow(points), ], nrow = 1L),
      matrix(tangents[nrow(points), ], nrow = 1L),
      matrix(tangents[nrow(points), ], nrow = 1L)
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
