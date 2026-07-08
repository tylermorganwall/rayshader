#' Render Z Axis
#'
#' @keywords internal
format_zaxis_labels = function(values) {
  format(
    values,
    trim = TRUE,
    scientific = FALSE,
    big.mark = ","
  )
}

align_zaxis_nonnegative_labels = function(labels, breaks) {
  if (length(labels) != length(breaks)) {
    return(labels)
  }
  breaks = suppressWarnings(as.numeric(breaks))
  valid_breaks = is.finite(breaks)
  if (
    !any(valid_breaks) ||
      !any(breaks[valid_breaks] < 0) ||
      !any(breaks[valid_breaks] >= 0)
  ) {
    return(labels)
  }
  nonnegative_labels = valid_breaks & breaks >= 0
  labels[nonnegative_labels] = paste0(" ", labels[nonnegative_labels])
  labels
}

normalize_zaxis_text_side = function(side = "auto", arg_name = "zaxis_side") {
  if (is.null(side) || !length(side)) {
    return("auto")
  }
  side = tolower(as.character(side)[1])
  side = gsub("[-_[:space:]]", "", side)
  if (side %in% c("auto", "default")) {
    return("auto")
  }
  if (side %in% c("left", "l")) {
    return("left")
  }
  if (side %in% c("right", "r")) {
    return("right")
  }
  stop(
    sprintf("`%s` must be one of: auto, left, right.", arg_name),
    call. = FALSE
  )
}

resolve_zaxis_text_side_unit = function(
  side = "auto",
  auto_unit = c(1, 0),
  anchor_xyz = c(0, 0, 0),
  reference_y = 0,
  scale = 1
) {
  if (identical(side, "auto")) {
    return(auto_unit)
  }
  screen_right_unit = get_zaxis_screen_right_unit(
    anchor_xyz = anchor_xyz,
    reference_y = reference_y,
    scale = scale
  )
  if (is.null(screen_right_unit)) {
    current_side = get_zaxis_screen_side(
      unit = auto_unit,
      anchor_xyz = anchor_xyz,
      reference_y = reference_y,
      scale = scale
    )
    if (is.null(current_side) || identical(current_side, side)) {
      return(auto_unit)
    }
    return(-auto_unit)
  }
  if (identical(side, "left")) {
    return(-screen_right_unit)
  }
  screen_right_unit
}

resolve_zaxis_text_side_name = function(
  side = "auto",
  unit = c(1, 0),
  anchor_xyz = c(0, 0, 0),
  reference_y = 0,
  scale = 1,
  fallback_unit = unit
) {
  if (!identical(side, "auto")) {
    return(side)
  }
  screen_side = get_zaxis_screen_side(
    unit = unit,
    anchor_xyz = anchor_xyz,
    reference_y = reference_y,
    scale = scale
  )
  if (!is.null(screen_side)) {
    return(screen_side)
  }
  if (fallback_unit[1] >= 0) "right" else "left"
}

get_zaxis_screen_right_unit = function(
  anchor_xyz = c(0, 0, 0),
  reference_y = 0,
  scale = 1
) {
  x_delta = get_zaxis_screen_x_delta(
    unit = c(1, 0),
    anchor_xyz = anchor_xyz,
    reference_y = reference_y,
    scale = scale
  )
  z_delta = get_zaxis_screen_x_delta(
    unit = c(0, 1),
    anchor_xyz = anchor_xyz,
    reference_y = reference_y,
    scale = scale
  )
  gradient = c(x_delta, z_delta)
  if (any(!is.finite(gradient))) {
    return(NULL)
  }
  gradient_norm = sqrt(sum(gradient^2))
  if (!is.finite(gradient_norm) || gradient_norm <= 1e-12) {
    return(NULL)
  }
  gradient / gradient_norm
}

get_zaxis_screen_side = function(
  unit = c(1, 0),
  anchor_xyz = c(0, 0, 0),
  reference_y = 0,
  scale = 1
) {
  screen_delta = get_zaxis_screen_x_delta(
    unit = unit,
    anchor_xyz = anchor_xyz,
    reference_y = reference_y,
    scale = scale
  )
  if (!is.finite(screen_delta) || abs(screen_delta) <= 1e-12) {
    return(NULL)
  }
  if (screen_delta > 0) "right" else "left"
}

get_zaxis_screen_x_delta = function(
  unit = c(1, 0),
  anchor_xyz = c(0, 0, 0),
  reference_y = 0,
  scale = 1
) {
  scale = suppressWarnings(as.numeric(scale)[1])
  if (!is.finite(scale) || scale <= 0) {
    scale = 1
  }
  anchor_point = c(anchor_xyz[1], reference_y, anchor_xyz[3])
  offset_point = c(
    anchor_xyz[1] + unit[1] * scale,
    reference_y,
    anchor_xyz[3] + unit[2] * scale
  )
  anchor_screen_x = project_zaxis_point_screen_x(anchor_point)
  offset_screen_x = project_zaxis_point_screen_x(offset_point)
  if (!is.finite(anchor_screen_x) || !is.finite(offset_screen_x)) {
    return(NA_real_)
  }
  offset_screen_x - anchor_screen_x
}

project_zaxis_point_screen_x = function(point) {
  projection = tryCatch(rgl::rgl.projection(), error = function(e) NULL)
  if (
    is.null(projection) ||
      is.null(projection$model) ||
      is.null(projection$proj)
  ) {
    return(NA_real_)
  }
  point = suppressWarnings(as.numeric(point))[1:3]
  if (length(point) != 3 || any(!is.finite(point))) {
    return(NA_real_)
  }
  clip = tryCatch(
    projection$proj %*% projection$model %*% c(point, 1),
    error = function(e) NULL
  )
  if (is.null(clip) || length(clip) < 4 || !is.finite(clip[4])) {
    return(NA_real_)
  }
  if (abs(clip[4]) <= .Machine$double.eps) {
    return(NA_real_)
  }
  as.numeric(clip[1] / clip[4])
}

render_zaxis_internal = function(
  zaxis = FALSE,
  extent = NULL,
  zscale = 1,
  heightmap = NULL,
  zaxis_data = "auto",
  zaxis_location = "auto",
  zaxis_breaks = NULL,
  zaxis_labels = NULL,
  zaxis_title = "auto",
  zaxis_title_location = "side",
  zaxis_title_offset = 1.25,
  zaxis_title_size = NULL,
  zaxis_color = "black",
  zaxis_linewidth = 2,
  zaxis_text_offset = 1.5,
  zaxis_label_size = 0.8,
  zaxis_label_side = "auto",
  zaxis_title_side = "auto",
  zaxis_corner_offset = NULL,
  zaxis_tick_size = NULL
) {
  if (!isTRUE(zaxis)) {
    return(invisible(NULL))
  }
  if (is.null(extent)) {
    stop("If `zaxis = TRUE`, `extent` must be provided.")
  }

  extent_vals = get_extent(extent)
  heightmap = resolve_scene_render_heightmap(heightmap)
  zaxis_data_source = canonicalize_zaxis_data_source(zaxis_data)
  zaxis_data_info = get_scene_zaxis_data(zaxis_data_source, default = NULL)
  if (
    !zaxis_data_source %in% c("auto", "topographic") &&
      is.null(zaxis_data_info)
  ) {
    stop(
      sprintf(
        "No cached z-axis data found for `%s`. Call the matching render function first.",
        zaxis_data_source
      ),
      call. = FALSE
    )
  }
  panel_info = attr(extent, "panel_info", exact = TRUE)
  if (is.null(panel_info)) {
    panel_info = get0(
      "plot_gg_panel_info",
      envir = ray_cache_scene_envir,
      inherits = FALSE
    )
  }
  if (!is.null(panel_info) && !is.data.frame(panel_info)) {
    panel_info = NULL
  }
  has_panel_info = !is.null(panel_info) &&
    nrow(panel_info) > 0 &&
    all(
      c("data_xmin", "data_xmax", "data_ymin", "data_ymax") %in%
        names(panel_info)
    )
  cached_scene_zscale = get_scene_effective_zscale(default = NA_real_)
  if (is.null(zscale)) {
    zscale = cached_scene_zscale
  } else {
    zscale = as.numeric(zscale)[1]
    if (is.na(zscale) || zscale <= 0) {
      stop("`zscale` must be a positive number.")
    }
  }
  if (!is.finite(zscale) || zscale <= 0) {
    zscale = 1
  }
  # For terrain defaults, infer the scene zscale from the last plot_3d() call when
  # the caller leaves zscale at its default and no explicit breaks are supplied.
  if (
    is.null(zaxis_breaks) &&
      !has_panel_info &&
      is.null(heightmap) &&
      isTRUE(all.equal(zscale, 1)) &&
      is.finite(cached_scene_zscale) &&
      cached_scene_zscale > 0
  ) {
    zscale = cached_scene_zscale
  }

  valid_locations = c(
    "auto",
    "panel",
    "panelbottomleft",
    "panelbottomright",
    "paneltopleft",
    "paneltopright",
    "bottomleft",
    "bottomright",
    "topleft",
    "topright"
  )
  location_input = tolower(zaxis_location)
  location_key = gsub("[-_[:space:]]", "", location_input)
  if (location_key == "auto") {
    zaxis_location = "auto"
  } else if (location_key %in% c("panel", "panelbottomleft", "panelbl")) {
    zaxis_location = "panelbottomleft"
  } else if (location_key %in% c("panelbottomright", "panelbr")) {
    zaxis_location = "panelbottomright"
  } else if (location_key %in% c("paneltopleft", "paneltl")) {
    zaxis_location = "paneltopleft"
  } else if (location_key %in% c("paneltopright", "paneltr")) {
    zaxis_location = "paneltopright"
  } else if (location_key == "bottomleft") {
    zaxis_location = "bottomleft"
  } else if (location_key == "bottomright") {
    zaxis_location = "bottomright"
  } else if (location_key == "topleft") {
    zaxis_location = "topleft"
  } else if (location_key == "topright") {
    zaxis_location = "topright"
  } else {
    stop(
      "`zaxis_location` must be one of: ",
      paste(valid_locations, collapse = ", ")
    )
  }
  if (zaxis_location == "auto") {
    zaxis_location = if (has_panel_info) "panelbottomleft" else "bottomleft"
  }
  if (grepl("^panel", zaxis_location) && !has_panel_info) {
    zaxis_location = sub("^panel", "", zaxis_location)
    if (!nzchar(zaxis_location)) {
      zaxis_location = "bottomleft"
    }
  }
  zaxis_linewidth = as.numeric(zaxis_linewidth)[1]
  if (is.na(zaxis_linewidth) || zaxis_linewidth <= 0) {
    stop("`zaxis_linewidth` must be a positive number.")
  }
  zaxis_text_offset = as.numeric(zaxis_text_offset)[1]
  if (is.na(zaxis_text_offset) || zaxis_text_offset < 0) {
    stop("`zaxis_text_offset` must be a non-negative number.")
  }
  zaxis_label_size = as.numeric(zaxis_label_size)[1]
  if (is.na(zaxis_label_size) || zaxis_label_size <= 0) {
    stop("`zaxis_label_size` must be a positive number.")
  }
  if (is.null(zaxis_title_size)) {
    zaxis_title_size = zaxis_label_size
  }
  zaxis_label_side = normalize_zaxis_text_side(
    zaxis_label_side,
    arg_name = "zaxis_label_side"
  )
  zaxis_title_side = normalize_zaxis_text_side(
    zaxis_title_side,
    arg_name = "zaxis_title_side"
  )
  if (is.null(zaxis_title_location)) {
    zaxis_title_location = "side"
  }
  zaxis_title_location = tolower(as.character(zaxis_title_location)[1])
  zaxis_title_location = gsub("[-_[:space:]]", "", zaxis_title_location)
  if (!zaxis_title_location %in% c("side", "top")) {
    stop("`zaxis_title_location` must be either \"side\" or \"top\".")
  }
  zaxis_title_offset = as.numeric(zaxis_title_offset)[1]
  if (is.na(zaxis_title_offset) || zaxis_title_offset < 0) {
    stop("`zaxis_title_offset` must be a non-negative number.")
  }
  zaxis_title_size = as.numeric(zaxis_title_size)[1]
  if (is.na(zaxis_title_size) || zaxis_title_size <= 0) {
    stop("`zaxis_title_size` must be a positive number.")
  }
  if (is.null(zaxis_corner_offset)) {
    zaxis_corner_offset = if (has_panel_info) 0 else 0.08
  } else {
    zaxis_corner_offset = as.numeric(zaxis_corner_offset)[1]
    if (is.na(zaxis_corner_offset) || zaxis_corner_offset < 0) {
      stop("`zaxis_corner_offset` must be a non-negative number.")
    }
  }
  if (!is.null(zaxis_tick_size)) {
    zaxis_tick_size = as.numeric(zaxis_tick_size)[1]
    if (is.na(zaxis_tick_size) || zaxis_tick_size <= 0) {
      stop("`zaxis_tick_size` must be a positive number.")
    }
  }
  # Clear previous z-axis objects before deriving scene-based defaults.
  for (tag in c("zaxis_axis", "zaxis_ticks", "zaxis_labels", "zaxis_title")) {
    try(rgl::pop3d(tag = tag), silent = TRUE)
  }

  xmin = extent_vals["xmin"]
  xmax = extent_vals["xmax"]
  ymin = extent_vals["ymin"]
  ymax = extent_vals["ymax"]
  range_long = c(xmin, xmax)
  range_lat = c(ymin, ymax)
  center_long = mean(c(xmin, xmax))
  center_lat = mean(c(ymin, ymax))

  if (
    zaxis_location %in%
      c(
        "panelbottomleft",
        "panelbottomright",
        "paneltopleft",
        "paneltopright"
      )
  ) {
    panel_row = panel_info[1, , drop = FALSE]
    has_data_ranges = all(
      c("data_xmin", "data_xmax", "data_ymin", "data_ymax") %in%
        names(panel_row)
    ) &&
      all(is.finite(c(
        panel_row$data_xmin,
        panel_row$data_xmax,
        panel_row$data_ymin,
        panel_row$data_ymax
      )))
    if (has_data_ranges) {
      pxmin = panel_row$data_xmin
      pxmax = panel_row$data_xmax
      pymin = panel_row$data_ymin
      pymax = panel_row$data_ymax
    } else {
      pxmin = if ("extent_xmin" %in% names(panel_row)) {
        panel_row$extent_xmin
      } else {
        xmin
      }
      pxmax = if ("extent_xmax" %in% names(panel_row)) {
        panel_row$extent_xmax
      } else {
        xmax
      }
      pymin = if ("extent_ymin" %in% names(panel_row)) {
        panel_row$extent_ymin
      } else {
        ymin
      }
      pymax = if ("extent_ymax" %in% names(panel_row)) {
        panel_row$extent_ymax
      } else {
        ymax
      }
    }
    if (zaxis_location == "panelbottomleft") {
      anchor_long = pxmin
      anchor_lat = pymin
    } else if (zaxis_location == "panelbottomright") {
      anchor_long = pxmax
      anchor_lat = pymin
    } else if (zaxis_location == "paneltopleft") {
      anchor_long = pxmin
      anchor_lat = pymax
    } else if (zaxis_location == "paneltopright") {
      anchor_long = pxmax
      anchor_lat = pymax
    }
    center_long = mean(c(pxmin, pxmax))
    center_lat = mean(c(pymin, pymax))
    range_long = c(pxmin, pxmax)
    range_lat = c(pymin, pymax)
  } else if (zaxis_location == "bottomleft") {
    anchor_long = xmin
    anchor_lat = ymin
  } else if (zaxis_location == "bottomright") {
    anchor_long = xmax
    anchor_lat = ymin
  } else if (zaxis_location == "topleft") {
    anchor_long = xmin
    anchor_lat = ymax
  } else if (zaxis_location == "topright") {
    anchor_long = xmax
    anchor_lat = ymax
  }

  anchor_xyz = transform_into_heightmap_coords(
    extent = extent_vals,
    heightmap = matrix(0, nrow = 2, ncol = 2),
    lat = anchor_lat,
    long = anchor_long,
    altitude = 0,
    use_altitude = FALSE,
    zscale = zscale,
    transform_scene = FALSE
  )[1, ]
  center_xyz = transform_into_heightmap_coords(
    extent = extent_vals,
    heightmap = matrix(0, nrow = 2, ncol = 2),
    lat = center_lat,
    long = center_long,
    altitude = 0,
    use_altitude = FALSE,
    zscale = zscale,
    transform_scene = FALSE
  )[1, ]
  range_lat_vals = c(range_lat[1], range_lat[1], range_lat[2], range_lat[2])
  range_long_vals = c(
    range_long[1],
    range_long[2],
    range_long[1],
    range_long[2]
  )
  range_xyz = transform_into_heightmap_coords(
    extent = extent_vals,
    heightmap = matrix(0, nrow = 2, ncol = 2),
    lat = range_lat_vals,
    long = range_long_vals,
    altitude = 0,
    offset = rep(0, length(range_lat_vals)),
    use_altitude = FALSE,
    zscale = zscale,
    transform_scene = FALSE
  )
  range_x = range(range_xyz[, 1], finite = TRUE)
  range_z = range(range_xyz[, 3], finite = TRUE)
  scene_planar_span = max(c(diff(range_x), diff(range_z)), na.rm = TRUE)

  surface_vertices = NULL
  surface_ids = get_ids_with_labels(c("surface", "surface_tris"))$id
  if (length(surface_ids) > 0) {
    surface_vertices = lapply(
      surface_ids,
      function(id) rgl::rgl.attrib(id, "vertices")
    )
    surface_vertices = do.call("rbind", surface_vertices)
    if (!(is.matrix(surface_vertices) && nrow(surface_vertices) > 0)) {
      surface_vertices = NULL
    } else {
      valid = stats::complete.cases(surface_vertices[,
        c(1, 2, 3),
        drop = FALSE
      ])
      if (!any(valid)) {
        surface_vertices = NULL
      } else {
        surface_vertices = surface_vertices[valid, , drop = FALSE]
      }
    }
  }

  anchor_vec_2d = c(
    anchor_xyz[1] - center_xyz[1],
    anchor_xyz[3] - center_xyz[3]
  )
  anchor_vec_norm = sqrt(sum(anchor_vec_2d^2))
  if (is.finite(anchor_vec_norm) && anchor_vec_norm > 0) {
    outside_unit_2d = anchor_vec_2d / anchor_vec_norm
  } else {
    outside_unit_2d = c(1, 0)
  }
  if (!is.finite(scene_planar_span) || scene_planar_span <= 0) {
    scene_planar_span = max(1e-8, anchor_vec_norm * 2)
  }
  axis_offset = anchor_vec_norm * zaxis_corner_offset
  anchor_xyz[1] = anchor_xyz[1] + outside_unit_2d[1] * axis_offset
  anchor_xyz[3] = anchor_xyz[3] + outside_unit_2d[2] * axis_offset

  default_zaxis_labels = NULL
  zaxis_label_breaks = NULL
  if (!is.null(zaxis_data_info)) {
    if (is.null(zaxis_breaks)) {
      raw_zaxis_breaks = pretty(zaxis_data_info$raw_range, n = 4)
      raw_zaxis_breaks = raw_zaxis_breaks[is.finite(raw_zaxis_breaks)]
      if (length(raw_zaxis_breaks) < 2) {
        raw_zaxis_breaks = zaxis_data_info$raw_range
      }
    } else {
      raw_zaxis_breaks = as.numeric(zaxis_breaks)
      if (any(is.na(raw_zaxis_breaks))) {
        stop("`zaxis_breaks` must be numeric.")
      }
      raw_zaxis_breaks = sort(unique(raw_zaxis_breaks))
    }
    default_zaxis_labels = format_zaxis_labels(raw_zaxis_breaks)
    zaxis_label_breaks = raw_zaxis_breaks
    zaxis_breaks = map_zaxis_data_breaks(
      raw_zaxis_breaks,
      zaxis_data = zaxis_data_info
    )
  }
  if (is.null(zaxis_breaks)) {
    height_transform = get_scene_height_transform(
      heightmap = heightmap,
      extent = extent
    )
    if (!is.null(height_transform)) {
      height_range = suppressWarnings(as.numeric(height_transform$height_range))
      height_range = height_range[is.finite(height_range)]
      if (length(height_range) > 0) {
        height_range = range(height_range)
      }
      if (length(unique(height_range)) > 1) {
        raw_zaxis_breaks = pretty(height_range, n = 4)
        raw_zaxis_breaks = raw_zaxis_breaks[is.finite(raw_zaxis_breaks)]
        if (length(raw_zaxis_breaks) < 2) {
          raw_zaxis_breaks = height_range
        }
        default_zaxis_labels = format_zaxis_labels(raw_zaxis_breaks)
        zaxis_label_breaks = raw_zaxis_breaks
        zaxis_breaks = map_scene_altitudes(
          raw_zaxis_breaks,
          height_transform = height_transform,
          reference_values = height_range
        )
      }
    }
  }
  if (is.null(zaxis_breaks)) {
    altitude_range = c(NA_real_, NA_real_)
    if (!is.null(heightmap)) {
      height_vals = as.numeric(heightmap)
      height_vals = height_vals[is.finite(height_vals)]
      if (length(height_vals) > 0) {
        altitude_range = range(height_vals)
      }
    }
    if (any(!is.finite(altitude_range)) && !is.null(surface_vertices)) {
      altitude_range = range(surface_vertices[, 2] * zscale)
    }
    if (any(!is.finite(altitude_range))) {
      altitude_range = range(rgl::par3d()$bbox[3:4] * zscale)
    }
    if (length(unique(altitude_range)) == 1) {
      span = max(1, abs(altitude_range[1]) * 0.1)
      altitude_range = c(
        altitude_range[1] - span / 2,
        altitude_range[2] + span / 2
      )
    }
    zaxis_breaks = pretty(altitude_range, n = 4)
    if (length(zaxis_breaks) < 2) {
      zaxis_breaks = altitude_range
    }
  } else if (is.null(zaxis_data_info)) {
    zaxis_breaks = as.numeric(zaxis_breaks)
    if (any(is.na(zaxis_breaks))) {
      stop("`zaxis_breaks` must be numeric.")
    }
    zaxis_breaks = sort(unique(zaxis_breaks))
  }
  if (is.null(zaxis_label_breaks)) {
    zaxis_label_breaks = zaxis_breaks
  }

  if (is.null(zaxis_labels)) {
    if (is.null(default_zaxis_labels)) {
      zaxis_labels = format_zaxis_labels(zaxis_breaks)
    } else {
      zaxis_labels = default_zaxis_labels
    }
  } else {
    if (length(zaxis_labels) != length(zaxis_breaks)) {
      stop("`zaxis_labels` must be the same length as `zaxis_breaks`.")
    }
    zaxis_labels = as.character(zaxis_labels)
  }
  zaxis_labels = align_zaxis_nonnegative_labels(
    zaxis_labels,
    zaxis_label_breaks
  )
  if (identical(zaxis_title, FALSE)) {
    zaxis_title = NULL
  } else if (is.null(zaxis_title)) {
    zaxis_title = NULL
  } else if (
    {
      zaxis_title_value = as.character(zaxis_title)
      length(zaxis_title_value) == 1 &&
        !is.na(zaxis_title_value) &&
        tolower(zaxis_title_value) == "auto"
    }
  ) {
    transform_info = get_cached_plot_gg_transform_info(
      heightmap = heightmap,
      default = NULL
    )
    zaxis_title = if (
      !is.null(transform_info) &&
        isTRUE(transform_info$height_is_mapped) &&
        !is.null(transform_info$height_label)
    ) {
      transform_info$height_label
    } else if (!is.null(zaxis_data_info) && !is.null(zaxis_data_info$label)) {
      zaxis_data_info$label
    } else if (zaxis_data_source == "topographic") {
      "Elevation"
    } else {
      NULL
    }
  } else {
    zaxis_title = paste(as.character(zaxis_title), collapse = " ")
  }
  if (!is.null(zaxis_title)) {
    zaxis_title = gsub("[[:space:]]+", " ", trimws(as.character(zaxis_title)))
    if (!nzchar(zaxis_title) || is.na(zaxis_title)) {
      zaxis_title = NULL
    }
  }

  y_vals = zaxis_breaks / zscale
  y_min = min(y_vals)
  y_max = max(y_vals)
  if (length(y_vals) == 1 || identical(y_min, y_max)) {
    y_max = y_min + 1 / zscale
  }
  eps_break = .Machine$double.eps^0.5
  nonzero_idx = abs(zaxis_breaks) > eps_break
  # Panel inset axes can intersect the surface at zero; keep zero hidden there.
  # For non-panel/corner-offset axes, include zero markers/labels.
  show_zero = !grepl("^panel", zaxis_location)
  draw_idx = nonzero_idx | show_zero

  tick_len = 0.03 * scene_planar_span
  tick_marker_size = if (is.null(zaxis_tick_size)) {
    max(4, zaxis_linewidth * 1.25)
  } else {
    zaxis_tick_size
  }
  label_unit = resolve_zaxis_text_side_unit(
    side = zaxis_label_side,
    auto_unit = outside_unit_2d,
    anchor_xyz = anchor_xyz,
    reference_y = mean(c(y_min, y_max)),
    scale = tick_len
  )
  label_screen_side = resolve_zaxis_text_side_name(
    side = zaxis_label_side,
    unit = label_unit,
    anchor_xyz = anchor_xyz,
    reference_y = mean(c(y_min, y_max)),
    scale = tick_len,
    fallback_unit = outside_unit_2d
  )
  # Keep text extending away from the axis side instead of centered on the anchor point.
  text_adj_x = if (identical(label_screen_side, "right")) 0 else 1
  # Extra whitespace gives a reliable visual gap from the axis in billboarded text mode.
  space_pad = "  "
  text_labels = if (text_adj_x == 1) {
    paste0(zaxis_labels, space_pad)
  } else {
    paste0(space_pad, zaxis_labels)
  }

  rgl::segments3d(
    x = c(anchor_xyz[1], anchor_xyz[1]),
    y = c(y_min, y_max),
    z = c(anchor_xyz[3], anchor_xyz[3]),
    color = zaxis_color,
    lwd = zaxis_linewidth,
    tag = "zaxis_axis"
  )

  if (any(draw_idx)) {
    rgl::points3d(
      x = rep(anchor_xyz[1], sum(draw_idx)),
      y = y_vals[draw_idx],
      z = rep(anchor_xyz[3], sum(draw_idx)),
      color = zaxis_color,
      size = tick_marker_size,
      tag = "zaxis_ticks"
    )
  }

  for (i in which(draw_idx)) {
    text_x = anchor_xyz[1] + label_unit[1] * tick_len * zaxis_text_offset
    text_z = anchor_xyz[3] + label_unit[2] * tick_len * zaxis_text_offset
    rgl::texts3d(
      x = text_x,
      y = y_vals[i],
      z = text_z,
      texts = text_labels[i],
      color = zaxis_color,
      adj = c(text_adj_x, 0.5),
      cex = zaxis_label_size,
      tag = "zaxis_labels"
    )
  }

  if (!is.null(zaxis_title)) {
    if (identical(zaxis_title_side, "auto")) {
      title_unit = if (identical(zaxis_title_location, "side")) {
        -label_unit
      } else {
        label_unit
      }
      title_screen_side = resolve_zaxis_text_side_name(
        side = "auto",
        unit = title_unit,
        anchor_xyz = anchor_xyz,
        reference_y = mean(c(y_min, y_max)),
        scale = tick_len,
        fallback_unit = title_unit
      )
    } else {
      title_unit = resolve_zaxis_text_side_unit(
        side = zaxis_title_side,
        auto_unit = label_unit,
        anchor_xyz = anchor_xyz,
        reference_y = mean(c(y_min, y_max)),
        scale = tick_len
      )
      title_screen_side = zaxis_title_side
    }
    title_text_adj_x = if (identical(title_screen_side, "right")) 0 else 1
    if (zaxis_title_location == "side") {
      title_offset = max(zaxis_title_offset, zaxis_text_offset + 1)
      title_x = anchor_xyz[1] + title_unit[1] * tick_len * title_offset
      title_y = mean(c(y_min, y_max))
      title_z = anchor_xyz[3] + title_unit[2] * tick_len * title_offset
      title_adj = c(title_text_adj_x, 0.5)
      rgl::texts3d(
        x = title_x,
        y = title_y,
        z = title_z,
        texts = zaxis_title,
        color = zaxis_color,
        adj = title_adj,
        cex = zaxis_title_size,
        tag = "zaxis_title"
      )
    } else {
      title_side_offset = max(0.5, zaxis_text_offset)
      title_x = anchor_xyz[1] + title_unit[1] * tick_len * title_side_offset
      title_y = y_max + tick_len * zaxis_title_offset
      title_z = anchor_xyz[3] + title_unit[2] * tick_len * title_side_offset
      title_adj = c(title_text_adj_x, 0)
      rgl::texts3d(
        x = title_x,
        y = title_y,
        z = title_z,
        texts = zaxis_title,
        color = zaxis_color,
        adj = title_adj,
        cex = zaxis_title_size,
        tag = "zaxis_title"
      )
    }
  }

  invisible(
    list(
      location = zaxis_location,
      breaks = zaxis_breaks,
      labels = zaxis_labels,
      title = zaxis_title,
      title_location = zaxis_title_location,
      label_side = label_screen_side,
      title_side = if (!is.null(zaxis_title)) title_screen_side else NULL,
      data = zaxis_data_source
    )
  )
}
