#'@keywords internal
#'@noRd
get_ggplot_extent = function(heightmap = NULL, panel = NULL) {
  panel_info = NULL
  if (!is.null(heightmap)) {
    panel_info = attr(heightmap, "ggplot_panel_info", exact = TRUE)
  }
  if (is.null(panel_info)) {
    panel_info = get_scene_context_value("plot_gg_panel_info", default = NULL)
  }
  if (is.null(panel_info) || !nrow(panel_info)) {
    stop("No cached ggplot extent found. Call `plot_gg()` first.")
  }

  build_extent = function(info_row) {
    extent = c(
      xmin = info_row$extent_xmin,
      xmax = info_row$extent_xmax,
      ymin = info_row$extent_ymin,
      ymax = info_row$extent_ymax
    )
    attr(extent, "panel_info") = info_row
    extent
  }

  if (!is.null(panel)) {
    match_index = match(panel, panel_info$panel)
    if (is.na(match_index)) {
      stop(sprintf(
        "Could not find panel `%s`. Available panels: %s",
        as.character(panel),
        paste(panel_info$panel, collapse = ", ")
      ))
    }
    return(build_extent(panel_info[match_index, , drop = FALSE]))
  }

  if (nrow(panel_info) == 1) {
    return(build_extent(panel_info[1, , drop = FALSE]))
  }

  extents = lapply(seq_len(nrow(panel_info)), function(i) {
    build_extent(panel_info[i, , drop = FALSE])
  })
  names(extents) = paste0("panel_", panel_info$panel)
  attr(extents, "panel_info") = panel_info
  extents
}

get_plot_gg_transform_info = function(heightmap = NULL) {
  transform_info = NULL
  if (!is.null(heightmap)) {
    transform_info = attr(heightmap, "ggplot_transform_info", exact = TRUE)
  }
  if (is.null(transform_info)) {
    transform_info = get_scene_context_value(
      "plot_gg_transform_info",
      default = NULL
    )
  }
  if (is.null(transform_info)) {
    stop("No cached ggplot transform found. Call `plot_gg()` first.")
  }
  transform_info
}

get_cached_plot_gg_panel_info = function(heightmap = NULL, default = NULL) {
  panel_info = NULL
  if (!is.null(heightmap)) {
    panel_info = attr(heightmap, "ggplot_panel_info", exact = TRUE)
  }
  if (is.null(panel_info)) {
    panel_info = get_scene_context_value("plot_gg_panel_info", default = NULL)
  }
  if (is.null(panel_info)) {
    return(default)
  }
  panel_info
}

get_cached_plot_gg_transform_info = function(heightmap = NULL, default = NULL) {
  tryCatch(
    get_plot_gg_transform_info(heightmap = heightmap),
    error = function(e) default
  )
}

format_render_caller_prefix = function(caller = NULL) {
  if (!is.null(caller) && nzchar(caller)) {
    return(sprintf("%s(): ", caller))
  }
  ""
}

format_faceted_ggplot_panel_error = function(caller = NULL) {
  paste0(
    format_render_caller_prefix(caller),
    "This scene was created from a faceted ggplot. Supply `panel = <panel>` or provide an `extent` corresponding to a single panel."
  )
}

format_raybevel_error = function(error, caller = NULL) {
  paste0(
    format_render_caller_prefix(caller),
    conditionMessage(error)
  )
}

is_panel_extent_list = function(extent) {
  is.list(extent) && !is.data.frame(extent)
}

normalize_scene_resolved_extent = function(extent, caller = NULL) {
  if (!is_panel_extent_list(extent)) {
    return(extent)
  }
  if (length(extent) == 0) {
    return(NULL)
  }
  if (length(extent) == 1) {
    return(extent[[1]])
  }
  stop(format_faceted_ggplot_panel_error(caller), call. = FALSE)
}

validate_scene_extent_panel = function(
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  caller = NULL
) {
  if (is.null(extent) || is.null(panel)) {
    return(extent)
  }
  panel_info = get_cached_plot_gg_panel_info(
    heightmap = heightmap,
    default = NULL
  )
  if (is.null(panel_info) || !nrow(panel_info)) {
    return(extent)
  }
  extent_panel = match_plot_gg_panel_from_extent(extent, panel_info)
  if (
    !is.null(extent_panel) &&
      !identical(as.character(extent_panel), as.character(panel))
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "The supplied `panel` and `extent` refer to different facet panels."
      ),
      call. = FALSE
    )
  }
  extent
}

resolve_heightmap_extent = function(heightmap = NULL) {
  if (is.null(heightmap)) {
    return(NULL)
  }
  heightmap_extent = tryCatch(get_extent(heightmap), error = function(e) NULL)
  if (!is.null(heightmap_extent)) {
    return(heightmap_extent)
  }
  heightmap_extent = attr(heightmap, "extent", exact = TRUE)
  if (!is.null(heightmap_extent)) {
    return(tryCatch(get_extent(heightmap_extent), error = function(e) NULL))
  }
  NULL
}


get_scene_data_filter_extent = function(
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  caller = NULL
) {
  if (is.null(extent)) {
    extent = resolve_scene_render_extent(
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      caller = caller,
      error_if_missing = FALSE
    )
  }
  if (is.null(extent)) {
    return(NULL)
  }
  extent = normalize_scene_resolved_extent(extent, caller = caller)
  extent = validate_scene_extent_panel(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    caller = caller
  )
  panel_info = attr(extent, "panel_info", exact = TRUE)
  panel_extent_cols = c("data_xmin", "data_xmax", "data_ymin", "data_ymax")
  if (
    is.data.frame(panel_info) &&
      nrow(panel_info) > 0 &&
      all(panel_extent_cols %in% names(panel_info))
  ) {
    filter_extent = c(
      xmin = panel_info$data_xmin[1],
      xmax = panel_info$data_xmax[1],
      ymin = panel_info$data_ymin[1],
      ymax = panel_info$data_ymax[1]
    )
    if (all(is.finite(filter_extent))) {
      return(filter_extent)
    }
  }
  tryCatch(
    get_extent(extent),
    error = function(e) NULL
  )
}

filter_scene_xy_to_extent = function(
  x,
  y,
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  filter_to_extent = TRUE,
  caller = NULL
) {
  validate_filter_to_extent(filter_to_extent, caller = caller)
  keep_all = rep(TRUE, length(x))
  if (!isTRUE(filter_to_extent)) {
    return(list(x = x, y = y, keep = keep_all, extent = NULL))
  }
  if (is.null(x) || is.null(y)) {
    return(list(x = x, y = y, keep = keep_all, extent = NULL))
  }
  filter_extent = get_scene_data_filter_extent(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    caller = caller
  )
  if (is.null(filter_extent)) {
    return(list(x = x, y = y, keep = keep_all, extent = NULL))
  }
  x_num = suppressWarnings(as.numeric(x))
  y_num = suppressWarnings(as.numeric(y))
  keep = is.finite(x_num) &
    is.finite(y_num) &
    x_num >= filter_extent["xmin"] &
    x_num <= filter_extent["xmax"] &
    y_num >= filter_extent["ymin"] &
    y_num <= filter_extent["ymax"]
  list(
    x = x[keep],
    y = y[keep],
    keep = keep,
    extent = filter_extent
  )
}

subset_render_arg = function(value, keep, n_expected) {
  if (is.null(value) || is.null(keep) || length(keep) != n_expected) {
    return(value)
  }
  if (inherits(value, "matrix") || inherits(value, "data.frame")) {
    if (nrow(value) == n_expected) {
      return(value[keep, , drop = FALSE])
    }
    return(value)
  }
  if (is.list(value) && length(value) == n_expected) {
    return(value[keep])
  }
  if (length(value) == n_expected) {
    return(value[keep])
  }
  value
}

subset_render_row_arg = function(value, keep, n_expected) {
  if (is.null(value) || is.null(keep) || length(keep) != n_expected) {
    return(value)
  }
  if (inherits(value, "matrix") || inherits(value, "data.frame")) {
    if (nrow(value) == n_expected) {
      return(value[keep, , drop = FALSE])
    }
    return(value)
  }
  if (is.list(value) && length(value) == n_expected) {
    return(value[keep])
  }
  value
}

subset_render_color_arg = function(value, keep, n_expected) {
  if (is.numeric(value) && length(value) == 3) {
    return(value)
  }
  subset_render_arg(value, keep, n_expected)
}

subset_render_arg_by_index = function(value, index, n_expected) {
  if (is.null(value) || is.null(index)) {
    return(value)
  }
  if (inherits(value, "matrix") || inherits(value, "data.frame")) {
    if (nrow(value) == n_expected) {
      return(value[index, , drop = FALSE])
    }
    return(value)
  }
  if (is.list(value) && length(value) == n_expected) {
    return(value[index])
  }
  if (length(value) == n_expected) {
    return(value[index])
  }
  value
}

is_empty_scene_sf = function(sf_object) {
  if (inherits(sf_object, "sf")) {
    return(nrow(sf_object) == 0)
  }
  if (inherits(sf_object, "sfc")) {
    return(length(sf_object) == 0)
  }
  if (inherits(sf_object, "sfg")) {
    return(FALSE)
  }
  FALSE
}

filter_scene_sf_to_extent = function(
  sf_object,
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  filter_to_extent = TRUE,
  preserve_z = FALSE,
  caller = NULL
) {
  validate_filter_to_extent(filter_to_extent, caller = caller)
  if (!isTRUE(filter_to_extent)) {
    return(list(object = sf_object, source_index = NULL, extent = NULL))
  }
  filter_extent = get_scene_data_filter_extent(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    caller = caller
  )
  if (is.null(filter_extent)) {
    return(list(object = sf_object, source_index = NULL, extent = NULL))
  }
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    return(list(
      object = sf_object,
      source_index = NULL,
      extent = filter_extent
    ))
  }
  coerced_input = coerce_scene_sf_input(sf_object)
  sf_data = coerced_input$sf_data
  if (!nrow(sf_data)) {
    return(list(
      object = rebuild_scene_sf_output(sf_data, coerced_input$input_class),
      source_index = integer(),
      extent = filter_extent
    ))
  }
  source_index_col = ".rayshader_source_index"
  while (source_index_col %in% names(sf_data)) {
    source_index_col = paste0(source_index_col, "_")
  }
  sf_data[[source_index_col]] = seq_len(nrow(sf_data))
  crop_bbox = sf::st_bbox(
    c(
      xmin = unname(filter_extent["xmin"]),
      ymin = unname(filter_extent["ymin"]),
      xmax = unname(filter_extent["xmax"]),
      ymax = unname(filter_extent["ymax"])
    ),
    crs = sf::st_crs(sf_data)
  )
  if (isTRUE(preserve_z) && "Z" %in% colnames(sf::st_coordinates(sf_data))) {
    bbox_sfc = sf::st_as_sfc(crop_bbox)
    intersects_extent = lengths(sf::st_intersects(sf_data, bbox_sfc)) > 0
    cropped = sf_data[intersects_extent, , drop = FALSE]
  } else {
    cropped = suppressMessages(suppressWarnings(sf::st_crop(
      sf_data,
      crop_bbox
    )))
  }
  if (nrow(cropped)) {
    cropped = cropped[!sf::st_is_empty(cropped), , drop = FALSE]
  }
  source_index = if (nrow(cropped)) {
    cropped[[source_index_col]]
  } else {
    integer()
  }
  cropped[[source_index_col]] = NULL
  out = rebuild_scene_sf_output(cropped, coerced_input$input_class)
  if (!is.null(extent)) {
    attr(out, "extent") = extent
  }
  panel_attr = tryCatch(
    attr(sf_object, "panel", exact = TRUE),
    error = function(e) NULL
  )
  if (!is.null(panel_attr)) {
    attr(out, "panel") = panel_attr
  }
  list(
    object = out,
    source_index = source_index,
    extent = filter_extent
  )
}

get_cached_ggplot_extent_or_null = function(heightmap = NULL, panel = NULL) {
  tryCatch(
    get_ggplot_extent(heightmap = heightmap, panel = panel),
    error = function(e) {
      if (startsWith(conditionMessage(e), "No cached ggplot extent found.")) {
        return(NULL)
      }
      stop(e)
    }
  )
}

match_plot_gg_panel_from_extent = function(extent, panel_info) {
  if (is.null(extent) || is.null(panel_info) || !nrow(panel_info)) {
    return(NULL)
  }
  extent_panel_info = attr(extent, "panel_info", exact = TRUE)
  if (
    is.data.frame(extent_panel_info) &&
      nrow(extent_panel_info) &&
      "panel" %in% colnames(extent_panel_info)
  ) {
    return(extent_panel_info$panel[1])
  }
  extent_vals = tryCatch(
    get_extent(extent),
    error = function(e) NULL
  )
  if (is.null(extent_vals)) {
    return(NULL)
  }
  panel_matches = which(vapply(
    seq_len(nrow(panel_info)),
    function(i) {
      cached_extent = c(
        xmin = panel_info$extent_xmin[i],
        xmax = panel_info$extent_xmax[i],
        ymin = panel_info$extent_ymin[i],
        ymax = panel_info$extent_ymax[i]
      )
      isTRUE(all.equal(
        as.numeric(extent_vals),
        as.numeric(cached_extent),
        tolerance = 1e-8
      ))
    },
    logical(1)
  ))
  if (length(panel_matches) == 1) {
    return(panel_info$panel[panel_matches])
  }
  NULL
}

infer_plot_gg_panel = function(
  extent = NULL,
  heightmap = NULL,
  transform_info = NULL
) {
  transform_info = get_cached_plot_gg_transform_info(
    heightmap = heightmap,
    default = transform_info
  )
  if (is.null(transform_info)) {
    return(NULL)
  }
  panel_info = get_cached_plot_gg_panel_info(
    heightmap = heightmap,
    default = NULL
  )
  panel = match_plot_gg_panel_from_extent(extent, panel_info)
  if (!is.null(panel)) {
    return(panel)
  }
  panel_table = transform_info$layout
  if (!is.null(panel_table) && nrow(panel_table) == 1) {
    return(panel_table$panel[1])
  }
  NULL
}

canonicalize_plot_gg_extent = function(extent, heightmap = NULL) {
  transform_info = get_cached_plot_gg_transform_info(
    heightmap = heightmap,
    default = NULL
  )
  if (is.null(transform_info)) {
    return(extent)
  }
  panel = infer_plot_gg_panel(
    extent = extent,
    heightmap = heightmap,
    transform_info = transform_info
  )
  if (is.null(panel)) {
    return(extent)
  }
  get_ggplot_extent(heightmap = heightmap, panel = panel)
}

get_scene_transform_context = function(
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  error_if_missing = FALSE,
  caller = NULL
) {
  transform_info = get_cached_plot_gg_transform_info(
    heightmap = heightmap,
    default = NULL
  )
  if (is.null(transform_info)) {
    return(NULL)
  }
  extent = normalize_scene_resolved_extent(extent, caller = caller)
  extent = validate_scene_extent_panel(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    caller = caller
  )
  if (is.null(panel)) {
    panel = infer_plot_gg_panel(
      extent = extent,
      heightmap = heightmap,
      transform_info = transform_info
    )
  }
  if (is.null(panel)) {
    if (isTRUE(error_if_missing)) {
      stop(format_faceted_ggplot_panel_error(caller), call. = FALSE)
    }
    return(NULL)
  }
  get_plot_gg_panel_transform_context(
    transform_info = transform_info,
    panel = panel,
    heightmap = heightmap,
    caller = caller
  )
}

auto_transform_scene_xy = function(
  x,
  y,
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  crs = NULL,
  caller = NULL
) {
  transform_context = get_scene_transform_context(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    error_if_missing = TRUE,
    caller = caller
  )
  if (is.null(transform_context)) {
    if (is.null(crs)) {
      return(list(
        x = x,
        y = y,
        extent = extent,
        panel = NULL,
        transformed = FALSE
      ))
    }
    target_crs = get_scene_target_crs(
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      caller = caller
    )
    if (is.null(target_crs)) {
      return(list(
        x = x,
        y = y,
        extent = extent,
        panel = NULL,
        transformed = FALSE
      ))
    }
    transformed_xy = transform_xy_between_crs(
      x_vals = x,
      y_vals = y,
      source_crs = crs,
      target_crs = target_crs,
      caller = caller
    )
    return(list(
      x = transformed_xy$x,
      y = transformed_xy$y,
      extent = extent,
      panel = NULL,
      transformed = transformed_xy$transformed
    ))
  }
  transformed_coords = transform_ggplot_xy_with_context(
    x_vals = x,
    y_vals = y,
    transform_context = transform_context,
    crs = crs
  )
  resolved_extent = if (!is.null(extent)) {
    extent
  } else {
    transform_context$transformed_extent
  }
  list(
    x = transformed_coords$long,
    y = transformed_coords$lat,
    extent = resolved_extent,
    panel = transform_context$panel,
    transformed = TRUE
  )
}

auto_transform_scene_sf = function(
  sf_object,
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  crs = NULL,
  segmentize_df_max_length = NULL,
  caller = NULL
) {
  transform_context = get_scene_transform_context(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    error_if_missing = TRUE,
    caller = caller
  )
  if (is.null(transform_context)) {
    target_crs = get_scene_target_crs(
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      caller = caller
    )
    if (is.null(target_crs)) {
      return(list(
        object = sf_object,
        extent = extent,
        panel = NULL,
        transformed = FALSE
      ))
    }
    transformed_object = transform_scene_sf_to_target_crs(
      sf_object = sf_object,
      target_crs = target_crs,
      crs = crs,
      caller = caller
    )
    return(list(
      object = transformed_object$object,
      extent = extent,
      panel = NULL,
      transformed = transformed_object$transformed
    ))
  }
  transformed_object = transform_ggplot_sf(
    sf_object = sf_object,
    panel = transform_context$panel,
    heightmap = heightmap,
    crs = crs,
    segmentize_df_max_length = segmentize_df_max_length
  )
  transformed_extent = if (!is.null(extent)) {
    extent
  } else {
    attr(transformed_object, "extent", exact = TRUE)
  }
  if (is.null(transformed_extent)) {
    transformed_extent = transform_context$transformed_extent
  }
  attr(transformed_object, "extent") = transformed_extent
  list(
    object = transformed_object,
    extent = transformed_extent,
    panel = transform_context$panel,
    transformed = TRUE
  )
}

resolve_render_xy_aliases = function(
  x = NULL,
  y = NULL,
  long = NULL,
  lat = NULL,
  missing_x = TRUE,
  missing_y = TRUE,
  missing_long = TRUE,
  missing_lat = TRUE,
  caller = NULL
) {
  caller_prefix = if (!is.null(caller) && nzchar(caller)) {
    sprintf("%s(): ", caller)
  } else {
    ""
  }
  has_x = !isTRUE(missing_x) && !is.null(x)
  has_y = !isTRUE(missing_y) && !is.null(y)
  has_long = !isTRUE(missing_long) && !is.null(long)
  has_lat = !isTRUE(missing_lat) && !is.null(lat)
  if (has_x && has_long) {
    stop(
      sprintf("%sUse only one of `x` or `long`.", caller_prefix),
      call. = FALSE
    )
  }
  if (has_y && has_lat) {
    stop(
      sprintf("%sUse only one of `y` or `lat`.", caller_prefix),
      call. = FALSE
    )
  }
  list(
    x = if (has_long) long else x,
    y = if (has_lat) lat else y,
    source_crs = if (has_long && has_lat) 4326 else NULL
  )
}

# Accept POINT and MULTIPOINT spatial inputs for renderer `location=` arguments.
# MULTIPOINT features are flattened to per-point placements while preserving
# feature order, so callers can distinguish original feature count from the
# final number of point placements via `feature_count` and `geometry_count`.
coerce_scene_point_input = function(location, crs = NULL, caller = NULL) {
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`sf` package required for spatial point `location` inputs."
      ),
      call. = FALSE
    )
  }
  coerced_input = coerce_scene_sf_input(location)
  sf_data = coerced_input$sf_data
  if (!nrow(sf_data)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`location` cannot be empty."
      ),
      call. = FALSE
    )
  }
  if (any(sf::st_is_empty(sf_data))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`location` cannot contain empty geometries."
      ),
      call. = FALSE
    )
  }
  geometry_types = as.character(sf::st_geometry_type(
    sf_data,
    by_geometry = TRUE
  ))
  if (any(!geometry_types %in% c("POINT", "MULTIPOINT"))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`location` must contain only POINT or MULTIPOINT geometries."
      ),
      call. = FALSE
    )
  }
  resolved_input = resolve_scene_sf_source_crs(
    sf_data = sf_data,
    crs = crs,
    caller = caller
  )
  sf_data = resolved_input$sf_data
  point_sf_data = suppressWarnings(sf::st_cast(sf_data, "POINT", warn = FALSE))
  if (!nrow(point_sf_data)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`location` must resolve to at least one point."
      ),
      call. = FALSE
    )
  }
  point_coords = sf::st_coordinates(point_sf_data)
  list(
    sf_data = sf_data,
    point_sf_data = point_sf_data,
    x = point_coords[, 1],
    y = point_coords[, 2],
    feature_count = nrow(sf_data),
    geometry_count = nrow(point_sf_data),
    source_crs = resolved_input$source_crs
  )
}

# Transform spatial point `location=` inputs into active scene coordinates.
# The returned `x`/`y` vectors always reflect flattened point placements in the
# scene CRS or ggplot panel coordinate space expected by the renderers.
extract_scene_point_xy = function(
  location,
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  crs = NULL,
  caller = NULL
) {
  coerced_points = coerce_scene_point_input(
    location = location,
    crs = crs,
    caller = caller
  )
  scene_points = auto_transform_scene_sf(
    sf_object = coerced_points$sf_data,
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    caller = caller
  )
  scene_point_data = coerce_scene_point_input(
    location = scene_points$object,
    caller = caller
  )
  list(
    sf_data = scene_point_data$sf_data,
    point_sf_data = scene_point_data$point_sf_data,
    x = scene_point_data$x,
    y = scene_point_data$y,
    feature_count = coerced_points$feature_count,
    geometry_count = coerced_points$geometry_count,
    source_crs = coerced_points$source_crs,
    extent = scene_points$extent,
    panel = scene_points$panel,
    transformed = scene_points$transformed
  )
}

# Resolve renderer point placement inputs from either spatial `location=`
# objects or explicit numeric x/y (and lat/long aliases). Spatial locations
# remain centralized here so renderer-specific code can reuse the same POINT /
# MULTIPOINT validation and scene-transform path.
resolve_render_location_input = function(
  location = NULL,
  x = NULL,
  y = NULL,
  long = NULL,
  lat = NULL,
  missing_x = TRUE,
  missing_y = TRUE,
  missing_long = TRUE,
  missing_lat = TRUE,
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  crs = NULL,
  caller = NULL
) {
  if (!is.null(location)) {
    conflicting_args = character()
    if (!isTRUE(missing_x) && !is.null(x)) {
      conflicting_args = c(conflicting_args, "x")
    }
    if (!isTRUE(missing_y) && !is.null(y)) {
      conflicting_args = c(conflicting_args, "y")
    }
    if (!isTRUE(missing_long) && !is.null(long)) {
      conflicting_args = c(conflicting_args, "long")
    }
    if (!isTRUE(missing_lat) && !is.null(lat)) {
      conflicting_args = c(conflicting_args, "lat")
    }
    if (length(conflicting_args)) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`location` cannot be combined with `",
          paste(conflicting_args, collapse = "`, `"),
          "`."
        ),
        call. = FALSE
      )
    }
    point_input = extract_scene_point_xy(
      location = location,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      crs = crs,
      caller = caller
    )
    point_input$location_supplied = TRUE
    return(point_input)
  }
  xy_inputs = resolve_render_xy_aliases(
    x = x,
    y = y,
    long = long,
    lat = lat,
    missing_x = missing_x,
    missing_y = missing_y,
    missing_long = missing_long,
    missing_lat = missing_lat,
    caller = caller
  )
  list(
    x = xy_inputs$x,
    y = xy_inputs$y,
    extent = extent,
    panel = panel,
    feature_count = NULL,
    geometry_count = NULL,
    source_crs = xy_inputs$source_crs,
    transformed = FALSE,
    location_supplied = FALSE
  )
}

#'@keywords internal
#'@noRd
transform_ggplot_coords = function(
  x,
  y,
  panel = NULL,
  heightmap = NULL,
  crs = NULL
) {
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.")
  }
  transform_info = get_plot_gg_transform_info(heightmap = heightmap)
  transform_context = get_plot_gg_panel_transform_context(
    transform_info = transform_info,
    panel = panel,
    heightmap = heightmap
  )
  transformed_coords = transform_ggplot_xy_with_context(
    x_vals = x,
    y_vals = y,
    transform_context = transform_context,
    crs = crs
  )
  attr(transformed_coords, "extent") = transform_context$transformed_extent
  attr(transformed_coords, "panel") = transform_context$panel
  transformed_coords
}

get_plot_gg_panel_transform_context = function(
  transform_info,
  panel = NULL,
  heightmap = NULL,
  caller = NULL
) {
  panel_table = transform_info$layout
  if (!nrow(panel_table)) {
    stop("No ggplot panel transformation metadata found.")
  }
  if (is.null(panel)) {
    if (nrow(panel_table) != 1) {
      stop(format_faceted_ggplot_panel_error(caller), call. = FALSE)
    }
    panel = panel_table$panel[1]
  }
  panel_index = match(panel, panel_table$panel)
  if (is.na(panel_index)) {
    stop(sprintf(
      "Could not find panel `%s`. Available panels: %s",
      as.character(panel),
      paste(panel_table$panel, collapse = ", ")
    ))
  }
  panel_params = transform_info$panel_params[[panel_index]]
  transformed_extent = get_ggplot_extent(
    heightmap = heightmap,
    panel = panel
  )
  list(
    panel = panel,
    panel_index = panel_index,
    panel_table = panel_table,
    panel_params = panel_params,
    coord_obj = transform_info$coord,
    panel_scales_x = transform_info$panel_scales_x,
    panel_scales_y = transform_info$panel_scales_y,
    transformed_extent = transformed_extent,
    panel_extent_info = attr(transformed_extent, "panel_info")
  )
}

map_from_panel_npc = function(vals, target_range) {
  vals = as.numeric(vals)
  if (length(target_range) != 2 || any(!is.finite(target_range))) {
    return(vals)
  }
  if (diff(target_range) == 0) {
    return(rep(target_range[1], length(vals)))
  }
  target_range[1] + vals * diff(target_range)
}

fast_scene_data_frame = function(values) {
  structure(
    values,
    names = names(values),
    row.names = .set_row_names(length(values[[1]])),
    class = "data.frame"
  )
}

get_coord_sf_target_crs = function(panel_params) {
  target_crs = sf::st_crs(panel_params$crs)
  if (is.na(target_crs)) {
    target_crs = sf::st_crs(panel_params$default_crs)
  }
  if (is.na(target_crs)) {
    stop("Could not determine target CRS for this `coord_sf()` panel.")
  }
  target_crs
}

scene_crs_equal = function(x, y) {
  x_crs = suppressWarnings(tryCatch(sf::st_crs(x), error = function(e) NA))
  y_crs = suppressWarnings(tryCatch(sf::st_crs(y), error = function(e) NA))
  if (is.na(x_crs) || is.na(y_crs)) {
    return(FALSE)
  }
  identical(x_crs$wkt, y_crs$wkt)
}

try_parse_scene_crs = function(crs) {
  if (is.null(crs)) {
    return(NULL)
  }
  crs_candidates = list()
  add_candidate = function(candidate) {
    if (is.null(candidate) || length(candidate) == 0) {
      return(invisible(NULL))
    }
    if (is.character(candidate)) {
      candidate = trimws(candidate[1])
      if (!nzchar(candidate) || identical(candidate, "NA")) {
        return(invisible(NULL))
      }
    }
    crs_candidates[[length(crs_candidates) + 1]] <<- candidate
    invisible(NULL)
  }
  add_candidate(crs)
  if (inherits(crs, "CRS")) {
    add_candidate(tryCatch(comment(crs), error = function(e) NULL))
    add_candidate(tryCatch(methods::slot(crs, "projargs"), error = function(e) {
      NULL
    }))
    add_candidate(tryCatch(as.character(crs), error = function(e) NULL))
  }
  for (candidate in crs_candidates) {
    parsed_crs = suppressWarnings(tryCatch(
      sf::st_crs(candidate),
      error = function(e) {
        NULL
      }
    ))
    if (!is.null(parsed_crs) && !is.na(parsed_crs)) {
      return(parsed_crs)
    }
  }
  NULL
}

parse_scene_crs = function(crs, caller = NULL, arg_name = "crs") {
  parsed_crs = try_parse_scene_crs(crs)
  if (is.null(parsed_crs) || is.na(parsed_crs)) {
    stop(
      sprintf(
        "%sCould not interpret `%s`.",
        format_render_caller_prefix(caller),
        arg_name
      ),
      call. = FALSE
    )
  }
  parsed_crs
}

transform_xy_between_crs = function(
  x_vals,
  y_vals,
  source_crs,
  target_crs,
  caller = NULL
) {
  if (length(x_vals) != length(y_vals)) {
    stop("`x_vals` and `y_vals` must have the same length.")
  }
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop("`sf` package required for CRS transforms.", call. = FALSE)
  }
  source_crs = parse_scene_crs(source_crs, caller = caller, arg_name = "crs")
  target_crs = parse_scene_crs(
    target_crs,
    caller = caller,
    arg_name = "target CRS"
  )
  if (scene_crs_equal(source_crs, target_crs)) {
    return(list(
      x = as.numeric(x_vals),
      y = as.numeric(y_vals),
      transformed = FALSE
    ))
  }
  point_sf = sf::st_as_sf(
    data.frame(x = as.numeric(x_vals), y = as.numeric(y_vals)),
    coords = c("x", "y"),
    crs = source_crs
  )
  point_sf = sf::st_transform(point_sf, target_crs)
  point_coords = sf::st_coordinates(point_sf)
  list(
    x = point_coords[, 1],
    y = point_coords[, 2],
    transformed = TRUE
  )
}

coerce_scene_sf_input = function(sf_object) {
  input_class = if (inherits(sf_object, "sf")) {
    "sf"
  } else if (inherits(sf_object, "sfc")) {
    "sfc"
  } else if (inherits(sf_object, "sfg")) {
    "sfg"
  } else if (inherits(sf_object, "Spatial")) {
    "sp"
  } else {
    stop(
      "`sf_object` must be an `sf`, `sfc`, `sfg`, or `sp` spatial object.",
      call. = FALSE
    )
  }
  sf_data = if (input_class == "sf") {
    sf_object
  } else if (input_class == "sfc") {
    sf::st_sf(geometry = sf_object)
  } else if (input_class == "sfg") {
    sf::st_sf(geometry = sf::st_sfc(sf_object))
  } else {
    sf::st_as_sf(sf_object)
  }
  list(
    sf_data = sf_data,
    input_class = input_class
  )
}

rebuild_scene_sf_output = function(sf_data, input_class) {
  if (input_class %in% c("sf", "sp")) {
    return(sf_data)
  }
  if (input_class == "sfc") {
    return(sf::st_geometry(sf_data))
  }
  if (input_class == "sfg") {
    return(sf::st_geometry(sf_data)[[1]])
  }
  stop("Unsupported spatial input class.", call. = FALSE)
}

resolve_scene_sf_source_crs = function(
  sf_data,
  crs = NULL,
  target_crs = NULL,
  caller = NULL
) {
  explicit_crs = if (!is.null(crs)) {
    parse_scene_crs(crs, caller = caller, arg_name = "crs")
  } else {
    NULL
  }
  existing_crs = suppressWarnings(sf::st_crs(sf_data))
  has_existing_crs = !is.na(existing_crs)
  if (
    !is.null(explicit_crs) &&
      has_existing_crs &&
      !scene_crs_equal(existing_crs, explicit_crs)
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Input spatial data already has a CRS that conflicts with `crs`."
      ),
      call. = FALSE
    )
  }
  if (!has_existing_crs && !is.null(explicit_crs)) {
    sf_data = suppressWarnings(sf::st_set_crs(sf_data, explicit_crs))
  }
  source_crs = suppressWarnings(sf::st_crs(sf_data))
  if (is.na(source_crs)) {
    source_crs = NULL
  }
  if (!is.null(target_crs) && is.null(source_crs)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial inputs must carry a CRS or `crs` must be supplied before transforming into the active scene CRS."
      ),
      call. = FALSE
    )
  }
  list(
    sf_data = sf_data,
    source_crs = source_crs
  )
}

transform_scene_sf_to_target_crs = function(
  sf_object,
  target_crs,
  crs = NULL,
  caller = NULL
) {
  coerced_input = coerce_scene_sf_input(sf_object)
  resolved_input = resolve_scene_sf_source_crs(
    sf_data = coerced_input$sf_data,
    crs = crs,
    target_crs = target_crs,
    caller = caller
  )
  sf_data = resolved_input$sf_data
  source_crs = resolved_input$source_crs
  target_crs = parse_scene_crs(
    target_crs,
    caller = caller,
    arg_name = "target CRS"
  )
  transformed = FALSE
  if (!scene_crs_equal(source_crs, target_crs)) {
    sf_data = sf::st_transform(sf_data, target_crs)
    transformed = TRUE
  }
  list(
    object = rebuild_scene_sf_output(sf_data, coerced_input$input_class),
    source_crs = source_crs,
    target_crs = target_crs,
    transformed = transformed
  )
}

get_scene_target_crs = function(
  extent = NULL,
  heightmap = NULL,
  panel = NULL,
  caller = NULL
) {
  transform_info = get_cached_plot_gg_transform_info(
    heightmap = heightmap,
    default = NULL
  )
  if (!is.null(transform_info)) {
    transform_context = get_scene_transform_context(
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      error_if_missing = TRUE,
      caller = caller
    )
    if (is.null(transform_context)) {
      return(NULL)
    }
    if (inherits(transform_context$coord_obj, "CoordSf")) {
      return(get_coord_sf_target_crs(transform_context$panel_params))
    }
    return(NULL)
  }
  scene_crs = get_scene_crs(default = NULL)
  scene_crs = try_parse_scene_crs(scene_crs)
  if (!is.null(scene_crs)) {
    return(scene_crs)
  }
  hillshade_crs = get_hillshade_crs(default = NULL)
  hillshade_crs = try_parse_scene_crs(hillshade_crs)
  if (!is.null(hillshade_crs)) {
    return(hillshade_crs)
  }
  NULL
}

resolve_cached_extent_center_latlong = function(caller = NULL) {
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    return(NULL)
  }
  extent_candidates = list(
    list(
      extent = get_scene_extent(default = NULL),
      crs = try_parse_scene_crs(get_scene_crs(default = NULL)),
      source = "scene"
    ),
    list(
      extent = get_hillshade_extent(default = NULL),
      crs = try_parse_scene_crs(get_hillshade_crs(default = NULL)),
      source = "hillshade"
    )
  )
  for (candidate in extent_candidates) {
    if (is.null(candidate$extent) || is.null(candidate$crs)) {
      next
    }
    extent_vec = tryCatch(
      get_extent(candidate$extent),
      error = function(e) NULL
    )
    if (
      is.null(extent_vec) ||
        any(!is.finite(extent_vec[c("xmin", "xmax", "ymin", "ymax")]))
    ) {
      next
    }
    center_x = mean(extent_vec[c("xmin", "xmax")])
    center_y = mean(extent_vec[c("ymin", "ymax")])
    if (isTRUE(sf::st_is_longlat(candidate$crs))) {
      return(list(
        lat = unname(center_y),
        long = unname(center_x),
        source = candidate$source
      ))
    }
    center_ll = tryCatch(
      transform_xy_between_crs(
        x_vals = center_x,
        y_vals = center_y,
        source_crs = candidate$crs,
        target_crs = 4326,
        caller = caller
      ),
      error = function(e) NULL
    )
    if (is.null(center_ll)) {
      next
    }
    return(list(
      lat = unname(center_ll$y[1]),
      long = unname(center_ll$x[1]),
      source = candidate$source
    ))
  }
  NULL
}

transform_ggplot_xy_with_context = function(
  x_vals,
  y_vals,
  transform_context,
  crs = NULL,
  crs_already_transformed = FALSE
) {
  if (length(x_vals) != length(y_vals)) {
    stop("`x_vals` and `y_vals` must have the same length.")
  }
  panel = transform_context$panel
  panel_index = transform_context$panel_index
  panel_table = transform_context$panel_table
  panel_params = transform_context$panel_params
  panel_extent_info = transform_context$panel_extent_info
  coord_obj = transform_context$coord_obj

  if (inherits(coord_obj, "CoordSf")) {
    if (is.null(crs) && !isTRUE(crs_already_transformed)) {
      stop(
        "Bare numeric `x`/`y` inputs for `coord_sf()` scenes must include `crs`.",
        call. = FALSE
      )
    }
    if (!isTRUE(crs_already_transformed)) {
      target_crs = get_coord_sf_target_crs(panel_params)
      transformed_xy = transform_xy_between_crs(
        x_vals = x_vals,
        y_vals = y_vals,
        source_crs = crs,
        target_crs = target_crs,
        caller = NULL
      )
      x_vals = transformed_xy$x
      y_vals = transformed_xy$y
    }
    x_vals = as.numeric(x_vals)
    y_vals = as.numeric(y_vals)
    coord_input = fast_scene_data_frame(list(
      x = x_vals,
      y = y_vals,
      PANEL = rep.int(panel, length(x_vals))
    ))
  } else {
    scale_x_index = panel_table$scale_x[panel_index]
    scale_y_index = panel_table$scale_y[panel_index]
    x_scale = transform_context$panel_scales_x[[scale_x_index]]
    y_scale = transform_context$panel_scales_y[[scale_y_index]]
    if (x_scale$is_discrete()) {
      x_transformed = as.numeric(x_scale$map(x_vals))
    } else {
      x_transformed = as.numeric(
        x_scale$transform_df(fast_scene_data_frame(list(x = x_vals)))[["x"]]
      )
    }
    if (y_scale$is_discrete()) {
      y_transformed = as.numeric(y_scale$map(y_vals))
    } else {
      y_transformed = as.numeric(
        y_scale$transform_df(fast_scene_data_frame(list(y = y_vals)))[["y"]]
      )
    }
    coord_input = fast_scene_data_frame(list(
      x = x_transformed,
      y = y_transformed,
      PANEL = rep.int(panel, length(x_transformed))
    ))
  }

  transformed = coord_obj$transform(coord_input, panel_params)
  if (!all(c("x", "y") %in% names(transformed))) {
    stop("ggplot coord transform did not return x/y columns.")
  }
  x_range = c(panel_extent_info$data_xmin[1], panel_extent_info$data_xmax[1])
  y_range = c(panel_extent_info$data_ymin[1], panel_extent_info$data_ymax[1])
  if (any(!is.finite(x_range))) {
    x_range = tryCatch(
      get_ggplot_panel_range(panel_params, "x"),
      error = function(e) c(0, 1)
    )
  }
  if (any(!is.finite(y_range))) {
    y_range = tryCatch(
      get_ggplot_panel_range(panel_params, "y"),
      error = function(e) c(0, 1)
    )
  }
  fast_scene_data_frame(list(
    long = map_from_panel_npc(transformed$x, x_range),
    lat = map_from_panel_npc(transformed$y, y_range)
  ))
}

#'@keywords internal
#'@noRd
transform_ggplot_sf = function(
  sf_object,
  panel = NULL,
  heightmap = NULL,
  crs = NULL,
  segmentize_df_max_length = NULL
) {
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop("`sf` package required for `transform_ggplot_sf()`.")
  }
  coerced_input = coerce_scene_sf_input(sf_object)
  sf_data = coerced_input$sf_data
  input_class = coerced_input$input_class
  if (!is.null(segmentize_df_max_length)) {
    sf_data = tryCatch(
      sf::st_segmentize(
        sf_data,
        dfMaxLength = segmentize_df_max_length
      ),
      error = function(e) {
        stop(
          paste0(
            "Could not segmentize geometry. Install `lwgeom` if required ",
            "for your geometry/CRS.\nOriginal error: ",
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
  }

  transform_info = get_plot_gg_transform_info(heightmap = heightmap)
  transform_context = get_plot_gg_panel_transform_context(
    transform_info = transform_info,
    panel = panel,
    heightmap = heightmap
  )
  coord_sf_already_transformed = FALSE
  output_crs = NULL
  if (inherits(transform_context$coord_obj, "CoordSf")) {
    target_crs = get_coord_sf_target_crs(transform_context$panel_params)
    resolved_input = resolve_scene_sf_source_crs(
      sf_data = sf_data,
      crs = crs,
      target_crs = target_crs,
      caller = "transform_ggplot_sf"
    )
    sf_data = resolved_input$sf_data
    crs = resolved_input$source_crs
    if (!scene_crs_equal(crs, target_crs)) {
      sf_data = sf::st_transform(sf_data, target_crs)
    }
    crs = target_crs
    output_crs = target_crs
    coord_sf_already_transformed = TRUE
  } else {
    crs = NULL
  }

  transform_matrix = function(mat) {
    if (!is.matrix(mat) || ncol(mat) < 2) {
      stop("Could not transform geometry: expected matrix coordinates.")
    }
    if (!nrow(mat)) {
      return(mat)
    }
    xy = transform_ggplot_xy_with_context(
      x_vals = mat[, 1],
      y_vals = mat[, 2],
      transform_context = transform_context,
      crs = crs,
      crs_already_transformed = coord_sf_already_transformed
    )
    extra_dims = if (ncol(mat) > 2) {
      mat[, -(1:2), drop = FALSE]
    } else {
      NULL
    }
    out = cbind(xy$long, xy$lat, extra_dims)
    colnames(out) = NULL
    out
  }

  transform_sfg = NULL
  transform_sfg = function(geom) {
    supported_geom_types = c(
      "POINT",
      "LINESTRING",
      "MULTIPOINT",
      "POLYGON",
      "MULTILINESTRING",
      "MULTIPOLYGON",
      "GEOMETRYCOLLECTION"
    )
    geom_type = intersect(class(geom), supported_geom_types)[1]
    if (is.na(geom_type)) {
      stop(sprintf(
        "Unsupported geometry class `%s`.",
        paste(class(geom), collapse = ", ")
      ))
    }
    if (geom_type == "POINT") {
      coords = as.numeric(unclass(geom))
      xy = transform_ggplot_xy_with_context(
        x_vals = coords[1],
        y_vals = coords[2],
        transform_context = transform_context,
        crs = crs,
        crs_already_transformed = coord_sf_already_transformed
      )
      return(sf::st_point(c(xy$long[1], xy$lat[1], coords[-c(1, 2)])))
    }
    if (geom_type == "LINESTRING") {
      return(sf::st_linestring(transform_matrix(unclass(geom))))
    }
    if (geom_type == "MULTIPOINT") {
      return(sf::st_multipoint(transform_matrix(unclass(geom))))
    }
    if (geom_type == "POLYGON") {
      return(sf::st_polygon(
        lapply(unclass(geom), transform_matrix)
      ))
    }
    if (geom_type == "MULTILINESTRING") {
      return(sf::st_multilinestring(
        lapply(unclass(geom), transform_matrix)
      ))
    }
    if (geom_type == "MULTIPOLYGON") {
      return(sf::st_multipolygon(
        lapply(unclass(geom), function(poly) {
          lapply(poly, transform_matrix)
        })
      ))
    }
    if (geom_type == "GEOMETRYCOLLECTION") {
      return(sf::st_geometrycollection(
        lapply(unclass(geom), transform_sfg)
      ))
    }
    stop(sprintf(
      "Unsupported geometry type `%s`.",
      geom_type
    ))
  }

  geom_transformed = lapply(sf::st_geometry(sf_data), transform_sfg)
  if (is.null(output_crs)) {
    sf::st_geometry(sf_data) = sf::st_sfc(geom_transformed)
  } else {
    sf::st_geometry(sf_data) = sf::st_sfc(
      geom_transformed,
      crs = output_crs
    )
  }
  attr(sf_data, "extent") = transform_context$transformed_extent
  attr(sf_data, "panel") = transform_context$panel

  if (input_class == "sf") {
    return(sf_data)
  }
  if (input_class == "sfc") {
    out = sf::st_geometry(sf_data)
    attr(out, "extent") = transform_context$transformed_extent
    attr(out, "panel") = transform_context$panel
    return(out)
  }
  out = sf::st_geometry(sf_data)[[1]]
  attr(out, "extent") = transform_context$transformed_extent
  attr(out, "panel") = transform_context$panel
  out
}

get_ggplot_panel_range = function(panel_params, axis = c("x", "y")) {
  axis = match.arg(axis)
  range_candidates = c(
    paste0(axis, ".range"),
    paste0(axis, "_range")
  )
  for (candidate in range_candidates) {
    candidate_val = panel_params[[candidate]]
    if (!is.null(candidate_val)) {
      return(as.numeric(candidate_val))
    }
  }
  axis_info = panel_params[[axis]]
  if (!is.null(axis_info$continuous_range)) {
    return(as.numeric(axis_info$continuous_range))
  }
  if (!is.null(axis_info$range$range)) {
    return(as.numeric(axis_info$range$range))
  }
  stop(sprintf(
    "Could not determine panel %s range from ggplot build object.",
    axis
  ))
}

get_ggplot_panel_viewport = function(panel_layout_row) {
  sprintf(
    "%s.%s-%s-%s-%s",
    panel_layout_row$name,
    panel_layout_row$t,
    panel_layout_row$l,
    panel_layout_row$b,
    panel_layout_row$r
  )
}

get_device_panel_bbox = function(width_px, height_px) {
  bbox_in = c(
    unlist(grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), TRUE)),
    unlist(grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"), TRUE))
  )
  dev_size_in = grDevices::dev.size("in")
  bbox_px = bbox_in
  bbox_px[c(1, 3)] = bbox_px[c(1, 3)] / dev_size_in[1] * width_px
  bbox_px[c(2, 4)] = bbox_px[c(2, 4)] / dev_size_in[2] * height_px
  bbox_px[c(2, 4)] = height_px - bbox_px[c(4, 2)]
  stats::setNames(as.numeric(bbox_px), c("xmin", "ymin", "xmax", "ymax"))
}

expand_ggplot_x_extent = function(data_range, panel_range, scene_width) {
  slope = diff(data_range) / diff(panel_range)
  c(
    xmin = data_range[1] - (panel_range[1] - 1) * slope,
    xmax = data_range[2] + (scene_width - panel_range[2]) * slope
  )
}

expand_ggplot_y_extent = function(data_range, panel_range, scene_height) {
  slope = diff(data_range) / diff(panel_range)
  c(
    ymin = data_range[1] - (scene_height - panel_range[2]) * slope,
    ymax = data_range[2] + (panel_range[1] - 1) * slope
  )
}

get_current_scene_context_token = function(default = NULL) {
  current_scene = tryCatch(rgl::cur3d(), error = function(e) NULL)
  if (is.null(current_scene)) {
    return(default)
  }
  current_scene = suppressWarnings(as.integer(current_scene)[1])
  if (!is.finite(current_scene) || current_scene <= 0) {
    return(default)
  }
  current_scene
}

cache_scene_context_token = function(
  token = get_current_scene_context_token(default = NULL)
) {
  assign("scene_context_token", token, envir = ray_cache_scene_envir)
  invisible(NULL)
}

get_scene_context_token = function(default = NULL) {
  scene_token = get0(
    "scene_context_token",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(scene_token)) {
    return(default)
  }
  scene_token = suppressWarnings(as.integer(scene_token)[1])
  if (!is.finite(scene_token) || scene_token <= 0) {
    return(default)
  }
  scene_token
}

is_current_scene_context = function(
  token = get_scene_context_token(default = NULL)
) {
  current_token = get_current_scene_context_token(default = NULL)
  !is.null(token) && !is.null(current_token) && identical(token, current_token)
}

get_scene_context_value = function(name, default = NULL) {
  if (!is_current_scene_context()) {
    return(default)
  }
  value = get0(name, envir = ray_cache_scene_envir, inherits = FALSE)
  if (is.null(value)) {
    return(default)
  }
  value
}

cache_scene_cache = function(scene_cache = NULL) {
  assign("scene_cache", scene_cache, envir = ray_cache_scene_envir)
  invisible(NULL)
}

get_scene_cache = function(default = NULL) {
  get_scene_context_value("scene_cache", default = default)
}

#' Cache eagerly built road meshes for the active scene
#'
#' @param meshes Default `NULL`. Absolute-coordinate road `mesh3d` objects.
#' @param append Default `FALSE`. Whether to append a new road layer.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
cache_scene_road_meshes = function(meshes = NULL, append = FALSE) {
  if (is.null(meshes)) {
    assign("road_meshes", NULL, envir = ray_cache_scene_envir)
    return(invisible(NULL))
  }
  append = isTRUE(append)
  existing = if (append) {
    get_scene_road_meshes(default = list())
  } else {
    list()
  }
  assign(
    "road_meshes",
    c(existing, unclass(meshes)),
    envir = ray_cache_scene_envir
  )
  invisible(NULL)
}

#' Get eagerly built road meshes for the active scene
#'
#' @param default Default `NULL`. Value returned when no cache is available.
#'
#' @return Cached absolute-coordinate road `mesh3d` objects.
#' @keywords internal
get_scene_road_meshes = function(default = NULL) {
  get_scene_context_value("road_meshes", default = default)
}

reset_scene_context = function(
  clear_scene_metadata = TRUE,
  clear_scene_cache = TRUE
) {
  if (isTRUE(clear_scene_cache)) {
    cache_scene_cache(NULL)
    cache_scene_road_meshes(NULL)
  }
  if (isTRUE(clear_scene_metadata)) {
    cache_scene_context_token(NULL)
    cache_scene_zscale(NULL, label = NULL)
    cache_scene_vertical_exaggeration(NULL, label = NULL)
    cache_scene_triangulate(NULL)
    cache_scene_heightmap(NULL, label = NULL)
    cache_scene_extent(NULL, label = NULL)
    cache_scene_crs(NULL, label = NULL)
    cache_plot_gg_panel_info(NULL)
    cache_plot_gg_transform_info(NULL)
    clear_scene_zaxis_data()
    clear_render_road_path_info()
    clear_render_water_path_info()
  }
  invisible(NULL)
}

cache_plot_gg_panel_info = function(panel_info = NULL) {
  assign("plot_gg_panel_info", panel_info, envir = ray_cache_scene_envir)
  invisible(NULL)
}

cache_plot_gg_transform_info = function(transform_info = NULL) {
  assign(
    "plot_gg_transform_info",
    transform_info,
    envir = ray_cache_scene_envir
  )
  invisible(NULL)
}

cache_scene_zscale = function(zscale = NULL, label = NULL) {
  assign("scene_zscale", zscale, envir = ray_cache_scene_envir)
  assign("scene_zscale_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

get_scene_zscale = function(default = NULL) {
  scene_zscale = get_scene_context_value("scene_zscale", default = default)
  if (is.null(scene_zscale)) {
    return(default)
  }
  scene_zscale = suppressWarnings(as.numeric(scene_zscale)[1])
  if (!is.finite(scene_zscale) || scene_zscale <= 0) {
    return(default)
  }
  scene_zscale
}

get_scene_zscale_label = function(default = NULL) {
  scene_zscale_label = get_scene_context_value(
    "scene_zscale_label",
    default = default
  )
  if (is.null(scene_zscale_label)) {
    return(default)
  }
  scene_zscale_label
}

cache_scene_vertical_exaggeration = function(
  vertical_exaggeration = NULL,
  label = NULL
) {
  assign(
    "scene_vertical_exaggeration",
    vertical_exaggeration,
    envir = ray_cache_scene_envir
  )
  assign(
    "scene_vertical_exaggeration_label",
    label,
    envir = ray_cache_scene_envir
  )
  invisible(NULL)
}

get_scene_vertical_exaggeration = function(default = NULL) {
  vertical_exaggeration = get_scene_context_value(
    "scene_vertical_exaggeration",
    default = default
  )
  if (is.null(vertical_exaggeration)) {
    return(default)
  }
  vertical_exaggeration = suppressWarnings(as.numeric(vertical_exaggeration)[1])
  if (!is.finite(vertical_exaggeration) || vertical_exaggeration <= 0) {
    return(default)
  }
  vertical_exaggeration
}

get_scene_vertical_exaggeration_label = function(default = NULL) {
  vertical_exaggeration_label = get_scene_context_value(
    "scene_vertical_exaggeration_label",
    default = default
  )
  if (is.null(vertical_exaggeration_label)) {
    return(default)
  }
  vertical_exaggeration_label
}

get_scene_effective_zscale = function(default = NULL) {
  scene_zscale = get_scene_zscale(default = NA_real_)
  if (!is.finite(scene_zscale) || scene_zscale <= 0) {
    return(default)
  }
  scene_vertical_exaggeration = get_scene_vertical_exaggeration(default = 1)
  if (
    !is.finite(scene_vertical_exaggeration) ||
      scene_vertical_exaggeration <= 0
  ) {
    return(default)
  }
  scene_zscale / scene_vertical_exaggeration
}

cache_scene_triangulate = function(triangulate = NULL) {
  assign("scene_triangulate", triangulate, envir = ray_cache_scene_envir)
  invisible(NULL)
}

get_scene_triangulate = function(default = FALSE) {
  triangulate = get_scene_context_value("scene_triangulate", default = default)
  if (is.null(triangulate)) {
    return(default)
  }
  isTRUE(triangulate)
}

cache_scene_heightmap = function(heightmap = NULL, label = NULL) {
  assign("scene_heightmap", heightmap, envir = ray_cache_scene_envir)
  assign("scene_heightmap_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

with_suppressed_hillshade_zscale_cache = function(expr) {
  old = isTRUE(get0(
    "suppress_hillshade_zscale_cache",
    envir = ray_cache_scene_envir,
    inherits = FALSE,
    ifnotfound = FALSE
  ))
  assign("suppress_hillshade_zscale_cache", TRUE, envir = ray_cache_scene_envir)
  on.exit(
    assign(
      "suppress_hillshade_zscale_cache",
      old,
      envir = ray_cache_scene_envir
    ),
    add = TRUE
  )
  force(expr)
}

cache_hillshade_zscale = function(zscale = NULL, label = NULL) {
  suppress_cache = isTRUE(get0(
    "suppress_hillshade_zscale_cache",
    envir = ray_cache_scene_envir,
    inherits = FALSE,
    ifnotfound = FALSE
  ))
  if (suppress_cache && !is.null(zscale)) {
    return(invisible(NULL))
  }
  assign("hillshade_zscale", zscale, envir = ray_cache_scene_envir)
  assign("hillshade_zscale_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

get_hillshade_zscale = function(default = NULL) {
  hillshade_zscale = get0(
    "hillshade_zscale",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_zscale)) {
    return(default)
  }
  hillshade_zscale = suppressWarnings(as.numeric(hillshade_zscale)[1])
  if (!is.finite(hillshade_zscale) || hillshade_zscale <= 0) {
    return(default)
  }
  hillshade_zscale
}

get_hillshade_zscale_label = function(default = NULL) {
  hillshade_zscale_label = get0(
    "hillshade_zscale_label",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_zscale_label)) {
    return(default)
  }
  hillshade_zscale_label
}

cache_hillshade_heightmap = function(heightmap = NULL, label = NULL) {
  assign("hillshade_heightmap", heightmap, envir = ray_cache_scene_envir)
  assign("hillshade_heightmap_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

cache_hillshade_extent = function(extent = NULL, label = NULL) {
  assign("hillshade_extent", extent, envir = ray_cache_scene_envir)
  assign("hillshade_extent_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

cache_hillshade_crs = function(crs = NULL, label = NULL) {
  assign("hillshade_crs", crs, envir = ray_cache_scene_envir)
  assign("hillshade_crs_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

cache_hillshade_input_context = function(heightmap_info, label = NULL) {
  cache_hillshade_heightmap(heightmap_info$heightmap, label = label)
  if (is.finite(heightmap_info$zscale) && heightmap_info$zscale > 0) {
    cache_hillshade_zscale(heightmap_info$zscale, label = label)
  } else {
    cache_hillshade_zscale(NULL, label = NULL)
  }
  if (!is.null(heightmap_info$extent)) {
    cache_hillshade_extent(heightmap_info$extent, label = label)
  } else {
    cache_hillshade_extent(NULL, label = NULL)
  }
  if (!is.null(heightmap_info$crs)) {
    cache_hillshade_crs(heightmap_info$crs, label = label)
  } else {
    cache_hillshade_crs(NULL, label = NULL)
  }
  invisible(NULL)
}

cache_hillshade_map = function(hillshade = NULL, label = NULL) {
  assign("hillshade_map", hillshade, envir = ray_cache_scene_envir)
  assign("hillshade_map_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

get_hillshade_heightmap = function(default = NULL) {
  hillshade_heightmap = get0(
    "hillshade_heightmap",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_heightmap)) {
    return(default)
  }
  hillshade_heightmap
}

get_hillshade_heightmap_label = function(default = NULL) {
  hillshade_heightmap_label = get0(
    "hillshade_heightmap_label",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_heightmap_label)) {
    return(default)
  }
  hillshade_heightmap_label
}

get_hillshade_extent = function(default = NULL) {
  hillshade_extent = get0(
    "hillshade_extent",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_extent)) {
    return(default)
  }
  hillshade_extent
}

get_hillshade_extent_label = function(default = NULL) {
  hillshade_extent_label = get0(
    "hillshade_extent_label",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_extent_label)) {
    return(default)
  }
  hillshade_extent_label
}

get_hillshade_crs = function(default = NULL) {
  hillshade_crs = get0(
    "hillshade_crs",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_crs)) {
    return(default)
  }
  hillshade_crs
}

get_hillshade_crs_label = function(default = NULL) {
  hillshade_crs_label = get0(
    "hillshade_crs_label",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_crs_label)) {
    return(default)
  }
  hillshade_crs_label
}

get_hillshade_map = function(default = NULL) {
  hillshade_map = get0(
    "hillshade_map",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_map)) {
    return(default)
  }
  hillshade_map
}

get_hillshade_map_label = function(default = NULL) {
  hillshade_map_label = get0(
    "hillshade_map_label",
    envir = ray_cache_scene_envir,
    inherits = FALSE
  )
  if (is.null(hillshade_map_label)) {
    return(default)
  }
  hillshade_map_label
}

clear_hillshade_cache = function() {
  cache_hillshade_heightmap(NULL, label = NULL)
  cache_hillshade_zscale(NULL, label = NULL)
  cache_hillshade_extent(NULL, label = NULL)
  cache_hillshade_crs(NULL, label = NULL)
  cache_hillshade_map(NULL, label = NULL)
  invisible(NULL)
}

cache_scene_extent = function(extent = NULL, label = NULL) {
  assign("scene_extent", extent, envir = ray_cache_scene_envir)
  assign("scene_extent_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

cache_scene_crs = function(crs = NULL, label = NULL) {
  assign("scene_crs", crs, envir = ray_cache_scene_envir)
  assign("scene_crs_label", label, envir = ray_cache_scene_envir)
  invisible(NULL)
}

get_scene_extent = function(default = NULL) {
  scene_extent = get_scene_context_value("scene_extent", default = default)
  if (is.null(scene_extent)) {
    return(default)
  }
  scene_extent
}

get_scene_extent_label = function(default = NULL) {
  scene_extent_label = get_scene_context_value(
    "scene_extent_label",
    default = default
  )
  if (is.null(scene_extent_label)) {
    return(default)
  }
  scene_extent_label
}

get_scene_crs = function(default = NULL) {
  scene_crs = get_scene_context_value("scene_crs", default = default)
  if (is.null(scene_crs)) {
    return(default)
  }
  scene_crs
}

get_scene_crs_label = function(default = NULL) {
  scene_crs_label = get_scene_context_value(
    "scene_crs_label",
    default = default
  )
  if (is.null(scene_crs_label)) {
    return(default)
  }
  scene_crs_label
}

get_scene_heightmap = function(default = NULL) {
  scene_heightmap = get_scene_context_value(
    "scene_heightmap",
    default = default
  )
  if (is.null(scene_heightmap)) {
    return(default)
  }
  scene_heightmap
}

get_scene_heightmap_label = function(default = NULL) {
  scene_heightmap_label = get_scene_context_value(
    "scene_heightmap_label",
    default = default
  )
  if (is.null(scene_heightmap_label)) {
    return(default)
  }
  scene_heightmap_label
}

format_scene_cache_label = function(label) {
  if (is.null(label)) {
    return(NULL)
  }
  label = paste(label, collapse = " ")
  label = gsub("[\r\n\t]+", " ", label)
  label = trimws(label)
  if (!nzchar(label)) {
    return(NULL)
  }
  label
}

emit_scene_cache_message = function(
  caller,
  argument_name,
  cache_name,
  cache_label = NULL
) {
  if (is.null(caller) || !nzchar(caller)) {
    return(invisible(NULL))
  }
  if (!isTRUE(getOption("rayshader.verbose_scene_cache", FALSE))) {
    return(invisible(NULL))
  }
  cache_label = format_scene_cache_label(cache_label)
  if (is.null(cache_label)) {
    message(sprintf(
      "%s(): using cached `%s` from `%s`.",
      caller,
      argument_name,
      cache_name
    ))
  } else {
    message(sprintf(
      "%s(): using cached `%s` from `%s` (%s).",
      caller,
      argument_name,
      cache_name,
      cache_label
    ))
  }
  invisible(NULL)
}

resolve_vertical_exaggeration = function(
  vertical_exaggeration = 1,
  caller = NULL
) {
  vertical_exaggeration = suppressWarnings(as.numeric(vertical_exaggeration)[1])
  if (!is.finite(vertical_exaggeration) || vertical_exaggeration <= 0) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`vertical_exaggeration` must be a positive number."
      ),
      call. = FALSE
    )
  }
  vertical_exaggeration
}

apply_vertical_exaggeration = function(
  zscale = 1,
  vertical_exaggeration = 1,
  caller = NULL
) {
  vertical_exaggeration = resolve_vertical_exaggeration(
    vertical_exaggeration = vertical_exaggeration,
    caller = caller
  )
  zscale = suppressWarnings(as.numeric(zscale)[1])
  if (!is.finite(zscale) || zscale <= 0) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`zscale` must be a positive number."
      ),
      call. = FALSE
    )
  }
  zscale / vertical_exaggeration
}

resolve_scene_render_zscale = function(
  zscale = 1,
  zscale_missing = FALSE,
  caller = NULL
) {
  cached_scene_zscale = get_scene_zscale(default = NA_real_)
  if (
    isTRUE(zscale_missing) &&
      is.finite(cached_scene_zscale) &&
      cached_scene_zscale > 0
  ) {
    emit_scene_cache_message(
      caller = caller,
      argument_name = "zscale",
      cache_name = "scene_zscale",
      cache_label = get_scene_zscale_label(default = NULL)
    )
    return(cached_scene_zscale)
  }
  if (is.null(zscale)) {
    if (is.finite(cached_scene_zscale) && cached_scene_zscale > 0) {
      emit_scene_cache_message(
        caller = caller,
        argument_name = "zscale",
        cache_name = "scene_zscale",
        cache_label = get_scene_zscale_label(default = NULL)
      )
      return(cached_scene_zscale)
    }
    return(1)
  }
  zscale = suppressWarnings(as.numeric(zscale)[1])
  if (!is.finite(zscale) || zscale <= 0) {
    if (is.finite(cached_scene_zscale) && cached_scene_zscale > 0) {
      emit_scene_cache_message(
        caller = caller,
        argument_name = "zscale",
        cache_name = "scene_zscale",
        cache_label = get_scene_zscale_label(default = NULL)
      )
      return(cached_scene_zscale)
    }
    return(1)
  }
  zscale
}

resolve_scene_render_vertical_exaggeration = function(
  vertical_exaggeration = 1,
  vertical_exaggeration_missing = FALSE,
  caller = NULL
) {
  cached_scene_vertical_exaggeration =
    get_scene_vertical_exaggeration(default = NA_real_)
  if (
    isTRUE(vertical_exaggeration_missing) &&
      is.finite(cached_scene_vertical_exaggeration) &&
      cached_scene_vertical_exaggeration > 0
  ) {
    emit_scene_cache_message(
      caller = caller,
      argument_name = "vertical_exaggeration",
      cache_name = "scene_vertical_exaggeration",
      cache_label = get_scene_vertical_exaggeration_label(default = NULL)
    )
    return(cached_scene_vertical_exaggeration)
  }
  if (is.null(vertical_exaggeration)) {
    if (
      is.finite(cached_scene_vertical_exaggeration) &&
        cached_scene_vertical_exaggeration > 0
    ) {
      emit_scene_cache_message(
        caller = caller,
        argument_name = "vertical_exaggeration",
        cache_name = "scene_vertical_exaggeration",
        cache_label = get_scene_vertical_exaggeration_label(default = NULL)
      )
      return(cached_scene_vertical_exaggeration)
    }
    return(1)
  }
  resolve_vertical_exaggeration(
    vertical_exaggeration = vertical_exaggeration,
    caller = caller
  )
}

resolve_scene_render_effective_zscale = function(
  zscale = 1,
  zscale_missing = FALSE,
  vertical_exaggeration = 1,
  vertical_exaggeration_missing = FALSE,
  heightmap = NULL,
  caller = NULL
) {
  heightmap_zscale = suppressWarnings(as.numeric(
    attr(heightmap, "zscale", exact = TRUE)
  )[1])
  if (
    isTRUE(zscale_missing) &&
      is.finite(heightmap_zscale) &&
      heightmap_zscale > 0
  ) {
    zscale = heightmap_zscale
  } else {
    zscale = resolve_scene_render_zscale(
      zscale = zscale,
      zscale_missing = zscale_missing,
      caller = caller
    )
  }
  vertical_exaggeration = resolve_scene_render_vertical_exaggeration(
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = vertical_exaggeration_missing,
    caller = caller
  )
  apply_vertical_exaggeration(
    zscale = zscale,
    vertical_exaggeration = vertical_exaggeration,
    caller = caller
  )
}

warn_scale_data_with_vertical_exaggeration = function(
  scale_data_missing = TRUE,
  vertical_exaggeration_missing = TRUE,
  caller = NULL
) {
  if (isTRUE(scale_data_missing) || isTRUE(vertical_exaggeration_missing)) {
    return(invisible(NULL))
  }
  warning(
    paste0(
      format_render_caller_prefix(caller),
      "`scale_data` and `vertical_exaggeration` both scale vertical data values; both will be applied."
    ),
    call. = FALSE
  )
  invisible(NULL)
}

resolve_scene_render_heightmap = function(
  heightmap = NULL,
  heightmap_missing = FALSE,
  caller = NULL
) {
  if (!isTRUE(heightmap_missing) && !is.null(heightmap)) {
    if (is_spatial_heightmap_input(heightmap)) {
      heightmap_info = coerce_plot_3d_heightmap(heightmap)
      if (!is.null(heightmap_info$extent)) {
        attr(heightmap_info$heightmap, "extent") = heightmap_info$extent
      }
      if (!is.null(heightmap_info$crs)) {
        attr(heightmap_info$heightmap, "crs") = heightmap_info$crs
      }
      if (
        is.finite(heightmap_info$zscale) &&
          heightmap_info$zscale > 0
      ) {
        attr(heightmap_info$heightmap, "zscale") = heightmap_info$zscale
      }
      return(heightmap_info$heightmap)
    }
    return(heightmap)
  }
  scene_heightmap = get_scene_heightmap(default = NULL)
  if (!is.null(scene_heightmap)) {
    emit_scene_cache_message(
      caller = caller,
      argument_name = "heightmap",
      cache_name = "scene_heightmap",
      cache_label = get_scene_heightmap_label(default = NULL)
    )
  }
  scene_heightmap
}


resolve_hillshade_heightmap = function(
  heightmap = NULL,
  heightmap_missing = FALSE,
  caller = NULL
) {
  if (!isTRUE(heightmap_missing) && !is.null(heightmap)) {
    return(list(
      heightmap = heightmap,
      source = "explicit",
      label = NULL
    ))
  }
  hillshade_heightmap = get_hillshade_heightmap(default = NULL)
  if (!is.null(hillshade_heightmap)) {
    emit_scene_cache_message(
      caller = caller,
      argument_name = "heightmap",
      cache_name = "hillshade_heightmap",
      cache_label = get_hillshade_heightmap_label(default = NULL)
    )
    return(list(
      heightmap = hillshade_heightmap,
      source = "hillshade",
      label = get_hillshade_heightmap_label(default = NULL)
    ))
  }
  scene_heightmap = get_scene_heightmap(default = NULL)
  if (!is.null(scene_heightmap)) {
    emit_scene_cache_message(
      caller = caller,
      argument_name = "heightmap",
      cache_name = "scene_heightmap",
      cache_label = get_scene_heightmap_label(default = NULL)
    )
    return(list(
      heightmap = scene_heightmap,
      source = "scene",
      label = get_scene_heightmap_label(default = NULL)
    ))
  }
  stop(
    "`heightmap` missing and no cached hillshade or scene heightmap is available.",
    call. = FALSE
  )
}

resolve_overlay_heightmap = function(
  heightmap = NULL,
  heightmap_missing = FALSE,
  width = NA,
  height = NA,
  caller = NULL
) {
  if (!isTRUE(heightmap_missing) && !is.null(heightmap)) {
    return(heightmap)
  }
  if (!is.na(width) && !is.na(height)) {
    return(heightmap)
  }
  resolve_hillshade_heightmap(
    heightmap = heightmap,
    heightmap_missing = TRUE,
    caller = caller
  )$heightmap
}

resolve_hillshade_zscale = function(
  zscale = 1,
  zscale_missing = FALSE,
  caller = NULL,
  auto_zscale = NA_real_,
  allow_hillshade_cache = TRUE,
  allow_scene_cache = TRUE
) {
  zscale = suppressWarnings(as.numeric(zscale)[1])
  if (!isTRUE(zscale_missing) && is.finite(zscale) && zscale > 0) {
    return(list(zscale = zscale, source = "explicit", label = NULL))
  }
  if (is.finite(auto_zscale) && auto_zscale > 0) {
    return(list(zscale = auto_zscale, source = "auto", label = NULL))
  }
  if (isTRUE(allow_hillshade_cache)) {
    hillshade_zscale = get_hillshade_zscale(default = NA_real_)
    if (is.finite(hillshade_zscale) && hillshade_zscale > 0) {
      emit_scene_cache_message(
        caller = caller,
        argument_name = "zscale",
        cache_name = "hillshade_zscale",
        cache_label = get_hillshade_zscale_label(default = NULL)
      )
      return(list(
        zscale = hillshade_zscale,
        source = "hillshade",
        label = get_hillshade_zscale_label(default = NULL)
      ))
    }
  }
  if (isTRUE(allow_scene_cache)) {
    scene_zscale = get_scene_zscale(default = NA_real_)
    if (is.finite(scene_zscale) && scene_zscale > 0) {
      emit_scene_cache_message(
        caller = caller,
        argument_name = "zscale",
        cache_name = "scene_zscale",
        cache_label = get_scene_zscale_label(default = NULL)
      )
      return(list(
        zscale = scene_zscale,
        source = "scene",
        label = get_scene_zscale_label(default = NULL)
      ))
    }
  }
  list(zscale = 1, source = "default", label = NULL)
}

resolve_scene_render_extent = function(
  extent = NULL,
  heightmap = NULL,
  caller = NULL,
  panel = NULL,
  allow_ggplot_extent = TRUE,
  allow_scene_extent = TRUE,
  error_if_missing = TRUE
) {
  if (!is.null(extent)) {
    extent = canonicalize_plot_gg_extent(
      extent = extent,
      heightmap = heightmap
    )
    extent = normalize_scene_resolved_extent(extent, caller = caller)
    return(validate_scene_extent_panel(
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      caller = caller
    ))
  }

  heightmap_extent = resolve_heightmap_extent(heightmap)
  if (!is.null(heightmap_extent)) {
    return(validate_scene_extent_panel(
      extent = heightmap_extent,
      heightmap = heightmap,
      panel = panel,
      caller = caller
    ))
  }

  if (isTRUE(allow_ggplot_extent)) {
    gg_extent = get_cached_ggplot_extent_or_null(
      heightmap = heightmap,
      panel = panel
    )
    if (is_panel_extent_list(gg_extent) && length(gg_extent) > 1) {
      if (isTRUE(error_if_missing)) {
        stop(format_faceted_ggplot_panel_error(caller), call. = FALSE)
      }
      gg_extent = NULL
    }
    gg_extent = normalize_scene_resolved_extent(gg_extent, caller = caller)
    if (!is.null(gg_extent)) {
      return(gg_extent)
    }
  }

  if (isTRUE(allow_scene_extent)) {
    scene_extent = get_scene_extent(default = NULL)
    if (!is.null(scene_extent)) {
      emit_scene_cache_message(
        caller = caller,
        argument_name = "extent",
        cache_name = "scene_extent",
        cache_label = get_scene_extent_label(default = NULL)
      )
      return(scene_extent)
    }
  }

  hillshade_extent = get_hillshade_extent(default = NULL)
  if (!is.null(hillshade_extent)) {
    emit_scene_cache_message(
      caller = caller,
      argument_name = "extent",
      cache_name = "hillshade_extent",
      cache_label = get_hillshade_extent_label(default = NULL)
    )
    return(hillshade_extent)
  }

  if (isTRUE(error_if_missing)) {
    stop(
      paste(
        "Could not determine `extent`.",
        "Pass `extent` explicitly, or use a scene or raster-backed hillshade with cached extent metadata."
      )
    )
  }
  NULL
}

build_plot_gg_transform_info = function(
  ggplot_build_obj,
  height_scale = NULL,
  height_color_scale = NULL,
  height_aes = NULL,
  height_label = NULL,
  height_is_mapped = FALSE,
  height_inverted = FALSE,
  height_use_data_scale = FALSE
) {
  build_layout = ggplot_build_obj$layout$layout
  data.frame(
    panel = as.integer(as.character(build_layout$PANEL)),
    row = if ("ROW" %in% colnames(build_layout)) {
      as.integer(as.character(build_layout$ROW))
    } else {
      NA_integer_
    },
    col = if ("COL" %in% colnames(build_layout)) {
      as.integer(as.character(build_layout$COL))
    } else {
      NA_integer_
    },
    scale_x = as.integer(as.character(build_layout$SCALE_X)),
    scale_y = as.integer(as.character(build_layout$SCALE_Y))
  ) -> panel_table

  list(
    coord = ggplot_build_obj$layout$coord,
    panel_params = ggplot_build_obj$layout$panel_params,
    panel_scales_x = ggplot_build_obj$layout$panel_scales_x,
    panel_scales_y = ggplot_build_obj$layout$panel_scales_y,
    layout = panel_table,
    height_scale = height_scale,
    height_color_scale = height_color_scale,
    height_aes = height_aes,
    height_label = height_label,
    height_is_mapped = isTRUE(height_is_mapped),
    height_inverted = isTRUE(height_inverted),
    height_use_data_scale = isTRUE(height_use_data_scale),
    height_range = if (!is.null(height_scale$range$range)) {
      height_scale$range$range
    } else {
      NULL
    }
  )
}

plot_gg_has_spatraster_height_source = function(plot_obj) {
  if (!inherits(plot_obj, "ggplot") || length(plot_obj$layers) == 0) {
    return(FALSE)
  }
  any(vapply(
    plot_obj$layers,
    function(layer) {
      layer_data = layer$data
      if (
        is.null(layer_data) ||
          !is.data.frame(layer_data) ||
          !("spatraster" %in% names(layer_data))
      ) {
        return(FALSE)
      }
      spatraster_data = layer_data$spatraster
      if (!length(spatraster_data)) {
        return(FALSE)
      }
      any(vapply(
        spatraster_data,
        function(entry) {
          if (inherits(entry, "SpatRaster")) {
            return(TRUE)
          }
          if (is.list(entry) && length(entry)) {
            return(any(vapply(
              entry,
              inherits,
              logical(1),
              what = "SpatRaster"
            )))
          }
          FALSE
        },
        logical(1)
      ))
    },
    logical(1)
  ))
}

get_plot_gg_spatraster_height_source = function(plot_obj) {
  if (!inherits(plot_obj, "ggplot") || length(plot_obj$layers) == 0) {
    return(NULL)
  }
  for (layer in plot_obj$layers) {
    layer_data = layer$data
    if (
      is.null(layer_data) ||
        !is.data.frame(layer_data) ||
        !("spatraster" %in% names(layer_data))
    ) {
      next
    }
    spatraster_data = layer_data$spatraster
    if (!length(spatraster_data)) {
      next
    }
    for (entry in spatraster_data) {
      if (inherits(entry, "SpatRaster")) {
        return(entry)
      }
      if (is.list(entry) && length(entry)) {
        for (subentry in entry) {
          if (inherits(subentry, "SpatRaster")) {
            return(subentry)
          }
        }
      }
    }
  }
  NULL
}

resolve_plot_gg_height_zscale = function(
  zscale = NULL,
  zscale_missing = TRUE,
  scale = NULL,
  scale_missing = TRUE,
  vertical_exaggeration = NULL,
  vertical_exaggeration_missing = TRUE,
  height_plot_source = NULL,
  caller = NULL
) {
  auto_zscale = NA_real_
  spatraster_height_source = get_plot_gg_spatraster_height_source(
    height_plot_source
  )
  if (!is.null(spatraster_height_source)) {
    auto_zscale = extract_spatial_heightmap_zscale(spatraster_height_source)
  }

  if (!isTRUE(scale_missing) && !isTRUE(vertical_exaggeration_missing)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`scale` is deprecated; supply only `vertical_exaggeration`."
      ),
      call. = FALSE
    )
  }
  if (!isTRUE(scale_missing)) {
    vertical_exaggeration = scale
    vertical_exaggeration_missing = FALSE
  }
  if (isTRUE(vertical_exaggeration_missing) || is.null(vertical_exaggeration)) {
    explicit_zscale_supplied = !isTRUE(zscale_missing) && !is.null(zscale)
    vertical_exaggeration = if (
      explicit_zscale_supplied ||
        (is.finite(auto_zscale) && auto_zscale > 0)
    ) {
      1
    } else {
      300
    }
  }
  vertical_exaggeration = resolve_vertical_exaggeration(
    vertical_exaggeration = vertical_exaggeration,
    caller = caller
  )

  if (!isTRUE(zscale_missing) && !is.null(zscale)) {
    base_zscale = suppressWarnings(as.numeric(zscale)[1])
    if (!is.finite(base_zscale) || base_zscale <= 0) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`zscale` must be a positive number."
        ),
        call. = FALSE
      )
    }
    source = "explicit"
  } else if (is.finite(auto_zscale) && auto_zscale > 0) {
    base_zscale = auto_zscale
    source = "auto"
  } else {
    base_zscale = 1
    source = "default"
  }

  list(
    base_zscale = base_zscale,
    auto_zscale = auto_zscale,
    vertical_exaggeration = vertical_exaggeration,
    source = source,
    height_use_data_scale = is.finite(auto_zscale) && auto_zscale > 0
  )
}

get_plot_gg_scene_extent_from_panel_info = function(panel_info = NULL) {
  if (is.null(panel_info) || !is.data.frame(panel_info) || !nrow(panel_info)) {
    return(NULL)
  }
  required_cols = c(
    "extent_xmin",
    "extent_xmax",
    "extent_ymin",
    "extent_ymax"
  )
  if (!all(required_cols %in% names(panel_info))) {
    return(NULL)
  }
  extent_vals = c(
    xmin = min(panel_info$extent_xmin, na.rm = TRUE),
    xmax = max(panel_info$extent_xmax, na.rm = TRUE),
    ymin = min(panel_info$extent_ymin, na.rm = TRUE),
    ymax = max(panel_info$extent_ymax, na.rm = TRUE)
  )
  if (!all(is.finite(extent_vals))) {
    return(NULL)
  }
  extent_vals
}

resolve_plot_gg_rendered_zscale = function(
  panel_info = NULL,
  height_matrix = NULL,
  default = NA_real_
) {
  scene_extent = get_plot_gg_scene_extent_from_panel_info(panel_info)
  if (is.null(scene_extent)) {
    return(default)
  }
  if (is.null(height_matrix) || length(dim(height_matrix)) < 2) {
    return(default)
  }
  if (nrow(height_matrix) < 2 || ncol(height_matrix) < 2) {
    return(default)
  }
  resolution = c(
    (scene_extent["xmax"] - scene_extent["xmin"]) / (nrow(height_matrix) - 1),
    (scene_extent["ymax"] - scene_extent["ymin"]) / (ncol(height_matrix) - 1)
  )
  resolution = suppressWarnings(as.numeric(resolution))
  resolution = abs(resolution[is.finite(resolution) & resolution > 0])
  if (length(resolution) == 0) {
    return(default)
  }
  mean(resolution)
}

format_plot_gg_zscale_number = function(x) {
  format(signif(x, 6), scientific = FALSE, trim = TRUE)
}

get_plot_gg_horizontal_units = function(transform_info = NULL) {
  if (
    !is.null(transform_info) &&
      inherits(transform_info$coord, "CoordSf") &&
      length(transform_info$panel_params) > 0 &&
      length(find.package("sf", quiet = TRUE)) > 0
  ) {
    target_crs = tryCatch(
      get_coord_sf_target_crs(transform_info$panel_params[[1]]),
      error = function(e) NULL
    )
    if (!is.null(target_crs)) {
      crs_units = tryCatch(target_crs$units_gdal, error = function(e) NULL)
      if (!is.null(crs_units) && !is.na(crs_units) && nzchar(crs_units)) {
        return(crs_units)
      }
      if (
        isTRUE(tryCatch(sf::st_is_longlat(target_crs), error = function(e) {
          FALSE
        }))
      ) {
        return("degrees")
      }
    }
  }
  "scene units"
}

format_plot_gg_zscale_source = function(resolved_height_zscale) {
  source = resolved_height_zscale$source
  if (identical(source, "explicit")) {
    return("explicit zscale")
  }
  if (identical(source, "auto")) {
    return("rendered scene spacing")
  }
  if (identical(source, "default")) {
    return("default zscale")
  }
  "resolved zscale"
}

emit_plot_gg_zscale_message = function(
  resolved_height_zscale,
  zscale,
  transform_info = NULL
) {
  units = get_plot_gg_horizontal_units(transform_info)
  message(sprintf(
    paste0(
      "plot_gg(): computed zscale = %s %s per height unit ",
      "(base zscale = %s, vertical_exaggeration = %s, source = %s)."
    ),
    format_plot_gg_zscale_number(zscale),
    units,
    format_plot_gg_zscale_number(resolved_height_zscale$base_zscale),
    format_plot_gg_zscale_number(resolved_height_zscale$vertical_exaggeration),
    format_plot_gg_zscale_source(resolved_height_zscale)
  ))
  invisible(NULL)
}

get_plot_gg_height_data_range = function(transform_info = NULL) {
  if (
    is.null(transform_info) ||
      !isTRUE(transform_info$height_is_mapped) ||
      !isTRUE(transform_info$height_use_data_scale) ||
      is.null(transform_info$height_range)
  ) {
    return(NULL)
  }
  height_range = suppressWarnings(as.numeric(transform_info$height_range))
  height_range = height_range[is.finite(height_range)]
  if (length(height_range) != 2) {
    return(NULL)
  }
  if (isTRUE(transform_info$height_inverted)) {
    height_range = rev(height_range)
  }
  height_scale = transform_info$height_scale
  if (
    !is.null(height_scale) &&
      is.function(height_scale$get_transformation)
  ) {
    transformation = tryCatch(
      height_scale$get_transformation(),
      error = function(e) NULL
    )
    if (!is.null(transformation) && is.function(transformation$inverse)) {
      height_range = tryCatch(
        transformation$inverse(height_range),
        error = function(e) height_range
      )
    }
  }
  height_range = suppressWarnings(as.numeric(height_range))
  height_range = height_range[is.finite(height_range)]
  if (length(height_range) != 2) {
    return(NULL)
  }
  height_range
}

restore_plot_gg_height_matrix_data_scale = function(
  height_matrix,
  transform_info = NULL
) {
  height_range = get_plot_gg_height_data_range(transform_info = transform_info)
  if (is.null(height_range)) {
    return(height_matrix)
  }
  finite_vals = is.finite(height_matrix)
  if (!any(finite_vals)) {
    return(height_matrix)
  }
  restored = height_matrix
  normalized_vals = pmin(pmax(restored[finite_vals], 0), 1)
  restored[finite_vals] = scales::rescale(
    normalized_vals,
    to = height_range,
    from = c(0, 1)
  )
  restored
}

get_scene_height_transform = function(heightmap = NULL, extent = NULL) {
  panel_info = NULL
  if (!is.null(extent)) {
    panel_info = attr(extent, "panel_info", exact = TRUE)
  }
  if (is.null(panel_info) || !is.data.frame(panel_info) || !nrow(panel_info)) {
    return(NULL)
  }
  transform_info = tryCatch(
    get_plot_gg_transform_info(heightmap = heightmap),
    error = function(e) NULL
  )
  if (
    is.null(transform_info) ||
      !isTRUE(transform_info$height_is_mapped) ||
      isTRUE(transform_info$height_use_data_scale) ||
      is.null(transform_info$height_scale)
  ) {
    return(NULL)
  }
  height_target_range = c(0, 1)
  if (!is.null(heightmap)) {
    height_vals = as.numeric(heightmap)
    height_vals = height_vals[is.finite(height_vals)]
    if (length(height_vals) > 1) {
      height_target_range = range(height_vals)
      if (identical(height_target_range[1], height_target_range[2])) {
        height_target_range = c(0, 1)
      }
    }
  }
  list(
    height_scale = transform_info$height_scale,
    height_aes = transform_info$height_aes,
    height_inverted = isTRUE(transform_info$height_inverted),
    height_range = transform_info$height_range,
    height_target_range = height_target_range
  )
}

map_scene_altitudes = function(
  values,
  height_transform,
  reference_values = values
) {
  if (is.null(values) || is.null(height_transform)) {
    return(values)
  }
  reference_values = suppressWarnings(as.numeric(reference_values))
  reference_values = reference_values[is.finite(reference_values)]
  if (length(unique(reference_values)) <= 1) {
    return(values)
  }
  missing_vals = is.na(values)
  normalized_height = scales::rescale(
    values,
    to = height_transform$height_target_range,
    from = range(reference_values)
  )
  normalized_height[missing_vals] = NA_real_
  as.numeric(normalized_height)
}

transform_scene_altitudes = function(
  values,
  extent = NULL,
  heightmap = NULL,
  reference_values = values
) {
  height_transform = get_scene_height_transform(
    heightmap = heightmap,
    extent = extent
  )
  if (is.null(height_transform)) {
    return(values)
  }
  map_scene_altitudes(
    values,
    height_transform = height_transform,
    reference_values = reference_values
  )
}

normalize_scene_zaxis_args = function(
  zaxis_args = list(),
  altitude = NULL,
  extent = NULL,
  heightmap = NULL
) {
  if (length(zaxis_args) == 0 || is.null(altitude)) {
    return(zaxis_args)
  }
  height_transform = get_scene_height_transform(
    heightmap = heightmap,
    extent = extent
  )
  if (is.null(height_transform)) {
    return(zaxis_args)
  }
  altitude_vals = suppressWarnings(as.numeric(altitude))
  altitude_vals = altitude_vals[is.finite(altitude_vals)]
  if (length(unique(altitude_vals)) <= 1) {
    return(zaxis_args)
  }
  raw_breaks = zaxis_args$zaxis_breaks
  if (is.null(raw_breaks)) {
    raw_breaks = pretty(range(altitude_vals), n = 4)
    raw_breaks = raw_breaks[is.finite(raw_breaks)]
  }
  if (is.null(zaxis_args$zaxis_labels)) {
    zaxis_args$zaxis_labels = format(
      raw_breaks,
      trim = TRUE,
      scientific = FALSE
    )
  }
  zaxis_args$zaxis_breaks = transform_scene_altitudes(
    raw_breaks,
    extent = extent,
    heightmap = heightmap,
    reference_values = altitude_vals
  )
  zaxis_args
}

capture_plot_gg_panel_info = function(
  ggplot_grob,
  ggplot_build_obj,
  original_width_px,
  original_height_px
) {
  panel_layout = ggplot_grob$layout[grepl("^panel", ggplot_grob$layout$name), ]
  panel_params = ggplot_build_obj$layout$panel_params
  build_layout = ggplot_build_obj$layout$layout

  if (!nrow(panel_layout)) {
    return(NULL)
  }
  if (nrow(build_layout) == 1 && any(panel_layout$name == "panel")) {
    panel_layout = panel_layout[
      match("panel", panel_layout$name),
      ,
      drop = FALSE
    ]
  } else if (all(c("ROW", "COL") %in% colnames(build_layout))) {
    expected_panel_names = paste0(
      "panel-",
      as.integer(as.character(build_layout$COL)),
      "-",
      as.integer(as.character(build_layout$ROW))
    )
    panel_layout = panel_layout[
      match(expected_panel_names, panel_layout$name),
      ,
      drop = FALSE
    ]
  }
  if (anyNA(panel_layout$name)) {
    warning(
      "Unable to cache ggplot extent: could not match panel grobs to ggplot layout."
    )
    return(NULL)
  }
  if (nrow(panel_layout) != length(panel_params)) {
    warning(
      "Unable to cache ggplot extent: panel layout and panel parameter counts differ."
    )
    return(NULL)
  }

  grid::grid.force()
  panel_info = vector("list", length(panel_params))
  for (i in seq_along(panel_params)) {
    panel_vp = get_ggplot_panel_viewport(panel_layout[i, , drop = FALSE])
    grid::seekViewport(panel_vp)
    bbox_orig = get_device_panel_bbox(original_width_px, original_height_px)
    grid::upViewport(0)

    x_range = tryCatch(
      get_ggplot_panel_range(panel_params[[i]], "x"),
      error = function(e) c(0, 1)
    )
    y_range = tryCatch(
      get_ggplot_panel_range(panel_params[[i]], "y"),
      error = function(e) c(0, 1)
    )

    panel_info[[i]] = data.frame(
      panel = as.integer(as.character(build_layout$PANEL[i])),
      row = if ("ROW" %in% colnames(build_layout)) {
        as.integer(as.character(build_layout$ROW[i]))
      } else {
        NA_integer_
      },
      col = if ("COL" %in% colnames(build_layout)) {
        as.integer(as.character(build_layout$COL[i]))
      } else {
        NA_integer_
      },
      panel_name = panel_layout$name[i],
      viewport_name = panel_vp,
      data_xmin = x_range[1],
      data_xmax = x_range[2],
      data_ymin = y_range[1],
      data_ymax = y_range[2],
      panel_xmin = bbox_orig["xmin"],
      panel_xmax = bbox_orig["xmax"],
      panel_ymin = bbox_orig["ymin"],
      panel_ymax = bbox_orig["ymax"]
    )
  }
  do.call("rbind", panel_info)
}

finalize_plot_gg_panel_info = function(
  panel_info,
  original_width_px,
  original_height_px,
  scene_width_px,
  scene_height_px
) {
  if (is.null(panel_info) || !nrow(panel_info)) {
    return(panel_info)
  }

  panel_info$panel_xmin = panel_info$panel_xmin /
    original_width_px *
    scene_width_px
  panel_info$panel_xmax = panel_info$panel_xmax /
    original_width_px *
    scene_width_px
  panel_info$panel_ymin = panel_info$panel_ymin /
    original_height_px *
    scene_height_px
  panel_info$panel_ymax = panel_info$panel_ymax /
    original_height_px *
    scene_height_px

  for (i in seq_len(nrow(panel_info))) {
    extent_x = expand_ggplot_x_extent(
      c(panel_info$data_xmin[i], panel_info$data_xmax[i]),
      c(panel_info$panel_xmin[i], panel_info$panel_xmax[i]),
      scene_width_px
    )
    extent_y = expand_ggplot_y_extent(
      c(panel_info$data_ymin[i], panel_info$data_ymax[i]),
      c(panel_info$panel_ymin[i], panel_info$panel_ymax[i]),
      scene_height_px
    )
    panel_info$extent_xmin[i] = extent_x["xmin"]
    panel_info$extent_xmax[i] = extent_x["xmax"]
    panel_info$extent_ymin[i] = extent_y["ymin"]
    panel_info$extent_ymax[i] = extent_y["ymax"]
  }
  panel_info
}
