#'@title Render Scale Bar
#'
#'@description Places a scale bar on the map in 3D.
#'
#
#' @param limits Default `NULL`. The distances represented by the scale bar. If
#' `NULL`, rayshader uses cached scene dimensions to generate pretty breaks for
#' a bar approximately half the longer side of the map. Otherwise, the maximum
#' value is the total represented distance. Must be non-negative.
#' @param position Default `NULL`. Scale-bar side: `"N"`, `"E"`, `"S"`, or
#' `"W"`. If `NULL`, the scale bar follows the longer map axis, using `"S"` for
#' wider maps and `"W"` for taller maps.
#'@param y Default `NULL`. The height of the scale bar, automatically calculated if `NULL`.
#' @param scale_length Default `NULL`. Length of the scale bar relative to the
#' side selected by `position`. If `NULL`, this is calculated from `limits` and
#' cached spatial metadata. A scalar starts at the beginning of the side; a
#' length-2 vector specifies the start and end proportions.
#' @param label_unit Default `NULL`. Distance-unit suffix. If `NULL`, rayshader
#' uses the projected CRS unit, or kilometres for geographic coordinates and
#' metres when kilometres would be too large for the scene.
#'@param offset Default `NULL`. The distance away from the edge to place the scale bar.
#'If `NULL`, automatically calculated.
#'@param segments Default `10`. Number of colored segments in the scalebar.
#'@param radius Default `NULL`. The radius of the cylinder representing the scale bar.
#'If `NULL`, automatically calculated.
#'@param color_first Default `darkred`. Primary color in the scale bar.
#'@param color_second Default `grey90`. Seconary color in the scale bar.
#'@param color_text Default `black`. Color of the text.
#'@param text_switch_side Default `FALSE`. Switches the order of the text.
#'@param text_x_offset Default `0`. Distance offset for text in the x direction.
#'@param text_y_offset Default `0`. Distance offset for text in the y direction.
#'@param text_z_offset Default `0`. Distance offset for text in the z direction.
#' @param clear_previous Default `FALSE`. Clears any existing scale bars before
#' rendering. A clear-only call returns without rendering a replacement.
#'
#' @return The resolved scale-bar specification, invisibly.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Add an automatically sized scale bar to the montereybay_spatial dataset
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration = 20) |>
#'  plot_3d(theta=45, vertical_exaggeration = 4, water=TRUE)
#'render_scalebar()
#'render_snapshot()
#'#This function works with `render_highquality()`
#'render_highquality(lightdirection = 250, lightaltitude = 40, samples = 16)
#'render_scalebar(clear_previous = TRUE)
#'#We can change the position by specifying a cardinal direction to `position`, and the
#'#color by setting `color_first` and `color_second`
#'
#'render_scalebar(limits=c(0,80), label_unit = "km", position = "N",
#'                color_first = "darkgreen", color_second = "lightgreen")
#'render_snapshot()
#'render_scalebar(clear_previous = TRUE)
#'#And switch the orientation by setting `text_switch_side = TRUE`
#'render_scalebar(limits=c(0,80), label_unit = "km", position = "N", text_switch_side = TRUE,
#'                color_first = "darkgreen", color_second = "lightgreen")
#'render_snapshot()
#'render_scalebar(clear_previous = TRUE)
#'#We can add additional breaks by specifying additional distances in `limits`
#'
#'render_scalebar(limits=c(0,40,80), label_unit = "km")
#'render_snapshot()
#'render_scalebar(clear_previous = TRUE)
#'#We can also manually specify the height by setting the `y` argument:
#'
#'render_scalebar(limits=c(0,40,80), y=-70, label_unit = "km")
#'render_snapshot()
#'render_scalebar(clear_previous = TRUE)
#'#Here we change the total size by specifying a start and end point along the side,
#'#and set the number of colored `segments`:
#'
#'render_scalebar(limits=c(0,20, 40), segments = 4, scale_length = c(0.5,1), label_unit = "km")
#'render_scalebar(limits=c(0,20, 40), segments = 4, position = "N", text_switch_side = TRUE,
#'                scale_length = c(0.25,0.75), label_unit = "km")
#'render_snapshot()
#'render_scalebar(clear_previous = TRUE)
#'#Change the radius of the scale bar with `radius`. Here, the autopositioning doesn't work well with
#'#the labels, so we provide additional offsets with `text_y_offset` and `text_x_offset` to fix it.
#'
#'render_scalebar(limits=c(0,20, 40), segments = 4, scale_length = c(0.5,1),
#'                label_unit = "km", radius=10,text_y_offset=-20,text_x_offset=20)
#'render_snapshot()
render_scalebar = function(
  limits = NULL,
  position = NULL,
  y = NULL,
  segments = 10,
  scale_length = NULL,
  label_unit = NULL,
  offset = NULL,
  radius = NULL,
  color_first = "darkred",
  color_second = "grey80",
  color_text = "black",
  text_switch_side = FALSE,
  text_x_offset = 0,
  text_y_offset = 0,
  text_z_offset = 0,
  clear_previous = FALSE
) {
  if (
    is_render_clear_only_call(
      clear_previous,
      match.call(),
      function() {
        ids = get_ids_with_labels(c(
          "scalebar_col1",
          "scalebar_col2",
          "text_scalebar"
        ))$id
        rgl::pop3d(id = ids)
      }
    )
  ) {
    return(invisible(NULL))
  }
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if (!is.null(limits)) {
    limits = suppressWarnings(as.numeric(limits))
    if (
      length(limits) < 1L ||
        any(!is.finite(limits)) ||
        any(limits < 0)
    ) {
      stop("limits must be greater than (or equal to) 0", call. = FALSE)
    }
    if (max(limits) <= 0) {
      stop("`limits` must include at least one positive value.", call. = FALSE)
    }
  }

  id_base = get_ids_with_labels("surface")$id
  if (length(id_base) == 0) {
    id_base = get_ids_with_labels("surface_tris")$id
  }
  fullverts = rgl::rgl.attrib(id_base, "vertices")
  xyz_range = apply(fullverts, 2, range, na.rm = TRUE)
  widths = xyz_range[2, c(1, 3)] - xyz_range[1, c(1, 3)]
  scalebar_specification = resolve_render_scalebar_specification(
    limits = limits,
    position = position,
    scale_length = scale_length,
    label_unit = label_unit,
    scene_info = resolve_render_scalebar_scene_info(widths)
  )
  limits = scalebar_specification$limits
  position = scalebar_specification$position
  scale_length = scalebar_specification$scale_length
  label_unit = scalebar_specification$label_unit

  if (is.null(offset)) {
    if (position %in% c("N", "S")) {
      offset = widths[1] / 10
    } else if (position %in% c("E", "W")) {
      offset = widths[2] / 10
    }
  }
  if (is.null(radius)) {
    radius = offset / 8
  }
  if (is.null(y)) {
    y = xyz_range[2, 2]
  }
  if (position %in% c("N", "S")) {
    xstart = xyz_range[2, 1] *
      scale_length[2] +
      (1 - scale_length[2]) * xyz_range[1, 1]
    xend = xyz_range[1, 1] *
      (1 - scale_length[1]) +
      xyz_range[2, 1] * scale_length[1]
  } else {
    xstart = xyz_range[2, 1]
    xend = xyz_range[1, 1]
  }
  if (position %in% c("E", "W")) {
    zstart = xyz_range[2, 3] *
      scale_length[2] +
      (1 - scale_length[2]) * xyz_range[1, 3]
    zend = xyz_range[1, 3] *
      (1 - scale_length[1]) +
      xyz_range[2, 3] * scale_length[1]
  } else {
    zstart = xyz_range[2, 3]
    zend = xyz_range[1, 3]
  }
  x_break_length = (xend - xstart) / segments
  z_break_length = (zend - zstart) / segments
  meshlist1 = list()
  meshlist2 = list()
  counter1 = 1
  counter2 = 1
  if (position == "N") {
    temp = xstart
    for (i in 1:segments) {
      if (i %% 2 == 1) {
        meshlist1[[counter1]] = rgl::cylinder3d(
          center = matrix(
            c(temp, temp + x_break_length, y, y, zend - offset, zend - offset),
            ncol = 3,
            nrow = 2
          ),
          radius = radius,
          closed = -2
        )
        counter1 = counter1 + 1
        temp = temp + x_break_length
      } else {
        meshlist2[[counter2]] = rgl::cylinder3d(
          center = matrix(
            c(temp, temp + x_break_length, y, y, zend - offset, zend - offset),
            ncol = 3,
            nrow = 2
          ),
          radius = radius,
          closed = -2
        )
        counter2 = counter2 + 1
        temp = temp + x_break_length
      }
    }
  } else if (position == "W") {
    temp = zstart
    for (i in 1:segments) {
      if (i %% 2 == 1) {
        meshlist1[[counter1]] = rgl::cylinder3d(
          center = matrix(
            c(xend - offset, xend - offset, y, y, temp, temp + z_break_length),
            ncol = 3,
            nrow = 2
          ),
          radius = radius,
          closed = -2
        )
        counter1 = counter1 + 1
        temp = temp + z_break_length
      } else {
        meshlist2[[counter2]] = rgl::cylinder3d(
          center = matrix(
            c(xend - offset, xend - offset, y, y, temp, temp + z_break_length),
            ncol = 3,
            nrow = 2
          ),
          radius = radius,
          closed = -2
        )
        counter2 = counter2 + 1
        temp = temp + z_break_length
      }
    }
  } else if (position == "S") {
    temp = xstart
    for (i in 1:segments) {
      if (i %% 2 == 1) {
        meshlist1[[counter1]] = rgl::cylinder3d(
          center = matrix(
            c(
              temp,
              temp + x_break_length,
              y,
              y,
              zstart + offset,
              zstart + offset
            ),
            ncol = 3,
            nrow = 2
          ),
          radius = radius,
          closed = -2
        )
        counter1 = counter1 + 1
        temp = temp + x_break_length
      } else {
        meshlist2[[counter2]] = rgl::cylinder3d(
          center = matrix(
            c(
              temp,
              temp + x_break_length,
              y,
              y,
              zstart + offset,
              zstart + offset
            ),
            ncol = 3,
            nrow = 2
          ),
          radius = radius,
          closed = -2
        )
        counter2 = counter2 + 1
        temp = temp + x_break_length
      }
    }
  } else if (position == "E") {
    temp = zstart
    for (i in 1:segments) {
      if (i %% 2 == 1) {
        meshlist1[[counter1]] = rgl::cylinder3d(
          center = matrix(
            c(
              xstart + offset,
              xstart + offset,
              y,
              y,
              temp,
              temp + z_break_length
            ),
            ncol = 3,
            nrow = 2
          ),
          radius = radius,
          closed = -2
        )
        counter1 = counter1 + 1
        temp = temp + z_break_length
      } else {
        meshlist2[[counter2]] = rgl::cylinder3d(
          center = matrix(
            c(
              xstart + offset,
              xstart + offset,
              y,
              y,
              temp,
              temp + z_break_length
            ),
            ncol = 3,
            nrow = 2
          ),
          radius = radius,
          closed = -2
        )
        counter2 = counter2 + 1
        temp = temp + z_break_length
      }
    }
  }
  shapelist3d(
    meshlist1,
    lit = FALSE,
    tag = "scalebar_col1",
    color = color_first,
    plot = TRUE
  )
  shapelist3d(
    meshlist2,
    lit = FALSE,
    tag = "scalebar_col2",
    color = color_second,
    plot = TRUE
  )

  max_distance = max(limits)
  breakpoints = limits / max_distance
  for (i in seq_along(breakpoints)) {
    if (position == "N") {
      if (text_switch_side) {
        break_dist = breakpoints[i] * xend + (1 - breakpoints[i]) * xstart
        text3d(
          x = break_dist + text_x_offset,
          y = y + text_y_offset + radius * 3,
          z = zend - offset + text_z_offset - radius * 5,
          texts = paste0(c(as.character(limits[i]), label_unit), collapse = ""),
          color = color_text,
          tag = "text_scalebar"
        )
      } else {
        break_dist = breakpoints[i] * xstart + (1 - breakpoints[i]) * xend
        text3d(
          x = break_dist + text_x_offset,
          y = y + text_y_offset + radius * 3,
          z = zend - offset + text_z_offset - radius * 5,
          texts = paste0(c(as.character(limits[i]), label_unit), collapse = ""),
          color = color_text,
          tag = "text_scalebar"
        )
      }
    } else if (position == "W") {
      if (text_switch_side) {
        break_dist = breakpoints[i] * zstart + (1 - breakpoints[i]) * zend
        text3d(
          x = xend - offset + text_x_offset - radius * 5,
          y = y + text_y_offset + radius * 3,
          z = break_dist + text_z_offset,
          texts = paste0(c(as.character(limits[i]), label_unit), collapse = ""),
          color = color_text,
          tag = "text_scalebar"
        )
      } else {
        break_dist = breakpoints[i] * zend + (1 - breakpoints[i]) * zstart
        text3d(
          x = xend - offset + text_x_offset - radius * 5,
          y = y + text_y_offset + radius * 3,
          z = break_dist + text_z_offset,
          texts = paste0(c(as.character(limits[i]), label_unit), collapse = ""),
          color = color_text,
          tag = "text_scalebar"
        )
      }
    } else if (position == "S") {
      if (text_switch_side) {
        break_dist = breakpoints[i] * xstart + (1 - breakpoints[i]) * xend
        text3d(
          x = break_dist + text_x_offset,
          y = y + text_y_offset + radius * 3,
          z = zstart + offset + text_z_offset + radius * 5,
          texts = paste0(c(as.character(limits[i]), label_unit), collapse = ""),
          color = color_text,
          tag = "text_scalebar"
        )
      } else {
        break_dist = breakpoints[i] * xend + (1 - breakpoints[i]) * xstart
        text3d(
          x = break_dist + text_x_offset,
          y = y + text_y_offset + radius * 3,
          z = zstart + offset + text_z_offset + radius * 5,
          texts = paste0(c(as.character(limits[i]), label_unit), collapse = ""),
          color = color_text,
          tag = "text_scalebar"
        )
      }
    } else if (position == "E") {
      if (text_switch_side) {
        break_dist = breakpoints[i] * zend + (1 - breakpoints[i]) * zstart
        text3d(
          x = xstart + offset + text_x_offset + radius * 5,
          y = y + text_y_offset + radius * 3,
          z = break_dist + text_z_offset,
          texts = paste0(c(as.character(limits[i]), label_unit), collapse = ""),
          color = color_text,
          tag = "text_scalebar"
        )
      } else {
        break_dist = breakpoints[i] * zstart + (1 - breakpoints[i]) * zend
        text3d(
          x = xstart + offset + text_x_offset + radius * 5,
          y = y + text_y_offset + radius * 3,
          z = break_dist + text_z_offset,
          texts = paste0(c(as.character(limits[i]), label_unit), collapse = ""),
          color = color_text,
          tag = "text_scalebar"
        )
      }
    }
  }
  invisible(scalebar_specification)
}

#' Resolve cached scale-bar scene measurements
#'
#' @param widths Two-value x/z size of the rendered terrain.
#'
#' @return Cached scene dimensions and unit metadata.
#' @keywords internal
resolve_render_scalebar_scene_info = function(widths) {
  widths = suppressWarnings(as.numeric(widths))
  if (
    length(widths) != 2L ||
      any(!is.finite(widths)) ||
      any(widths <= 0)
  ) {
    stop("Could not determine the rendered map dimensions.", call. = FALSE)
  }

  scene_crs = try_parse_scene_crs(get_scene_crs(default = NULL))
  scene_aspect = get_scene_geographic_aspect()
  meters_per_scene_unit = scene_aspect$cell_meters / scene_aspect$scale
  has_metric_dimensions = all(is.finite(meters_per_scene_unit)) &&
    all(meters_per_scene_unit > 0)

  if (!has_metric_dimensions) {
    scene_heightmap = get_scene_heightmap(default = NULL)
    scene_extent = get_scene_extent(default = NULL)
    if (
      is.matrix(scene_heightmap) &&
        !is.null(scene_extent) &&
        !is.null(scene_crs)
    ) {
      meters_per_scene_unit = calculate_road_path_world_scale(
        heightmap = scene_heightmap,
        extent = scene_extent,
        crs = scene_crs
      )
      has_metric_dimensions = all(is.finite(meters_per_scene_unit)) &&
        all(meters_per_scene_unit > 0)
    }
  }

  list(
    dimensions = if (has_metric_dimensions) {
      widths * meters_per_scene_unit
    } else {
      widths
    },
    metric = has_metric_dimensions,
    crs = scene_crs
  )
}

#' Resolve automatic scale-bar arguments
#'
#' @param limits Scale-bar label breaks.
#' @param position Scale-bar side.
#' @param scale_length Scale-bar side proportions.
#' @param label_unit Distance-unit suffix.
#' @param scene_info Scene measurements returned by
#' `resolve_render_scalebar_scene_info()`.
#'
#' @return Resolved scale-bar arguments and physical distance metadata.
#' @keywords internal
resolve_render_scalebar_specification = function(
  limits = NULL,
  position = NULL,
  scale_length = NULL,
  label_unit = NULL,
  scene_info
) {
  dimensions = suppressWarnings(as.numeric(scene_info$dimensions))
  if (
    length(dimensions) != 2L ||
      any(!is.finite(dimensions)) ||
      any(dimensions <= 0)
  ) {
    stop("Could not determine the rendered map dimensions.", call. = FALSE)
  }

  position_was_automatic = is.null(position)
  if (position_was_automatic) {
    position = if (dimensions[[1L]] >= dimensions[[2L]]) "S" else "W"
  } else if (
    !is.character(position) ||
      length(position) != 1L ||
      is.na(position) ||
      !toupper(position) %in% c("N", "E", "S", "W")
  ) {
    stop(
      "`position` must be one of \"N\", \"E\", \"S\", or \"W\".",
      call. = FALSE
    )
  } else {
    position = toupper(position)
  }

  side_index = if (position %in% c("N", "S")) 1L else 2L
  side_distance = dimensions[[side_index]]
  scale_length_was_automatic = is.null(scale_length)
  if (!scale_length_was_automatic) {
    scale_length = suppressWarnings(as.numeric(scale_length))
    if (
      length(scale_length) < 1L ||
        length(scale_length) > 2L ||
        any(!is.finite(scale_length)) ||
        any(scale_length < 0) ||
        any(scale_length > 1)
    ) {
      stop(
        "`scale_length` must contain one or two values between 0 and 1.",
        call. = FALSE
      )
    }
    if (length(scale_length) == 1L) {
      scale_length = c(0, scale_length)
    }
  }

  default_distance = min(max(dimensions) / 2, side_distance * 0.9)
  requested_distance = if (scale_length_was_automatic) {
    default_distance
  } else {
    abs(diff(scale_length)) * side_distance
  }
  unit_info = resolve_render_scalebar_unit(
    label_unit = label_unit,
    scene_info = scene_info,
    target_distance = requested_distance
  )
  limits_were_automatic = is.null(limits)

  if (limits_were_automatic) {
    if (!is.finite(unit_info$distance_per_unit)) {
      stop(
        "Could not convert the requested `label_unit` to the scene distance units.",
        call. = FALSE
      )
    }
    maximum = requested_distance / unit_info$distance_per_unit
    limits = pretty_render_scalebar_limits(
      maximum,
      shrink = scale_length_was_automatic
    )
  } else {
    limits = suppressWarnings(as.numeric(limits))
    if (
      length(limits) < 1L ||
        any(!is.finite(limits)) ||
        any(limits < 0)
    ) {
      stop("limits must be greater than (or equal to) 0", call. = FALSE)
    }
  }
  if (max(limits) <= 0) {
    stop("`limits` must include at least one positive value.", call. = FALSE)
  }

  represented_distance = if (is.finite(unit_info$distance_per_unit)) {
    max(limits) * unit_info$distance_per_unit
  } else {
    NA_real_
  }
  if (scale_length_was_automatic) {
    if (!is.finite(represented_distance)) {
      stop(
        "Could not convert the requested `label_unit` to the scene distance units.",
        call. = FALSE
      )
    }
    scale_fraction = represented_distance / side_distance
    if (scale_fraction > 1 + sqrt(.Machine$double.eps)) {
      stop(
        "The requested `limits` are longer than the selected map side.",
        call. = FALSE
      )
    }
    scale_fraction = min(scale_fraction, 1)
    scale_length = c(
      (1 - scale_fraction) / 2,
      (1 + scale_fraction) / 2
    )
  }

  list(
    limits = limits,
    position = position,
    scale_length = scale_length,
    label_unit = unit_info$label,
    represented_distance = represented_distance,
    scene_dimensions = dimensions,
    position_was_automatic = position_was_automatic,
    limits_were_automatic = limits_were_automatic,
    scale_length_was_automatic = scale_length_was_automatic
  )
}

#' Resolve a scale-bar display unit
#'
#' @param label_unit Requested unit suffix.
#' @param scene_info Cached scene measurement metadata.
#' @param target_distance Target scale-bar distance in scene measurement units.
#'
#' @return Display label and scene-distance units per displayed unit.
#' @keywords internal
resolve_render_scalebar_unit = function(
  label_unit = NULL,
  scene_info,
  target_distance
) {
  if (!isTRUE(scene_info$metric)) {
    return(list(
      label = if (is.null(label_unit)) "" else as.character(label_unit)[1L],
      distance_per_unit = 1
    ))
  }

  if (!is.null(label_unit)) {
    label_unit = as.character(label_unit)[1L]
    if (!nzchar(trimws(label_unit))) {
      automatic_unit = resolve_render_scalebar_unit(
        label_unit = NULL,
        scene_info = scene_info,
        target_distance = target_distance
      )
      automatic_unit$label = ""
      return(automatic_unit)
    }
    return(list(
      label = label_unit,
      distance_per_unit = render_scalebar_unit_meters(label_unit)
    ))
  }

  scene_crs = try_parse_scene_crs(scene_info$crs)
  is_longlat = !is.null(scene_crs) &&
    requireNamespace("sf", quietly = TRUE) &&
    isTRUE(sf::st_is_longlat(scene_crs))
  if (is_longlat || is.null(scene_crs)) {
    if (target_distance < 1000) {
      return(list(label = "m", distance_per_unit = 1))
    }
    return(list(label = "km", distance_per_unit = 1000))
  }

  crs_unit = scene_crs$units_gdal
  unit_meters = render_scalebar_unit_meters(crs_unit)
  if (!is.finite(unit_meters)) {
    return(list(label = "m", distance_per_unit = 1))
  }
  list(
    label = abbreviate_render_scalebar_unit(crs_unit),
    distance_per_unit = unit_meters
  )
}

#' Generate pretty scale-bar breaks
#'
#' @param maximum Maximum displayed distance.
#' @param shrink Default `TRUE`. Whether to select a pretty endpoint no greater
#' than `maximum`.
#'
#' @return Numeric scale-bar breaks beginning at zero.
#' @keywords internal
pretty_render_scalebar_limits = function(maximum, shrink = TRUE) {
  maximum = suppressWarnings(as.numeric(maximum)[1L])
  if (!is.finite(maximum) || maximum <= 0) {
    stop("Could not calculate positive scale-bar limits.", call. = FALSE)
  }
  breaks = pretty(c(0, maximum), n = 4)
  tolerance = maximum * sqrt(.Machine$double.eps)
  breaks = breaks[breaks >= 0 & breaks <= maximum + tolerance]
  if (isTRUE(shrink)) {
    positive_breaks = breaks[breaks > 0]
    if (length(positive_breaks) > 0L) {
      maximum = max(positive_breaks)
    }
  }
  breaks = pretty(c(0, maximum), n = 4)
  breaks = breaks[breaks >= 0 & breaks <= maximum + tolerance]
  sort(unique(c(0, breaks, maximum)))
}

#' Convert a scale-bar unit to metres
#'
#' @param unit Distance unit name or abbreviation.
#'
#' @return Number of metres per unit, or `NA_real_` when unknown.
#' @keywords internal
render_scalebar_unit_meters = function(unit) {
  if (is.null(unit) || length(unit) == 0L || is.na(unit[[1L]])) {
    return(NA_real_)
  }
  unit = trimws(as.character(unit)[1L])
  normalized_unit = gsub("[ _-]", "", tolower(unit))
  known_unit = switch(
    normalized_unit,
    m = 1,
    meter = 1,
    meters = 1,
    metre = 1,
    metres = 1,
    km = 1000,
    kilometer = 1000,
    kilometers = 1000,
    kilometre = 1000,
    kilometres = 1000,
    ft = 0.3048,
    foot = 0.3048,
    feet = 0.3048,
    ussurveyfoot = 1200 / 3937,
    mi = 1609.344,
    mile = 1609.344,
    miles = 1609.344,
    NA_real_
  )
  if (is.finite(known_unit)) {
    return(known_unit)
  }
  unit_name = gsub(" ", "_", unit, fixed = TRUE)
  tryCatch(
    {
      native_value = units::set_units(1, unit_name, mode = "standard")
      as.numeric(units::set_units(native_value, "m", mode = "standard"))
    },
    error = function(error) NA_real_
  )
}

#' Abbreviate a projected scale-bar unit
#'
#' @param unit CRS distance-unit name.
#'
#' @return Short display label when known, otherwise the original unit.
#' @keywords internal
abbreviate_render_scalebar_unit = function(unit) {
  unit = as.character(unit)[1L]
  normalized_unit = gsub("[ _-]", "", tolower(unit))
  switch(
    normalized_unit,
    meter = ,
    meters = ,
    metre = ,
    metres = "m",
    kilometer = ,
    kilometers = ,
    kilometre = ,
    kilometres = "km",
    foot = ,
    feet = ,
    ussurveyfoot = "ft",
    yard = ,
    yards = "yd",
    mile = ,
    miles = "mi",
    unit
  )
}
