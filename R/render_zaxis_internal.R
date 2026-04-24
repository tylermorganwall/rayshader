#' Render Z Axis
#'
#' @keywords internal
render_zaxis_internal = function(
	zaxis = FALSE,
	extent = NULL,
	zscale = 1,
	heightmap = NULL,
	zaxis_location = "auto",
	zaxis_breaks = NULL,
	zaxis_labels = NULL,
	zaxis_color = "black",
	zaxis_linewidth = 2,
	zaxis_text_offset = 1.5,
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
	for (tag in c("zaxis_axis", "zaxis_ticks", "zaxis_labels")) {
		try(rgl::pop3d(tag = tag), silent = TRUE)
	}

	xmin = extent_vals["xmin"]
	xmax = extent_vals["xmax"]
	ymin = extent_vals["ymin"]
	ymax = extent_vals["ymax"]
	xrange = max(1e-8, xmax - xmin)
	yrange = max(1e-8, ymax - ymin)
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
		xrange = max(1e-8, pxmax - pxmin)
		yrange = max(1e-8, pymax - pymin)
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
			valid = stats::complete.cases(surface_vertices[, c(1, 2, 3), drop = FALSE])
			if (!any(valid)) {
				surface_vertices = NULL
			} else {
				surface_vertices = surface_vertices[valid, , drop = FALSE]
			}
		}
	}

	anchor_vec_2d = c(anchor_xyz[1] - center_xyz[1], anchor_xyz[3] - center_xyz[3])
	anchor_vec_norm = sqrt(sum(anchor_vec_2d^2))
	if (is.finite(anchor_vec_norm) && anchor_vec_norm > 0) {
		outside_unit_2d = anchor_vec_2d / anchor_vec_norm
	} else {
		outside_unit_2d = c(1, 0)
	}
	axis_offset = anchor_vec_norm * zaxis_corner_offset
	anchor_xyz[1] = anchor_xyz[1] + outside_unit_2d[1] * axis_offset
	anchor_xyz[3] = anchor_xyz[3] + outside_unit_2d[2] * axis_offset

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
			altitude_range = c(altitude_range[1] - span / 2, altitude_range[2] + span / 2)
		}
		zaxis_breaks = pretty(altitude_range, n = 4)
		if (length(zaxis_breaks) < 2) {
			zaxis_breaks = altitude_range
		}
	} else {
		zaxis_breaks = as.numeric(zaxis_breaks)
		if (any(is.na(zaxis_breaks))) {
			stop("`zaxis_breaks` must be numeric.")
		}
		zaxis_breaks = sort(unique(zaxis_breaks))
	}

	if (is.null(zaxis_labels)) {
		zaxis_labels = format(zaxis_breaks, trim = TRUE, scientific = FALSE)
	} else {
		if (length(zaxis_labels) != length(zaxis_breaks)) {
			stop("`zaxis_labels` must be the same length as `zaxis_breaks`.")
		}
		zaxis_labels = as.character(zaxis_labels)
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

	tick_len = 0.03 * max(xrange, yrange)
	tick_marker_size = if (is.null(zaxis_tick_size)) {
		max(4, zaxis_linewidth * 1.25)
	} else {
		zaxis_tick_size
	}
	outside_unit = outside_unit_2d
	side_sign = if (outside_unit[1] >= 0) 1 else -1
	# Keep text extending away from the axis side instead of centered on the anchor point.
	text_adj_x = if (side_sign > 0) 0 else 1
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
		text_x = anchor_xyz[1] + outside_unit[1] * tick_len * zaxis_text_offset
		text_z = anchor_xyz[3] + outside_unit[2] * tick_len * zaxis_text_offset
		rgl::texts3d(
			x = text_x,
			y = y_vals[i],
			z = text_z,
			texts = text_labels[i],
			color = zaxis_color,
			adj = c(text_adj_x, 0.5),
			cex = 0.8,
			tag = "zaxis_labels"
		)
	}

	invisible(
		list(
			location = zaxis_location,
			breaks = zaxis_breaks,
			labels = zaxis_labels
		)
	)
}
