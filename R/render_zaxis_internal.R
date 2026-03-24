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
	zaxis_text_offset = 3,
	zaxis_tick_size = NULL
) {
	if (!isTRUE(zaxis)) {
		return(invisible(NULL))
	}
	if (is.null(extent)) {
		stop("If `zaxis = TRUE`, `extent` must be provided.")
	}

	extent_vals = get_extent(extent)
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
	if (is.na(zaxis_text_offset) || zaxis_text_offset <= 1) {
		stop("`zaxis_text_offset` must be a numeric value greater than 1.")
	}
	if (!is.null(zaxis_tick_size)) {
		zaxis_tick_size = as.numeric(zaxis_tick_size)[1]
		if (is.na(zaxis_tick_size) || zaxis_tick_size <= 0) {
			stop("`zaxis_tick_size` must be a positive number.")
		}
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
		zscale = zscale
	)[1, ]
	center_xyz = transform_into_heightmap_coords(
		extent = extent_vals,
		heightmap = matrix(0, nrow = 2, ncol = 2),
		lat = center_lat,
		long = center_long,
		altitude = 0,
		use_altitude = FALSE,
		zscale = zscale
	)[1, ]

	base_y = NA_real_
	if (!is.null(heightmap)) {
		base_y = tryCatch(
			transform_into_heightmap_coords(
				extent = extent_vals,
				heightmap = heightmap,
				lat = anchor_lat,
				long = anchor_long,
				zscale = zscale
			)[1, 2],
			error = function(e) NA_real_
		)
	}
	if (!is.finite(base_y)) {
		surface_ids = get_ids_with_labels(c("surface", "surface_tris"))$id
		if (length(surface_ids) > 0) {
			surface_vertices = lapply(
				surface_ids,
				function(id) rgl::rgl.attrib(id, "vertices")
			)
			surface_vertices = do.call("rbind", surface_vertices)
			if (is.matrix(surface_vertices) && nrow(surface_vertices) > 0) {
				valid = stats::complete.cases(surface_vertices[,
					c(1, 2, 3),
					drop = FALSE
				])
				if (any(valid)) {
					surface_vertices = surface_vertices[valid, , drop = FALSE]
					d2 = (surface_vertices[, 1] - anchor_xyz[1])^2 +
						(surface_vertices[, 3] - anchor_xyz[3])^2
					base_y = surface_vertices[which.min(d2), 2]
				}
			}
		}
	}
	if (!is.finite(base_y)) {
		base_y = rgl::par3d()$bbox[3]
	}

	if (is.null(zaxis_breaks)) {
		max_altitude = max(0, (rgl::par3d()$bbox[4] - base_y) * zscale)
		if (max_altitude == 0) {
			max_altitude = 1
		}
		zaxis_breaks = pretty(c(0, max_altitude), n = 4)
		zaxis_breaks = zaxis_breaks[
			zaxis_breaks >= 0 &
				zaxis_breaks <= max_altitude + .Machine$double.eps^0.5
		]
		if (length(zaxis_breaks) < 2) {
			zaxis_breaks = c(0, max_altitude)
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

	y_vals = base_y + zaxis_breaks / zscale
	y_min = min(y_vals)
	y_max = max(y_vals)
	if (length(y_vals) == 1 || identical(y_min, y_max)) {
		y_max = y_min + 1 / zscale
	}
	eps_break = .Machine$double.eps^0.5
	nonzero_idx = abs(zaxis_breaks) > eps_break

	tick_len = 0.03 * max(xrange, yrange)
	tick_marker_size = if (is.null(zaxis_tick_size)) {
		max(4, zaxis_linewidth * 1.25)
	} else {
		zaxis_tick_size
	}
	side_sign = if (anchor_xyz[1] >= center_xyz[1]) 1 else -1
	outside_unit = c(side_sign, 0)
	# Keep text extending away from the axis side instead of centered on the anchor point.
	text_adj_x = if (side_sign > 0) 0 else 1
	# Extra whitespace gives a reliable visual gap from the axis in billboarded text mode.
	space_pad = "  "
	text_labels = if (text_adj_x == 1) {
		paste0(zaxis_labels, space_pad)
	} else {
		paste0(space_pad, zaxis_labels)
	}

	for (tag in c("zaxis_axis", "zaxis_ticks", "zaxis_labels")) {
		try(rgl::pop3d(tag = tag), silent = TRUE)
	}

	rgl::segments3d(
		x = c(anchor_xyz[1], anchor_xyz[1]),
		y = c(y_min, y_max),
		z = c(anchor_xyz[3], anchor_xyz[3]),
		color = zaxis_color,
		lwd = zaxis_linewidth,
		tag = "zaxis_axis"
	)

	# Never draw a marker at the zero break.
	if (any(nonzero_idx)) {
		rgl::points3d(
			x = rep(anchor_xyz[1], sum(nonzero_idx)),
			y = y_vals[nonzero_idx],
			z = rep(anchor_xyz[3], sum(nonzero_idx)),
			color = zaxis_color,
			size = tick_marker_size,
			tag = "zaxis_ticks"
		)
	}

	for (i in which(nonzero_idx)) {
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
