#'@keywords internal
#'@noRd
get_ggplot_extent = function(heightmap = NULL, panel = NULL) {
	panel_info = NULL
	if (!is.null(heightmap)) {
		panel_info = attr(heightmap, "ggplot_panel_info", exact = TRUE)
	}
	if (is.null(panel_info)) {
		panel_info = get0(
			"plot_gg_panel_info",
			envir = ray_cache_scene_envir,
			inherits = FALSE
		)
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
		transform_info = get0(
			"plot_gg_transform_info",
			envir = ray_cache_scene_envir,
			inherits = FALSE
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
		panel_info = get0(
			"plot_gg_panel_info",
			envir = ray_cache_scene_envir,
			inherits = FALSE
		)
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
	panel_matches = which(vapply(seq_len(nrow(panel_info)), function(i) {
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
	}, logical(1)))
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
	panel_info = get_cached_plot_gg_panel_info(heightmap = heightmap, default = NULL)
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
	transform_info = get_cached_plot_gg_transform_info(heightmap = heightmap, default = NULL)
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
	error_if_missing = FALSE
) {
	transform_info = get_cached_plot_gg_transform_info(heightmap = heightmap, default = NULL)
	if (is.null(transform_info)) {
		return(NULL)
	}
	if (is.null(panel)) {
		panel = infer_plot_gg_panel(
			extent = extent,
			heightmap = heightmap,
			transform_info = transform_info
		)
	}
	if (is.null(panel)) {
		if (isTRUE(error_if_missing)) {
			stop(
				"Could not determine which ggplot panel to use from the cached scene metadata."
			)
		}
		return(NULL)
	}
	get_plot_gg_panel_transform_context(
		transform_info = transform_info,
		panel = panel,
		heightmap = heightmap
	)
}

auto_transform_scene_xy = function(
	x,
	y,
	extent = NULL,
	heightmap = NULL,
	panel = NULL,
	crs = NULL
) {
	transform_context = get_scene_transform_context(
		extent = extent,
		heightmap = heightmap,
		panel = panel,
		error_if_missing = FALSE
	)
	if (is.null(transform_context)) {
		return(list(
			x = x,
			y = y,
			extent = extent,
			panel = NULL,
			transformed = FALSE
		))
	}
	transformed_coords = transform_ggplot_xy_with_context(
		x_vals = x,
		y_vals = y,
		transform_context = transform_context,
		crs = crs
	)
	list(
		x = transformed_coords$long,
		y = transformed_coords$lat,
		extent = transform_context$transformed_extent,
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
	segmentize_df_max_length = NULL
) {
	transform_context = get_scene_transform_context(
		extent = extent,
		heightmap = heightmap,
		panel = panel,
		error_if_missing = FALSE
	)
	if (is.null(transform_context)) {
		return(list(
			object = sf_object,
			extent = extent,
			panel = NULL,
			transformed = FALSE
		))
	}
	transformed_object = transform_ggplot_sf(
		sf_object = sf_object,
		panel = transform_context$panel,
		heightmap = heightmap,
		crs = crs,
		segmentize_df_max_length = segmentize_df_max_length
	)
	transformed_extent = attr(transformed_object, "extent", exact = TRUE)
	if (is.null(transformed_extent)) {
		transformed_extent = transform_context$transformed_extent
	}
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
		y = if (has_lat) lat else y
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
	heightmap = NULL
) {
	panel_table = transform_info$layout
	if (!nrow(panel_table)) {
		stop("No ggplot panel transformation metadata found.")
	}
	if (is.null(panel)) {
		if (nrow(panel_table) != 1) {
			stop("`panel` must be supplied for faceted ggplots.")
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

transform_ggplot_xy_with_context = function(
	x_vals,
	y_vals,
	transform_context,
	crs = NULL
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
		if (!is.null(crs)) {
			if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
				stop("`sf` package required when using `crs` with coord_sf().")
			}
			target_crs = panel_params$crs
			if (is.null(target_crs)) {
				target_crs = panel_params$default_crs
			}
			point_sf = sf::st_as_sf(
				data.frame(x = as.numeric(x_vals), y = as.numeric(y_vals)),
				coords = c("x", "y"),
				crs = crs
			)
			point_sf = sf::st_transform(point_sf, target_crs)
			point_coords = sf::st_coordinates(point_sf)
			x_vals = point_coords[, 1]
			y_vals = point_coords[, 2]
		}
		coord_input = data.frame(
			x = as.numeric(x_vals),
			y = as.numeric(y_vals),
			PANEL = panel
		)
	} else {
		scale_x_index = panel_table$scale_x[panel_index]
		scale_y_index = panel_table$scale_y[panel_index]
		x_scale = transform_context$panel_scales_x[[scale_x_index]]
		y_scale = transform_context$panel_scales_y[[scale_y_index]]
		if (x_scale$is_discrete()) {
			x_transformed = as.numeric(x_scale$map(x_vals))
		} else {
			x_transformed = as.numeric(
				x_scale$transform_df(data.frame(x = x_vals))[["x"]]
			)
		}
		if (y_scale$is_discrete()) {
			y_transformed = as.numeric(y_scale$map(y_vals))
		} else {
			y_transformed = as.numeric(
				y_scale$transform_df(data.frame(y = y_vals))[["y"]]
			)
		}
		coord_input = data.frame(
			x = x_transformed,
			y = y_transformed,
			PANEL = panel
		)
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
	data.frame(
		long = map_from_panel_npc(transformed$x, x_range),
		lat = map_from_panel_npc(transformed$y, y_range)
	)
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
	input_class = if (inherits(sf_object, "sf")) {
		"sf"
	} else if (inherits(sf_object, "sfc")) {
		"sfc"
	} else if (inherits(sf_object, "sfg")) {
		"sfg"
	} else {
		stop("`sf_object` must be an `sf`, `sfc`, or `sfg` object.")
	}
	if (input_class == "sf") {
		sf_data = sf_object
	} else if (input_class == "sfc") {
		sf_data = sf::st_sf(geometry = sf_object)
	} else {
		sf_data = sf::st_sf(geometry = sf::st_sfc(sf_object))
	}
	sf_data = suppressWarnings(sf::st_zm(sf_data, drop = TRUE, what = "ZM"))
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
	if (inherits(transform_context$coord_obj, "CoordSf")) {
		if (!is.null(crs)) {
			input_crs = sf::st_crs(crs)
			existing_crs = sf::st_crs(sf_data)
			if (!is.na(existing_crs) &&
				!is.na(input_crs) &&
				!identical(existing_crs$wkt, input_crs$wkt)) {
				warning("Replacing CRS metadata on `sf_object` with `crs`.")
			}
			sf_data = suppressWarnings(sf::st_set_crs(sf_data, input_crs))
		}
		sf_crs = sf::st_crs(sf_data)
		if (!is.na(sf_crs)) {
			crs = sf_crs
		}
	} else if (!is.null(crs)) {
		warning("`crs` is only used when the plot uses `coord_sf()`.")
	}

	transform_matrix = function(mat) {
		if (!is.matrix(mat) || ncol(mat) < 2) {
			stop("Could not transform geometry: expected matrix coordinates.")
		}
		if (!nrow(mat)) {
			return(mat[, 1:2, drop = FALSE])
		}
		xy = transform_ggplot_xy_with_context(
			x_vals = mat[, 1],
			y_vals = mat[, 2],
			transform_context = transform_context,
			crs = crs
		)
		out = cbind(xy$long, xy$lat)
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
			coords = as.numeric(unclass(geom))[1:2]
			xy = transform_ggplot_xy_with_context(
				x_vals = coords[1],
				y_vals = coords[2],
				transform_context = transform_context,
				crs = crs
			)
			return(sf::st_point(c(xy$long[1], xy$lat[1])))
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
	output_crs = NULL
	if (inherits(transform_context$coord_obj, "CoordSf")) {
		output_crs = transform_context$panel_params$crs
		if (is.null(output_crs)) {
			output_crs = transform_context$panel_params$default_crs
		}
	}
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
	scene_zscale = get0("scene_zscale", envir = ray_cache_scene_envir, inherits = FALSE)
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
	scene_zscale_label = get0(
		"scene_zscale_label",
		envir = ray_cache_scene_envir,
		inherits = FALSE
	)
	if (is.null(scene_zscale_label)) {
		return(default)
	}
	scene_zscale_label
}

cache_scene_heightmap = function(heightmap = NULL, label = NULL) {
	assign("scene_heightmap", heightmap, envir = ray_cache_scene_envir)
	assign("scene_heightmap_label", label, envir = ray_cache_scene_envir)
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
	scene_extent = get0(
		"scene_extent",
		envir = ray_cache_scene_envir,
		inherits = FALSE
	)
	if (is.null(scene_extent)) {
		return(default)
	}
	scene_extent
}

get_scene_extent_label = function(default = NULL) {
	scene_extent_label = get0(
		"scene_extent_label",
		envir = ray_cache_scene_envir,
		inherits = FALSE
	)
	if (is.null(scene_extent_label)) {
		return(default)
	}
	scene_extent_label
}

get_scene_crs = function(default = NULL) {
	scene_crs = get0(
		"scene_crs",
		envir = ray_cache_scene_envir,
		inherits = FALSE
	)
	if (is.null(scene_crs)) {
		return(default)
	}
	scene_crs
}

get_scene_crs_label = function(default = NULL) {
	scene_crs_label = get0(
		"scene_crs_label",
		envir = ray_cache_scene_envir,
		inherits = FALSE
	)
	if (is.null(scene_crs_label)) {
		return(default)
	}
	scene_crs_label
}

get_scene_heightmap = function(default = NULL) {
	scene_heightmap = get0(
		"scene_heightmap",
		envir = ray_cache_scene_envir,
		inherits = FALSE
	)
	if (is.null(scene_heightmap)) {
		return(default)
	}
	scene_heightmap
}

get_scene_heightmap_label = function(default = NULL) {
	scene_heightmap_label = get0(
		"scene_heightmap_label",
		envir = ray_cache_scene_envir,
		inherits = FALSE
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

emit_scene_cache_message = function(caller, argument_name, cache_name, cache_label = NULL) {
	if (is.null(caller) || !nzchar(caller)) {
		return(invisible(NULL))
	}
	cache_label = format_scene_cache_label(cache_label)
	if (is.null(cache_label)) {
		cat(sprintf(
			"%s(): using cached `%s` from `%s`.",
			caller,
			argument_name,
			cache_name
		), "\n")
	} else {
		cat(sprintf(
			"%s(): using cached `%s` from `%s` (%s).",
			caller,
			argument_name,
			cache_name,
			cache_label
		), "\n")
	}
	invisible(NULL)
}

resolve_scene_render_zscale = function(
	zscale = 1,
	zscale_missing = FALSE,
	caller = NULL
) {
	cached_scene_zscale = get_scene_zscale(default = NA_real_)
	if (isTRUE(zscale_missing) && is.finite(cached_scene_zscale) && cached_scene_zscale > 0) {
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

resolve_scene_render_heightmap = function(heightmap = NULL, caller = NULL) {
	if (!is.null(heightmap)) {
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

resolve_scene_render_extent = function(
	extent = NULL,
	heightmap = NULL,
	caller = NULL,
	allow_ggplot_extent = TRUE,
	allow_scene_extent = TRUE,
	error_if_missing = TRUE
) {
	if (!is.null(extent)) {
		return(canonicalize_plot_gg_extent(
			extent = extent,
			heightmap = heightmap
		))
	}

	if (isTRUE(allow_ggplot_extent)) {
		gg_extent = tryCatch(
			get_ggplot_extent(heightmap = heightmap),
			error = function(e) NULL
		)
		if (is.list(gg_extent) && !is.data.frame(gg_extent)) {
			if (length(gg_extent) > 0) {
				gg_extent = gg_extent[[1]]
			} else {
				gg_extent = NULL
			}
		}
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

	if (isTRUE(error_if_missing)) {
		stop(
			"Could not determine `extent`. Pass `extent` explicitly, or use a scene with cached extent metadata."
		)
	}
	NULL
}

build_plot_gg_transform_info = function(
	ggplot_build_obj,
	height_scale = NULL,
	height_aes = NULL,
	height_is_mapped = FALSE,
	height_inverted = FALSE
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
		height_aes = height_aes,
		height_is_mapped = isTRUE(height_is_mapped),
		height_inverted = isTRUE(height_inverted),
		height_range = if (!is.null(height_scale$range$range)) {
			height_scale$range$range
		} else {
			NULL
		}
	)
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
