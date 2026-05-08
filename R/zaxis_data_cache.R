canonicalize_zaxis_data_source = function(source = "auto") {
	if (is.null(source) || !length(source)) {
		return("auto")
	}
	source = tolower(as.character(source)[1])
	source = gsub("[-_[:space:]]", "", source)
	if (source %in% c("auto", "default")) {
		return("auto")
	}
	if (source %in% c("topographic", "topography", "terrain", "heightmap")) {
		return("topographic")
	}
	if (
		source %in%
			c("polygon", "polygons", "renderpolygons", "beveledpolygon",
				"beveledpolygons", "renderbeveledpolygons")
	) {
		return("polygon")
	}
	if (source %in% c("point", "points", "renderpoints")) {
		return("point")
	}
	if (source %in% c("path", "paths", "renderpath")) {
		return("path")
	}
	if (source %in% c("obj", "object", "objects", "renderobj")) {
		return("obj")
	}
	if (source %in% c("raymesh", "mesh", "meshes", "renderraymesh")) {
		return("raymesh")
	}
	if (source %in% c("tree", "trees", "rendertree")) {
		return("tree")
	}
	if (source %in% c("label", "labels", "renderlabel")) {
		return("label")
	}
	if (source %in% c("building", "buildings", "renderbuildings")) {
		return("building")
	}
	if (source %in% c("cloud", "clouds", "renderclouds")) {
		return("cloud")
	}
	stop(
		"`zaxis_data` must be one of: auto, topographic, polygon, point, ",
		"path, obj, raymesh, tree, label, building, cloud.",
		call. = FALSE
	)
}

cache_scene_zaxis_data = function(
	source,
	raw_values = NULL,
	scene_values = NULL,
	label = NULL
) {
	source = canonicalize_zaxis_data_source(source)
	if (source == "auto") {
		return(invisible(NULL))
	}
	raw_values = suppressWarnings(as.numeric(raw_values))
	scene_values = suppressWarnings(as.numeric(scene_values))
	if (!length(raw_values) || !length(scene_values)) {
		return(invisible(NULL))
	}
	if (length(raw_values) == 1 && length(scene_values) > 1) {
		raw_values = rep(raw_values, length(scene_values))
	}
	if (length(scene_values) == 1 && length(raw_values) > 1) {
		scene_values = rep(scene_values, length(raw_values))
	}
	common_length = min(length(raw_values), length(scene_values))
	raw_values = raw_values[seq_len(common_length)]
	scene_values = scene_values[seq_len(common_length)]
	finite_idx = is.finite(raw_values) & is.finite(scene_values)
	raw_values = raw_values[finite_idx]
	scene_values = scene_values[finite_idx]
	if (!length(raw_values) || !length(scene_values)) {
		return(invisible(NULL))
	}
	zaxis_data = get0(
		"scene_zaxis_data",
		envir = ray_cache_scene_envir,
		inherits = FALSE
	)
	if (is.null(zaxis_data) || !is.list(zaxis_data)) {
		zaxis_data = list()
	}
	zaxis_data[[source]] = list(
		source = source,
		raw_range = range(raw_values),
		scene_range = range(scene_values),
		label = label
	)
	assign("scene_zaxis_data", zaxis_data, envir = ray_cache_scene_envir)
	invisible(NULL)
}

cache_polygon_like_zaxis_data = function(
	source,
	polygon,
	top = NULL,
	bottom = NULL,
	data_column_top = NULL,
	data_column_bottom = NULL,
	scale_data = 1
) {
	label = NULL
	if (!is.null(data_column_top)) {
		raw_values = polygon[[data_column_top]]
		scene_values = suppressWarnings(as.numeric(raw_values)) * scale_data
		label = data_column_top
	} else if (!is.null(data_column_bottom)) {
		raw_values = polygon[[data_column_bottom]]
		scene_values = suppressWarnings(as.numeric(raw_values)) * scale_data
		label = data_column_bottom
	} else {
		raw_values = c(top, bottom)
		scene_values = raw_values
		label = source
	}
	cache_scene_zaxis_data(
		source = source,
		raw_values = raw_values,
		scene_values = scene_values,
		label = label
	)
}

cache_polygon_zaxis_data = function(
	polygon,
	top = NULL,
	bottom = NULL,
	data_column_top = NULL,
	data_column_bottom = NULL,
	scale_data = 1
) {
	cache_polygon_like_zaxis_data(
		source = "polygon",
		polygon = polygon,
		top = top,
		bottom = bottom,
		data_column_top = data_column_top,
		data_column_bottom = data_column_bottom,
		scale_data = scale_data
	)
}

cache_altitude_zaxis_data = function(
	source,
	altitude = NULL,
	scene_altitude = NULL,
	label = NULL
) {
	if (is.null(scene_altitude)) {
		return(invisible(NULL))
	}
	if (is.null(altitude)) {
		altitude = scene_altitude
	}
	cache_scene_zaxis_data(
		source = source,
		raw_values = altitude,
		scene_values = scene_altitude,
		label = label
	)
}

cache_point_zaxis_data = function(altitude = NULL, scene_altitude = NULL) {
	cache_altitude_zaxis_data(
		source = "point",
		altitude = altitude,
		scene_altitude = scene_altitude,
		label = "point"
	)
}

get_scene_zaxis_data = function(source = "auto", default = NULL) {
	source = canonicalize_zaxis_data_source(source)
	if (source == "auto") {
		return(default)
	}
	zaxis_data = get_scene_context_value("scene_zaxis_data", default = NULL)
	if (is.null(zaxis_data) || is.null(zaxis_data[[source]])) {
		return(default)
	}
	zaxis_data[[source]]
}

clear_scene_zaxis_data = function() {
	assign("scene_zaxis_data", NULL, envir = ray_cache_scene_envir)
	invisible(NULL)
}

map_zaxis_data_breaks = function(breaks, zaxis_data) {
	if (is.null(zaxis_data)) {
		return(breaks)
	}
	if (length(unique(zaxis_data$raw_range)) <= 1) {
		return(breaks)
	}
	scales::rescale(
		breaks,
		to = zaxis_data$scene_range,
		from = zaxis_data$raw_range
	)
}
