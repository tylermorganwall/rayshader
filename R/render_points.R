#'@title Render Points
#'
#'@description Adds 3D datapoints to the current scene, using x/y coordinates in the reference
#'system defined by the extent object. If no altitude is provided, the points will be elevated a constant offset
#'above the heightmap. If the points goes off the edge, the nearest height on the heightmap will be used (unless that
#'value is NA, in which the point will be removed).
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param x Vector of x coordinates (or other coordinate in the same coordinate reference system as extent).
#'@param y Vector of y coordinates (or other coordinate in the same coordinate reference system as extent).
#'@param lat Default `NULL`. Alias for `y` for geographic workflows.
#'@param long Default `NULL`. Alias for `x` for geographic workflows.
#'@param location Default `NULL`. Spatial point input used to place the rendered point(s) in the scene. Accepts `sf`, `sfc`, `sfg`, or `sp` POINT or MULTIPOINT geometries. MULTIPOINT inputs are flattened to point placements internally, and vectorized arguments such as `size`, `color`, and `altitude` are applied against that flattened point count. If the input carries a CRS, it will be transformed automatically into the active scene CRS. If it has no CRS, supply `crs`.
#'@param altitude Default `NULL`. Elevation of each point, in units of the elevation matrix (scaled by zscale). If a single value,
#'all data will be rendered at that altitude.
#'@param extent Either an object representing the spatial extent of the 3D scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use extent metadata cached by [plot_3d()] or [plot_gg()].
#'@param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#'to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#'for non-ggplot scenes.
#'@param crs Default `NULL`. CRS of the input numeric x/y coordinates, or CRS to assign to CRS-less spatial data before transforming it into the active scene CRS. If spatial data already carries a CRS, that CRS is used automatically.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param size Default `3`. The point size. This can be a vector (the same length as `x` and `y`) specifying
#'a size for each point.
#'@param color Default `black`. Color of the point. This can also be a vector specifying the color of each point.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing points.
#'@param filter_to_extent Default `TRUE`. If `TRUE`, points outside the scene extent are omitted. For scenes created with [plot_gg()], filtering uses the ggplot panel extent rather than the full rendered 3D ggplot extent.
#'@param ... Optional z-axis arguments passed to [render_zaxis()], such as
#'`zaxis = TRUE`, `zaxis_location`, `zaxis_breaks`, and `zaxis_labels`.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Starting at Moss Landing in Monterey Bay, we are going to simulate a flight of a bird going
#'#out to sea and diving for food.
#'
#'#First, create simulated lat/long data
#'set.seed(2009)
#'moss_landing_coord = c(36.806807, -121.793332)
#'x_vel_out = -0.001 + rnorm(1000)[1:300]/1000
#'y_vel_out = rnorm(1000)[1:300]/200
#'z_out = c(seq(0,2000,length.out = 180), seq(2000,0,length.out=10),
#'          seq(0,2000,length.out = 100), seq(2000,0,length.out=10))
#'
#'bird_track_lat = list()
#'bird_track_long = list()
#'bird_track_lat[[1]] = moss_landing_coord[1]
#'bird_track_long[[1]] = moss_landing_coord[2]
#'for(i in 2:300) {
#' bird_track_lat[[i]] = bird_track_lat[[i-1]] + y_vel_out[i]
#' bird_track_long[[i]] = bird_track_long[[i-1]] + x_vel_out[i]
#'}
#'
#'
#'#Render the 3D map
#'montereybay |>
#'  sphere_shade() |>
#'  plot_3d(vertical_exaggeration = 4,water=TRUE,
#'          shadowcolor="#40310a", background = "tan",
#'          theta=210,  phi=22, zoom=0.20, fov=55)
#'
#'#Pass in the latitude/longitude coordinates and altitudes of the track.
#'render_points(lat = unlist(bird_track_lat), long = unlist(bird_track_long),
#'              altitude = z_out, color="white")
#'render_snapshot()
#'#We'll set the altitude to zero to give the tracks a "shadow" over the water.
#'render_points(lat = unlist(bird_track_lat), long = unlist(bird_track_long),
#'              offset = 0, color="black")
#'render_camera(theta=30,phi=35,zoom=0.45,fov=70)
#'render_snapshot()
#'#Remove the points:
#'render_points(clear_previous=TRUE)
#'
#'# Finally, we can also plot just GPS coordinates offset from the surface by leaving altitude `NULL`
#'# Here we plot a circle of values surrounding Moss Landing.
#'
#'t = seq(0,2*pi,length.out=100)
#'circle_coords_lat = moss_landing_coord[1] + 0.3 * sin(t)
#'circle_coords_long = moss_landing_coord[2] + 0.3 * cos(t)
#'render_points(lat = unlist(circle_coords_lat), long = unlist(circle_coords_long),
#'            color="red", offset=100, size=5)
#'render_camera(theta = 160, phi=33, zoom=0.4, fov=55)
#'render_snapshot()
#'#And all of these work with `render_highquality()`
#'render_highquality(point_radius = 1, min_variance = 0, samples = 16)
#'
#'#We can also change the material of the objects by setting the `point_material` and
#'#`point_material_args` arguments in `render_highquality()`
#'render_highquality(point_radius = 1, min_variance = 0, samples = 16,
#'                   point_material = rayrender::glossy,
#'                   point_material_args = list(gloss = 0.5, reflectance = 0.2))
render_points = function(
	y = NULL,
	x = NULL,
	altitude = NULL,
	extent = NULL,
	panel = NULL,
	zscale = 1,
	vertical_exaggeration = 1,
	heightmap = NULL,
	size = 0.5,
	color = "black",
	offset = 5,
	clear_previous = FALSE,
	lat = NULL,
	long = NULL,
	location = NULL,
	crs = NULL,
	filter_to_extent = TRUE,
	...
) {
	validate_filter_to_extent(filter_to_extent, caller = "render_points")
	zaxis_split = split_zaxis_dots(list(...))
	zscale = resolve_scene_render_effective_zscale(
		zscale = zscale,
		zscale_missing = missing(zscale),
		vertical_exaggeration = vertical_exaggeration,
		vertical_exaggeration_missing = missing(vertical_exaggeration),
		caller = "render_points"
	)
	heightmap = resolve_scene_render_heightmap(
		heightmap,
		caller = "render_points"
	)
	extent = resolve_scene_render_extent(
		extent = extent,
		heightmap = heightmap,
		caller = "render_points",
		panel = panel,
		error_if_missing = FALSE
	)
	if (
		is.null(extent) &&
			!is.null(heightmap) &&
			is.null(get_cached_plot_gg_transform_info(
				heightmap = heightmap,
				default = NULL
			))
	) {
		extent = c(
			xmin = 1,
			xmax = nrow(heightmap),
			ymin = 1,
			ymax = ncol(heightmap)
		)
	}
	point_input = resolve_render_location_input(
		location = location,
		x = x,
		y = y,
		long = long,
		lat = lat,
		missing_x = missing(x),
		missing_y = missing(y),
		missing_long = missing(long),
		missing_lat = missing(lat),
		extent = extent,
		heightmap = heightmap,
		panel = panel,
		crs = crs,
		caller = "render_points"
	)
	x = point_input$x
	y = point_input$y
	input_crs = if (is.null(crs)) point_input$source_crs else crs
	if (!is.null(point_input$extent)) {
		extent = point_input$extent
	}
	zaxis_args = normalize_scene_zaxis_args(
		zaxis_args = zaxis_split$zaxis_args,
		altitude = altitude,
		extent = extent,
		heightmap = heightmap
	)
	if (rgl::cur3d() == 0) {
		stop("No rgl window currently open.")
	}
	if (clear_previous) {
		rgl::pop3d(tag = "points3d")
		if (is.null(y) || is.null(x)) {
			render_zaxis_from_dots(
				zaxis_args = zaxis_args,
				extent = extent,
				panel = panel,
				zscale = zscale,
				heightmap = heightmap,
				caller = "render_points"
			)
			return(invisible())
		}
	}
	if (!point_input$location_supplied && !is.null(x) && !is.null(y)) {
		scene_xy = auto_transform_scene_xy(
			x = x,
			y = y,
			extent = extent,
			heightmap = heightmap,
			panel = panel,
			crs = input_crs,
			caller = "render_points"
		)
		x = scene_xy$x
		y = scene_xy$y
		if (!is.null(scene_xy$extent)) {
			extent = scene_xy$extent
		}
	}
	n_points_before_filter = length(x)
	filtered_points = filter_scene_xy_to_extent(
		x = x,
		y = y,
		extent = extent,
		heightmap = heightmap,
		panel = panel,
		filter_to_extent = filter_to_extent,
		caller = "render_points"
	)
	x = filtered_points$x
	y = filtered_points$y
	if (length(filtered_points$keep) == n_points_before_filter) {
		altitude = subset_render_arg(altitude, filtered_points$keep, n_points_before_filter)
		size = subset_render_arg(size, filtered_points$keep, n_points_before_filter)
		color = subset_render_color_arg(color, filtered_points$keep, n_points_before_filter)
	}
	if (!length(x) || !length(y)) {
		render_zaxis_from_dots(
			zaxis_args = zaxis_args,
			extent = extent,
			panel = panel,
			zscale = zscale,
			heightmap = heightmap,
			caller = "render_points"
		)
		return(invisible(NULL))
	}
	xyz = transform_into_heightmap_coords(
		extent,
		heightmap,
		y,
		x,
		altitude,
		offset,
		zscale,
		crs = input_crs,
		panel = panel,
		transform_scene = FALSE,
		caller = "render_points"
	)

	if (length(unique(size)) > 1) {
		stopifnot(length(size) == nrow(xyz))
		color_length = length(color)
		for (i in seq_len(nrow(xyz))) {
			rgl::points3d(
				xyz[i, 1],
				xyz[i, 2],
				xyz[i, 3],
				color = color[((i - 1) %% color_length) + 1],
				tag = "points3d",
				size = size[i]
			)
		}
	} else {
		rgl::points3d(
			xyz[, 1],
			xyz[, 2],
			xyz[, 3],
			color = color,
			tag = "points3d",
			size = size
		)
	}
	render_zaxis_from_dots(
		zaxis_args = zaxis_args,
		extent = extent,
		panel = panel,
		zscale = zscale,
		heightmap = heightmap,
		caller = "render_points"
	)
	invisible(NULL)
}
