#'@title Render MULTIPOLYGON Z Geometry
#'
#'@description Adds MULTIPOLYGONZ will be plotted in the coordinate system set by the user-specified
#'`extent` argument as-is.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'You can also use [save_multipolygonz_to_obj()] manually to convert sf objects
#'
#'@param sfobj An sf object with MULTIPOLYGON Z geometry.
#'@param extent Either an object representing the spatial extent of the scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use extent metadata cached by [plot_3d()] or [plot_gg()].
#'@param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#'to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#'for non-ggplot scenes.
#'@param crs Default `NULL`. CRS of the input numeric x/y coordinates, or CRS to assign to CRS-less spatial data before transforming it into the active scene CRS. If spatial data already carries a CRS, that CRS is used automatically.
#'@param obj_zscale Default `TRUE`. Whether to scale the size of the OBJ by zscale to have it match
#'the size of the map. If zscale is very big, this will make the model very small.
#'@param swap_yz Default `TRUE`. Whether to swap and Y and Z axes. (Y axis is vertical in
#'rayshader coordinates, but data is often provided with Z being vertical).
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param color Default `black`. Color of the 3D model, if `load_material = FALSE`.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing points.
#'@param rgl_tag Default `""`. Tag to add to the rgl scene id, will be prefixed by `"obj"`
#'@param baseshape Default `rectangle`. Shape of the base. Options are `c("rectangle","circle","hex")`.
#'@param ... Additional arguments to pass to `rgl::triangles3d()`.
#'@export
#'@examplesIf length(find.package("sf", quiet = TRUE)) && length(find.package("elevatr", quiet = TRUE)) && length(find.package("raster", quiet = TRUE)) && (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#'library(sf)
#'#Set location of washington monument
#'washington_monument_location =  st_point(c(-77.035249, 38.889462))
#'wm_point = washington_monument_location |>
#'  st_point() |>
#'  st_sfc(crs = 4326) |>
#'  st_transform(st_crs(washington_monument_multipolygonz))
#'
#'elevation_data = elevatr::get_elev_raster(locations = wm_point, z = 14)
#'
#'scene_bbox = st_bbox(st_buffer(wm_point,300))
#'cropped_data = raster::crop(elevation_data, scene_bbox)
#'
#'#Use rayshader to convert that raster data to a matrix
#'dc_elevation_matrix = raster_to_matrix(cropped_data)
#'
#'#Remove negative elevation data
#'dc_elevation_matrix[dc_elevation_matrix < 0] = 0
#'
#'#Plot a 3D map of the national mall
#'dc_elevation_matrix |>
#'  height_shade() |>
#'  add_shadow(lamb_shade(dc_elevation_matrix), 0) |>
#'  plot_3d(dc_elevation_matrix, zscale=3.7, water = TRUE, waterdepth = 1,
#'          soliddepth=-50, windowsize = 800)
#'render_snapshot()
#'#Zoom in on the monument
#'render_camera(theta=150,  phi=35, zoom= 0.55, fov=70)
#'#Render the national monument
#'rgl::par3d(ignoreExtent = TRUE)
#'render_multipolygonz(washington_monument_multipolygonz,
#'                     extent = raster::extent(cropped_data),
#'                     zscale = 4, color = "grey80",
#'                     heightmap = dc_elevation_matrix)
#'render_snapshot()
#'#This works with `render_highquality()`
#'render_highquality(min_variance = 0, samples = 16)
render_multipolygonz = function(
	sfobj,
	extent = NULL,
	panel = NULL,
	zscale = 1,
	vertical_exaggeration = 1,
	heightmap = NULL,
	color = "grey50",
	offset = 0,
	obj_zscale = TRUE,
	swap_yz = TRUE,
	clear_previous = FALSE,
	baseshape = "rectangle",
	rgl_tag = "_multipolygon",
	crs = NULL,
	...
) {
	dot_split = split_zaxis_dots(list(...))
	zscale = resolve_scene_render_effective_zscale(
		zscale = zscale,
		zscale_missing = missing(zscale),
		vertical_exaggeration = vertical_exaggeration,
		vertical_exaggeration_missing = missing(vertical_exaggeration),
		caller = "render_multipolygonz"
	)
	heightmap = resolve_scene_render_heightmap(
		heightmap,
		caller = "render_multipolygonz"
	)
	if (clear_previous) {
		rgl::pop3d(tag = sprintf("obj%s", rgl_tag))
		if (missing(sfobj)) {
			render_zaxis_from_dots(
				zaxis_args = dot_split$zaxis_args,
				extent = extent,
				panel = panel,
				zscale = zscale,
				heightmap = heightmap,
				caller = "render_multipolygonz"
			)
			return(invisible())
		}
	}
	if (inherits(sfobj, "Spatial")) {
		sfobj = sf::st_as_sf(sfobj)
	}
	if (inherits(sfobj, "sfc")) {
		sfobj = sf::st_sf(geometry = sfobj)
	}
	if (inherits(sfobj, "sfg")) {
		sfobj = sf::st_sf(geometry = sf::st_sfc(sfobj))
	}
	scene_sfobj = auto_transform_scene_sf(
		sf_object = sfobj,
		extent = extent,
		heightmap = heightmap,
		panel = panel,
		crs = crs,
		caller = "render_multipolygonz"
	)
	sfobj = scene_sfobj$object
	if (!is.null(scene_sfobj$extent)) {
		extent = scene_sfobj$extent
	}
	multipolygon_mesh = multipolygonz_to_raymesh(sfobj)
	render_raymesh(
		raymesh = multipolygon_mesh,
		extent = extent,
		panel = panel,
		obj_zscale = obj_zscale,
		clear_previous = FALSE,
		zscale = zscale,
		vertical_exaggeration = 1,
		color = color,
		offset = offset,
		swap_yz = swap_yz,
		heightmap = heightmap,
		baseshape = baseshape,
		rgl_tag = rgl_tag,
		crs = crs,
		rgl_tag_prefix = "obj",
		swap_yz_transform = "rotate",
		...
	)
}
