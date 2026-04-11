#'@title Render Z-Axis
#'
#'@description Add a standalone z-axis to the active 3D scene.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param extent Default `NULL`. Either an object representing the spatial extent of the scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use cached extent metadata from [plot_gg()] or from [plot_3d()]
#'(either from an explicitly passed `extent` argument, or the built-in `montereybay` scene metadata).
#'@param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#'to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#'for non-ggplot scenes.
#'@param zscale Default `1`. The ratio between x/y spacing and z units.
#'If left at `1` with `zaxis_breaks = NULL` on non-ggplot terrain scenes, rayshader
#'will attempt to use the cached `plot_3d()` zscale to generate more meaningful defaults.
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param zaxis_location Default `"auto"`. Axis location. Options:
#'`"auto"`, `"panel"`, `"panelbottomleft"`, `"panelbottomright"`,
#'`"paneltopleft"`, `"paneltopright"`, `"bottomleft"`, `"bottomright"`,
#'`"topleft"`, `"topright"`.
#'@param zaxis_breaks Default `NULL`. Numeric breaks (in altitude units). If `NULL`,
#'breaks are generated from the full scene height range (minimum to maximum elevation).
#'@param zaxis_labels Default `NULL`. Labels for `zaxis_breaks`.
#'@param zaxis_color Default `"black"`. Axis/tick/label color.
#'@param zaxis_linewidth Default `2`. Axis line width.
#'@param zaxis_text_offset Default `0`. Label offset multiplier from the axis,
#'applied in the outward corner direction (diagonal for corner placements).
#'@param zaxis_corner_offset Default `NULL`. Corner offset as a proportion of the
#'center-to-corner planar distance. If `NULL`, this defaults to `0` for ggplot scenes
#'and `0.08` for non-ggplot scenes. `0` places the axis exactly at the corner.
#'@param zaxis_tick_size Default `NULL`. Tick marker size. If `NULL`, auto-sized from line width.
#'@export
render_zaxis = function(
	extent = NULL,
	panel = NULL,
	zscale = 1,
	heightmap = NULL,
	zaxis_location = "auto",
	zaxis_breaks = NULL,
	zaxis_labels = NULL,
	zaxis_color = "black",
	zaxis_linewidth = 2,
	zaxis_text_offset = 0,
	zaxis_corner_offset = NULL,
	zaxis_tick_size = NULL
) {
	if (rgl::cur3d() == 0) {
		stop("No rgl window currently open.")
	}
	zscale = resolve_scene_render_zscale(
		zscale,
		missing(zscale),
		caller = "render_zaxis"
	)
	extent_was_missing = missing(extent)
	if (!extent_was_missing && !is.null(extent)) {
		cache_scene_extent(
			extent,
			label = format_scene_cache_label(deparse(substitute(extent)))
		)
	}
	heightmap = resolve_scene_render_heightmap(
		heightmap,
		caller = "render_zaxis"
	)
	extent = resolve_scene_render_extent(
		extent = extent,
		heightmap = heightmap,
		caller = "render_zaxis",
		panel = panel
	)
	render_zaxis_internal(
		zaxis = TRUE,
		extent = extent,
		zscale = zscale,
		heightmap = heightmap,
		zaxis_location = zaxis_location,
		zaxis_breaks = zaxis_breaks,
		zaxis_labels = zaxis_labels,
		zaxis_color = zaxis_color,
		zaxis_linewidth = zaxis_linewidth,
		zaxis_text_offset = zaxis_text_offset,
		zaxis_corner_offset = zaxis_corner_offset,
		zaxis_tick_size = zaxis_tick_size
	)
}

zaxis_dot_names = function() {
	c(
		"zaxis",
		"zaxis_location",
		"zaxis_breaks",
		"zaxis_labels",
		"zaxis_color",
		"zaxis_linewidth",
		"zaxis_text_offset",
		"zaxis_corner_offset",
		"zaxis_tick_size"
	)
}

split_zaxis_dots = function(dots) {
	if (is.null(dots)) {
		dots = list()
	}
	dot_names = names(dots)
	is_zaxis = rep(FALSE, length(dots))
	if (length(dots) > 0 && !is.null(dot_names)) {
		is_zaxis = nzchar(dot_names) & dot_names %in% zaxis_dot_names()
	}
	list(
		zaxis_args = dots[is_zaxis],
		other_args = dots[!is_zaxis]
	)
}

render_zaxis_from_dots = function(
	zaxis_args = list(),
	extent = NULL,
	panel = NULL,
	zscale = 1,
	heightmap = NULL,
	caller = NULL
) {
	if (length(zaxis_args) == 0) {
		return(invisible(NULL))
	}
	zscale = resolve_scene_render_zscale(
		zscale,
		zscale_missing = isTRUE(all.equal(as.numeric(zscale)[1], 1))
	)
	heightmap = resolve_scene_render_heightmap(heightmap)
	extent = resolve_scene_render_extent(
		extent = extent,
		heightmap = heightmap,
		panel = panel,
		caller = caller
	)
	if (is.null(zaxis_args$zaxis)) {
		zaxis_args$zaxis = TRUE
	}
	do.call(
		render_zaxis_internal,
		c(
			zaxis_args,
			list(
				extent = extent,
				zscale = zscale,
				heightmap = heightmap
			)
		)
	)
}
