#'@title Render Water Layer
#'
#'@description Adds water layer to the scene, removing the previous water layer if desired.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param waterdepth Default `0`.
#'@param watercolor Default `lightblue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param visual_exaggeration Default `1`. One-off multiplier applied to the effective visual relief for this call. Values greater than `1` increase apparent relief and values between `0` and `1` flatten it.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency.
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param remove_water Default `TRUE`. If `TRUE`, will remove existing water layer and replace it with new layer.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'montereybay |>
#'  sphere_shade() |>
#'  plot_3d(montereybay,zscale=50)
#'render_snapshot()
#'
#'#We want to add a layer of water after the initial render.
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'render_water(montereybay,zscale=50)
#'render_snapshot()
#'
#'#Call it again to change the water depth
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'render_water(montereybay,zscale=50,waterdepth=-1000)
#'render_snapshot()
#'
#'#Add waterlines
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'render_camera(theta=-45)
#'render_water(montereybay,zscale=50,waterlinecolor="white")
#'render_snapshot()
render_water = function(
	heightmap = NULL,
	waterdepth = 0,
	watercolor = "lightblue",
	zscale = 1,
	visual_exaggeration = 1,
	wateralpha = 0.5,
	waterlinecolor = NULL,
	waterlinealpha = 1,
	linewidth = 2,
	remove_water = TRUE
) {
	zscale = resolve_scene_render_zscale(
		zscale,
		missing(zscale),
		caller = "render_water"
	)
	zscale = apply_visual_exaggeration(
		zscale = zscale,
		visual_exaggeration = visual_exaggeration,
		caller = "render_water"
	)
	heightmap = resolve_scene_render_heightmap(
		heightmap,
		caller = "render_water"
	)
	if (is.null(heightmap)) {
		stop(
			"No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
		)
	}
	if (rgl::cur3d() == 0) {
		stop("No rgl window currently open.")
	}
	if (remove_water) {
		rgl::pop3d(tag = c("waterlines", "water"))
	}
	make_water(
		heightmap / zscale,
		waterheight = waterdepth / zscale,
		wateralpha = wateralpha,
		watercolor = watercolor
	)
	if (!is.null(waterlinecolor)) {
		if (all(!is.na(heightmap))) {
			make_lines(
				fliplr(heightmap),
				basedepth = waterdepth / zscale,
				linecolor = waterlinecolor,
				zscale = zscale,
				linewidth = linewidth,
				alpha = waterlinealpha,
				solid = FALSE
			)
		}
		make_waterlines(
			heightmap,
			waterdepth = waterdepth / zscale,
			linecolor = waterlinecolor,
			zscale = zscale,
			alpha = waterlinealpha,
			linewidth = linewidth
		)
	}
}
