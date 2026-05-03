#'@title Render Contours
#'
#'@description Adds 3D contours to the current scene, using the heightmap of the 3D surface.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param levels Default `NA`. Automatically generated with 10 levels. This argument specifies the exact height levels of each contour.
#'@param nlevels Default `NA`. Controls the auto-generation of levels. If levels is length-2,
#'this will automatically generate `nlevels` breaks between `levels[1]` and `levels[2]`.
#'@param linewidth Default `3`. The line width.
#'@param antialias Default `FALSE`. If `TRUE`, the line with be have anti-aliasing applied. NOTE: anti-aliasing can cause some unpredictable behavior with transparent surfaces.
#'@param color Default `black`. Color of the line.
#'@param palette Default `NULL`. Overrides `color`. Either a function that returns a color palette
#'of `n` colors, or a character vector with colors that specifies each color manually.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing paths.
#'@param ... Optional z-axis arguments passed to [render_zaxis()], such as
#'`zaxis = TRUE`, `zaxis_location`, `zaxis_breaks`, and `zaxis_labels`.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Add contours to the montereybay dataset
#'montereybay |>
#'  height_shade() |>
#'  add_shadow(ray_shade(vertical_exaggeration = 4),0.3) |>
#'  plot_3d(theta = -45, vertical_exaggeration = 4, zoom=0.9, windowsize=800)
#'render_contours(offset = 100)
#'render_snapshot()
#'
#'#Specify the number of levels
#'render_contours(offset = 100, nlevels = 30,
#'                clear_previous = TRUE)
#'render_snapshot()
#'
#'#Manually specify the breaks with levels
#'render_contours(linewidth = 2,  offset = 100,
#'                levels = seq(-2000, 0, 100), clear_previous = TRUE)
#'render_snapshot()
#'
#'#Use a color palette for the contours
#'volcano |>
#'  constant_shade() |>
#'  plot_3d(zscale = 2, solid = FALSE, zoom = 0.8)
#'palette = grDevices::colorRampPalette(c("red", "purple", "pink"))
#'render_contours(offset = 1, palette = palette, nlevels = 20)
#'render_snapshot()
#'
#'#Render using `render_highquality()` for a neon light effect
#'render_highquality(light = FALSE, samples = 16,
#'                   line_radius = 0.1,
#'                   path_material = rayrender::light, ground_size = 0,
#'                   path_material_args = list(importance_sample = FALSE,
#'                                             color = "purple", intensity = 2))
render_contours = function(
	heightmap = NULL,
	zscale = 1,
	vertical_exaggeration = 1,
	levels = NA,
	nlevels = NA,
	linewidth = 1,
	color = "black",
	palette = NULL,
	antialias = FALSE,
	offset = 0,
	clear_previous = FALSE,
	...
) {
	zaxis_split = split_zaxis_dots(list(...))
	zscale = resolve_scene_render_effective_zscale(
		zscale = zscale,
		zscale_missing = missing(zscale),
		vertical_exaggeration = vertical_exaggeration,
		vertical_exaggeration_missing = missing(vertical_exaggeration),
		caller = "render_contours"
	)
	if (clear_previous) {
		rgl::pop3d(tag = "contour3d")
		if (missing(heightmap) && is.null(get_scene_heightmap(default = NULL))) {
			return(invisible())
		}
	}
	heightmap = resolve_scene_render_heightmap(
		heightmap,
		caller = "render_contours"
	)
	if (is.null(heightmap)) {
		stop(
			"No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
		)
	}
	if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
		stop("`sf` package required for generate_contour_overlay()")
	}
	if (!(length(find.package("isoband", quiet = TRUE)) > 0)) {
		stop("`isoband` package required for generate_contour_overlay()")
	}
	if (is.na(levels[1])) {
		if (is.na(nlevels[1])) {
			nlevels = 10
		}
		rangelevels = range(heightmap, na.rm = TRUE)
		levels = seq(rangelevels[1], rangelevels[2], length.out = nlevels + 2)
	} else if (length(levels) == 2 && !is.na(nlevels)) {
		rangelevels = range(levels, na.rm = TRUE)
		levels = seq(rangelevels[1], rangelevels[2], length.out = nlevels + 2)
	}
	levels = sort(unique(levels))
	extent_heightmap = c(1, nrow(heightmap), 1, ncol(heightmap))
	if (
		all(levels > max(heightmap, na.rm = TRUE)) ||
			all(levels < min(heightmap, na.rm = TRUE))
	) {
		warning(sprintf(
			"Contour levels [range %f-%f] do not fall between min [%f] and max [%f] of DEM: no contours drawn.",
			min(levels),
			max(levels),
			min(heightmap, na.rm = TRUE),
			max(heightmap, na.rm = TRUE)
		))
		return(invisible())
	}
	levels = levels[levels > min(heightmap, na.rm = TRUE)]
	levels = levels[levels < max(heightmap, na.rm = TRUE)]
	heightmap2 = flipud(t(heightmap))
	isolineval = isoband::isolines(
		x = seq_len(ncol(heightmap2)),
		y = seq_len(nrow(heightmap2)),
		z = heightmap2,
		levels = levels
	)
	contour_heights = as.numeric(names(isolineval))
	if (!is.null(palette)) {
		if (is.function(palette)) {
			color = palette(length(isolineval))
		} else {
			if (length(palette) == length(isolineval) && is.character(palette)) {
				color = palette
			}
		}
		for (i in seq_len(length(isolineval))) {
			contour_height = contour_heights[i] + offset
			do.call(
				render_path,
				c(
					list(
						lat = isolineval[[i]]$y,
						long = isolineval[[i]]$x,
						groups = isolineval[[i]]$id,
						altitude = contour_height,
						heightmap = heightmap,
						extent = extent_heightmap,
						tag = "contour3d",
						zscale = zscale,
						vertical_exaggeration = 1,
						linewidth = linewidth,
						offset = offset,
						antialias = antialias,
						color = color[i]
					),
					zaxis_split$other_args
				)
			)
		}
	} else {
		prev_id_max = 0
		isoline_list = vector("list", length = length(isolineval))
		for (i in seq_len(length(isolineval))) {
			isolineval[[i]]$id = isolineval[[i]]$id + prev_id_max
			isolineval[[i]]$altitude = contour_heights[i] + offset
			prev_id_max = max(isolineval[[i]]$id)
			isoline_list[[i]] = data.frame(isolineval[[i]])
		}
		isolines_combined = do.call("rbind", isoline_list)
		do.call(
			render_path,
			c(
				list(
					lat = isolines_combined$y,
					long = isolines_combined$x,
					groups = isolines_combined$id,
					altitude = isolines_combined$altitude,
					extent = extent_heightmap,
					tag = "contour3d",
					heightmap = heightmap,
					zscale = zscale,
					vertical_exaggeration = 1,
					linewidth = linewidth,
					offset = offset,
					antialias = antialias,
					color = color
				),
				zaxis_split$other_args
			)
		)
	}
	render_zaxis_from_dots(
		zaxis_args = zaxis_split$zaxis_args,
		extent = extent_heightmap,
		zscale = zscale,
		heightmap = heightmap
	)
}
