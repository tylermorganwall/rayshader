#'@title Render Label
#'
#'@description Adds a marker and label to the current 3D plot
#'
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param text The label text.
#'@param y Default `NULL`. Y coordinate for the label in the same coordinate reference system as `extent`.
#'If no `extent` is available and the scene uses a plain matrix heightmap, this defaults to matrix dimensions.
#'@param x Default `NULL`. X coordinate for the label in the same coordinate reference system as `extent`.
#'If no `extent` is available and the scene uses a plain matrix heightmap, this defaults to matrix dimensions.
#'@param z Default `NULL`. Elevation of the label, in units of the elevation matrix (scaled by zscale).
#'@param altitude Default `NULL`. Elevation of the label, in units of the elevation matrix (scaled by zscale). If none is passed, this will default to 10 percent above the maximum altitude in the heightmap.
#'@param extent Either an object representing the spatial extent of the scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use extent metadata cached by [plot_3d()] or [plot_gg()]. If no extent metadata
#' is available for a plain matrix scene, rayshader defaults to `c(xmin = 1, xmax = nrow(heightmap), ymin = 1, ymax = ncol(heightmap))`.
#'@param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#'to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#'for non-ggplot scenes.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'@param relativez Default `TRUE`. Whether `z` should be measured in relation to the underlying elevation at that point in the heightmap, or set absolutely (`FALSE`).
#'@param offset Elevation above the surface (at the label point) to start drawing the line.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing text and lines rendered with [render_label()]. If no
#'other arguments are passed to [render_label()], this will just remove all existing lines.
#'@param textsize Default `1`. A numeric character expansion value.
#'@param dashed Default `FALSE`. If `TRUE`, the label line is dashed.
#'@param dashlength Default `auto`. Length, in units of the elevation matrix (scaled by `zscale`) of the dashes if `dashed = TRUE`.
#'@param linewidth Default `3`. The line width.
#'@param antialias Default `FALSE`. If `TRUE`, the line with be have anti-aliasing applied. NOTE: anti-aliasing can cause some unpredictable behavior with transparent surfaces.
#'@param alpha Default `1`. Transparency of the label line.
#'@param textalpha Default `1`. Transparency of the label text.
#'@param freetype Default `TRUE`. Set to `FALSE` if freetype is not installed (freetype enables anti-aliased fonts). NOTE: There are occasionally transparency issues when positioning Freetype fonts in front and behind a transparent surface.
#'@param adjustvec Default `c(0.5,-0.5)`. The horizontal and vertical offset for the text. If `freetype = FALSE` and on macOS/Linux, this is adjusted to `c(0.33,-0.5)` to keep the type centered.
#'@param family Default `"sans"`. Font family. Choices are `c("serif", "sans", "mono", "symbol")`.
#'@param fonttype Default `"standard"`. The font type. Choices are `c("standard", "bold", "italic", "bolditalic")`. NOTE: These require FreeType fonts, which may not be installed on your system. See the documentation for rgl::text3d() for more information.
#'@param linecolor Default `black`. Color of the line.
#'@param textcolor Default `black`. Color of the text.
#'@param lat Default `NULL`. Alias for `y` for geographic workflows.
#'@param long Default `NULL`. Alias for `x` for geographic workflows.
#'@param location Default `NULL`. Spatial point input used to place the rendered label in the scene. Accepts `sf`, `sfc`, `sfg`, or `sp` POINT or MULTIPOINT geometries. MULTIPOINT inputs are flattened to point placements internally. `render_label()` requires `location` to resolve to exactly one point after flattening. If the input carries a CRS, it will be transformed automatically into the active scene CRS. If it has no CRS, supply `crs`.
#'@param crs Default `NULL`. CRS of the input numeric x/y coordinates, or CRS to assign to CRS-less spatial data before transforming it into the active scene CRS. If spatial data already carries a CRS, that CRS is used automatically.
#'@export
#'@examples
#'if(run_documentation()) {
#'montereybay |>
#'  sphere_shade() |>
#'  plot_3d(montereybay,zscale=50,water=TRUE, watercolor="#233aa1",
#'          zoom=0.9, windowsize = 800)
#'render_snapshot()
#'}
#'
#'santa_cruz = c(36.962957, -122.021033)
#'#We want to add a label to Santa Cruz, so we use x/y coordinates in the same extent as the map.
#'if(run_documentation()) {
#'render_label(montereybay, y = santa_cruz[1], x = santa_cruz[2],
#'             extent = attr(montereybay, "extent"), textsize = 2,
#'             altitude=12000, zscale=50, text = "Santa Cruz")
#'render_snapshot()
#'}
#'
#'monterey = c(36.603053, -121.892933)
#'#We can also change the linetype to dashed by setting `dashed = TRUE` (additional options allow
#'#the user to control the dash length). You can clear the existing lines by setting
#'#`clear_previous = TRUE`.
#'if(run_documentation()) {
#'render_label(montereybay, y = monterey[1], x = monterey[2], altitude = 10000,
#'             extent = attr(montereybay, "extent"), textsize = 2,
#'             zscale = 50, text = "Monterey", textcolor = "white", linecolor="darkred",
#'             dashed = TRUE, clear_previous = TRUE)
#'render_snapshot()
#'}
#'
#'canyon = c(36.621049, -122.333912)
#'#By default, z specifies the altitude above that point on the elevation matrix. We can also specify
#'#an absolute height by setting `relativez=FALSE`.
#'if(run_documentation()) {
#'render_label(montereybay, y = canyon[1], x = canyon[2], altitude = 2000,
#'             extent = attr(montereybay,"extent"), textsize = 2,
#'             zscale=50,text = "Monterey Canyon", relativez=FALSE)
#'render_snapshot()
#'}
#'
#'#We can also render labels in high quality with `render_highquality()`, specifying a custom
#'#line radius. By default, the labels point towards the camera, but you can fix their angle with
#'#argument `text_angle`.
#'if(run_documentation()) {
#'render_camera(theta=35, phi = 35, zoom = 0.80, fov=60)
#'render_label(montereybay, y = monterey[1], x = monterey[2], altitude = 10000,
#'             extent = attr(montereybay, "extent"), textsize = 2,
#'             zscale = 50, text = "Monterey", textcolor = "white", linecolor="darkred",
#'             dashed = TRUE, clear_previous = TRUE)
#'
#'render_label(montereybay, y = canyon[1], x = canyon[2],
#'             altitude = 2000, zscale=50, textsize = 2,
#'             extent = attr(montereybay,"extent"), textcolor = "white", linecolor="white",
#'             text = "Monterey Canyon", relativez=FALSE)
#'
#'render_highquality(samples = 16,text_size = 64, line_radius = 3, text_offset = c(0, 20, 0),
#'                   lightdirection = 180, min_variance = 0)
#'}
#'if(run_documentation()) {
#'#Fixed text angle
#'render_highquality(samples = 16,text_size = 64, line_radius = 3, text_offset = c(0, 20, 0),
#'                   lightdirection = 180, text_angle = 0, min_variance = 0)
#'}
#'#We can remove all existing labels by calling `render_label(clear_previous = TRUE)`
#'if(run_documentation()) {
#'render_label(clear_previous = TRUE)
#'render_snapshot()
#'}
render_label = function(
	heightmap = NULL,
	text,
	y = NULL,
	x = NULL,
	z = NULL,
	altitude = NULL,
	extent = NULL,
	panel = NULL,
	zscale = 1,
	relativez = TRUE,
	offset = 0,
	clear_previous = FALSE,
	textsize = 1,
	dashed = FALSE,
	dashlength = "auto",
	linewidth = 3,
	antialias = FALSE,
	alpha = 1,
	textalpha = 1,
	freetype = TRUE,
	adjustvec = NULL,
	family = "sans",
	fonttype = "standard",
	linecolor = "black",
	textcolor = "black",
	lat = NULL,
	long = NULL,
	location = NULL,
	crs = NULL
) {
	exit_early = FALSE
	if (clear_previous) {
		rgl::pop3d(tag = c("textline", "raytext"))
		if (missing(text)) {
			exit_early = TRUE
		}
	}
	if (!exit_early) {
		zscale = resolve_scene_render_zscale(
			zscale,
			missing(zscale),
			caller = "render_label"
		)
		heightmap = resolve_scene_render_heightmap(
			heightmap,
			caller = "render_label"
		)
		if (is.null(heightmap)) {
			stop(
				"No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
			)
		}
		if (!is.null(altitude)) {
			z = altitude
		}
		if (is.null(z)) {
			z = max(heightmap, na.rm = TRUE) * 1.1
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
			caller = "render_label"
		)
		x = point_input$x
		y = point_input$y
		if (!is.null(point_input$extent)) {
			extent = point_input$extent
		}
		if (is.null(x) || is.null(y)) {
			stop("Must provide `x`/`y` coordinates.", call. = FALSE)
		}
		if (point_input$location_supplied && point_input$geometry_count != 1) {
			stop(
				paste0(
					format_render_caller_prefix("render_label"),
					"`location` must resolve to exactly one point."
				),
				call. = FALSE
			)
		}
		extent = resolve_scene_render_extent(
			extent = extent,
			heightmap = heightmap,
			caller = "render_label",
			panel = panel,
			error_if_missing = FALSE
		)
		if (is.null(extent)) {
			extent = c(
				xmin = 1,
				xmax = nrow(heightmap),
				ymin = 1,
				ymax = ncol(heightmap)
			)
		}
		if (!point_input$location_supplied) {
			scene_xy = auto_transform_scene_xy(
				x = x,
				y = y,
				extent = extent,
				heightmap = heightmap,
				panel = panel,
				crs = crs,
				caller = "render_label"
			)
			x = scene_xy$x
			y = scene_xy$y
			if (!is.null(scene_xy$extent)) {
				extent = scene_xy$extent
			}
		}
		if (rgl::cur3d() == 0) {
			stop("No rgl window currently open.")
		}
		if (.Platform$OS.type == "unix") {
			windows = FALSE
		} else {
			windows = TRUE
		}
		fontlist = list("standard" = 1, "bold" = 2, "italic" = 3, "bolditalic" = 4)
		fonttype = fontlist[[fonttype]]
		e = get_extent(extent)
		nrow_map = nrow(heightmap) - 1
		ncol_map = ncol(heightmap) - 1
		x_index = (x - e["xmin"]) / (e["xmax"] - e["xmin"]) * nrow_map + 1
		y_index = 1 + ncol_map - (y - e["ymin"]) / (e["ymax"] - e["ymin"]) * ncol_map
		x_index_clamped = x_index
		y_index_clamped = y_index
		x_index_clamped[floor(x_index_clamped) >= nrow(heightmap)] = nrow(heightmap)
		y_index_clamped[floor(y_index_clamped) >= ncol(heightmap)] = ncol(heightmap)
		x_index_clamped[floor(x_index_clamped) < 1] = 1
		y_index_clamped[floor(y_index_clamped) < 1] = 1
		in_bounds = TRUE
		if (
			x_index > nrow(heightmap) ||
				x_index < 1 ||
				y_index < 1 ||
				y_index > ncol(heightmap)
		) {
			in_bounds = FALSE
		} else {
			if (!length(find.package("rayimage", quiet = TRUE)) > 0) {
				flipped_mat = flipud(t(heightmap))
				surface_altitude = flipped_mat[
					floor(y_index_clamped),
					floor(x_index_clamped)
				]
			} else {
				surface_altitude = rayimage::interpolate_array(
					t(heightmap),
					x_index_clamped,
					y_index_clamped
				)
			}
			if (is.na(surface_altitude)) {
				in_bounds = FALSE
			}
		}
		startline = 0
		if (!in_bounds) {
			shadow_id = get_ids_with_labels("shadow")$id
			if (length(shadow_id) > 0) {
				shadow_vertices = rgl::rgl.attrib(shadow_id, "vertices")
				startline = min(shadow_vertices[, 2], na.rm = TRUE)
			}
		}

		z = z / zscale
		offset = offset / zscale
		if (in_bounds) {
			startline = surface_altitude / zscale
		}
		if (relativez && in_bounds) {
			z = z + startline
		}
		if (dashlength == "auto") {
			dashlength = (z - startline + offset) / 20
		} else {
			dashlength = as.numeric(dashlength)
		}
		# dashlength = dashlength/zscale
		ignoreex = par3d()$ignoreExtent
		ignoreex = par3d(ignoreExtent = TRUE)
		linelist = list()
		x = x_index - nrow_map / 2 - 1
		y = y_index - ncol_map / 2 - 1
		if (dashed) {
			counter = 1
			while (startline + dashlength < z) {
				linelist[[counter]] = matrix(
					c(x, x, startline + dashlength + offset, startline + offset, y, y),
					2,
					3
				)
				startline = startline + dashlength * 2
				counter = counter + 1
			}
			linelist[[counter]] = matrix(
				c(x, x, z + offset, startline + offset, y, y),
				2,
				3
			)
		} else {
			linelist[[1]] = matrix(
				c(x, x, z + offset, startline + offset, y, y),
				2,
				3
			)
		}
		for (i in 1:length(linelist)) {
			rgl::lines3d(
				linelist[[i]],
				color = linecolor,
				lwd = linewidth,
				lit = FALSE,
				line_antialias = antialias,
				depth_test = "less",
				alpha = alpha,
				tag = "textline"
			)
		}
		if (freetype) {
			seriflist = c(
				"fonts/FreeSerif.ttf",
				"fonts/FreeSerifBold.ttf",
				"fonts/FreeSerifItalic.ttf",
				"fonts/FreeSerifBoldItalic.ttf"
			)
			sanslist = c(
				"fonts/FreeSans.ttf",
				"fonts/FreeSansBold.ttf",
				"fonts/FreeSansOblique.ttf",
				"fonts/FreeSansBoldOblique.ttf"
			)
			monolist = c(
				"fonts/FreeMono.ttf",
				"fonts/FreeMonoBold.ttf",
				"fonts/FreeMonoOblique.ttf",
				"fonts/FreeMonoBoldOblique.ttf"
			)
			symbollist = c(
				"fonts/ESSTIX10.TTF",
				"fonts/ESSTIX12.TTF",
				"fonts/ESSTIX9_.TTF",
				"fonts/ESSTIX11.TTF"
			)
			seriflist2 = unlist(lapply(seriflist, system.file, package = "rgl"))
			sanslist2 = unlist(lapply(sanslist, system.file, package = "rgl"))
			monolist2 = unlist(lapply(monolist, system.file, package = "rgl"))
			symbollist2 = unlist(lapply(symbollist, system.file, package = "rgl"))
			rglFonts(
				serif = seriflist2,
				sans = sanslist2,
				mono = monolist2,
				symbol = symbollist2
			)
			warningstring = " "
			if (family == "serif") {
				if (nchar(seriflist2[[fonttype]]) == 0) {
					family = "bitmap"
					if (fonttype != 1) {
						warningstring = ", setting fonttype to \"standard\", "
					}
					freetype = FALSE
					if (!windows) {
						textsize = 1
						windowsstring = "and setting textsize to 1."
					} else {
						windowsstring = "."
					}
					warning(paste0(
						seriflist[[fonttype]],
						" not found. Turning freetype off",
						warningstring,
						windowsstring
					))
					fonttype = 1
				}
			}
			if (family == "sans") {
				if (nchar(sanslist2[[fonttype]]) == 0) {
					family = "bitmap"
					if (fonttype != 1) {
						warningstring = ", setting fonttype to \"standard\", "
					}
					if (!windows) {
						textsize = 1
						windowsstring = "and setting textsize to 1."
					} else {
						windowsstring = "."
					}
					freetype = FALSE
					warning(paste0(
						sanslist[[fonttype]],
						" not found. Turning freetype off",
						warningstring,
						windowsstring
					))
					fonttype = 1
				}
			}
			if (family == "mono") {
				if (nchar(monolist2[[fonttype]]) == 0) {
					family = "bitmap"
					if (fonttype != 1) {
						warningstring = ", setting fonttype to \"standard\", "
					}
					if (!windows) {
						textsize = 1
						windowsstring = "and setting textsize to 1."
					} else {
						windowsstring = "."
					}
					freetype = FALSE
					warning(paste0(
						monolist[[fonttype]],
						" not found. Turning freetype off",
						warningstring,
						windowsstring
					))
					fonttype = 1
				}
			}
			if (family == "symbol") {
				if (nchar(symbollist2[[fonttype]]) == 0) {
					family = "bitmap"
					if (fonttype != 1) {
						warningstring = ", setting fonttype to \"standard\", "
					}
					if (!windows) {
						textsize = 1
						windowsstring = "and setting textsize to 1."
					} else {
						windowsstring = "."
					}
					freetype = FALSE
					warning(paste0(
						symbollist[[fonttype]],
						" not found. Turning freetype off",
						warningstring,
						windowsstring
					))
					fonttype = 1
				}
			}
		} else {
			warningstring = ""
			family = "bitmap"
			if (fonttype != 1) {
				warningstring = " and fonttype to \"standard\""
				fonttype = 1
			}
			freetype = FALSE
			if (textsize != 1 && !windows) {
				warning(
					"Bitmap fonts do not support variable text sizes--setting textsize back to 1",
					warningstring,
					"."
				)
				textsize = 1
			}
		}
		if (is.null(adjustvec)) {
			if (freetype || windows) {
				adjustvec = c(0.5, -0.5)
			} else {
				adjustvec = c(0.33, -0.5)
			}
		}
		text3d(
			x,
			z + offset,
			y,
			text,
			color = textcolor,
			adj = adjustvec,
			useFreeType = freetype,
			alpha = textalpha,
			family = family,
			font = fonttype,
			cex = textsize,
			depth_test = "less",
			tag = "raytext",
			lit = FALSE
		)
		par3d(ignoreExtent = ignoreex)
	}
}

resolve_render_label_text_angle = function(text_angle = NULL, default_angle) {
	if (is.null(text_angle)) {
		return(default_angle)
	}
	if (length(text_angle) == 1) {
		return(c(0, text_angle, 0))
	}
	text_angle
}

resolve_render_label_text_angle_rayrender = function(text_angle = NULL, phi, theta) {
	resolve_render_label_text_angle(
		text_angle = text_angle,
		default_angle = c(-phi, theta + 180, 0)
	)
}

resolve_render_label_text_angle_rayvertex = function(text_angle = NULL, theta, rotmat) {
	resolve_render_label_text_angle(
		text_angle = text_angle,
		default_angle = c(rotmat[1], -theta, 0)
	)
}
