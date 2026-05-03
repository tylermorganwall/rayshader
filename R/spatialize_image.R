#' Spatialize a Rayshader Image
#'
#' `spatialize_image()` converts a rayshader image into a georeferenced
#' `terra::SpatRaster`. If they are omitted, rayshader reuses
#' cached extent and CRS metadata from the active scene, following the same
#' cache-resolution rules used by the `render_*()` helpers. Cached metadata can
#' come from [plot_3d()] or [plot_gg()], and `panel` can be used to select a
#' specific facet panel for faceted ggplot scenes.  Explicit `extent` and `crs`
#' arguments always take precedence.
#'
#' If no explicit extent is supplied and no cached extent metadata is available,
#' `spatialize_image()` errors. CRS is resolved in this order: explicit `crs`,
#' CRS carried by the explicit `extent` object, cached scene CRS, then empty CRS
#' if none is available.
#'
#' Cache fallback messages are disabled by default. Set
#' `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata
#' is reused.
#'
#' @param image A matrix, RGB/RGBA array, or any input accepted by
#' `rayimage::ray_read_image()`.
#' @param extent Default `NULL`. Spatial extent metadata for the output raster.
#' Explicit `extent` overrides cached metadata. Accepts the same extent-like
#' inputs supported by [get_extent()], including numeric
#' `c(xmin, xmax, ymin, ymax)` vectors, `terra::SpatExtent`,
#' `terra::SpatRaster`, `raster::Extent`, `Raster*`, `sf`/`sfc`, and `sp`
#' objects. If omitted, rayshader reuses cached scene extent metadata.
#' @param crs Default `NULL`. CRS for the output raster. Explicit `crs`
#' overrides everything else. If omitted, rayshader first tries to inherit CRS
#' from the explicit `extent` object, then from cached scene metadata.
#' @param panel Default `NULL`. Facet panel identifier for scenes created with
#' [plot_gg()]. Required to disambiguate faceted ggplot scenes when panel-
#' specific cached metadata is needed. Ignored for non-faceted/non-ggplot
#' scenes.
#' @param include_alpha Default `FALSE`. If `FALSE` and `image` has four
#' channels, drop alpha. If `TRUE`, include alpha as a fourth raster layer.
#' @param toRGB Default `TRUE`. If `TRUE`, convert color channels from linear
#' rayshader values to sRGB with [rayimage::render_gamma_linear()], then scale
#' to the `0-255` range and clamp. Alpha is not gamma-adjusted, but is scaled
#' and clamped to match.
#' @param flip_vertical Default `FALSE`. If `TRUE`, reverse the image row order
#' before rasterization.
#' @param flip_horizontal Default `FALSE`. If `TRUE`, reverse the image column
#' order before rasterization.
#' @param layer_names Default `NULL`. Optional output layer names. Defaults are
#' `"value"` for a single layer, `c("red", "green", "blue")` for RGB output,
#' and `c("red", "green", "blue", "alpha")` for RGBA output.
#' @param include_height Default `FALSE`. If `TRUE`, append a final layer with
#' raw height values from `heightmap` or cached rayshader scene metadata. When
#' the output image resolution differs from the source heightmap, heights are
#' bilinearly resampled onto the output raster grid.
#' @param heightmap Default `NULL`. Optional source elevation matrix or spatial
#' raster used when `include_height = TRUE`. Explicit `heightmap` overrides
#' cached heightmap metadata. If omitted, rayshader reuses cached hillshade or
#' scene heightmap metadata when available. Faceted cached `plot_gg()` scenes
#' require an explicit `heightmap` for height export.
#' @param height_layer_name Default `"height"`. Name of the appended height
#' layer when `include_height = TRUE`.
#'
#' @returns A `terra::SpatRaster`.
#' @export
#'
#' @examplesIf length(find.package("terra", quiet = TRUE)) > 0
#' hillshade_img = elmat |>
#'   sphere_shade() |>
#'   add_shadow(ray_shade(), 0.5)
#'
#' hillshade_rast = spatialize_image(
#'   hillshade_img,
#'   extent = raster::extent(c(0, 1, 0, 1)),
#'   crs = "EPSG:4326"
#' )
spatialize_image = function(
	image,
	extent = NULL,
	crs = NULL,
	panel = NULL,
	include_alpha = FALSE,
	toRGB = TRUE,
	flip_vertical = FALSE,
	flip_horizontal = FALSE,
	layer_names = NULL,
	include_height = FALSE,
	heightmap = NULL,
	height_layer_name = "height"
) {
	if (!(length(find.package("terra", quiet = TRUE)) > 0)) {
		stop("`terra` package required for spatialize_image().", call. = FALSE)
	}
	if (
		!is.logical(include_alpha) ||
			length(include_alpha) != 1 ||
			is.na(include_alpha)
	) {
		stop("`include_alpha` must be `TRUE` or `FALSE`.", call. = FALSE)
	}
	if (!is.logical(toRGB) || length(toRGB) != 1 || is.na(toRGB)) {
		stop("`toRGB` must be `TRUE` or `FALSE`.", call. = FALSE)
	}
	if (
		!is.logical(include_height) ||
			length(include_height) != 1 ||
			is.na(include_height)
	) {
		stop("`include_height` must be `TRUE` or `FALSE`.", call. = FALSE)
	}
	if (
		!is.logical(flip_vertical) ||
			length(flip_vertical) != 1 ||
			is.na(flip_vertical)
	) {
		stop("`flip_vertical` must be `TRUE` or `FALSE`.", call. = FALSE)
	}
	if (
		!is.logical(flip_horizontal) ||
			length(flip_horizontal) != 1 ||
			is.na(flip_horizontal)
	) {
		stop("`flip_horizontal` must be `TRUE` or `FALSE`.", call. = FALSE)
	}
	if (
		!is.character(height_layer_name) ||
			length(height_layer_name) != 1 ||
			!nzchar(trimws(height_layer_name))
	) {
		stop(
			"`height_layer_name` must be a single non-empty character string.",
			call. = FALSE
		)
	}

	image_info = coerce_spatialize_image_input(
		image = image,
		include_alpha = include_alpha,
		caller = "spatialize_image"
	)
	image_array = flip_spatialize_image_array(
		image = image_info$image,
		flip_vertical = flip_vertical,
		flip_horizontal = flip_horizontal
	)
	image_array = convert_spatialize_image_to_rgb(
		image = image_array,
		toRGB = toRGB
	)

	resolved_extent = resolve_spatialize_image_extent(
		extent = extent,
		panel = panel,
		caller = "spatialize_image"
	)
	resolved_crs = resolve_spatialize_image_crs(
		crs = crs,
		explicit_extent = extent,
		resolved_extent = resolved_extent$extent,
		panel = panel,
		caller = "spatialize_image"
	)
	layer_names = resolve_spatialize_image_layer_names(
		layer_names = layer_names,
		nlayers = image_info$nlayers
	)

	raster = terra::rast(
		nrows = image_info$nrows,
		ncols = image_info$ncols,
		nlyrs = image_info$nlayers,
		xmin = resolved_extent$values["xmin"],
		xmax = resolved_extent$values["xmax"],
		ymin = resolved_extent$values["ymin"],
		ymax = resolved_extent$values["ymax"]
	)
	if (!is.null(resolved_crs)) {
		terra::crs(raster) = resolved_crs$wkt
	}
	terra::values(raster) = spatialize_image_values_matrix(
		image = image_array,
		nlayers = image_info$nlayers
	)
	names(raster) = layer_names
	raster = append_spatialize_image_height_layer(
		raster = raster,
		include_height = include_height,
		heightmap = heightmap,
		panel = panel,
		height_layer_name = height_layer_name,
		caller = "spatialize_image"
	)
	raster
}

coerce_spatialize_image_input = function(
	image,
	include_alpha = FALSE,
	caller = NULL
) {
	if (!(is.matrix(image) || is.array(image))) {
		image = rayimage::ray_read_image(image)
	}

	if (is.matrix(image)) {
		return(list(
			image = image,
			nrows = nrow(image),
			ncols = ncol(image),
			nlayers = 1L
		))
	}

	if (!is.array(image)) {
		stop(
			paste0(
				format_render_caller_prefix(caller),
				"`image` must resolve to a matrix or 3D array."
			),
			call. = FALSE
		)
	}

	image_dim = dim(image)
	if (length(image_dim) != 3) {
		stop(
			paste0(
				format_render_caller_prefix(caller),
				"`image` must be a 2D matrix or 3D array after reading."
			),
			call. = FALSE
		)
	}

	nchannels = image_dim[3]
	if (!nchannels %in% c(1L, 3L, 4L)) {
		stop(
			paste0(
				format_render_caller_prefix(caller),
				"`image` must have 1, 3, or 4 channels."
			),
			call. = FALSE
		)
	}

	if (nchannels == 4L && !isTRUE(include_alpha)) {
		image = image[,, 1:3, drop = FALSE]
		nchannels = 3L
	}

	list(
		image = image,
		nrows = image_dim[1],
		ncols = image_dim[2],
		nlayers = as.integer(nchannels)
	)
}

flip_spatialize_image_array = function(
	image,
	flip_vertical = FALSE,
	flip_horizontal = FALSE
) {
	if (isTRUE(flip_vertical)) {
		if (is.matrix(image)) {
			image = image[seq.int(nrow(image), 1L), , drop = FALSE]
		} else {
			image = image[seq.int(dim(image)[1], 1L), , , drop = FALSE]
		}
	}
	if (isTRUE(flip_horizontal)) {
		if (is.matrix(image)) {
			image = image[, seq.int(ncol(image), 1L), drop = FALSE]
		} else {
			image = image[, seq.int(dim(image)[2], 1L), , drop = FALSE]
		}
	}
	image
}

convert_spatialize_image_to_rgb = function(image, toRGB = FALSE) {
	if (!isTRUE(toRGB)) {
		return(image)
	}
	image = rayimage::render_gamma_linear(
		image,
		srgb_to_linear = FALSE
	)
	pmax(pmin(image * 255, 255), 0)
}

resolve_spatialize_image_extent = function(
	extent = NULL,
	panel = NULL,
	caller = NULL
) {
	resolved_extent = resolve_scene_render_extent(
		extent = extent,
		panel = panel,
		caller = caller
	)
	extent_values = tryCatch(
		get_extent(normalize_spatialize_image_extent_input(resolved_extent)),
		error = function(e) {
			stop(
				paste0(
					format_render_caller_prefix(caller),
					"Could not interpret `extent`: ",
					conditionMessage(e)
				),
				call. = FALSE
			)
		}
	)
	if (any(!is.finite(extent_values[c("xmin", "xmax", "ymin", "ymax")]))) {
		stop(
			paste0(
				format_render_caller_prefix(caller),
				"`extent` must contain finite xmin/xmax/ymin/ymax values."
			),
			call. = FALSE
		)
	}
	list(
		extent = resolved_extent,
		values = extent_values
	)
}

normalize_spatialize_image_extent_input = function(extent) {
	if (inherits(extent, c("sfc", "sfg"))) {
		return(sf::st_bbox(extent))
	}
	extent
}

infer_spatialize_image_extent_crs = function(extent) {
	if (is.null(extent)) {
		return(NULL)
	}

	parse_candidates = function(candidates) {
		for (candidate in candidates) {
			parsed_crs = try_parse_scene_crs(candidate)
			if (!is.null(parsed_crs)) {
				return(parsed_crs)
			}
		}
		NULL
	}

	if (inherits(extent, "SpatRaster")) {
		return(parse_candidates(list(
			tryCatch(terra::crs(extent), error = function(e) NULL),
			tryCatch(terra::crs(extent, proj = TRUE), error = function(e) NULL)
		)))
	}

	if (
		inherits(extent, c("RasterLayer", "RasterBrick", "RasterStack", "Spatial"))
	) {
		raster_crs = tryCatch(raster::crs(extent), error = function(e) NULL)
		return(parse_candidates(list(
			raster_crs,
			tryCatch(comment(raster_crs), error = function(e) NULL),
			tryCatch(slot(raster_crs, "projargs"), error = function(e) NULL),
			tryCatch(as.character(raster_crs), error = function(e) NULL),
			tryCatch(raster::projection(extent), error = function(e) NULL)
		)))
	}

	if (inherits(extent, c("sf", "sfc", "sfg", "bbox"))) {
		if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
			return(NULL)
		}
		return(parse_candidates(list(
			tryCatch(sf::st_crs(extent), error = function(e) NULL),
			tryCatch(attr(extent, "crs", exact = TRUE), error = function(e) NULL)
		)))
	}

	parse_candidates(list(
		tryCatch(attr(extent, "crs", exact = TRUE), error = function(e) NULL),
		tryCatch(attr(extent, "proj4string", exact = TRUE), error = function(e) {
			NULL
		})
	))
}

resolve_spatialize_image_crs = function(
	crs = NULL,
	explicit_extent = NULL,
	resolved_extent = NULL,
	panel = NULL,
	caller = NULL
) {
	if (!is.null(crs)) {
		if (
			(length(crs) == 1 && is.na(crs)) ||
				(is.character(crs) &&
					length(crs) >= 1 &&
					(!nzchar(trimws(crs[1])) || identical(trimws(crs[1]), "NA")))
		) {
			return(NULL)
		}
		return(parse_scene_crs(crs, caller = caller, arg_name = "crs"))
	}

	explicit_extent_crs = infer_spatialize_image_extent_crs(explicit_extent)
	if (!is.null(explicit_extent_crs)) {
		return(explicit_extent_crs)
	}

	try_parse_scene_crs(get_scene_target_crs(
		extent = resolved_extent,
		panel = panel,
		caller = caller
	))
}

resolve_spatialize_image_layer_names = function(
	layer_names = NULL,
	nlayers
) {
	if (is.null(layer_names)) {
		return(switch(
			as.character(nlayers),
			"1" = "value",
			"3" = c("red", "green", "blue"),
			"4" = c("red", "green", "blue", "alpha"),
			stop("Unsupported layer count.", call. = FALSE)
		))
	}
	if (!is.character(layer_names) || length(layer_names) != nlayers) {
		stop(
			sprintf(
				"`layer_names` must be a character vector of length %d.",
				nlayers
			),
			call. = FALSE
		)
	}
	layer_names
}

spatialize_image_values_matrix = function(image, nlayers) {
	if (nlayers == 1L) {
		return(as.vector(t(image)))
	}
	matrix(
		aperm(image, c(2, 1, 3)),
		ncol = nlayers
	)
}

append_spatialize_image_height_layer = function(
	raster,
	include_height = FALSE,
	heightmap = NULL,
	panel = NULL,
	height_layer_name = "height",
	caller = NULL
) {
	if (!isTRUE(include_height)) {
		return(raster)
	}
	height_raster = resolve_spatialize_image_height_layer(
		template = raster[[1]],
		heightmap = heightmap,
		panel = panel,
		height_layer_name = height_layer_name,
		caller = caller
	)
	c(raster, height_raster)
}

resolve_spatialize_image_height_layer = function(
	template,
	heightmap = NULL,
	panel = NULL,
	height_layer_name = "height",
	caller = NULL
) {
	if (is.null(heightmap)) {
		panel_info = get_cached_plot_gg_panel_info(default = NULL)
		if (!is.null(panel_info) && nrow(panel_info) > 1) {
			stop(
				paste0(
					format_render_caller_prefix(caller),
					"Cached height export is not supported for faceted `plot_gg()` scenes. Supply `heightmap` explicitly."
				),
				call. = FALSE
			)
		}
		heightmap = resolve_hillshade_heightmap(
			heightmap = NULL,
			heightmap_missing = TRUE,
			caller = caller
		)$heightmap
	}

	heightmap_info = coerce_plot_3d_heightmap(heightmap)
	heightmap_matrix = t(heightmap_info$heightmap)
	if (!is.matrix(heightmap_matrix)) {
		stop(
			paste0(
				format_render_caller_prefix(caller),
				"`heightmap` must resolve to a two-dimensional matrix or spatial raster."
			),
			call. = FALSE
		)
	}

	source_height_raster = terra::rast(
		nrows = terra::nrow(template),
		ncols = terra::ncol(template),
		xmin = terra::xmin(template),
		xmax = terra::xmax(template),
		ymin = terra::ymin(template),
		ymax = terra::ymax(template)
	)
	template_crs = tryCatch(terra::crs(template), error = function(e) "")
	if (is.character(template_crs) && length(template_crs) && nzchar(template_crs[1])) {
		terra::crs(source_height_raster) = template_crs
	}
	if (
		terra::nrow(source_height_raster) == nrow(heightmap_matrix) &&
			terra::ncol(source_height_raster) == ncol(heightmap_matrix)
	) {
		height_values = heightmap_matrix
	} else {
		height_values = interpolate_spatialize_image_height_matrix(
			heightmap = heightmap_matrix,
			nrows = terra::nrow(source_height_raster),
			ncols = terra::ncol(source_height_raster)
		)
	}
	terra::values(source_height_raster) = spatialize_image_values_matrix(
		height_values,
		nlayers = 1L
	)

	names(source_height_raster) = height_layer_name
	source_height_raster
}

interpolate_spatialize_image_height_matrix = function(
	heightmap,
	nrows,
	ncols
) {
	x_coords = rep(seq(1, ncol(heightmap), length.out = ncols), each = nrows)
	y_coords = rep(seq(1, nrow(heightmap), length.out = nrows), times = ncols)
	interpolated_vals = rayimage::interpolate_array(heightmap, x_coords, y_coords)
	matrix(interpolated_vals, nrow = nrows, byrow = FALSE)
}
