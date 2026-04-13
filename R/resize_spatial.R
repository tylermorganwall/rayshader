#' Resize a SpatRaster by relative scale while preserving spatial metadata
#'
#' `resize_spatial()` mirrors the ergonomics of `rayshader::resize_matrix()`,
#' but returns a `terra::SpatRaster` with extent and CRS intact.
#'
#' Downsampling and upsampling are handled separately because the best visual
#' method is often different in each direction. Integer-factor reductions can
#' use `terra::aggregate()`; everything else uses `terra::resample()` onto a
#' template raster with the requested geometry.
#'
#' @param x Default none. A `terra::SpatRaster`.
#' @param scale Default `1`. Relative output size. Values below `1` reduce resolution and values above `1` increase resolution.
#' @param width Default `NULL`. Target number of columns. If `NULL`, computed from `scale`.
#' @param height Default `NULL`. Target number of rows. If `NULL`, computed from `scale`.
#' @param method_down Default `"mean"`. Method used when reducing resolution. Aggregation methods such as `"mean"` or `"max"` will use `terra::aggregate()` when the implied factor is integer; otherwise this is passed to `terra::resample()`.
#' @param method_up Default `"bilinear"`. Method used when increasing resolution. Passed to `terra::resample()`.
#' @param aggregate_if_possible Default `TRUE`. If `TRUE`, use `terra::aggregate()` for integer-factor downsampling when `method_down` is an aggregation method.
#' @param tol Default `1e-8`. Tolerance for deciding whether an implied aggregation factor is effectively an integer.
#' @param threads Default `FALSE`. Passed to `terra::resample()`.
#' @param filename Default `""`. Optional output filename.
#' @param overwrite Default `FALSE`. Whether to overwrite `filename`.
#' @param ... Default none. Additional arguments passed to `terra::aggregate()`, `terra::resample()`, or `terra::writeRaster()`.
#'
#' @returns A `terra::SpatRaster`.
#' @export
resize_spatial = function(
	x,
	scale = 1,
	width = NULL,
	height = NULL,
	method_down = "mean",
	method_up = "bilinear",
	aggregate_if_possible = TRUE,
	tol = 1e-8,
	threads = FALSE,
	filename = "",
	overwrite = FALSE,
	...
) {
	if (!inherits(x, "SpatRaster")) {
		stop("`x` must be a terra::SpatRaster.", call. = FALSE)
	}

	if (!is.numeric(scale) || length(scale) != 1 || is.na(scale) || scale <= 0) {
		stop("`scale` must be a single positive number.", call. = FALSE)
	}

	current_ncol = terra::ncol(x)
	current_nrow = terra::nrow(x)

	if (is.null(width) && is.null(height)) {
		width = scale * current_ncol
		height = scale * current_nrow
	} else {
		if (any(is.null(c(width, height)))) {
			stop(
				"If specifying explicit width and height, both must be passed in as arguments.",
				call. = FALSE
			)
		}
	}

	if (
		!is.numeric(width) ||
			!is.numeric(height) ||
			length(width) != 1 ||
			length(height) != 1 ||
			is.na(width) ||
			is.na(height) ||
			width <= 0 ||
			height <= 0
	) {
		stop("`width` and `height` must be single positive numbers.", call. = FALSE)
	}

	if (width <= 1 && height <= 1) {
		width = width * current_ncol
		height = height * current_nrow
	}

	width = max(1L, as.integer(round(width)))
	height = max(1L, as.integer(round(height)))

	if (identical(method_down, "mode")) {
		method_down = "modal"
	}
	if (identical(method_up, "mode")) {
		method_up = "modal"
	}

	valid_resample_methods = c(
		"bilinear",
		"mean",
		"near",
		"modal",
		"cubic",
		"cubicspline",
		"lanczos",
		"sum",
		"min",
		"q1",
		"median",
		"q3",
		"max",
		"rms"
	)

	valid_aggregate_methods = c(
		"mean",
		"max",
		"min",
		"median",
		"sum",
		"modal",
		"any",
		"all",
		"prod",
		"which.min",
		"which.max",
		"table",
		"sd",
		"std"
	)

	valid_methods = unique(c(valid_resample_methods, valid_aggregate_methods))

	if (
		!is.character(method_down) ||
			length(method_down) != 1 ||
			!method_down %in% valid_methods
	) {
		stop(
			paste0(
				"`method_down` must be one of: ",
				paste(valid_methods, collapse = ", "),
				"."
			),
			call. = FALSE
		)
	}

	if (
		!is.character(method_up) ||
			length(method_up) != 1 ||
			!method_up %in% valid_resample_methods
	) {
		stop(
			paste0(
				"`method_up` must be one of: ",
				paste(valid_resample_methods, collapse = ", "),
				"."
			),
			call. = FALSE
		)
	}

	if (width == current_ncol && height == current_nrow) {
		if (nzchar(filename)) {
			return(terra::writeRaster(
				x,
				filename = filename,
				overwrite = overwrite,
				...
			))
		}
		return(x)
	}

	factor_x = current_ncol / width
	factor_y = current_nrow / height
	is_downsample = (width < current_ncol) || (height < current_nrow)

	if (is_downsample) {
		can_aggregate = isTRUE(aggregate_if_possible) &&
			factor_x >= 1 &&
			factor_y >= 1 &&
			abs(factor_x - round(factor_x)) <= tol &&
			abs(factor_y - round(factor_y)) <= tol &&
			method_down %in% valid_aggregate_methods

		if (can_aggregate) {
			return(
				terra::aggregate(
					x,
					fact = c(as.integer(round(factor_y)), as.integer(round(factor_x))),
					fun = method_down,
					filename = filename,
					overwrite = overwrite,
					...
				)
			)
		}

		method = method_down
	} else {
		method = method_up
	}

	template = terra::rast(
		nrows = height,
		ncols = width,
		xmin = terra::xmin(x),
		xmax = terra::xmax(x),
		ymin = terra::ymin(x),
		ymax = terra::ymax(x),
		crs = terra::crs(x)
	)

	terra::resample(
		x,
		y = template,
		method = method,
		threads = threads,
		filename = filename,
		overwrite = overwrite,
		...
	)
}
