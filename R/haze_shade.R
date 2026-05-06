#' @title Calculate Haze Overlay
#'
#' @description Generates a semi-transparent constant-color overlay whose
#' opacity follows an exponential atmospheric column model.
#'
#' @param heightmap Default `missing`. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' @param color Default `"white"`. Color of the airlight term.
#' @param optical_depth Default `0.5`. Near-surface optical depth of the haze layer.
#' @param scale_height Default `NULL`. Atmospheric scale height in the same units as `heightmap`, or a proportion in `(0, 1]` if `relative = TRUE`.
#' @param relative Default `FALSE`. If `TRUE`, interpret `scale_height` as a proportion of the height range.
#' @param reference_height Default `NULL`. Reference elevation for zero relative height. If `NULL`, uses `min(heightmap)`.
#' @param view_cos Default `1`. Cosine of the view zenith angle.
#' @param alpha Default `1`. Multiplier applied to the final haze alpha.
#' @param blur Default `0`. Smoothing applied to the haze alpha map. Set to `0` to disable.
#' @return 4-layer RGB array representing the haze overlay.
#' @export
#' @examples
#' montereybay |>
#'	raster_to_matrix() |>
#'		(\(x) {
#'			x[x < 0] = 0
#'			x[1:270 + 270, 1:270 + 270]
#'		})() -> mb_mountains
#'
#'	# Add haze
#'	mb_mountains |>
#'		sphere_shade(texture = "imhof2", zscale = 20) |>
#'		add_shadow(lamb_shade(), 0) |>
#'		add_overlay(haze_shade(optical_depth = 0.3)) |>
#'		plot_map()
#'
#'	# Add thicker haze
#'	mb_mountains |>
#'		sphere_shade(texture = "imhof2", zscale = 20) |>
#'		add_shadow(lamb_shade(), 0) |>
#'		add_overlay(haze_shade(optical_depth = 0.6)) |>
#'		plot_map()
#'
#'	# Add bluish haze
#'	mb_mountains |>
#'		sphere_shade(texture = "imhof2", zscale = 20) |>
#'		add_shadow(lamb_shade(), 0) |>
#'		add_overlay(haze_shade(optical_depth = 0.3, color = "skyblue")) |>
#'		plot_map()
#' 
haze_shade = function(
	heightmap,
	color = "white",
	optical_depth = 0.5,
	scale_height = NULL,
	relative = FALSE,
	reference_height = NULL,
	view_cos = 1,
	alpha = 1,
	blur = 0
) {
	heightmap_missing = missing(heightmap)
	heightmap_cache_label = format_scene_cache_label(deparse(substitute(
		heightmap
	)))

	if (heightmap_missing) {
		resolved_heightmap = resolve_hillshade_heightmap(
			heightmap_missing = TRUE,
			caller = "haze_shade"
		)
		heightmap = resolved_heightmap$heightmap
	} else {
		heightmap_info = coerce_plot_3d_heightmap(heightmap)
		heightmap = heightmap_info$heightmap
		cache_hillshade_input_context(heightmap_info, label = heightmap_cache_label)
	}

	hillshade_cache_label = if (heightmap_missing) {
		resolved_heightmap$label
	} else {
		heightmap_cache_label
	}

	stopifnot(is.matrix(heightmap))

	if (
		!is.numeric(optical_depth) ||
			length(optical_depth) != 1 ||
			optical_depth < 0
	) {
		stop("`optical_depth` must be a single number greater than or equal to 0.")
	}
	if (!is.logical(relative) || length(relative) != 1) {
		stop("`relative` must be a single `TRUE` or `FALSE`.")
	}
	if (
		!is.numeric(view_cos) ||
			length(view_cos) != 1 ||
			view_cos <= 0 ||
			view_cos > 1
	) {
		stop("`view_cos` must be a single number in (0, 1].")
	}
	if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1) {
		stop("`alpha` must be a single number between 0 and 1.")
	}
	if (!is.numeric(blur) || length(blur) != 1 || blur < 0) {
		stop("`blur` must be a single number greater than or equal to 0.")
	}

	elev_range = range(heightmap, na.rm = TRUE)
	elev_span = diff(elev_range)

	if (is.null(reference_height)) {
		reference_height = elev_range[1]
	}
	if (!is.numeric(reference_height) || length(reference_height) != 1) {
		stop("`reference_height` must be a single numeric value.")
	}

	if (relative) {
		if (is.null(scale_height)) {
			scale_height = 0.2
		}
		if (
			!is.numeric(scale_height) ||
				length(scale_height) != 1 ||
				scale_height <= 0 ||
				scale_height > 1
		) {
			stop(
				"`scale_height` must be a single number in (0, 1] when `relative = TRUE`."
			)
		}
		scale_height = if (elev_span > 0) scale_height * elev_span else 1
	} else {
		if (is.null(scale_height)) {
			scale_height = if (elev_span > 0) 0.2 * elev_span else 1
		}
		if (
			!is.numeric(scale_height) ||
				length(scale_height) != 1 ||
				scale_height <= 0
		) {
			stop("`scale_height` must be a single number greater than 0.")
		}
	}

	haze = constant_shade(heightmap, color = color, alpha = 1)

	heightmap_img = t(heightmap)
	if (any(dim(heightmap_img) != dim(haze)[1:2])) {
		heightmap_img = rayimage::render_resized(
			heightmap_img,
			dims = dim(haze)[1:2]
		)
	}

	height_rel = heightmap_img - reference_height

	tau_map = optical_depth * exp(-height_rel / scale_height)
	alpha_map = 1 - exp(-tau_map / view_cos)
	alpha_map = pmin(pmax(alpha_map, 0), 1)

	if (blur > 0) {
		alpha_map = rayimage::render_convolution(
			alpha_map,
			kernel = blur,
			kernel_dim = 21
		)
		alpha_map = pmin(pmax(alpha_map, 0), 1)
	}

	haze[,, 4] = alpha_map * alpha

	haze = rayimage::ray_read_image(
		haze,
		assume_colorspace = rayimage::CS_SRGB,
		assume_white = "D65"
	)

	cache_hillshade_map(haze, label = hillshade_cache_label)
	return(haze)
}
