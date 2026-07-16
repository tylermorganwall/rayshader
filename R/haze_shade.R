#' @title Calculate Haze Overlay
#'
#' @description Generates a semi-transparent constant-color overlay whose
#' opacity follows an exponential atmospheric column model.
#'
#' @details
#' `haze_shade()` creates a haze overlay whose alpha channel is largest near
#' `reference_height` and decreases exponentially with elevation above that
#' height.
#'
#' The haze opacity is computed as:
#'
#' ```
#' height_rel = heightmap - reference_height
#' log_tau = log(optical_depth) - height_rel / scale_height
#' haze_alpha = -expm1(-exp(log_tau))
#' ```
#'
#' `optical_depth` controls the haze strength at `reference_height`.
#' `scale_height` controls how quickly haze decreases with elevation. After
#' increasing elevation by one `scale_height`, optical depth is multiplied by
#' `exp(-1)`.
#'
#' When `scale_height = NULL`, the automatic scale height depends on the finite
#' elevation range: it is 20 percent of that range for non-flat terrain and one
#' elevation unit for flat terrain. Consequently, subsetting a heightmap or
#' adding an extreme outlier can change the default haze profile.
#'
#' The final alpha channel is multiplied by `alpha` and clamped to `[0, 1]`.
#'
#' @param heightmap A two-dimensional matrix, where each entry in the matrix is
#' the elevation at that point. If omitted, the function attempts to use the
#' cached heightmap from the current rayshader context.
#' @param color Default `"white"`. Color of the haze overlay.
#' @param optical_depth Default `0.5`. Optical depth of the haze layer at
#' `reference_height`. Larger values make haze more opaque at all elevations.
#' @param scale_height Default `NULL`. Vertical distance over which haze optical
#' depth decreases by a factor of `exp(-1)`. If `scale_height_relative = FALSE`,
#' this is in the same elevation units as `heightmap`. If
#' `scale_height_relative = TRUE`, this is a proportion of the heightmap
#' elevation range and must be in `(0, 1]`. If `NULL`, defaults to `0.2` of the
#' elevation range, or `1` for a flat heightmap.
#' @param scale_height_relative Default `FALSE`. If `TRUE`, interpret
#' `scale_height` as a fraction of the heightmap elevation range rather than as
#' an absolute elevation distance.
#' @param reference_height Default `NULL`. Elevation where haze optical depth is
#' equal to `optical_depth`. Elevations above this height have less haze;
#' elevations below this height have more haze. If `NULL`, uses the minimum
#' finite elevation in `heightmap`.
#' @param alpha Default `1`. Multiplier applied to the final haze alpha. Must be
#' in `[0, 1]`.
#' @param blur Default `0`. Gaussian standard deviation in output-image pixels.
#' `0` disables convolution. Positive values use a normalized kernel spanning
#' approximately plus or minus three standard deviations.
#'
#' @return A 4-layer RGBA array representing the haze overlay.
#' @export
#'
#' @examples
#' # Prepare a mountain heightmap.
#' mb_mountains = raster_to_matrix(montereybay_spatial)
#' mb_mountains[mb_mountains < 0] = 0
#' mb_mountains = mb_mountains[271:540, 271:540]
#'
#' # Render terrain without haze.
#' no_haze = mb_mountains |>
#'   sphere_shade(
#'     texture = "imhof2",
#'     zscale = 200,
#'     vertical_exaggeration = 4
#'   ) |>
#'   add_shadow(
#'     lamb_shade(
#'       mb_mountains,
#'       vertical_exaggeration = 4
#'     ),
#'     0
#'   ) |>
#'   add_water(
#'     detect_water(mb_mountains, cutoff = 0.9999999),
#'     "dodgerblue4"
#'   )
#'
#' plot_map(no_haze)
#'
#' # Add default white haze.
#' no_haze |>
#'   add_overlay(
#'     haze_shade(mb_mountains)
#'   ) |>
#'   plot_map()
#'
#' # Increase optical depth to make the haze denser everywhere.
#' no_haze |>
#'   add_overlay(
#'     haze_shade(
#'       mb_mountains,
#'       optical_depth = 1
#'     )
#'   ) |>
#'   plot_map()
#'
#' # Use a smaller scale height (150 meters) to keep haze concentrated in valleys.
#' no_haze |>
#'   add_overlay(
#'     haze_shade(
#'       mb_mountains,
#'       optical_depth = 0.7,
#'       scale_height = 150
#'     )
#'   ) |>
#'   plot_map()
#'
#' # Express scale height as a fraction of the elevation range.
#' no_haze |>
#'   add_overlay(
#'     haze_shade(
#'       mb_mountains,
#'       optical_depth = 0.7,
#'       scale_height = 0.02,
#'       scale_height_relative = TRUE
#'     )
#'   ) |>
#'   plot_map()
#'
#' # Raise the haze layer by setting the reference height.
#' no_haze |>
#'   add_overlay(
#'     haze_shade(
#'       mb_mountains,
#'       optical_depth = 0.6,
#'       reference_height = as.numeric(quantile(mb_mountains, 0.25))
#'     )
#'   ) |>
#'   plot_map()
#'
#' # Add bluish atmospheric haze.
#' no_haze |>
#'   add_overlay(
#'     haze_shade(
#'       mb_mountains,
#'       color = "#abbbca",
#'       optical_depth = 1
#'     )
#'   ) |>
#'   plot_map()
#'
#' # Blur the haze alpha map to soften transitions.
#' no_haze |>
#'   add_overlay(
#'     haze_shade(
#'       mb_mountains,
#'       optical_depth = 1,
#'       blur = 2
#'     )
#'   ) |>
#'   plot_map()
haze_shade = function(
  heightmap,
  color = "white",
  optical_depth = 0.5,
  scale_height = NULL,
  scale_height_relative = FALSE,
  reference_height = NULL,
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

  if (!is.matrix(heightmap)) {
    stop("`heightmap` must be a matrix.", call. = FALSE)
  }

  finite_height = heightmap[is.finite(heightmap)]

  if (length(finite_height) == 0) {
    stop(
      "`heightmap` must contain at least one finite elevation.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(optical_depth) ||
      length(optical_depth) != 1 ||
      !is.finite(optical_depth) ||
      optical_depth < 0
  ) {
    stop(
      "`optical_depth` must be a single finite number greater than or equal to 0.",
      call. = FALSE
    )
  }

  if (
    !is.logical(scale_height_relative) ||
      length(scale_height_relative) != 1 ||
      is.na(scale_height_relative)
  ) {
    stop(
      "`scale_height_relative` must be a single `TRUE` or `FALSE`.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(alpha) ||
      length(alpha) != 1 ||
      !is.finite(alpha) ||
      alpha < 0 ||
      alpha > 1
  ) {
    stop("`alpha` must be a single finite number in [0, 1].", call. = FALSE)
  }

  if (
    !is.numeric(blur) ||
      length(blur) != 1 ||
      !is.finite(blur) ||
      blur < 0
  ) {
    stop(
      "`blur` must be a single finite number greater than or equal to 0.",
      call. = FALSE
    )
  }
  if (blur > 0) {
    kernel_radius = ceiling(3 * blur)
    kernel_dim = 2L * kernel_radius + 1L
    if (!is.finite(kernel_dim) || kernel_dim > 501) {
      stop(
        "`blur` is too large: the Gaussian kernel cannot exceed 501 by 501 pixels.",
        call. = FALSE
      )
    }
  }

  elev_range = range(finite_height)
  elev_span = diff(elev_range)

  if (is.null(reference_height)) {
    reference_height = elev_range[1]
  }

  if (
    !is.numeric(reference_height) ||
      length(reference_height) != 1 ||
      !is.finite(reference_height)
  ) {
    stop(
      "`reference_height` must be a single finite numeric value.",
      call. = FALSE
    )
  }

  if (scale_height_relative) {
    if (is.null(scale_height)) {
      scale_height = 0.2
    }

    if (
      !is.numeric(scale_height) ||
        length(scale_height) != 1 ||
        !is.finite(scale_height) ||
        scale_height <= 0 ||
        scale_height > 1
    ) {
      stop(
        "`scale_height` must be a single finite number in (0, 1] when `scale_height_relative = TRUE`.",
        call. = FALSE
      )
    }

    scale_height = if (elev_span > 0) {
      scale_height * elev_span
    } else {
      1
    }
  } else {
    if (is.null(scale_height)) {
      scale_height = if (elev_span > 0) {
        0.2 * elev_span
      } else {
        1
      }
    }

    if (
      !is.numeric(scale_height) ||
        length(scale_height) != 1 ||
        !is.finite(scale_height) ||
        scale_height <= 0
    ) {
      stop(
        "`scale_height` must be a single finite number greater than 0.",
        call. = FALSE
      )
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
  finite_mask = is.finite(heightmap_img)
  alpha_map = matrix(0, nrow = nrow(heightmap_img), ncol = ncol(heightmap_img))

  if (optical_depth > 0) {
    log_tau = log(optical_depth) - height_rel[finite_mask] / scale_height
    saturation_tau = -log(.Machine$double.eps)
    is_saturated = log_tau >= log(saturation_tau)
    finite_alpha = numeric(length(log_tau))
    finite_alpha[is_saturated] = 1
    if (any(!is_saturated)) {
      tau = exp(log_tau[!is_saturated])
      finite_alpha[!is_saturated] = -expm1(-tau)
    }
    alpha_map[finite_mask] = pmin(pmax(finite_alpha, 0), 1)
  }

  if (blur > 0) {
    maximum_image_kernel = 2 * min(dim(alpha_map)) + 1
    if (kernel_dim > maximum_image_kernel) {
      stop(
        paste0(
          "`blur` is too large for the output dimensions: the ",
          "kernel would be ",
          kernel_dim,
          " by ",
          kernel_dim,
          " pixels."
        ),
        call. = FALSE
      )
    }
    kernel_coordinates = seq.int(-kernel_radius, kernel_radius)
    kernel_1d = exp(-0.5 * (kernel_coordinates / blur)^2)
    kernel = outer(kernel_1d, kernel_1d)
    kernel = kernel / sum(kernel)
    alpha_map = rayimage::render_convolution(
      alpha_map,
      kernel = kernel
    )
    alpha_map = pmin(pmax(alpha_map, 0), 1)
  }
  alpha_map[!finite_mask] = 0

  haze[,, 4] = alpha_map * alpha

  haze = rayimage::ray_read_image(
    haze,
    assume_colorspace = rayimage::CS_SRGB,
    assume_white = "D65"
  )

  cache_hillshade_map(haze, label = hillshade_cache_label)

  return(haze)
}
