#'@title Add Overlay
#'
#'@description Overlays an image (with a transparency layer) on the current map.
#'
#'@param hillshade A three-dimensional RGB array or 2D matrix of shadow intensities.
#'@param overlay A three or four dimensional RGB array, where the 4th dimension represents the alpha (transparency) channel.
#'If the array is 3D, `alpha_color` should also be passed to indicate transparent regions.
#'@param alpha_layer Default `1`. Defines minimum tranparaency of layer. If transparency already exists in `overlay`, the way [add_overlay()] combines
#'the two is determined in argument `alpha_method`.
#'@param alpha_color Default `NULL`. If `overlay` is a 3-layer array, this argument tells which color is interpretted as completely transparent.
#'@param alpha_method Default `max`. Method for dealing with pre-existing transparency with `layeralpha`.
#'If `max`, converts all alpha levels higher than `layeralpha` to the value set in `layeralpha`. Otherwise,
#'this just sets all transparency to `layeralpha`.
#'@param color_epsilon Default `1e-3`. Tolerance for equality for alpha_color to determine transparency.
#'@param rescale_original Default `FALSE`. If `TRUE`, `hillshade` will be scaled to match the dimensions of `overlay` (instead of
#'the other way around).
#'@return Hillshade with overlay.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Combining base R plotting with rayshader's spherical color mapping and raytracing:
#'montereybay_spatial |>
#'   sphere_shade() |>
#'   add_overlay(height_shade(),alpha_layer = 0.6)  |>
#'   add_shadow(ray_shade(vertical_exaggeration = 4)) |>
#'   plot_map()
#'
#'# Add contours with `generate_contour_overlay()`
#'montereybay_spatial |>
#'   height_shade() |>
#'   add_overlay(generate_contour_overlay(montereybay_spatial))  |>
#'   add_shadow(ray_shade(vertical_exaggeration = 4)) |>
#'   plot_map()
add_overlay = function(
  hillshade = NULL,
  overlay = NULL,
  alpha_layer = 1,
  alpha_color = NULL,
  alpha_method = "max",
  color_epsilon = 1e-3,
  rescale_original = FALSE
) {
  force(hillshade)
  hillshade_cache_label = get_hillshade_map_label(
    default = format_scene_cache_label(deparse(substitute(hillshade)))
  )
  if (any(alpha_layer > 1 || alpha_layer < 0)) {
    stop("Argument `alpha_layer` must not be less than 0 or more than 1")
  }
  overlay = rayimage::ray_read_image(overlay)

  if (alpha_layer != 1) {
    if (alpha_method == "max") {
      overlay[,, 4][overlay[,, 4] > alpha_layer] = alpha_layer
    } else {
      overlay[,, 4] = alpha_layer
    }
  }
  eq = function(x, y) abs(x - y) < color_epsilon
  if (!is.null(alpha_color)) {
    colorvals = col2rgb_linear(alpha_color)
    alphalayer1 = eq(overlay[,, 1], colorvals[1]) &
      eq(overlay[,, 2], colorvals[2]) &
      eq(overlay[,, 3], colorvals[3])
    temp_over = overlay[,, 4]
    temp_over[alphalayer1] = 0
    overlay[,, 4] = temp_over
  }
  if (is.null(hillshade)) {
    return(overlay)
  }
  if (is.null(overlay)) {
    return(hillshade)
  }
  if (length(dim(hillshade)) == 2) {
    hillshade = rayimage::render_image_overlay(
      fliplr(t(hillshade)),
      overlay,
      alpha = alpha_layer,
      rescale_original = rescale_original
    )
  } else {
    hillshade = rayimage::render_image_overlay(
      hillshade,
      overlay,
      alpha = alpha_layer,
      rescale_original = rescale_original
    )
  }
  cache_hillshade_map(hillshade, label = hillshade_cache_label)
  return(hillshade)
}
