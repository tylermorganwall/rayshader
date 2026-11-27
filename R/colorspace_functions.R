#' @title Darken Color
#'
#' @description Scale CIELuv lightness and chroma together to darken a color
#' while preserving its chromaticity.
#'
#' @param col Color name, hexadecimal color, or numeric sRGB vector with values
#' between `0` and `1`.
#' @param darken Default `0.3`. Lightness multiplier. Values between `0` and `1`
#' darken the color; `0` returns black and `1` preserves the original color.
#' @return Numeric sRGB color vector with values between `0` and `1`.
#' @keywords internal
darken_color = function(col, darken = 0.3) {
  color_luv = grDevices::convertColor(
    convert_color(col),
    from = "sRGB",
    to = "Luv"
  )
  # Scaling chroma with lightness preserves hue and avoids out-of-gamut blues.
  as.numeric(grDevices::convertColor(
    color_luv * darken,
    from = "Luv",
    to = "sRGB"
  ))
}
