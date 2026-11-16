#' Convert Color
#'
#' @param color The color to convert. Can be an R color name, a hexadecimal code, an `rgb(r, g, b)` string
#' with channels between `0` and `255`, or a numeric rgb
#' vector listing three intensities between `0` and `1`.
#'
#' @param as_hex Default `FALSE`. Return a hexadecimal color instead of numeric channels.
#' @param linear Default `FALSE`. Convert numeric output from sRGB to linear RGB.
#' @return Color vector
#' @keywords internal
convert_color = function(color, as_hex = FALSE, linear = FALSE) {
  if (inherits(color, "character")) {
    color = as.vector(col2rgb(normalize_rgb_color(color))) / 255
  }
  if (linear & !as_hex) {
    color = rayimage::render_gamma_linear(color)
  }
  if (!all(color <= 1)) {
    stop("invalid color")
  }
  if (!all(color >= 0)) {
    stop("invalid color")
  }
  if (as_hex) {
    paste0(
      "#",
      paste0(
        format(as.hexmode(round(color * 255, 0)), width = 2),
        collapse = ""
      ),
      collapse = ""
    )
  } else {
    color
  }
}

#' Convert Color to Linear RGB
#'
#' @param color Color name, hexadecimal color, RGB string, or numeric RGB values.
#' @return Linear RGB color values.
#' @keywords internal
col2rgb_linear = function(color) {
  rayimage::render_gamma_linear(normalize_rgb_color(color))
}

#' @param color Color input, including character vectors with comma-separated
#'   `rgb(r, g, b)` strings whose channels are between `0` and `255`.
#' @return The input with RGB strings converted to hexadecimal colors. Other
#'   values, including numeric RGB vectors, names, and missing values, are unchanged.
#' @keywords internal
#' @noRd
normalize_rgb_color = function(color) {
  if (!is.character(color) || !length(color)) {
    return(color)
  }
  rgb_indices = which(grepl(
    "^\\s*rgb\\b",
    color,
    ignore.case = TRUE,
    perl = TRUE
  ))
  if (!length(rgb_indices)) {
    return(color)
  }

  # Parse only three comma-separated channels; never evaluate the supplied text.
  # Normalize before downstream R/rgl color handling so its existing alpha,
  # palette-index, and numeric-vector semantics remain unchanged.
  channel = "([0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)"
  pattern = paste0(
    "^\\s*rgb\\s*\\(\\s*",
    paste(rep(channel, 3), collapse = "\\s*,\\s*"),
    "\\s*\\)\\s*$"
  )
  matches = regmatches(
    color[rgb_indices],
    regexec(pattern, color[rgb_indices], ignore.case = TRUE, perl = TRUE)
  )
  valid = lengths(matches) == 4L
  if (!all(valid)) {
    stop(
      "Invalid RGB color: ",
      color[rgb_indices[which(!valid)[1]]],
      ". Use rgb(r, g, b) with three numeric channels between 0 and 255.",
      call. = FALSE
    )
  }
  channels = t(vapply(
    matches,
    function(match) as.numeric(match[-1]),
    numeric(3)
  ))
  if (any(!is.finite(channels) | channels > 255)) {
    stop("RGB color channels must be between 0 and 255.", call. = FALSE)
  }
  color[rgb_indices] = grDevices::rgb(
    channels[, 1],
    channels[, 2],
    channels[, 3],
    maxColorValue = 255
  )
  color
}
