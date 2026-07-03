#' Check for ggplot height palette color sentinel
#'
#' @param color Color argument value.
#'
#' @return Logical value.
#' @keywords internal
is_ggplot_height_palette_color = function(color) {
  if (!is.character(color) || length(color) != 1 || is.na(color)) {
    return(FALSE)
  }
  color = tolower(gsub("[-_[:space:]]", "", color))
  color %in% c("height", "ggplotheight", "plotggheight")
}

#' Map values through cached plot_gg height palette
#'
#' @param values Values to map.
#' @param heightmap Default `NULL`. Height matrix with cached plot_gg metadata.
#' @param transform_info Default `NULL`. Cached plot_gg transform info.
#' @param caller Default `NULL`. Calling function name used in error messages.
#' @param arg_name Default `"color"`. Color argument name used in error messages.
#'
#' @return Character vector of colors.
#' @keywords internal
map_plot_gg_height_palette = function(
  values,
  heightmap = NULL,
  transform_info = NULL,
  caller = NULL,
  arg_name = "color"
) {
  if (is.null(values) || !length(values)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`",
        arg_name,
        " = \"height\"` requires height values for the render object."
      ),
      call. = FALSE
    )
  }
  if (is.null(transform_info)) {
    transform_info = get_cached_plot_gg_transform_info(
      heightmap = heightmap,
      default = NULL
    )
  }
  if (is.null(transform_info) || is.null(transform_info$height_color_scale)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`",
        arg_name,
        " = \"height\"` requires a scene created by `plot_gg()` with a ",
        "mapped height aesthetic."
      ),
      call. = FALSE
    )
  }
  height_color_scale = transform_info$height_color_scale
  mapped = tryCatch(
    height_color_scale$map(values),
    error = function(e) NULL
  )
  if (is.null(mapped) || !length(mapped)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Could not map values through the cached `plot_gg()` height palette."
      ),
      call. = FALSE
    )
  }
  mapped = as.character(mapped)
  if (length(mapped) == 1 && length(values) > 1) {
    mapped = rep(mapped, length(values))
  }
  mapped[is.na(mapped) | !nzchar(mapped)] = "grey50"
  mapped
}

#' Resolve ggplot height palette color sentinel
#'
#' @param color Color argument value.
#' @param values Values to map if `color` requests the ggplot height palette.
#' @param heightmap Default `NULL`. Height matrix with cached plot_gg metadata.
#' @param caller Default `NULL`. Calling function name used in error messages.
#' @param arg_name Default `"color"`. Color argument name used in error messages.
#'
#' @return Original color value or mapped colors.
#' @keywords internal
resolve_ggplot_height_palette_color = function(
  color,
  values,
  heightmap = NULL,
  caller = NULL,
  arg_name = "color"
) {
  if (!is_ggplot_height_palette_color(color)) {
    return(color)
  }
  map_plot_gg_height_palette(
    values = values,
    heightmap = heightmap,
    caller = caller,
    arg_name = arg_name
  )
}
