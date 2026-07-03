#'@title Coerce render data column to numeric
#'
#'@param data_values Values to coerce.
#'@param data_column_name Column name used for diagnostics.
#'@param data_column_arg Argument name used for diagnostics.
#'@param caller Default `NULL`. Calling function name used in error messages.
#'
#'@keywords internal
coerce_render_data_column_numeric = function(
  data_values,
  data_column_name,
  data_column_arg,
  caller = NULL
) {
  if (inherits(data_values, "units")) {
    data_values = units::drop_units(data_values)
  }
  if (is.factor(data_values)) {
    data_values = as.character(data_values)
  }
  data_values = tryCatch(
    suppressWarnings(as.numeric(data_values)),
    error = function(e) rep(NA_real_, length(data_values))
  )
  data_values
}

#'@title Coerce polygon data column to numeric
#'
#'@param polygon This is an sf object.
#'@param data_column_name Column name to coerce.
#'@param data_column_arg Argument name used for diagnostics.
#'@param caller Default `NULL`. Calling function name used in error messages.
#'
#'@keywords internal
coerce_polygon_data_column = function(
  polygon,
  data_column_name,
  data_column_arg,
  caller = NULL
) {
  if (is.null(data_column_name)) {
    return(polygon)
  }
  if (!data_column_name %in% colnames(polygon)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`",
        data_column_arg,
        "` was not found in `polygon`: ",
        data_column_name
      ),
      call. = FALSE
    )
  }
  polygon[[data_column_name]] = coerce_render_data_column_numeric(
    data_values = polygon[[data_column_name]],
    data_column_name = data_column_name,
    data_column_arg = data_column_arg,
    caller = caller
  )
  polygon
}

#'@title Coerce polygon data columns to numeric
#'
#'@param polygon This is an sf object.
#'@param data_column_top Default `NULL`. Column name to use for the top values.
#'@param data_column_bottom Default `NULL`. Column name to use for the bottom values.
#'@param caller Default `NULL`. Calling function name used in error messages.
#'
#'@keywords internal
coerce_polygon_data_columns = function(
  polygon,
  data_column_top = NULL,
  data_column_bottom = NULL,
  caller = NULL
) {
  keep = rep(TRUE, nrow(polygon))
  polygon = coerce_polygon_data_column(
    polygon = polygon,
    data_column_name = data_column_top,
    data_column_arg = "data_column_top",
    caller = caller
  )
  if (!is.null(data_column_top)) {
    keep = keep & is.finite(polygon[[data_column_top]])
  }
  polygon = coerce_polygon_data_column(
    polygon = polygon,
    data_column_name = data_column_bottom,
    data_column_arg = "data_column_bottom",
    caller = caller
  )
  if (!is.null(data_column_bottom)) {
    keep = keep & is.finite(polygon[[data_column_bottom]])
  }
  list(
    polygon = polygon[keep, , drop = FALSE],
    keep = keep
  )
}

#'@title Get Data Value from spatial object
#'
#'@param polygon This is an sf object.
#'@param data_column_name Default `NULL`. Column name to use for values.
#'@param default_value Default `0`. Value to use when `data_column_name` is `NULL`.
#'@param scale_data Default `1`. Amount to scale values by.
#'@param data_column_arg Default `"data_column"`. Argument name used for diagnostics.
#'@param caller Default `NULL`. Calling function name used in error messages.
#'
#'@keywords internal
get_polygon_data_value = function(
  polygon,
  data_column_name = NULL,
  default_value = 0,
  scale_data = 1,
  data_column_arg = "data_column",
  caller = NULL
) {
  if (!is.null(data_column_name)) {
    if (!data_column_name %in% colnames(polygon)) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`",
          data_column_arg,
          "` was not found in `polygon`: ",
          data_column_name
        ),
        call. = FALSE
      )
    }
    data_vals = coerce_render_data_column_numeric(
      data_values = polygon[[data_column_name]],
      data_column_name = data_column_name,
      data_column_arg = data_column_arg,
      caller = caller
    )
    data_vals = data_vals[is.finite(data_vals)]
  } else {
    polygon$new_data_column = default_value
    data_vals = polygon$new_data_column
  }
  data_vals = data_vals * scale_data
  stopifnot(is.numeric(data_vals))
  return(data_vals)
}
