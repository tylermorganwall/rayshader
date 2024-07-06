#'@title Get Data Value from spatial object
#'
#'@param polygon This is an sf object
#'
#'@keywords internal
get_polygon_data_value = function(polygon, data_column_name = NULL, default_value = 0, scale_data = 1) {
  if(!is.null(data_column_name)) {
    data_vals = polygon[[data_column_name]]
  } else {
    polygon$new_data_column = default_value
    data_vals = polygon$new_data_column
  }
  data_vals = data_vals * scale_data
  stopifnot(is.numeric(data_vals))
  return(data_vals)
}
