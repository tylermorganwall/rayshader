#' Validate cloud rendering parameters
#'
#' @param start_altitude Starting cloud altitude.
#' @param end_altitude Ending cloud altitude.
#' @param layers Number of cloud layers.
#' @param sun_altitude Default `NULL`. Sun altitude in degrees.
#'
#' @keywords internal
#' @noRd
validate_cloud_parameters = function(
  start_altitude,
  end_altitude,
  layers,
  sun_altitude = NULL
) {
  if (
    !is.numeric(start_altitude) ||
      length(start_altitude) != 1 ||
      !is.finite(start_altitude)
  ) {
    stop("`start_altitude` must be a single finite number.", call. = FALSE)
  }
  if (
    !is.numeric(end_altitude) ||
      length(end_altitude) != 1 ||
      !is.finite(end_altitude)
  ) {
    stop("`end_altitude` must be a single finite number.", call. = FALSE)
  }
  if (start_altitude == end_altitude) {
    stop(
      "`start_altitude` and `end_altitude` must be different.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(layers) ||
      length(layers) < 1 ||
      !is.finite(layers[1]) ||
      layers[1] <= 0
  ) {
    stop(
      "`layers` must begin with a finite number greater than 0.",
      call. = FALSE
    )
  }
  if (
    !is.null(sun_altitude) &&
      (!is.numeric(sun_altitude) ||
        length(sun_altitude) != 1 ||
        !is.finite(sun_altitude) ||
        sun_altitude <= 0 ||
        sun_altitude > 90)
  ) {
    stop(
      "`sun_altitude` must be a single finite number greater than 0 and less than or equal to 90.",
      call. = FALSE
    )
  }
  invisible(NULL)
}
