#' Resize a SpatRaster while preserving spatial metadata
#'
#' `resize_spatial()` mirrors the ergonomics of `rayshader::resize_matrix()`,
#' but returns a `terra::SpatRaster` with extent and CRS intact.
#'
#' Downsampling and upsampling are handled separately because the best visual
#' method is often different in each direction. Integer-factor reductions can
#' use `terra::aggregate()`; everything else uses `terra::resample()` onto a
#' template raster with the requested geometry.
#'
#' @param x A `terra::SpatRaster`.
#' @param scale Default `1`. Relative output size used only when both `width`
#' and `height` are `NULL`.
#' @param width Default `NULL`. Target number of columns. When supplied alone,
#' `height` is derived while preserving the current aspect ratio.
#' @param height Default `NULL`. Target number of rows. When supplied alone,
#' `width` is derived while preserving the current aspect ratio.
#' @param method_down Default `"mean"`. Method used when reducing resolution.
#' Integer-factor reductions use `terra::aggregate()` when possible; other
#' reductions use `terra::resample()`. The default becomes `"modal"` when any
#' input layer is categorical.
#' @param method_up Default `"bilinear"`. Method passed to `terra::resample()`
#' when increasing resolution. The default becomes `"near"` when any input
#' layer is categorical.
#' @param aggregate_if_possible Default `TRUE`. If `TRUE`, use
#' `terra::aggregate()` for integer-factor downsampling when `method_down` is an
#' aggregation method.
#' @param tol Default `1e-8`. Tolerance for deciding whether an implied
#' aggregation factor is effectively an integer.
#' @param threads Default `FALSE`. Passed to `terra::resample()`.
#' @param filename Default `""`. Optional output filename.
#' @param overwrite Default `FALSE`. Whether to overwrite `filename`.
#' @param write_args Default `list()`. Named arguments passed only to
#' `terra::writeRaster()` when `filename` is supplied.
#' @param ... Arguments passed only to the resize operation that is used:
#' `terra::aggregate()` for aggregation or `terra::resample()` for resampling.
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
  write_args = list(),
  ...
) {
  method_down_missing = missing(method_down)
  method_up_missing = missing(method_up)

  if (!inherits(x, "SpatRaster")) {
    stop("`x` must be a terra::SpatRaster.", call. = FALSE)
  }

  if (!is.list(write_args)) {
    stop("`write_args` must be a list.", call. = FALSE)
  }
  if (
    length(write_args) > 0 &&
      (is.null(names(write_args)) || any(!nzchar(names(write_args))))
  ) {
    stop("`write_args` must be an empty or fully named list.", call. = FALSE)
  }
  reserved_write_args = intersect(
    names(write_args),
    c("x", "filename", "overwrite")
  )
  if (length(reserved_write_args)) {
    stop(
      paste0(
        "`write_args` cannot replace `x`, `filename`, or `overwrite`; found: ",
        paste(reserved_write_args, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  current_ncol = terra::ncol(x)
  current_nrow = terra::nrow(x)
  width_supplied = !is.null(width)
  height_supplied = !is.null(height)

  if (width_supplied) {
    if (
      !is.numeric(width) ||
        length(width) != 1 ||
        !is.finite(width) ||
        width <= 0 ||
        width != round(width)
    ) {
      stop("`width` must be a positive whole number of columns.", call. = FALSE)
    }
  }
  if (height_supplied) {
    if (
      !is.numeric(height) ||
        length(height) != 1 ||
        !is.finite(height) ||
        height <= 0 ||
        height != round(height)
    ) {
      stop("`height` must be a positive whole number of rows.", call. = FALSE)
    }
  }

  if (!width_supplied && !height_supplied) {
    if (
      !is.numeric(scale) ||
        length(scale) != 1 ||
        !is.finite(scale) ||
        scale <= 0
    ) {
      stop("`scale` must be a single positive finite number.", call. = FALSE)
    }
    width = round(scale * current_ncol)
    height = round(scale * current_nrow)
  } else if (width_supplied && !height_supplied) {
    height = round(width * current_nrow / current_ncol)
  } else if (!width_supplied && height_supplied) {
    width = round(height * current_ncol / current_nrow)
  }

  width = max(1, width)
  height = max(1, height)
  if (
    !is.finite(width) ||
      !is.finite(height) ||
      width > .Machine$integer.max ||
      height > .Machine$integer.max
  ) {
    stop("The target raster dimensions are too large.", call. = FALSE)
  }
  width = as.integer(width)
  height = as.integer(height)

  column_direction = sign(width - current_ncol)
  row_direction = sign(height - current_nrow)
  if (column_direction * row_direction < 0) {
    stop(
      paste0(
        "Mixed-direction resizing is not supported: one axis would be ",
        "downsampled while the other is upsampled. Resize each direction ",
        "separately."
      ),
      call. = FALSE
    )
  }

  is_downsample = column_direction < 0 || row_direction < 0
  is_upsample = column_direction > 0 || row_direction > 0
  has_categorical_layer = any(terra::is.factor(x))
  if (has_categorical_layer && is_downsample && method_down_missing) {
    method_down = "modal"
  }
  if (has_categorical_layer && is_upsample && method_up_missing) {
    method_up = "near"
  }

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
      invisible(do.call(
        terra::writeRaster,
        c(
          list(x = x, filename = filename, overwrite = overwrite),
          write_args
        )
      ))
    }
    return(x)
  }

  dots = list(...)
  factor_x = current_ncol / width
  factor_y = current_nrow / height
  result = NULL
  normalize_nan = TRUE

  if (is_downsample) {
    can_aggregate = isTRUE(aggregate_if_possible) &&
      factor_x >= 1 &&
      factor_y >= 1 &&
      abs(factor_x - round(factor_x)) <= tol &&
      abs(factor_y - round(factor_y)) <= tol &&
      method_down %in% valid_aggregate_methods

    if (can_aggregate) {
      result = do.call(
        terra::aggregate,
        c(
          list(
            x = x,
            fact = c(
              as.integer(round(factor_y)),
              as.integer(round(factor_x))
            ),
            fun = method_down
          ),
          dots
        )
      )
      normalize_nan = !identical(method_down, "all")
    } else {
      method = method_down
    }
  } else {
    method = method_up
  }

  if (is.null(result)) {
    template = terra::rast(
      nrows = height,
      ncols = width,
      xmin = terra::xmin(x),
      xmax = terra::xmax(x),
      ymin = terra::ymin(x),
      ymax = terra::ymax(x),
      crs = terra::crs(x)
    )
    result = do.call(
      terra::resample,
      c(
        list(x = x, y = template, method = method, threads = threads),
        dots
      )
    )
  }

  if (normalize_nan) {
    result = terra::ifel(is.nan(result), NA, result)
  }
  names(result) = names(x)

  if (nzchar(filename)) {
    invisible(do.call(
      terra::writeRaster,
      c(
        list(x = result, filename = filename, overwrite = overwrite),
        write_args
      )
    ))
  }
  result
}
