#' Generate Raster Overlay
#'
#' @description Converts a spatial raster into a semi-transparent RGBA overlay
#' for the current rayshader scene. Raster values can be drawn with a single
#' color or mapped through a color palette.
#'
#' @param raster A raster layer. This can be a `terra::SpatRaster`, a
#' `raster::RasterLayer`/`RasterBrick`/`RasterStack`, or a raster filename.
#' @param extent Default `NULL`. Either an object representing the spatial extent of the scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will infer the extent from `heightmap` when possible,
#' otherwise reuse cached extent metadata from the active scene or the most recent
#' raster-backed hillshade call.
#' @param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting
#' overlay automatically. If omitted, rayshader will reuse the cached heightmap
#' from the active scene or the most recent hillshade call.
#' @param width Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#' @param height Default `NA`. Height of the resulting overlay. Default the same dimensions as height map.
#' @param resolution_multiply Default `1`. If passing in `heightmap` instead of width/height, amount to
#' increase the resolution of the overlay.
#' Should be combined with \code{\link[=add_overlay]{add_overlay()}} with `rescale_original = TRUE` to ensure those added details are captured
#' in the final map.
#' @param palette Default `"red"`. Single color, vector of colors, or palette function.
#' A single color draws every finite raster cell with that color. Multiple colors
#' or a palette function map finite raster values continuously from low to high.
#' @param alpha Default `0.5`. Alpha multiplier for finite raster cells. Must be between `0` and `1`.
#' Alpha values included in `palette` colors are multiplied by this value.
#' @param range Default `NULL`. Numeric length-2 range used to map raster values
#' through `palette`. Values outside this range are clamped. If `NULL`, the
#' finite value range of the aligned raster is used.
#' @param resample_method Default `"bilinear"`. Method passed to
#' `terra::resample()` or `terra::project()` when aligning `raster` to the
#' scene grid.
#'
#' @return 4-layer RGBA array representing the raster overlay.
#' @export
#' @examples
#' if (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")) {
#'   # Create a semi-transparent overlay from low elevations:
#'   low_elevation = montereybay_spatial
#'   low_elevation[low_elevation > 0] = NA
#'
#'   montereybay_spatial |>
#'     height_shade() |>
#'     add_overlay(generate_raster_overlay(low_elevation, palette = "dodgerblue3")) |>
#'     plot_map()
#' }
generate_raster_overlay = function(
  raster,
  extent = NULL,
  heightmap = NULL,
  width = NA,
  height = NA,
  resolution_multiply = 1,
  palette = "red",
  alpha = 0.5,
  range = NULL,
  resample_method = "bilinear"
) {
  if (!(length(find.package("terra", quiet = TRUE)) > 0)) {
    stop(
      "`terra` package required for generate_raster_overlay().",
      call. = FALSE
    )
  }
  if (missing(raster)) {
    stop("`raster` must be supplied.", call. = FALSE)
  }

  explicit_extent = extent
  resolution_multiply = validate_raster_overlay_resolution_multiply(
    resolution_multiply,
    caller = "generate_raster_overlay"
  )
  alpha = validate_raster_overlay_alpha(
    alpha,
    caller = "generate_raster_overlay"
  )

  heightmap = resolve_overlay_heightmap(
    heightmap = heightmap,
    heightmap_missing = missing(heightmap),
    width = width,
    height = height,
    caller = "generate_raster_overlay"
  )
  heightmap = coerce_raster_overlay_heightmap(heightmap)
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "generate_raster_overlay"
  )
  overlay_dims = resolve_raster_overlay_dimensions(
    heightmap = heightmap,
    width = width,
    height = height,
    caller = "generate_raster_overlay"
  )
  target_crs = attr(heightmap, "crs", exact = TRUE)
  if (is.null(target_crs)) {
    target_crs = infer_spatialize_image_extent_crs(explicit_extent)
  }
  if (is.null(target_crs)) {
    target_crs = tryCatch(
      get_scene_target_crs(
        extent = extent,
        heightmap = heightmap,
        caller = "generate_raster_overlay"
      ),
      error = function(e) NULL
    )
  }
  aligned_raster = align_raster_overlay_to_scene(
    raster = raster,
    extent = extent,
    width = overlay_dims$width * resolution_multiply,
    height = overlay_dims$height * resolution_multiply,
    target_crs = target_crs,
    method = resample_method,
    caller = "generate_raster_overlay"
  )
  value_matrix = raster_to_matrix(aligned_raster, verbose = FALSE)
  raster_overlay_matrix_to_rgba(
    value_matrix = value_matrix,
    palette = palette,
    alpha = alpha,
    range = range,
    caller = "generate_raster_overlay"
  )
}

#' Coerce raster overlay heightmap
#'
#' @param heightmap Default `NULL`. Heightmap input.
#'
#' @return Heightmap matrix or `NULL`.
#' @keywords internal
coerce_raster_overlay_heightmap = function(heightmap = NULL) {
  if (is.null(heightmap) || !is_spatial_heightmap_input(heightmap)) {
    return(heightmap)
  }
  heightmap_info = coerce_plot_3d_heightmap(heightmap)
  heightmap = heightmap_info$heightmap
  if (!is.null(heightmap_info$extent)) {
    attr(heightmap, "extent") = heightmap_info$extent
  }
  if (!is.null(heightmap_info$crs)) {
    attr(heightmap, "crs") = heightmap_info$crs
  }
  if (is.finite(heightmap_info$zscale) && heightmap_info$zscale > 0) {
    attr(heightmap, "zscale") = heightmap_info$zscale
  }
  heightmap
}

#' Resolve raster overlay dimensions
#'
#' @param heightmap Default `NULL`. Heightmap matrix.
#' @param width Default `NA`. Requested overlay width.
#' @param height Default `NA`. Requested overlay height.
#' @param caller Default `NULL`. Calling function.
#'
#' @return List containing `width` and `height`.
#' @keywords internal
resolve_raster_overlay_dimensions = function(
  heightmap = NULL,
  width = NA,
  height = NA,
  caller = NULL
) {
  if (is.na(height)) {
    if (is.null(heightmap)) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`height` must be supplied when `heightmap` is not available."
        ),
        call. = FALSE
      )
    }
    height = ncol(heightmap)
  }
  if (is.na(width)) {
    if (is.null(heightmap)) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`width` must be supplied when `heightmap` is not available."
        ),
        call. = FALSE
      )
    }
    width = nrow(heightmap)
  }
  if (
    !is.numeric(width) ||
      !is.numeric(height) ||
      length(width) != 1 ||
      length(height) != 1 ||
      is.na(width) ||
      is.na(height) ||
      width <= 0 ||
      height <= 0
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`width` and `height` must be single positive numbers."
      ),
      call. = FALSE
    )
  }
  list(
    width = as.integer(round(width)),
    height = as.integer(round(height))
  )
}

#' Validate raster overlay alpha
#'
#' @param alpha Default `0.5`. Alpha multiplier.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Numeric alpha.
#' @keywords internal
validate_raster_overlay_alpha = function(alpha = 0.5, caller = NULL) {
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1 ||
      is.na(alpha) ||
      alpha < 0 ||
      alpha > 1
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`alpha` must be a single number between 0 and 1."
      ),
      call. = FALSE
    )
  }
  alpha
}

#' Validate raster overlay resolution multiplier
#'
#' @param resolution_multiply Default `1`. Resolution multiplier.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Positive integer-ish multiplier.
#' @keywords internal
validate_raster_overlay_resolution_multiply = function(
  resolution_multiply = 1,
  caller = NULL
) {
  if (
    !is.numeric(resolution_multiply) ||
      length(resolution_multiply) != 1 ||
      is.na(resolution_multiply) ||
      resolution_multiply <= 0
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`resolution_multiply` must be a single positive number."
      ),
      call. = FALSE
    )
  }
  resolution_multiply
}

#' Align raster overlay to scene
#'
#' @param raster Raster layer.
#' @param extent Scene extent.
#' @param width Target raster columns.
#' @param height Target raster rows.
#' @param target_crs Default `NULL`. Scene CRS.
#' @param method Default `"bilinear"`. Resampling method.
#' @param caller Default `NULL`. Calling function.
#'
#' @return A `terra::SpatRaster`.
#' @keywords internal
align_raster_overlay_to_scene = function(
  raster,
  extent,
  width,
  height,
  target_crs = NULL,
  method = "bilinear",
  caller = NULL
) {
  overlay_raster = coerce_raster_overlay_raster(raster, caller = caller)
  extent = tryCatch(
    get_extent(extent),
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not interpret `extent`: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  target_template = terra::rast(
    nrows = as.integer(round(height)),
    ncols = as.integer(round(width)),
    xmin = extent["xmin"],
    xmax = extent["xmax"],
    ymin = extent["ymin"],
    ymax = extent["ymax"]
  )
  target_crs = raster_overlay_terra_crs(target_crs)
  if (!is.null(target_crs)) {
    terra::crs(target_template) = target_crs
  }
  source_crs = tryCatch(terra::crs(overlay_raster), error = function(e) "")
  target_crs_value = tryCatch(
    terra::crs(target_template),
    error = function(e) ""
  )
  source_has_crs = is.character(source_crs) &&
    length(source_crs) &&
    nzchar(source_crs[1])
  target_has_crs = is.character(target_crs_value) &&
    length(target_crs_value) &&
    nzchar(target_crs_value[1])

  if (target_has_crs && !source_has_crs) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `raster` inputs must have a CRS when the active heightmap has a CRS."
      ),
      call. = FALSE
    )
  }

  tryCatch(
    {
      if (target_has_crs) {
        same_crs = isTRUE(tryCatch(
          scene_crs_equal(source_crs, target_crs_value),
          error = function(e) FALSE
        ))
        if (same_crs) {
          terra::resample(overlay_raster, target_template, method = method)
        } else {
          terra::project(overlay_raster, target_template, method = method)
        }
      } else {
        terra::resample(overlay_raster, target_template, method = method)
      }
    },
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not project/resample spatial `raster` to the active heightmap grid: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

#' Coerce raster overlay input
#'
#' @param raster Raster layer.
#' @param caller Default `NULL`. Calling function.
#'
#' @return A one-layer `terra::SpatRaster`.
#' @keywords internal
coerce_raster_overlay_raster = function(raster, caller = NULL) {
  if (is.character(raster)) {
    raster = terra::rast(raster)
  } else if (inherits(raster, c("RasterLayer", "RasterBrick", "RasterStack"))) {
    raster = terra::rast(raster)
  }
  if (!inherits(raster, "SpatRaster")) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`raster` must resolve to a spatial raster."
      ),
      call. = FALSE
    )
  }
  if (terra::nlyr(raster) > 1) {
    warning("`raster` has multiple layers; using the first layer.")
    raster = raster[[1]]
  }
  raster
}

#' Convert CRS for raster overlay
#'
#' @param crs Default `NULL`. CRS input.
#'
#' @return CRS string or `NULL`.
#' @keywords internal
raster_overlay_terra_crs = function(crs = NULL) {
  parsed_crs = try_parse_scene_crs(crs)
  if (!is.null(parsed_crs) && !is.na(parsed_crs)) {
    return(parsed_crs$wkt)
  }
  if (is.character(crs) && length(crs) && nzchar(trimws(crs[1]))) {
    return(crs[1])
  }
  NULL
}

#' Convert raster matrix to RGBA overlay
#'
#' @param value_matrix Matrix of aligned raster values.
#' @param palette Default `"red"`. Color palette.
#' @param alpha Default `0.5`. Alpha multiplier.
#' @param range Default `NULL`. Numeric value range.
#' @param caller Default `NULL`. Calling function.
#'
#' @return 4-layer RGBA array.
#' @keywords internal
raster_overlay_matrix_to_rgba = function(
  value_matrix,
  palette = "red",
  alpha = 0.5,
  range = NULL,
  caller = NULL
) {
  if (is.function(palette)) {
    palette = palette(256)
  }
  if (
    !is.character(palette) ||
      length(palette) < 1 ||
      any(is.na(palette))
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`palette` must be a color, a character vector of colors, or a palette function."
      ),
      call. = FALSE
    )
  }
  value_image = t(value_matrix)
  valid_values = is.finite(value_image)
  overlay = array(0, dim = c(nrow(value_image), ncol(value_image), 4))
  if (!any(valid_values)) {
    return(rayimage::ray_read_image(
      overlay,
      assume_colorspace = rayimage::CS_SRGB,
      assume_white = "D65"
    ))
  }

  color_values = grDevices::col2rgb(palette, alpha = TRUE) / 255
  color_indices = raster_overlay_color_indices(
    values = value_image,
    valid_values = valid_values,
    ncolors = ncol(color_values),
    range = range,
    caller = caller
  )
  for (channel in seq_len(4)) {
    channel_values = overlay[,, channel]
    channel_values[valid_values] = color_values[channel, color_indices]
    overlay[,, channel] = channel_values
  }
  overlay[,, 4] = overlay[,, 4] * alpha
  rayimage::ray_read_image(
    overlay,
    assume_colorspace = rayimage::CS_SRGB,
    assume_white = "D65"
  )
}

#' Calculate raster overlay color indices
#'
#' @param values Matrix of raster values in image orientation.
#' @param valid_values Logical matrix indicating finite values.
#' @param ncolors Number of colors in the palette.
#' @param range Default `NULL`. Numeric value range.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Integer color indices.
#' @keywords internal
raster_overlay_color_indices = function(
  values,
  valid_values,
  ncolors,
  range = NULL,
  caller = NULL
) {
  if (ncolors == 1) {
    return(rep(1L, sum(valid_values)))
  }
  if (is.null(range)) {
    value_range = base::range(values[valid_values], na.rm = TRUE)
  } else {
    if (
      !is.numeric(range) ||
        length(range) != 2 ||
        any(!is.finite(range))
    ) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`range` must be `NULL` or a finite numeric vector of length 2."
        ),
        call. = FALSE
      )
    }
    value_range = base::range(range)
  }
  if (diff(value_range) == 0) {
    return(rep(as.integer(ceiling(ncolors / 2)), sum(valid_values)))
  }
  scaled_values = (values[valid_values] - value_range[1]) / diff(value_range)
  scaled_values = pmax(pmin(scaled_values, 1), 0)
  as.integer(floor(scaled_values * (ncolors - 1))) + 1L
}
