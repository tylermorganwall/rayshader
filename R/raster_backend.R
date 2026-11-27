has_spatial_raster_package = function(package) {
  requireNamespace(package, quietly = TRUE)
}

warn_raster_support_deprecated = function() {
  warning(
    "Support for the `raster` package will soon be deprecated; switch to `terra`.",
    call. = FALSE
  )
}

read_spatial_raster = function(x, layer = NULL, caller = NULL) {
  if (has_spatial_raster_package("terra")) {
    result = terra::rast(x)
    if (!is.null(layer)) {
      result = result[[layer]]
    }
    return(result)
  }
  if (has_spatial_raster_package("raster")) {
    warn_raster_support_deprecated()
    if (is.null(layer)) {
      return(raster::raster(x))
    }
    return(raster::raster(x, layer = layer))
  }
  stop(
    paste0(
      format_render_caller_prefix(caller),
      "The `terra` package is required to read spatial rasters; ",
      "the legacy `raster` package can be used as a temporary fallback."
    ),
    call. = FALSE
  )
}
