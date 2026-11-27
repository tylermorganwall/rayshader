#' Get Extent
#'
#' @param extent Matrix
#'
#' @return Bounding box
#' @keywords internal
get_extent = function(extent) {
  if (
    inherits(
      extent,
      c(
        "sf",
        "SpatialPolygonsDataFrame",
        "SpatialPoints",
        "SpatialPointsDataFrame",
        "SpatialMultiPoints",
        "SpatialMultiPointsDataFrame",
        "SpatialPixels",
        "SpatialPixelsDataFrame",
        "SpatialGrid",
        "SpatialGridDataFrame",
        "SpatialLines",
        "SpatialLinesDataFrame",
        "SpatialPolygons",
        "SpatialPolygonsDataFrame"
      )
    )
  ) {
    extent = sf::st_bbox(extent)
  }
  if (inherits(extent, "SpatRaster")) {
    extent = terra::ext(extent)
  }
  if (inherits(extent, c("RasterLayer", "RasterBrick", "RasterStack"))) {
    extent = raster::extent(extent)
  }

  if (!inherits(extent, c("Extent", "bbox", "numeric", "SpatExtent"))) {
    stop(sprintf(
      "class of extent (`%s`) not one of supported types (`Extent`, `bbox`, `numeric`, `SpatExtent`)",
      class(extent)[1]
    ))
  }
  if (is.numeric(extent)) {
    if (length(extent) != 4) {
      stop(
        sprintf(
          "`extent` must contain exactly four values; received %d.",
          length(extent)
        ),
        call. = FALSE
      )
    }
  }
  if (inherits(extent, "Extent")) {
    extent = unname(as.vector(extent))
    xmin = extent[1]
    xmax = extent[2]
    ymin = extent[3]
    ymax = extent[4]
    return(c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax))
  }
  if (inherits(extent, "SpatExtent")) {
    extent = unname(as.vector(extent))
    xmin = extent[1]
    xmax = extent[2]
    ymin = extent[3]
    ymax = extent[4]
    return(c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax))
  }
  if (inherits(extent, "bbox")) {
    extent = unname(as.vector(extent))
    xmin = extent[1]
    xmax = extent[3]
    ymin = extent[2]
    ymax = extent[4]
    return(c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax))
  }
  if (inherits(extent, "numeric")) {
    extent = unname(as.vector(extent))
    xmin = extent[1]
    xmax = extent[2]
    ymin = extent[3]
    ymax = extent[4]
    if (
      length(extent) != 4 ||
        any(!is.finite(extent)) ||
        xmax <= xmin ||
        ymax <= ymin
    ) {
      stop(
        paste0(
          "`extent` must contain four finite values ordered as ",
          "`xmin`, `xmax`, `ymin`, and `ymax`, with each maximum greater ",
          "than its minimum."
        ),
        call. = FALSE
      )
    }
    return(c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax))
  }
}
