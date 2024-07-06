#' Get Extent
#'
#' @param extent Matrix
#'
#' @return Bounding box
#' @keywords internal
get_extent = function(extent) {
  if(inherits(extent, c("sf","SpatialPolygonsDataFrame","SpatialPoints", "SpatialPointsDataFrame",
                        "SpatialMultiPoints", "SpatialMultiPointsDataFrame", "SpatialPixels",
                        "SpatialPixelsDataFrame", "SpatialGrid", "SpatialGridDataFrame", 
                        "SpatialLines", "SpatialLinesDataFrame", "SpatialPolygons", "SpatialPolygonsDataFrame"))) {
    extent = sf::st_bbox(extent)
  }
  if(inherits(extent, "SpatRaster")) {
    extent = terra::ext(extent)
  }
  if(inherits(extent, "RasterLayer")) {
    extent = raster::extent(extent)
  }
  
  if(!inherits(extent, c("Extent", "bbox", "numeric", "SpatExtent"))){
    stop(sprintf("class of extent (`%s`) not one of supported types (`Extent`, `bbox`, `numeric`, `SpatExtent`)", class(extent)[1]))
  }
  if(is.numeric(extent)) {
    if(length(extent) != 4) {
      stop("If extent is numeric vector, must be a vector of length 4 (length of vector given: %d)", length(extent))
    }
  }
  if(inherits(extent, "Extent")) {
    extent = unname(as.vector(extent))
    xmin = extent[1]
    xmax = extent[2]
    ymin = extent[3]
    ymax = extent[4]
    return(c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax))
  }
  if(inherits(extent, "SpatExtent")) {
    extent = unname(as.vector(extent))
    xmin = extent[1]
    xmax = extent[2]
    ymin = extent[3]
    ymax = extent[4]
    return(c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax))
  }
  if(inherits(extent, "bbox")) {
    extent = unname(as.vector(extent))
    xmin = extent[1]
    xmax = extent[3]
    ymin = extent[2]
    ymax = extent[4]
    return(c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax))
  }
  if(inherits(extent, "numeric")) {
    extent = unname(as.vector(extent))
    xmin = extent[1]
    xmax = extent[2]
    ymin = extent[3]
    ymax = extent[4]
    stopifnot(xmax > xmin && ymax > ymin)
    return(c("xmin" = xmin, "xmax" = xmax, "ymin" = ymin, "ymax" = ymax))
  }
}