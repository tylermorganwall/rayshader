#'@title Raster to Matrix
#'
#'@description Turns a raster into a matrix suitable for rayshader. 
#'
#'@param raster The input raster. Either a RasterLayer object, a terra SpatRaster object, or a filename.
#'@param verbose Default `interactive()`. Will print dimensions of the resulting matrix.
#'@export
#'@examples
#'#Save montereybay as a raster and open using the filename.
#'if(rayshader:::run_documentation()) {
#'temp_raster_filename = paste0(tempfile(),".tif")
#'raster::writeRaster(raster::raster(t(montereybay)),temp_raster_filename)
#'elmat = raster_to_matrix(temp_raster_filename)
#'elmat %>%
#'  sphere_shade() %>%
#'  plot_map()
#'}
raster_to_matrix = function(raster, verbose = interactive()) {
  if(is.character(raster)) {
    raster = raster::raster(raster)
  }
  return_mat = matrix(ncol=0,nrow=0)
  if(inherits(raster,"SpatRaster")) {
    if(length(find.package("terra", quiet = TRUE)) > 0) {
      raster_mat = as.matrix(raster)
    } else {
      stop("{terra} package required if passing SpatRaster object")
    }
    return_mat = matrix(as.numeric(terra::values(raster)), 
                  nrow = ncol(raster), ncol = nrow(raster))
  } else {
    return_mat = matrix(raster::extract(raster, raster::extent(raster)), 
                  nrow = ncol(raster), ncol = nrow(raster))
  }
  if(verbose) {
    print(paste0("Dimensions of matrix are: ", ncol(return_mat),"x" , nrow(return_mat)))
  }
  if(all(dim(return_mat) == 0)) {
    stop("Was not able to convert object to a matrix: double check to ensure object is either of class `character`, `RasterLayer`, or `SpatRaster`.")
  }
  return(return_mat)
}
