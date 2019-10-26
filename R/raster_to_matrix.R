#'@title Raster to Matrix
#'
#'@description Turns a raster into a matrix suitable for rayshader. 
#'
#'@param raster The input raster. Either a RasterLayer object, or a filename.
#'@param verbose Default `interactive()`. Will print dimensions of the resulting matrix.
#'@export
#'@examples
#'#Save montereybay as a raster and open using the filename.
#'
#'\donttest{
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
  if(verbose) {
    if(is.na(raster::projection(raster))) {
      raster_mat = raster::as.matrix(raster)
      print(paste0("Dimensions of matrix are: ", ncol(raster_mat),"x" , nrow(raster_mat)))
    } else {
      print(paste0("Dimensions of matrix are: ", ncol(raster),"x" , nrow(raster), "."))
    }
  }
  return(matrix(raster::extract(raster, raster::extent(raster)), 
         nrow = ncol(raster), ncol = nrow(raster)))
}