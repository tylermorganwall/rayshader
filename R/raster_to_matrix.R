#'@title Raster to Matrix
#'
#'@description Turns a raster into a matrix suitable for rayshader. Also attaches the
#'
#'@param raster The input raster. Either a RasterLayer object, or a filename.
#'@param height_units Default `m` for meters. If `feet`, printed zscale (with
#'`verbose = TRUE` will be properly scaled.
#'@param verbose Default `interactive()`. Will print dimensions and (if available) zscale of the resulting matrix.
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
raster_to_matrix = function(raster, height_units="m", verbose = interactive()) {
  if(is.character(raster)) {
    raster = raster::raster(raster)
  }
  if(verbose) {
    #Thanks to Neil Charles (@neilcharles_uk) for providing part of this snippet
    if(is.na(raster::projection(raster))) {
      raster_mat = raster::as.matrix(raster)
      print(paste0("Dimensions of matrix are: ", ncol(raster_mat),"x" , nrow(raster_mat)))
    } else {
      raster_wgs84 = raster::projectRaster(raster, crs = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
      scaling = raster::pointDistance(c(raster::extent(raster_wgs84)@xmin, raster::extent(raster_wgs84)@ymin), 
                                      c(raster::extent(raster_wgs84)@xmax, raster::extent(raster_wgs84)@ymin), 
                                      lonlat = TRUE)/ncol(raster_wgs84)
      if (height_units == "feet") {
        scaling = scaling * 3.28
      }
      print(paste0("Dimensions of matrix are: ", ncol(raster),"x" , nrow(raster) , ", zscale factor is: ", round(scaling,3),"."))
    }
  }
  return(matrix(raster::extract(raster, raster::extent(raster)), 
         nrow = ncol(raster), ncol = nrow(raster)))
}