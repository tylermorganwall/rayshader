#'@title Reduce Matrix Size
#'
#'@description Reduces the resolution of the matrix by specifyin the desired output dimensions, or a scaling factor.
#'
#'@param heightmap The elevation matrix.
#'@param scale Default `0.5`. The amount to scale down the matrix. Scales down using bilinear interpolation.
#'@param width Default `NULL`.  Alternative to `scale` argument. The desired output width. If `width` is less than 1, it will be interpreted as a scaling factor--
#'e.g. 0.5 would halve the resolution for the width. 
#'@param height Default `NULL`. Alternative to `scale` argument. The desired output width. If `height` is less than 1, it will be interpreted as a scaling factor--
#'e.g. 0.5 would halve the resolution for the height.
#'@export
#'@examples
#'#Reduce the size of the monterey bay dataset by half
#'
#'montbaysmall = reduce_matrix_size(montereybay, 0.5)
#'montbaysmall %>%
#'  sphere_shade() %>%
#'  plot_map()
#'
#'#Reduce the size of the monterey bay dataset from 401x401 to 100x100
#'
#'montbaysmall = reduce_matrix_size(montereybay, width = 100, height = 100)
#'montbaysmall %>%
#'  sphere_shade() %>%
#'  plot_map()
reduce_matrix_size = function(heightmap, scale=0.5, width=NULL, height=NULL) {
  if(is.null(width) && is.null(height)) {
    width = scale
    height = scale
  } else {
    if(any(is.null(c(width,height)))) {
      stop("If specifying explicit width and height, both must be passed in as arguments.")
    }
  }
  currentdim = dim(heightmap)
  if(width > currentdim[1] || height > currentdim[2]) {
    stop("Both width (input value: ", width,") and height (input value: ", height,
         ") must be less than the heightmap dimensions: ", currentdim[1], "x", currentdim[2])
  }
  if(width <= 1 && height <= 1) {
    width = width * currentdim[2]
    height = height * currentdim[1]
  }
  heightmapr = raster::raster(t(heightmap))
  rasternew = raster::raster(t(heightmap[1:height, 1:width]))
  rasternew = raster::resample(heightmapr, rasternew, method = "bilinear")
  return(matrix(raster::extract(rasternew, raster::extent(rasternew)), 
         nrow = ncol(rasternew), ncol = nrow(rasternew)))
}