#'@title Resize Matrix
#'
#'@description Resizes a matrix (preserving contents) by specifying the desired output dimensions or a scaling factor.
#'
#'@param heightmap The elevation matrix.
#'@param scale Default `0.5`. The amount to scale down the matrix. Scales using bilinear interpolation.
#'@param width Default `NULL`.  Alternative to `scale` argument. The desired output width. If `width` is less than 1, it will be interpreted as a scaling factor--
#'e.g. 0.5 would halve the resolution for the width. 
#'@param height Default `NULL`. Alternative to `scale` argument. The desired output width. If `height` is less than 1, it will be interpreted as a scaling factor--
#'e.g. 0.5 would halve the resolution for the height.
#'@param method Default `bilinear`. Method of interpolation. Alteratively `cubic`, which is slightly smoother, although
#'current implementation slightly scales the image.
#'@export
#'@examples
#'#Reduce the size of the monterey bay dataset by half
#'
#'\donttest{
#'montbaysmall = resize_matrix(montereybay, scale=0.5)
#'montbaysmall %>%
#'  sphere_shade() %>%
#'  plot_map()
#'
#'#Reduce the size of the monterey bay dataset from 540x540 to 100x100
#'montbaysmall = resize_matrix(montereybay, width = 100, height = 100)
#'montbaysmall %>%
#'  sphere_shade() %>%
#'  plot_map()
#'  
#'#Increase the size of the volcano dataset 3x
#'volcanobig = resize_matrix(volcano, scale=3)
#'volcanobig %>% 
#'  sphere_shade() %>%
#'  plot_map()
#'  
#'#Increase the size of the volcano dataset 2x, using cubic interpolation
#'volcanobig = resize_matrix(volcano, scale=3, method="cubic")
#'volcanobig %>% 
#'  sphere_shade() %>%
#'  plot_map()
#'}
resize_matrix = function(heightmap, scale=1, width=NULL, height=NULL, method = "bilinear") {
  currentdim = dim(heightmap)
  if(is.null(width) && is.null(height)) {
    width = scale * currentdim[2]
    height = scale * currentdim[1]
  } else {
    if(any(is.null(c(width,height)))) {
      stop("If specifying explicit width and height, both must be passed in as arguments.")
    }
  }
  if(width <= 1 && height <= 1) {
    width = as.integer(width * currentdim[2])
    height = as.integer(height * currentdim[1])
  }
  if(method == "bilinear") {
    rasternew = raster::raster(matrix(0,width,height))
    heightmapr = raster::raster(t(heightmap))
    rasternew = raster::resample(heightmapr, rasternew, method = "bilinear")
    return(matrix(raster::extract(rasternew, raster::extent(rasternew)), 
           nrow = ncol(rasternew), ncol = nrow(rasternew)))
  } else if (method == "cubic") {
    resized_matrix = matrix(0,width,height)
    heightvals = seq(0,1,length.out = height)
    widthvals = seq(0,1,length.out = width) 
    scaled_height = round(heightvals*(nrow(heightmap)-1),10)
    scaled_width = round(widthvals*(ncol(heightmap)-1),10)
    
    #Indices into original matrix
    index_height = floor(scaled_height) + 1
    index_width = floor(scaled_width) + 1

    #Indices into new matrix
    index_height_new = round(heightvals*height,10) + 1
    index_width_new = round(widthvals*width,10) + 1

    #Fraction amount between matrices
    fraction_height = scaled_height - index_height + 1
    fraction_width = scaled_width - index_width + 1 
    hmr = add_padding(add_padding(heightmap))
    for(i in seq_len(length(index_height)-1)) {
      for(j in seq_len(length(index_width)-1)) {
        ih = index_height[i]+2
        iw = index_width[j]+2
        ihn = index_height_new[i]
        iwn = index_width_new[j]
        frh = fraction_height[i]
        frw = fraction_width[j]
        resized_matrix[iwn, ihn] = bicubic_interpolate(hmr[(ih-1):(ih+2),(iw-1):(iw+2)],frh, frw)
      }
    }
    for(i in seq_len(length(index_height)-1)) {
      ih = index_height[i] + 2
      frh = fraction_height[i]
      values = hmr[(ih-1):(ih+2),ncol(heightmap)+2]
      resized_matrix[nrow(resized_matrix),i] = cubic_interpolate(values[1],values[2],values[3],values[4],frh)
    }
    for(i in seq_len(length(index_width)-1)) {
      iw = index_width[i] + 2
      frw = fraction_width[i]
      values = hmr[nrow(heightmap)+2, (iw-1):(iw+2)]
      resized_matrix[i,ncol(resized_matrix)] = cubic_interpolate(values[1],values[2],values[3],values[4],frw)
    }
    resized_matrix[nrow(resized_matrix), ncol(resized_matrix)] = heightmap[nrow(heightmap),ncol(heightmap)]
    return(t(resized_matrix))
  }
}

#' Reduce Matrix Size (deprecated)
#'
#' @param ... Arguments to pass to resize_matrix() function.
#'
#' @return Reduced matrix.
#' @export
#'
#' @examples
#' #Deprecated lambertian material. Will display a warning.
#'\donttest{
#'montbaysmall = reduce_matrix_size(montereybay, scale=0.5)
#'montbaysmall %>%
#'  sphere_shade() %>%
#'  plot_map()
#'}
reduce_matrix_size = function(...) {
  warning("reduce_matrix_size() deprecated--use resize_matrix() instead.")
  resize_matrix(...)
}
