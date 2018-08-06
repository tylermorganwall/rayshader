#'@title make_shadow
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param rows A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param cols Default `0`.
#'@param basedepth Default `grey20`.
#'@param shadowwidth Default `50`. Shadow width.
#'@import imager
#'@keywords internal
make_shadow = function(rows, cols, basedepth, shadowwidth) {
  basedepth = matrix(basedepth,nrow = rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  imagemat = matrix(1,nrow = rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  imagemat[shadowwidth:(rows-shadowwidth),shadowwidth:(cols-shadowwidth)] = 0
  shadowarray = as.array(imager::isoblur(imager::as.cimg(imagemat),sigma=shadowwidth/2))[,,1,1]
  browser()
  tempmap = tempfile()
  write_png(shadowarray,tempmap)
  rgl.surface((-shadowwidth+1):(rows+shadowwidth),(-shadowwidth):(cols+shadowwidth-1),basedepth,texture=paste0(tempmap,".png"),lit=FALSE)
}