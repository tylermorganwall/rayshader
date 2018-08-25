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
make_shadow = function(rows, cols, basedepth, shadowwidth, color, shadowcolor) {
  colors = col2rgb(color)
  shadowcolors = col2rgb(shadowcolor)
  basedepth = matrix(basedepth,nrow = rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  rmat = matrix(colors[1]/255,nrow=rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  gmat = matrix(colors[2]/255,nrow=rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  bmat = matrix(colors[3]/255,nrow=rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  rmat[shadowwidth:(rows+shadowwidth),shadowwidth:(cols+shadowwidth)] = shadowcolors[1]/255
  gmat[shadowwidth:(rows+shadowwidth),shadowwidth:(cols+shadowwidth)] = shadowcolors[2]/255
  bmat[shadowwidth:(rows+shadowwidth),shadowwidth:(cols+shadowwidth)] = shadowcolors[3]/255
  shadowarray = array(1,dim = c(cols+shadowwidth*2,rows+shadowwidth*2,3))
  shadowarray[,,1] = t(rmat)
  shadowarray[,,2] = t(gmat)
  shadowarray[,,3] = t(bmat)
  shadowarray = suppressWarnings(as.array(imager::isoblur(imager::as.cimg(shadowarray),sigma=shadowwidth/2))[,,1,])
  tempmap = tempfile()
  save_png(shadowarray,tempmap)
  rgl.surface((-shadowwidth+1):(rows+shadowwidth),
              -(-shadowwidth+1):-(cols+shadowwidth),
              basedepth,texture=paste0(tempmap,".png"),
              lit=FALSE,back="culled")
}