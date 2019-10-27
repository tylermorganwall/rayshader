#'@title make_shadow
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param basedepth Default `grey20`.
#'@param shadowwidth Default `50`. Shadow width.
#'@import imager
#'@keywords internal
make_shadow = function(heightmap, basedepth, shadowwidth, color, shadowcolor) {
  rows = nrow(heightmap)
  cols = ncol(heightmap)
  colors = col2rgb(color)
  shadowcolors = col2rgb(shadowcolor)
  fliplr = function(x) {
    x[,ncol(x):1]
  }
  basedepthmat = matrix(basedepth,nrow = rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  na_depth = matrix(FALSE,nrow = rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  na_depth[(shadowwidth+1):(rows+shadowwidth),(shadowwidth+1):(cols+shadowwidth)] = is.na(heightmap)
  rmat = matrix(colors[1]/255,nrow=rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  gmat = matrix(colors[2]/255,nrow=rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  bmat = matrix(colors[3]/255,nrow=rows+shadowwidth*2, ncol = cols+shadowwidth*2)
  rmat[(shadowwidth+1):(rows+shadowwidth),(shadowwidth+1):(cols+shadowwidth)] = shadowcolors[1]/255
  gmat[(shadowwidth+1):(rows+shadowwidth),(shadowwidth+1):(cols+shadowwidth)] = shadowcolors[2]/255
  bmat[(shadowwidth+1):(rows+shadowwidth),(shadowwidth+1):(cols+shadowwidth)] = shadowcolors[3]/255
  rmat[fliplr(na_depth)] = colors[1]/255
  gmat[fliplr(na_depth)] = colors[2]/255
  bmat[fliplr(na_depth)] = colors[3]/255
  shadowarray = array(1,dim = c(cols+shadowwidth*2,rows+shadowwidth*2,3))
  shadowarray[,,1] = t(rmat)
  shadowarray[,,2] = t(gmat)
  shadowarray[,,3] = t(bmat)
  shadowarray = suppressWarnings(as.array(imager::isoblur(imager::as.cimg(shadowarray),sigma=shadowwidth/2))[,,1,])
  tempmap = tempfile()
  save_png(shadowarray,tempmap)
  rgl.surface((-shadowwidth+1):(rows+shadowwidth) - rows/2,
              -(-shadowwidth+1):-(cols+shadowwidth) + cols/2+1,
              basedepthmat,texture=paste0(tempmap,".png"),
              lit=FALSE,back="culled",ambient = "#000006")
}