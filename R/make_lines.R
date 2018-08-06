#'@title make_lines
#'
#'@description Makes the lines in the corner of the base.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param basedepth Default `0`.
#'@param linecolor Default `grey40`. 
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param alpha Default `1`. Transparency.
#'@param linewidth Default `2`. Linewidth
#'@keywords internal
make_lines = function(heightmap,basedepth=0,linecolor="grey40",zscale=1,alpha=1,linewidth = 2) {
  heightmap = heightmap[,ncol(heightmap):1]/zscale
  heightval1 = heightmap[2,2]
  heightval2 = heightmap[nrow(heightmap)-1,2]
  heightval3 = heightmap[2,ncol(heightmap)-1]
  heightval4 = heightmap[nrow(heightmap)-1,ncol(heightmap)-1]
  heightlist = list()
  heightlist[[1]] = matrix(c(1,1,basedepth,heightval1,1,1),2,3)
  heightlist[[2]] = matrix(c(nrow(heightmap),nrow(heightmap),basedepth,heightval2,1,1),2,3)
  heightlist[[3]] = matrix(c(1,1,basedepth,heightval3,ncol(heightmap),ncol(heightmap)),2,3)
  heightlist[[4]] = matrix(c(nrow(heightmap),nrow(heightmap),basedepth,heightval4,ncol(heightmap),ncol(heightmap)),2,3)
  for(i in 1:4) {
    rgl::lines3d(heightlist[[i]],color=linecolor,lwd=linewidth,alpha=alpha)
  }
}