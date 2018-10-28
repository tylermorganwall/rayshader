#'@title make_base
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param basedepth Default `0`.
#'@param basecolor Default `grey20`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@keywords internal
make_base = function(heightmap,basedepth=0,basecolor="grey20",zscale=1) {
  heightmap = heightmap[,ncol(heightmap):1]/zscale
  heightmap1 = heightmap[1,]
  heightmap2 = heightmap[,1]
  heightmap3 = heightmap[nrow(heightmap),]
  heightmap4 = heightmap[,ncol(heightmap)]
  heightlist = make_base_cpp(heightmap1,heightmap2,heightmap3,heightmap4,heightmap,basedepth)
  fullsides = do.call(rbind,heightlist)
  rgl::triangles3d(fullsides,lit=FALSE,color=basecolor,front="fill",back="culled")
}
