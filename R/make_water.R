#'@title make_water
#'
#'@description Makes the water in the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param waterheight Default `0`.
#'@param watercolor Default `blue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param wateralpha Default `0.5`. Water transparency.
make_water = function(heightmap,waterheight=mean(heightmap),watercolor="lightblue",zscale=1,wateralpha=0.5) {
  heightmap = heightmap[,ncol(heightmap):1]/zscale
  heightmap1 = heightmap[1,]
  heightmap2 = heightmap[,1]
  heightmap3 = heightmap[nrow(heightmap),]
  heightmap4 = heightmap[,ncol(heightmap)]
  heightlist = make_water_cpp(heightmap1,heightmap2,heightmap3,heightmap4,heightmap,waterheight)
  fullsides = do.call(rbind,heightlist)
  rgl::triangles3d(fullsides,lit=FALSE,color=watercolor,alpha=wateralpha,front="fill",back="culled")
}
