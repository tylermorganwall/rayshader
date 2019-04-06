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
#'@keywords internal
make_water = function(heightmap,waterheight=mean(heightmap),watercolor="lightblue",zscale=1,wateralpha=0.5) {
  heightmap = heightmap[,ncol(heightmap):1]/zscale
  na_matrix = is.na(heightmap)
  waterheight = waterheight/zscale
  heightlist = make_water_cpp(heightmap,na_matrix, waterheight)
  if(all(!na_matrix)) {
    triangles3d(matrix(c(nrow(heightmap),1,nrow(heightmap), waterheight,waterheight,waterheight,-ncol(heightmap),-1,-1),3,3),
                color=watercolor,alpha=wateralpha,front="fill",back="fill",texture=NULL,ambient = "#000003")
    triangles3d(matrix(c(1,1,nrow(heightmap),waterheight,waterheight,waterheight,-ncol(heightmap),-1,-ncol(heightmap)),3,3),
                color=watercolor,alpha=wateralpha,front="fill",back="fill",texture=NULL,ambient = "#000003")
    fullsides = do.call(rbind,heightlist)
    rgl::triangles3d(fullsides,lit=FALSE,color=watercolor,alpha=wateralpha,front="fill",depth_test="less",texture=NULL,ambient = "#000003")
  } else {
    fullsides = do.call(rbind,heightlist)
    rgl::triangles3d(fullsides,lit=FALSE,color=watercolor,alpha=wateralpha,front="fill",texture=NULL,ambient = "#000003")
    basemat = matrix(waterheight,nrow(heightmap),ncol(heightmap))
    basemat[is.na(heightmap)] = NA
    rgl.surface(1:nrow(basemat),-(1:ncol(basemat)),basemat,color=watercolor,alpha=wateralpha,lit=FALSE,texture=NULL,ambient = "#000003")
  }
}
