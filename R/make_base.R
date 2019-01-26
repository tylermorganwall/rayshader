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
  na_matrix = is.na(heightmap)
  heightlist = make_base_cpp(heightmap, na_matrix, basedepth)
  if(all(!is.na(heightmap))) {
    heightlist[[length(heightlist)+1]] = matrix(c(1,nrow(heightmap),nrow(heightmap), basedepth,basedepth,basedepth,-1,-ncol(heightmap),-1),3,3)
    heightlist[[length(heightlist)+2]] = matrix(c(1,nrow(heightmap),1,basedepth,basedepth,basedepth,-ncol(heightmap),-ncol(heightmap),-1),3,3)
    fullsides = do.call(rbind,heightlist)
    rgl::triangles3d(fullsides,lit=FALSE,color=basecolor,front="filled",back="filled",ambient = "#000002")
  } else {
    fullsides = do.call(rbind,heightlist)
    basemat = matrix(basedepth,nrow(heightmap),ncol(heightmap))
    basemat[is.na(heightmap)] = NA
    rgl.surface(1:nrow(basemat),-(1:ncol(basemat)),basemat,color=basecolor,lit=FALSE,back="filled",front="filled",ambient = "#000002")
    rgl::triangles3d(fullsides,lit=FALSE,color=basecolor,front="filled",back="filled",ambient = "#000002")
  }
}
