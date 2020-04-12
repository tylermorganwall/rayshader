#'@title make_base
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param basedepth Default `0`.
#'@param basecolor Default `grey20`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param litbase Default `FALSE`.
#'@keywords internal
make_base = function(heightmap,basedepth=0,basecolor="grey20",zscale=1,litbase=FALSE) {
  heightmap = heightmap/zscale
  edge_vals = unique(c(heightmap[1,], heightmap[,1],heightmap[nrow(heightmap),],heightmap[,ncol(heightmap)]))
  if(length(edge_vals) == 1 && all(!is.na(edge_vals))) {
    heightlist = list()
    normallist = list()
    nc = ncol(heightmap)
    nr = nrow(heightmap)
    heightlist[[1]] = matrix(c(1, nr, nr, basedepth,basedepth,basedepth,-1, -nc,-1),3,3)
    heightlist[[2]] = matrix(c(1, nr,1,basedepth,basedepth,basedepth, -nc, -nc,-1),3,3)
    normallist[[1]]= matrix(c(0,0,0,-1,-1,-1,0,0,0),3,3)
    normallist[[2]]= matrix(c(0,0,0,-1,-1,-1,0,0,0),3,3)
    heightlist[[3]] = matrix(c(1, nr,1, basedepth,basedepth,edge_vals,-1,-1,-1),3,3)
    heightlist[[4]] = matrix(c( nr, nr,1,basedepth,edge_vals,edge_vals,-1,-1,-1),3,3)
    normallist[[3]]= matrix(c(0,0,0,0,0,0,-1,-1,-1),3,3)
    normallist[[4]]= matrix(c(0,0,0,0,0,0,-1,-1,-1),3,3)
    heightlist[[5]] = matrix(c(1, nr, 1, edge_vals,basedepth,basedepth, -nc, -nc, -nc),3,3)
    heightlist[[6]] = matrix(c( 1, nr, nr,edge_vals,edge_vals,basedepth, -nc, -nc, -nc),3,3)
    normallist[[5]]= matrix(c(0,0,0,0,0,0,1,1,1),3,3)
    normallist[[6]]= matrix(c(0,0,0,0,0,0,1,1,1),3,3)
    heightlist[[7]] = matrix(c(nr, nr, nr, basedepth,basedepth,edge_vals,-1,-nc,-1),3,3)
    heightlist[[8]] = matrix(c(nr, nr, nr,basedepth,edge_vals,edge_vals,-nc,-nc,-1),3,3)
    normallist[[7]]= matrix(c(-1,-1,-1,0,0,0,0,0,0),3,3)
    normallist[[8]]= matrix(c(-1,-1,-1,0,0,0,0,0,0),3,3)
    heightlist[[9]] = matrix( c(1,1,1, edge_vals,basedepth,basedepth, -1, -nc, -1),3,3)
    heightlist[[10]] = matrix(c(1,1,1, edge_vals,edge_vals,basedepth, -1, -nc, -nc),3,3)
    normallist[[9]]= matrix( c(1,1,1,0,0,0,0,0,0),3,3)
    normallist[[10]]= matrix(c(1,1,1,0,0,0,0,0,0),3,3)
    fullsides = do.call(rbind,heightlist)
    fullnormals = do.call(rbind,normallist)
    fullsides[,1] = fullsides[,1] - nrow(heightmap)/2
    fullsides[,3] = -fullsides[,3] - ncol(heightmap)/2
    fullsides = fullsides[nrow(fullsides):1,]
    fullnormals = fullnormals[nrow(fullnormals):1,]
    rgl::triangles3d(fullsides, normals=fullnormals,
                     lit=litbase,color=basecolor,front="filled",back="culled",ambient = "#000002")
  } else if(all(!is.na(heightmap))) {
    na_matrix = is.na(heightmap)
    baselist = make_base_cpp(heightmap, na_matrix, basedepth)
    heightlist = baselist$vertices
    normallist = baselist$normals
    heightlist[[length(heightlist)+1]] = matrix(c(1,nrow(heightmap),nrow(heightmap), basedepth,basedepth,basedepth,-1,-ncol(heightmap),-1),3,3)
    heightlist[[length(heightlist)+2]] = matrix(c(1,nrow(heightmap),1,basedepth,basedepth,basedepth,-ncol(heightmap),-ncol(heightmap),-1),3,3)
    normallist[[length(normallist)+1]]= matrix(c(0,0,0,-1,-1,-1,0,0,0),3,3)
    normallist[[length(normallist)+2]]= matrix(c(0,0,0,-1,-1,-1,0,0,0),3,3)
    fullsides = do.call(rbind,heightlist)
    fullnormals = do.call(rbind,normallist)
    fullsides[,1] = fullsides[,1] - nrow(heightmap)/2
    fullsides[,3] = -fullsides[,3] - ncol(heightmap)/2
    fullsides = fullsides[nrow(fullsides):1,]
    fullnormals = fullnormals[nrow(fullnormals):1,]
    rgl::triangles3d(fullsides, normals=fullnormals,
                     lit=litbase,color=basecolor,front="filled",back="filled",ambient = "#000002")
  } else {
    na_matrix = is.na(heightmap)
    baselist = make_base_cpp(heightmap, na_matrix, basedepth)
    heightlist = baselist$vertices
    normallist = baselist$normals
    fullsides = do.call(rbind,heightlist)
    fullsides[,1] = fullsides[,1] - nrow(heightmap)/2
    fullsides[,3] = -fullsides[,3] - ncol(heightmap)/2
    fullsides = fullsides[nrow(fullsides):1,]
    fullnormals = do.call(rbind,normallist)
    fullnormals = fullnormals[nrow(fullnormals):1,]
    basemat = matrix(basedepth,nrow(heightmap),ncol(heightmap))
    basemat[is.na(heightmap)] = NA
    normalmat = matrix(0,nrow(heightmap),ncol(heightmap))
    xznormals = fliplr(heightmap)
    ynormals = fliplr(heightmap)
    xznormals[!is.na(xznormals)] = 0
    ynormals[!is.na(ynormals)] = -1
    rgl.surface(1:nrow(basemat)-nrow(basemat)/2,1:ncol(basemat)-ncol(basemat)/2,basemat,color=basecolor,
                lit=litbase,back="filled",front="filled",ambient = "#000007", 
                normal_x = xznormals, normal_z = xznormals, normal_y = ynormals)
    rgl::triangles3d(fullsides, normals = fullnormals,
                     lit=litbase,color=basecolor,front="filled",back="filled",ambient = "#000002")
  }
}
