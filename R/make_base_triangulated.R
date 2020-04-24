#'@title Make Base (for triangulated height maps)
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param tris A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param basedepth Default `0`.
#'@param basecolor Default `grey20`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@keywords internal
make_base_triangulated = function(tris, basedepth=0,basecolor="grey20") {
  bd = basedepth
  edge_row_max = max(tris[,1])
  edge_row_min = min(tris[,1])
  edge_col_max = max(tris[,3])
  edge_col_min = min(tris[,3])
  just_edge_verts = unique(tris[which(tris[,1] == edge_row_max |
                               tris[,1] == edge_row_min |
                               tris[,3] == edge_col_max |
                               tris[,3] == edge_col_min),])
  edge_verts = list()
  counter = 1
  edge_verts[[counter]] = matrix(c(edge_row_max, edge_row_max, edge_row_min, basedepth,basedepth,basedepth,edge_col_min, edge_col_max,edge_col_min),3,3)
  counter = counter + 1
  edge_verts[[counter]] = matrix(c(edge_row_min, edge_row_max,edge_row_min,basedepth,basedepth,basedepth, edge_col_min, edge_col_max,edge_col_max),3,3)
  counter = counter + 1
  # normallist[[1]]= matrix(c(0,0,0,-1,-1,-1,0,0,0),3,3)
  # normallist[[2]]= matrix(c(0,0,0,-1,-1,-1,0,0,0),3,3)
  side_r_min = just_edge_verts[just_edge_verts[,1] == edge_row_min,]
  side_r_min = side_r_min[order(side_r_min[,3]),]
  for(i in 1:(nrow(side_r_min)-1)) {
    nr = edge_row_min
    edge_verts[[counter]] = matrix(c(nr, nr, nr,bd,side_r_min[i,2],bd,side_r_min[i+1,3],side_r_min[i,3],side_r_min[i,3]),3,3)
    counter = counter + 1
    edge_verts[[counter]] = matrix(c(nr, nr, nr,bd,side_r_min[i+1,2],side_r_min[i,2],side_r_min[i+1,3],side_r_min[i+1,3],side_r_min[i,3]),3,3)
    counter = counter + 1
  }
  side_r_max = just_edge_verts[just_edge_verts[,1] == edge_row_max,]
  side_r_max = side_r_max[rev(order(side_r_max[,3])),]
  for(i in 1:(nrow(side_r_max)-1)) {
    nr = edge_row_max
    edge_verts[[counter]] = matrix(c(nr, nr, nr,bd,side_r_max[i,2],bd,side_r_max[i+1,3],side_r_max[i,3],side_r_max[i,3]),3,3)
    counter = counter + 1
    edge_verts[[counter]] = matrix(c(nr, nr, nr,bd,side_r_max[i+1,2],side_r_max[i,2],side_r_max[i+1,3],side_r_max[i+1,3],side_r_max[i,3]),3,3)
    counter = counter + 1
  }
  side_c_min = just_edge_verts[just_edge_verts[,3] == edge_col_min,]
  side_c_min = side_c_min[rev(order(side_c_min[,1])),]
  for(i in 1:(nrow(side_c_min)-1)) {
    nc = edge_col_min
    edge_verts[[counter]] = matrix(c(side_c_min[i+1,1],side_c_min[i,1],side_c_min[i,1],bd,side_c_min[i,2],bd, nc,nc,nc),3,3)
    counter = counter + 1
    edge_verts[[counter]] = matrix(c(side_c_min[i+1,1],side_c_min[i+1,1],side_c_min[i,1],bd,side_c_min[i+1,2],side_c_min[i,2],nc,nc,nc),3,3)
    counter = counter + 1
  }
  side_c_max = just_edge_verts[just_edge_verts[,3] == edge_col_max,]
  side_c_max = side_c_max[order(side_c_max[,1]),]
  for(i in 1:(nrow(side_c_max)-1)) {
    nc = edge_col_max
    edge_verts[[counter]] = matrix(c(side_c_max[i+1,1],side_c_max[i,1],side_c_max[i,1],bd,side_c_max[i,2],bd, nc,nc,nc),3,3)
    counter = counter + 1
    edge_verts[[counter]] = matrix(c(side_c_max[i+1,1],side_c_max[i+1,1],side_c_max[i,1],bd,side_c_max[i+1,2],side_c_max[i,2],nc,nc,nc),3,3)
    counter = counter + 1
  }
  fullsides = do.call(rbind,edge_verts)
  rgl::triangles3d(fullsides, #normals = fullnormals,
                   lit=FALSE,color=basecolor,front="filled",back="culled",ambient = "#000002")
}
