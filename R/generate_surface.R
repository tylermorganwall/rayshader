#'@title generate_surface
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@keywords internal
generate_surface = function(heightmap, zscale) {
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  vertex_matrix_row = matrix(seq_len(nr), 
                             nrow = nr, 
                             ncol = nc) - nr/2
  vertex_matrix_col = matrix(seq_len(nc), 
                             nrow = nr, 
                             ncol = nc, byrow = TRUE) - nc/2
  row_vert = c(vertex_matrix_row)
  col_vert = c(vertex_matrix_col)
  verts = matrix(c(row_vert,c(heightmap)/zscale,col_vert), ncol = 3L)

  #Each surface index associated with two triangles
  offset_row_top = matrix(c(0L, nr, 1L), 
                          ncol = 3L, nrow = (nr - 1L) * (nc - 1L), byrow=TRUE)
  offset_row_bot = matrix(c(1L, nr, nr + 1L),
                          ncol = 3L, nrow = (nr - 1L) * (nc - 1L), byrow=TRUE)
  
  indices_all = matrix(seq_len(length(heightmap)), 
                       nrow = nr,
                       ncol = nc)
  indices_truncated = indices_all[seq(1L, nr-1L), seq(1L, nc-1L)]
  indices_matrix = matrix(c(indices_truncated), 
                          ncol = 3L, nrow = (nr - 1L) * (nc - 1L))
  indices_top_triangle = indices_matrix + offset_row_top
  indices_bot_triangle = indices_matrix + offset_row_bot
  
  has_no_na = function(x) {
    na_mat = matrix(is.na(verts[x,2]), ncol = 3L, nrow = length(x)/3L)
    return(na_mat[,1L] | na_mat[,2L] | na_mat[,3L])
  }
  remove_tri_top = has_no_na(indices_top_triangle)
  remove_tri_bot = has_no_na(indices_bot_triangle)
  
  remove_tri = remove_tri_top | remove_tri_bot
  
  indices_top_triangle = indices_top_triangle[!remove_tri,]
  indices_bot_triangle = indices_bot_triangle[!remove_tri,]
  
  interleved_triangles = matrix(c(t(cbind(indices_top_triangle,indices_bot_triangle))), nrow=3L)
  
  #Set NA values to mean height since rgl uses all the vertices to determine the bounding box
  #(even if they aren't referenced in indices)
  verts[is.na(verts[, 2L]), 2L] = mean(verts[, 2L], na.rm=TRUE)
  
  texcoords_x = matrix(seq(0,1,length.out=nr), 
                       nrow=nr, ncol = nc, byrow = FALSE)
  texcoords_y = matrix(seq(1,0,length.out=nc),
                       nrow=nr, ncol = nc, byrow = TRUE)
  
  texcoords = matrix(c(c(texcoords_x),c(texcoords_y)), ncol = 2L)
  
  return(list(verts = verts, inds = interleved_triangles, texcoords = texcoords))
}