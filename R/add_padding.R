#'@title add_padding
#'
#'@description Adds padding to the matrix
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@return Hillshade with edges padded
#'@keywords internal
add_padding = function(heightmap) {
  temp = matrix(0, nrow = nrow(heightmap) + 2, ncol = ncol(heightmap) + 2)
  temp[2:(nrow(temp)-1), 2:(ncol(temp)-1)] = heightmap
  temp[2:(nrow(temp)-1), 1] = heightmap[,1]
  temp[1, 2:(ncol(temp)-1)] = heightmap[1,]
  temp[2:(nrow(temp)-1), ncol(temp)] = heightmap[,ncol(heightmap)]
  temp[nrow(temp), 2:(ncol(temp)-1)] = heightmap[nrow(heightmap),]
  temp[1,1] = temp[1,2]
  temp[1,ncol(temp)] = temp[1,ncol(temp)-1]
  temp[nrow(temp),1] = temp[nrow(temp)-1,2]
  temp[nrow(temp),ncol(temp)] = temp[nrow(temp)-1,ncol(temp)]
  temp
}