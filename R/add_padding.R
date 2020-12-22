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

#'@title add_multi_padding
#'
#'@description Adds multiple levels padding to the matrix
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param pad Number of padding entries
#'@return Hillshade with edges padded
#'@keywords internal
add_multi_padding = function(heightmap, pad = 1) {
  temp = matrix(0, nrow = nrow(heightmap) + 2*pad, ncol = ncol(heightmap) + 2*pad)
  temp[(1+pad):(pad + nrow(heightmap)), (1+pad):(pad+ncol(heightmap))] = heightmap
  temp[(1+pad):(pad + nrow(heightmap)), 1:pad] = heightmap[,1]
  for(i in 1:pad) {
    temp[i, (1+pad):(pad + ncol(heightmap))] = heightmap[1,]
  }
  for(i in (pad+ncol(heightmap)+1):ncol(temp)) {
    temp[(1+pad):(pad+nrow(heightmap)), i] = heightmap[,ncol(heightmap)]
  }
  for(i in (pad+nrow(heightmap)+1):nrow(temp)) {
    temp[i, (1+pad):(pad+ncol(heightmap))] = heightmap[nrow(heightmap),]
  }
  temp[1:pad,1:pad] = heightmap[1,1]
  temp[1:pad,(pad+ncol(heightmap)+1):ncol(temp)] = heightmap[1,ncol(heightmap)]
  temp[(pad+nrow(heightmap)+1):nrow(temp),1:pad] = heightmap[nrow(heightmap),1]
  temp[(pad+nrow(heightmap)+1):nrow(temp),(pad+ncol(heightmap)+1):ncol(temp)] = heightmap[nrow(heightmap),ncol(heightmap)]
  temp
}

#'@title trim_padding
#'
#'@description Trims padding
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param pad Number of padding entries
#'@return Hillshade with edges trimmed
#'@keywords internal
trim_padding = function(heightmap, pad = 1) {
  heightmap[(1+pad):(nrow(heightmap)-pad), (1+pad):(ncol(heightmap)-pad)]
}





