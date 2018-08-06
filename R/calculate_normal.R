#'@title Calculate Normal
#'
#'@description Calculates the normal unit vector for every point on the grid.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param zscale Default 1.
#'@param remove_edges Default TRUE. Slices off artifacts on the edge of the shadow matrix.
#'@param progbar Default `FALSE`. If `TRUE`, turns on progress bar.
#'@return Matrix of light intensities at each point.
#'@export
#'@examples
#'#Here we produce a light intensity map of the `volcano` elevation map.
calculate_normal = function(heightmap, zscale=1, remove_edges = FALSE, progbar=FALSE) {
  flipud = function(x) {
    x[,ncol(x):1]
  }
  heightmap = heightmap / zscale
  matrices = calculate_normal_cpp(heightmap=heightmap, progbar = progbar)
  returnnormal = list()
  if(remove_edges) {
    returnnormal[["x"]] = t(matrices$x[c(-1,-nrow(heightmap)),c(-1,-ncol(heightmap))])
    returnnormal[["y"]] = t(matrices$y[c(-1,-nrow(heightmap)),c(-1,-ncol(heightmap))])
    returnnormal[["z"]] = t(matrices$z[c(-1,-nrow(heightmap)),c(-1,-ncol(heightmap))])
    return(returnnormal)
  } else {
    returnnormal[["x"]] = t(matrices$x)
    returnnormal[["y"]] = t(matrices$y)
    returnnormal[["z"]] = t(matrices$z)
    return(returnnormal)
  }
}
