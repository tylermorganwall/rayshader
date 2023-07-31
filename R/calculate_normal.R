#'@title Calculate Normal
#'
#'@description Calculates the normal unit vector for every point on the grid.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param zscale Default 1.
#'@param progbar Default `FALSE`. If `TRUE`, turns on progress bar.
#'@return Matrix of light intensities at each point.
#'@export
#'@examples
#'#Here we produce a light intensity map of the `volcano` elevation map.
#'
#'#Cache the normal vectors of the volcano dataset
#'if(run_documentation()) {
#'volcanocache = calculate_normal(volcano)
#'}
#'
#'#Use the cached vectors to speed up calculation of `sphere_shade()` on a map.
#'if(run_documentation()) {
#'sphere_shade(volcano,normalvectors = volcanocache) %>%
#'  plot_map()
#'}
calculate_normal = function(heightmap, zscale=1, progbar=FALSE) {
  heightmap = add_padding(heightmap)
  heightmap = heightmap / zscale
  matrices = calculate_normal_cpp(heightmap=heightmap, progbar = progbar)
  matrices$x[is.na(heightmap)] = NA
  matrices$y[is.na(heightmap)] = NA
  matrices$z[is.na(heightmap)] = NA
  
  returnnormal = list()
  returnnormal[["x"]] = t(matrices$x)
  returnnormal[["y"]] = t(matrices$y)
  returnnormal[["z"]] = t(matrices$z)
  return(returnnormal)
}
