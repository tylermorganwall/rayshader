#'@title lambshade
#'
#'@description Calculates local shadow map for a elevation matrix by calculating the dot 
#'product between light direction and the surface normal vector at that point. Each point's
#'intensity is proportional to the cosine of the normal ve
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param rayangle The angle as measured from the horizon from which the light originates.
#'@param sunangle The angle around the matrix from which the light originates. 
#'@param zscale Default 1. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. 
#'@param zero_negative Default TRUE. Zeros out all values below 0 (corresponding to surfaces facing away from the light source).
#'@param remove_edges Default TRUE. Slices off artifacts on the edge of the shadow matrix.
#'@return Matrix of light intensities at each point.
#'@export
#'@examples
#'#Here we produce a light intensity map of the `volcano` elevation map.
#'volcanointensity = lambshade(heightmap = volcano, 
#'    rayangle = 55, 
#'    sunangle = 45, 
#'    zscale = 1)
lambshade = function(heightmap, rayangle, sunangle, zscale = 1, zero_negative = TRUE, remove_edges=TRUE) {
  sunang_rad = (sunangle-90)*pi/180;
  rayang_rad = rayangle*pi/180;
  rayvector = c(sin(sunang_rad)*cos(rayang_rad),cos(sunang_rad)*cos(rayang_rad),-sin(rayang_rad))
  heightmap = heightmap / zscale;
  shadowmatrix = lambshade_cpp(heightmap = heightmap, rayvector = rayvector)
  if(zero_negative) {
    shadowmatrix[shadowmatrix < 0] = 0
  }
  if(remove_edges) {
    shadowmatrixremove = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
    return(shadowmatrixremove)
  } else {
    return(shadowmatrix)
  }
}