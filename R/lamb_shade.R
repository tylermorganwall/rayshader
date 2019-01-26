#'@title Calculate Lambert Shading Map
#'
#'@description Calculates local shadow map for a elevation matrix by calculating the dot 
#'product between light direction and the surface normal vector at that point. Each point's
#'intensity is proportional to the cosine of the normal ve
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param rayangle Default `45`. The azimuth angle as measured from the horizon from which the light originates.
#'@param sunangle Default `315` (NW). The angle around the matrix from which the light originates. 
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. 
#'@param zero_negative Default `TRUE`. Zeros out all values below 0 (corresponding to surfaces facing away from the light source).
#'@return Matrix of light intensities at each point.
#'@export
#'@examples
#'#Here we produce a light intensity map of the `volcano` elevation map.
#'volcanointensity = lamb_shade(heightmap = volcano, 
#'    rayangle = 60, 
#'    sunangle = 25, 
#'    zscale = 1) 
#'    
#'plot_map(volcanointensity)
lamb_shade = function(heightmap, rayangle=45, sunangle=315, zscale = 1, zero_negative = TRUE) {
  sunang_rad = (-sunangle)*pi/180;
  rayang_rad = rayangle*pi/180;
  rayvector = c(sin(sunang_rad)*cos(rayang_rad),cos(sunang_rad)*cos(rayang_rad),-sin(rayang_rad))
  flipud = function(x) {
    x[,ncol(x):1]
  }
  heightmap = add_padding(heightmap)
  heightmap = t(flipud(heightmap)) / zscale;
  shadowmatrix = lambshade_cpp(heightmap = heightmap, rayvector = rayvector)
  shadowmatrix = scales::rescale_max(shadowmatrix,c(0,1))
  if(zero_negative) {
    shadowmatrix[shadowmatrix < 0] = 0
  }
  shadowmatrixremove = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
  return(t(shadowmatrixremove))
}