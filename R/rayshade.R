#'@title Rayshade
#'
#'@description Calculates global shadow map for a elevation matrix by propogating rays from each matrix point to the light source(s),
#' lowering the brightness at each point for each ray that intersects the surface.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param anglebreaks The angle(s) as measured from the horizon from which the light originates.
#'@param sunangle The angle around the matrix from which the light originates. 
#'@param maxsearch The maximum distance that the system should propogate rays to check. For longer 
#'@param zscale Default 1. The ratio between the x and y spacing (which are assumed to be equal) and the z axis.  
#'@param multicore Default FALSE. If TRUE, multiple cores will be used to compute the shadow matrix. By default, this uses all cores available, unless the user has
#'set `options("cores")` in which the multicore option will only use that many cores.
#'@param verbose Default TRUE. Tracks progress by outputing the current column being shaded to the console. 
#'@import foreach doParallel parallel
#'@return Matrix of light intensities at each point.
#'@export
#'@examples
#'#Here we produce an shadow map of the `volcano` elevation map.
#'volcanoshadow = rayshade(heightmap = volcano, 
#'    anglebreaks = seq(-90,90,10)*pi/180, 
#'    sunangle = 45*pi/180, 
#'    maxsearch = 100,
#'    zscale = 1)
#'    
#'\dontrun{volcanoshadow_fast = rayshade(heightmap = volcano, 
#'    anglebreaks = seq(-90,90,10)*pi/180, 
#'    sunangle = 45*pi/180, 
#'    maxsearch = 100,
#'    zscale = 1, 
#'    multicore=TRUE)}
rayshade = function(heightmap, anglebreaks, sunangle, maxsearch, zscale=1, multicore = FALSE, verbose=TRUE) {
  anglebreaks = anglebreaks[order(anglebreaks)]
  if(!multicore) {
    return(rayshade_cpp(sunangle = sunangle, anglebreaks = anglebreaks, heightmap = heightmap, zscale = zscale, maxsearch = maxsearch,verbose=verbose))
  } else {
    if(is.null(options("cores")[[1]])) {
      numbercores = parallel::detectCores()
    } else {
      numbercores = options("cores")[[1]]
    }
    cl = parallel::makeCluster(numbercores)
    doParallel::registerDoParallel(cl, cores = numbercores)
    shadowmatrix = tryCatch({
      foreach::foreach(i=1:nrow(heightmap), .combine="rbind", .packages = c("rayshader")) %dopar% {
        rayshade_multicore(sunangle = sunangle, anglebreaks = anglebreaks, heightmap = heightmap, zscale = zscale, maxsearch = maxsearch, row = i-1)
      }
    }, finally = {
      tryCatch({
        parallel::stopCluster(cl)
      }, error = function (e) {})
    })
    return(shadowmatrix)
  }
}
globalVariables('i')