#'@title Rayshade
#'
#'@description Calculates global shadow map for a elevation matrix by propogating rays from each matrix point to the light source(s),
#' lowering the brightness at each point for each ray that intersects the surface.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param anglebreaks The angle(s), in degrees, as measured from the horizon from which the light originates.
#'@param sunangle The angle, in degrees, around the matrix from which the light originates. Zero degrees is due North.
#'@param maxsearch The maximum distance that the system should propogate rays to check. For longer 
#'@param lambert Default TRUE. Changes the intensity of the light at each point based proportional to the
#'dot product of the ray direction and the surface normal at that point. Zeros out all values directed away from
#'the ray.
#'@param zscale Default 1. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. 
#'@param multicore Default FALSE. If TRUE, multiple cores will be used to compute the shadow matrix. By default, this uses all cores available, unless the user has
#'set `options("cores")` in which the multicore option will only use that many cores.
#'@param remove_edges Default TRUE. Slices off artifacts on the edge of the shadow matrix.
#'@import foreach doParallel parallel progress
#'@return Matrix of light intensities at each point.
#'@export
#'@examples
#'#Here we produce an shadow map of the `volcano` elevation map with the light from the NE.
#'volcanoshadow = rayshade(heightmap = volcano, 
#'    anglebreaks = seq(55,65,10), 
#'    sunangle = 315, 
#'    maxsearch = 100)
#'    
#'\dontrun{volcanoshadow_fast = rayshade(heightmap = volcano, 
#'    anglebreaks = seq(0,90,10), 
#'    sunangle = 45, 
#'    maxsearch = 100,
#'    multicore=TRUE)}
#'    
#'#Turn off Lambertian shading to get a shadow map solely based on the raytraced shadows.
#'volcanoshadow = rayshade(heightmap = volcano, 
#'    anglebreaks = seq(30,40,10), 
#'    sunangle = 45, 
#'    maxsearch = 100,
#'    lambert = FALSE)
rayshade = function(heightmap, anglebreaks, sunangle, maxsearch, lambert=TRUE, zscale=1, 
                    multicore = FALSE,  remove_edges=TRUE) {
  anglebreaks = anglebreaks[order(anglebreaks)]
  anglebreaks_rad = anglebreaks*pi/180
  sunangle_rad = sunangle*pi/180
  if(!multicore) {
    shadowmatrix = rayshade_cpp(sunangle = sunangle_rad, anglebreaks = anglebreaks_rad, 
                                heightmap = heightmap, zscale = zscale, 
                                maxsearch = maxsearch)
    if(remove_edges) {
      shadowmatrix = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
    }
    shadowmatrix[shadowmatrix<0] = 0
    if(lambert) {
      shadowmatrix = shadowmatrix * lambshade(heightmap, rayangle = mean(anglebreaks), 
                                              sunangle = sunangle, zscale = zscale, remove_edges=remove_edges)
    }
    return(shadowmatrix)
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
        rayshade_multicore(sunangle = sunangle_rad, anglebreaks = anglebreaks_rad, 
                           heightmap = heightmap, zscale = zscale, 
                           maxsearch = maxsearch, row = i-1)
      }
    }, finally = {
      tryCatch({
        parallel::stopCluster(cl)
      }, error = function (e) {})
    })
    shadowmatrix[shadowmatrix<0] = 0
    if(remove_edges) {
      shadowmatrix = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
    }
    if(lambert) {
      shadowmatrix = shadowmatrix * lambshade(heightmap, rayangle = mean(anglebreaks), 
                                              sunangle = sunangle, zscale = zscale, remove_edges=remove_edges)
    }
    return(shadowmatrix)
  }
}
globalVariables('i')