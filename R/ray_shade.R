#'@title Calculate Raytraced Shadow Map
#'
#'@description Calculates shadow map for a elevation matrix by propogating rays from each matrix point to the light source(s),
#' lowering the brightness at each point for each ray that intersects the surface.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param anglebreaks Default `seq(40,50,1)`. The azimuth angle(s), in degrees, as measured from the horizon from which the light originates.
#'@param sunangle Default `315` (NW). The angle, in degrees, around the matrix from which the light originates. Zero degrees is North, increasing clockwise.
#'@param maxsearch Default `100`. The maximum distance that the system should propogate rays to check. For longer 
#'@param lambert Default `TRUE`. Changes the intensity of the light at each point based proportional to the
#'dot product of the ray direction and the surface normal at that point. Zeros out all values directed away from
#'the ray.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param multicore Default `FALSE`. If `TRUE`, multiple cores will be used to compute the shadow matrix. By default, this uses all cores available, unless the user has
#'set `options("cores")` in which the multicore option will only use that many cores.
#'@param cache_mask Default `NULL`. A matrix of 1 and 0s, indicating which points on which the raytracer will operate.
#'@param shadow_cache Default `NULL`. The shadow matrix to be updated at the points defined by the argument `cache_mask`.
#'If present, this will only compute the raytraced shadows for those points with value `1` in the mask.
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar.
#'@param ... Additional arguments to pass to the `makeCluster` function when `multicore=TRUE`.
#'@import foreach doParallel parallel progress
#'@return Matrix of light intensities at each point.
#'@export
#'@examples
#'#Here we produce an shadow map of the `volcano` elevation map with the light from the NE.
#'#The default angle is from 40-50 degrees azimuth, from the north east.
#'volcanoshadow = ray_shade(volcano)
#'    
#'#Turn off Lambertian shading to get a shadow map solely based on the raytraced shadows.
#'volcanoshadow = ray_shade(heightmap = volcano, 
#'    anglebreaks = seq(30,40,10), 
#'    sunangle = 45, 
#'    maxsearch = 100,
#'    lambert = FALSE)
#'    
#'plot_map(volcanoshadow)
ray_shade = function(heightmap, anglebreaks=seq(40,50,1), sunangle=315, maxsearch=100, lambert=TRUE, zscale=1, 
                    multicore = FALSE, cache_mask = NULL, shadow_cache=NULL, progbar=interactive(), ...) {
  anglebreaks = anglebreaks[order(anglebreaks)]
  anglebreaks_rad = anglebreaks*pi/180
  sunangle_rad = sunangle*pi/180
  flipud = function(x) {
    x[,ncol(x):1]
  }
  originalheightmap = heightmap
  heightmap = add_padding(heightmap)
  if(is.null(cache_mask)) {
    cache_mask = matrix(1,nrow = nrow(heightmap),ncol=ncol(heightmap))
  } else {
    padding = matrix(0,nrow(cache_mask)+2,ncol(cache_mask)+2)
    padding[2:(nrow(padding)-1),2:(ncol(padding)-1)] = cache_mask
    cache_mask = padding
  }
  if(!multicore) {
    shadowmatrix = rayshade_cpp(sunangle = sunangle_rad, anglebreaks = anglebreaks_rad, 
                                heightmap = flipud(heightmap), zscale = zscale, 
                                maxsearch = maxsearch, cache_mask = cache_mask, progbar = progbar)
    shadowmatrix = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
    cache_mask = cache_mask[c(-1,-nrow(cache_mask)),c(-1,-ncol(cache_mask))]
    shadowmatrix[shadowmatrix<0] = 0
    if(lambert) {
      shadowmatrix = shadowmatrix * lamb_shade(originalheightmap, rayangle = mean(anglebreaks), 
                                                         sunangle = sunangle, zscale = zscale)
    }
    if(!is.null(shadow_cache)) {
      shadow_cache[cache_mask == 1] = shadowmatrix[cache_mask == 1]
      shadowmatrix = matrix(shadow_cache,nrow=nrow(shadowmatrix),ncol=ncol(shadowmatrix))
    }
    return(shadowmatrix)
  } else {
    if(is.null(options("cores")[[1]])) {
      numbercores = parallel::detectCores()
    } else {
      numbercores = options("cores")[[1]]
    }
    if(nrow(heightmap) < numbercores*16) {
      if(nrow(heightmap) < 4) {
        chunksize = 1
      }
      chunksize = 4
    } else {
      chunksize = 16
    }
    if(nrow(heightmap) %% chunksize == 0) {
      number_multicore_iterations = nrow(heightmap)/chunksize
    } else {
      number_multicore_iterations = floor(nrow(heightmap)/chunksize) + 1
    }
    itervec = rep(1,number_multicore_iterations)
    for(i in 0:number_multicore_iterations) {
      itervec[i+1] = 1 + i*chunksize
    }
    itervec[length(itervec)] = nrow(heightmap) + 1
    cl = parallel::makeCluster(numbercores, ...)
    doParallel::registerDoParallel(cl, cores = numbercores)
    shadowmatrixlist = tryCatch({
      foreach::foreach(i=1:(length(itervec)-1),.export = c("rayshade_multicore")) %dopar% {
        rayshade_multicore(sunangle = sunangle_rad, anglebreaks = anglebreaks_rad, 
                           heightmap = flipud(heightmap), zscale = zscale, chunkindices = c(itervec[i],(itervec[i+1])),
                           maxsearch = maxsearch, cache_mask = cache_mask)
      }
    }, finally = {
      tryCatch({
        parallel::stopCluster(cl)
      }, error = function (e) {print(e)})
    })
    shadowmatrix = do.call(rbind,shadowmatrixlist) 
    shadowmatrix[shadowmatrix<0] = 0
    shadowmatrix = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
    cache_mask = cache_mask[c(-1,-nrow(cache_mask)),c(-1,-ncol(cache_mask))]
    if(lambert) {
      shadowmatrix = shadowmatrix * lamb_shade(originalheightmap, rayangle = mean(anglebreaks), 
                                              sunangle = sunangle, zscale = zscale)
    }
    if(!is.null(shadow_cache)) {
      shadow_cache[cache_mask == 1] = shadowmatrix[cache_mask == 1]
      shadowmatrix = matrix(shadow_cache,nrow=nrow(shadowmatrix),ncol=ncol(shadowmatrix))
    }
    return(shadowmatrix)
  }
}
globalVariables('i')