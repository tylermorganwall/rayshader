#'@title Calculate Raytraced Shadow Map
#'
#'@description Calculates shadow map for a elevation matrix by propogating rays from each matrix point to the light source(s),
#' lowering the brightness at each point for each ray that intersects the surface.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param sunaltitude Default `45`. The angle, in degrees (as measured from the horizon) from which the light originates. The width of the light
#'is centered on this value and has an angular extent of 0.533 degrees, which is the angular extent of the sun. Use the `anglebreaks` argument
#'to create a softer (wider) light. This has a hard minimum/maximum of 0/90 degrees.
#'@param sunangle Default `315` (NW). The angle, in degrees, around the matrix from which the light originates. Zero degrees is North, increasing clockwise.
#'@param maxsearch Defaults to the longest possible shadow given the `sunaltitude` and `heightmap`. 
#'Otherwise, this argument specifies the maximum distance that the system should propagate rays to check. 
#'@param lambert Default `TRUE`. Changes the intensity of the light at each point based proportional to the
#'dot product of the ray direction and the surface normal at that point. Zeros out all values directed away from
#'the ray.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation is in units
#'of meters and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param multicore Default `FALSE`. If `TRUE`, multiple cores will be used to compute the shadow matrix. By default, this uses all cores available, unless the user has
#'set `options("cores")` in which the multicore option will only use that many cores.
#'@param cache_mask Default `NULL`. A matrix of 1 and 0s, indicating which points on which the raytracer will operate.
#'@param shadow_cache Default `NULL`. The shadow matrix to be updated at the points defined by the argument `cache_mask`.
#'If present, this will only compute the raytraced shadows for those points with value `1` in the mask.
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar.
#'@param anglebreaks Default `NULL`. A vector of angle(s) in degrees (as measured from the horizon) specifying from where the light originates. 
#'Use this instead of `sunaltitude` to create a softer shadow by specifying a wider light. E.g. `anglebreaks = seq(40,50,by=0.5)` creates a light 
#'10 degrees wide, as opposed to the default
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
#'    sunaltitude = 35, 
#'    sunangle = 45, 
#'    maxsearch = 100,
#'    lambert = FALSE)
#'    
#'plot_map(volcanoshadow)
ray_shade = function(heightmap, sunaltitude=45, sunangle=315, maxsearch=NULL, lambert=TRUE, zscale=1, 
                    multicore = FALSE, cache_mask = NULL, shadow_cache=NULL, progbar=interactive(), 
                    anglebreaks = NULL, ...) {
  if(is.null(anglebreaks)) {
    anglebreaks = seq(max(0,sunaltitude-0.533/2), min(90,sunaltitude+0.533/2), length.out = 10)
  }
  if(all(anglebreaks <= 0)) {
    return(matrix(0,nrow=nrow(heightmap),ncol=ncol(heightmap)))
  }
  if(is.null(maxsearch)) {
    maxsearch = (max(heightmap,na.rm=TRUE) - min(heightmap,na.rm=TRUE))/(zscale*sinpi(min(anglebreaks[anglebreaks > 0])/180))
  }
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
      shadowmatrix = shadowmatrix * lamb_shade(originalheightmap, sunaltitude = mean(anglebreaks), 
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
      shadowmatrix = shadowmatrix * lamb_shade(originalheightmap, sunaltitude = mean(anglebreaks), 
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