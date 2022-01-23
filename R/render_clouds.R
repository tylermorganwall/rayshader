#' Generate Fractal Perlin Noise
#'
#' @param image Matrix
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
gen_fractal_perlin = function(ray_d, xyz, altitude, nrow,ncol, levels=8, inc=100, freq=0.01/2, seed=1) {
  fract_perlin = ambient::gen_perlin(x=xyz[,1]       + ray_d[1] * inc,
                                     y=altitude + ray_d[2] * inc,
                                     z=xyz[,3]        - ray_d[3] * inc,
                                     frequency = freq / 2, 
                                     seed = seed)
  if(levels > 1) {
    for(i in seq_len(levels)) {
      fract_perlin = fract_perlin + 1/i *  ambient::gen_perlin(x = xyz[,1]   +     ray_d[1] * inc,
                                                               y = altitude + ray_d[2] * inc,
                                                               z = xyz[,3]   -      ray_d[3] * inc,
                                                               frequency = freq * i, 
                                                               seed = seed+i)
    }
  }
  return(matrix(fract_perlin,nrow,ncol))
}

#' Calculate a single raymarched cloud layer
#'
#' @param image Matrix
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
generate_cloud_layer = function(sun_altitude = 90, sun_angle=315, levels=8,
                                start_altitude = 1000, end_altitude=2500, 
                                alpha_coef = 0.8, nrow=540, ncol=540,
                                freq=0.01/2, coef = 0.05,seed=1) {
  ray_d = c(cospi(sun_altitude/180)*cospi(sun_angle/180),
            sinpi(sun_altitude/180),
            cospi(sun_altitude/180)*sinpi(sun_angle/180))
  xyz = as.matrix(expand.grid(x = 1:nrow, y = 0, z = 1:ncol))
  alpha_layer = scales::rescale(gen_fractal_perlin(x=xyz,ray_d = ray_d, nrow=nrow,ncol=ncol,
                                                   altitude=start_altitude, levels=levels,
                                                   inc=0,seed=seed,freq=freq),
                                to = c(alpha_coef, 1.0))
  if(sinpi(sun_altitude/180) > 0) {
    step = 1/sinpi(sun_altitude/180)
  } else {
    stop("Zero/negative sun altitudes are not valid")
  }
  
  alpha_layer[alpha_layer < 0] = 0
  atten = matrix(1, nrow, ncol)
  altitude = start_altitude
  inc = step
  
  while(altitude < end_altitude) {
    trans_mat = scales::rescale(gen_fractal_perlin(ray_d, xyz, nrow = nrow, ncol = ncol,
                                                   altitude = start_altitude, levels = levels,
                                                   inc = inc, seed = seed, freq = freq),
                                to = c(alpha_coef, 1.0))
    trans_mat[trans_mat < 0] = 0
    atten = atten * (1 - coef * trans_mat)
    altitude = start_altitude + ray_d[2] * inc
    inc = inc + step
  }
  full_layer = array(1, dim = c(nrow, ncol, 4))
  full_layer[,,1] = atten
  full_layer[,,2] = atten
  full_layer[,,3] = atten
  full_layer[,,4] = alpha_layer
  return(full_layer)
}

#' Calculate a single raymarched cloud layer
#'
#' @param image Matrix
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
raymarch_cloud_layer = function(heightmap, sun_altitude = 90, sun_angle=315, levels=8,
                                start_altitude = 1000, end_altitude=2500, 
                                alpha_coef = 0.8, 
                                step = 100, freq=0.01/2, coef = 0.05,seed=1) {
  ray_d = c(cospi(sun_altitude/180)*cospi(sun_angle/180),
            sinpi(sun_altitude/180),
            cospi(sun_altitude/180)*sinpi(sun_angle/180))
  nrow = nrow(heightmap)
  ncol = ncol(heightmap)
  xyz = as.matrix(expand.grid(x = 1:nrow, y = 1, z = 1:ncol))
  xyz[,2] = heightmap
  
  alpha_layer = scales::rescale(gen_fractal_perlin(x=xyz,ray_d = ray_d, nrow=nrow,ncol=ncol,
                                                   altitude=start_altitude, levels=levels,
                                                   inc=0,seed=seed,freq=freq),
                                to = c(alpha_coef, 1.0))
  
  alpha_layer[alpha_layer < 0] = 0
  atten = matrix(1, nrow, ncol)
  altitude = start_altitude
  inc = step
  
  while(altitude < end_altitude) {
    trans_mat = scales::rescale(gen_fractal_perlin(ray_d, xyz, nrow = nrow, ncol = ncol,
                                                   altitude = start_altitude, levels = levels,
                                                   inc = inc, seed = seed, freq = freq),
                                to = c(alpha_coef, 1.0))
    trans_mat[trans_mat < 0] = 0
    atten = atten * (1 - coef * trans_mat)
    altitude = start_altitude + ray_d[2] * inc
    inc = inc + step
  }
  full_layer = array(1, dim = c(nrow, ncol, 4))
  full_layer[,,1] = atten
  full_layer[,,2] = atten
  full_layer[,,3] = atten
  full_layer[,,4] = alpha_layer
  return(full_layer)
}

#'@title Render Clouds
#'
#'@description Render a 3D floating cloud layer of the map.
#'
#'Note: Underlying layers with transparency can cause rendering issues in rgl.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'@param altitude Altitude to place the overlay.
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
#'@param alpha Default `1`. Multiplies the layer's transparency by this factor. 0 is completely transparent.
#'@param triangulate Default `TRUE`. If a `heightmap` is passed, this will triangulate the height field for a smaller map.
#'Set this to `TRUE` if generating the model is slow, or moving it is choppy. Will also reduce the size
#'of 3D models saved to disk.
#'@param max_error Default `0.001`. Maximum allowable error when triangulating the height map,
#'when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with `render_highquality()`, or need 
#'to save a smaller 3D OBJ file to disk with `save_obj()`,
#'@param max_tri Default `0`, which turns this setting off and uses `max_error`. 
#'Maximum number of triangles allowed with triangulating the
#'height map, when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with `render_highquality()`, or need 
#'to save a smaller 3D OBJ file to disk with `save_obj()`,
#'@param verbose Default `TRUE`, if `interactive()`. Prints information about the mesh triangulation
#'if `triangulate = TRUE`.
#'@param clear_layers Default `FALSE`. Clears all existing floating layers on the visualization.
#'@return Adds a 3D floating layer to the map. No return value.
#'@export
#'@examples
#'\dontrun{
#'#Render a cloud layer
#'}
render_clouds = function(heightmap, altitude = NULL, zscale=1, layers = 10, baseshape="rectangle",
                         sun_altitude = 10, sun_angle=315, 
                         start_altitude = 1000, end_altitude=2000, 
                         frequency = 0.005, fractal_levels = 16,
                         cloud_cover = 0.5, seed = 1, attenuation_coef = 0.1,
                         clear_clouds = FALSE) {
  if(all(length(find.package("ambient", quiet = TRUE)) == 0)) {
    stop("`render_clouds()` requires the `ambient` package to be installed")
  }
  sun_angle = sun_angle + 180
  if(clear_clouds) {
    rgl::pop3d(tag = c("floating_overlay","floating_overlay_tris"))
    if(missing(heightmap)) {
      return(invisible())
    }
  }
  alpha_coef = 1-1/cloud_cover
  layers = layers[1]
  display_altitudes = seq(start_altitude,end_altitude,length.out=layers)
  
  altitudes = seq(start_altitude,start_altitude+layers,length.out=layers)
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  for(i in seq_len(layers)) {
    render_floating_overlay(generate_cloud_layer(coef=attenuation_coef, 
                                                 start_altitude = i-1, end_altitude = layers,
                                                 sun_altitude = sun_altitude, alpha_coef = alpha_coef, 
                                                 sun_angle = sun_angle, levels = fractal_levels,
                                                 nrow = nr, ncol=nc,
                                                 seed=seed,freq=frequency)  ,
                            display_altitudes[i], baseshape = baseshape,
                            zscale=zscale)
  }
}

#'@title Render Clouds
#'
#'@description Render a 3D floating cloud layer of the map.
#'
#'Note: Underlying layers with transparency can cause rendering issues in rgl.
#'
#'@param overlay Overlay to be added to the 3D map, eit
#'@param altitude Altitude to place the overlay.
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
#'@param alpha Default `1`. Multiplies the layer's transparency by this factor. 0 is completely transparent.
#'@param triangulate Default `TRUE`. If a `heightmap` is passed, this will triangulate the height field for a smaller map.
#'Set this to `TRUE` if generating the model is slow, or moving it is choppy. Will also reduce the size
#'of 3D models saved to disk.
#'@param max_error Default `0.001`. Maximum allowable error when triangulating the height map,
#'when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with `render_highquality()`, or need 
#'to save a smaller 3D OBJ file to disk with `save_obj()`,
#'@param max_tri Default `0`, which turns this setting off and uses `max_error`. 
#'Maximum number of triangles allowed with triangulating the
#'height map, when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with `render_highquality()`, or need 
#'to save a smaller 3D OBJ file to disk with `save_obj()`,
#'@param verbose Default `TRUE`, if `interactive()`. Prints information about the mesh triangulation
#'if `triangulate = TRUE`.
#'@param clear_layers Default `FALSE`. Clears all existing floating layers on the visualization.
#'@return Adds a 3D floating layer to the map. No return value.
#'@export
#'@examples
#'\dontrun{
#'#Render a cloud layer
#'}
cloud_shade = function(heightmap, altitude = NULL, zscale=1, baseshape="rectangle",
                       layers = 10, sun_altitude = 10, sun_angle=315, 
                       start_altitude = 1000, end_altitude=2000, 
                       frequency = 0.01, fractal_levels = 16,
                       cloud_cover = 0.5, seed = 1, attenuation_coef = 0.1) {
  if(all(length(find.package("ambient", quiet = TRUE)) == 0)) {
    stop("`render_clouds()` requires the `ambient` package to be installed")
  }
  sun_angle = sun_angle + 180
  
  alpha_coef = 1-1/cloud_cover
  layers = layers[1]
  display_altitudes = seq(start_altitude,end_altitude,length.out=layers)
  altitudes = seq(start_altitude,start_altitude+layers,length.out=layers)
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  for(i in seq_len(layers)) {
    render_floating_overlay(generate_cloud_layer(coef=attenuation_coef, 
                                                 start_altitude = i-1, end_altitude = layers,
                                                 sun_altitude = sun_altitude, alpha_coef = alpha_coef, 
                                                 sun_angle = sun_angle, levels = fractal_levels,
                                                 nrow = nr, ncol=nc,
                                                 seed=seed,freq=frequency)  ,
                            display_altitudes[i], baseshape = baseshape,
                            heightmap = heightmap,
                            zscale=zscale)
  }
}