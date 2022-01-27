#' Generate Fractal Perlin Noise
#'
#' @param image Matrix
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
gen_fractal_perlin = function(ray_d, xyz, altitude, nrow,ncol, levels=8, inc=100, freq=0.01/2, seed=1, time = 0) {
  fract_perlin = ambient::gen_perlin(x=xyz[,1]       + ray_d[1] * inc,
                                     y=altitude + ray_d[2] * inc + time,
                                     z=xyz[,3]        - ray_d[3] * inc,
                                     frequency = freq / 2, 
                                     seed = seed)
  if(levels > 1) {
    for(i in seq_len(levels)) {
      fract_perlin = fract_perlin + 1/i *  ambient::gen_perlin(x = xyz[,1]   +     ray_d[1] * inc,
                                                               y = altitude + ray_d[2] * inc + time,
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
                                offset_x = 0, offset_y = 0,
                                time = 0,
                                start_altitude = 1000, end_altitude=2500, 
                                alpha_coef = 0.8, nrow=540, ncol=540,
                                scale_x = 1, scale_y = 1, scale_z = 1,
                                freq=0.01/2, coef = 0.05,seed=1) {
  ray_d = c(cospi(sun_altitude/180)*cospi(sun_angle/180),
            sinpi(sun_altitude/180),
            cospi(sun_altitude/180)*sinpi(sun_angle/180))
  xyz = as.matrix(expand.grid(x = 1:nrow + offset_y, y = 0, z = 1:ncol + offset_x))
  alpha_layer = scales::rescale(gen_fractal_perlin(xyz=xyz,ray_d = ray_d, nrow=nrow,ncol=ncol,
                                                   time = time,
                                                   altitude=start_altitude, levels=levels,
                                                   inc=0,seed=seed,freq=freq),
                                to = c(alpha_coef, 1.0))
  if(ray_d[2] > 0) {
    step = 1/ray_d[2]
  } else {
    stop("Zero/negative sun altitudes are not valid")
  }
  
  alpha_layer[alpha_layer < 0] = 0
  atten = matrix(1, nrow, ncol)
  altitude = start_altitude
  inc = step
  while(altitude < end_altitude-1) {
    trans_mat = scales::rescale(gen_fractal_perlin(ray_d, xyz, nrow = nrow, ncol = ncol,
                                                   time = time,
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
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. This is used by `render_clouds()` to 
#'calculate the regions the clouds should be rendered in.
#'@param start_altitude Default `1000`. The bottom of the cloud layer.
#'@param end_altitude Default `2000`. The top of the cloud layer.
#'@param sun_altitude Default `90`. The angle, in degrees (as measured from the horizon) from which the light originates.
#'@param sun_angle Default `315` (NW). The angle, in degrees, around the matrix from which the light originates. Zero degrees is North, increasing clockwise
#'@param time Default `0`. Advance this to make the clouds evolve and change in shape.
#'@param cloud_cover Default `0.5`. The percentage of cloud cover.
#'@param layers Default `10`. The number of layers to render the cloud layer. 
#'The default is `layers/(start_altitude - end_altitude)`.
#'@param offset_x Default `0`. Change this to move the cloud layer sideways. 
#'@param offset_y Default `0`. Change this to move the cloud layer backwards and forwards.
#'@param scale_x Default `1`. Scale the fractal pattern in the x direction.
#'@param scale_y Default `1`. Scale the fractal pattern in the y direction.
#'@param scale_z Default `NULL` (automatically calculated). Scale the fractal pattern in the z (vertical) direction. 
#'@param frequency Default `0.005`. The base frequency of the noise used to calculate the fractal cloud structure.
#'@param fractal_levels Default `16`. The fractal dimension used to calculate the noise. Higher values give more fine structure, but take longer to calculate.
#'@param attenuation_coef Default `1`. Amount of attenuation in the cloud (higher numbers give darker shadows).  This value is automatically scaled to account for increasing the number of layers.
#'@param seed Default `1`. Random seed used to generate clouds.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. 
#'@param baseshape Default `rectangle`. Shape of the base. Options are `c("rectangle","circle","hex")`.
#'@param clear_clouds Default `FALSE`. Clears all existing floating layers on the visualization.
#'@return Adds a 3D floating cloud layer to the map. No return value.
#'@export
#'@examples
#'\dontrun{
#'#Render a cloud layer over Monterey Bay
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  plot_3d(montereybay,background="brown",zscale=50)
#'
#'#Render some clouds
#'render_clouds(montereybay, zscale=50)  
#'render_snapshot()
#'rgl::rgl.clear()
#'
#'
#'#Change the seed for a different set of clouds and add cloud shadows on the ground
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  add_shadow(cloud_shade(montereybay,zscale=50, seed = 2), 0.0) %>%
#'  plot_3d(montereybay,background="brown",zscale=50)
#'render_camera(theta=-65, phi = 25, zoom = 0.45, fov = 80)
#'render_clouds(montereybay, zscale=50, seed=2, clear_clouds = T)    
#'render_snapshot()
#'rgl::rgl.clear()
#'
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  plot_3d(montereybay,background="brown",zscale=50)
#'  
#'#Lower the frequency for larger, smoother clouds
#'render_clouds(montereybay, zscale=50, frequency = 0.001, clear_clouds = T)
#'render_snapshot()
#'
#'#Increase the frequency for more broken clouds
#'render_clouds(montereybay, zscale=50, frequency = 0.05, clear_clouds = T)
#'render_snapshot()
#'
#'#Increase the fractal level for fluffier, bumpier clouds
#'render_clouds(montereybay, zscale=50, fractal_levels = 32, clear_clouds = T)
#'render_snapshot()
#'
#'#Decrease the fractal level for more smoother, continuous clouds
#'render_clouds(montereybay, zscale=50, fractal_levels = 4, clear_clouds = T)
#'render_snapshot()
#'
#'#Increase the cloud cover
#'render_clouds(montereybay, zscale=50, cloud_cover=0.8, clear_clouds = T)            
#'render_snapshot()
#'
#'#Decrease the cloud cover
#'render_clouds(montereybay, zscale=50, cloud_cover=0.2, clear_clouds = T)            
#'render_snapshot()
#'
#'#Change the altitude range of the clouds
#'render_clouds(montereybay,zscale=50,start_altitude=2000,end_altitude = 4000, clear_clouds = T)            
#'render_snapshot()
#'
#'#Increase the number of layers 
#'render_clouds(montereybay, zscale=50,start_altitude=2000,end_altitude = 4000, layers = 20,
#'              clear_clouds = T)
#'render_snapshot()
#'
#'#Change the sun angle and altitude, and increase the attenuation for darker clouds
#'render_clouds(montereybay,zscale=50,sun_angle=45, sun_altitude= 5, attenuation_coef = 5,
#'              clear_clouds = T)
#'render_snapshot()
#'
#'#Render the scene with a different baseshape
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  plot_3d(montereybay,background="brown",zscale=50, baseshape="hex")
#'render_clouds(montereybay,zscale=50, seed=3, baseshape="hex", clear_clouds = T)  
#'render_camera(zoom=0.65)
#'render_snapshot()
#'rgl::rgl.close()
#'}
render_clouds = function(heightmap, start_altitude = 1000, end_altitude=2000, 
                         sun_altitude = 90, sun_angle=315, time = 0,
                         cloud_cover = 0.5, layers = 10, offset_x = 0, offset_y = 0,
                         scale_x = 1, scale_y = 1, scale_z = NULL,
                         frequency = 0.005, fractal_levels = 16,
                         attenuation_coef = 1, seed = 1, 
                         zscale=1, baseshape="rectangle",
                         clear_clouds = FALSE) {
  time = -time
  if(start_altitude > end_altitude) {
    temp_alt = start_altitude
    start_altitude = end_altitude
    end_altitude = temp_alt
  }
  if(is.null(scale_z)) {
    if(end_altitude != start_altitude) {
      scale_z = layers/(end_altitude - start_altitude)
    } else {
      scale_z = 1
    }
  }
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
  if(cloud_cover < 0 || cloud_cover > 1) {
    stop("`cloud_cover` must be between zero and one.")
  }
  alpha_coef = 1-1/cloud_cover
  layers = layers[1]
  display_altitudes = seq(start_altitude,end_altitude,length.out=layers)
  stopifnot(start_altitude < end_altitude)
  stopifnot(layers > 0)
  stopifnot(sun_altitude > 0 && sun_altitude <= 90)
  
  altitudes = seq(start_altitude,start_altitude+layers,length.out=layers)
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  attenuation_coef = attenuation_coef/layers

  for(i in seq_len(layers)) {
    render_floating_overlay(generate_cloud_layer(coef=attenuation_coef, 
                                                 start_altitude = i-1 + time, end_altitude = layers + time,
                                                 time = time,
                                                 sun_altitude = sun_altitude, alpha_coef = alpha_coef, 
                                                 sun_angle = sun_angle, levels = fractal_levels,
                                                 offset_x = offset_x, offset_y = offset_y,
                                                 scale_x = scale_x, scale_y = scale_y, scale_z = scale_z,
                                                 nrow = nr, ncol=nc,
                                                 seed=seed,freq=frequency)  ,
                            display_altitudes[i], baseshape = baseshape,
                            zscale=zscale)
  }
}


#' Generate Fractal Perlin Noise
#'
#' @param image Matrix
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
gen_fractal_perlin_shadow = function(ray_d, xyz, t_mat, levels=8, inc=100, freq=0.01/2, seed=1) {
  ncol = ncol(t_mat)
  nrow = nrow(t_mat)
  
  fract_perlin = ambient::gen_perlin(x=xyz[,1],
                                     y=xyz[,2],
                                     z=xyz[,3],
                                     frequency = freq / 2, 
                                     seed = seed)
  fract_perlin[t_mat < 0] = 0
  if(levels > 1) {
    for(i in seq_len(levels)) {
      temp_fract = 1/i *  ambient::gen_perlin(x = xyz[,1]   +     ray_d[1] * inc,
                                              y = xyz[,2]   +     ray_d[2] * inc,
                                              z = xyz[,3]   -     ray_d[3] * inc,
                                              frequency = freq * i, 
                                              seed = seed+i)
      temp_fract[t_mat < 0] = 0
      fract_perlin = fract_perlin + temp_fract
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
raymarch_cloud_layer = function(heightmap, sun_altitude = 90, sun_angle=315, levels=8,
                                start_altitude = 1000, end_altitude=2500,  time = 0,
                                alpha_coef = 0.8, layers = 10,
                                offset_x = 0, offset_y = 0,
                                scale_x = 1, scale_y = 1, scale_z = 1,
                                step = 100, freq=0.01/2, coef = 0.05, seed = 1) {
  ray_d = c(cospi(sun_altitude/180)*cospi(sun_angle/180),
            sinpi(sun_altitude/180),
            cospi(sun_altitude/180)*sinpi(sun_angle/180))
  if(ray_d[2] > 0) {
    step = 1/ray_d[2]
    real_step = (end_altitude - start_altitude)/layers/ray_d[2]
  } else {
    stop("Zero/negative sun altitudes are not valid")
  }
  nrow = nrow(heightmap)
  ncol = ncol(heightmap)
  t_mat = matrix(start_altitude - heightmap, nrow, ncol) / ray_d[2]

  xyz = as.matrix(expand.grid(x = 1:nrow + offset_y, y = 1, z = 1:ncol + offset_x))

  xyz[,1] = xyz[,1] + ray_d[1] * t_mat
  xyz[,2] = time
  xyz[,3] = xyz[,3] - ray_d[3] * t_mat
  
  atten = matrix(1, nrow, ncol)
  altitude = start_altitude
  inc = step
  
  while(altitude <= end_altitude) {
    trans_mat = scales::rescale(gen_fractal_perlin_shadow(ray_d, xyz, t_mat = t_mat, 
                                                   levels = levels,
                                                   inc = inc, seed = seed, freq = freq),
                                to = c(alpha_coef, 1.0))
    trans_mat[trans_mat < 0] = 0
    atten = atten * (1 - coef * trans_mat)
    altitude = start_altitude + ray_d[2] * inc
    t_mat = t_mat + real_step
    inc = inc + step
    if(step == 0) {
      break
    }
  }
  return(atten)
}

#'@title Cloud Shade
#'
#'@description Render shadows from the 3D floating cloud layer on the ground. Use this function
#'to add shadows to the map with the `add_shadow()` function.
#'
#'For realistic results, argument should match those passed to `render_clouds()`. The exception to this
#'is `attenuation_coef`, which can be used to adjust the darkness of the resulting shadows.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. This is used by `render_clouds()` to 
#'calculate the regions the clouds should be rendered in.
#'@param start_altitude Default `1000`. The bottom of the cloud layer.
#'@param end_altitude Default `2000`. The top of the cloud layer.
#'@param sun_altitude Default `10`. The angle, in degrees (as measured from the horizon) from which the light originates.
#'@param sun_angle Default `315` (NW). The angle, in degrees, around the matrix from which the light originates. Zero degrees is North, increasing clockwise
#'@param time Default `0`. Advance this to make the clouds evolve and change in shape.
#'@param cloud_cover Default `0.5`. The percentage of cloud cover.
#'@param layers Default `90`. The number of layers to render the cloud layer.
#'@param offset_x Default `0`. Change this to move the cloud layer sideways. 
#'@param offset_y Default `0`. Change this to move the cloud layer backwards and forward
#'@param scale_x Default `1`. Scale the fractal pattern in the x direction.
#'@param scale_y Default `1`. Scale the fractal pattern in the y direction.
#'@param scale_z Default `NULL` (automatically calculated). Scale the fractal pattern in the z (vertical) direction. s.
#'@param frequency Default `0.005`. The base frequency of the noise used to calculate the fractal cloud structure.
#'@param fractal_levels Default `16`. The fractal dimension used to calculate the noise. Higher values give more fine structure, but take longer to calculate.
#'@param attenuation_coef Default `1`. Amount of attenuation in the cloud (higher numbers give darker shadows). This value is automatically scaled to account for increasing the number of layers.
#'@param seed Default `1`. Random seed used to generate clouds.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. 
#'@return A 2D shadow matrix.
#'@export
#'@examples
#'\dontrun{
#'#Render clouds with cloud shadows on the ground
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  add_shadow(cloud_shade(montereybay,zscale=50), 0.0) %>%
#'  plot_3d(montereybay,background="brown",zscale=50)
#'render_camera(theta=-65, phi = 25, zoom = 0.45, fov = 80)
#'render_clouds(montereybay, zscale=50)    
#'render_snapshot()
#'rgl::rgl.clear()
#'}
cloud_shade = function(heightmap, start_altitude = 1000, end_altitude=2000, 
                       sun_altitude = 90, sun_angle=315, time = 0,
                       cloud_cover = 0.5, layers = 10, offset_x = 0, offset_y = 0,
                       scale_x = 1, scale_y = 1, scale_z = NULL,
                       frequency = 0.005, fractal_levels = 16,
                       attenuation_coef = 1, seed = 1, 
                       zscale=1) {
  time = -time
  if(start_altitude > end_altitude) {
    temp_alt = start_altitude
    start_altitude = end_altitude
    end_altitude = temp_alt
  }
  if(is.null(scale_z)) {
    if(end_altitude != start_altitude) {
      scale_z = layers/(end_altitude - start_altitude)
    } else {
      scale_z = 1
    }
  }
  if(all(length(find.package("ambient", quiet = TRUE)) == 0)) {
    stop("`render_clouds()` requires the `ambient` package to be installed")
  }
  if(cloud_cover < 0 || cloud_cover > 1) {
    stop("`cloud_cover` must be between zero and one.")
  }
  sun_angle = sun_angle + 180
  alpha_coef = 1-1/cloud_cover
  layers = layers[1]
  stopifnot(start_altitude < end_altitude)
  stopifnot(layers > 0)
  
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  attenuation_coef = attenuation_coef/layers
  if(layers == 1) {
    end_altitude = start_altitude
  }
  return(flipud(t(raymarch_cloud_layer(heightmap = (t(heightmap))/zscale, coef = attenuation_coef, 
                              start_altitude = start_altitude/zscale, end_altitude = end_altitude/zscale, time = time, 
                              sun_altitude = sun_altitude, alpha_coef = alpha_coef, 
                              sun_angle = sun_angle, levels = fractal_levels, layers = layers,
                              offset_x = offset_x, offset_y = offset_y,
                              scale_x = scale_x, scale_y = scale_y, scale_z = scale_z, 
                              seed = seed, freq = frequency))))
}