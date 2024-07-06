#' Generate Fractal Perlin Noise
#'
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
gen_fractal_perlin = function(ray_d, xyz, altitude, nrow = NULL, ncol = NULL, t_mat = NULL, levels=8, inc=100, 
                              freq=0.01/2, seed=1, time = 0,
                              scale_x = 1, scale_y = 1, scale_z = 1) {
  fract_perlin = ambient::gen_perlin(x = (xyz[,1]  + ray_d[1] * inc) / scale_x,
                                     y = (altitude + ray_d[2] * inc) / scale_z + time,
                                     z = (xyz[,3]  - ray_d[3] * inc) / scale_y,
                                     frequency = freq / 2, 
                                     seed = seed)
  if(levels > 1) {
    for(i in seq_len(levels)) {
      temp_fract =  1/i *  ambient::gen_perlin(x = (xyz[,1]   + ray_d[1] * inc) / scale_x,
                                               y = (altitude  + ray_d[2] * inc) / scale_z + time,
                                               z = (xyz[,3]   - ray_d[3] * inc) / scale_y,
                                               frequency = freq * i, 
                                               seed = seed+i)
      fract_perlin = fract_perlin + temp_fract
    }
  }
  shadow = !is.null(t_mat)
  if(shadow) {
    ncol = ncol(t_mat)
    nrow = nrow(t_mat)
  }
  if(shadow) {
    fract_perlin[t_mat <= 0] = NA
  }
  return(matrix(fract_perlin,nrow,ncol))
}

#' Calculate a single raymarched cloud layer
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
generate_cloud_layer = function(heightmap, sun_altitude = 90, sun_angle=315, levels=8,
                                offset_x = 0, offset_y = 0,
                                time = 0,
                                start_altitude = 1000, end_altitude=2500, 
                                alpha_coef = 0.8, 
                                scale_x = 1, scale_y = 1, scale_z = 1,
                                freq=0.01/2, coef = 0.05,seed=1) {
  nrow = nrow(heightmap)
  ncol = ncol(heightmap)
  ray_d = c(cospi(sun_altitude/180)*cospi(sun_angle/180),
            sinpi(sun_altitude/180),
            cospi(sun_altitude/180)*sinpi(sun_angle/180))
  xyz = as.matrix(expand.grid(x = 1:nrow - offset_x, y = 0, z = 1:ncol + offset_y))
  alpha_layer = scales::rescale(gen_fractal_perlin(ray_d = ray_d, xyz=xyz, nrow = nrow, ncol = ncol,
                                                   time = time,
                                                   altitude = start_altitude, levels = levels,
                                                   inc = 0, seed = seed, freq = freq,
                                                   scale_x  = scale_x, scale_y  = scale_y, scale_z = scale_z),
                                to = c(alpha_coef, 1.0))
  alpha_layer[alpha_layer < 0] = 0
  
  if(ray_d[2] > 0) {
    step = 1/ray_d[2]
  } else {
    stop("Zero/negative sun altitudes are not valid")
  }
  
  atten = matrix(1, nrow, ncol)
  altitude = start_altitude 
  inc = 1
  while(altitude < end_altitude) {
    trans_mat = scales::rescale(gen_fractal_perlin(ray_d = ray_d, xyz = xyz, nrow = nrow, ncol = ncol,
                                                   time = time,
                                                   altitude = start_altitude, levels = levels,
                                                   inc = inc, seed = seed, freq = freq,
                                                   scale_x  = scale_x, scale_y  = scale_y, scale_z = scale_z),
                                to = c(alpha_coef, 1.0))
    trans_mat[trans_mat < 0] = 0
    atten = atten * (1 - coef * trans_mat)
    altitude = start_altitude +  inc
    inc = inc + 1
  }
  atten[atten < 0] = 0
  full_layer = array(1, dim = c(nrow, ncol, 4))
  full_layer[,,1] = atten
  full_layer[,,2] = atten
  full_layer[,,3] = atten
  full_layer[,,4] = alpha_layer
  return(aperm(full_layer,c(2,1,3)))
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
#'@param scale_z Default `1`. Scale the fractal pattern in the z (vertical) direction. (automatically calculated). Scale the fractal pattern in the z (vertical) direction. 
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
#'if(run_documentation()) {
#'#Render a cloud layer over Monterey Bay
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  plot_3d(montereybay,background="brown",zscale=50)
#'
#'#Render some clouds
#'render_clouds(montereybay, zscale=50)  
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Change the seed for a different set of clouds and add cloud shadows on the ground
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  add_shadow(cloud_shade(montereybay,zscale=50, seed = 2), 0.0) %>%
#'  plot_3d(montereybay,background="brown",zscale=50)
#'render_camera(theta=-65, phi = 25, zoom = 0.45, fov = 80)
#'render_clouds(montereybay, zscale=50, seed=2, clear_clouds = T)    
#'render_snapshot()
#'}
#'
#'if(run_documentation()) {
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  plot_3d(montereybay,background="brown",zscale=50)
#'  
#'#Lower the frequency for larger, smoother clouds
#'render_clouds(montereybay, zscale=50, frequency = 0.001, clear_clouds = T)
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Increase the frequency for more broken clouds
#'render_clouds(montereybay, zscale=50, frequency = 0.05, clear_clouds = T)
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Increase the fractal level for fluffier, bumpier clouds
#'render_clouds(montereybay, zscale=50, fractal_levels = 32, clear_clouds = T)
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Decrease the fractal level for more smoother, continuous clouds
#'render_clouds(montereybay, zscale=50, fractal_levels = 4, clear_clouds = T)
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Increase the cloud cover
#'render_clouds(montereybay, zscale=50, cloud_cover=0.8, clear_clouds = T)            
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Decrease the cloud cover
#'render_clouds(montereybay, zscale=50, cloud_cover=0.2, clear_clouds = T)            
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Change the altitude range of the clouds
#'render_clouds(montereybay,zscale=50,start_altitude=2000,end_altitude = 4000, clear_clouds = T)            
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Increase the number of layers 
#'render_clouds(montereybay, zscale=50,start_altitude=2000,end_altitude = 4000, layers = 20,
#'              clear_clouds = T)
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Change the sun angle and altitude, and increase the attenuation for darker clouds
#'render_clouds(montereybay,zscale=50,sun_angle=45, sun_altitude= 5, attenuation_coef = 5,
#'              clear_clouds = T)
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Render the scene with a different baseshape
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  plot_3d(montereybay,background="darkred",zscale=50, baseshape="hex")
#'render_clouds(montereybay,zscale=50, seed=3, baseshape="hex", clear_clouds = T)  
#'render_camera(zoom=0.65)
#'render_snapshot()
#'}
render_clouds = function(heightmap, start_altitude = 1000, end_altitude=2000, 
                         sun_altitude = 10, sun_angle=315, time = 0,
                         cloud_cover = 0.5, layers = 10, offset_x = 0, offset_y = 0,
                         scale_x = 1, scale_y = 1, scale_z = 1,
                         frequency = 0.005, fractal_levels = 16,
                         attenuation_coef = 1, seed = 1, 
                         zscale=1, baseshape="rectangle",
                         clear_clouds = FALSE) {
  if(all(length(find.package("ambient", quiet = TRUE)) == 0)) {
    stop("`render_clouds()` requires the `ambient` package to be installed")
  }
  time = -time
  if(start_altitude > end_altitude) {
    temp_alt = start_altitude
    start_altitude = end_altitude
    end_altitude = temp_alt
  }
  if(end_altitude != start_altitude) {
    scale_layers = layers/(end_altitude - start_altitude)
  } else {
    scale_layers = 1
    layers = 1
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
  altitudes = seq(start_altitude,end_altitude,length.out=layers)
  stopifnot(start_altitude < end_altitude)
  stopifnot(layers > 0)
  stopifnot(sun_altitude > 0 && sun_altitude <= 90)
  
  altitudes = seq(start_altitude,end_altitude,length.out=layers+1)
  attenuation_coef = attenuation_coef/layers
  
  if(sun_altitude != 90) {
    scaled_angle = zscale * tanpi(sun_altitude/180)
    sun_altitude = atan(scaled_angle)*180/pi
  }
  
  #Generate single slices, ranging from 0 to n_layers
  for(i in seq_len(layers)) {
    render_floating_overlay(generate_cloud_layer(heightmap, coef=attenuation_coef, 
                                                 start_altitude = (altitudes[i]-start_altitude)*scale_layers, 
                                                 end_altitude = (end_altitude-start_altitude)*scale_layers,
                                                 time = time,
                                                 sun_altitude = sun_altitude, alpha_coef = alpha_coef, 
                                                 sun_angle = sun_angle, levels = fractal_levels,
                                                 offset_x = offset_x, offset_y = offset_y,
                                                 scale_x = scale_x, scale_y = scale_y, scale_z = scale_z,
                                                 seed=seed,freq=frequency)  ,
                            altitudes[i], baseshape = baseshape, heightmap = heightmap,
                            zscale=zscale)
  }
}

#' Calculate a single raymarched cloud layer
#'
#' @return image array
#' @keywords internal
#'
#' @examples
#' #Fake example
raymarch_cloud_layer = function(heightmap, sun_altitude = 90, sun_angle=315, levels=8,
                                start_noise = 0, end_noise=10,  
                                start_altitude_real = 0, end_altitude_real = 0,
                                time = 0, 
                                alpha_coef = 0.8, layers = 10,
                                offset_x = 0, offset_y = 0,
                                scale_x = 1, scale_y = 1, scale_z = 1,
                                step = 100, freq=0.01/2, coef = 0.05, seed = 1) {
  ray_d = c(cospi(sun_altitude/180)*cospi(sun_angle/180),
            sinpi(sun_altitude/180),
            cospi(sun_altitude/180)*sinpi(sun_angle/180))
  nrow = nrow(heightmap)
  ncol = ncol(heightmap)
  xyz = as.matrix(expand.grid(x = 1:nrow - offset_x, y = 0, z = 1:ncol + offset_y))
  t_mat = (start_altitude_real - heightmap) / ray_d[2]
  if(ray_d[2] > 0) {
    step = 1/ray_d[2]
  } else {
    stop("Zero/negative sun altitudes are not valid")
  }
  if(ray_d[2] > 0) {
    step = 1/ray_d[2]
    real_step = 1/ray_d[2]
    
  } else {
    stop("Zero/negative sun altitudes are not valid")
  }

  #Offset x/y positions from heightmap to first altitude layer 
  xyz[,1] = xyz[,1] + ray_d[1] * t_mat
  xyz[,2] = 0
  xyz[,3] = xyz[,3] - ray_d[3] * t_mat
  
  atten = matrix(1, nrow, ncol)
  noise_height = start_noise
  
  #This starts at 0 because it needs to take into account bottom layer when calculating the shadow
  inc = 0
  while(noise_height < end_noise) {
    trans_mat = scales::rescale(gen_fractal_perlin(ray_d, xyz, t_mat = t_mat, 
                                                   altitude = noise_height,
                                                   levels = levels, 
                                                   inc = inc, seed = seed, freq = freq, time = time,
                                                   scale_x = scale_x, scale_y = scale_y, scale_z = scale_z),
                                to = c(alpha_coef, 1.0))
    trans_mat[is.na(trans_mat)] = 0
    trans_mat[trans_mat < 0] = 0
    atten = atten * (1 - coef * trans_mat)
    noise_height = noise_height + 1
    t_mat = t_mat + real_step
    inc = inc + 1
    if(step == 0) {
      break
    }
  }
  atten[atten < 0] = 0
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
#'@param scale_z Default `1`. Scale the fractal pattern in the z (altitude) direction. (automatically calculated). Scale the fractal pattern in the z (vertical) direction. s.
#'@param frequency Default `0.005`. The base frequency of the noise used to calculate the fractal cloud structure.
#'@param fractal_levels Default `16`. The fractal dimension used to calculate the noise. Higher values give more fine structure, but take longer to calculate.
#'@param attenuation_coef Default `1`. Amount of attenuation in the cloud (higher numbers give darker shadows). This value is automatically scaled to account for increasing the number of layers.
#'@param seed Default `1`. Random seed used to generate clouds.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. 
#'@return A 2D shadow matrix.
#'@export
#'@examples
#'if(run_documentation()) {
#'#Render clouds with cloud shadows on the ground
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  add_shadow(cloud_shade(montereybay,zscale=50), 0.0) %>%
#'  plot_3d(montereybay,background="darkred",zscale=50)
#'render_camera(theta=-65, phi = 25, zoom = 0.45, fov = 80)
#'render_clouds(montereybay, zscale=50)    
#'render_snapshot()
#'}
#'if(run_documentation()) {
#'#Adjust the light direction for shadows and increase the attenuation for darker clouds
#'montereybay  %>%
#'  sphere_shade()  %>%
#'  add_shadow(cloud_shade(montereybay,zscale=50, sun_altitude=20, attenuation_coef = 3), 0.0) %>%
#'  plot_3d(montereybay,background="darkred",zscale=50)
#'render_camera(theta=-65, phi = 25, zoom = 0.45, fov = 80)
#'render_clouds(montereybay, zscale=50)    
#'render_snapshot()
#'}
cloud_shade = function(heightmap, start_altitude = 1000, end_altitude=2000, 
                       sun_altitude = 90, sun_angle=315, time = 0,
                       cloud_cover = 0.5, layers = 10, offset_x = 0, offset_y = 0,
                       scale_x = 1, scale_y = 1, scale_z = 1,
                       frequency = 0.005, fractal_levels = 16,
                       attenuation_coef = 1, seed = 1, 
                       zscale=1) {
  time = -time
  if(start_altitude > end_altitude) {
    temp_alt = start_altitude
    start_altitude = end_altitude
    end_altitude = temp_alt
  }
  if(end_altitude != start_altitude) {
    scale_layers = layers/(end_altitude - start_altitude)
  } else {
    scale_layers = 1
    layers = 1
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
  
  attenuation_coef = attenuation_coef/layers
  if(layers == 1) {
    end_altitude = start_altitude
  }
  if(sun_altitude != 90) {
    scaled_angle = zscale * tanpi(sun_altitude/180)
    sun_altitude = atan(scaled_angle)*180/pi
  }
  return(flipud(raymarch_cloud_layer(heightmap = heightmap, coef = attenuation_coef, 
                              start_noise = 0, end_noise = (end_altitude-start_altitude)*scale_layers,
                              start_altitude_real = start_altitude, end_altitude_real = end_altitude,
                              time = time, 
                              sun_altitude = sun_altitude, alpha_coef = alpha_coef, 
                              sun_angle = sun_angle, levels = fractal_levels, layers = layers,
                              offset_x = offset_x, offset_y = offset_y,
                              scale_x = scale_x, scale_y = scale_y, scale_z = scale_z, 
                              seed = seed, freq = frequency)))
}
