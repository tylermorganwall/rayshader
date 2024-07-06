#'@title Generate Soil Textures
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param heightmap Height matrix;
#'@param color1 Default `tan`.
#'@param color2 Default `brown`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@keywords internal
generate_soil_textures = function(heightmap, base_depth = 10, color1= "#7d6f5b", color2="#3b3020", freq=0.1,
                                  zscale=1, levels=8, soil_gradient = 0, gradient_darken = 4) {
  if(all(length(find.package("ambient", quiet = TRUE)) == 0)) {
    stop("textured soil requires the `ambient` package to be installed")
  }
  full_range = max(c(base_depth,10))
  asp = 1
  if(floor(full_range) > 500) {
    asp = 500/floor(full_range)
  }
  nr = floor(nrow(heightmap)*asp)
  nc = floor(ncol(heightmap)*asp)
  soil_array1 =  height_shade(ambient::noise_perlin(dim=c(nr,floor(full_range) * asp),
                                               frequency = freq, 
                                               octaves = levels),
                              texture = grDevices::colorRampPalette(c(color1,color2))(256)) 
  soil_array2 =  height_shade(ambient::noise_perlin(dim=c(nc,floor(full_range) * asp),
                                                    frequency = freq, 
                                                    octaves = levels),
                              texture = grDevices::colorRampPalette(c(color1,color2))(256)) 
  if(soil_gradient > 0) {
    color2 = convert_color(darken_color(color2, darken=1/gradient_darken))
    grad_soil1 = array(0,dim = c(nr,floor(full_range)*asp,4))
    grad_soil1[,,1] = color2[1]
    grad_soil1[,,2] = color2[2]
    grad_soil1[,,3] = color2[3]
    grad_soil1[,,4] = matrix(1-seq(0,1,length.out=nr)^(soil_gradient),nrow=nr, ncol = floor(full_range)*asp)
    
    
    soil_array1 = add_overlay(soil_array1, grad_soil1)
  
    grad_soil2 = array(0,dim = c(nc,floor(full_range)*asp,4))
    grad_soil2[,,1] = color2[1]
    grad_soil2[,,2] = color2[2]
    grad_soil2[,,3] = color2[3]
    grad_soil2[,,4] = matrix(1-seq(0,1,length.out=nc)^(soil_gradient),nrow=nc, ncol = floor(full_range)*asp)
    
    soil_array2 = add_overlay(soil_array2, grad_soil2)
  }
  
  tempfile1 = tempfile(fileext = ".png")
  tempfile2 = tempfile(fileext = ".png")
  
  png::writePNG(soil_array1, tempfile1)
  png::writePNG(soil_array2, tempfile2)
  return(c(tempfile1,tempfile2))
}
