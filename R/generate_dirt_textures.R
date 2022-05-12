#'@title Generate Dirt Textures
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param heightmap Height matrix;
#'@param color1 Default `tan`.
#'@param color2 Default `brown`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@keywords internal
generate_dirt_textures = function(heightmap, base_depth = 10, color1= "#7d6f5b", color2="#3b3020", freq=0.1,
                                  zscale=1, levels=8, dirt_gradient = 0, gradient_darken = 4) {
  if(all(length(find.package("ambient", quiet = TRUE)) == 0)) {
    stop("textured dirt requires the `ambient` package to be installed")
  }
  full_range = max(c(base_depth,10))
  asp = 1
  if(floor(full_range) > 500) {
    asp = 500/floor(full_range)
  }
  nr = floor(nrow(heightmap)*asp)
  nc = floor(ncol(heightmap)*asp)
  dirt_array1 =  height_shade(ambient::noise_perlin(dim=c(nr,floor(full_range) * asp),
                                               frequency = freq, 
                                               octaves = levels),
                              texture = grDevices::colorRampPalette(c(color1,color2))(256)) 
  dirt_array2 =  height_shade(ambient::noise_perlin(dim=c(nc,floor(full_range) * asp),
                                                    frequency = freq, 
                                                    octaves = levels),
                              texture = grDevices::colorRampPalette(c(color1,color2))(256)) 
  if(dirt_gradient > 0) {
    color2 = convert_color(darken_color(color2, darken=1/gradient_darken))
    grad_dirt1 = array(0,dim = c(nr,floor(full_range)*asp,4))
    grad_dirt1[,,1] = color2[1]
    grad_dirt1[,,2] = color2[2]
    grad_dirt1[,,3] = color2[3]
    grad_dirt1[,,4] = matrix(1-seq(0,1,length.out=nr)^(dirt_gradient),nrow=nr, ncol = floor(full_range)*asp)
    
    
    dirt_array1 = add_overlay(dirt_array1, grad_dirt1)
  
    grad_dirt2 = array(0,dim = c(nc,floor(full_range)*asp,4))
    grad_dirt2[,,1] = color2[1]
    grad_dirt2[,,2] = color2[2]
    grad_dirt2[,,3] = color2[3]
    grad_dirt2[,,4] = matrix(1-seq(0,1,length.out=nc)^(dirt_gradient),nrow=nc, ncol = floor(full_range)*asp)
    
    dirt_array2 = add_overlay(dirt_array2, grad_dirt2)
  }
  
  tempfile1 = tempfile(fileext = ".png")
  tempfile2 = tempfile(fileext = ".png")
  
  png::writePNG(dirt_array1, tempfile1)
  png::writePNG(dirt_array2, tempfile2)
  return(c(tempfile1,tempfile2))
}
