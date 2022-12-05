#'@title Add Shadow
#'
#'@description Multiplies a texture array or shadow map by a shadow map. 
#'
#'@param hillshade A three-dimensional RGB array or 2D matrix of shadow intensities. 
#'@param shadowmap A matrix that incidates the intensity of the shadow at that point. 0 is full darkness, 1 is full light.
#'@param max_darken Default `0.7`. The lower limit for how much the image will be darkened. 0 is completely black,
#'1 means the shadow map will have no effect.
#'@param rescale_original Default `FALSE`. If `TRUE`, `hillshade` will be scaled to match the dimensions of `shadowmap` (instead of
#'the other way around).
#'@return Shaded texture map.
#'@export
#'@examples
#'#First we plot the sphere_shade() hillshade of `montereybay` with no shadows
#'
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  sphere_shade(colorintensity=0.5) %>%
#'  plot_map()
#'}
#'
#'#Raytrace the `montereybay` elevation map and add that shadow to the output of sphere_shade()
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  sphere_shade(colorintensity=0.5) %>%
#'  add_shadow(ray_shade(montereybay,sunaltitude=20,zscale=50),max_darken=0.3) %>%
#'  plot_map()
#'}
#'
#'#Increase the intensity of the shadow map with the max_darken argument.
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  sphere_shade(colorintensity=0.5) %>%
#'  add_shadow(ray_shade(montereybay,sunaltitude=20,zscale=50),max_darken=0.1) %>%
#'  plot_map()
#'}
#'
#'#Decrease the intensity of the shadow map.
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  sphere_shade(colorintensity=0.5) %>%
#'  add_shadow(ray_shade(montereybay,sunaltitude=20,zscale=50),max_darken=0.7) %>%
#'  plot_map()
#'}
add_shadow = function(hillshade, shadowmap, max_darken = 0.7, rescale_original = FALSE) {
  if(length(dim(shadowmap)) == 3 && length(dim(hillshade)) == 2) {
    tempstore = hillshade
    hillshade = shadowmap
    shadowmap = tempstore
  }
  if(length(dim(hillshade)) == 3) {
    hillshade = hillshade ^ 2.2
    if(!all(dim(hillshade)[1:2] == dim(shadowmap)[2:1])) {
      if(rescale_original) {
        temphillshade = array(0, dim = c(dim(shadowmap),3))
        temphillshade[,,1] = rayimage::render_resized(hillshade[,,1], dims = dim(shadowmap))
        temphillshade[,,2] = rayimage::render_resized(hillshade[,,2], dims = dim(shadowmap))
        temphillshade[,,3] = rayimage::render_resized(hillshade[,,3], dims = dim(shadowmap))
        hillshade = temphillshade
      } else {
        shadowmap = fliplr(rayimage::render_resized(t(shadowmap), dims = dim(hillshade)))
      }
    } else {
      shadowmap = t(flipud(shadowmap))
    }
    hillshade[,,1] = hillshade[,,1] * scales::rescale(shadowmap,c(max_darken,1))
    hillshade[,,2] = hillshade[,,2] * scales::rescale(shadowmap,c(max_darken,1))
    hillshade[,,3] = hillshade[,,3] * scales::rescale(shadowmap,c(max_darken,1))
    hillshade = hillshade ^ (1/2.2)
    hillshade
  } else if (length(dim(hillshade)) == 2) {
    if(any(hillshade > 1 | hillshade < 0)) {
      stop("Error: Not a shadow matrix. Intensities must be between 0 and 1. Pass your elevation matrix to ray_shade/lamb_shade/ambient_shade/sphere_shade first.")
    }
    hillshade = hillshade ^ 2.2
    hillshade = hillshade *  scales::rescale(shadowmap,c(max_darken,1))
    hillshade = hillshade ^ (1/2.2)
    hillshade
  } else {
    stop("length(dim(hillshade)) must be 2 or 3, not ", length(dim(hillshade)))
  }
}
