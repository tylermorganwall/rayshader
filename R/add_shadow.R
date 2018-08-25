#'@title Add Shadow
#'
#'@description Multiplies a texture array or shadow map by a shadow map. 
#'
#'@param hillshade A three-dimensional RGB array or 2D matrix of shadow intensities. 
#'@param shadowmap A matrix that incidates the intensity of the shadow at that point. 0 is full darkness, 1 is full light.
#'@param max_darken Default 0.7. The lower limit for how much the image will be darkened. 0 is completely black,
#'1 means the shadow map will have no effect.
#'@return Shaded texture map.
#'@export
#'@examples
#'#Raytrace the `volcano` elevation map and add that shadow to the output of sphere_shade()
#'shadowmap = ray_shade(volcano)
#'
#'volcano %>%
#'  sphere_shade() %>%
#'  add_shadow(shadowmap) %>%
#'  plot_map()
#'  
#'#Increase the intensity of the shadow:
#'volcano %>%
#'  sphere_shade() %>%
#'  add_shadow(shadowmap,0.3) %>%
#'  plot_map()
add_shadow = function(hillshade, shadowmap, max_darken = 0.7) {
  if(class(shadowmap) == "array" && class(hillshade) == "matrix") {
    tempstore = hillshade
    hillshade = shadowmap
    shadowmap = tempstore
  }
  shadowmap = t(shadowmap[,ncol(shadowmap):1])
  if(class(hillshade) == "array") {
    hillshade[,,1] = hillshade[,,1] * scales::rescale(shadowmap,c(max_darken,1))
    hillshade[,,2] = hillshade[,,2] * scales::rescale(shadowmap,c(max_darken,1))
    hillshade[,,3] = hillshade[,,3] * scales::rescale(shadowmap,c(max_darken,1))
    hillshade
  } else if (class(hillshade) == "matrix") {
    if(any(hillshade > 1 | hillshade < 0)) {
      stop("Error: Not a shadow matrix. Intensities must be between 0 and 1. Pass your elevation matrix to ray_shade/lamb_shade/ambient_shade/sphere_shade first.")
    }
    hillshade * t(scales::rescale(shadowmap[nrow(shadowmap):1,],c(max_darken,1)))
  }
}