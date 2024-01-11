#'@title Add Overlay
#'
#'@description Overlays an image (with a transparency layer) on the current map.
#'
#'@param hillshade A three-dimensional RGB array or 2D matrix of shadow intensities. 
#'@param overlay A three or four dimensional RGB array, where the 4th dimension represents the alpha (transparency) channel. 
#'If the array is 3D, `alphacolor` should also be passed to indicate transparent regions.
#'@param alphalayer Default `1`. Defines minimum tranparaency of layer. If transparency already exists in `overlay`, the way `add_overlay` combines 
#'the two is determined in argument `alphamethod`.
#'@param alphacolor Default `NULL`. If `overlay` is a 3-layer array, this argument tells which color is interpretted as completely transparent.
#'@param alphamethod Default `max`. Method for dealing with pre-existing transparency with `layeralpha`. 
#'If `max`, converts all alpha levels higher than `layeralpha` to the value set in `layeralpha`. Otherwise,
#'this just sets all transparency to `layeralpha`.
#'@param rescale_original Default `FALSE`. If `TRUE`, `hillshade` will be scaled to match the dimensions of `overlay` (instead of
#'the other way around).
#'@return Hillshade with overlay.
#'@export
#'@examples
#'#Combining base R plotting with rayshader's spherical color mapping and raytracing:
#'if(run_documentation()) {
#'montereybay %>%
#'   sphere_shade() %>%
#'   add_overlay(height_shade(montereybay),alphalayer = 0.6)  %>%
#'   add_shadow(ray_shade(montereybay,zscale=50)) %>%
#'   plot_map()
#'}
#'
#'if(run_documentation()) {
#'#Add contours with `generate_contour_overlay()`
#'montereybay %>%
#'   height_shade() %>%
#'   add_overlay(generate_contour_overlay(montereybay))  %>%
#'   add_shadow(ray_shade(montereybay,zscale=50)) %>%
#'   plot_map()
#'}
add_overlay = function(hillshade = NULL, overlay = NULL, alphalayer = 1, alphacolor=NULL, 
                       alphamethod = "max", rescale_original = FALSE) {
  if(any(alphalayer > 1 || alphalayer < 0)) {
    stop("Argument `alphalayer` must not be less than 0 or more than 1")
  }
  if(inherits(overlay,"character")) {
    overlay = png::readPNG(overlay)
    if(length(dim(overlay)) == 3) {
      if(dim(overlay)[3] == 3) {
        temparray = array(0,dim = c(nrow(overlay),ncol(overlay),4))
        temparray[,,1] = overlay[,,1]
        temparray[,,2] = overlay[,,2]
        temparray[,,3] = overlay[,,3]
        temparray[,,4] = alphalayer[1]
        overlay = temparray
      }
    }
  } else {
    if(length(dim(overlay)) == 3) {
      if(dim(overlay)[3] == 3) {
        temparray = array(0,dim = c(nrow(overlay),ncol(overlay),4))
        temparray[,,1] = overlay[,,1]
        temparray[,,2] = overlay[,,2]
        temparray[,,3] = overlay[,,3]
        temparray[,,4] = alphalayer[1]
        overlay = temparray
      }
    }
  }
  if(alphalayer != 1) {
    if(alphamethod == "max") {
      overlay[,,4][overlay[,,4] > alphalayer] = alphalayer
    } else  {
      overlay[,,4] = alphalayer
    }
  }
  if(!is.null(alphacolor)) {
    colorvals = col2rgb(alphacolor)/255
    alphalayer1 = overlay[,,1] == colorvals[1] & overlay[,,2] == colorvals[2] & overlay[,,3] == colorvals[3]
    temp_over = overlay[,,4]
    temp_over[alphalayer1] = 0
    overlay[,,4] = temp_over
  }
  if(is.null(hillshade)) {
    return(overlay)
  }
  if(is.null(overlay)) {
    return(hillshade)
  }
  if(length(dim(hillshade)) == 2) {
    rayimage::add_image_overlay(fliplr(t(hillshade)),overlay,alpha=alphalayer, rescale_original = rescale_original)
  } else {
    rayimage::add_image_overlay(hillshade,overlay,alpha=alphalayer, rescale_original = rescale_original)
  }
}
