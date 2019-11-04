#'@title Make Vignette Overlay
#'
#'@description Makes an overlay to simulate vignetting in a camera
#'
#'@param width Width of the image.
#'@param height Height of the image.
#'@param intensity Default `0.4`. `1` is max intensity, `0` is min.
#'@param radius Default `NULL`. Max of height or width, divided by 2.
#'@keywords internal
make_vignette_overlay = function(width, height, intensity=0.3, radius=NULL) {
  value = array(0, dim = c(width,height,4))
  value[,,4] = gen_ellipse(intensity, width, height)
  tempcircle = tempfile(fileext = "png")
  png::writePNG(value,tempcircle)
  imageval = magick::image_read(tempcircle)
  imageval %>%
    magick::image_border(paste0("#000000",as.hexmode(as.integer(intensity*255))), paste0(radius,"x",radius)) %>%
    magick::image_blur(radius=radius/2, sigma = radius/4) %>% 
    magick::image_crop(paste0(magick::image_info(imageval)$width,"x",
                              magick::image_info(imageval)$height,"+",radius,"+",radius)) %>% 
    magick::image_write(tempcircle)
  return(tempcircle)
}