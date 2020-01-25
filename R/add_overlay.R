#'@title Add Overlay
#'
#'@description Overlays an image (with a transparency layer) on the current map.
#'
#'@param hillshade A three-dimensional RGB array or 2D matrix of shadow intensities. 
#'@param overlay A three or four dimensional RGB array, where the 4th dimension represents the alpha (transparency) channel. 
#'If the array is 3D, `alphacolor` should also be passed to indicate transparent regions.
#'@param alphacolor Default `NULL`. If `overlay` is a 3-layer array, this argument tells which color is interpretted as completely transparent.
#'@param alphalayer Default `1`. Defines minimum tranparaency of layer. If transparency already exists in `overlay`, the way `add_overlay` combines 
#'the two is determined in argument `alphamethod`.
#'@param alphamethod Default `max`. Method for dealing with pre-existing transparency with `layeralpha`. 
#'If `max`, converts all alpha levels higher than `layeralpha` to the value set in `layeralpha`.
#'If `multiply`, multiples all pre-existing alpha values with `layeralpha`.
#'If `none`, keeps existing tranparencies and only changes opaque entries.
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@return Hillshade with overlay.
#'@export
#'@examples
#'#Here, we overlay a base R elevation plot with our raytraced shadow layer:
#'
#'fliplr = function(x) {
#'  x[,ncol(x):1]
#'}
#'\dontrun{
#'tempfilename = tempfile()
#'old.par = par(no.readonly = TRUE)
#'on.exit(par(old.par))
#'png(tempfilename,width = 401,height=401)
#'par(mar = c(0,0,0,0))
#'raster::image(fliplr(montereybay),axes = FALSE,col = rev(terrain.colors(1000)))
#'dev.off()
#'tempmap = png::readPNG(tempfilename)
#'}
#' 
#'\dontrun{
#'montereybay %>%
#'   ray_shade(zscale=50,maxsearch = 500,anglebreaks = seq(20,30,0.1)) %>%
#'   add_overlay(tempmap,alphalayer = 0.5) %>%
#'   plot_map()
#'}
#'
#'#Combining base R plotting with rayshader's spherical color mapping and raytracing:
#'\dontrun{
#'montereybay %>%
#'   sphere_shade() %>%
#'   add_overlay(tempmap,alphalayer = 0.4) %>%
#'   add_shadow(ray_shade(montereybay,zscale=50,maxsearch = 500)) %>%
#'   plot_map()
#'}
add_overlay = function(hillshade, overlay, alphacolor=NULL, alphalayer = 1, alphamethod = "max", 
                       gamma_correction = TRUE) {
  if((length(dim(hillshade)) == 2) && any(dim(hillshade) != dim(overlay)[2:1])) {
    if("magick" %in% rownames(utils::installed.packages())) {
      resized_overlay_file = tempfile(fileext=".png")
      resized_overlay_file2 = tempfile(fileext=".png")
      png::writePNG(overlay,resized_overlay_file)
      magick::image_read(resized_overlay_file) %>%
        magick::image_resize(paste0(c(dim(hillshade)[2],"x",dim(hillshade)[1],"!"),collapse="")) %>%
        magick::image_write(resized_overlay_file2)
      overlay = png::readPNG(resized_overlay_file2)
      if(length(dim(overlay)) != 3) {
        tempoverlay = array(0, dim = c(dim(overlay)[1],dim(overlay)[2],3))
        tempoverlay[,,1] = overlay
        tempoverlay[,,2] = overlay
        tempoverlay[,,3] = overlay
        overlay = tempoverlay
      }
      warning("Overlay doesn't match dimension of hillshade--resizing to match")
    } else {
      stop("Overlay doesn't match dimension of hillshade.",
           "`magick` package required for auto-resizing of overlay.",
           "Resize overlay manually or install `magick` to continue.")
    }
  }
  if((length(dim(hillshade)) == 3) && any(dim(hillshade)[1:2] != dim(overlay)[1:2])) {
    if("magick" %in% rownames(utils::installed.packages())) {
      resized_overlay_file = tempfile(fileext=".png")
      resized_overlay_file2 = tempfile(fileext=".png")
      png::writePNG(overlay,resized_overlay_file)
      magick::image_read(resized_overlay_file) %>%
        magick::image_resize(paste0(c(dim(hillshade)[2],"x",dim(hillshade)[1],"!"),collapse="")) %>%
        magick::image_write(resized_overlay_file2)
      overlay = png::readPNG(resized_overlay_file2)
      if(length(dim(overlay)) != 3) {
        tempoverlay = array(0, dim = c(dim(overlay)[1],dim(overlay)[2],3))
        tempoverlay[,,1] = overlay
        tempoverlay[,,2] = overlay
        tempoverlay[,,3] = overlay
        overlay = tempoverlay
      }
      warning("Overlay doesn't match dimension of hillshade--resizing to match")
    } else {
      stop("Overlay doesn't match dimension of hillshade.",
           "`magick` package required for auto-resizing of overlay.",
           "Resize overlay manually or install `magick` to continue.")
    }
  }
  if(any(alphalayer > 1 || alphalayer < 0)) {
    stop("Argument `alphalayer` must not be less than 0 or more than 1")
  }
  if(length(dim(hillshade)) == 2) {
    if(any(hillshade > 1 | hillshade < 0)) {
      stop("Error: Not a shadow matrix. Intensities must be between 0 and 1. Pass your elevation matrix to ray_shade/lamb_shade/ambient_shade/sphere_shade first.")
    }
    temp = array(0,dim = c(nrow(hillshade),ncol(hillshade),3))
    temp[,,1] = flipud(hillshade)
    temp[,,2] = flipud(hillshade)
    temp[,,3] = flipud(hillshade)
    hillshade = aperm(temp,c(2,1,3))
  } 
  if(gamma_correction) {
    hillshade = hillshade ^ 2.2
  }
  overlay[,,1:3] = overlay[,,1:3] ^ 2.2
  if((dim(overlay)[3] == 3 || (dim(overlay)[3] == 4 && all(overlay[,,4] == 1))) && alphalayer == 1) {
    if(is.null(alphacolor)) {
      stop("If `overlay` array is only 3D (or is completely opaque), argument `alphacolor` must be defined")
    }
    colorvals = col2rgb(alphacolor)/255
    alphalayer1 = overlay[,,1] == colorvals[1] & overlay[,,2] == colorvals[2] & overlay[,,3] == colorvals[3]
    for(i in 1:dim(hillshade)[1]) {
      for(j in 1:dim(hillshade)[2]) {
        if(!alphalayer1[j,i]) {
          hillshade[i,j,1] = overlay[j,i,1]
          hillshade[i,j,2] = overlay[j,i,2]
          hillshade[i,j,3] = overlay[j,i,3]
        }
      }
    }
    if(gamma_correction) {
      hillshade = hillshade^(1/2.2)
    }
    return(hillshade)
  }
  
  if(dim(overlay)[3] == 4 || alphalayer != 1) {
    if(dim(overlay)[3] == 3) {
      temp = array(alphalayer,dim = c(nrow(overlay),ncol(overlay),4))
      temp[,,1:3] = overlay[,,1:3]
      overlay = temp
    }
    if(alphalayer != 1) {
      if(alphamethod == "max") {
        alphamat = overlay[,,4]
        alphamat[alphamat > alphalayer] = alphalayer
        overlay[,,4] = alphamat
      } else if (alphamethod == "multiply") {
        overlay[,,4] =  overlay[,,4] * alphalayer
      } else if (alphamethod == "none") {
        alphamat = overlay[,,4]
        alphamat[alphamat == 1] = alphalayer
        overlay[,,4] = alphamat
      }
    }
    if(!is.null(alphacolor)) {
      colorvals = col2rgb(alphacolor)/255
      alphalayer1 = overlay[,,1] == colorvals[1] & overlay[,,2] == colorvals[2] & overlay[,,3] == colorvals[3]
      overlay[,,4][alphalayer1] = 0
    }
    if(any(overlay[,,4] > 1) | any(overlay[,,4] < 0) ) {
      stop("Alpha channel in `overlay` can't be greater than 1 or less than 0")
    }
    hillshade[,,1] = hillshade[,,1] * (1 - overlay[,,4]) + overlay[,,1] * overlay[,,4]
    hillshade[,,2] = hillshade[,,2] * (1 - overlay[,,4]) + overlay[,,2] * overlay[,,4]
    hillshade[,,3] = hillshade[,,3] * (1 - overlay[,,4]) + overlay[,,3] * overlay[,,4]
    hillshade[hillshade > 1] = 1
    if(gamma_correction) {
      hillshade = hillshade^(1/2.2)
    }
    return(hillshade)
  }
}
