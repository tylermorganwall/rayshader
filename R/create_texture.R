#'@title Create Texture
#'
#'@description Creates a texture map based on 5 user-supplied colors.
#'
#'@param lightcolor The main highlight color. Corresponds to the top center of the texture map.
#'@param shadowcolor The main shadow color. Corresponds to the bottom center of the texture map. This color represents slopes directed 
#'directly opposite to the main highlight color.
#'@param leftcolor The left fill color. Corresponds to the left center of the texture map. This color represents slopes directed 
#'90 degrees to the left of the main highlight color.
#'@param rightcolor The right fill color. Corresponds to the right center of the texture map. This color represents slopes directed 
#'90 degrees to the right of the main highlight color.
#'@param centercolor The center color. Corresponds to the center of the texture map. This color represents flat areas.
#'@param cornercolors Default `NULL`. The colors at the corners, in this order: NW, NE, SW, SE. If this vector isn't present (or
#'all corners are specified), the mid-points will just be interpolated from the main colors.
#'@export
#'@examples
#'#Here is the `imhof1` palette:
#'create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9") %>%
#'  plot_map()
#'
#'#Here is the `unicorn` palette:
#'create_texture("red","green","blue","yellow","white") %>%
#'  plot_map()
create_texture = function(lightcolor,shadowcolor,leftcolor,rightcolor,centercolor,cornercolors=NULL) {
  lightrgb = col2rgb(lightcolor)
  shadowrgb = col2rgb(shadowcolor)
  leftrgb = col2rgb(leftcolor)
  rightrgb = col2rgb(rightcolor)
  centerrgb = col2rgb(centercolor)
  if(is.null(cornercolors) || length(cornercolors) != 4) {
    nw_corner = (lightrgb+leftrgb)/2
    ne_corner = (lightrgb+rightrgb)/2
    sw_corner = (shadowrgb+leftrgb)/2
    se_corner = (shadowrgb+rightrgb)/2
  } else {
    cornercolorsrgb = lapply(cornercolors,col2rgb)
    nw_corner = cornercolorsrgb[[1]]
    ne_corner = cornercolorsrgb[[2]]
    se_corner = cornercolorsrgb[[3]]
    sw_corner = cornercolorsrgb[[4]]
  }
  colorarray = array(0,dim=c(3,3,3))
  for(i in 1:3) {
    #center
    colorarray[2,2,i] = centerrgb[i]
    
    #edges
    colorarray[1,2,i] = lightrgb[i]
    colorarray[2,1,i] = leftrgb[i]
    colorarray[2,3,i] = rightrgb[i]
    colorarray[3,2,i] = shadowrgb[i]
    
    #corners
    colorarray[1,1,i] = nw_corner[i]
    colorarray[1,3,i] = ne_corner[i]
    colorarray[3,1,i] = se_corner[i]
    colorarray[3,3,i] = sw_corner[i]
  }
  returnarray = array(0,dim=c(512,512,3))
  for(i in 1:3) {
    returnarray[,,i] = bilineargrid(colorarray[,,i]/256)
  }
  returnarray
}
