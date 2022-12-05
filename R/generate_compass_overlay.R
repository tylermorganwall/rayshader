#'@title Generate Compass Overlay
#'
#'@description This adds the compass
#'
#'Based on code from "Auxiliary Cartographic Functions in R: North Arrow, Scale Bar, and Label with a Leader Arrow"
#'
#'@param x Default `NULL`. The horizontal percentage across the map (measured from the bottom-left corner) where
#'the compass is located.
#'@param y Default `NULL`. The vertical percentage across the map (measured from the bottom-left corner) where
#'the compass is located.
#'@param size Default `0.05`. Size of the compass, in percentage of the map size..
#'@param text_size Default `1`. Text size.
#'@param bearing Default `0`. Angle (in degrees) of north.
#'@param color1 Default `white`. Primary color of the compass.
#'@param color2 Default `black`. Secondary color of the symcompass.
#'@param text_color Default `black`. Text color.
#'@param border_color Default `black`. Border color of the scale bar.
#'@param border_width Default `1`. Width of the scale bar border.
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting 
#'RGB image array automatically.
#'@param width Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param halo_color Default `NA`, no halo. If a color is specified, the compass will be surrounded by a halo
#'of this color.
#'@param halo_expand Default `1`. Number of pixels to expand the halo.
#'@param halo_alpha Default `1`. Transparency of the halo.
#'@param halo_offset Default `c(0,0)`. Horizontal and vertical offset to apply to the halo, in percentage of the image.
#'@param halo_blur Default `1`. Amount of blur to apply to the halo. Values greater than `30` won't result in further blurring.
#'@return Semi-transparent overlay with a compass.
#'@export
#'@examples
#'if(rayshader:::run_documentation()) {
#'#Create the water palette
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'
#'#Generate flat water heightmap
#'mbay = montereybay
#'mbay[mbay < 0] = 0
#'
#'base_map = mbay %>% 
#'  height_shade() %>% 
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_shadow(lamb_shade(montereybay,zscale=50),0.3)
#'  
#'#Plot a compass
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay)) %>% 
#'  plot_map()
#'}
#'
#'if(rayshader:::run_documentation()) {
#'#Change the position to be over the water
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15)) %>% 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the text color for visibility
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, text_color="white")) %>% 
#'  plot_map()
#'} 
#'if(rayshader:::run_documentation()) {
#'#Alternatively, add a halo color to improve contrast
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              halo_color="white", halo_expand = 1)) %>% 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Alternatively, add a halo color to improve contrast
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              halo_color="white", halo_expand = 1)) %>% 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the color scheme
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              halo_color="white", halo_expand = 1, color1 = "purple", color2 = "red")) %>% 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Remove the inner border
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              border_color=NA,
#'              halo_color="white", halo_expand = 1, 
#'              color1 = "darkolivegreen4", color2 = "burlywood3")) %>% 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the size of the compass and text
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.75, y=0.75,
#'              halo_color="white", halo_expand = 1, 
#'              size=0.075*2, text_size = 1.25)) %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.45, y=0.45,
#'              halo_color="white", halo_expand = 1, 
#'              size=0.075)) %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              halo_color="white", halo_expand = 1, 
#'              size=0.075/2, text_size = 0.75)) %>% 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the bearing of the compass
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.85, y=0.85,
#'              halo_color="white", halo_expand = 1, bearing=30,
#'              size=0.075)) %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.5, y=0.5,
#'              halo_color="white", halo_expand = 1, bearing=15,
#'              size=0.075)) %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              halo_color="white", halo_expand = 1, bearing=-45,
#'              size=0.075)) %>% 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Create a drop shadow effect
#'base_map %>% 
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              text_color="white", halo_alpha=0.5, halo_blur=2,
#'              halo_color="black", halo_expand = 1, halo_offset = c(0.003,-0.003))) %>% 
#'  plot_map()
#'}
generate_compass_overlay = function(x=0.85, y=0.15, 
                                    size=0.075, text_size=1, bearing=0,
                                    heightmap = NULL, width=NA, height=NA, 
                                    color1 = "white", color2 = "black", text_color = "black",
                                    border_color = "black", border_width = 1,
                                    halo_color = NA, halo_expand = 1,
                                    halo_alpha = 1, halo_offset = c(0,0), halo_blur = 1) {
  loc = rep(0,2)
  loc[1] = x
  loc[2] = y
  if(is.na(height)) {
    height  = ncol(heightmap)
  }
  if(is.na(width)) {
    width  = nrow(heightmap)
  }
  # default colors are white and black
  cols <- rep(c(color1,color2),8)
  bearing = bearing*pi/180
  # calculating coordinates of polygons
  radii <- rep(size/c(1,4,2,4),4)
  x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
  y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
  # drawing polygons
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  graphics::plot(x=c(0,1),y=c(0,1), xlim = c(0,1),
       ylim =  c(0,1), asp=1, pch = 0,bty="n",axes=FALSE,
       xaxs = "i", yaxs = "i", cex = 0, col = NA)
  for (i in 1:15) {
    x1 <- c(x[i],x[i+1],loc[1])
    y1 <- c(y[i],y[i+1],loc[2])
    graphics::polygon(x1,y1,col=cols[i], 
                      border = border_color, lwd = border_width)
  }
  # drawing the last polygon
  graphics::polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16],
                    border = border_color, lwd = border_width)
  # drawing letters
  b <- c("E","N","W","S") 
  for (i in 0:3) {
    graphics::text((size+graphics::par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
         (size+graphics::par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],
         cex=text_size,col=text_color)
  }
  grDevices::dev.off() #resets par
  overlay_temp = png::readPNG(tempoverlay)
  if(!is.na(halo_color)) {
    if(!(length(find.package("rayimage", quiet = TRUE)) > 0)) {
      stop("{rayimage} package required for `halo_color`")
    }
    tempoverlay = tempfile(fileext = ".png")
    grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
    graphics::par(mar = c(0,0,0,0))
    graphics::plot(x=c(0,1),y=c(0,1), xlim = c(0,1),
         ylim =  c(0,1), asp=1, pch = 0,bty="n",axes=FALSE,
         xaxs = "i", yaxs = "i", cex = 0, col = NA)
    for (i in 1:15) {
      x1 <- c(x[i],x[i+1],loc[1])
      y1 <- c(y[i],y[i+1],loc[2])
      graphics::polygon(x1+halo_offset[1],y1+halo_offset[2],col=cols[i], 
                        border = border_color, lwd = border_width)
    }
    # drawing the last polygon
    graphics::polygon(c(x[16],x[1],loc[1])+halo_offset[1],
                      c(y[16],y[1],loc[2])+halo_offset[2],col=cols[16],
                      border = border_color, lwd = border_width)
    # drawing letters
    b <- c("E","N","W","S") 
    for (i in 0:3) {
      graphics::text((size+graphics::par("cxy")[1])*cos(bearing+i*pi/2)+loc[1]+halo_offset[1],
           (size+graphics::par("cxy")[2])*sin(bearing+i*pi/2)+loc[2]+halo_offset[2],b[i+1],
           cex=text_size,col=text_color)
    }
    grDevices::dev.off() #resets par
    overlay_temp_under = png::readPNG(tempoverlay)
    if(halo_expand != 0 || any(halo_offset != 0)) {
      temp_alpha = overlay_temp_under[,,4]
      temp_alpha[temp_alpha > 0] = 1
      booldistance = rayimage::render_boolean_distance(temp_alpha)
      booldistance = booldistance - halo_expand
      temp_alpha[booldistance <= 0] = 1
      temp_alpha[booldistance < 1 & booldistance > 0] = 1 - booldistance[booldistance < 1 & booldistance > 0]
      temp_alpha[booldistance > 1] = 0
      col_below = convert_color(halo_color)
      temp_array = array(0, dim = dim(overlay_temp_under))
      temp_array[,,1] = col_below[1]
      temp_array[,,2] = col_below[2]
      temp_array[,,3] = col_below[3]
      temp_array[,,4] = temp_alpha * halo_alpha
      overlay_temp_under = temp_array
    }
    if(halo_blur > 0) {
      overlay_temp_under = rayimage::render_convolution(overlay_temp_under, 
                                                        kernel = rayimage::generate_2d_gaussian(sd = halo_blur, dim = 31,
                                                                                                width = 30),
                                                        progress = FALSE)
    }
    return(add_overlay(overlay_temp_under, overlay_temp))
  }
  return(overlay_temp)
}
