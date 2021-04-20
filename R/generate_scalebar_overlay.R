#'@title Generate Scalebar Overlay
#'
#'@description This function creates an overlay with a scale bar of a user-specified length. 
#'It uses the coordinates of the map (specified by passing an extent) 
#'and then creates a scale bar at a specified x/y proportion across the map. If the map is not projected
#'(i.e. is in lat/long coordinates) this function will use the `geosphere` package to create a 
#'scale bar of the proper length.
#'
#'@param extent A `raster::Extent` object with the bounding box for the height map used to generate the original map. If this is in
#'lat/long coordinates, be sure to set `latlong = TRUE`.
#'@param length The length of the scale bar, in `units`. This should match the units used on the map, 
#'unless `extent` uses lat/long coordinates. In that case, the distance should be in meters.
#'@param x Default `0.05`. The x-coordinate of the bottom-left corner of the scale bar, as a proportion of the full map width. 
#'@param y Default `0.05`. The y-coordinate of the bottom-left corner of the scale bar, as a proportion of the full map height. 
#'@param latlong Default `FALSE`. Set to `TRUE` if the map is in lat/long coordinates to get an accurate
#'scale bar (using distance calculated with the `geosphere` package).
#'@param thickness Default `NA`, automatically computed as 1/20th the length of the scale bar. Width of the scale bar.
#'@param bearing Default `90`, horizontal. Direction (measured from north) of the scale bar.
#'@param unit Default `m`. Displayed unit on the scale bar.
#'@param labels Default `NA`. Manually specify the three labels with a length-3 character vector. 
#'Use this if you want display units other than meters.
#'@param flip_ticks Default `FALSE`. Whether to flip the ticks to the other side of the scale bar.
#'@param text_size Default `1`. Text size.
#'@param decimals Default `0`. Number of decimal places for scale bar labels.
#'@param text_offset Default `1`. Amount of offset to apply to the text from the scale bar, as a multiple of
#'`thickness`.
#'@param adj Default `0.5`, centered. Text justification. `0` is left-justified, and `1` is right-justified.
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting 
#'RGB image array automatically.
#'@param width Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param color1 Default `black`. Primary color of the scale bar.
#'@param color2 Default `white`. Secondary color of the scale bar.
#'@param text_color Default `black`. Text color.
#'@param font Default `1`. An integer which specifies which font to use for text. 
#'If possible, device drivers arrange so that 1 corresponds to plain text (the default), 
#'2 to bold face, 3 to italic and 4 to bold italic. 
#'@param border_color Default `black`. Border color of the scale bar.
#'@param tick_color Default `black`. Tick color of the scale bar.
#'@param border_width Default `1`. Width of the scale bar border.
#'@param tick_width Default `1`. Width of the tick.
#'@param halo_color Default `NA`, no halo. If a color is specified, the text label will be surrounded by a halo
#'of this color.
#'@param halo_expand Default `1`. Number of pixels to expand the halo.
#'@param halo_alpha Default `1`. Transparency of the halo.
#'@param halo_offset Default `c(0,0)`. Horizontal and vertical offset to apply to the halo, as a proportion of the full scene.
#'@param halo_blur Default `1`. Amount of blur to apply to the halo. Values greater than `30` won't result in further blurring.
#'@return Semi-transparent overlay with a scale bar.
#'@export
#'@examples
#'#Only run these examples if the `magick` package is installed.
#'if ("magick" %in% rownames(utils::installed.packages())) {
#'\donttest{
#'#Create the water palette
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'
#'#Generate flat water heightmap
#'mbay = montereybay
#'mbay[mbay < 0] = 0
#'
#'base_map = mbay %>%
#' height_shade() %>%
#' add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#' add_shadow(lamb_shade(montereybay,zscale=50),0.3)
#'
#'#For convenience, the extent of the montereybay dataset is included as an attribute
#'mb_extent = attr(montereybay, "extent")
#'
#'#Add a scalebar
#'base_map %>%
#' add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 40000,
#'                                       heightmap = montereybay, 
#'                                       latlong=TRUE)) %>%
#' plot_map()
#'
#'
#'#Change the text color
#'base_map %>%
#'  add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 40000,
#'                                        text_color = "white",
#'                                        heightmap = montereybay, 
#'                                        latlong=TRUE)) %>%
#'  plot_map()
#'
#'
#'#Change the length
#'base_map %>%
#'  add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 30000,
#'                                        text_color = "white",
#'                                        heightmap = montereybay, 
#'                                        latlong=TRUE)) %>%
#'  plot_map()
#'
#'#Change the thickness (default is length/20)
#'base_map %>%
#'  add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 30000,
#'                                        text_color = "white", thickness = 30000/10,
#'                                        heightmap = montereybay, 
#'                                        latlong=TRUE)) %>%
#'  plot_map()
#'
#'#Change the text offset (given in multiples of thickness)
#'base_map %>%
#'  add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 30000,
#'                                        text_color = "white", thickness = 30000/10,
#'                                        text_offset = 0.75,
#'                                        heightmap = montereybay, 
#'                                        latlong=TRUE)) %>%
#'  plot_map()
#'
#'#Change the primary and secondary colors, along with the border and tick color
#'base_map %>%
#'  add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 30000,
#'                                        text_color = "white", border_color = "white",
#'                                        tick_color = "white",
#'                                        color1 = "darkolivegreen4", color2 = "burlywood3",
#'                                        heightmap = montereybay, 
#'                                        latlong=TRUE)) %>%
#'  plot_map()
#'
#'#Add a halo
#'base_map %>%
#'  add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 40000,
#'                                        halo_color = "white", halo_expand = 1,
#'                                        heightmap = montereybay, 
#'                                        latlong=TRUE)) %>%
#'  plot_map()
#'
#'#Change the orientation, position, text alignment, and flip the ticks to the other side
#'base_map %>%
#'  add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 40000, x = 0.07,
#'                                        bearing=0, adj = 0, flip_ticks = TRUE,
#'                                        halo_color = "white", halo_expand = 1.5,
#'                                        heightmap = montereybay, 
#'                                        latlong=TRUE)) %>%
#'  plot_map()
#'  
#'  
#'#64373.8 meters in 40 miles
#'#Create custom labels, change font and text size, remove the border/ticks, and change the color
#'#Here, we specify a width and height to double the resolution of the image (for sharper text)
#'base_map %>%
#'  add_overlay(generate_scalebar_overlay(extent = mb_extent, length = 64373.8, x = 0.07,
#'                                        labels = c("0", "20", "40 miles"), thickness=2500,
#'                                        text_size=3, font = 2, text_offset = 0,
#'                                        text_color="white", color2="#bf323b", border_color=NA,
#'                                        tick_color="red", tick_width=0,
#'                                        bearing=0, adj = 0, flip_ticks = TRUE,
#'                                        halo_color="black", halo_blur=3, halo_alpha=0.5,
#'                                        width = ncol(montereybay)*2,
#'                                        height = nrow(montereybay)*2,
#'                                        latlong=TRUE), rescale_original=TRUE) %>%
#'  plot_map()
#'}
#'}
generate_scalebar_overlay = function(extent, length, x=0.05, y=0.05, 
                                     latlong = FALSE, thickness = NA,
                                     bearing=90, unit="m", flip_ticks = FALSE,
                                     labels = NA, text_size=1, decimals = 0, 
                                     text_offset = 1, adj = 0.5,
                                     heightmap = NULL, width=NA, height=NA,
                                     color1 = "white", color2 = "black", 
                                     text_color = "black", font = 1,
                                     border_color = "black", tick_color = "black", 
                                     border_width = 1, tick_width = 1,
                                     halo_color = NA, halo_expand = 1,
                                     halo_alpha = 1, halo_offset = c(0,0), halo_blur = 1) {
  loc = rep(0,2)
  loc[1] = x * (extent[2]-extent[1]) + extent[1]
  loc[2] = y * (extent[4]-extent[3]) + extent[3]
  
  halo_offset[1] = halo_offset[1] * (extent[2]-extent[1])
  halo_offset[2] = halo_offset[2] * (extent[4]-extent[3])
  
  if(is.na(height)) {
    height  = ncol(heightmap)
  }
  if(is.na(width)) {
    width  = nrow(heightmap)
  }
  if(all(!is.na(labels)) && length(labels) != 3) {
    stop("If specified, `labels` must be length-3 vector")
  }

  proj_length = length
  if(is.na(thickness)) {
    thickness = proj_length/20
  }
  poly_list = list()
  line_list = list()
  text_list = list()
  
  if(latlong) {
    if(!("geosphere" %in% rownames(utils::installed.packages()))) {
      stop("{geosphere} package required for generate_scalebar_overlay() using lat/long coordinates")
    }
    length_val = length /4
    for(i in 1:4) {
      temppoly = matrix(0,nrow=4,ncol=2)
      templine = matrix(0,nrow=2,ncol=2)
      if(i == 1) {
        temppoly[1,] = c(loc[1],loc[2])
        temppoly[2,] = geosphere::destPoint(c(loc[1],loc[2]), b = bearing, d=length_val)
        temppoly[4,] = geosphere::destPoint(c(loc[1],loc[2]), b = bearing-90, d=thickness)
        temppoly[3,] = geosphere::destPoint(temppoly[4,], b = bearing, d=length_val)
        if(!flip_ticks) {
          templine[1,] = temppoly[4,]
          templine[2,] = geosphere::destPoint(temppoly[4,], b = bearing-90, d=thickness/2)
        } else {
          templine[1,] = temppoly[1,]
          templine[2,] = geosphere::destPoint(temppoly[1,], b = bearing+90, d=thickness/2)
        }
      } else {
        temppoly[1,] = poly_list[[i-1]][2,]
        temppoly[2,] = geosphere::destPoint(temppoly[1,], b = bearing, d=length_val)
        temppoly[4,] = geosphere::destPoint(temppoly[1,], b = bearing-90, d=thickness)
        temppoly[3,] = geosphere::destPoint(temppoly[4,], b = bearing, d=length_val)
        if(!flip_ticks) {
          templine[1,] = temppoly[4,]
          templine[2,] = geosphere::destPoint(temppoly[4,], b = bearing-90, d=thickness/2)
        } else {
          templine[1,] = temppoly[1,]
          templine[2,] = geosphere::destPoint(temppoly[1,], b = bearing+90, d=thickness/2)
        }
      }
      poly_list[[i]] = temppoly
      line_list[[i]] = templine
    }
    line_list[[5]] = matrix(0,nrow=2,ncol=2)

    if(!flip_ticks) {
      line_list[[5]][1,] = poly_list[[4]][3,]
      line_list[[5]][2,] = geosphere::destPoint(poly_list[[4]][3,], b = bearing-90, d=thickness/2)
      
      text_list[[1]] = geosphere::destPoint(line_list[[1]][2,], b = bearing-90, d=thickness*text_offset)
      text_list[[2]] = geosphere::destPoint(line_list[[3]][2,], b = bearing-90, d=thickness*text_offset)
      text_list[[3]] = geosphere::destPoint(line_list[[5]][2,], b = bearing-90, d=thickness*text_offset)
    } else {
      line_list[[5]][1,] = poly_list[[4]][2,]
      line_list[[5]][2,] = geosphere::destPoint(poly_list[[4]][2,], b = bearing+90, d=thickness/2)
      
      text_list[[1]] = geosphere::destPoint(line_list[[1]][2,], b = bearing+90, d=thickness*text_offset)
      text_list[[2]] = geosphere::destPoint(line_list[[3]][2,], b = bearing+90, d=thickness*text_offset)
      text_list[[3]] = geosphere::destPoint(line_list[[5]][2,], b = bearing+90, d=thickness*text_offset)
    }
  } else {
    length_val = length / 4
    dir = c(sinpi(bearing/180), cospi(bearing/180))
    dir2 = c(sinpi(bearing/180-1/2), cospi(bearing/180-1/2))
    dir3 = c(sinpi(bearing/180+1/2), cospi(bearing/180+1/2))
    
    for(i in 1:4) {
      temppoly = matrix(0,nrow=4,ncol=2)
      templine = matrix(0,nrow=2,ncol=2)
      if(i == 1) {
        temppoly[1,] = c(loc[1],loc[2])
        temppoly[2,] = c(loc[1],loc[2]) + length_val * dir
        temppoly[4,] = c(loc[1],loc[2]) + thickness  * dir2
        temppoly[3,] = temppoly[4,]     + length_val * dir
        if(!flip_ticks) {
          templine[1,] = temppoly[4,]
          templine[2,] = temppoly[4,] + thickness/2  * dir2
        } else {
          templine[1,] = temppoly[1,]
          templine[2,] = temppoly[1,] + thickness/2  * dir3
        }
      } else {
        temppoly[1,] = poly_list[[i-1]][2,]
        temppoly[2,] = temppoly[1,] + length_val * dir
        temppoly[4,] = temppoly[1,] + thickness  * dir2
        temppoly[3,] = temppoly[4,] + length_val * dir
        if(!flip_ticks) {
          templine[1,] = temppoly[4,]
          templine[2,] = temppoly[4,] + thickness/2  * dir2
        } else {
          templine[1,] = temppoly[1,]
          templine[2,] = temppoly[1,] + thickness/2  * dir3
        }
      }
      poly_list[[i]] = temppoly
      line_list[[i]] = templine
    }
    line_list[[5]] = matrix(0,nrow=2,ncol=2)
    if(!flip_ticks) {
      line_list[[5]][1,] = poly_list[[4]][3,]
      line_list[[5]][2,] = poly_list[[4]][3,] + thickness/2  * dir2
      
      text_list[[1]] = line_list[[1]][2,] + thickness*text_offset  * dir2
      text_list[[2]] = line_list[[3]][2,] + thickness*text_offset  * dir2
      text_list[[3]] = line_list[[5]][2,] + thickness*text_offset  * dir2
    } else {
      line_list[[5]][1,] = poly_list[[4]][2,]
      line_list[[5]][2,] = poly_list[[4]][2,] + thickness/2  * dir3
      
      text_list[[1]] = line_list[[1]][2,] + thickness*text_offset  * dir3
      text_list[[2]] = line_list[[3]][2,] + thickness*text_offset  * dir3
      text_list[[3]] = line_list[[5]][2,] + thickness*text_offset  * dir3
    }
  }
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  graphics::plot(x=c(extent[1],extent[3]),y=c(extent[2],extent[4]), xlim = c(extent[1],extent[2]),
                 ylim =  c(extent[3],extent[4]), pch = 0,bty="n",axes=FALSE,
                 xaxs = "i", yaxs = "i", cex = 0, col = NA)
  
  cols <- rep(c(color1,color2),2)
  for (i in 1:4) {
    graphics::polygon(poly_list[[i]],col=cols[i], border = border_color, lwd = border_width)
  }
  for (i in 1:5) {
    graphics::segments(line_list[[i]][1,1],line_list[[i]][1,2],
                       line_list[[i]][2,1],line_list[[i]][2,2],
                       col=tick_color, lwd = tick_width)
  }
  
  if(all(is.na(labels)) || length(labels) != 3) {
    format_string = paste0(c("%0.",decimals,"f"),collapse="")
    labels <- paste0(c(sprintf(format_string,c(0,length/2,length))),c("","",unit))
  }
  
  graphics::text(text_list[[1]][1],text_list[[1]][2],labels=labels[1],
                 adj=adj,cex=text_size,col=text_color,font=font)
  graphics::text(text_list[[2]][1],text_list[[2]][2],labels=labels[2],
                 adj=adj,cex=text_size,col=text_color,font=font)
  graphics::text(text_list[[3]][1],text_list[[3]][2],labels=labels[3],
                 adj=adj,cex=text_size,col=text_color,font=font)

  grDevices::dev.off() #resets par
  overlay_temp = png::readPNG(tempoverlay)
  if(!is.na(halo_color)) {
    if(!("rayimage" %in% rownames(utils::installed.packages()))) {
      stop("{rayimage} package required for `halo_color`")
    }
    tempoverlay = tempfile(fileext = ".png")
    grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
    graphics::par(mar = c(0,0,0,0))
    graphics::plot(x=c(extent[1],extent[3]),y=c(extent[2],extent[4]), xlim = c(extent[1],extent[2]),
         ylim =  c(extent[3],extent[4]), pch = 0,bty="n",axes=FALSE,
         xaxs = "i", yaxs = "i", cex = 0, col = NA)
    
    cols <- rep(c(color1,color2),2)
    offset_mat = matrix(halo_offset,nrow=4,ncol=2,byrow = TRUE)
    for (i in 1:4) {
      graphics::polygon(poly_list[[i]]+offset_mat,col=cols[i], 
                        border = border_color, lwd = border_width)
    }
    for (i in 1:5) {
      graphics::segments(line_list[[i]][1,1]+halo_offset[1],line_list[[i]][1,2]+halo_offset[2],
                         line_list[[i]][2,1]+halo_offset[1],line_list[[i]][2,2]+halo_offset[2],
                         col=tick_color, lwd = tick_width)
    }
    
    graphics::text(text_list[[1]][1]+halo_offset[1],text_list[[1]][2]+halo_offset[2],
         labels=labels[1],adj=adj,cex=text_size,col=text_color,font=font)
    graphics::text(text_list[[2]][1]+halo_offset[1],text_list[[2]][2]+halo_offset[2],
         labels=labels[2],adj=adj,cex=text_size,col=text_color,font=font)
    graphics::text(text_list[[3]][1]+halo_offset[1],text_list[[3]][2]+halo_offset[2],
         labels=labels[3],adj=adj,cex=text_size,col=text_color,font=font)
    
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
