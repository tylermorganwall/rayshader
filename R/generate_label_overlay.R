#'@title Generate Label Overlay
#'
#'@description This uses the `maptools::placeLabel()` function to generate labels for the given scene. Either
#'use an `sf` object or manually specify the x/y coordinates and label. 
#'
#'@param labels A character vector of labels, or an `sf` object with `POINT` geometry and a column for labels.
#'@param extent A `raster::Extent` object with the bounding box for the height map used to generate the original map.
#'@param x Default `NULL`. The x-coordinate, if `labels` is not an `sf` object.
#'@param y Default `NULL`. The y-coordinate, if `labels` is not an `sf` object.
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting 
#'overlay automatically.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param text_size Default `1`. Text size.
#'@param point_size Default `0`, no points. Point size.
#'@param color Default `black`. Color of the labels.
#'@param point_color Default `NA`. Colors of the points. Unless otherwise specified, this defaults to `color`.
#'@param font Default `1`. An integer which specifies which font to use for text. 
#'If possible, device drivers arrange so that 1 corresponds to plain text (the default), 
#'2 to bold face, 3 to italic and 4 to bold italic. 
#'@param pch Default `20`, solid. Point symbol. 
#' `0` = square, `1` = circle, `2` = triangle point up, `3` = plus, `4` = cross, 
#' `5` = diamond, `6` = triangle point down, `7` = square cross, `8` = star, 
#' `9` = diamond plus, `10` = circle plus, `11` = triangles up and down, 
#' `12` = square plus, `13` = circle cross, `14` = square and triangle down, 
#' `15` = filled square, `16` = filled circle, `17` = filled triangle point-up, 
#' `18` = filled diamond, `19` = solid circle, `20` = bullet (smaller circle), 
#' `21` = filled circle blue, `22` = filled square blue, `23` = filled diamond blue, 
#' `24` = filled triangle point-up blue, `25` = filled triangle point down blue
#'@param offset Default `c(0,0)`. Horizontal and vertical offset to apply to the label, in units of `geometry`.
#'@param data_label_column Default `NULL`. The column in the `sf` object that contains the labels.
#'@param halo_color Default `NA`, no halo. If a color is specified, the text label will be surrounded by a halo
#'of this color.
#'@param halo_expand Default `2`. Number of pixels to expand the halo.
#'@param halo_alpha Default `1`. Transparency of the halo.
#'@param halo_offset Default `c(0,0)`. Horizontal and vertical offset to apply to the halo, in units of `geometry`.
#'@param halo_blur Default `1`. Amount of blur to apply to the halo. Values greater than `30` won't result in further blurring.
#'@param seed Default `NA`, no seed. Random seed for ensuring the consistent placement of labels around points. 
#'@return Semi-transparent overlay with labels.
#'@export
#'@examples
#'#Add the included `sf` object with roads to the montereybay dataset
#'#Only run these examples if the `magick` package is installed.
#'if ("magick" %in% rownames(utils::installed.packages())) {
#'\donttest{
#'#Create the water palette
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'
#'#We're plotting the polygon data here for counties around Monterey Bay. We'll first
#'#plot the county names at the polygon centroids.
#'bathy_hs %>% 
#'  add_shadow(lamb_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_polygon_overlay(monterey_counties_sf, palette = rainbow, 
#'                                       extent = attr(montereybay,"extent"),
#'                                       heightmap = montereybay)) %>% 
#'  add_overlay(generate_label_overlay(labels=monterey_counties_sf,
#'                                     color="black", point_size = 1, text_size = 1,
#'                                     data_label_column = "NAME",
#'                                     extent= attr(montereybay,"extent"), heightmap = montereybay,
#'                                     seed=1))  %>%
#'  plot_map()
#'
#'#It's hard to read these values, so we'll add a white halo.
#'bathy_hs %>% 
#'  add_shadow(lamb_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_polygon_overlay(monterey_counties_sf, palette = rainbow, 
#'                                       extent = attr(montereybay,"extent"),
#'                                       heightmap = montereybay)) %>% 
#'  add_overlay(generate_label_overlay(labels=monterey_counties_sf,
#'                                     color="black", point_size = 1, text_size = 1,
#'                                     data_label_column = "NAME",
#'                                     extent= attr(montereybay,"extent"), heightmap = montereybay,
#'                                     halo_color = "white", halo_expand = 3,
#'                                     seed=1))  %>%
#'  plot_map()
#'
#'
#'#Plot the actual town locations, using the manual plotting interface instead of the `sf` object
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0)) %>% 
#'  add_shadow(lamb_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_label_overlay(labels=as.character(monterey_counties_sf$NAME),
#'                                     x=as.numeric(as.character(monterey_counties_sf$INTPTLON)),
#'                                     y=as.numeric(as.character(monterey_counties_sf$INTPTLAT)),
#'                                     color="black", point_size = 1, text_size = 1,
#'                                     extent= attr(montereybay,"extent"), heightmap = montereybay,
#'                                     halo_color = "white", halo_expand = 3,
#'                                     seed=1))  %>%
#'  plot_map()
#'
#'#Adding a softer blurred halo
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0)) %>% 
#'  add_shadow(lamb_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_label_overlay(labels=as.character(monterey_counties_sf$NAME),
#'                                     x=as.numeric(as.character(monterey_counties_sf$INTPTLON)),
#'                                     y=as.numeric(as.character(monterey_counties_sf$INTPTLAT)),
#'                                     color="black", point_size = 1, text_size = 1,
#'                                     extent= attr(montereybay,"extent"), heightmap = montereybay,
#'                                     halo_color = "white", halo_expand = 3, halo_blur=10,
#'                                     seed=1))  %>%
#'  plot_map()
#'  
#'#Changing the seed changes the locations of the labels
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0)) %>% 
#'  add_shadow(lamb_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_label_overlay(labels=as.character(monterey_counties_sf$NAME),
#'                                     x=as.numeric(as.character(monterey_counties_sf$INTPTLON)),
#'                                     y=as.numeric(as.character(monterey_counties_sf$INTPTLAT)),
#'                                     color="black", point_size = 1, text_size = 1,
#'                                     extent= attr(montereybay,"extent"), heightmap = montereybay,
#'                                     halo_color = "white", halo_expand = 3, halo_blur=10,
#'                                     seed=2))  %>%
#'  plot_map()
#'}
#'}
generate_label_overlay = function(labels, extent, x=NULL, y=NULL, 
                                  heightmap = NULL, width=NA, height=NA, text_size = 1,
                                  color = "black", font = 1, pch = 16, 
                                  point_size = 1, point_color = NA, offset = c(0,0),
                                  data_label_column = NULL,
                                  halo_color = NA, halo_expand = 0,
                                  halo_alpha = 1, halo_offset = c(0,0), halo_blur = 1, 
                                  seed = NA) {
  if(!is.na(seed)) {
    set.seed(seed)
  } else {
    seed = .Random.seed
  }
  if(is.na(point_color)) {
    point_color = color
  }
  if(!("maptools" %in% rownames(utils::installed.packages()))) {
    stop("{maptools} package required for generate_label_overlay()")
  }
  if((inherits(labels,"sf") || inherits(labels,"sfc")) && 
     (is.null(x) && is.null(y)) &&
     !is.null(data_label_column)) {
    if(!("sf" %in% rownames(utils::installed.packages()))) {
      stop("{sf} package required for {sf} object support")
    }
    geometry_list = sf::st_geometry(labels)
    xycoords = list()
    for(i in seq_len(length(geometry_list))) {
      if(inherits(geometry_list[[i]],"POLYGON") || inherits(geometry_list[[i]],"MULTIPOLYGON")) {
        xycoords[[i]] = sf::st_coordinates(sf::st_centroid(geometry_list[[i]]))
      } else {
        xycoords[[i]] = sf::st_coordinates(geometry_list[[i]])
      }
    }
    xycoords = do.call(rbind,xycoords)
    labels = as.character(labels[[data_label_column]])
    x = xycoords[,1]
    y = xycoords[,2]
  }
  if(is.null(extent)) {
    stop("`extent` must not be NULL")
  }
  stopifnot(!is.null(heightmap) || (!is.na(width) && !is.na(height)))
  stopifnot(!missing(extent))
  stopifnot(!missing(labels))

  if(is.na(height)) {
    height  = ncol(heightmap)
  }
  if(is.na(width)) {
    width  = nrow(heightmap)
  }
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  graphics::plot(x=x+offset[1],y=y+offset[2], xlim = c(extent@xmin,extent@xmax),
                 ylim =  c(extent@ymin,extent@ymax), asp=1, pch = pch,bty="n",axes=FALSE,
                 xaxs = "i", yaxs = "i", cex = point_size, col = point_color)
  maptools::pointLabel(x=x+offset[1],y=y+offset[2],labels=labels, cex = text_size,
                 xlim = c(extent@xmin,extent@xmax), bty="n",
                 ylim =  c(extent@ymin,extent@ymax), asp=1, font = font,
                 xaxs = "i", yaxs = "i",  col = color)
  grDevices::dev.off() #resets par
  overlay_temp = png::readPNG(tempoverlay)
  if(!is.na(halo_color)) {
    if(!("rayimage" %in% rownames(utils::installed.packages()))) {
      stop("{rayimage} package required for `halo_color`")
    }
    set.seed(seed)
    tempoverlay = tempfile(fileext = ".png")
    grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
    graphics::par(mar = c(0,0,0,0))
    graphics::plot(x=x+halo_offset[1],y=y+halo_offset[2], xlim = c(extent@xmin,extent@xmax),
                   ylim =  c(extent@ymin,extent@ymax), asp=1, pch = pch, bty="n",axes=FALSE,
                   xaxs = "i", yaxs = "i", cex = point_size, col = halo_color)
    maptools::pointLabel(x=x+halo_offset[1],y=y+halo_offset[2],labels=labels, cex = text_size, 
                         xlim = c(extent@xmin,extent@xmax), bty="n", 
                         ylim =  c(extent@ymin,extent@ymax), asp=1, font = font,
                         xaxs = "i", yaxs = "i",  col = halo_color)
    grDevices::dev.off() #resets par
    overlay_temp_under = png::readPNG(tempoverlay)
    if(halo_expand != 0 || any(halo_offset != 0)) {
      temp_alpha = overlay_temp_under[,,4]
      temp_alpha[temp_alpha > 0] = 1
      booldistance = rayimage::render_boolean_distance(temp_alpha)
      temp_alpha[booldistance <= halo_expand] = 1
      temp_alpha[booldistance > halo_expand] = 0
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