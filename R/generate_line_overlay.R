#'@title Generate Line Overlay
#'
#'@description Calculates and returns an overlay of contour lines for the current height map.
#'
#'@param geometry An `sf` object with LINESTRING geometry.
#'@param extent A `raster::Extent` object with the bounding box for the height map used to generate the original map.
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting 
#'overlay automatically.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param color Default `black`. Color of the lines.
#'@param linewidth Default `1`. Line width.
#'@param data_column_width Default `NULL`. The numeric column to map the width to. The maximum width will be the value
#'specified in `linewidth`. 
#'@return Semi-transparent overlay with contours.
#'@export
#'@examples
#'#Add the included `sf` object with roads to the montereybay dataset
#'\donttest{
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'montereybay %>% 
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_overlay(generate_line_overlay(monterey_roads_sf, 
#'                                    attr(montereybay,"extent"), heightmap = montereybay))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'  
#'#Change the line width, color, and transparency
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_overlay(generate_line_overlay(monterey_roads_sf, linewidth=3, color="white",
#'                                    attr(montereybay,"extent"), heightmap = montereybay),
#'                                    alphalayer=0.8)  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'  
#'#Manually specify the width and height to improve visual quality of the lines
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_line_overlay(monterey_roads_sf, linewidth=3, color="white",
#'                                    attr(montereybay,"extent"), width = 1080, height = 1080),
#'                                    alphalayer=0.8)  %>%
#'  plot_map()
#'}
generate_line_overlay = function(geometry, extent, heightmap = NULL,
                                 width=NA, height=NA, 
                                 color = "black", linewidth = 1, data_column_width = NULL) {
  if(!("sf" %in% rownames(utils::installed.packages()))) {
    stop("{sf} package required for generate_line_overlay()")
  }
  if(is.null(extent)) {
    stop("`extent` must not be NULL")
  }
  stopifnot(!is.null(heightmap) || (!is.na(width) && !is.na(height)))
  stopifnot(!missing(extent))
  stopifnot(!missing(geometry))
  if(!inherits(geometry,"sf")) {
    stop("geometry must be {sf} object")
  }
  sf_contours_cropped = base::suppressMessages(base::suppressWarnings(sf::st_crop(geometry, extent)))
  
  if(is.na(height)) {
    height  = ncol(heightmap)
  }
  if(is.na(width)) {
    width  = nrow(heightmap)
  }
  if(!is.null(data_column_width)) {
    if(data_column_width %in% colnames(sf_contours_cropped)) {
      widthvals = sf_contours_cropped[[data_column_width]] / max(sf_contours_cropped[[data_column_width]],na.rm = TRUE) * linewidth
    } else {
      warning("Was not able to find data_column_width `", data_column_width, "` in {sf} object.")
      widthvals = linewidth
    }
  } else {
    widthvals = linewidth
  }
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  graphics::plot(base::suppressWarnings(sf::st_geometry(sf_contours_cropped)), xlim = c(extent@xmin,extent@xmax), 
                 ylim =  c(extent@ymin,extent@ymax), asp=1,
                 xaxs = "i", yaxs = "i", lwd = widthvals, col = color)
  grDevices::dev.off() #resets par
  png::readPNG(tempoverlay)
}