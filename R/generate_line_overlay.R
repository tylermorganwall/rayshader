#'@title Generate Line Overlay
#'
#'@description Calculates and returns an overlay of lines for the current height map.
#'
#'@param geometry An `sf` object with LINESTRING geometry.
#'@param extent Either an object representing the spatial extent of the scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting 
#'overlay automatically.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param color Default `black`. Color of the lines.
#'@param linewidth Default `1`. Line width.
#'@param lty Default `1`. Line type. `1` is solid, `2` is dashed, `3` is dotted,`4` is dot-dash,
#'`5` is long dash, and `6` is dash-long-dash.
#'@param offset Default `c(0,0)`. Horizontal and vertical offset to apply to the line, in units of `geometry`.
#'@param data_column_width Default `NULL`. The numeric column to map the width to. The maximum width will be the value
#'specified in `linewidth`. 
#'@return Semi-transparent overlay with contours.
#'@export
#'@examples
#'#Add the included `sf` object with roads to the montereybay dataset
#'if(rayshader:::run_documentation()) {
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'montereybay %>% 
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_overlay(generate_line_overlay(monterey_roads_sf, 
#'                                    attr(montereybay,"extent"), heightmap = montereybay))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the line width, color, and transparency
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_overlay(generate_line_overlay(monterey_roads_sf, linewidth=3, color="white",
#'                                    attr(montereybay,"extent"), heightmap = montereybay),
#'                                    alphalayer=0.8)  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
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
                                 color = "black", linewidth = 1,  lty = 1,
                                 data_column_width = NULL, offset = c(0,0)) {
  if(!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop("{sf} package required for generate_line_overlay()")
  }
  if(is.null(extent)) {
    stop("`extent` must not be NULL")
  }
  stopifnot(!is.null(heightmap) || (!is.na(width) && !is.na(height)))
  stopifnot(!missing(extent))
  stopifnot(!missing(geometry))
  if(!(inherits(geometry,"sf") || inherits(geometry,"sfc") || inherits(geometry,"sfg"))) {
    stop("geometry must be {sf} object")
  }
  if(inherits(geometry,"sfg")) {
    geometry = sf::st_sfc(geometry)
  }
  sf_lines_cropped = base::suppressMessages(base::suppressWarnings(sf::st_crop(geometry, extent)))
  
  if(is.na(height)) {
    height  = ncol(heightmap)
  }
  if(is.na(width)) {
    width  = nrow(heightmap)
  }
  if(!is.null(data_column_width)) {
    if(data_column_width %in% colnames(sf_lines_cropped)) {
      widthvals = sf_lines_cropped[[data_column_width]] / max(sf_lines_cropped[[data_column_width]],na.rm = TRUE) * linewidth
    } else {
      warning("Was not able to find data_column_width `", data_column_width, "` in {sf} object.")
      widthvals = linewidth
    }
  } else {
    widthvals = linewidth
  }
  if(any(offset != 0)) {
    if(length(offset) == 2) {
      for(i in seq_len(length(sf_lines_cropped$geometry))) {
        sf_lines_cropped$geometry[[i]] = sf_lines_cropped$geometry[[i]] + offset
      }
    } else {
      stop("`offset` must be of length-2")
    }
  }
  extent = get_extent(extent)
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  graphics::plot(base::suppressWarnings(sf::st_geometry(sf_lines_cropped)), 
                 xlim = c(extent["xmin"],extent["xmax"]), 
                 ylim = c(extent["ymin"],extent["ymax"]), asp=1, lty = lty,
                 xaxs = "i", yaxs = "i", lwd = widthvals, col = color)
  grDevices::dev.off() #resets par
  png::readPNG(tempoverlay)
}
