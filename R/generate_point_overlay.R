#'@title Generate Point Overlay
#'
#'@description Calculates and returns an overlay of points for the current map.
#'
#'@param geometry An `sf` object with POINT geometry.
#'@param extent Either an object representing the spatial extent of the scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting 
#'overlay automatically.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param resolution_multiply Default `1`. If passing in `heightmap` instead of width/height, amount to 
#'increase the resolution of the overlay, which should make lines/polygons/points finer. 
#'Should be combined with `add_overlay(rescale_original = TRUE)` to ensure those added details are captured
#'in the final map.
#'@param color Default `black`. Color of the points.
#'@param size Default `1`. Point size.
#'@param pch Default `20`, solid. Point symbol. 
#' `0` = square, `1` = circle, `2` = triangle point up, `3` = plus, `4` = cross, 
#' `5` = diamond, `6` = triangle point down, `7` = square cross, `8` = star, 
#' `9` = diamond plus, `10` = circle plus, `11` = triangles up and down, 
#' `12` = square plus, `13` = circle cross, `14` = square and triangle down, 
#' `15` = filled square, `16` = filled circle, `17` = filled triangle point-up, 
#' `18` = filled diamond, `19` = solid circle, `20` = bullet (smaller circle), 
#' `21` = filled circle blue, `22` = filled square blue, `23` = filled diamond blue, 
#' `24` = filled triangle point-up blue, `25` = filled triangle point down blue
#'@param offset Default `c(0,0)`. Horizontal and vertical offset to apply to the polygon, in units of `geometry`.
#'@param data_column_width Default `NULL`. The numeric column to map the width to. The maximum width will be the value
#'specified in `linewidth`. 
#'@return Semi-transparent overlay with contours.
#'@export
#'@examples
#'#Add the included `sf` object with roads to the montereybay dataset
#'if(run_documentation()) {
#'  monterey_city = sf::st_sfc(sf::st_point(c(-121.893611, 36.603056)))
#'  montereybay %>% 
#'    height_shade() %>%
#'    add_overlay(generate_point_overlay(monterey_city, color="red", size=12, 
#'                                    attr(montereybay,"extent"), heightmap = montereybay))  %>%
#'    add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'    plot_map()
#'}
generate_point_overlay = function(geometry, extent, heightmap = NULL,
                                  width=NA, height=NA, resolution_multiply = 1,
                                  pch = 20,  
                                  color = "black", size = 1, offset = c(0,0), data_column_width = NULL) {
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
  sf_point_cropped = base::suppressMessages(base::suppressWarnings(sf::st_crop(geometry, extent)))
  
  if(is.na(height)) {
    height = ncol(heightmap)
  }
  if(is.na(width)) {
    width  = nrow(heightmap)
  }
  height = height * resolution_multiply
  width = width * resolution_multiply
  
  if(!is.null(data_column_width)) {
    if(data_column_width %in% colnames(sf_point_cropped)) {
      widthvals = sf_point_cropped[[data_column_width]] / max(sf_point_cropped[[data_column_width]],na.rm = TRUE) * size
    } else {
      warning("Was not able to find data_column_width `", data_column_width, "` in {sf} object.")
      widthvals = size
    }
  } else {
    widthvals = size
  }
  if(any(offset != 0)) {
    if(length(offset) == 2) {
      for(i in seq_len(length(sf_point_cropped$geometry))) {
        sf_point_cropped$geometry[[i]] = sf_point_cropped$geometry[[i]] + offset
      }
    } else {
      stop("`offset` must be of length-2")
    }
  }
  extent = get_extent(extent)
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  graphics::plot(base::suppressWarnings(sf::st_geometry(sf_point_cropped)), 
                 xlim = c(extent["xmin"],extent["xmax"]), 
                 ylim = c(extent["ymin"],extent["ymax"]), asp=1, pch = pch,
                 xaxs = "i", yaxs = "i", lwd = widthvals, col = color)
  grDevices::dev.off() #resets par
  png::readPNG(tempoverlay)
}
