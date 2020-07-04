#'@title Generate Polygon Overlay
#'
#'@description Calculates and returns an overlay of contour lines for the current height map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All grid points are assumed to be evenly spaced.
#'@param extent A `raster::Extent` object with the bounding box for the height map used to generate the original map.
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting 
#'overlay automatically.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param palette Default `black`. Single color, named vector color palette, or palette function. 
#'If this is a named vector and `data_column_fill` is not `NULL`, 
#'it will map the colors in the vector to the names. If `data_column_fill` is a numeric column,
#'this will give a continuous mapping.
#'@param linewidth Default `1`. Line width.
#'@param data_column_fill Default `NULL`. The column to map the polygon fill color. If numeric
#'@return Semi-transparent overlay with contours.
#'@export
#'@examples
#'#Add contours to the montereybay dataset
#'\donttest{
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_overlay(generate_line_overlay(montbay$osm_lines, attr(montereybay,"extent"), heightmap=montereybay))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
generate_polygon_overlay = function(geometry, extent, heightmap = NULL, 
                                    width=NA, height=NA, data_column_fill = NULL, 
                                    linecolor = "black", palette = "white", linewidth = 1) {
  if(!("sf" %in% rownames(utils::installed.packages()))) {
    stop("{sf} package required for generate_line_overlay()")
  }
  stopifnot(!is.null(heightmap) || (is.na(width) && is.na(height)))
  stopifnot(!missing(extent))
  stopifnot(!missing(geometry))
  if(!inherits(geometry,"sf")) {
    stop("geometry must be {sf} object")
  }
  sf_contours_cropped = sf::st_crop(geometry, extent)
  
  if(is.na(height)) {
    height  = ncol(heightmap)
  }
  if(is.na(width)) {
    width  = nrow(heightmap)
  }
  if (is.function(palette)) {
    palette = palette(nrow(sf_contours_cropped))
  }
  if(is.function(palette)) {
    transparent = FALSE
  } else if(length(palette) == 1 && is.na(palette[1])) {
    transparent = TRUE
  } else {
    transparent = FALSE
  }
  if(!transparent) {
    #Calculate colors
    stopifnot(is.character(palette))
    if(!is.null(data_column_fill)) {
      if(data_column_fill %in% colnames(sf_contours_cropped)) {
        if(is.numeric(sf_contours_cropped[[data_column_fill]])) {
          max_col = max(sf_contours_cropped[[data_column_fill]],na.rm = TRUE)
          min_col = min(sf_contours_cropped[[data_column_fill]],na.rm = TRUE)
          indices = (sf_contours_cropped[[data_column_fill]] - min_col) / (max_col - min_col) * length(palette)
          fillvals = palette[as.integer(indices)]
        } else if (is.character(sf_contours_cropped[[data_column_fill]])) {
          mapping = names(palette)
          indices = match(sf_contours_cropped[[data_column_fill]],mapping)
          fillvals = palette[as.integer(indices)]
        } else if  (is.factor(sf_contours_cropped[[data_column_fill]])) {
          character_col = as.character(sf_contours_cropped[[data_column_fill]])
          mapping = names(palette)
          indices = match(sf_contours_cropped[[data_column_fill]],mapping)
          fillvals = palette[as.integer(indices)]
        }
      } else {
        warning("Was not able to find data_column_fill `", data_column_fill, "` in {sf} object.")
        fillvals = palette
      }
    } else {
      if(nrow(sf_contours_cropped) %% length(palette) != 0) {
        stop("Number of explicitly defined colors does not match (or recycle within) number of polygons")
      }
      fillvals = palette
    }
  }
  if(linewidth == 0 || is.na(linewidth)) {
    lty = "blank"
    linewidth=10
  } else {
    lty = "solid"
  }
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  if(!transparent) {
    graphics::plot(sf::st_geometry(sf_contours_cropped), xlim = c(extent@xmin,extent@xmax),
                   ylim =  c(extent@ymin,extent@ymax), asp = 1, lty = lty, border = NA,
                   xaxs = "i", yaxs = "i", lwd = linewidth, col = fillvals)
  }
  if(!is.na(linewidth) && linewidth > 0) {
    graphics::plot(sf::st_geometry(sf_contours_cropped), xlim = c(extent@xmin,extent@xmax), 
                   ylim =  c(extent@ymin,extent@ymax), asp = 1, lty = lty, add=!transparent,
                   xaxs = "i", yaxs = "i", lwd = linewidth, col = NA, border = linecolor)
  }
  grDevices::dev.off() #resets par
  png::readPNG(tempoverlay)
}