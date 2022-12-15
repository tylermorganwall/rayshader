#'@title Generate Polygon Overlay
#'
#'@description Transforms an input `sf` object into an image overlay for the current height map.
#'
#'@param geometry An `sf` object with POLYGON geometry.
#'@param extent Either an object representing the spatial extent of the scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting 
#'overlay automatically.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as height map.
#'@param linecolor Default `black`. Color of the lines.
#'@param palette Default `black`. Single color, named vector color palette, or palette function. 
#'If this is a named vector and `data_column_fill` is not `NULL`, 
#'it will map the colors in the vector to the names. If `data_column_fill` is a numeric column,
#'this will give a continuous mapping.
#'@param linewidth Default `1`. Line width.
#'@param offset Default `c(0,0)`. Horizontal and vertical offset to apply to the polygon, in units of `geometry`.
#'@param data_column_fill Default `NULL`. The column to map the polygon fill color to.
#'@return Image overlay representing the input polygon data.
#'@export
#'@examples
#'#Plot the counties around Monterey Bay, CA
#'if(rayshader:::run_documentation()) {
#'generate_polygon_overlay(monterey_counties_sf, palette = rainbow, 
#'                         extent = attr(montereybay,"extent"), heightmap = montereybay) %>%
#'  plot_map() 
#'}
#'if(rayshader:::run_documentation()) {
#'#These counties include the water, so we'll plot bathymetry data over the polygon
#'#data to only include parts of the polygon that fall on land.
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'
#'generate_polygon_overlay(monterey_counties_sf, palette = rainbow, 
#'                         extent = attr(montereybay,"extent"), heightmap = montereybay) %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, start_transition = 0)) %>%
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Add a semi-transparent hillshade and change the palette, and remove the polygon lines
#'montereybay %>%
#'  sphere_shade(texture = "bw") %>%
#'  add_overlay(generate_polygon_overlay(monterey_counties_sf, 
#'                         palette = terrain.colors, linewidth=NA,
#'                         extent = attr(montereybay,"extent"), heightmap = montereybay),
#'                         alphalayer=0.7) %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, start_transition = 0)) %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0) %>%
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Map one of the variables in the sf object and use an explicitly defined color palette
#'county_palette = c("087" = "red",    "053" = "blue",   "081" = "green", 
#'                   "069" = "yellow", "085" = "orange", "099" = "purple") 
#'montereybay %>%
#'  sphere_shade(texture = "bw") %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0) %>%
#'  add_overlay(generate_polygon_overlay(monterey_counties_sf, linecolor="white", linewidth=3,
#'                         palette = county_palette, data_column_fill = "COUNTYFP",
#'                         extent = attr(montereybay,"extent"), heightmap = montereybay),
#'                         alphalayer=0.7) %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, start_transition = 0)) %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.5) %>%
#'  plot_map()
#'}
generate_polygon_overlay = function(geometry, extent, heightmap = NULL, 
                                    width=NA, height=NA, offset = c(0,0), data_column_fill = NULL, 
                                    linecolor = "black", palette = "white", linewidth = 1) {
  if(!(length(find.package("sf", quiet = TRUE)) > 0)) {
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
  sf_polygons_cropped = base::suppressMessages(base::suppressWarnings(sf::st_crop(geometry, extent)))
  
  if(is.na(height)) {
    height  = ncol(heightmap)
  }
  if(is.na(width)) {
    width  = nrow(heightmap)
  }
  if (is.function(palette)) {
    palette = palette(nrow(sf_polygons_cropped))
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
      if(data_column_fill %in% colnames(sf_polygons_cropped)) {
        if(is.numeric(sf_polygons_cropped[[data_column_fill]])) {
          max_col = max(sf_polygons_cropped[[data_column_fill]],na.rm = TRUE)
          min_col = min(sf_polygons_cropped[[data_column_fill]],na.rm = TRUE)
          indices = (sf_polygons_cropped[[data_column_fill]] - min_col) / (max_col - min_col) * (length(palette)-1)
          fillvals = palette[as.integer(indices)+1]
        } else if (is.character(sf_polygons_cropped[[data_column_fill]])) {
          mapping = names(palette)
          indices = match(sf_polygons_cropped[[data_column_fill]],mapping)
          fillvals = palette[as.integer(indices)]
        } else if  (is.factor(sf_polygons_cropped[[data_column_fill]])) {
          character_col = as.character(sf_polygons_cropped[[data_column_fill]])
          mapping = names(palette)
          indices = match(sf_polygons_cropped[[data_column_fill]],mapping)
          fillvals = palette[as.integer(indices)]
        }
      } else {
        warning("Was not able to find data_column_fill `", data_column_fill, "` in {sf} object.")
        fillvals = palette
      }
    } else {
      if(nrow(sf_polygons_cropped) %% length(palette) != 0) {
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
  if(any(offset != 0)) {
    if(length(offset) == 2) {
      for(i in seq_len(length(sf_polygons_cropped$geometry))) {
        sf_polygons_cropped$geometry[[i]] = sf_polygons_cropped$geometry[[i]] + offset
      }
    } else {
      stop("`offset` must be of length-2")
    }
  }
  extent = get_extent(extent)
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  if(!transparent) {
    graphics::plot(sf::st_geometry(sf_polygons_cropped), 
                   xlim = c(extent["xmin"],extent["xmax"]), 
                   ylim = c(extent["ymin"],extent["ymax"]), 
                   lty = lty, border = NA, asp = 1,
                   xaxs = "i", yaxs = "i", lwd = linewidth, col = fillvals)
  }
  if(!is.na(linewidth) && linewidth > 0) {
    graphics::plot(sf::st_geometry(sf_polygons_cropped), 
                   xlim = c(extent["xmin"],extent["xmax"]), 
                   ylim = c(extent["ymin"],extent["ymax"]), 
                   lty = lty, add=!transparent, asp = 1,
                   xaxs = "i", yaxs = "i", lwd = linewidth, col = NA, border = linecolor)
  }
  grDevices::dev.off() #resets par
  png::readPNG(tempoverlay)
}
