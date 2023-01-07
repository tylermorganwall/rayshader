#'@title Generate Contour Overlay
#'
#'@description Calculates and returns an overlay of contour lines for the current height map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All grid points are assumed to be evenly spaced.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param levels Default `NA`. Automatically generated with 10 levels. This argument specifies the exact height levels of each contour.
#'@param nlevels Default `NA`. Controls the auto-generation of levels. If levels is length-2, 
#'this will automatically generate `nlevels` breaks between `levels[1]` and `levels[2]`.
#'@param width Default `NA`. Width of the resulting overlay. Default the same dimensions as heightmap.
#'@param height Default `NA`. Width of the resulting overlay. Default the same dimensions as heightmap.
#'@param color Default `black`. Color.
#'@param linewidth Default `1`. Line width.
#'@return Semi-transparent overlay with contours.
#'@export
#'@examples
#'#Add contours to the montereybay dataset
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_contour_overlay(montereybay))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
#'
#'#Add a different contour color for above and below water, and specify levels manually
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'breaks = seq(range(montereybay)[1],range(montereybay)[2],length.out=50)
#'water_breaks = breaks[breaks < 0]
#'land_breaks = breaks[breaks > 0]
#'
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_contour_overlay(montereybay, levels = water_breaks, color="white"))  %>%
#'  add_overlay(generate_contour_overlay(montereybay, levels = land_breaks, color="black"))  %>%
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Increase the resolution of the contour to improve the appearance of lines
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_contour_overlay(montereybay, levels = water_breaks, color="white",
#'                                       height = nrow(montereybay)*2, 
#'                                       width  = ncol(montereybay)*2))  %>%
#'  add_overlay(generate_contour_overlay(montereybay, levels = land_breaks, color="black",
#'                                       height = nrow(montereybay)*2, 
#'                                       width  = ncol(montereybay)*2))  %>%
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Increase the number of breaks and the transparency (via add_overlay)
#'montereybay %>%
#'  height_shade() %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  add_overlay(generate_contour_overlay(montereybay, linewidth=2, nlevels=100,
#'                                       height = nrow(montereybay)*2, color="black",
#'                                       width  = ncol(montereybay)*2), alphalayer=0.5) %>%
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Manually specify the breaks with levels
#'montereybay %>%
#'  height_shade() %>%
#'  add_overlay(generate_contour_overlay(montereybay, linewidth=2, levels = seq(-2000,0,100))) %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
generate_contour_overlay = function(heightmap, levels=NA, nlevels=NA, 
                                    zscale = 1, width=NA, height=NA, 
                                    color = "black", linewidth = 1) {
  if(!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop("`sf` package required for generate_contour_overlay()")
  }
  if(!(length(find.package("isoband", quiet = TRUE)) > 0)) {
    stop("`isoband` package required for generate_contour_overlay()")
  }
  if(is.na(levels[1])) {
    if(is.na(nlevels[1])) {
      nlevels = 10
    }
    rangelevels = range(heightmap,na.rm=TRUE)
    levels = seq(rangelevels[1], rangelevels[2], length.out = nlevels + 2)
  } else if (length(levels) == 2 && !is.na(nlevels)) {
    rangelevels = range(levels, na.rm=TRUE)
    levels = seq(rangelevels[1], rangelevels[2], length.out = nlevels + 2)
  }
  levels = levels[levels > min(heightmap,na.rm = TRUE)] 
  levels = levels[levels < max(heightmap,na.rm = TRUE)] 
  heightmap = flipud(t(heightmap))
  isolineval = isoband::isolines(x = 1:ncol(heightmap), 
                                 y = 1:nrow(heightmap), 
                                 z = heightmap, 
                                 levels=levels)
  contours = isoband::iso_to_sfg(isolineval)
  sf_contours = sf::st_sf(level = names(contours), geometry = sf::st_sfc(contours))
  if(is.na(width)) {
    width  = ncol(heightmap)
  }
  if(is.na(height)) {
    height  = nrow(heightmap)
  }
  tempoverlay = tempfile(fileext = ".png")
  grDevices::png(filename = tempoverlay, width = width, height = height, units="px",bg = "transparent")
  graphics::par(mar = c(0,0,0,0))
  graphics::plot(sf::st_geometry(sf_contours), xlim = c(1,ncol(heightmap)), ylim =  c(1,nrow(heightmap)),
       xaxs = "i", yaxs = "i", lwd = linewidth, col = color)
  grDevices::dev.off() #resets par
  png::readPNG(tempoverlay)
}
