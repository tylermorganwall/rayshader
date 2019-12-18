#'@title Calculate Terrain Color Map
#'
#'@description Calculates a color for each point on the surface using a direct elevation-to-color mapping.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. 
#'@param texture Default `terrain.colors(256)`. A color palette for the plot.
#'@return RGB array of hillshaded texture mappings.
#'@export
#'@examples
#'#Create a direct mapping of elevation to color:
#'montereybay %>%
#'  height_shade() %>%
#'  plot_map()
#'  
#'#Add a shadow:
#'\donttest{
#'montereybay %>%
#'  height_shade() %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
#'
#'#Change the palette:
#'\donttest{
#'montereybay %>%
#'  height_shade(texture = topo.colors(256)) %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
#'
#'#Really change the palette:
#'\donttest{
#'montereybay %>%
#'  height_shade(texture = rainbow(256)) %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
height_shade = function(heightmap, texture=grDevices::terrain.colors(256)) {
  tempfilename = tempfile()
  old.par = graphics::par(no.readonly = TRUE)
  if(all(old.par$pin > 0)) {
    on.exit(suppressWarnings(graphics::par(old.par)))
  }
  grDevices::png(tempfilename,width = nrow(heightmap),height=ncol(heightmap))
  graphics::par(mar = c(0,0,0,0))
  raster::image(fliplr(heightmap),axes = FALSE,col = texture, useRaster=TRUE)
  grDevices::dev.off()
  tempmap = png::readPNG(tempfilename)
  return(tempmap)
}
