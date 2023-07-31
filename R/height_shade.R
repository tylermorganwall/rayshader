#'@title Calculate Terrain Color Map
#'
#'@description Calculates a color for each point on the surface using a direct elevation-to-color mapping.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. 
#'@param texture Default `terrain.colors(256)`. A color palette for the plot.
#'@param range Default `NULL`, the full range of the heightmap. A length-2 vector specifying the maximum
#'and minimum values to map the color palette to.
#'@param keep_user_par Default `TRUE`. Whether to keep the user's `par()` settings. Set to `FALSE` if you 
#'want to set up a multi-pane plot (e.g. set `par(mfrow)`).
#'@return RGB array of hillshaded texture mappings.
#'@export
#'@examples
#'#Create a direct mapping of elevation to color:
#'montereybay %>%
#'  height_shade() %>%
#'  plot_map()
#'  
#'#Add a shadow:
#'if(run_documentation()) {
#'montereybay %>%
#'  height_shade() %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
#'
#'#Change the palette:
#'if(run_documentation()) {
#'montereybay %>%
#'  height_shade(texture = topo.colors(256)) %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
#'
#'#Really change the palette:
#'if(run_documentation()) {
#'montereybay %>%
#'  height_shade(texture = rainbow(256)) %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
height_shade = function(heightmap, 
                        texture=grDevices::colorRampPalette(c("#6AA85B","#D9CC9A","#FFFFFF"))(256),
                        range = NULL,
                        keep_user_par = TRUE) {
  if(!is.null(range)) {
    range = base::sort(range[1:2])
  } else {
    range = range(heightmap,na.rm=TRUE)
  }
  tempfilename = tempfile() 
  if(keep_user_par) {
    old.par = graphics::par(no.readonly = TRUE)
    if(all(old.par$pin > 0)) {
      on.exit(suppressWarnings(graphics::par(old.par)))
    }
  }
  grDevices::png(tempfilename,width = nrow(heightmap),height=ncol(heightmap))
  graphics::par(mar = c(0,0,0,0))
  graphics::image(fliplr(heightmap),axes = FALSE,col = texture, useRaster=TRUE,zlim=range)
  grDevices::dev.off()
  tempmap = png::readPNG(tempfilename)
  return(tempmap)
}
