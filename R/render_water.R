#'@title Render Water Layer
#'
#'@description Adds water layer to the scene, removing the previous water layer if desired.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param waterdepth Default `0`.
#'@param watercolor Default `lightblue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency. 
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param remove_water Default `FALSE`. If `TRUE`, will remove existing water layer and replace it with new layer.
#'@export
#'@examples
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay)
#'  
#'#We want to add a layer of water after the initial render.
#'render_water(montereybay)
render_water = function(heightmap, waterdepth=0, watercolor="lightblue",
                        zscale=1, wateralpha=0.5, waterlinecolor=NULL, waterlinealpha = 1, 
                        linewidth = 2, remove_water = FALSE) {
  if(remove_water) {
    ids = rgl::rgl.ids()
    for(i in seq(nrow(ids),2,-1)) {
      if(ids$type[i] != "surface") {
        rgl::rgl.pop(id=ids$id[i])
      } else {
        rgl::rgl.pop(id=ids$id[i])
        break
      }
    }
  }
  rgl.surface(c(1,nrow(heightmap)),c(-1,-ncol(heightmap)),matrix(waterdepth/zscale,2,2),color=watercolor,lit=FALSE,alpha=wateralpha)
  make_water(heightmap/zscale,waterheight=waterdepth/zscale,wateralpha=wateralpha,watercolor=watercolor)
  if(!is.null(waterlinecolor)) {
    make_lines(heightmap,basedepth=waterdepth/zscale,linecolor=waterlinecolor,zscale=zscale,linewidth = linewidth,alpha=waterlinealpha,solid=FALSE)
    make_waterlines(heightmap,waterdepth=waterdepth/zscale,linecolor=waterlinecolor,zscale=zscale,alpha=waterlinealpha,lwd=linewidth)
  }
}