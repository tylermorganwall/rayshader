#'@title make_waterlines
#'
#'@description Makes the edge lines of 
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param waterdepth Default `0`.
#'@param linecolor Default `grey40`. 
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param alpha Default `1`. Transparency of lines.
#'@param linewidth Default `2`. Water line width. 
#'@param antialias Default `FALSE`.
#'@keywords internal
make_waterlines = function(heightmap,waterdepth=0,linecolor="grey40",zscale=1,alpha=1,linewidth=2,antialias = FALSE) {
  heightmap = heightmap/zscale
  na_matrix = is.na(heightmap)
  heightlist = make_waterlines_cpp(heightmap,na_matrix,waterdepth/zscale)
  if(length(heightlist) > 0) {
    segmentlist = do.call(rbind,heightlist)
    segmentlist[,1] = segmentlist[,1] - nrow(heightmap)/2
    segmentlist[,3] = -(segmentlist[,3] + ncol(heightmap)/2)
    rgl::segments3d(segmentlist,color=linecolor,lwd=linewidth,alpha=alpha,depth_mask=TRUE, 
                    line_antialias=antialias, depth_test="lequal",tag = "waterlines",
                    lit = FALSE)
  }
}