#'@title make_waterlines
#'
#'@description Makes the edge lines of
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param water_input Default `0`.
#'@param line_color Default `grey40`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param alpha Default `1`. Transparency of lines.
#'@param linewidth Default `2`. Water line width.
#'@param antialias Default `FALSE`.
#'@keywords internal
make_waterlines = function(
  heightmap,
  water_input = 0,
  line_color = "grey40",
  zscale = 1,
  alpha = 1,
  linewidth = 2,
  antialias = FALSE
) {
  heightmap = heightmap / zscale
  na_matrix = is.na(heightmap)
  heightlist = make_waterlines_cpp(heightmap, na_matrix, water_input / zscale)
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  if (length(heightlist) > 0) {
    segmentlist = do.call(rbind, heightlist)
    segmentlist[, 3] = -segmentlist[, 3]
    segmentlist[, 1] = segmentlist[, 1] - 1
    segmentlist[, 3] = segmentlist[, 3] - 1

    segmentlist[, 1] = segmentlist[, 1] - (nr - 1) / 2
    segmentlist[, 3] = segmentlist[, 3] - (nc - 1) / 2
    segmentlist = apply_geographic_aspect_to_vertices(segmentlist)
    rgl::segments3d(
      segmentlist,
      color = line_color,
      lwd = linewidth,
      alpha = alpha,
      depth_mask = TRUE,
      line_antialias = antialias,
      depth_test = "lequal",
      tag = "waterlines",
      lit = FALSE
    )
  }
}
