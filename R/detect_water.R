#'@title Detect water
#'
#'@description Detects bodies of water (of a user-defined minimum size) within an elevation matrix.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All grid points are assumed to be evenly spaced.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param cutoff Default `0.999`. The lower limit of the z-component of the unit normal vector to be classified as water.
#'@param min_area Default length(heightmap)/400. Minimum area (in units of the height matrix x and y spacing) to be considered a body of water.
#'@param max_height Default `NULL`. If passed, this number will specify the maximum height a point can be considered to be water.
#'@param normalvectors Default `NULL`. Pre-computed array of normal vectors from the `calculate_normal` function. Supplying this will speed up water detection.
#'@param keep_groups Default `FALSE`. If `TRUE`, the matrix returned will retain the numbered grouping information.
#'@param progbar Default `FALSE`. If `TRUE`, turns on progress bar.
#'@return Matrix indicating whether water was detected at that point. 1 indicates water, 0 indicates no water.
#'@export
#'@examples
#'library(magrittr)
#'#Here we even out a portion of the volcano dataset to simulate water:
#'island_volcano = volcano
#'island_volcano[island_volcano < mean(island_volcano)] = mean(island_volcano)
#'
#'#Setting a minimum area avoids classifying small flat areas as water:
#'island_volcano %>%
#'  sphere_shade(texture="imhof3") %>%
#'  add_water(detect_water(island_volcano, min_area = 400),color="imhof3") %>%
#'  plot_map()
detect_water = function(heightmap, zscale = 1, cutoff = 0.999, 
                        min_area=length(heightmap)/400,
                        max_height = NULL,
                        normalvectors=NULL,
                        keep_groups=FALSE, progbar = FALSE) {
  if(!is.null(normalvectors)) {
    zmatrix = abs(normalvectors$z)
    zmatrix = abs(zmatrix)
    zmatrix[zmatrix < cutoff] = 0
    zmatrix[zmatrix >= cutoff] = 1
    zmatrix[1,] = 0
    zmatrix[,1] = 0
    zmatrix[nrow(zmatrix),] = 0
    zmatrix[,ncol(zmatrix)] = 0
  } else {
    zmatrix = calculate_normal(heightmap,zscale=zscale,progbar=progbar)$z
    zmatrix = abs(zmatrix)
    zmatrix[zmatrix < cutoff] = 0
    zmatrix[zmatrix >= cutoff] = 1
    zmatrix[1,] = 0
    zmatrix[,1] = 0
    zmatrix[nrow(zmatrix),] = 0
    zmatrix[,ncol(zmatrix)] = 0
  }
  if(!is.null(max_height)) {
    heightmap_padded = matrix(max_height+1,nrow(heightmap)+2,ncol(heightmap)+2)
    heightmap_padded[2:(nrow(heightmap_padded)-1),2:(ncol(heightmap_padded)-1)] = heightmap
    zmatrix[t(heightmap_padded) > max_height] = 0
  }
  flipud = function(x) {
    x[nrow(x):1,]
  }
  padding = matrix(0,nrow=nrow(zmatrix)+2,ncol=ncol(zmatrix)+2)
  padding[2:(nrow(padding)-1),2:(ncol(padding)-1)] = zmatrix

  
  water_groups = fill_find_groups(padding)
  group_table = table(water_groups[water_groups>0]) 
  entries = names(group_table[group_table > min_area])
  water_groups[!(water_groups %in% entries)] = 0
  if(!keep_groups) {
    water_groups[water_groups != 0] = 1
  }
  
  water_groups2 = water_groups[c(-1,-2,-nrow(water_groups)+1,-nrow(water_groups)),c(-1,-2,-ncol(water_groups)+1,-ncol(water_groups))]
  return(t(flipud(water_groups2)))
}