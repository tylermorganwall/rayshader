#'@title plot_map
#'
#'@description Displays the shaded map. If the input is an array, it uses raster's plotRGB
#'function. If it's a matrix, it uses the 
#'
#'@param hillshade Hillshade to be plotted.
#'@param rotate Default 0. Rotates the output. Possible values: 0, 90, 180, 270.
#'@param ... Additional arguments to pass to the `raster::plotRGB` function that displays the map.
#'@export
#'@examples
#'#Plotting a spherical texture map of the volcano dataset.
#'plot_map(sphere_shade(volcano))
plot_map = function(hillshade, rotate=0, ...) {
  rotatef = function(x) t(apply(x, 2, rev))
  if(!(rotate %in% c(0,90,180,270))) {
    if(length(rotate) == 1) {
      warning(paste0("Rotation value ",rotate," not in c(0,90,180,270). Ignoring"))
    } else {
      warning(paste0("Rotation argument `rotate` not in c(0,90,180,270). Ignoring"))
    }
    number_of_rots = 0
  } else {
    number_of_rots = rotate/90
  }
  if(class(hillshade) == "array") {
    if(number_of_rots != 0) {
      newarray = hillshade
      newarrayt = array(0,dim=c(ncol(hillshade),nrow(hillshade),3))
      for(i in 1:number_of_rots) {
        for(j in 1:3) {
          if(i == 2) {
            newarray[,,j] = rotatef(newarrayt[,,j])
          } else {
            newarrayt[,,j] = rotatef(newarray[,,j])
          }
        }
      }
      if(number_of_rots == 2) {
        hillshade = newarray
      } else {
        hillshade = newarrayt
      }
    }
    suppressWarnings(raster::plotRGB(raster::brick(hillshade),scale=1, ...))
  } else if(class(hillshade) == "matrix") {
    flipud = function(matrix) {
      matrix[nrow(matrix):1,]
    }
    fliplr = function(matrix) {
      matrix[,ncol(matrix):1]
    }
    if(number_of_rots != 0) {
      for(j in 1:number_of_rots) {
        hillshade = rotatef(hillshade)
      }
    }
    array_from_mat = array(flipud(t(hillshade)),dim=c(ncol(hillshade),nrow(hillshade),3))
    suppressWarnings(raster::plotRGB(raster::brick(array_from_mat),scale=1, ...))
  } else {
    stop("`hillshade` is neither array nor matrix--convert to either to plot.")
  }
}