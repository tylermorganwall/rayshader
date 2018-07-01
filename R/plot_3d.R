#'@title plot_3d
#'
#'@description Displays the shaded map in 3D with the `rgl` package. 
#'
#'@param hillshade Hillshade/image to be added to 3D surface map.
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param theta Default 0. Rotation around z-axis.
#'@param phi Default `45`. Azimuth angle.
#'@param fov Field-of-view angle in degrees.
#'@param zoom Default `1`. Zoom factor.
#'@param background Default `grey10`. Color of the background.
#'@param windowsize Default `c(600,600)`. Width and height of the `rgl` device displaying the plot.
#'@param ... Additional arguments to pass to the `rgl::par3d` function.
#'@import rgl
#'@export
#'@examples
#'#Plotting a spherical texture map of the volcano dataset.
#'plot_3d(sphere_shade(volcano,texture="desert"),volcano, zscale=5)
plot_3d = function(hillshade, heightmap, zscale=1, theta=0, phi = 45, fov=60 , zoom = 1, background="grey10", windowsize= c(600,600), ...) {
  if(is.null(heightmap)) {
    stop("heightmap argument missing--need to input both hillshade and original elevation matrix")
  }
  tempmap = tempfile()
  write_png(hillshade,tempmap)
  rgl.surface(1:nrow(heightmap),1:ncol(heightmap),heightmap[,ncol(heightmap):1]/zscale,texture=paste0(tempmap,".png"),lit=FALSE)
  rgl.bg(color=background)
  rgl.viewpoint(zoom=zoom,phi=phi,theta=theta,fov=fov)
  par3d("windowRect" = c(0,0,windowsize), ...)
}