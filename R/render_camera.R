#'@title Render Camera
#'
#'@description Changes the position and properties of the camera around the scene. Wrapper around \link[rgl]{rgl.viewpoint}.
#'
#'@param theta Default `45`. Rotation angle.
#'@param phi Default `45`. Azimuth angle. Maximum `90`.
#'@param zoom Defaults to current value. Positive value indicating camera magnification.
#'@param fov Defaults to current value. Field of view of the camera. Maximum `180`.
#'@export
#'@examples
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale = 50)
#'render_snapshot()
#'}
#'
#'#Shift the camera over
#'\dontrun{
#'render_camera(theta = -45, phi = 45)
#'render_snapshot()
#'}
#'
#'#Shift to an overhead view
#'\dontrun{
#'render_camera(theta = 0, phi = 90,zoom = 0.7)
#'render_snapshot()
#'}
#'
#'#Shift to an front view
#'\dontrun{
#'render_camera(theta = -90, phi = 30,zoom = 0.5)
#'render_snapshot()
#'}
#'
#'#Change the FOV
#'\dontrun{
#'render_camera(theta = -90, phi = 30,zoom = 0.5,fov = 130)
#'render_snapshot()
#'rgl::rgl.close()
#'}
#'
#'#Here we render a series of frames to later stitch together into a movie.
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale = 50)
#'
#'phivec = 20 + 70 * 1/(1 + exp(seq(-5, 10, length.out = 180)))
#'phivecfull = c(phivec, rev(phivec))
#'thetavec = 270 + 90 * sin(seq(0,359,length.out = 360) * pi/180)
#'zoomvec = 0.5 + 0.5 * 1/(1 + exp(seq(-5, 10, length.out = 180)))
#'zoomvecfull = c(zoomvec, rev(zoomvec))
#'
#'for(i in 1:360) {
#'  render_camera(theta = thetavec[i],phi = phivecfull[i],zoom = zoomvecfull[i])
#'  #uncomment the next line to save each frame to the working directory
#'  #render_snapshot(paste0("frame", i, ".png"))
#'}
#'#Run this command in the command line using ffmpeg to stitch together a video:
#'#ffmpeg -framerate 60 -i frame%d.png -vcodec libx264 raymovie.mp4
#'
#'#And run this command to convert the video to post to the web:
#'#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4
#'}
render_camera = function(theta = 45, phi = 45, zoom = NULL, fov = NULL) {
  if(is.null(fov)) {
    fov = rgl::par3d()$FOV
  }
  if(is.null(zoom)) {
    zoom = rgl::par3d()$zoom
  }
  rgl::rgl.viewpoint(theta = theta, phi = phi, fov = fov, zoom = zoom)
}