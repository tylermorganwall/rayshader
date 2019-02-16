#'@title Render Camera
#'
#'@description Changes the position and properties of the camera around the scene. Wrapper around \link[rgl]{rgl.viewpoint}.
#'
#'@param theta Default `45`. Rotation angle.
#'@param phi Default `45`. Azimuth angle. Maximum `90`.
#'@param zoom Default `1`. Positive value indicating camera magnification.
#'@param fov Default `0`. Field of view of the camera. Maximum `180`.
#'@export
#'@examples
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50)
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
#'render_camera(theta = 0, phi = 90,zoom=0.7)
#'render_snapshot()
#'}
#'
#'#Shift to an front view
#'\dontrun{
#'render_camera(theta = -90, phi = 30,zoom=0.5)
#'render_snapshot()
#'}
#'
#'#Change the FOV
#'\dontrun{
#'render_camera(theta = -90, phi = 30,zoom=0.5,fov=130)
#'render_snapshot()
#'rgl::rgl.close()
#'}
render_camera = function(theta = 45, phi = 45, zoom = 1, fov = 0) {
  rgl::rgl.viewpoint( theta = theta, phi = phi, fov = fov, zoom = zoom)
}