#'@title Render Camera
#'
#'@description Changes the position and properties of the camera around the scene. If no 
#'values are entered, prints and returns the current values.
#'
#'@param theta Defaults to current value. Rotation angle.
#'@param phi Defaults to current value. Azimuth angle. Maximum `90`.
#'@param zoom Defaults to current value. Positive value indicating camera magnification.
#'@param fov Defaults to current value. Field of view of the camera. Maximum `180`.
#'@export
#'@examples
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale = 50, water = TRUE, waterlinecolor="white")
#'render_snapshot()
#'}
#'
#'#Shift the camera over and add a title
#'\dontrun{
#'render_camera(theta = -45, phi = 45)
#'render_snapshot(title_text = "Monterey Bay, CA",
#'                title_bar_color = "grey50")
#'}
#'
#'#Shift to an overhead view (and change the text/title bar color)
#'\dontrun{
#'render_camera(theta = 0, phi = 90, zoom = 0.9)
#'render_snapshot(title_text = "Monterey Bay, CA",
#'                title_color = "white",
#'                title_bar_color = "darkgreen")
#'}
#'
#'#Shift to an front view and add a vignette effect
#'\dontrun{
#'render_camera(theta = -90, phi = 30,zoom = 0.8)
#'render_snapshot(title_text = "Monterey Bay, CA",
#'                title_color = "white",
#'                title_bar_color = "blue",
#'                vignette = TRUE)
#'}
#'
#'#Change the field of view (fov) and make the title bar opaque.
#'\dontrun{
#'render_camera(theta = -90, phi = 30,zoom = 0.5,fov = 130)
#'render_snapshot(title_text = "Monterey Bay, CA",
#'                title_color = "black",
#'                title_bar_alpha = 1,
#'                title_bar_color = "lightblue",
#'                vignette = TRUE)
#'}
#'
#'#Here we render a series of frames to later stitch together into a movie.
#'
#'\dontrun{
#'phivec = 20 + 70 * 1/(1 + exp(seq(-5, 10, length.out = 180)))
#'phivecfull = c(phivec, rev(phivec))
#'thetavec = 270 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
#'zoomvechalf = 0.5 + 0.5 * 1/(1 + exp(seq(-5, 10, length.out = 180)))
#'zoomvec = c(zoomvechalf, rev(zoomvechalf))
#'
#'for(i in 1:360) {
#'  render_camera(theta = thetavec[i],phi = phivecfull[i],zoom = zoomvec[i])
#'  #uncomment the next line to save each frame to the working directory
#'  #render_snapshot(paste0("frame", i, ".png"))
#'}
#'#Run this command in the command line using ffmpeg to stitch together a video:
#'#ffmpeg -framerate 60 -i frame%d.png -vcodec libx264 raymovie.mp4
#'
#'#And run this command to convert the video to post to the web:
#'#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4
#'
#'#Or we can use render_movie() to do this all automatically with type="custom" (uncomment to run):
#'#render_movie(filename = tempfile(fileext = ".mp4"), type = "custom", 
#'#             theta = thetavec, phi = phivecfull, zoom = zoomvec, fov=0)
#'rgl::rgl.close()
#'}
#'
#'
#'
render_camera = function(theta = NULL, phi = NULL, zoom = NULL, fov = NULL) {
  if(is.null(theta) && is.null(phi) && is.null(zoom) && is.null(fov)) {
    allmissing = TRUE
  } else {
    allmissing = FALSE
  }
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(is.null(fov)) {
    fov = rgl::par3d()$FOV
  }
  if(is.null(zoom)) {
    zoom = rgl::par3d()$zoom
  }
  if(is.null(phi) || is.null(theta)) {
    rotmat = rot_to_euler(rgl::par3d()$userMatrix)
    if(is.null(phi)) {
      phi = rotmat[1]
    }
    if(is.null(theta)) {
      if(0.001 > abs(abs(rotmat[3]) - 180)) {
        theta = -rotmat[2] + 180
      } else {
        theta = rotmat[2]
      }
    }
  }
  rgl::rgl.viewpoint(theta = theta, phi = phi, fov = fov, zoom = zoom)
  if(allmissing) {
    return(c("theta"=theta,"phi"=phi,"zoom"=zoom,"fov"=fov))
  }
}