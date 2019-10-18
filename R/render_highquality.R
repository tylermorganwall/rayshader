#'@title Render High Quality
#'
#'@description Changes the position and properties of the camera around the scene. Wrapper around \link[rgl]{rgl.viewpoint}.
#'
#'@param filename Name of temporary filename to store OBJ file. 
#'@import rayrender
#'@export
#'@examples
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale = 50)
#'render_highquality()
#'}
render_highquality = function(filename=NULL, 
                              light = FALSE, lightdirection = 315, lightangle = 45, 
                              offset_camera = c(0,0,0), offset_lookat = c(0,0,0), ...) {
  if(is.null(filename)) {
    stop("filename needs to be provided--")
  }
  fov = rgl::par3d()$FOV
  rotmat = rot_to_euler(rgl::par3d()$userMatrix)
  projmat = rgl::par3d()$projMatrix
  phi = rotmat[1]
  if(0.001 > abs(abs(rotmat[3]) - 180)) {
    theta = -rotmat[2] + 180
  } else {
    theta = rotmat[2]
  }
  observer_radius = rgl::par3d()$observer[3]
  lookvals = rgl::par3d()$bbox
  if(fov == 0) {
    ortho_dimensions = c(2/projmat[1,1],2/projmat[2,2])
  } else {
    ortho_dimensions = c(1,1)
  }
  bbox_center = c(mean(lookvals[1:2]),mean(lookvals[3:4]),mean(lookvals[5:6]))
  observery = sinpi(phi/180) * observer_radius
  observerx = cospi(phi/180) * sinpi(theta/180) * observer_radius 
  observerz = cospi(phi/180) * cospi(theta/180) * observer_radius 
  lookfrom = c(observerx,observery,observerz) + offset_camera
  if(!file.exists(filename)) {
    if(substring(filename, nchar(filename)-3,nchar(filename)) != ".obj") {
      filename = paste0(filename,".obj")
    }
    save_obj(filename)
  }
  scene = obj_model(filename, x=-bbox_center[1],y = -bbox_center[2],z=-bbox_center[3], texture=TRUE)
  if(light) {
    lightintensity = 500*(observer_radius^2)/(120^2)
    scene = add_object(scene, sphere(x=observer_radius*5 * cospi(lightangle/180) * sinpi(lightdirection/180),
                                     y=observer_radius*5 * sinpi(lightangle/180),
                                     z=-observer_radius*5 * cospi(lightangle/180) * cospi(lightdirection/180), radius=observer_radius/5,
                                     material = diffuse(lightintensity = lightintensity, implicit_sample=TRUE)))
  }
  if(is.null(filename)) {
    render_scene(scene, lookfrom = lookfrom, lookat = offset_lookat, fov = fov, ortho_dimensions = ortho_dimensions, ...)
  } else {
    render_scene(scene, lookfrom = lookfrom, lookat = offset_lookat, fov = fov, ortho_dimensions = ortho_dimensions, ...)
  }
}