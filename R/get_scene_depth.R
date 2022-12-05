#' Get the scene depth from rgl to offset rayrender scenes
#'
#' @return numeric
#' @keywords internal
get_scene_depth = function() {
  rotmat = rot_to_euler(rgl::par3d()$userMatrix)
  phi = rotmat[1]
  if(90 - abs(phi) < 1e-3) {
    phi = -phi
  }
  if(0.001 > abs(abs(rotmat[3]) - 180)) {
    theta = -rotmat[2] + 180
    movevec = rgl::rotationMatrix(-rotmat[2]*pi/180, 0, 1, 0) %*%
      rgl::rotationMatrix(-phi*pi/180, 1, 0, 0) %*% 
      rgl::par3d()$userMatrix[,4]
  } else {
    theta = rotmat[2]
    movevec = rgl::rotationMatrix(rotmat[3]*pi/180, 0, 0, 1) %*%
      rgl::rotationMatrix(-rotmat[2]*pi/180, 0, 1, 0) %*%
      rgl::rotationMatrix(-phi*pi/180, 1, 0, 0) %*% 
      rgl::par3d()$userMatrix[,4]
  }
  movevec = movevec[1:3]
  lookvals = rgl::par3d()$bbox
  bbox_center = c(mean(lookvals[1:2]),mean(lookvals[3:4]),mean(lookvals[5:6])) - movevec
  return(bbox_center[2])
}
