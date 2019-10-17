#'@title Rotation Matrix to Euler Angle Transform
#'
#'@description Transforms a rotation matrix (R_X R_Y R_Z) into Euler angles. 
#'
#'@param rotmat The rotation matrix. Can be 3x3 or 4x4 (homogeneous coordinates).
#'@return Euler angles in degrees. c(phi,theta,yaw)
#'@keywords internal
#'@examples
#'#None
rot_to_euler = function(rotmat) {
  x = atan2(-rotmat[2,3],rotmat[3,3])
  y = -asin(rotmat[1,3])
  sinz = cos(x) * rotmat[2,1] + sin(x) * rotmat[3,1]
  cosz = cos(x) * rotmat[2,2] + sin(x) * rotmat[3,2]
  z = atan2(sinz,cosz)
  if(x*180/pi < -90) {
    x = x + pi
  }
  if(x*180/pi > 90) {
    x = x - pi
  }
  return(c(x, y, z)*180/pi)
}