#'@title Darken Color
#'
#'@description Convert RGB to XYZ color
#'
#'@param col RGB colors
#'@return Euler angles in degrees. c(phi,theta,yaw)
#'@keywords internal
#'@examples
#'#None
darken_color = function(col, darken = 0.3) {
  as.numeric(
    grDevices::convertColor(
      as.numeric(
        grDevices::convertColor(
          convert_color(col),
        from = "sRGB", to = "Luv")
      ) * c(darken,1,1),
    from = "Luv", to = "sRGB")
  )
}
