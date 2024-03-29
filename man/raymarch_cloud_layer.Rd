% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_clouds.R
\name{raymarch_cloud_layer}
\alias{raymarch_cloud_layer}
\title{Calculate a single raymarched cloud layer}
\usage{
raymarch_cloud_layer(
  heightmap,
  sun_altitude = 90,
  sun_angle = 315,
  levels = 8,
  start_noise = 0,
  end_noise = 10,
  start_altitude_real = 0,
  end_altitude_real = 0,
  time = 0,
  alpha_coef = 0.8,
  layers = 10,
  offset_x = 0,
  offset_y = 0,
  scale_x = 1,
  scale_y = 1,
  scale_z = 1,
  step = 100,
  freq = 0.01/2,
  coef = 0.05,
  seed = 1
)
}
\value{
image array
}
\description{
Calculate a single raymarched cloud layer
}
\examples{
#Fake example
}
\keyword{internal}
