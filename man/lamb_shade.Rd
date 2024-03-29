% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lamb_shade.R
\name{lamb_shade}
\alias{lamb_shade}
\title{Calculate Lambert Shading Map}
\usage{
lamb_shade(
  heightmap,
  sunaltitude = 45,
  sunangle = 315,
  zscale = 1,
  zero_negative = TRUE
)
}
\arguments{
\item{heightmap}{A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.}

\item{sunaltitude}{Default `45`. The azimuth angle as measured from the horizon from which the light originates.}

\item{sunangle}{Default `315` (NW). The angle around the matrix from which the light originates.}

\item{zscale}{Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis.}

\item{zero_negative}{Default `TRUE`. Zeros out all values below 0 (corresponding to surfaces facing away from the light source).}
}
\value{
Matrix of light intensities at each point.
}
\description{
Calculates local shadow map for a elevation matrix by calculating the dot 
product between light direction and the surface normal vector at that point. Each point's
intensity is proportional to the cosine of the normal vector.
}
\examples{
if(run_documentation()) {
#Generate a basic hillshade
montereybay \%>\% 
 lamb_shade(zscale=200) \%>\% 
 plot_map()
}
if(run_documentation()) {
#Increase the intensity by decreasing the zscale
montereybay \%>\% 
 lamb_shade(zscale=50) \%>\% 
 plot_map()
}
if(run_documentation()) { 
#Change the sun direction
montereybay \%>\% 
 lamb_shade(zscale=200, sunangle=45) \%>\% 
 plot_map()
}
if(run_documentation()) {
#Change the sun altitude
montereybay \%>\% 
 lamb_shade(zscale=200, sunaltitude=60) \%>\% 
 plot_map()
}
}
