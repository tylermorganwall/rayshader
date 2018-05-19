
rayshader
=========

<img src="man/figures/volcano_r_small.gif" ></img>

Overview
--------

**rayshader** is an open source package for producing global shadow maps of elevation matrices.

Installation
------------

``` r
# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")
```

Functions
---------

-   `rayshade` uses user specified light directions to calculate a global shadow map for an elevation matrix. By default, this also scales the light intensity at each point by the dot product of the mean ray direction and the surface normal (this can be turned off by setting `lambert=FALSE`.
-   `lambshade` uses a single user specified light direction to calculate a local shadow map based on the dot product between the surface normal and the light direction for an elevation matrix.

Usage
-----

``` r
library(rayshader)
#Here we produce an shadow map of the `volcano` elevation map with the light from the NE.
volcanoshadow = rayshade(heightmap = volcano, 
    anglebreaks = seq(55,65,10), 
    sunangle = 315, 
    maxsearch = 100)
image(volcanoshadow,col=colorRampPalette(c("black","white"))(100))
    
#Turn on multicore support (no progress bar).
volcanoshadow = rayshade(heightmap = volcano, 
    anglebreaks = seq(55,65,10), 
    sunangle = 315, 
    maxsearch = 100,
    multicore=TRUE)
    
#Turn off Lambertian shading to get a shadow map solely based on the raytraced shadows.
volcanoshadow = rayshade(heightmap = volcano, 
    anglebreaks = seq(30,40,10), 
    sunangle = 45, 
    maxsearch = 100,
    lambert = FALSE)
    
image(volcanoshadow,col=colorRampPalette(c("black","white"))(100))
```
