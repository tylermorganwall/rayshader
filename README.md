
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

-   `rayshade` uses user specified light directions to calculate a global shadow map for an elevation matrix.

Usage
-----

``` r
library(rayshader)
volcanoshadow = rayshade(heightmap = volcano, 
  anglebreaks = seq(-90,90,1)*pi/180, 
  sunangle = 45*pi/180, 
  maxsearch = 100,
  zscale = 1)
image(volcanoshadow,col=colorRampPalette(c("black","white"))(100))
```
