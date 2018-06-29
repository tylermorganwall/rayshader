
rayshader
=========

<img src="man/figures/volcano_r_small.gif" ></img>

Overview
--------

**rayshader** is an open source package for producing hillshaded maps of elevation matrices with raytracing and spherical texture mapping.

Installation
------------

``` r
# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")
```

Functions
---------

Rayshader has five functions related to hillshading:
<p style="min-height: 110px">
<img align="right" height="110" width="102" src="man/figures/ray_small.png"> • `ray_shade` uses user specified light directions to calculate a global shadow map for an elevation matrix. By default, this also scales the light intensity at each point by the dot product of the mean ray direction and the surface normal (also implemented in function `lamb_shade`, this can be turned off by setting `lambert=FALSE`.
</p>
<p style="min-height: 110px">
<img align="right" height="110" width="102" src="man/figures/sphere_small.png"> • `sphere_shade` maps an RGB texture to a hillshade by spherical mapping. A texture can be generated with the `create_texture` function, or loaded from an image. `sphere_shade` also includes 7 built-in palettes: "imhof1","imhof2","imhof3",imhof4","desert","bw","unicorn".
</p>
<p style="min-height: 110px">
<img align="right" height="102" width="102" src="man/figures/imhof_small.png"> • `create_texture` programmatically creates texture maps given five colors: a highlight, a shadow, a left fill light, a right fill light, and a center color for flat areas. The user can also optionally specify the colors at the corners, but `create_texture` will interpolate those if they aren't given.
</p>
<p style="min-height: 110px">
<img align="right" height="110" width="102" src="man/figures/amb_small.png"> • `ambient_shade` creates an ambient occlusion shadow layer, darkening areas that have less scattered light from the atmosphere. This results in valleys being darker than flat areas and ridges.
</p>
<p style="min-height: 110px">
<img align="right" height="110" width="102" src="man/figures/lamb_small.png"> • `lamb_shade` uses a single user specified light direction to calculate a local shadow map based on the dot product between the surface normal and the light direction for an elevation matrix.
</p>
<p style="min-height: 110px">
<img align="right" height="110" width="102" src="man/figures/alltogether_small.png"> • `add_shadow` takes two of the shadow maps above and combines them, scaling the second one (or, if the second is an RGB array, the matrix) as specified by the user.
</p>
Rayshader also has two functions to detect and add water to maps:

-   `detect_water` uses a flood-fill algorithm to detect bodies of water of a user-specified minimum area.
-   `add_water` uses the output of `detect_water` to add a water color to the map. The user can input their own color, or pass the name of one of the pre-defined palettes from `sphere_shade` to get a matching hue.

And two functions to display and save your maps:

-   `plot_map` Plots the current map. Accepts either a matrix or an array.
-   `write_png` Writes the current map to disk with a user-specified filename.

All of these functions are designed to be used with the pipe magrittr `%>%`.

Usage
-----

``` r
library(rayshader)
library(magrittr)

#Here, I load a map with the raster package:
tif_file = raster::raster("dem_01.tif")

#And convert it to a matrix:
elmat = matrix(raster::extract(tif_file,raster::extent(tif_file),buffer=1000),
               nrow=ncol(tif_file),ncol=nrow(tif_file))

#We first texture the map with sphere_shade and one of rayshader's built in textures, "desert."
#By default, the highlight is towards the NW.
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()
```

![](tools/readme/first.jpg)

``` r
#sphere_shade can shift the sun direction to the NE:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()
```

![](tools/readme/second.jpg)

``` r
#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  plot_map()
```

![](tools/readme/third.jpg)

``` r
#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,sunangle = 45)) %>%
  plot_map()
```

![](tools/readme/fourth.jpg)

``` r
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,sunangle = 45)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_map()
```

![](tools/readme/fifth.jpg)
