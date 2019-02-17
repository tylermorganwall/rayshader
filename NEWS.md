# rayshader 0.10.0:

## Additions
* Add `render_camera()` function

## Changes
* Fixed some documentation issues

# rayshader 0.9.2:

## Additions
* Add additional arguments to windowsize in `plot_3d()` to specify location of viewport

# rayshader 0.9.1:

## Changes
* Fix out-of-bounds memory issue with water layer generation

# rayshader 0.9.0:

## Additions
* Added `render_snapshot` function capture and save images of the current rgl view to either file or the current device.
* Added a `pkgdown` website for the package.
* Added a `NEWS.md` file to track changes to the package.


## Changes
* Documentation updates and additions
* Updated README 

# rayshader 0.8.1: 2018-12-20
* `na.rm=FALSE` bug and `add_overlay` issue fixed
*  Fix add_overlay dimensions when no transparency layer present

# rayshader 0.8.0: 2018-12-14

## Additions

* Updated `plot_3d` to draw `NA` entries in the elevation matrix as holes in the 3D model. This can be used to slice the model into any shape.
* Updated `plot_3d` to include the new `baseshape` argument, which has several built-in shapes `c("circle","hex")` to overlay onto a matrix. This removes points in the elevation matrix outside the masked region.
* Added `lineantialias` option to `plot_3d` to allow the user to turn on antialiasing for the lines in the plot.

## Changes

* Removed `shadow_exist` and `remove_water` options from `save_3dprint` to improve robustness. User now has to remove those 3D elements themselves when creating .stl file.
* Fixed `add_overlay` when the input is a matrix
* Updated documentation for `plot_3d` with examples.
* Fixed reference to `save_png` in README

# rayshader 0.7.0: 2018-11-21

## Additions

* Added `render_label()` function to add text labels to 3D plots.

## Changes

* Updated README
* Fixed all `progbar` arguments to be either `FALSE` or `interactive()` by default

# rayshader 0.6.4: 2018-11-08
* Changed progress bar to appear only when the user is in an interactive session (thanks @hadley)
* Fixed bug with water edges not following non-zero water depth
* Cleaned up documentation

# rayshader 0.6.3: 2018-10-27
* Speed improvements for 3D rendering with `plot_3d`
* Fixed small gaps and overlays in rendered water and edge lines in `plot_3d`

# rayshader 0.6.2: 2018-10-25
* Implemented binary search for intersection, massive speed increase for ray_shade()

# rayshader 0.6.1: 2018-10-25
* Moved Rcpp::checkUserInterrupt() to outer loop to speed up raytracing (thanks @brodieG)

# rayshader 0.6.0: 2018-09-23

## Additions
* Added `render_depth()`, a post-processing effect to add depth of field to 3D maps.
* Added `render_water()`, which either adds or replaces a water layer on the existing 3D map.
* Added `add_overlay()`, which adds a transparent or semi-transparent overlay to the current hillshade.
* Added attribute to `montereybay` to mark it as internal data (for better user messages)

## Changes
* Changed `save_3dprint()` to include the option to remove the shadow and water layers.

# rayshader 0.5.4: 2018-09-20

* Altered coordinates of plot_map to align with the center of the grid

# rayshader 0.5.3: 2018-09-18

* Fix for add_water hex color bug

# rayshader 0.5.2: 2018-09-16

* Fixed aspect ratio in plot_map()

# rayshader 0.5.1: 2018-09-06

## Changes
* Fixed out-of-bounds bug in C++ normal vector generation
* `plot_map()` no longer crashes R when "rotate" is a matrix rather than a single numeric value.
* `plot_3d()` now displays warning if hillshade argument contains any values below 0 or more than 1.

# rayshader 0.5.0: 2018-08-25

## Additions
* Added `save_3dprint`, a function that exports 3D maps to a stereolithography (STL) format for 3D printing.
* Add hex logo to README

## Changes
* Fixed small overhand on 3D base.
* Changed `write_png` to `save_png`
* Corrected orientation of polygons in `plot_3d`

# rayshader 0.4.7: 2018-08-24

## CRAN fixes
* Fixed lack of examples in some functions for CRAN and changed \dontrun{} calls to \donttest{}

## Changes
* Removed `remove_edges` option: the x/y dimensions of the outputs and inputs are now always consistent.

# rayshader 0.4.6: 2018-08-20

* `make_shadow` bugfix

# rayshader 0.4.5: 2018-08-17

## Additions
* Implemented custom C++ version of `akima::bilinear.grid` to remove non-GPL software dependency
* Add ability for `add_water` to work with matrices

## Changes
* Turn off examples for `plot_3d`
* Fix issue with non-exporting internal Rcpp functions to foreach
* Added description to ray
* Update DESCRIPTION

# rayshader 0.4.4: 2018-08-16

## Additions
* Added shadow color option

## Changes
* Fixed background color to blend in with shadow
* Fixed description of error in sphere_shade
* Small updates to documentation

# rayshader 0.4.3: 2018-08-09

## Additions
* Added default settings for zscale when `montereybay` dataset used and zscale is not passed in by user

# rayshader 0.4.2: 2018-08-08

## Additions
* Added example dataset `montereybay`

# rayshader 0.4.1: 2018-08-06

* Update pipe import from magrittr

# rayshader 0.4.0: 2018-08-06

## Additions
* Added `plot_3d`, a function to take hillshades/textures and visualize them with an elevation matrix in 3D. Includes water visualization and the ability to view the map as a solid object.
* Added zscale argument to detect_water.

## Changes
* Changed default orientation to match the orientation of the matrix.
* Changed detect_water and calculate_normals to not show a progress bar by default.
* Changed ambient_shade to default to 1-45 degrees, instead of 0-45.
* Changed write_png to stop when user doesn’t provide a filename.
* Updated README

# rayshader 0.3.1: 2018-06-30

* `ray_shade` and `ambient_shade` - Added option to pass arguments to makeCluster function for parallel execution

# rayshader 0.3.0: 2018-06-28

## Additions
* Added `sphere_shade` function to apply spherical texture maps to elevation matrices.
* Added `create_texture` function to allow user to programmatically create texture maps by specifying colors.
* Added `ambient_shade` function to calculate ambient occlusion shadow maps.
* Added `add_shadow` function to add shadow to existing shadow matrices/texture arrays.
* Added `calculate_normal` function to pre-calculate normal vectors of elevation matrix for faster spherical shading.
* Added `detect_water` function to automatically detect bodies of water (of a specified size) on an elevation matrix.
* Added `write_png` function to save hillshaded maps to file.
* Added `plot_map` function to parse and plot resulting hillshaded maps.
* Added the ability to add a mask to raytracing and ambient occlusion so only certain areas are raytraced.

## Changes
* Changed names of functions to be snake_case.
* Various bugfixes


# rayshader 0.2.1: 2018-05-20

* Out of bounds memory bug fix 

# rayshader 0.2.0: 2018-05-19

## Additions
* Added Lambertian shading by default to the `rayshade` function.
* Added standalone `lambshade` function.
* Added progress bar support.
* Added option to remove edges. Default TRUE.

## Changes
* Changed angles all to degrees instead of radians.
* Removed `verbose` option.

# rayshader 0.1.0: 2018-05-13

* Initial commit. Basic functionality implemented and documentation written.