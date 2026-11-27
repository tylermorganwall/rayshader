# rayshader 1.0.0

## Spatial rasters and scene coordinates

- Build a terrain scene directly from a georeferenced elevation raster.
  `montereybay_spatial |> sphere_shade() |> plot_3d()` carries the elevation,
  extent, and coordinate reference system (CRS) through shading into the
  scene. Heightmaps can be matrices, terra or raster package objects, or raster
  filenames. Rayshader infers `zscale` from cell spacing, including the spacing
  in meters at the center of a longitude/latitude raster. Use
  `vertical_exaggeration` to scale relief independently. Matrix-based
  workflows and explicit spatial arguments are still supported. The default
  geographic aspect correction accounts for unequal east-west and north-south
  cell spacing.
- Add layers in their own coordinates. After `plot_3d()`, a call such as
  `render_path(roads)` can use the scene's cached terrain, extent, CRS, and
  vertical scale to transform the roads and place them on the surface.
  Spatial layers support filtering to the scene extent with `filter_to_extent`.
  `get_scene_metadata()` exposes the coordinate information for the active scene.
- Take a shaded map back into GIS with `spatialize_image()`. It returns a
  georeferenced `terra::SpatRaster` with RGB bands and optional alpha and
  original elevation bands. `generate_raster_overlay()` aligns a raster
  overlay to a map, and `resize_spatial()` changes raster resolution while
  preserving extent and CRS.
- The bundled `montereybay_spatial`, `volcano_spatial`, and `volcano_trails`
  provide elevations and trail geometry for spatial examples. The original
  `montereybay` elevation matrix remains available.

## Integrating 3D data with ggplot2

- `plot_gg()` records how ggplot2 maps data into each panel. Points, paths,
  buildings, and extruded polygons can now share coordinates and heights
  with the original plot, including maps drawn with `coord_sf()`. Use
  `flat_substrate = TRUE` for a flat base beneath the 3D geometry.
- `render_polygons()` maps a height column through the plot's height scale.
  A subset uses the same limits, transformation, binning, and inversion as
  the full data, including on flat plots with hidden legends.
  `color = "height"` also respects nonlinear scales. Supply `scale_data`
  for manual scaling or `top` and `bottom` for heights in scene units.
- `render_zaxis()` adds a vertical axis to terrain and ggplot scenes. Set
  breaks, labels, and a title in the units of the data being shown. Faceted
  ggplots retain panel-specific coordinate information for layers and cameras.

## Lakes, shorelines, and streams

- `render_water()` and `plot_3d()` now default to
  `water_render_method = "polygon"` for exact polygon footprints. Each part
  extends from the lowest terrain beneath it to a level surface, preserving
  islands, small inlets, and exposed walls. Choose `"optimized"` to fit a
  lake's elevation by matching flooded terrain area to its mapped footprint,
  `"raster"` for a cell-based shoreline, or `"legacy"` for the earlier
  box/grid renderer. Code using the development version's area-fitting
  `"polygon"` method should select `"optimized"`.
- `render_streams()` draws streams along the terrain with widths that can
  vary by feature. Set their units with `width_units`. `render_highquality(joined_stream_mesh = TRUE)` can join
  the stream geometry. For reflected sunlight on rippled water, use
  `water_material = "microfacet"` in `render_highquality()`. The default
  material is `"glassy"`.
- `add_water()` accepts spatial masks such as `montereybay_spatial < 0`.
  It checks alignment, preserves the cached elevations, and leaves cells
  with missing mask values alone. `shift_terrain()` raises or lowers terrain
  beneath spatial features by a constant or a per-feature amount, for example
  to make room for a lake bed.

## Roads, trails, and people at map scale

- `render_roads()` builds pavement and lane markings at real-world widths
  and spacing. It reads OpenStreetMap lane, bridge, and layer attributes
  and models vertical road profiles from layer ordering, grades, and
  clearance constraints. Connected ramps meet at junctions even where
  several roads occupy different levels.
- `terrain_grading = TRUE` cuts the terrain beneath roads so that surface
  junctions can stay near ground level. Later layers use the same graded
  terrain as the visible scene. `render_trails()` follows the surface with
  a fixed width or a width column and a choice of width units.
- `render_people()` places posed human figures at geographic locations or
  along paths. Both model variants stand 1.8 meters tall, and other poses
  retain their relative proportions. Use `simple_rgl = TRUE` to preview a
  crowd as points. High-quality renders share full geometry among people
  with the same pose, model variant, and material.

## Spatial objects, shading, and Earth curvature

- `render_points()` and `render_path()` accept spatial points and lines.
  `render_label()` accepts points and multipoints and can label polygons at
  their centroids, with text from a data column and a choice of font.
  `render_polygons()` and `render_beveled_polygons()` use
  cached terrain and bounds. `render_buildings()` places footprints using
  CRS-aware filtering, height columns, roof angles, and roof materials.
  `render_obj()` can place models from spatial point data and repeat a single
  location at several altitudes. `render_tree()` accepts a tree-height column
  and offers a lightweight point preview with `preview_simple = TRUE`.
- `haze_shade()` creates a transparent haze overlay from an exponential
  atmospheric model. `radiance_shade()` renders a top-down lighting pass from
  a DEM, cached shading data, or the active scene. Haze retains the raster's
  inferred `zscale`, extent, and CRS for subsequent calls to `plot_3d()`.
  `generate_altitude_overlay(relative = TRUE)` sets transitions as fractions
  of the elevation range.
- For regional scenes, `plot_3d(earth_curvature = TRUE)` bends the terrain
  relative to the center of its bounding box using a spherical Earth.
  Vertical exaggeration scales terrain relief independently of curvature.
  Spatial layers reuse the curvature setting for water, terrain-relative
  objects, and explicit altitudes. Inspect it with
  `get_scene_metadata()$earth_curvature`. It is off by default.
- `plot_3d(normals = FALSE)` skips smooth terrain vertex normals and uses
  face normals in high-quality renders and OBJ exports. Other objects keep
  their own normals. `shadow_texture_size` controls the maximum resolution
  of the blurred base shadow.

## High-quality rendering and animation

- Set a camera position using map coordinates in `render_camera()` or
  `render_highquality()`. High-quality renders also accept explicit
  orthographic dimensions and per-object-type materials through `rgl_materials`.
- Give `render_highquality()` a `datetime` to light a georeferenced scene
  for its location and time. Depending on the sky model, the sky can include
  stars, planets, and the Moon's phase and brightness. For a faceted map,
  select a `panel` directly or through a spatial camera target or location.
  The sky uses that panel's map bounds and display CRS.
- The default `text_render = "screen"` draws camera-facing labels and annotation lines
  without casting shadows on the scene. Labels keep their screen size while
  their anchors follow the map. They appear in the interactive preview.
  `deferred_render = TRUE` gives you time to set up the camera before sampling.
- `render_movie_hq()` renders camera animations from keyframes. Returned
  scenes carry their camera, sky, environment, and integrator settings into
  the movie. With animation overlay support in rayrender, screen-space labels,
  leader lines, and z-axis ticks are projected and checked for occlusion on
  each frame.
- `render_highquality(return_scene = TRUE)` returns the resolved camera and
  render defaults. Collect texture assets explicitly with
  `rayrender::write_usd()` when exporting. Native sky lighting uses
  rayrender's default integrator unless one is supplied.
- `light = TRUE` uses infinite disk lights. `light_size` is the full angular
  diameter in degrees (default `5`), independent of scene size and camera
  position. Older rayrender versions without disk lights use spherical lights.

## Updating existing code

- Public arguments with joined descriptive words now use snake case,
  including `window_size`, `base_shape`, `solid_depth`, `shadow_color`,
  `water_color`, `water_alpha`, `water_line_color`, `water_input`,
  `text_size`, `sun_angle`, `sun_altitude`, `normal_vectors`, and
  `progress_bar`. Legacy spellings passed through `...` warn with the
  replacement name. Passing both spellings raises an error. Established
  names such as `heightmap`, `hillshade`, `zscale`, `filename`, `linewidth`,
  and `camera_lookat` keep their spelling. In `sphere_shade()`, use
  `vertical_exaggeration` in place of the deprecated `color_intensity`.
- `render_zaxis()` uses `data`, `location`, `breaks`, `labels`, `title`, and
  styling arguments without the development version's `zaxis_` prefix.
- Rayshader no longer re-exports `%>%` or exports `run_documentation()`.
  Use R's native `|>` for simple pipelines or import `%>%` from magrittr.
- Color arguments also accept strings of the form `"rgb(20, 100, 255)"`,
  with channels from 0 to 255 and optional whitespace. Named, hexadecimal,
  and numeric colors retain their behavior.

## Fixes and performance improvements

### Terrain, ggplot2, and cameras

- `sphere_shade()` bounds texture lookups on steep terrain.
  `calculate_normal()` returns unit-length edge normals oriented upward
  at all four corners.
- Single-panel numeric `geom_spatraster()` height layers retain source
  precision, eliminating 8-bit elevation steps and gaps beneath buildings.
  Mixed and faceted height layers still use the image-based path.
  `plot_gg()` reuses the built height plot so that raster statistics, panel
  metadata, and the drawn image agree.
- Geographic clipping follows straight panel boundaries. Polygon bases
  respect vertical scaling, and `render_multipolygonz()`, `render_raymesh()`,
  and `render_obj()` align absolute-elevation meshes horizontally with
  points and roads at different values of `zscale` and vertical exaggeration.
- High-quality renders honor explicit `fov` values and choose a valid up
  vector for straight-down cameras. Screen-space text matches rgl's vertical
  alignment, including top z-axis titles and offsets beyond the text bounds.
- Scene conversion takes one snapshot of rgl object IDs and skips unrelated
  preview materials. `verbose = TRUE` reports conversion and rayrender
  construction timings.

### Water geometry

- Polygon meshing uses native `libgeos` intersections only for triangles
  crossing the shoreline. Threads share one terrain snapshot and reuse
  terrain coverage and missing-data boundaries. `options(cores = ...)`
  controls concurrency and `parallel = FALSE` selects one thread.
  Diagnostics follow input order. Native terrain sampling preserves water
  levels, waterlines, and exact terrain intersections.
- Mask boundaries have closed sidewalls. Meshing checks interior terrain
  minima, removes roundoff-only shoreline vertices, and separates bottom
  caps from coplanar terrain. `water_surface_tolerance` controls clearance
  for nearly coincident tops. Mixed geometry collections retain polygon
  parts and their original elevations.
- Raster water aligns with pixel centers on spatial ggplots and uses the
  scene's actual terrain triangulation, including simplified terrain.
  Inferred levels get a small clearance, while explicit levels and aligned
  raster elevations retain their precision. Surface winding is corrected,
  and collapsed shoreline triangles are discarded. `add_water()` applies
  matrix and raster masks without horizontally mirroring them.
- Exact polygon water uploads as one rgl triangle object even for disconnected
  features. Redraws are deferred until the batch and optional lines are ready.
  The previous redraw setting is restored on completion, error, or interruption.

### Roads and bridges

- Bridge tags activate profiles even without layer tags or crossings. Bridge
  inputs retain junctions with untagged approaches and branching ramps.
  Covered surface roads tagged `tunnel = "building_passage"` stay at ground
  level unless explicitly marked underground.
- Road profiles preserve multipart, loop, and clipped segment identity through
  solving, grading, and meshing. Ground clearance is checked between vertices
  and across pavement width. Sharp and reversing joins produce finite terrain
  samples, and clipped endpoints tolerate micrometer-scale rounding. Small
  feasible networks converge more reliably, and diagnostics distinguish a
  solver failure from infeasible constraints.
- Opaque pavement is 0.11 elevation units thick, independent of placement
  clearance. Both use the effective `zscale`, with a small precision margin
  against z-fighting. Graded terrain no longer hides rgl road-line previews.
- Indexed native batches find intersections, continuations, endpoints, and
  crossing events without all-pairs searches. Profile preparation avoids
  repeated CRS parsing and spatial-table copies. Serial and parallel paths
  share a terrain snapshot across densification, cross-sections, bridge
  contacts, and stabilization retries. A new call sees any terrain edits.
  Road replacement defers redraws. `verbose = TRUE` reports preparation
  and solver stages before meshing.

### Buildings, trees, and native builds

- `render_multipolygonz()` and `save_multipolygonz_to_obj()` preserve
  concave faces and holes on horizontal, sloping, and vertical surfaces.
  Ordinary faces are triangulated in native batches. Crossed or overlapping
  boundaries are repaired in the face plane, and a summary warning identifies
  faces skipped after triangulation fails or yields zero area. Set
  `options(rayshader.multipolygonz_repair = FALSE)` to drop ambiguous
  intersections. `verbose = TRUE` reports preparation and upload stages.
  `render_polygons()` uses `rayvertex::extruded_polygon_mesh()` for extrusion.
- `render_tree()` defaults to `load_normals = FALSE` to avoid shading artifacts
  on low-poly crowns. Set it to `TRUE` to use the model's normals. Negative
  sizes are discarded after converting absolute elevations, keeping each
  tree's color, dimensions, and rotation together.
- Repeated trees and OBJ models share materials. Unrotated instances are built
  in native batches, preserving geometry, normals, and texture coordinates.
  Rotated instances retain the general transformation path.
- Fixed GCC array-bounds warnings in native stream and road code, including
  stream mesh compilation with Rtools45 on Windows.

# rayshader 0.10.0:

## Additions
* Add `render_camera()` function

## Changes
* Fixed some documentation issues

# rayshader 0.9.2:

## Additions
* Add additional arguments to window_size in `plot_3d()` to specify location of viewport

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
* Updated `plot_3d` to include the new `base_shape` argument, which has several built-in shapes `c("circle","hex")` to overlay onto a matrix. This removes points in the elevation matrix outside the masked region.
* Added `line_antialias` option to `plot_3d` to allow the user to turn on antialiasing for the lines in the plot.

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
* Fixed all `progress_bar` arguments to be either `FALSE` or `interactive()` by default

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
