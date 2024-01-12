#'@title Render Buildings
#'
#'@description Adds 3D polygons with roofs to the current scene, 
#'using latitude/longitude or coordinates in the reference system defined by the extent object. 
#'
#' @param polygon `sf` object, "SpatialPolygon" `sp` object,  or xy coordinates
#' of polygon represented in a way that can be processed by `xy.coords()`.  If
#' xy-coordinate based polygons are open, they will be closed by adding an
#' edge from the last point to the first.
#' @param extent Either an object representing the spatial extent of the 3D scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax", "ymin", "ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#' @param material Default `"grey80"`. If a color string, this will specify the color of the sides/base of the building 
#' Alternatively (for more customization), this can be a r`ayvertex::material_list()` object to specify
#' the full color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param roof_material Default `NA`, defaults to the material specified in `material`. If a color string, this will specify the color of the roof of the building. 
#' Alternatively (for more customization), this can be a `rayvertex::material_list()` object to specify
#' the full color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param roof_height Default `1`. Height from the base of the building to the start of the roof.
#' @param base_height Default `0`. Height of the base of the roof.
#' @param heights_relative_to_centroid Default `FALSE`. Whether the heights should be measured in absolute
#' terms, or relative to the centroid of the polygon.
#' @param data_column_top Default `NULL`. A string indicating the column in the `sf` object to use 
#' to specify the top of the extruded polygon.
#' @param data_column_bottom Default `NULL`. A string indicating the column in the `sf` object to use 
#' to specify the bottom of the extruded polygon.
#' @param scale_data Default `1`. How much to scale the `top`/`bottom` value when rendering. Use
#' `zscale` to adjust the data to account for `x`/`y` grid spacing, and this argument to scale the data
#' for visualization.
#' @param holes Default `0`. If passing in a polygon directly, this specifies which index represents
#' the holes in the polygon. See the `earcut` function in the `decido` package for more information.
#' @param heightmap Default `NULL`. Automatically extracted from the rgl window--only use if auto-extraction
#' of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'  All points are assumed to be evenly spaced.
#' @param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#' @param alpha Default `1`. Transparency of the polygons.
#' @param lit Default `TRUE`. Whether to light the polygons. 
#' @param light_altitude Default `c(45, 30)`. Degree(s) from the horizon from which to light the polygons.
#' @param light_direction Default `c(315, 225)`. Degree(s) from north from which to light the polygons.
#' @param light_intensity Default `1`. Intensity of the specular highlight on the polygons.
#' @param light_relative Default `FALSE`. Whether the light direction should be taken relative to the camera,
#' or absolute.
#' @param angle Default `45`. Angle of the roof.
#' @param relative_heights Default `TRUE`. Whether the heights specified in `roof_height` and `base_height` should
#' be measured relative to the underlying heightmap.
#' @param flat_shading Default `FALSE`. Set to `TRUE` to have nicer shading on the 3D polygons. This comes
#' with the slight penalty of increasing the memory use of the scene due to vertex duplication. This
#' will not affect software or high quality renders.
#' @param ... Additional arguments to pass to `rgl::triangles3d()`.
#' @param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing polygons.
#'
#' @export
#' @examples
#' if(run_documentation()) {
#' # Load and visualize building footprints from Open Street Map
#' library(osmdata)
#' library(sf)
#' library(raster)
#' 
#' osm_bbox = c(-121.9472, 36.6019, -121.9179, 36.6385)
#' 
#' #Get buildings from OpenStreetMap
#' opq(osm_bbox) |>
#'   add_osm_feature("building") |>
#'   osmdata_sf() ->
#' osm_data
#' 
#' #Get roads from OpenStreetMap
#' opq(osm_bbox) |>
#'   add_osm_feature("highway") |>
#'   osmdata_sf() ->
#' osm_road
#' 
#' #Get extent
#' building_polys = osm_data$osm_polygons
#' osm_dem = elevatr::get_elev_raster(building_polys, z = 11, clip = "bbox")
#' e = extent(building_polys)
#' 
#' # Crop DEM, but note that the cropped DEM will have an extent slightly different than what's 
#' # specified in `e`. Save that new extent to `new_e`.
#' osm_dem |> 
#'   crop(e) |> 
#'   extent() ->
#' new_e
#' 
#' osm_dem |> 
#'   crop(e) |> 
#'   raster_to_matrix() ->
#' osm_mat
#' 
#' #Visualize areas less than one meter as water (approximate tidal range)
#' osm_mat[osm_mat <= 1] = -2
#' 
#' osm_mat %>%
#'   rayimage::render_resized(mag=4) |> 
#'   sphere_shade(texture = "desert") |>
#'   add_overlay(generate_polygon_overlay(building_polys, extent = new_e,
#'                                        heightmap = osm_mat, 
#'                                        linewidth = 6,
#'                                        resolution_multiply = 50), rescale_original = TRUE) |>
#'   add_overlay(generate_line_overlay(osm_road$osm_lines, extent = new_e,
#'                                     heightmap = osm_mat, 
#'                                     linewidth = 6,
#'                                     resolution_multiply = 50), rescale_original = TRUE) |>
#'   plot_3d(osm_mat, water = TRUE, windowsize = 800, watercolor = "dodgerblue",
#'           zscale = 10,
#'           background = "pink")
#' 
#' #Render buildings
#' render_buildings(building_polys,  flat_shading  = TRUE, 
#'                  angle = 30 , heightmap = osm_mat, 
#'                  material = "white", roof_material = "white",
#'                  extent = new_e, roof_height = 3, base_height = 0,
#'                  zscale=10)
#' render_camera(theta=220, phi=22, zoom=0.45, fov=0)
#' render_snapshot()
#' }
#' 
#' if(run_documentation()) {
#' #Zoom in to show roof details and render with render_highquality()
#' render_camera(fov=110)
#' render_highquality(camera_location = c(18.22, 0.57, -50.83),
#'                    camera_lookat = c(20.88, -2.83, -38.87),
#'                    focal_distance = 13, 
#'                    lightdirection = 45)
#' 
#' }
render_buildings = function(polygon, extent,  
                            material = "grey",
                            roof_material = NA,
                            angle = 45,
                            zscale = 1,
                            scale_data = 1,
                            relative_heights = TRUE, 
                            heights_relative_to_centroid = FALSE,
                            roof_height = 1, base_height = 0, 
                            data_column_top = NULL, data_column_bottom = NULL,
                            heightmap = NULL, 
                            holes = 0, alpha = 1, lit = TRUE, flat_shading = FALSE,
                            light_altitude = c(45,30), light_direction = c(315,225), 
                            light_intensity = 1, light_relative = FALSE,
                            clear_previous = FALSE, ...) {
  top = roof_height
  bottom = base_height
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if(!(length(find.package("raybevel", quiet = TRUE)) > 0)) {
    stop("raybevel required to use render_roofs()")
  }
  if(clear_previous) {
    rgl::pop3d(tag = "obj_raymesh_building")
    if(missing(polygon)) {
      return(invisible())
    }
  }
  if(is.character(material)) {
    material = rayvertex::material_list(diffuse = material)
  }
  if(is.character(roof_material)) {
    roof_material = rayvertex::material_list(diffuse = roof_material)
  }
  e = get_extent(extent)
  if(heights_relative_to_centroid) {
    if(is.null(heightmap)) {
      stop("Must pass in heightmap argument if using relative heights")
    } 
    centroids = sf::st_coordinates(sf::st_centroid(polygon))
    xyz = transform_into_heightmap_coords(e, heightmap, centroids[,2], centroids[,1],
                                          altitude = NULL, offset = 0,
                                          zscale = 1)
    bottom = xyz[,2] + bottom
    bottom[is.na(bottom)] = min(xyz[,2])
  }
  if(length(top) != 1) {
    stopifnot(length(top) == nrow(polygon))
  }
  if(!is.null(data_column_top)) {
    stopifnot(data_column_top %in% colnames(polygon))
  }
  if(length(bottom) != 1) {
    stopifnot(length(bottom) == nrow(polygon))
  }
  if(!is.null(data_column_bottom)) {
    stopifnot(!data_column_bottom %in% colnames(polygon))
  }
  
  top_values = get_polygon_data_value(polygon, data_column_name = data_column_top, 
                               scale_data = scale_data, default_value = top)
  
  bottom_values =  get_polygon_data_value(polygon, data_column_name = data_column_bottom, 
                                   scale_data = scale_data, default_value = bottom)
  
  polygon = transform_polygon_into_raycoords(polygon,
                                             heightmap = heightmap, 
                                             e=e, top = top_values, bottom = bottom_values)
  top = polygon$top / zscale
  bottom = polygon$bottom / zscale
  skeletons = raybevel::skeletonize(polygon)
  idx_sans_missing_geometry = unlist(lapply(skeletons, \(x) attr(x,"original_sf_row_index")))
  top = top[idx_sans_missing_geometry]
  bottom = bottom[idx_sans_missing_geometry]
  
  if(!heights_relative_to_centroid) {
    roof_mesh = raybevel::generate_roof(skeletons, 
                                        vertical_offset = top, 
                                        base_height = 0,
                                        angle = angle,
                                        material = material,
                                        roof_material = roof_material,
                                        base = TRUE,
                                        sides = TRUE)
  } else {
    roof_mesh = raybevel::generate_roof(skeletons, 
                                        vertical_offset = top, 
                                        base_height = bottom,
                                        angle = angle,
                                        material = material,
                                        roof_material = roof_material,
                                        base = TRUE,
                                        sides = TRUE)
  }
  if(relative_heights && !heights_relative_to_centroid) {
    if(is.null(heightmap)) {
      stop("Must pass in heightmap argument if using relative heights")
    }
    offset_building_heightmap = function(verts, bottom_value) {
      tmpval = verts
      tmpval[,1] = tmpval[,1] + nrow(heightmap)/2 + 0.5
      tmpval[,3] = tmpval[,3] + ncol(heightmap)/2 + 0.5
      tmpval[tmpval[,1] < 1,1] = 1
      tmpval[tmpval[,1] > nrow(heightmap),1] = nrow(heightmap)
      tmpval[tmpval[,3] < 1,3] = 1
      tmpval[tmpval[,3] > ncol(heightmap),3] = ncol(heightmap)
      new_heights = rayimage::interpolate_array(t(heightmap), tmpval[,1], tmpval[,3]) / zscale
      base_verts = tmpval[,2] == 0
      verts[,2] = verts[,2] + new_heights
      verts[base_verts, 2] = (new_heights[base_verts] + bottom_value)
      return(verts)
    }
    for(i in seq_len(length(roof_mesh$vertices))) {
      roof_mesh$vertices[[i]] = offset_building_heightmap(roof_mesh$vertices[[i]], bottom[i])
    }
  }
  
  render_raymesh(roof_mesh, 
                 xyz = matrix(c(0,0,0),ncol=3), 
                 flat_shading = flat_shading, change_material = FALSE,
                 lit = lit, light_altitude = light_altitude, light_direction = light_direction, 
                 light_intensity = light_intensity, light_relative = light_relative,
                 rgl_tag = "_building", ...)
}
