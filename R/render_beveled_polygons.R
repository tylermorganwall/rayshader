#'@title Render Beveled Polygons
#'
#'@description Adds beveled polygon to the scene using the `raybevel` package. See
#'the `raybevel::generate_beveled_polygon()` function for more information.
#'
#' @param polygon `sf` object, "SpatialPolygon" `sp` object,  or xy coordinates
#' of polygon represented in a way that can be processed by `xy.coords()`.  If
#' xy-coordinate based polygons are open, they will be closed by adding an
#' edge from the last point to the first.
#' @param extent Either an object representing the spatial extent of the 3D scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax", "ymin", "ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' @param bevel_width Default `5`. Width of the bevel.
#' @param width_raw_units Default `FALSE`. Whether the bevel width should be measured in raw display units,
#' or the actual units of the map.
#' @param bevel Default `NULL`. A list with `x`/`y` components that specify a bevel profile. See `raybevel::generate_bevel()`
#' @param angle Default `45`. Angle of the bevel.
#' @param material Default `"grey80"`. If a color string, this will specify the color of the sides/base of the polygon. 
#' Alternatively (for more customization), this can be a r`ayvertex::material_list()` object to specify
#' the full color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param bevel_material Default `NA`, defaults to the material specified in `material`. If a color string, this will specify the color of the polygon bevel. 
#' Alternatively (for more customization), this can be a `rayvertex::material_list()` object to specify
#' the full color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param bevel_height Default `1`. Height from the base of the polygon to the start of the beveled top.
#' @param base_height Default `0`. Height of the base of the polygon.
#' @param set_max_height Default `FALSE`. A logical flag that controls whether to set the max height of the roof based on the `max_height` argument.
#' @param max_height Default `1`. The maximum height of the polygon.
#' @param scale_all_max Default `FALSE`. If passing in a list of multiple skeletons with polygons, whether to scale each polygon to the overall
#' max height, or whether to scale each max height to the maximum internal distance in the polygon.
#' @param raw_offsets Default `FALSE`. A logical flag indicating whether the `bevel_offsets` are already 
#' in raw format and do not need to be multiplied by the maximum time of the skeleton. 
#' See the documentation for `raybevel::generate_beveled_polygon()` for more info.
#' @param raw_heights Default `FALSE`. A logical flag indicating whether the `bevel_heights` are already 
#' in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' See the documentation for `raybevel::generate_beveled_polygon()` for more info.
#' @param heights_relative_to_centroid Default `FALSE`. Whether the heights should be measured in absolute
#' terms, or relative to the centroid of the polygon.
#' @param data_column_top Default `NULL`. A string indicating the column in the `sf` object to use 
#' to specify the top of the beveled polygon.
#' @param data_column_bottom Default `NULL`. A string indicating the column in the `sf` object to use 
#' to specify the bottom of the beveled polygon.
#' @param scale_data Default `1`. If specifying `data_column_top` or `data_column_bottom`, how
#' much to scale that value when rendering.
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
#' @param flat_shading Default `FALSE`. Set to `TRUE` to have nicer shading on the 3D polygons. This comes
#' with the slight penalty of increasing the memory use of the scene due to vertex duplication. This
#' will not affect software or high quality renders.
#' @param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing polygons.
#' @param ... Additional arguments to pass to `rgl::triangles3d()`.
#' @export
#' @examples
#' 
#' # This function can also create fake "terrain" from polygons by visualizing the distance 
#' # to the nearest edge.
#' if(run_documentation()) {
#' #Render the county borders as polygons in Monterey Bay as terrain
#' montereybay %>%
#'   sphere_shade(texture = "desert") %>%
#'   add_shadow(ray_shade(montereybay,zscale = 50)) %>%
#'   plot_3d(montereybay, water = TRUE, windowsize = 800, watercolor = "dodgerblue",
#'           background = "pink")
#' 
#' #We will apply a negative buffer to create space between adjacent polygons. You may
#' #have to call `sf::sf_use_s2(FALSE)` before running this code to get it to run.
#' sf::sf_use_s2(FALSE)
#' mont_county_buff = sf::st_simplify(sf::st_buffer(monterey_counties_sf,-0.003), dTolerance=0.001)
#' 
#' render_beveled_polygons(mont_county_buff,  flat_shading  = TRUE, angle = 45 , 
#'                         heightmap = montereybay, bevel_width=2000,
#'                         material = "red",
#'                         extent = attr(montereybay,"extent"),  
#'                         bevel_height = 5000, base_height=0, 
#'                         zscale=200) 
#' render_camera(theta = 0,  phi = 90, zoom = 0.65, fov = 0)
#' render_snapshot()
#' render_camera(theta=194, phi= 35,   zoom = 0.5, fov= 80)
#' render_snapshot()
#' }
#' 
#' # Changing the color of the beveled top:
#' if(run_documentation()) {
#' render_beveled_polygons(mont_county_buff,  flat_shading  = TRUE, angle = 45 , 
#'                         heightmap = montereybay, bevel_width=2000,
#'                         material = "tan", bevel_material = "darkgreen",
#'                         extent = attr(montereybay,"extent"), clear_previous=TRUE,
#'                         bevel_height = 5000, base_height=0, 
#'                         zscale=200) 
#' }
#' # We can create a nice curved surface by passing in a bevel generated with the 
#' # `raybevel::generate_bevel()` function. 
#' if(run_documentation()) {
#' render_beveled_polygons(mont_county_buff, flat_shading  = TRUE, heightmap = montereybay,
#'                         bevel = raybevel::generate_bevel("exp",bevel_end = 0.4),
#'                         #max_height = 10, scale_all_max = TRUE, set_max_height = TRUE,
#'                         material = rayvertex::material_list(diffuse="red", 
#'                                                             ambient = "darkred", 
#'                                                             diffuse_intensity = 0.2,
#'                                                             ambient_intensity = 0.1),
#'                         light_intensity = 1, light_relative = FALSE,
#'                         extent = attr(montereybay,"extent"), bevel_height = 5000, 
#'                         base_height=0, clear_previous = TRUE,
#'                         zscale=200) 
#' render_snapshot()
#' }
#' 
#' # While the bevels all start at the same point in the above example,
#' # they rise to different levels due to being scaled by the maximum internal distance
#' # in the polygon. Setting `scale_all_max = TRUE` ensures the bevels are all scaled to the 
#' # same maximum height (in this case, 3000m above the 5000m bevel start height).
#' if(run_documentation()) {
#' render_beveled_polygons(mont_county_buff, flat_shading  = TRUE, heightmap = montereybay,
#'                  bevel = raybevel::generate_bevel("exp",bevel_end = 0.4),
#'                  max_height = 3000, scale_all_max = TRUE, set_max_height = TRUE,
#'                  material = rayvertex::material_list(diffuse="red", 
#'                                                      ambient = "darkred", 
#'                                                      diffuse_intensity = 0.2,
#'                                                      ambient_intensity = 0.1),
#'                  light_intensity = 1, light_relative = FALSE,
#'                  extent = attr(montereybay,"extent"), bevel_height = 5000, 
#'                  base_height=0, clear_previous = TRUE,
#'                  zscale=200) 
#' render_snapshot()
#' }
#' 
#' # Rendering the polygons with `render_highquality()`
#' if(run_documentation()) {
#'   render_highquality()
#' }
#' 
#' # We can scale the size of the polygon to a column in the `sf` object as well:
#' # raybevel::generate_bevel() function. We can scale this data down using the `scale_data`
#' # argument. Note that this is applied as well as the `zscale` argument, and that you 
#' # must think carefully about your scales and values if trying to represent a meaningful
#' # data visualization with this object.
#' if(run_documentation()) {
#' render_beveled_polygons(mont_county_buff,  flat_shading  = TRUE, angle = 45, bevel_width=1000, 
#'                  data_column_top = "ALAND", scale_data = 1e-5, heightmap = montereybay,
#'                  #max_height = 1000, scale_all_max = TRUE, set_max_height = TRUE,
#'                  material = rayvertex::material_list(diffuse="red"),
#'                  light_intensity = 1, light_relative = FALSE,
#'                  extent = attr(montereybay,"extent"), clear_previous = TRUE,
#'                  zscale=200) 
#' render_snapshot()
#' }
render_beveled_polygons = function(polygon, extent,  
                                   material = "grey",
                                   bevel_material = NA,
                                   angle = 45, bevel_width = 5, width_raw_units = FALSE,
                                   bevel = NA, zscale = 1,
                                   bevel_height = 1, base_height = 0, 
                                   raw_heights = FALSE,
                                   raw_offsets = FALSE, 
                                   heights_relative_to_centroid = TRUE,
                                   set_max_height = FALSE, max_height = 10,
                                   scale_all_max = TRUE,
                                   data_column_top = NULL, data_column_bottom = NULL,
                                   heightmap = NULL, scale_data = 1, 
                                   holes = 0, alpha = 1, lit = TRUE, flat_shading = FALSE,
                                   light_altitude = c(45,30), light_direction = c(315,225), 
                                   light_intensity = 1, light_relative = FALSE,
                                   clear_previous = FALSE, ...) {
  top = bevel_height
  bottom = base_height
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if(!(length(find.package("raybevel", quiet = TRUE)) > 0)) {
    stop("raybevel required to use render_roofs()")
  }
  if(clear_previous) {
    rgl::pop3d(tag = "obj_raymesh_beveled_polygon")
    if(missing(polygon)) {
      return(invisible())
    }
  }
  if(is.character(material)) {
    material = rayvertex::material_list(diffuse = material)
  }
  if(is.character(bevel_material)) {
    bevel_material = rayvertex::material_list(diffuse = bevel_material)
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
  
  if(!is.list(bevel)) {
    stopifnot(angle > 0 && angle < 90)
    angle_slope = tanpi(angle/180)
    # Setting width_raw_units to TRUE makes it easier to generate visually nice bevels but 
    # don't necessarily corrspond to any meaningful real world distance
    if(!width_raw_units) {
      bevel_width = bevel_width / zscale
    }
    max_height = bevel_width * angle_slope
    bevel = list(x=c(0,bevel_width), y=c(0,max_height))
    raw_heights = TRUE
    raw_offsets = TRUE
  } else {
    max_height = max_height / zscale
  }
  
  if(!heights_relative_to_centroid) {
    poly_mesh = raybevel::generate_beveled_polygon(skeletons, 
                                                   bevel_offsets = bevel,
                                                   vertical_offset = top,
                                                   raw_heights = raw_heights,
                                                   raw_offsets = raw_offsets,
                                                   base_height = 0,
                                                   set_max_height = set_max_height,
                                                   max_height = max_height,
                                                   material = material,
                                                   bevel_material = bevel_material,
                                                   scale_all_max = scale_all_max,
                                                   sides = TRUE,
                                                   base = TRUE)
  } else {
    poly_mesh = raybevel::generate_beveled_polygon(skeletons, 
                                                   bevel_offsets = bevel,
                                                   vertical_offset = top,
                                                   raw_heights = raw_heights,
                                                   raw_offsets = raw_offsets,
                                                   base_height = bottom,
                                                   set_max_height = set_max_height,
                                                   max_height = max_height,
                                                   material = material,
                                                   bevel_material = bevel_material,
                                                   scale_all_max = scale_all_max,
                                                   sides = TRUE,
                                                   base = TRUE)
  }

  
  render_raymesh(poly_mesh, 
                 xyz = matrix(c(0,0,0),ncol=3), 
                 flat_shading = flat_shading, change_material = FALSE,
                 lit = lit, light_altitude = light_altitude, light_direction = light_direction, 
                 light_intensity = light_intensity, light_relative = light_relative,
                 rgl_tag = "_beveled_polygon", ...)
}
