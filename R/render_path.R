#'@title Render Path
#'
#'@description Adds a 3D path to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object. If no altitude is provided, the path will be elevated a constant offset 
#'above the heightmap. If the path goes off the edge, the nearest height on the heightmap will be used.
#'
#'@param lat Vector of latitudes (or other coordinate in the same coordinate reference system as extent).
#'Can also be an `sf` or `SpatialLineDataFrame` object.
#'@param long Default `NULL`. Vector of longitudes (or other coordinate in the same coordinate reference system as extent).
#'Ignored if lat is an `sf` or `SpatialLineDataFrame` object.
#'@param altitude Default `NULL`. Elevation of each point, in units of the elevation matrix (scaled by zscale).
#'If left `NULL`, this will be just the elevation value at ths surface, offset by `offset`. If a single value, 
#'all data will be rendered at that altitude.
#'@param groups Default `NULL`. Integer vector specifying the grouping of each lat/long path segment, if lat/long are
#'specified as numeric vectors (as opposed to `sf` or `SpatialLineDataFrame` objects, where this information
#'is built-in to the object).
#'@param extent Either an object representing the spatial extent of the 3D scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param heightmap Default `NULL`. Pass this if not including an `altitude` argument, or if no extent passed. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param resample_evenly Default `FALSE`. If `TRUE`, this will re-sample the path evenly from beginning to end, which can help vastly
#'reduce the number of points used to draw it (which can improve the performance of `render_highquality()` and `render_snapshot(software_render = TRUE)`).
#'This function works only if `reorder = TRUE`, or if the sf object is already ordered from beginning to end.
#'@param resample_n Default `360`. Number of breaks in which to evenly resample the line if `resample_evenly = TRUE`.
#'@param linewidth Default `3`. The line width.
#'@param antialias Default `FALSE`. If `TRUE`, the line with be have anti-aliasing applied. NOTE: anti-aliasing can cause some unpredictable behavior with transparent surfaces.
#'@param color Default `black`. Color of the line.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param reorder Default `FALSE`. If `TRUE`, this will attempt to re-order the rows within an `sf` object with
#'multiple paths to be one continuous, end-to-end path. This happens in two steps: merging duplicate 
#'paths that have end points that match with another object (within `reorder_duplicate_tolerance` distance), and then
#'merges them (within `reorder_merge_tolerance` distance) to form a continuous path.
#'@param reorder_first_index Default `1`. The index (row) of the `sf` object in which to begin the reordering
#'process. This merges and reorders paths within `reorder_merge_tolerance` distance until it cannot 
#'merge any more, and then repeats the process in the opposite direction.
#'@param reorder_duplicate_tolerance Default `0.1`. Lines that have start and end points (does not matter which)
#'within this tolerance that match a line already processed (order determined by `reorder_first_index`) will be 
#'discarded.
#'@param reorder_merge_tolerance Default `1`. Lines that have start points that are within this distance 
#'to a previously processed line's end point (order determined by `reorder_first_index`) will be reordered 
#'within the `sf` object to form a continuous, end-to-end path. 
#'@param simplify_tolerance Default `0` (no simplification). If greater than zero, simplifies
#'the path to the tolerance specified. This happens after the data has been merged if `reorder = TRUE`. 
#'If the input data is specified with long-lat coordinates and `sf_use_s2()` returns `TRUE`, 
#'then the value of simplify_tolerance must be specified in meters.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing paths.
#'@param return_coords Default `FALSE`. If `TRUE`, this will return the internal rayshader coordinates of the path, instead of 
#'plotting the line. 
#'@param tag Default `"path3d"`. The rgl tag to use when adding the path to the scene.
#'@export
#'@examples
#'if(rayshader:::run_documentation()) {
#'#Starting at Moss Landing in Monterey Bay, we are going to simulate a flight of a bird going
#'#out to sea and diving for food.
#'
#'#First, create simulated lat/long data
#'set.seed(2009)
#'moss_landing_coord = c(36.806807, -121.793332)
#'x_vel_out = -0.001 + rnorm(1000)[1:300]/1000
#'y_vel_out = rnorm(1000)[1:300]/200
#'z_out = c(seq(0,2000,length.out = 180), seq(2000,0,length.out=10),
#'          seq(0,2000,length.out = 100), seq(2000,0,length.out=10))
#'
#'bird_track_lat = list()
#'bird_track_long = list()
#'bird_track_lat[[1]] = moss_landing_coord[1]
#'bird_track_long[[1]] = moss_landing_coord[2]
#'for(i in 2:300) {
#' bird_track_lat[[i]] = bird_track_lat[[i-1]] + y_vel_out[i]
#' bird_track_long[[i]] = bird_track_long[[i-1]] + x_vel_out[i]
#'}
#'
#'
#'#Render the 3D map 
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,water=TRUE,
#'          shadowcolor="#40310a", watercolor="#233aa1", background = "tan",
#'          theta=210,  phi=22, zoom=0.20, fov=55)
#'
#'#Pass in the extent of the underlying raster (stored in an attribute for the montereybay
#'#dataset) and the latitudes, longitudes, and altitudes of the track.
#'render_path(extent = attr(montereybay,"extent"), 
#'            lat = unlist(bird_track_lat), long = unlist(bird_track_long), 
#'            altitude = z_out, zscale=50,color="white", antialias=TRUE)
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#We'll set the altitude to right above the water to give the tracks a "shadow".
#'render_path(extent = attr(montereybay,"extent"), 
#'            lat = unlist(bird_track_lat), long = unlist(bird_track_long), 
#'            altitude = 10, zscale=50, color="black", antialias=TRUE)
#'render_camera(theta=30,phi=35,zoom=0.45,fov=70)
#'render_snapshot()
#'}
#'
#'if(rayshader:::run_documentation()) {
#'#Remove the path:
#'render_path(clear_previous=TRUE)
#'
#'#Finally, we can also plot just GPS coordinates offset from the surface by leaving altitude `NULL`
#'# Here we plot a spiral of values surrounding Moss Landing. This requires the original heightmap.
#'
#'t = seq(0,2*pi,length.out=1000)
#'circle_coords_lat = moss_landing_coord[1] + 0.5 * t/8 * sin(t*6)
#'circle_coords_long = moss_landing_coord[2] + 0.5 * t/8 *  cos(t*6)
#'render_path(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), 
#'            zscale=50, color="red", antialias=TRUE,offset=100, linewidth=5)
#'render_camera(theta = 160, phi=33, zoom=0.4, fov=55)
#'render_snapshot()
#'}
#'
#'if(rayshader:::run_documentation()) {
#'#And all of these work with `render_highquality()`
#'render_highquality(clamp_value=10, line_radius=3, min_variance = 0,
#'                   sample_method = "sobol_blue", samples = 128)
#'}
#'if(rayshader:::run_documentation()) {
#'#We can also change the material of the objects by setting the `point_material` and
#'#`point_material_args` arguments in `render_highquality()`
#'render_highquality(clamp_value=10, line_radius=3, min_variance = 0,
#'                   sample_method = "sobol_blue", samples = 128,
#'                   path_material = rayrender::glossy, 
#'                   path_material_args = list(gloss = 0.5, reflectance = 0.2))
#'}
#'
#'if(rayshader:::run_documentation()) {
#'#For transmissive materials (like `dielectric`), we should specify that the path
#'#should be rendered with an extruded path. We'll use the `attenuation` argument in 
#'#the `dielectric` function to specify a realistic glass color.
#'render_path(extent = attr(montereybay,"extent"), heightmap = montereybay, clear_previous = TRUE,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), 
#'            zscale=50, color="white", offset=200, linewidth=5)
#'render_highquality(clamp_value=10, line_radius=6, min_variance = 0,
#'                   sample_method = "sobol_blue", samples = 128,
#'                   lightsize = 2000, lightintensity = 10,
#'                   path_material = rayrender::dielectric, use_extruded_paths = TRUE,
#'                   path_material_args = list(refraction = 1.5, attenuation = c(0.05,0.2,0.2)))
#'}
render_path = function(lat, long = NULL, altitude = NULL, groups = NULL,
                       extent = NULL, 
                       zscale=1, heightmap = NULL, 
                       resample_evenly = FALSE, resample_n = 360,
                       reorder = FALSE, 
                       reorder_first_index = 1,
                       reorder_duplicate_tolerance = 0.1, 
                       reorder_merge_tolerance = 1, 
                       simplify_tolerance = 0,
                       linewidth = 3, color = "black", antialias = FALSE, offset = 5,
                       clear_previous = FALSE, return_coords = FALSE,
                       tag = "path3d") {
  if(rgl::cur3d() == 0 && !return_coords) {
    stop("No rgl window currently open.")
  }
  if(clear_previous) {
    rgl::pop3d(tag = tag)
    if(missing(lat)) {
      return(invisible())
    }
  }
  if(resample_evenly) {
    stopifnot(resample_n > 1)
    xyz = render_path(extent = extent, lat = lat, long = long, altitude = altitude, 
                      zscale=zscale, heightmap = heightmap, offset = offset, resample_evenly = FALSE,
                      reorder = reorder, reorder_first_index = reorder_first_index, 
                      reorder_duplicate_tolerance = reorder_duplicate_tolerance,
                      reorder_merge_tolerance = reorder_merge_tolerance,
                      simplify_tolerance = simplify_tolerance,
                      clear_previous = FALSE, return_coords = TRUE)
    xyz = do.call(rbind,xyz)
    xyz = get_interpolated_points_path(xyz, n = resample_n)
    if(!return_coords) {
      rgl::lines3d(xyz[,1] + 0.5,xyz[,2],xyz[,3] + 0.5,
                   color = color, tag = tag, lwd = linewidth, line_antialias = antialias)
      return(invisible())
    } else {
      return(xyz)
    }
  }
  
  
  #Remove empty geometries
  if(inherits(lat, "sf")) {
    lat = lat[!sf::st_is_empty(lat),]
  }
  if(reorder && inherits(lat, "sf")) {
    lat = ray_merge_reorder(lat, start_index = reorder_first_index, 
                            merge_tolerance = reorder_merge_tolerance,
                            duplicate_tolerance = reorder_duplicate_tolerance)
  }
  
  if(simplify_tolerance > 0 && (inherits(lat, "sf") || inherits(lat, "sfc_LINESTRING"))) {
    lat = sf::st_sf(sf::st_simplify(lat, dTolerance = simplify_tolerance, preserveTopology = TRUE))
    lat = lat[!sf::st_is_empty(lat),]
    lat = suppressWarnings(sf::st_cast(sf::st_cast(lat, "MULTILINESTRING"),"LINESTRING"))
  }
  
  if(inherits(lat,"SpatialLinesDataFrame")) {
    latlong = sf::st_coordinates(sf::st_as_sf(lat))
    long = latlong[,1]
    lat = latlong[,2]
    groups = latlong[,3]
  } else if(inherits(lat,"sf")) {
    latlong = sf::st_coordinates(lat)
    if(ncol(latlong) == 3) {
      long = latlong[,1]
      lat = latlong[,2]
      groups = latlong[,3]
    } else if (ncol(latlong) == 4) {
      long = latlong[,1]
      lat = latlong[,2]
      groups = interaction(latlong[,3],latlong[,4])
    }
  } else if(inherits(lat,"sfc_LINESTRING")) {
    latlong = sf::st_coordinates(lat)
    if(ncol(latlong) == 3) {
      long = latlong[,1]
      lat = latlong[,2]
      groups = latlong[,3]
    } else if (ncol(latlong) == 4) {
      long = latlong[,1]
      lat = latlong[,2]
      groups = interaction(latlong[,3],latlong[,4])
    }
  } else if (inherits(lat,"sfc_GEOMETRY")) {
    geometry_list = list()
    for(i in seq_len(lat)) {
      geometry_list[[i]] = sf::st_coordinates(lat[i])
    }
    lat = do.call(rbind,geometry_list)
    if(ncol(latlong) == 3) {
      long = latlong[,1]
      lat = latlong[,2]
      groups = latlong[,3]
    } else if (ncol(latlong) == 4) {
      long = latlong[,1]
      lat = latlong[,2]
      groups = interaction(latlong[,3],latlong[,4])
    }
  } else if (is.null(groups)) {
    groups = rep(1,length(lat))
  }
  split_lat = split(lat, groups)
  split_long = split(long, groups)
  if(is.null(heightmap)) {
    vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
    nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
    ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
  } else {
    ncol_map = ncol(heightmap)
    nrow_map = nrow(heightmap)
  }
  if(!is.null(altitude)) {
    offset = 0
  }
  
  coord_list = list()
  for(group in seq_along(split_lat)) {
    lat = split_lat[[group]]
    long = split_long[[group]]
    
    xyz = transform_into_heightmap_coords(extent, heightmap, lat, long, 
                                          altitude, offset, zscale, filter_bounds = FALSE)
    if(!return_coords) {
      rgl::lines3d(xyz[,1] + 0.5,xyz[,2],xyz[,3] + 0.5,
                   color = color, tag = tag, 
                   lwd = linewidth, line_antialias = antialias)
    } else {
      coord_list[[group]] = xyz
    }
  }
  if(return_coords) {
    return(coord_list)
  }
}
