#'@title Render Path
#'
#'@description Adds a 3D path to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object. If no altitude is provided, the path will be elevated a constant offset 
#'above the heightmap. If the path goes off the edge, the nearest height on the heightmap will be used.
#'
#'@param extent A `raster::Extent` object with the bounding box of the displayed 3D scene.
#'@param lat Vector of latitudes (or other coordinate in the same coordinate reference system as extent).
#'Can also be an `sf` or `SpatialLineDataFrame` object.
#'@param long Default `NULL`. Vector of longitudes (or other coordinate in the same coordinate reference system as extent).
#'Ignored if lat is an `sf` or `SpatialLineDataFrame` object.
#'@param altitude Default `NULL`. Elevation of each point, in units of the elevation matrix (scaled by zscale).
#'If left `NULL`, this will be just the elevation value at ths surface, offset by `offset`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param heightmap Default `NULL`. Automatically extracted from the rgl window--only use if auto-extraction
#'of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param linewidth Default `3`. The line width.
#'@param antialias Default `FALSE`. If `TRUE`, the line with be have anti-aliasing applied. NOTE: anti-aliasing can cause some unpredictable behavior with transparent surfaces.
#'@param color Default `black`. Color of the line.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing paths.
#'@export
#'@examples
#'\donttest{
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
#'     
#'#We'll set the altitude to right above the water to give the tracks a "shadow".
#'render_path(extent = attr(montereybay,"extent"), 
#'            lat = unlist(bird_track_lat), long = unlist(bird_track_long), 
#'            altitude = 10, zscale=50, color="black", antialias=TRUE)
#'render_camera(theta=30,phi=35,zoom=0.45,fov=70)
#'render_snapshot()
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
#'
#'#And all of these work with `render_highquality()`
#'render_highquality(clamp_value=10, line_radius=3)
#'rgl::rgl.close()
#'}
render_path = function(extent = NULL, lat, long = NULL, altitude = NULL, 
                       zscale=1, heightmap = NULL,
                       linewidth = 3, color = "black", antialias = FALSE, offset = 5,
                       clear_previous = FALSE) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(clear_previous) {
    ray_ids = get_ids_with_labels(c("path3d"))
    if(nrow(ray_ids) > 0) {
      remove_ids = ray_ids$id
      rgl::pop3d(id = remove_ids)
      if(missing(lat)) {
        return(invisible())
      }
    }
  }
  if(inherits(lat,"SpatialLinesDataFrame")) {
    latlong = sf::st_coordinates(sf::st_as_sf(lat))
    long = latlong[,1]
    lat = latlong[,2]
    groups = latlong[,3]
  } else if(inherits(lat,"sf")) {
    latlong = sf::st_coordinates(lat)
    long = latlong[,1]
    lat = latlong[,2]
    groups = latlong[,3]
  } else {
    groups = rep(1,length(lat))
  }
  split_lat = split(lat, groups)
  split_long = split(long, groups)
  for(group in seq_along(split_lat)) {
    lat = split_lat[[group]]
    long = split_long[[group]]
  
    if(is.null(heightmap)) {
      vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
      nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
      ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
    } else {
      ncol_map = ncol(heightmap)
      nrow_map = nrow(heightmap)
    }
    e = extent
    distances_x = (long-e@xmin)/(e@xmax - e@xmin) * nrow_map
    distances_y = ncol_map - (lat-e@ymin)/(e@ymax - e@ymin) * ncol_map
    if(is.null(altitude)) {
      if(is.null(heightmap)) {
        stop("No altitude data requires heightmap argument be passed")
      }
      distances_x_index = distances_x
      distances_y_index = distances_y
      distances_x_index[floor(distances_x_index) > nrow(heightmap)] = nrow(heightmap)
      distances_y_index[floor(distances_y_index) > ncol(heightmap)] = ncol(heightmap)
      distances_x_index[floor(distances_x_index) < 1] = 1
      distances_y_index[floor(distances_y_index) < 1] = 1
      if(!"rayimage" %in% rownames(utils::installed.packages())) {
        xy = matrix(c(floor(distances_x_index),floor(distances_y_index)),
                    nrow=length(distances_x_index),ncol=2)
        flipped_mat = flipud(t(heightmap))
        altitude = apply(xy,1,(function(x) flipped_mat[x[2],x[1]])) + offset
      } else {
        altitude = rayimage::interpolate_array((t(heightmap)), distances_x_index,distances_y_index) + offset
      }
    }
    rgl::rgl.material(color = color, ambient = "#000018", lwd = linewidth, line_antialias = antialias)
    rgl::lines3d(distances_x-nrow_map/2, altitude/zscale, distances_y-ncol_map/2)
    altitude = NULL
  }
}
