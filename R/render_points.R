#'@title Render Points
#'
#'@description Adds 3D datapoints to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object.
#'
#'@param extent A `raster::Extent` object with the bounding box of the displayed 3D scene.
#'@param long Vector of longitudes (or other coordinate in the same coordinate reference system as extent).
#'@param lat Vector of latitudes (or other coordinate in the same coordinate reference system as extent).
#'@param altitude Elevation of each point, in units of the elevation matrix (scaled by zscale).
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param heightmap Default `NULL`. Automatically extracted from the rgl window--only use if auto-extraction
#'of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param size Default `3`. The point size.
#'@param color Default `black`. Color of the point.
#'@export
#'@examples
#'\donttest{
#'#Starting at Moss Landing in Monterey Bay, we are going to simulate a flight of a bird going
#'#out to sea and diving for food.
#'
#'#First, create simulated lat/long data
#'set.seed(2009)
#'moss_landing_coord = c(36.806807, -121.793332)
#'x_vel_out = -0.003 + rnorm(1000)[1:300]/1000
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
#'          theta=235,  phi=3.5, zoom=0.20, fov=55)
#'
#'#Pass in the extent of the underlying raster (stored in an attribute for the montereybay
#'#dataset) and the latitudes, longitudes, and altitudes of the track.
#'render_points(extent = attr(montereybay,"extent"), 
#'              lat = unlist(bird_track_lat), long = unlist(bird_track_long), 
#'              altitude = z_out, zscale=50,color="white")
#'render_snapshot()
#'render_camera(phi=45,zoom=0.55)
#'render_snapshot()
#'     
#'#We'll set the altitude to zero to give the tracks a "shadow" over the water.
#'render_points(extent = attr(montereybay,"extent"), 
#'              lat = unlist(bird_track_lat), long = unlist(bird_track_long), 
#'              altitude = 0, zscale=50, color="black", antialias=TRUE)
#'render_camera(theta=30,phi=35,zoom=0.45,fov=70)
#'render_snapshot()
#'rgl::rgl.close()
#'}
render_points = function(extent, lat, long, altitude, zscale=1, heightmap = NULL,
                       linewidth = 3, color = "black", antialias = FALSE) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(is.null(heightmap)) {
    vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
    nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
    ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
  } else {
    ncol_map = ncol(heightmap)
    nrow_map = nrow(heightmap)
  }
  e = extent
  cell_size_x = raster::pointDistance(c(e@xmin, e@ymin), c(e@xmax, e@ymin), lonlat = FALSE)/ncol_map
  cell_size_y = raster::pointDistance(c(e@xmin, e@ymin), c(e@xmin, e@ymax), lonlat = FALSE)/nrow_map
  distances_x = raster::pointDistance(c(e@xmin, e@ymin), cbind(long, rep(e@ymin, length(long))), 
                                      lonlat = FALSE)/cell_size_x - (e@xmax - e@xmin)/2/cell_size_x
  distances_y = raster::pointDistance(c(e@xmin, e@ymin), cbind(rep(e@xmin, length(lat)), lat), 
                                      lonlat = FALSE)/cell_size_y - (e@ymax - e@ymin)/2/cell_size_y
  rgl::rgl.material(color = color, ambient = "#000019", size = linewidth, line_antialias = antialias)
  rgl::points3d(distances_x, altitude/zscale, -distances_y)
}
