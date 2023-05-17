#'@title Render Polygons
#'
#'@description Adds 3D polygons to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object. 
#'
#' @param polygon `sf` object, "SpatialPolygon" `sp` object,  or xy coordinates
#' of polygon represented in a way that can be processed by `xy.coords()`.  If
#' xy-coordinate based polygons are open, they will be closed by adding an
#' edge from the last point to the first.
#' @param extent Either an object representing the spatial extent of the 3D scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax", "ymin", "ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#' @param color Default `black`. Color of the polygon.
#' @param top Default `1`. Extruded top distance. If this equals `bottom`, the polygon will not be
#' extruded and just the one side will be rendered.
#' @param bottom Default `0`. Extruded bottom distance. If this equals `top`, the polygon will not be
#' extruded and just the one side will be rendered.
#' @param data_column_top Default `NULL`. A string indicating the column in the `sf` object to use 
#' to specify the top of the extruded polygon.
#' @param data_column_bottom Default `NULL`. A string indicating the column in the `sf` object to use 
#' to specify the bottom of the extruded polygon.
#' @param scale_data Default `1`. If specifying `data_column_top` or `data_column_bottom`, how
#' much to scale that value when rendering.
#' @param parallel Default `FALSE`. If `TRUE`, polygons will be extruded in parallel, which
#' may be faster (depending on how many geometries are in `polygon`).
#' @param holes Default `0`. If passing in a polygon directly, this specifies which index represents
#' the holes in the polygon. See the `earcut` function in the `decido` package for more information.
#' @param heightmap Default `NULL`. Automatically extracted from the rgl window--only use if auto-extraction
#' of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'  All points are assumed to be evenly spaced.
#' @param alpha Default `1`. Transparency of the polygons.
#' @param lit Default `TRUE`. Whether to light the polygons. 
#' @param light_altitude Default `c(45, 60)`. Degree(s) from the horizon from which to light the polygons.
#' @param light_direction Default `c(45, 60)`. Degree(s) from north from which to light the polygons.
#' @param light_intensity Default `0.3`. Intensity of the specular highlight on the polygons.
#' @param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing polygons.
#' @export
#' @examples
#' if(rayshader:::run_documentation()) {
#' #Render the county borders as polygons in Monterey Bay
#' montereybay %>%
#'   sphere_shade(texture = "desert") %>%
#'   add_shadow(ray_shade(montereybay,zscale = 50)) %>%
#'   plot_3d(montereybay, water = TRUE, windowsize = 800, watercolor = "dodgerblue")
#' render_camera(theta = 140,  phi = 55, zoom = 0.85, fov = 30)
#' 
#' #We will apply a negative buffer to create space between adjacent polygons. You may 
#' #have to call `sf::sf_use_s2(FALSE)` before running this code to get it to run.
#' sf::sf_use_s2(FALSE)
#' mont_county_buff = sf::st_simplify(sf::st_buffer(monterey_counties_sf,-0.003), dTolerance=0.001)
#' 
#' render_polygons(mont_county_buff, 
#'                 extent = attr(montereybay,"extent"), top = 10,
#'                 parallel = TRUE)
#' render_snapshot()
#' }
#' if(rayshader:::run_documentation()) {
#' #We can specify the bottom of the polygons as well. Here I float the polygons above the surface
#' #by specifying the bottom argument. We clear the previous polygons with `clear_previous = TRUE`.
#' render_camera(theta=-60,  phi=20, zoom = 0.85, fov=0)
#' render_polygons(mont_county_buff, 
#'                 extent = attr(montereybay,"extent"), bottom = 190, top=200,
#'                 parallel=TRUE,clear_previous=TRUE)
#' render_snapshot()
#' }
#' if(rayshader:::run_documentation()) {
#' #We can set the height of the data to a column in the sf object: we'll use the land area.
#' #We'll have to scale this value because its max value is 2.6 billion:
#' render_camera(theta=-60,  phi=60, zoom = 0.85, fov=30)
#' render_polygons(mont_county_buff, 
#'                 extent = attr(montereybay, "extent"), data_column_top = "ALAND",
#'                 scale_data = 300/(2.6E9), color = "chartreuse4",
#'                 parallel = TRUE, clear_previous = TRUE)
#' render_snapshot()      
#' }  
#' if(rayshader:::run_documentation()) {
#' #This function also works with `render_highquality()`
#' render_highquality(samples = 128, clamp_value = 10, sample_method="sobol_blue",
#'                    min_variance = 0)
#' }
render_polygons = function(polygon, extent,  color = "red", top = 1, bottom = NA,
                           data_column_top = NULL, data_column_bottom = NULL,
                           heightmap = NULL, scale_data = 1, parallel = FALSE,
                           holes = 0, alpha = 1, lit = TRUE, 
                           light_altitude = c(45,30), light_direction = c(315,135), 
                           light_intensity = 0.3, clear_previous = FALSE) {
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if(clear_previous) {
    rgl::pop3d(tag = "polygon3d")
    if(missing(polygon)) {
      return(invisible())
    }
  }
  if(!(length(find.package("rayrender", quiet = TRUE)) > 0)) {
    stop("rayrender required to use render_polygon()")
  }
  if(is.na(bottom)) {
    vertex_info = get_ids_with_labels(typeval = c("base"))
    vertex_info2 = get_ids_with_labels(typeval = c("surface","surface_tris"))
    
    bottom1 = min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,2],na.rm=TRUE)
    bottom2 = min(rgl::rgl.attrib(vertex_info2$id[1], "vertices")[,2],na.rm=TRUE)
    bottom = (bottom1+bottom2)/2
  }
  shape_to_vertex = function(poly_list) {
    matrix(poly_list[4:12],ncol=3,nrow=3,byrow=TRUE)
  }
  vertex_list = list()
  if(!parallel) {
    if(inherits(polygon,"data.frame")) {
      for(i in 1:nrow(polygon)) {
        if(inherits(polygon[i,],"SpatialPolygonsDataFrame") || 
           inherits(polygon[i,],"SpatialPolygons") ||
           inherits(polygon[i,],"sf")) {
          holes = NULL
        }
        mesh = rayrender::extruded_polygon(polygon[i,],top=top,bottom=bottom,
                                           data_column_top=data_column_top,
                                           data_column_bottom=data_column_bottom,
                                           scale_data=scale_data,holes=holes)$mesh_info[[1]]
        mesh_obj = rgl::mesh3d(vertices=c(t(mesh$vertices)),
                               triangles =c(t(mesh$indices))+1)
        vertex_list[[i]] = mesh_obj
      }
    }
  } else {
    if(is.null(options("cores")[[1]])) {
      numbercores = parallel::detectCores()
    } else {
      numbercores = options("cores")[[1]]
    }
    cl = parallel::makeCluster(numbercores)
    doParallel::registerDoParallel(cl, cores = numbercores)
    vertex_list = tryCatch({
      foreach::foreach(i=1:nrow(polygon), .packages = c("rayrender","sf")) %dopar% {
        if(inherits(polygon[i,],"SpatialPolygonsDataFrame") || 
           inherits(polygon[i,],"SpatialPolygons") ||
           inherits(polygon[i,],"sf")) {
          holes = NULL
        }
        mesh = rayrender::extruded_polygon(polygon[i,],top=top,bottom=bottom,
                                           data_column_top=data_column_top,
                                           data_column_bottom=data_column_bottom,
                                           scale_data=scale_data,holes=holes)$mesh_info[[1]]
        mesh_obj = rgl::mesh3d(vertices=c(t(mesh$vertices)),
                               triangles =c(t(mesh$indices))+1)
        mesh_obj
      }
    }, finally = {
      tryCatch({
        parallel::stopCluster(cl)
      }, error = function (e) {print(e)})
    })
  }
  if(is.null(heightmap)) {
    vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
    nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,1])
    ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3]) - min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[,3])
  } else {
    ncol_map = ncol(heightmap)
    nrow_map = nrow(heightmap)
  }
  e = get_extent(extent)
  for(group in seq_along(vertex_list)) {
    if(!is.null(vertex_list[[group]])) {
      single_poly = vertex_list[[group]]
      single_poly$vb[1,] = (-single_poly$vb[1,]-e["xmin"])/(e["xmax"] - e["xmin"]) * nrow_map - nrow_map/2
      single_poly$vb[3,] = ncol_map/2 - (single_poly$vb[3,]-e["ymin"])/(e["ymax"] - e["ymin"]) * ncol_map 
      single_poly$vb[2,] = single_poly$vb[2,]
      
      rgl::shade3d(single_poly, color = color, tag = "polygon3d", lit = lit, alpha = alpha)
    }
  }
  if(lit) {
    existing_lights = rgl::ids3d(type = "lights")
    for(i in 1:nrow(existing_lights)) {
      rgl::pop3d(type="lights")
    }
    if(length(light_altitude) < length(light_direction)) {
      stop("light_altitude and light_direction must be same length")
    }
    for(i in 1:length(light_direction)) {
      rgl::light3d(theta = -light_direction[i]+180, phi = light_altitude[i], 
                     specular = convert_color(rep(light_intensity,3), as_hex = TRUE),
                     viewpoint.rel = FALSE)
    }
  } 
}
