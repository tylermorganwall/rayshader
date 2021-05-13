#'@title Render Polygons
#'
#'@description Adds 3D polygons to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object. 
#'
#' @param polygon `sf` object, "SpatialPolygon" `sp` object,  or xy coordinates
#' of polygon represented in a way that can be processed by `xy.coords()`.  If
#' xy-coordinate based polygons are open, they will be closed by adding an
#' edge from the last point to the first.
#' @param extent A `raster::Extent` object with the bounding box for the height map 
#' used to generate the original map.
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
#' @param lit Default `TRUE`. Whether to light the polygons. 
#' @param light_altitude Default `c(45, 60)`. Degree(s) from the horizon from which to light the polygons.
#' @param light_direction Default `c(45, 60)`. Degree(s) from north from which to light the polygons.
#' @param light_intensity Default `0.3`. Intensity of the specular highlight on the polygons.
#' @param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing polygons.
#' @export
#' @examples
#' if(interactive()) {
#' \donttest{
#' #Render the county borders as polygons in Monterey Bay
#' montereybay %>%
#'   sphere_shade(texture = "desert") %>%
#'   add_shadow(ray_shade(montereybay,zscale=50)) %>%
#'   plot_3d(montereybay,water=TRUE, windowsize=800, watercolor="dodgerblue")
#' render_camera(theta=140,  phi=55, zoom = 0.85, fov=30)
#' 
#' #We will apply a negative buffer to create space between adjacent polygons:
#' mont_county_buff = sf::st_simplify(sf::st_buffer(monterey_counties_sf,-0.003), dTolerance=0.001)
#' 
#' render_polygons(mont_county_buff, 
#'                 extent = attr(montereybay,"extent"), top=10,
#'                 parallel=TRUE)
#' render_snapshot()
#' 
#' #We can specify the bottom of the polygons as well. Here I float the polygons above the surface
#' #by specifying the bottom argument. We clear the previous polygons with `clear_previous = TRUE`.
#' render_camera(theta=-60,  phi=20, zoom = 0.85, fov=0)
#' render_polygons(mont_county_buff, 
#'                 extent = attr(montereybay,"extent"), bottom = 190, top=200,
#'                 parallel=TRUE,clear_previous=TRUE)
#' render_snapshot()
#' 
#' #We can set the height of the data to a column in the sf object: we'll use the land area.
#' #We'll have to scale this value because it's max value is 2.6 billion:
#' render_camera(theta=-60,  phi=60, zoom = 0.85, fov=30)
#' render_polygons(mont_county_buff, 
#'                 extent = attr(montereybay,"extent"), data_column_top = "ALAND",
#'                 scale_data = 300/(2.6E9), color="chartreuse4",
#'                 parallel=TRUE,clear_previous=TRUE)
#' render_snapshot()        
#' 
#' #This function also works with `render_highquality()`
#' render_highquality(samples=400, clamp_value=10)
#' rgl::rgl.close()
#' }
#' }
render_polygons = function(polygon, extent,  color = "red", top = 1, bottom = NA,
                           data_column_top = NULL, data_column_bottom = NULL,
                           heightmap = NULL, scale_data = 1, parallel = FALSE,
                           holes = 0, lit = TRUE, 
                           light_altitude = c(45,30), light_direction = c(315,135), 
                           light_intensity = 0.3, clear_previous = FALSE) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(clear_previous) {
    ray_ids = get_ids_with_labels(c("polygon3d"))
    if(nrow(ray_ids) > 0) {
      remove_ids = ray_ids$id
      rgl::pop3d(id = remove_ids)
      if(missing(polygon)) {
        return(invisible())
      }
    }
  }
  if(!"rayrender" %in% rownames(utils::installed.packages())) {
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
        poly = rayrender::extruded_polygon(polygon[i,],top=top,bottom=bottom,
                                           data_column_top=data_column_top,
                                           data_column_bottom=data_column_bottom,
                                           scale_data=scale_data,holes=holes)
        vertex_list[[i]] = do.call(rbind,lapply(poly$properties, shape_to_vertex))
        if(any(is.na(vertex_list[[i]]))) {
          vertex_list[[i]] = NULL
        }
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
        poly = rayrender::extruded_polygon(polygon[i,],top=top,bottom=bottom,
                                           data_column_top=data_column_top,
                                           data_column_bottom=data_column_bottom,
                                           scale_data=scale_data)
        vl = do.call(rbind,lapply(poly$properties, shape_to_vertex))
        if(any(is.na(vl))) {
          vl = NULL
        }
        vl
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
  e = extent
  for(group in seq_along(vertex_list)) {
    if(!is.null(vertex_list[[group]])) {
      single_poly = vertex_list[[group]]
      single_poly[,1] = (-single_poly[,1]-e@xmin)/(e@xmax - e@xmin) * nrow_map - nrow_map/2
      single_poly[,3] = ncol_map/2 - (single_poly[,3]-e@ymin)/(e@ymax - e@ymin) * ncol_map 
      single_poly[,2] = single_poly[,2]
      
      rgl::rgl.material(color = color, ambient = "#000020", lit = lit)
      rgl::triangles3d(single_poly)
    }
  }
  if(lit) {
    existing_lights = rgl::rgl.ids(type = "lights")
    for(i in 1:nrow(existing_lights)) {
      rgl::rgl.pop(type="lights")
    }
    if(length(light_altitude) < length(light_direction)) {
      stop("light_altitude and light_direction must be same length")
    }
    for(i in 1:length(light_direction)) {
      rgl::rgl.light(theta = -light_direction[i]+180, phi = light_altitude[i], 
                     specular = convert_color(rep(light_intensity,3), as_hex = TRUE),
                     viewpoint.rel = FALSE)
    }
  } 
}
