#'@title Render Obj
#'
#'@description Adds 3D OBJ model to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object. If no altitude is provided, the OBJ will be elevated a constant offset 
#'above the heightmap. If the OBJ goes off the edge, the nearest height on the heightmap will be used.
#'
#'@param filename Filename for the OBJ file.
#'@param extent A `raster::Extent` object with the bounding box of the displayed 3D scene.
#'@param long Vector of longitudes (or other coordinate in the same coordinate reference system as extent).
#'@param lat Vector of latitudes (or other coordinate in the same coordinate reference system as extent).
#'@param altitude Elevation of each point, in units of the elevation matrix (scaled by zscale).
#'@param load_material Default `TRUE`. Whether to load the accompanying MTL file to load materials for the 3D model.
#'@param load_normals Default `TRUE`. Whether to load normals for the 3D model.
#'@param angle Default `c(0,0,0)`. Angle of rotation around the x, y, and z axes. If this is a matrix or list,
#'each row (or list entry) specifies the rotation of the nth model specified (number of rows/length of list must
#'equal the length of `lat`/`long`).
#'@param scale Default `c(1,1,1)`. Amount to scale the 3D model in the x, y, and z axes. If this is a matrix or list,
#'each row (or list entry) specifies the scale of the nth model specified (number of rows/length of list must
#'equal the length of `lat`/`long`).
#'@param obj_zscale Default `FALSE`. Whether to scale the size of the OBJ by zscale to have it match
#'the size of the map. If zscale is very big, this will make the model very small.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param heightmap Default `NULL`. Automatically extracted from the rgl window--only use if auto-extraction
#'of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param color Default `black`. Color of the 3D model, if `load_material = FALSE`.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing points.
#'@param ... Additional arguments to pass to `rgl::rgl.triangles()`.
#'@export
#'@examples
#'\donttest{
#'#Render the 3D map
#'moss_landing_coord = c(36.806807, -121.793332)
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,water=TRUE,
#'          shadowcolor="#40310a", background = "tan",
#'          theta=210,  phi=22, zoom=0.20, fov=55)
#'
#'t = seq(0,2*pi,length.out=100)
#'circle_coords_lat = moss_landing_coord[1] + 0.3 * sin(t)
#'circle_coords_long = moss_landing_coord[2] + 0.3 * cos(t)
#'
#'#Create a rainbow spectrum of flags
#'render_obj(flag_full_obj(), extent = attr(montereybay,"extent"), heightmap = montereybay,
#'           lat = unlist(circle_coords_lat), long = unlist(circle_coords_long),
#'           scale=c(2,2,2), angle=c(0,45,0),
#'           zscale=50, color=rainbow(100), smooth = FALSE, clear_previous = TRUE) 
#'render_snapshot()
#'
#'#Rotate the flag to follow the circle
#'render_obj(flag_full_obj(), extent = attr(montereybay,"extent"), heightmap = montereybay,
#'           lat = unlist(circle_coords_lat), long = unlist(circle_coords_long),
#'           scale=c(2,2,2), 
#'           angle=matrix(c(rep(0,100), seq(0,-360,length.out=101)[-1],rep(0,100)),ncol=3),
#'           zscale=50, color=rainbow(100), smooth = FALSE, clear_previous = TRUE) 
#'render_snapshot()
#'
#'#Style the pole with a different color
#'render_obj(flag_pole_obj(), extent = attr(montereybay,"extent"), heightmap = montereybay,
#'           lat = unlist(circle_coords_lat), long = unlist(circle_coords_long),
#'           scale=c(2,2,2), 
#'           angle=matrix(c(rep(0,100), seq(0,-360,length.out=101)[-1],rep(0,100)),ncol=3),
#'           zscale=50, color="grey20", smooth = FALSE, clear_previous = TRUE) 
#'render_obj(flag_banner_obj(), extent = attr(montereybay,"extent"), heightmap = montereybay,
#'           lat = unlist(circle_coords_lat), long = unlist(circle_coords_long),
#'           scale=c(2,2,2),
#'           angle=matrix(c(rep(0,100), seq(0,-360,length.out=101)[-1],rep(0,100)),ncol=3),
#'           zscale=50, color=rainbow(100), smooth = FALSE) 
#'
#'#And all of these work with `render_highquality()`
#'render_highquality()
#'rgl::rgl.close()
#'}
render_obj = function(filename, extent = NULL, lat = NULL, long = NULL, altitude=NULL, 
                      zscale=1, heightmap = NULL, load_material = FALSE, load_normals = TRUE,
                      color = "grey50", offset = 0, obj_zscale = FALSE,
                      angle=c(0,0,0), scale = c(1,1,1), clear_previous = FALSE,
                      rgl_tag = "",
                      ...) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  xyz = transform_into_heightmap_coords(extent, heightmap, lat, long, 
                                        altitude, offset, zscale)
  if(clear_previous) {
    rgl::pop3d(tag = sprintf("obj%s", rgl_tag))
    if(missing(filename)) {
      return(invisible())
    }
  }
  if(is.numeric(color) && length(color) == 3) {
    color = convert_color(color, as_hex = TRUE)
  }
  if(length(color) == 1 && nrow(xyz) > 0) {
    color = rep(color,nrow(xyz))
  } else {
    if(length(color) != nrow(xyz) && nrow(xyz) > 0) {
      stop("If passing individual colors for each object, the number of colors must match the number of objects")
    }
  }
  if(load_material) {
    obj = rayvertex::read_obj(path.expand(filename), 
                              materialspath = dirname(filename)) 
  } else {
    obj = rayvertex::read_obj(path.expand(filename)) 
  }
  if(inherits(angle, "matrix")) {
    stopifnot(is.numeric(angle))
    stopifnot(ncol(angle) == 3)
  } else if (inherits(angle,"list")) {
    angle = do.call(rbind,angle)
    stopifnot(is.numeric(angle))
    stopifnot(ncol(angle) == 3)
  } else {
    stopifnot(length(angle) == 3)
    if(nrow(xyz) > 0) {
      angle = matrix(angle, ncol=3, nrow=nrow(xyz),byrow=TRUE)
    } else {
      angle = matrix(angle, ncol=3, nrow=1,byrow=TRUE)
    }
  }
  if(inherits(scale, "matrix")) {
    stopifnot(is.numeric(scale))
    stopifnot(ncol(scale) == 3)
  } else if (inherits(scale,"list")) {
    scale = do.call(rbind,scale)
    stopifnot(is.numeric(scale))
    stopifnot(ncol(scale) == 3)
  } else {
    stopifnot(length(scale) == 3)
    if(nrow(xyz) > 0) {
      scale = matrix(scale, ncol=3, nrow=nrow(xyz),byrow=T)
    } else {
      scale = matrix(scale, ncol=3, nrow=1,byrow=T)
    }
  }
  scenelist = list()
  for(k in seq_len(nrow(xyz))) {
    tempobj = obj
    if(any(angle != 0)) {
      tempobj = rayvertex::rotate_mesh(tempobj,as.numeric(angle[k,]))
    }
    if(any(scale[k,] != 1)) {
      tempobj = rayvertex::scale_mesh(tempobj,as.numeric(scale[k,]))
    }
    if(!load_material) {
      tempobj = rayvertex::set_material(tempobj, diffuse = color[k])
      
    }
    scenelist[[k]] = rayvertex::translate_mesh(tempobj, as.numeric(xyz[k,]))
  }
  if(is.null(lat) || is.null(long) && length(scenelist[[k]]) == 1) {
    stopifnot(!is.null(heightmap))
    nrow_map = nrow(heightmap)
    ncol_map = ncol(heightmap)
    
    minpoint_x = (extent@xmax + extent@xmin) / 2
    minpoint_y = (extent@ymax + extent@ymin) / 2
    scale_x = nrow_map / (extent@xmax - extent@xmin) 
    scale_z = ncol_map / (extent@ymax - extent@ymin) 
    scale_y = 1
    if(obj_zscale) {
      scale_y = 1/zscale
    }
    idvals = rgl::ids3d(tags=TRUE)
    if(any(c("surface","surface_tris") %in% idvals$tag)) {
      id = idvals$id[idvals$tag %in% c("surface","surface_tris")]
      id = id[1]
      yvals = rgl::rgl.attrib(id,"vertices")[,2]
      base_offset = (max(yvals,na.rm=TRUE) - min(yvals, na.rm=TRUE))/2 
    } else {
      base_offset = 0
    }
    scenelist[[1]] = rayvertex::translate_mesh(scenelist[[1]], c(-minpoint_x,-base_offset,-minpoint_y)) |> 
      rayvertex::scale_mesh(c(scale_x,scale_y,scale_z))
  }
  if(nrow(xyz) == 0) {
    scenelist[[1]] = obj
  }
  obj = rayvertex::scene_from_list(scenelist)
  if(length(obj$materials[[1]]) == 0) {
    obj = rayvertex::set_material(obj, rayvertex::material_list(diffuse = color))
  }
  obj = rayvertex:::merge_scene(obj, flatten_materials = TRUE)
  obj = rayvertex:::remove_duplicate_materials(obj)

  number_shapes = length(obj$shapes)
  number_materials = length(obj$materials)

  inds_by_material = vector(mode="list", length=number_materials)
  tex_by_material = vector(mode="list", length=number_materials)
  norm_by_material = vector(mode="list", length=number_materials)
  
  for(i in seq_len(number_shapes)) {
    for(j in seq_len(number_materials)) {
      select_material = obj$shapes[[i]]$material_ids == (j - 1)
      inds_by_material[[j]] = rbind(inds_by_material[[j]], obj$shapes[[i]]$indices[select_material,])
      tex_by_material[[j]]  = rbind(tex_by_material[[j]], obj$shapes[[i]]$tex_indices[select_material,])
      norm_by_material[[j]] = rbind(norm_by_material[[j]], obj$shapes[[i]]$norm_indices[select_material,])
    }
  }
  
  for(j in seq_len(number_materials)) {
    new_tex = matrix(0,nrow=nrow(obj$vertices), ncol=2)
    new_norm = matrix(0,nrow=nrow(obj$vertices), ncol=3)

    ind_temp = c(t(inds_by_material[[j]]+1))
    tex_vec = c(t(tex_by_material[[j]]+1))
    norm_vec = c(t(norm_by_material[[j]]+1))
    
    for(k in seq_len(length(ind_temp))) {
      if(tex_vec[k] != 0) {
        new_tex[ind_temp[k],] = obj$texcoords[tex_vec[k],]
      }
      if(norm_vec[k] != 0) {
        new_norm[ind_temp[k],] = obj$normals[norm_vec[k],]
      }
    }
    texture = obj$materials[[j]]$diffuse_texname
    diffuse_col = "white"
    specular_col = "black"
    ambient_col = "black"
    has_texture = TRUE
    if(nchar(texture) == 0) {
      texture = NULL
      has_texture = FALSE
      diffuse_col = convert_color(obj$materials[[j]]$diffuse,as_hex=TRUE)
      specular_col = convert_color(obj$materials[[j]]$specular,as_hex=TRUE)
      ambient_col = convert_color(obj$materials[[j]]$ambient,as_hex=TRUE)
    } 
    mat_has_norm = all(new_norm != 0) && load_normals
    if(has_texture) {
      if(mat_has_norm) {
        id = rgl::rgl.triangles(x=obj$vertices,
                           texcoords = new_tex,
                           indices = ind_temp,
                           textype = "rgba",
                           specular="black",
                           color = "white",
                           normals = new_norm,
                           texture = texture,
                           tag = sprintf("obj%s",rgl_tag),
                           back = "culled",
                           ...)
        
      } else {
        id = rgl::rgl.triangles(x=obj$vertices,
                           texcoords = new_tex,
                           textype = "rgba",
                           specular="black",
                           color = "white",
                           indices = ind_temp,
                           texture = texture,
                           tag = sprintf("obj%s",rgl_tag),
                           back = "culled",
                           ...)
      }
    } else {
      if(mat_has_norm) {
        id = rgl::rgl.triangles(x=obj$vertices,
                           indices = ind_temp,
                           specular=specular_col,
                           color =  diffuse_col,
                           ambient = ambient_col,
                           normals = new_norm,
                           tag = sprintf("obj%s",rgl_tag),
                           back = "culled",
                           ...)
      } else {
        id = rgl::rgl.triangles(x=obj$vertices,
                           specular= specular_col,
                           color = diffuse_col,
                           ambient = ambient_col,
                           indices = ind_temp,
                           tag = sprintf("obj%s",rgl_tag),
                           back = "culled",
                           ...)
      }
    }
    assign(as.character(id), mat_has_norm, envir = ray_has_norm_envir)
    assign(as.character(id), has_texture, envir = ray_has_tex_envir)
  }
}
