#'@title Render Raymesh
#'
#'@description Adds 3D raymesh model to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object. If no altitude is provided, the raymesh will be elevated a constant offset 
#'above the heightmap. If the raymesh goes off the edge, the raymesh will be filtered out.
#'
#'If no latitudes or longitudes are passed in, the raymesh will be plotted in the coordinate system set by the user-specified
#'`extent` argument as-is. Use this alongside `save_multipolygonz_to_obj()` to plot 3D polygons imported from geospatial sources
#'in the proper location (but for ease of use, use `render_multipolygonz()` to plot this data directly).
#'
#'@param raymesh `raymesh` object (see the rayvertex package for a description)
#'@param extent Either an object representing the spatial extent of the scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#'@param long Vector of longitudes (or other coordinate in the same coordinate reference system as extent).
#'@param lat Vector of latitudes (or other coordinate in the same coordinate reference system as extent).
#'@param altitude Default `NULL`. Elevation of each point, in units of the elevation matrix (scaled by `zscale`). 
#'If left `NULL`, this will be just the elevation value at ths surface, offset by `offset`. If a single value, 
#'the OBJ will be rendered at that altitude.
#'@param xyz Default `NULL`, ignored. A 3 column numeric matrix, with each row specifying the x/y/z 
#'coordinates of the OBJ model(s). Overrides lat/long/altitude and ignores extent to plot the OBJ in raw rgl coordinates.
#'@param load_normals Default `TRUE`. Whether to load normals for the 3D model.
#'@param change_material Default `TRUE`. Whether to change the raymesh material (to customize the color).
#'@param angle Default `c(0,0,0)`. Angle of rotation around the x, y, and z axes. If this is a matrix or list,
#'each row (or list entry) specifies the rotation of the nth model specified (number of rows/length of list must
#'equal the length of `lat`/`long`).
#'@param scale Default `c(1,1,1)`. Amount to scale the 3D model in the x, y, and z axes. If this is a matrix or list,
#'each row (or list entry) specifies the scale of the nth model specified (number of rows/length of list must
#'equal the length of `lat`/`long`).
#'@param obj_zscale Default `FALSE`. Whether to scale the size of the OBJ by zscale to have it match
#'the size of the map. If zscale is very big, this will make the model very small.
#'@param swap_yz Default `NULL`, defaults to `FALSE` unless plotting raw coordinates (no lat or long passed).
#' Whether to swap and Y and Z axes. (Y axis is vertical in 
#'rayshader coordinates, but data is often provided with Z being vertical).
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param heightmap Default `NULL`. Automatically extracted from the rgl window--only use if auto-extraction
#'of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param baseshape Default `rectangle`. Shape of the base. Options are `c("rectangle","circle","hex")`.
#'@param flat_shading Default `FALSE`. If `TRUE`, this will use rgl's flat shading.
#'@param lit Default `TRUE`. Whether to light the polygons. 
#'@param light_altitude Default `c(45, 60)`. Degree(s) from the horizon from which to light the polygons.
#'@param light_direction Default `c(45, 60)`. Degree(s) from north from which to light the polygons.
#'@param light_intensity Default `0.3`. Intensity of the specular highlight on the polygons.
#'@param light_relative Default `FALSE`. Whether the light direction should be taken relative to the camera,
#'or absolute.
#'@param color Default `black`. Color of the 3D model, if `load_material = FALSE`.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing points.
#'@param rgl_tag Default `""`. Tag to add to the rgl scene id, will be prefixed by `"objraymsh"`
#'@param ... Additional arguments to pass to `rgl::triangles3d()`.
#'@export
#'@examples
#'if(run_documentation()) {
#'}
render_raymesh = function(raymesh, extent = NULL, lat = NULL, long = NULL, altitude=NULL, 
                          xyz = NULL,
                          zscale=1, heightmap = NULL, load_normals = TRUE, change_material = TRUE,
                          color = "grey50", offset = 0, obj_zscale = FALSE, swap_yz = NULL,
                          angle=c(0,0,0), scale = c(1,1,1), clear_previous = FALSE,
                          baseshape = "rectangle", flat_shading = FALSE, lit = FALSE,
                          light_altitude = c(45,30), light_direction = c(315,135), 
                          light_intensity = 1, light_relative = FALSE,
                          rgl_tag = "",
                          ...) {
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if(is.null(lat) || is.null(long)) {
    single_obj = TRUE
  } else {
    single_obj = FALSE
  }
  heightmap = generate_base_shape(heightmap, baseshape)
  if(is.null(xyz)) {
    raw_coords = FALSE
    if(!single_obj) {
      if(is.null(swap_yz)) {
        swap_yz = FALSE
      }
      xyz = transform_into_heightmap_coords(extent, heightmap, lat, long, 
                                            altitude, offset, zscale)
    } else {
      if(is.null(swap_yz)) {
        swap_yz = TRUE
      }
      xyz = transform_into_heightmap_coords(extent, heightmap, lat, long, 
                                            altitude, offset, zscale, use_altitude = FALSE)
    }
    if(swap_yz) {
      xyz = xyz[,c(1,3,2), drop = FALSE]
    }
  } else {
    raw_coords = TRUE
  }
  
  if(clear_previous) {
    rgl::pop3d(tag = sprintf("obj_raymesh%s", rgl_tag))
    if(missing(raymesh)) {
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
  obj = raymesh
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
  if(!is.null(lat) && !is.null(long) && any(is.na(xyz[,2]))) {
    scale = scale[!is.na(xyz[,2]),]
    angle = angle[!is.na(xyz[,2]),]
    color = color[!is.na(xyz[,2])]
    xyz = xyz[!is.na(xyz[,2]),]
    if(nrow(xyz) == 0) {
      stop("All models outside extent--check lat/long values and extent object.")
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
    scenelist[[k]] = rayvertex::translate_mesh(tempobj, as.numeric(xyz[k,]))
  }
  if(!raw_coords) {
    stopifnot(!is.null(heightmap))
    nrow_map = nrow(heightmap)
    ncol_map = ncol(heightmap)
    
    extent = get_extent(extent)
    minpoint_x = (extent["xmax"] + extent["xmin"]) / 2 - zscale / 2
    minpoint_y = (extent["ymax"] + extent["ymin"]) / 2 + zscale / 2
    scale_x = (nrow_map-1) / (extent["xmax"] - extent["xmin"])
    scale_z = (ncol_map-1) / (extent["ymax"] - extent["ymin"]) 
    scale_y = 1/zscale
    if(single_obj) {
      obj_zscale = FALSE
      idvals = rgl::ids3d(tags=TRUE)
      if(any(c("surface","surface_tris") %in% idvals$tag)) {
        id = idvals$id[idvals$tag %in% c("surface","surface_tris")]
        id = id[1]
        yvals = rgl::rgl.attrib(id,"vertices")[,2]
        base_offset = (max(yvals,na.rm=TRUE) - min(yvals, na.rm=TRUE))/2 
      } else {
        base_offset = 0
      }
      
      if(swap_yz) {
        scenelist[[1]] = rayvertex::translate_mesh(scenelist[[1]], 
                                                   c(-minpoint_x,-minpoint_y, 0)) |> 
          rayvertex::swap_yz() |> 
          rayvertex::scale_mesh(c(scale_x,scale_y,scale_z))
      } else {
        scenelist[[1]] = rayvertex::translate_mesh(scenelist[[1]], 
                                                   c(-minpoint_x,0, -minpoint_y)) |> 
          rayvertex::scale_mesh(c(scale_x,scale_y,scale_z))
      }
    }
  }
  if(nrow(xyz) == 0) {
    scenelist[[1]] = obj
  }
  obj = rayvertex::scene_from_list(scenelist)
  if(obj_zscale) {
    obj = rayvertex::scale_mesh(obj, c(1,1,1)/zscale)
  }
  if(length(obj$materials[[1]]) == 0) {
    obj = rayvertex::set_material(obj, rayvertex::material_list(diffuse = color))
  } else {
    if(change_material) {
      obj = rayvertex::change_material(obj, diffuse = color)
    }
  }
  obj = rayvertex:::merge_scene(obj, flatten_materials = TRUE)
  obj = rayvertex:::remove_duplicate_materials(obj)
  
  number_shapes = length(obj$shapes)
  number_materials = length(obj$materials)
  
  inds_by_material = vector(mode="list", length=number_materials)
  tex_by_material  = vector(mode="list", length=number_materials)
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
    shininess = obj$materials[[j]]$shininess

    has_texture = TRUE
    if(nchar(texture) == 0) {
      texture = NULL
      has_texture = FALSE
      diffuse_col = convert_color(darken_color(obj$materials[[j]]$diffuse,
                                               obj$materials[[j]]$diffuse_intensity),as_hex=TRUE)
      specular_col = convert_color(darken_color(obj$materials[[j]]$specular,
                                                obj$materials[[j]]$specular_intensity),as_hex=TRUE)
      ambient_col = convert_color(darken_color(obj$materials[[j]]$ambient,
                                               obj$materials[[j]]$ambient_intensity),as_hex=TRUE)
    } 
    mat_has_norm = all(new_norm != 0) && load_normals
    
    if(!flat_shading) {
      verts = obj$vertices
      ind_temp = ind_temp
      new_tex = new_tex
      new_norm = new_norm
    } else {
      verts = obj$vertices[ind_temp,]
      if(has_texture) {
        new_tex = new_tex[ind_temp,]
      }
      new_norm = NULL
      ind_temp = NULL
    }
    if(has_texture) {
      if(mat_has_norm) {
        id = rgl::triangles3d(x=verts,
                              texcoords = new_tex,
                              indices = ind_temp,
                              textype = "rgba",
                              specular="white",
                              color = "white",
                              shininess = shininess,
                              normals = new_norm,
                              texture = texture,
                              tag = sprintf("obj_raymesh%s",rgl_tag),
                              back = "filled",
                              lit = lit,
                              ...)
        
      } else {
        id = rgl::triangles3d(x=verts,
                              texcoords = new_tex,
                              textype = "rgba",
                              specular="black",
                              color = "white",
                              shininess = shininess,
                              indices = ind_temp,
                              texture = texture,
                              tag = sprintf("obj_raymesh%s",rgl_tag),
                              back = "filled",
                              lit = lit,
                              ...)
      }
    } else {
      if(mat_has_norm) {
        id = rgl::triangles3d(x=verts,
                              indices = ind_temp,
                              specular = specular_col,
                              color =  diffuse_col,
                              ambient = ambient_col,
                              shininess = shininess,
                              normals = new_norm,
                              tag = sprintf("obj_raymesh%s",rgl_tag),
                              back = "filled",
                              lit = lit,
                              ...)
      } else {
        id = rgl::triangles3d(x=verts,
                              specular= specular_col,
                              color = diffuse_col,
                              ambient = ambient_col,
                              shininess = shininess,
                              indices = ind_temp,
                              tag = sprintf("obj_raymesh%s",rgl_tag),
                              back = "filled",
                              lit = lit,
                              ...)
      }
    }
    assign(as.character(id), mat_has_norm, envir = ray_has_norm_envir)
    assign(as.character(id), has_texture, envir = ray_has_tex_envir)
  }
  if(lit) {
    existing_lights = rgl::ids3d(type = "lights")
    for(i in seq_len(nrow(existing_lights))) {
      rgl::pop3d(type="lights")
    }
    if(length(light_altitude) < length(light_direction)) {
      stop("light_altitude and light_direction must be same length")
    }
    for(i in seq_len(length(light_direction))) {
      rgl::light3d(theta = -light_direction[i]+180, phi = light_altitude[i], 
                   specular = convert_color(rep(light_intensity,3), as_hex = TRUE),
                   ambient = convert_color(rep(light_intensity,3), as_hex = TRUE),
                   diffuse = convert_color(rep(light_intensity,3), as_hex = TRUE),
                   viewpoint.rel = light_relative)
    }
  } 
}
