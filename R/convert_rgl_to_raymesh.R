#'@title Convert rayshader RGL scene to ray_mesh object
#'
#'@description Converts the current RGL rayshader scene to a `ray_mesh` object (see `rayvertex` package for more information)
#'
#'@param save_shadow Default `FALSE`. If `TRUE`, this saves a plane with the shadow texture below the model.
#'@return A `ray_mesh` object
#'@export
#'@examples
#'filename_obj = tempfile(fileext = ".obj")
#'#Save model of volcano
#'if(run_documentation()) {
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano, zscale = 2)
#'
#'rm_obj = convert_rgl_to_raymesh()
#'}
convert_rgl_to_raymesh = function(save_shadow = TRUE) {
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  final_scene = list()
  num_elems = 1
  
  vertex_info = get_ids_with_labels()
  basic_load_mesh = function(row, texture_loc, 
                             color = "white", alpha=1, obj = FALSE, specular = "white",shininess = 50,
                             lit = FALSE, quads = FALSE) {
    id = as.character(vertex_info$id[row])
    
    indices = matrix(rgl::rgl.attrib(vertex_info$id[row], "indices"),
                     ncol = 3L, byrow = TRUE) - 1
    vertices = rgl.attrib(vertex_info$id[row], "vertices")
    if(nrow(indices) == 0 && !quads) {
      indices = matrix(seq_len(nrow(vertices))-1, ncol = 3, nrow = nrow(vertices)/3, byrow = TRUE)
    } else if (quads) {
      quads = matrix(seq_len(nrow(vertices))-1,nrow=4)
      indices = t(matrix(rbind(quads[c(1L, 2L, 4L),], 
                               quads[c(2L, 3L, 4L),]), 3L))
    }
    textures = rgl.attrib(vertex_info$id[row], "texcoords")
    normals = rgl.attrib(vertex_info$id[row], "normals")
    if(obj) {
      has_norm = get(id, envir = ray_has_norm_envir)
      if(!has_norm || nrow(normals) == 0) {
        normals = NULL
        norm_indices = NULL
      } else {
        norm_indices = indices
      }
      has_tex = get(id, envir = ray_has_tex_envir)
      if(!has_tex || nrow(textures) == 0) {
        textures = NULL
        tex_indices = NULL
      } else {
        tex_indices = indices
      }
    } else {
      if(nrow(normals) != 0) {
        norm_indices = indices
      } else {
        normals = NULL
        norm_indices = NULL
      }
      if(nrow(textures) != 0) {
        tex_indices = indices
      } else {
        textures = NULL
        tex_indices = NULL
      }
    }
    #Get type
    if(is.na(lit)) {
      lit = FALSE
    }
    if(lit) {
      if(shininess < 10) {
        type_val = "diffuse"
      } else {
        type_val = "phong"
      }
    } else {
      type_val = "color"
    }
    
    texture_loc = ifelse(!is.na(texture_loc), texture_loc, "")
    return(rayvertex::construct_mesh(indices = indices,
                                     vertices = vertices,
                                     texcoords = textures,
                                     normals = normals,
                                     tex_indices = tex_indices,
                                     norm_indices = norm_indices,
                                     material = rayvertex::material_list(texture_location = texture_loc, 
                                                                         diffuse = color,
                                                                         type = type_val,
                                                                         shininess = shininess,
                                                                         dissolve = alpha,
                                                                         specular = specular)))
  }
  for(row in 1:nrow(vertex_info)) {
    if(!is.na(vertex_info$lit[row])) {
      lit_val = unlist(vertex_info$lit[row])
    } else {
      lit_val = FALSE
    }
    if(vertex_info$tag[row] == "surface") {
      dims = rgl::rgl.attrib(vertex_info$id[row], "dim")
      vertices = rgl.attrib(vertex_info$id[row], "vertices")
      textures = rgl.attrib(vertex_info$id[row], "texcoords")
      vertices_y = vertices[,2]
      #Need to add has_normals
      nx = dims[1]
      nz = dims[2] 
      indices = rep(0, 6 * (nz - 1) * (nx - 1))
      counter = 0
      na_counter = 0
      for(i in seq_len(nz)[-nz]) {
        for(j in seq_len(nx)[-nx]) {
          if(!is.na(vertices_y[(i-1)*nx + j]) && !is.na(vertices_y[(i+1-1)*nx + j]) && 
             !is.na(vertices_y[(i-1)*nx + j+1]) && !is.na(vertices_y[(i+1-1)*nx + j+1]))  {
            cindices = (i-1)*nx + c(j, j + nx, j + 1, j + nx, j + nx + 1, j + 1)
            indices[(1:6 + 6*counter)] = cindices
            counter = counter + 1
          } else {
            na_counter = na_counter + 2
          }
        }
      }
      vertices[is.na(vertices_y)] = mean(vertices_y)
      
      indices = indices - 1
      indices = matrix(indices, ncol=3, byrow=TRUE)
      indices = indices[1:(nrow(indices)-na_counter),]
      texture_loc = vertex_info$texture_file[[row]]
      texture_loc = ifelse(!is.na(texture_loc), texture_loc, "")
      final_scene[[num_elems]] = rayvertex::construct_mesh(indices = indices,
                                                           vertices = vertices,
                                                           texcoords = textures,
                                                           tex_indices = indices,
                                                           material = rayvertex::material_list(texture_location = texture_loc, 
                                                                                               type = "color"))
    } else if(vertex_info$tag[row] == "surface_tris") {
      final_scene[[num_elems]] = basic_load_mesh(row, 
                                                 texture_loc = vertex_info$texture_file[[row]])
    } else if (vertex_info$tag[row] == "basebottom") {
      
      texture_loc = vertex_info$texture_file[[row]]
      texture_loc = ifelse(!is.na(texture_loc), texture_loc, "")
      final_scene[[num_elems]] = basic_load_mesh(row, 
                                                 texture_loc = texture_loc,
                                                 lit = lit_val,
                                                 color = vertex_info$base_color[[row]])
    } else if (vertex_info$tag[row] == "base") {
      final_scene[[num_elems]] = basic_load_mesh(row, 
                                                 lit = lit_val,
                                                 texture_loc = NA,
                                                 color = vertex_info$base_color[[row]])
    } else if (vertex_info$tag[row] == "water") {
      final_scene[[num_elems]] = basic_load_mesh(row,
                                                 texture_loc = NA,
                                                 color = vertex_info$water_color[[row]],
                                                 alpha = vertex_info$water_alpha[[row]],
                                                 specular = vertex_info$water_color[[row]])
    } else if (vertex_info$tag[row] == "north_symbol") {
      final_scene[[num_elems]] = basic_load_mesh(row,
                                                 lit = lit_val,
                                                 texture_loc = NA,
                                                 color = vertex_info$north_color[[row]])
    } else if (vertex_info$tag[row] == "arrow_symbol") {
      final_scene[[num_elems]] = basic_load_mesh(row,
                                                 lit = lit_val,
                                                 texture_loc = NA,
                                                 color = vertex_info$arrow_color[[row]])
    } else if (vertex_info$tag[row] == "bevel_symbol") {
      final_scene[[num_elems]] = basic_load_mesh(row,
                                                 lit = lit_val,
                                                 texture_loc = NA,
                                                 color = vertex_info$bevel_color[[row]])
    } else if (vertex_info$tag[row] == "background_symbol") {
      final_scene[[num_elems]] = basic_load_mesh(row,
                                                 lit = lit_val,
                                                 texture_loc = NA,
                                                 color = vertex_info$background_color[[row]])
    } else if (vertex_info$tag[row] == "scalebar_col1") {
      if(vertex_info$type[row] == "quads") {
        final_scene[[num_elems]] = basic_load_mesh(row,
                                                   lit = lit_val,
                                                   texture_loc = NA,
                                                   quads = TRUE,
                                                   color = vertex_info$scalebar1_color[[row]])
      } else {
        final_scene[[num_elems]] = basic_load_mesh(row, 
                                                   lit = lit_val,
                                                   color = vertex_info$scalebar1_color[[row]],
                                                   texture_loc = NA)
      }
    } else if (vertex_info$tag[row] == "scalebar_col2") {
      if(vertex_info$type[row] == "quads") {
        final_scene[[num_elems]] = basic_load_mesh(row,
                                                   lit = lit_val,
                                                   texture_loc = NA,
                                                   quads = TRUE,
                                                   color = vertex_info$scalebar2_color[[row]])
      } else {
        final_scene[[num_elems]] = basic_load_mesh(row, 
                                                   lit = lit_val,
                                                   color = vertex_info$scalebar2_color[[row]],
                                                   texture_loc = NA)
      }
    } else if (vertex_info$tag[row] == "polygon3d") {
      final_scene[[num_elems]] = basic_load_mesh(row, texture_loc = NA,
                                                 lit = lit_val,
                                                 shininess = vertex_info$shininess[[row]],
                                                 color =  vertex_info$tricolor[[row]])
    } else if(vertex_info$tag[row] == "shadow" && save_shadow) {
      final_scene[[num_elems]] = basic_load_mesh(row,
                                                 texture_loc = vertex_info$shadow_texture_file[[row]])
    } else if(vertex_info$tag[row] %in% c("floating_overlay","floating_overlay_tris")) {
      final_scene[[num_elems]] = basic_load_mesh(row,
                                                 texture_loc = vertex_info$layer_texture_file[[row]])
    } else if (vertex_info$tag[row] %in% c("base_soil1","base_soil2")) {
      final_scene[[num_elems]] = basic_load_mesh(row,
                                                 lit = lit_val,
                                                 texture_loc = vertex_info$soil_texture[[row]])
    } else if (grepl("obj", vertex_info$tag[row], fixed=TRUE)) {
      tmp_obj = basic_load_mesh(row, texture_loc = vertex_info$texture_file[[row]],
                                lit = lit_val,
                                obj = TRUE)
      obj_color = vertex_info$obj_color[[row]]
      obj_alpha = vertex_info$obj_alpha[[row]]
      obj_ambient = vertex_info$obj_ambient[[row]]
      obj_specular = vertex_info$obj_specular[[row]]
      obj_emission = vertex_info$obj_emission[[row]]
      shininess = vertex_info$shininess[[row]]
      tmp_obj = rayvertex::change_material(tmp_obj, 
                                 diffuse = ifelse(is.na(obj_color), NULL, obj_color) ,
                                 dissolve = ifelse(is.na(obj_alpha), NULL, obj_alpha),   
                                 ambient = ifelse(is.na(obj_ambient), NULL, obj_ambient),   
                                 specular = ifelse(is.na(obj_specular),NULL, obj_specular),   
                                 emission = ifelse(is.na(obj_emission),NULL, obj_emission),
                                 shininess = ifelse(is.na(shininess), 50, shininess))
      final_scene[[num_elems]] = tmp_obj
      
    }
    num_elems = num_elems + 1
  }
  return(rayvertex::scene_from_list(final_scene))
}
