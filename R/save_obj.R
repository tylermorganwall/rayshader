#'@title Save OBJ
#'
#'@description Writes the textured 3D rayshader visualization to an OBJ file.
#'
#'@param filename String with the filename. If `.obj` is not at the end of the string, it will be appended automatically.
#'@param save_texture Default `TRUE`. If the texture should be saved along with the geometry.
#'@param water_index_refraction Default `1`. The index of refraction for the rendered water.
#'@param manifold_geometry Default `FALSE`. If `TRUE`, this will take the additional step of making the mesh manifold.
#'@param all_face_fields Default `FALSE`. If `TRUE`, all OBJ face fields (v/vn/vt) will always be written.
#'@param save_shadow Default `FALSE`. If `TRUE`, this saves a plane with the shadow texture below the model.
#'@export
#'@examples
#'if(interactive()) {
#'filename_obj = tempfile(fileext = ".obj")
#'
#'#Save model of volcano
#'if(rayshader:::run_documentation()) {
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano, zscale = 2)
#'
#'save_obj(filename_obj)
#'}
#'
#'#Save model of volcano without texture
#'if(rayshader:::run_documentation()) {
#'save_obj(filename_obj, save_texture = FALSE)
#'}
#'
#'#Make water have realistic index of refraction
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay, zscale = 50)
#'  
#'save_obj(filename_obj, water_index_refraction = 1.5)
#'}
#'}
save_obj = function(filename, save_texture = TRUE, water_index_refraction = 1, 
                    manifold_geometry = FALSE, all_face_fields = FALSE,
                    save_shadow = FALSE) {
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if(is.null(filename)) {
    stop("save_obj requires a filename")
  }
  noext_filename = tools::file_path_sans_ext(filename)
  if(is.character(filename)) {
    if(save_texture) {
      current_dir = FALSE
      if(basename(filename) != filename) {
        tempbase = basename(filename)
      } else {
        tempbase = filename
        current_dir = TRUE
      }
      if(tools::file_ext(filename) == "obj") {
        if(current_dir) {
          filename_mtl = sprintf("%s.mtl",tools::file_path_sans_ext(tempbase))
        } else {
          filename_mtl = sprintf("%s%s%s.mtl", dirname(filename), .Platform$file.sep,tools::file_path_sans_ext(tempbase))
        }
      } else {
        if(current_dir) {
          filename_mtl = sprintf("%s.mtl",tempbase)
        } else {
          filename_mtl = sprintf("%s%s%s.mtl", dirname(filename), .Platform$file.sep, tempbase)
        }
      }
    }
    if(tools::file_ext(filename) != "obj") {
      filename = sprintf("%s.obj",filename)
    }
    con = file(filename, "w")
    on.exit(close(con))
    if(save_texture) {
      con_mtl = file(filename_mtl, "w")
      on.exit(close(con_mtl), add = TRUE)
    }
  }

  number_vertices = 0
  number_texcoords = 0
  number_normals = 0
  # allvertices = matrix(nrow=0,ncol=3) #Debug line
  #Writes data and increments vertex/normal/texture counter
  write_data = function(id, con) {
    vertices = rgl.attrib(id, "vertices")
    vertices[is.na(vertices)] = 0
    # allvertices <<- rbind(allvertices,vertices) #Debug line
    textures = rgl.attrib(id, "texcoords")
    normals = rgl.attrib(id, "normals")
    # has_degenerate_normals = any(normals[,1] == 0 & normals[,2] == 0 & normals[,3] == 0)
    cat(paste0("v ", sprintf("%1.6f %1.6f %1.6f",vertices[,1], vertices[,2], vertices[,3])), file=con, sep = "\n")
    number_vertices <<- number_vertices + nrow(vertices)
  
    if (nrow(textures) > 0) {
      cat(paste0("vt ", sprintf("%1.6f %1.6f", textures[,1], textures[,2])), file=con, sep = "\n" )
      number_texcoords <<- number_texcoords + nrow(textures)
    } 
    if (nrow(normals) > 0) {
      cat(paste0("vn ",sprintf("%1.6f %1.6f %1.6f", normals[,1], normals[,2], normals[,3])), file=con, sep = "\n" )
      number_normals <<- number_normals + nrow(normals)
    } 
  }
  scalebar1_written = FALSE
  scalebar2_written = FALSE
  floating_layer_num = 1
  soil_textures = 1
  obj_materials = 1
  
  write_mtl = function(idrow, con) {
    if(!is.na(idrow$texture_file) && is.na(idrow$obj_color)) {
      cat(paste("newmtl ray_surface \n"), file=con)
      file.copy(idrow$texture_file[[1]], sprintf("%s.png",noext_filename), overwrite = TRUE)
      cat(sprintf("map_Kd %s.png \n",basename(noext_filename)), file=con)
      cat("illum 0 \n", file=con)
      cat("\n", file=con)
    } else if (!is.na(idrow$base_color[[1]])) {
      tempcol = col2rgb(idrow$base_color[[1]])/255
      cat(paste("newmtl ray_base"), file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat("illum 0 \n", file=con)
      cat("\n", file=con)
    } else if (!is.na(idrow$water_color[[1]])) {
      tempcol = col2rgb(idrow$water_color[[1]])/255
      cat(paste("newmtl ray_water"), file=con, sep="\n")
      cat(paste("Ks", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat("Ns 100", file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat(paste("d", sprintf("%1.4f",idrow$water_alpha[[1]]),collapse = " "), file=con, sep="\n")
      cat(paste("Ni", sprintf("%1.4f",water_index_refraction),collapse = " "), file=con, sep="\n")
      cat("illum 5 \n", file=con)
      cat("\n", file=con)
    } else if (!is.na(idrow$north_color[[1]])) {
      tempcol = col2rgb(idrow$north_color[[1]])/255
      cat(sprintf("newmtl ray_north%d",current_compass_number), file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat("illum 0 \n", file=con)
      cat("\n", file=con)
    } else if (!is.na(idrow$arrow_color[[1]])) {
      tempcol = col2rgb(idrow$arrow_color[[1]])/255
      cat(sprintf("newmtl ray_arrow%d",current_compass_number), file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat("illum 0 \n", file=con)
      cat("\n", file=con)
    } else if (!is.na(idrow$bevel_color[[1]])) {
      tempcol = col2rgb(idrow$bevel_color[[1]])/255
      cat(sprintf("newmtl ray_bevel%d",current_compass_number), file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat("illum 0 \n", file=con)
      cat("\n", file=con)
    } else if (!is.na(idrow$background_color[[1]])) {
      tempcol = col2rgb(idrow$background_color[[1]])/255
      cat(sprintf("newmtl ray_background%d",current_compass_number), file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat("illum 0 \n", file=con)
      cat("\n", file=con)
      current_compass_number <<- current_compass_number + 1
    } else if (!is.na(idrow$scalebar1_color[[1]])) {
      tempcol = col2rgb(idrow$scalebar1_color[[1]])/255
      if(!scalebar1_written) {
        scalebar1_written <<- TRUE
        cat(paste("newmtl ray_scalebar1"), file=con, sep="\n")
        cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
        cat("illum 0 \n", file=con)
        cat("\n", file=con)
      }
    } else if (!is.na(idrow$scalebar2_color[[1]])) {
      tempcol = col2rgb(idrow$scalebar2_color[[1]])/255
      if(!scalebar2_written) {
        scalebar2_written <<- TRUE
        cat(paste("newmtl ray_scalebar2"), file=con, sep="\n")
        cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
        cat("illum 0 \n", file=con)
        cat("\n", file=con)
      }
    } else if (!is.na(idrow$tricolor[[1]])) {
      tempcol = col2rgb(idrow$tricolor[[1]])/255
      cat(paste("newmtl ray_polygon3d"), file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      if(!idrow$lit[[1]]) {
        cat(paste("Ka", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      }
      cat(paste("d", sprintf("%1.4f",idrow$polygon_alpha[[1]]),collapse = " "), file=con, sep="\n")
      if(idrow$lit[[1]]) {
        cat("illum 1 \n", file=con)
      } else {
        cat("illum 0 \n", file=con)
      }
      cat("\n", file=con)
    } else if(!is.na(idrow$shadow_texture_file)) {
      if(save_shadow) {
        cat(paste("newmtl ray_shadow \n"), file=con)
        file.copy(idrow$shadow_texture_file[[1]], sprintf("%s_shadow.png",noext_filename), overwrite = TRUE)
        cat(sprintf("map_Kd %s \n",sprintf("%s_shadow.png",basename(noext_filename))), file=con)
        cat("\n", file=con)
      }
    } else if(!is.na(idrow$layer_texture_file)) {
      cat(sprintf("newmtl ray_layer_texture%d \n",floating_layer_num), file=con)
      file.copy(idrow$layer_texture_file[[1]], sprintf("%s_layer%d.png",noext_filename,floating_layer_num), overwrite = TRUE)
      cat(sprintf("map_Kd %s \n",sprintf("%s_layer%d.png",basename(noext_filename),floating_layer_num)), file=con)
      floating_layer_num <<- floating_layer_num + 1
      cat("illum 0 \n", file=con)
      
      cat("\n", file=con)
    } else if(!is.na(idrow$soil_texture)) {
      cat(sprintf("newmtl ray_soil_base%d \n",soil_textures), file=con)
      file.copy(idrow$soil_texture[[1]], sprintf("%s_soil%d.png",noext_filename, soil_textures), overwrite = TRUE)
      cat(sprintf("map_Kd %s \n",sprintf("%s_soil%d.png",basename(noext_filename),soil_textures)), file=con)
      soil_textures <<- soil_textures + 1
      cat("illum 0 \n", file=con)
      
      cat("\n", file=con)
    } else if(!is.na(idrow$obj_color)) {
      tempcol = col2rgb(idrow$obj_color[[1]])/255
      tempalpha = idrow$obj_alpha[[1]]
      tempambient = col2rgb(idrow$obj_ambient[[1]])/255
      tempspecular = col2rgb(idrow$obj_specular[[1]])/255
      tempemission = col2rgb(idrow$obj_emission[[1]])/255
      cat(sprintf("newmtl ray_obj%d \n",obj_materials), file=con)
      if(!is.na(idrow$texture_file[[1]]) && file.exists(idrow$texture_file[[1]])) {
        file.copy(idrow$texture_file[[1]], sprintf("%s_obj%d.png",noext_filename, obj_materials), overwrite = TRUE)
        cat(sprintf("map_Kd %s \n",sprintf("%s_obj%d.png",basename(noext_filename),obj_materials)), file=con)
      }
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat(paste("Ka", sprintf("%1.4f %1.4f %1.4f",tempambient[1],tempambient[2],tempambient[3]),collapse = " "), file=con, sep="\n")
      cat(paste("Ks", sprintf("%1.4f %1.4f %1.4f",tempspecular[1],tempspecular[2],tempspecular[3]),collapse = " "), file=con, sep="\n")
      cat(paste("Ke", sprintf("%1.4f %1.4f %1.4f",tempemission[1],tempemission[2],tempemission[3]),collapse = " "), file=con, sep="\n")
      cat(paste("d",  sprintf("%1.4f",tempalpha),collapse = " "), file=con, sep="\n")
      
      if(idrow$lit[[1]]) {
        cat("illum 1 \n", file=con)
      } else {
        cat("illum 0 \n", file=con)
      }
      cat("\n", file=con)
      
      obj_materials <<- obj_materials + 1
    }
  }
  write_comment = function(comment, con) {
    cat(comment, file=con)
  }
  
  string_num = function(n) {
    sprintf("%d", n)
  }
  
  #Begin file
  cat("#", paste0(filename, " produced by rayshader"), "\n", file=con)
  if(save_texture) {
    cat(sprintf("mtllib %s.mtl \n", basename(tools::file_path_sans_ext(filename))), file=con)
  }
  
  vertex_info = get_ids_with_labels()
  
  
  number_compass = sum(vertex_info$tag == "north_symbol")
  current_compass_number = 1
  vertex_info$startindex = NA
  vertex_info$startindextex = NA
  vertex_info$startindexnormals = NA
  vertex_info$endindex = NA
  vertex_info$endindextex = NA
  vertex_info$endindexnormals = NA
  wrote_water = FALSE
  wrote_base = FALSE
  for(row in 1:nrow(vertex_info)) {
    id = vertex_info$id[row]
    if(vertex_info$tag[row] %in% c("surface","base","basebottom","water",
                                   "north_symbol","arrow_symbol", "bevel_symbol",
                                   "background_symbol", "scalebar_col1", "scalebar_col2",
                                   "surface_tris","polygon3d", "shadow","floating_overlay","floating_overlay_tris",
                                   "base_soil1", "base_soil2") || grepl("obj",vertex_info$tag[row], fixed=TRUE)) {
      vertex_info$startindex[row] = number_vertices + 1
      if(vertex_info$tag[row] %in%  c("surface", "surface_tris", "shadow","floating_overlay","floating_overlay_tris", "base_soil1", "base_soil2") || grepl("obj", vertex_info$tag[row], fixed=TRUE)) {
        vertex_info$startindextex[row] = number_texcoords + 1
      }
      vertex_info$startindexnormals[row] = number_normals + 1
      write_comment(sprintf("# %s\n",vertex_info$tag[row]), con)
      write_data(id, con)
      if(vertex_info$tag[row] != "water") {
        if(save_texture) {
          if(vertex_info$tag[row] != "base") {
            write_mtl(vertex_info[row,], con_mtl)
          } else {
            if(!wrote_base) {
              write_mtl(vertex_info[row,], con_mtl)
              wrote_base = TRUE
            }
          }
        }
      } else {
        if(!wrote_water) {
          if(save_texture) {
            write_mtl(vertex_info[row,], con_mtl)
          }
          wrote_water = TRUE
        }
      }
      vertex_info$endindex[row] = number_vertices 
      vertex_info$endindextex[row] = number_texcoords
      vertex_info$endindexnormals[row] = number_normals
    }
  }
  current_compass_number = 1
  current_floating_number = 1
  current_obj_number = 1
  
  for(row in 1:nrow(vertex_info)) {
    if(vertex_info$tag[row] == "surface") {
      if(save_texture) {
        cat("g Surface", file=con, sep ="\n")
        cat("usemtl ray_surface", file=con, sep ="\n")
      }
      dims = rgl::rgl.attrib(vertex_info$id[row], "dim")
      vertices_y = rgl.attrib(vertex_info$id[row], "vertices")[,2]
      if(!is.na(vertex_info$startindexnormals[row]) && !is.na(vertex_info$endindexnormals[row])) {
        hasnormals = vertex_info$startindexnormals[row] < vertex_info$endindexnormals[row]
      } else {
        hasnormals = FALSE
      }
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
      indices = indices + vertex_info$startindextex[row] - 1
      if(hasnormals) {
        indices = sprintf("%d/%d/%d", indices, indices, indices)
      } else {
        indices = sprintf("%d/%d/", indices, indices)
      }
      indices = matrix(indices, ncol=3, byrow=TRUE)
      indices = indices[1:(nrow(indices)-na_counter),]
      cat(paste("f", indices[,1], indices[,2], indices[,3]), 
          sep="\n", file=con)
    } else if(vertex_info$tag[row] == "surface_tris") {
      if(save_texture) {
        cat("g Surface", file=con, sep ="\n")
        cat("usemtl ray_surface", file=con, sep ="\n")
      }
      if(!is.na(vertex_info$startindexnormals[row]) && !is.na(vertex_info$endindexnormals[row])) {
        hasnormals = vertex_info$startindexnormals[row] < vertex_info$endindexnormals[row]
      } else {
        hasnormals = FALSE
      }
      
      indices = matrix(rgl::rgl.attrib(vertex_info$id[row], "indices"),
                       ncol = 3L) + vertex_info$startindex[row] - 1
      if(hasnormals) {
        indices = sprintf("%d/%d/%d", indices, indices, indices)
      } else {
        indices = sprintf("%d/%d/", indices, indices)
      }
      indices = matrix(indices, ncol=3, byrow=TRUE)
      cat(paste("f", indices[,1], indices[,2], indices[,3]),
          sep="\n", file=con)
    } else if (vertex_info$tag[row] == "basebottom") {
      if(save_texture) {
        cat("g Basebottom", file=con, sep ="\n")
        cat("usemtl ray_base", file=con, sep ="\n")
      }
      if(!is.na(vertex_info$startindexnormals[row]) && !is.na(vertex_info$endindexnormals[row])) {
        hasnormals = vertex_info$startindexnormals[row] < vertex_info$endindexnormals[row]
      } else {
        hasnormals = FALSE
      }
      dims = rgl::rgl.attrib(vertex_info$id[row], "dim")
      vertices_y = rgl.attrib(vertex_info$id[row], "vertices")[,2]
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
      indices = indices + vertex_info$startindex[row] - 1
      if(all_face_fields) {
        indices = sprintf("%d//", indices)
      } else {
        indices = sprintf("%d", indices)
      }
      
      indices = matrix(indices, ncol=3, byrow=TRUE)
      indices = indices[1:(nrow(indices)-na_counter),]
      cat(paste("f", indices[,3], indices[,2], indices[,1]), 
          sep="\n", file=con)
    } else if (vertex_info$tag[row] == "base") {
      if(save_texture) {
        cat("g Base", file=con, sep ="\n")
        cat("usemtl ray_base", file=con, sep ="\n")
      }
      if(!is.na(vertex_info$startindexnormals[row]) && !is.na(vertex_info$endindexnormals[row])) {
        hasnormals = vertex_info$startindexnormals[row] < vertex_info$endindexnormals[row]
      } else {
        hasnormals = FALSE
      }
      baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      # if(hasnormals) {
      #   cat(paste("f", sprintf("%d//%d %d//%d %d//%d", baseindices[,1], baseindices[,1],
      #                          baseindices[,2], baseindices[,2], 
      #                          baseindices[,3], baseindices[,3])), 
            # sep="\n", file=con)
      # } else {
      if(all_face_fields) {
        cat(paste("f", sprintf("%d// %d// %d//", baseindices[,1],
                               baseindices[,2], 
                               baseindices[,3])), 
            sep="\n", file=con)
      } else {
        cat(paste("f", sprintf("%d %d %d", baseindices[,1],
                               baseindices[,2], 
                               baseindices[,3])), 
            sep="\n", file=con)
      }
      # }
    } else if (vertex_info$tag[row] == "water") {
      if(save_texture) {
        cat("g Water", file=con, sep ="\n")
        cat("usemtl ray_water", file=con, sep ="\n")
      }
      if(!is.na(vertex_info$startindexnormals[row]) && !is.na(vertex_info$endindexnormals[row])) {
        hasnormals = vertex_info$startindexnormals[row] < vertex_info$endindexnormals[row]
      } else {
        hasnormals = FALSE
      }
      
      indices = matrix(rgl::rgl.attrib(vertex_info$id[row], "indices"),
                       ncol = 3L, byrow = TRUE) + vertex_info$startindex[row] - 1
      
      cat(paste("f", sprintf("%d %d %d", indices[,1], indices[,2], indices[,3])),
            sep="\n", file=con)
    } else if (vertex_info$tag[row] == "north_symbol") {
      if(save_texture) {
        cat("g North", file=con, sep ="\n")
        cat(sprintf("usemtl ray_north%d",current_compass_number), file=con, sep ="\n")
      }
      baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3]), 
          sep="\n", file=con)
    } else if (vertex_info$tag[row] == "arrow_symbol") {
      if(save_texture) {
        cat("g Arrow", file=con, sep ="\n")
        cat(sprintf("usemtl ray_arrow%d",current_compass_number), file=con, sep ="\n")
      }
      baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3]), 
          sep="\n", file=con)
    } else if (vertex_info$tag[row] == "bevel_symbol") {
      if(save_texture) {
        cat("g Bevel", file=con, sep ="\n")
        cat(sprintf("usemtl ray_bevel%d",current_compass_number), file=con, sep ="\n")
      }
      baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3]), 
          sep="\n", file=con)
    } else if (vertex_info$tag[row] == "background_symbol") {
      if(save_texture) {
        cat("g Background", file=con, sep ="\n")
        cat(sprintf("usemtl ray_background%d",current_compass_number), file=con, sep ="\n")
      }
      baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3]), 
          sep="\n", file=con)
      current_compass_number = current_compass_number + 1
    } else if (vertex_info$tag[row] == "scalebar_col1") {
      if(save_texture) {
        cat("g Scalebar1", file=con, sep ="\n")
        cat("usemtl ray_scalebar1", file=con, sep ="\n")
      }
      if(vertex_info$type[row] == "quads") {
        baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=4, byrow=TRUE)
        cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3], baseindices[,4]), 
            sep="\n", file=con)
      } else {
        baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
        cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3]), 
            sep="\n", file=con)
      }
    } else if (vertex_info$tag[row] == "scalebar_col2") {
      if(save_texture) {
        cat("g Scalebar1", file=con, sep ="\n")
        cat("usemtl ray_scalebar2", file=con, sep ="\n")
      }
      if(vertex_info$type[row] == "quads") {
        baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=4, byrow=TRUE)
        cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3], baseindices[,4]), 
            sep="\n", file=con)
      } else {
        baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
        cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3]), 
            sep="\n", file=con)
      }
    } else if (vertex_info$tag[row] == "polygon3d") {
      if(save_texture) {
        cat("g Polygon", file=con, sep ="\n")
        cat("usemtl ray_polygon3d", file=con, sep ="\n")
      }
      baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3]), 
          sep="\n", file=con)
    } else if(vertex_info$tag[row] == "shadow" && save_shadow) {
      if(save_texture) {
        cat("g Shadow", file=con, sep ="\n")
        cat("usemtl ray_shadow", file=con, sep ="\n")
      }
      shadowindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      shadowtexindices = matrix(vertex_info$startindextex[row]:vertex_info$endindextex[row], ncol=3, byrow=TRUE)
      shadownormalindices = matrix(vertex_info$startindexnormals[row]:vertex_info$endindexnormals[row], ncol=3, byrow=TRUE)
      
      cat(paste("f", sprintf("%d/%d/%d %d/%d/%d %d/%d/%d", shadowindices[,1], shadowtexindices[,1], shadownormalindices[,1],
                             shadowindices[,2], shadowtexindices[,2], shadownormalindices[,2],
                             shadowindices[,3], shadowtexindices[,3], shadownormalindices[,3])), 
          sep="\n", file=con)

    } else if(vertex_info$tag[row] %in% c("floating_overlay","floating_overlay_tris")) {
      if(save_texture) {
        cat("g Floating", file=con, sep ="\n")
        cat(sprintf("usemtl ray_layer_texture%d",current_floating_number), file=con, sep ="\n")
        current_floating_number = current_floating_number + 1
      }
      floating_layer_indices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      floating_layer_texindices = matrix(vertex_info$startindextex[row]:vertex_info$endindextex[row], ncol=3, byrow=TRUE)
      floating_layer_normalindices = matrix(vertex_info$startindexnormals[row]:vertex_info$startindexnormals[row], ncol=3, byrow=TRUE)
      cat(paste("f", sprintf("%d/%d/%d %d/%d/%d %d/%d/%d", 
                             floating_layer_indices[,1], floating_layer_texindices[,1], floating_layer_normalindices[,1],
                             floating_layer_indices[,2], floating_layer_texindices[,2], floating_layer_normalindices[,2],
                             floating_layer_indices[,3], floating_layer_texindices[,3], floating_layer_normalindices[,3])),
          sep="\n", file=con)
      
    } else if (vertex_info$tag[row] %in% c("base_soil1","base_soil2")) {
      if(save_texture) {
        cat("g Base", file=con, sep ="\n")
        if(vertex_info$tag[row] == "base_soil1") {
          cat("usemtl ray_soil_base1", file=con, sep ="\n")
        } else {
          cat("usemtl ray_soil_base2", file=con, sep ="\n")
        }
      }
      soil_layer_indices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      soil_layer_texindices = matrix(vertex_info$startindextex[row]:vertex_info$endindextex[row], ncol=3, byrow=TRUE)
      # soil_layer_normalindices = matrix(vertex_info$startindexnormals[row]:vertex_info$startindexnormals[row], ncol=3, byrow=TRUE)
      cat(paste("f", sprintf("%d/%d %d/%d %d/%d", 
                             soil_layer_indices[,1], soil_layer_texindices[,1], #soil_layer_normalindices[,1],
                             soil_layer_indices[,2], soil_layer_texindices[,2], #soil_layer_normalindices[,2],
                             soil_layer_indices[,3], soil_layer_texindices[,3])), #soil_layer_normalindices[,3])),
          sep="\n", file=con)
    } else if (grepl("obj", vertex_info$tag[row], fixed=TRUE)) {
      if(save_texture) {
        cat("g Obj", file=con, sep ="\n")
        cat(sprintf("usemtl ray_obj%d",current_obj_number), file=con, sep ="\n")
        current_obj_number = current_obj_number + 1
      }
      id = as.character(vertex_info$id[row])
      has_norm = get(id, envir = ray_has_norm_envir)
      has_tex = get(id, envir = ray_has_tex_envir)
      inds = rgl::rgl.attrib(vertex_info$id[row], "indices") - 1
      obj_indices = matrix(inds +  vertex_info$startindex[row],byrow=TRUE, ncol=3)
      obj_tex_indices = matrix(inds +  vertex_info$startindextex[row],byrow=TRUE, ncol=3)
      obj_norm_indices = matrix(inds +  vertex_info$startindexnormals[row],byrow=TRUE, ncol=3)
      
      if(has_tex && has_norm) {
        cat(paste("f", sprintf("%d/%d/%d %d/%d/%d %d/%d/%d",
                               obj_indices[,1], obj_tex_indices[,1], obj_norm_indices[,1],
                               obj_indices[,2], obj_tex_indices[,2], obj_norm_indices[,2],
                               obj_indices[,3], obj_tex_indices[,3], obj_norm_indices[,3])),
            sep="\n", file=con)
      } else if (has_norm) {
        cat(paste("f", sprintf("%d//%d %d//%d %d//%d",
                               obj_indices[,1], obj_norm_indices[,1],
                               obj_indices[,2], obj_norm_indices[,2],
                               obj_indices[,3], obj_norm_indices[,3])),
            sep="\n", file=con)
      } else if (has_tex) {
        cat(paste("f", sprintf("%d/%d %d/%d %d/%d",
                               obj_indices[,1], obj_tex_indices[,1],
                               obj_indices[,2], obj_tex_indices[,2],
                               obj_indices[,3], obj_tex_indices[,3])),
            sep="\n", file=con)
      } else {
        cat(paste("f", sprintf("%d %d %d",
                               obj_indices[,1], 
                               obj_indices[,2], 
                               obj_indices[,3])),
            sep="\n", file=con)
      }
    }
  }
  if(manifold_geometry) {
    fix_manifold_geometry(filename)
  }
}
