#'@title Save OBJ
#'
#'@description Writes the textured 3D rayshader visualization to an OBJ file.
#'
#'@param filename String with the filename. If `.obj` is not at the end of the string, it will be appended automatically.
#'@param save_texture Default `TRUE`. If the texture should be saved along with the geometry.
#'@param water_index_refraction Default `1`. The index of refraction for the rendered water.
#'@export
#'@examples
#'filename_obj = tempfile(fileext = ".obj")
#'
#'#Save model of volcano
#'\donttest{
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano, zscale = 2)
#'
#'save_obj(filename_obj)
#'}
#'
#'#Save model of volcano without texture
#'\donttest{
#'save_obj(filename_obj, save_texture = FALSE)
#'rgl::rgl.close()
#'}
#'
#'#Make water have realistic index of refraction
#'\donttest{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay, zscale = 50)
#'  
#'save_obj(filename_obj, water_index_refraction = 1.5)
#'rgl::rgl.close()
#'}
save_obj = function(filename, save_texture = TRUE, water_index_refraction = 1) {
  if(is.null(filename)) {
    stop("save_obj requires a filename")
  }
  if(is.character(filename)) {
    if(save_texture) {
      current_dir = FALSE
      if(basename(filename) != filename) {
        tempbase = basename(filename)
      } else {
        tempbase = filename
        current_dir = TRUE
      }
      if(substring(filename, nchar(filename)-3,nchar(filename)) == ".obj") {
        if(current_dir) {
          filename_mtl = paste0(substring(tempbase, 1,nchar(tempbase)-4),".mtl")
        } else {
          filename_mtl = paste0(dirname(filename), .Platform$file.sep, substring(tempbase, 1,nchar(tempbase)-4),".mtl")
        }
      } else {
        if(current_dir) {
          filename_mtl = paste0(tempbase,".mtl")
        } else {
          filename_mtl = paste0(dirname(filename), .Platform$file.sep, tempbase,".mtl")
        }
      }
    }
    if(substring(filename, nchar(filename)-3,nchar(filename)) != ".obj") {
      filename = paste0(filename,".obj")
    }
    con = file(filename, "w")
    if(save_texture) {
      con_mtl = file(filename_mtl, "w")
    }
  }

  number_vertices = 0
  number_texcoords = 0
  number_normals = 0
  
  #Writes data and increments vertex/normal/texture counter
  write_data = function(id, con) {
    vertices = rgl.attrib(id, "vertices")
    textures = rgl.attrib(id, "texcoords")
    normals = rgl.attrib(id, "normals")
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
  write_mtl = function(idrow, con) {
    if(!is.na(idrow$texture_file)) {
      cat(paste("newmtl ray_surface \n"), file=con)
      file.copy(idrow$texture_file[[1]], paste0(dirname(filename), .Platform$file.sep , "raysurface.png"), overwrite = TRUE)
      cat(paste("map_Ka raysurface.png" ,"\n"), file=con)
      cat(paste("map_Kd raysurface.png" ,"\n"), file=con)
      cat("\n", file=con)
    } else if (!is.na(idrow$base_color[[1]])) {
      tempcol = col2rgb(idrow$base_color[[1]])/256
      cat(paste("newmtl ray_base"), file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat(paste("Ka", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat("\n", file=con)
    } else if (!is.na(idrow$water_color[[1]])) {
      tempcol = col2rgb(idrow$water_color[[1]])/256
      cat(paste("newmtl ray_water"), file=con, sep="\n")
      cat(paste("Ns", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat(paste("Kd", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat(paste("Ka", sprintf("%1.4f %1.4f %1.4f",tempcol[1],tempcol[2],tempcol[3]),collapse = " "), file=con, sep="\n")
      cat(paste("d", sprintf("%1.4f",idrow$water_alpha[[1]]),collapse = " "), file=con, sep="\n")
      cat(paste("Ni", sprintf("%1.4f",water_index_refraction),collapse = " "), file=con, sep="\n")
      cat("\n", file=con)
    }
  }
  
  string_num = function(n) {
    sprintf("%d", n)
  }
  
  #Begin file
  cat("#", paste0(filename, " produced by rayshader"), "\n", file=con)
  if(save_texture) {
    cat("mtllib", gsub(paste0(dirname(filename), .Platform$file.sep), replacement="", x=filename_mtl), "\n", file=con)
  }
  
  vertex_info = get_ids_with_labels()
  vertex_info$startindex = NA
  vertex_info$startindextex = NA
  vertex_info$startindexnormals = NA
  vertex_info$endindex = NA
  vertex_info$endindextex = NA
  vertex_info$endindexnormals = NA
  wrote_water = FALSE
  for(row in 1:nrow(vertex_info)) {
    id = vertex_info$id[row]
    if(vertex_info$raytype[row] %in% c("surface","base","basebottom","water")) {
      vertex_info$startindex[row] = number_vertices + 1
      vertex_info$startindextex[row] = number_texcoords + 1
      vertex_info$startindexnormals[row] = number_normals + 1
      write_data(id, con)
      if(vertex_info$raytype[row] != "water") {
        if(save_texture) {
          write_mtl(vertex_info[row,], con_mtl)
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
  browser()
  for(row in 1:nrow(vertex_info)) {
    if(vertex_info$raytype[row] == "surface") {
      if(save_texture) {
        cat("g Surface", file=con, sep ="\n")
        cat("usemtl ray_surface", file=con, sep ="\n")
      }
      dims = rgl::rgl.attrib(vertex_info$id[row], "dim")
      nx = dims[1]
      nz = dims[2] 
      indices = rep(0, 6 * (nz - 1) * (nx - 1))
      counter = 0
      for(i in seq_len(nz)[-nz]) {
        for(j in seq_len(nx)[-nx]) {
          cindices = (i-1)*nx + c(j, j + nx, j + 1, j + nx, j + nx + 1, j + 1)
          indices[(1:6 + 6*counter)] = cindices
          counter = counter + 1
        }
      }
      indices = sprintf("%d/%d/%d", indices, indices, indices)
      indices = matrix(indices, ncol=3, byrow=TRUE)
      cat(paste("f", indices[,1], indices[,2], indices[,3]), 
          sep="\n", file=con)
    } else if (vertex_info$raytype[row] == "base"){
      if(save_texture) {
        cat("g Base", file=con, sep ="\n")
        cat("usemtl ray_base", file=con, sep ="\n")
      }
      baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
      if(vertex_info$endindexnormals[row] > vertex_info$startindexnormals[row]) {
        cat(paste("f", sprintf("%d//%d %d//%d %d//%d", baseindices[,1], baseindices[,1],
                               baseindices[,2], baseindices[,2], 
                               baseindices[,3], baseindices[,3])), 
            sep="\n", file=con)
      } else {
        cat(paste("f", sprintf("%d %d %d", baseindices[,1],
                               baseindices[,2], 
                               baseindices[,3])), 
            sep="\n", file=con)
      }
    } else if (vertex_info$raytype[row] == "water") {
      if(save_texture) {
        cat("g Water", file=con, sep ="\n")
        cat("usemtl ray_water", file=con, sep ="\n")
      }
      if(vertex_info$type[row] == "surface") {
        dims = rgl::rgl.attrib(vertex_info$id[row], "dim")
        nx = dims[1]
        nz = dims[2] 
        indices = rep(0, 6 * (nz - 1) * (nx - 1))
        counter = 0
        for(i in seq_len(nz)[-nz]) {
          for(j in seq_len(nx)[-nx]) {
            cindices = (i-1)*nx + c(j, j + nx, j + 1, j + nx, j + nx + 1, j + 1) + vertex_info$startindex[row]/3
            indices[(1:6 + 6*counter)] = cindices
            counter = counter + 1
          }
        }
        indices = matrix(indices, ncol=3, byrow=TRUE)
        cat(paste("f", sprintf("%d %d %d", indices[,1], indices[,2], indices[,3])), 
            sep="\n", file=con)
      } else {
        baseindices = matrix(vertex_info$startindex[row]:vertex_info$endindex[row], ncol=3, byrow=TRUE)
        cat(paste("f", baseindices[,1], baseindices[,2], baseindices[,3]), 
            sep="\n", file=con)
      }
    }
  }
  close(con)
  if(save_texture) {
    close(con_mtl)
  }
}
