#'@title Fix Manifold Geometry
#'
#'@description Writes the textured 3D rayshader visualization to an OBJ file.
#'
#'@param filename String with the filename. If `.obj` is not at the end of the string, it will be appended automatically.
#'@keywords internal
#'@examples
#'#Test
fix_manifold_geometry = function(filename) {
  if(tools::file_ext(filename) != "obj") {
    filename = paste0(filename,".obj")
  }
  fullfile = scan(filename, what=character(), sep="\n", quiet = TRUE)
  file_entries = unlist(lapply(fullfile,strsplit,fixed=TRUE,split=" "), recursive = FALSE)
  vertex_list = list()
  vcount = 1
  face_list = list()
  fcount = 1
  texture_list = list()
  tcount = 1
  normal_list = list()
  ncount = 1
  header_list = list()
  hcount = 1
  header = TRUE
  for(i in 1:length(file_entries)) {
    
    if(file_entries[[i]][1] == "v") {
      vertex_list[[vcount]] = file_entries[[i]]
      vcount = vcount + 1
      header = FALSE
    }
    if(file_entries[[i]][1] == "vt") {
      texture_list[[tcount]] = file_entries[[i]]
      tcount = tcount + 1
      header = FALSE
    }
    if(file_entries[[i]][1] == "vn") {
      normal_list[[ncount]] = file_entries[[i]]
      ncount = ncount + 1
      header = FALSE
    }
    if(file_entries[[i]][1] == "f" || file_entries[[i]][1] == "g" || 
       file_entries[[i]][1] == "usemtl" || file_entries[[i]][1] == "s") {
      face_list[[fcount]] = file_entries[[i]]
      fcount = fcount + 1
      header = FALSE
    }
    if(header) {
      header_list[[hcount]] = file_entries[[i]]
      hcount = hcount + 1
    }
  }
  
  vert_mat = as.data.frame(do.call(rbind, vertex_list), stringsAsFactors = FALSE)
  vert_mat[,2] = as.numeric(vert_mat[,2])
  vert_mat[,3] = as.numeric(vert_mat[,3])
  vert_mat[,4] = as.numeric(vert_mat[,4])

  is_dupe = duplicated(vert_mat)
  unique_verts = vert_mat[!is_dupe,]
  
  new_indices = 1:nrow(vert_mat)
  
  tol = 1e-5
  pb = progress::progress_bar$new(format = "  Processing mesh: [:bar] :percent",
                                  total = nrow(vert_mat)/100, clear = TRUE, width= 60)
  for(i in 1:nrow(vert_mat)) {
    if(i %% 100 == 0) {
      pb$tick()
    }
    new_indices[i] = min(which(abs(vert_mat[i,2] - unique_verts[,2]) < tol & 
                           abs(vert_mat[i,3] - unique_verts[,3]) < tol & 
                           abs(vert_mat[i,4] - unique_verts[,4]) < tol))
  }
  pb$terminate()
  face_list2 = face_list
  removed = 0
  for(i in 1:length(face_list)) {
    if(face_list[[i]][1] == "f") {
      temp_face = face_list[[i]][-1]
      new_nums1 = as.character(new_indices[as.integer(unlist(strsplit(paste0(temp_face[1],"/"), split = "/",fixed=TRUE)))])
      new_nums1[is.na(new_nums1)] = ""
      new_nums1 = paste(new_nums1,collapse="/")
      
      new_nums2 = as.character(new_indices[as.integer(unlist(strsplit(paste0(temp_face[2],"/"), split = "/",fixed=TRUE)))])
      new_nums2[is.na(new_nums2)] = ""
      new_nums2 = paste(new_nums2,collapse="/")
      
      new_nums3 = as.character(new_indices[as.integer(unlist(strsplit(paste0(temp_face[3],"/"), split = "/",fixed=TRUE)))])
      new_nums3[is.na(new_nums3)] = ""
      new_nums3 = paste(new_nums3,collapse="/")

      if(length(unique(c(new_nums1,new_nums2,new_nums3))) == 3) {
        face_list2[[i - removed]] = c("f", as.character(c(new_nums1,new_nums2,new_nums3)))
      } else {
        face_list2[[i - removed]] = NULL
        removed = removed + 1
      }
    }
  }
  for(i in rev(seq_len(length(normal_list)))) {
    if(is_dupe[i]) {
      normal_list[[i]] = NULL
    }
  }
  new_file = file(filename, open="wt")
  on.exit(close(new_file))
  header_write = unlist(lapply(header_list,paste0, collapse=" "))
  vertex_write = unlist(lapply(unique(vertex_list),paste0, collapse=" "))
  texture_write = unlist(lapply(unique(texture_list),paste0, collapse=" "))
  normal_write = unlist(lapply(normal_list,paste0, collapse=" "))
  faces_write = unlist(lapply(face_list2,paste0, collapse=" "))
  
  cat(header_write, file = new_file, sep = "\n")
  cat(vertex_write, file = new_file, sep = "\n")
  cat(texture_write, file = new_file, sep = "\n")
  cat(normal_write, file = new_file, sep = "\n")
  cat(faces_write, file = new_file, sep = "\n")
}