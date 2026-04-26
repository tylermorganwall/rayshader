#'@title Save MULTIPOLYGON Z sf data to OBJ file
#'
#'@description Converts MULTIPOLYGON Z features into a 3D OBJ model
#'
#'@param sfobj sf object with MULTIPOLYGON Z geometry,
#'@param filename Filename of the OBJ to save the 3D model to.
#'@param swap_yz Default `TRUE`., Whether to swap and Y and Z axes. (Y axis is vertical in 
#'rayshader coordinates, but data is often provided with Z being vertical).
#'@export
#'@examples
#'#Convert the built-in Washington Monument MULTIPOLYGON Z data to an OBJ file
#'obj_temp = tempfile(fileext=".obj")
#'save_multipolygonz_to_obj(washington_monument_multipolygonz, obj_temp, swap_yz=TRUE)
#'#Render with rgl
#'rgl::open3d()
#'render_obj(filename=obj_temp, xyz=matrix(c(0,0,0),ncol=3), color="red")
#'render_camera(theta=30,phi=40)
save_multipolygonz_to_obj = function(sfobj, filename, swap_yz = FALSE) {
  con = file(filename, "w")
  on.exit(close(con))
  total_verts = 0
  geom = multipolygonz_geometry_indices(sfobj)
  mat_coords = geom$coords
  geometry_list = geom$geometry
  cat_list = vector("list", length(geometry_list) * 2)
  counter = 1

  for (i in seq_along(geometry_list)) {
    single_geom = mat_coords[geometry_list[[i]], 1:3, drop = FALSE]
    if (swap_yz) {
      mat = single_geom[-1, c(1, 3, 2), drop = FALSE]
    } else {
      mat = single_geom[-1, , drop = FALSE]
    }
    indices = seq_len(nrow(mat)) + total_verts
    if (swap_yz) {
      indices = rev(indices)
    }

    cat_list[[counter]] = sprintf("v %.4f %.4f %.4f", mat[, 1], mat[, 2], mat[, 3])
    counter = counter + 1
    cat_list[[counter]] = sprintf("f %s", paste0(indices, collapse = " "))
    counter = counter + 1
    total_verts = total_verts + nrow(mat)
  }
  writeLines(unlist(cat_list, use.names = FALSE), con)
}

multipolygonz_geometry_indices = function(sfobj) {
  mat_coords = sf::st_coordinates(sf::st_geometry(sfobj))
  if (!all(c("X", "Y", "Z", "L2") %in% colnames(mat_coords))) {
    stop("sfobj must contain MULTIPOLYGON Z geometry.", call. = FALSE)
  }
  group_cols = intersect(c("L3", "L2"), colnames(mat_coords))
  group_id = do.call(
    paste,
    c(as.data.frame(mat_coords[, group_cols, drop = FALSE]), sep = "\r")
  )
  list(
    coords = mat_coords,
    geometry = split(
      seq_len(nrow(mat_coords)),
      factor(group_id, levels = unique(group_id))
    )
  )
}

multipolygonz_triangulate_face = function(mat, face) {
  if (length(face) == 3) {
    return(matrix(face, ncol = 3))
  }
  matrix(
    c(
      rep(face[1], length(face) - 2),
      face[seq(2, length(face) - 1)],
      face[seq(3, length(face))]
    ),
    ncol = 3
  )
}

multipolygonz_to_raymesh = function(sfobj, swap_yz = FALSE) {
  geom = multipolygonz_geometry_indices(sfobj)
  verts_per_geometry = lengths(geom$geometry) - 1
  vertex_rows = unlist(lapply(geom$geometry, function(x) x[-1]), use.names = FALSE)
  vertices = geom$coords[vertex_rows, 1:3, drop = FALSE]
  if (swap_yz) {
    vertices = vertices[, c(1, 3, 2), drop = FALSE]
  }
  vertices = round(vertices, 4)
  vertex_offsets = cumsum(c(0, verts_per_geometry[-length(verts_per_geometry)]))
  indices = vector("list", length(geom$geometry))
  for (i in seq_along(geom$geometry)) {
    row_range = seq.int(vertex_offsets[i] + 1, vertex_offsets[i] + verts_per_geometry[i])
    mat = vertices[row_range, , drop = FALSE]
    face = row_range - 1
    if (swap_yz) {
      face = rev(face)
    }
    indices[[i]] = multipolygonz_triangulate_face(mat, face)
  }
  rayvertex::construct_mesh(
    vertices = vertices,
    indices = do.call(rbind, indices)
  )
}
