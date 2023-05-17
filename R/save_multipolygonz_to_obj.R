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
#'obj_temp = tempfile(fileext=".obk")
#'save_multipolygonz_to_obj(washington_monument_multipolygonz, obj_temp, swap_yz=TRUE)
#'#Render with rgl
#'rgl::open3d()
#'render_obj(filename=obj_temp, xyz=matrix(c(0,0,0),ncol=3), color="red")
#'render_camera(theta=30,phi=40)
save_multipolygonz_to_obj = function(sfobj, filename, swap_yz = FALSE) {
  con = file(filename, "w")
  on.exit(close(con))
  total_verts = 0
  cat_list = list()
  counter = 1
  geometry_list = sf::st_geometry(sfobj)
  num_polygons = length(sfobj)
  
  for(j in seq_len(num_polygons)) {
    single_geom = geometry_list
    number = length(single_geom)
    for(i in seq_len(number)) {
      if(swap_yz) {
        mat = as.matrix(single_geom[[i]])[-1,c(1,3,2)]
      } else {
        mat = as.matrix(single_geom[[i]])[-1,]
      }
      text_mat = matrix(sprintf("%0.4f", mat),ncol=ncol(mat), nrow=nrow(mat))
      indices = sprintf("%d", seq_len(nrow(mat))+total_verts)
      if(swap_yz) {
        indices = rev(indices)
      }
      
      cat_list[[counter]] = sprintf("v %s", apply(text_mat,1,paste0, collapse=" "))
      counter = counter + 1
      cat_list[[counter]] = sprintf("f %s", paste0(indices,collapse = " "))
      counter = counter + 1
      total_verts = total_verts + nrow(mat)
    }
  }
  cat(unlist(cat_list),file=con, sep="\n")
}
