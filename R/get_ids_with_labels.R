#'@title Get IDs with Labels
#'
#'@description Gets the rgl IDs with associated rayshader labels
#'
#'@param typeval Default `NULL`. Select to filter just one of the types: `surface`, `base`,`lines`, `waterlines`,`shadow`, `basebottom`,
#'@importFrom utils getFromNamespace
#'@return Data frame of IDs with labels
#'@keywords internal
get_ids_with_labels = function(typeval = NULL) {
  ray_types = c("surface",           "base",          "water",
                "lines",             "waterlines",    "shadow",
                "basebottom",        "textline",      "raytext",
                "north_symbol",      "arrow_symbol",  "bevel_symbol",  
                "background_symbol", "scalebar_col1", "scalebar_col2",
                "text_scalebar",     "surface_tris",  "path3d",       
                "points3d",          "polygon3d")
  get_rgl_material = getFromNamespace("rgl.getmaterial", "rgl")
  idvals = rgl::rgl.ids(tags = TRUE)
  material_type = idvals$tag
  material_properties = vector("list", nrow(idvals))
  for(i in 1:nrow(idvals)) {
    material_type_single = get_rgl_material(id=idvals[i,1])
    material_properties[[i]]$texture_file = NA
    material_properties[[i]]$base_color = NA
    material_properties[[i]]$water_color = NA
    material_properties[[i]]$water_alpha = NA
    material_properties[[i]]$line_color = NA
    material_properties[[i]]$waterline_color = NA
    material_properties[[i]]$waterline_alpha = NA
    material_properties[[i]]$shadow_texture_file = NA
    material_properties[[i]]$north_color = NA
    material_properties[[i]]$arrow_color = NA
    material_properties[[i]]$bevel_color = NA
    material_properties[[i]]$background_color = NA
    material_properties[[i]]$scalebar1_color = NA
    material_properties[[i]]$scalebar2_color = NA
    material_properties[[i]]$point_color = NA
    material_properties[[i]]$tricolor = NA
    
    if(idvals$type[i] != "text") {
      if(material_type[i] %in% c("surface", "surface_tris")) {
        material_properties[[i]]$texture_file = material_type_single$texture
      } 
      if(material_type[i] == "base") {
        material_properties[[i]]$base_color = material_type_single$color
      } 
      if(material_type[i] == "water") {
        material_properties[[i]]$water_color = material_type_single$color
        material_properties[[i]]$water_alpha = material_type_single$alpha
      } 
      if(material_type[i] == "lines" || material_type[i] == "path3d") {
        material_properties[[i]]$line_color = material_type_single$color
      }
      if(material_type[i] == "points3d") {
        material_properties[[i]]$point_color = material_type_single$color
      }
      if(material_type[i] == "waterlines") {
        material_properties[[i]]$waterline_color = material_type_single$color
        material_properties[[i]]$waterline_alpha = material_type_single$alpha
      }
      if(material_type[i] == "shadow") {
        material_properties[[i]]$shadow_texture_file = material_type_single$texture
      }
      if(material_type[i] == "north_symbol") {
        material_properties[[i]]$north_color = material_type_single$color
      }
      if(material_type[i] == "arrow_symbol") {
        material_properties[[i]]$arrow_color = material_type_single$color
      }
      if(material_type[i] == "bevel_symbol") {
        material_properties[[i]]$bevel_color = material_type_single$color
      }
      if(material_type[i] == "background_symbol") {
        material_properties[[i]]$background_color = material_type_single$color
      }
      if(material_type[i] == "scalebar_col1") {
        material_properties[[i]]$scalebar1_color = material_type_single$color
      }
      if(material_type[i] == "scalebar_col2") {
        material_properties[[i]]$scalebar2_color = material_type_single$color
      }
      if(material_type[i] == "polygon3d") {
        material_properties[[i]]$tricolor = material_type_single$color
      }
    } 
  }
  full_properties = do.call(rbind,material_properties)
  retval = cbind(idvals,full_properties)
  if(!is.null(typeval)) {
    if(any(typeval %in% ray_types)) {
      retval = retval[retval$tag %in% typeval,]
    } 
  }
  return(retval)
}