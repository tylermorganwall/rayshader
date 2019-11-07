#'@title Get IDs with Labels
#'
#'@description Gets the rgl IDs with associated rayshader labels
#'
#'@param typeval Default `NULL`. Select to filter just one of the types: `surface`, `base`,`lines`, `waterlines`,`shadow`, `basebottom`,
#'@importFrom utils getFromNamespace
#'@return Data frame of IDs with labels
#'@keywords internal
get_ids_with_labels = function(typeval = NULL) {
  ambient_encoder = c("#000001" = "surface","#000002" = "base","#000003" = "water",
                      "#000004" = "lines","#000005" = "waterlines","#000006" = "shadow",
                      "#000007" = "basebottom", "#000008" = "textline", "#000009" = "raytext")
  get_rgl_material = getFromNamespace("rgl.getmaterial", "rgl")
  idvals = rgl::rgl.ids()
  material_type = list()
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
    if(idvals$type[i] != "text") {
      material_type[[i]] = ambient_encoder[material_type_single$ambient]
      if(material_type[[i]] == "surface") {
        material_properties[[i]]$texture_file = material_type_single$texture
      } 
      if(material_type[[i]] == "base") {
        material_properties[[i]]$base_color = material_type_single$color
      } 
      if(material_type[[i]] == "water") {
        material_properties[[i]]$water_color = material_type_single$color
        material_properties[[i]]$water_alpha = material_type_single$alpha
      } 
      if(material_type[[i]] == "lines") {
        material_properties[[i]]$line_color = material_type_single$color
      }
      if(material_type[[i]] == "waterlines") {
        material_properties[[i]]$waterline_color = material_type_single$color
        material_properties[[i]]$waterline_alpha = material_type_single$alpha
      }
      if(material_type[[i]] == "shadow") {
        material_properties[[i]]$shadow_texture_file = material_type_single$texture
      }
    } else {
      material_type[[i]] = ambient_encoder[material_type_single$ambient]
    }
  }
  idvals$raytype = unlist(material_type)
  full_properties = do.call(rbind,material_properties)
  retval = cbind(idvals,full_properties)
  if(!is.null(typeval)) {
    if(any(typeval %in% ambient_encoder)) {
      retval = retval[retval$raytype %in% typeval,]
    } 
  }
  return(retval)
}