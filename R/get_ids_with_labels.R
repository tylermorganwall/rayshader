#'@title Get IDs with Labels
#'
#'@description Gets the rgl IDs with associated rayshader labels
#'
#'@importFrom utils getFromNamespace
#'@return Data frame of IDs with labels
#'@keywords internal
get_ids_with_labels = function() {
  ambient_encoder = c("#000001" = "surface","#000002" = "base","#000003" = "water",
                      "#000004" = "lines","#000005" = "waterlines","#000006" = "shadow")
  get_rgl_material = getFromNamespace("rgl.getmaterial", "rgl")
  idvals = rgl::rgl.ids()
  material_properties = list()
  for(i in 1:nrow(idvals)) {
    material_properties_single = get_rgl_material(id=idvals[i,1])
    if(idvals$type[i] != "surface") {
      material_properties[[i]] = ambient_encoder[material_properties_single$ambient]
      if(material_properties[[i]] == "surface") {
        texture_file = material_properties_single$texture
      }
      if(material_properties[[i]] == "base") {
        base_color = material_properties_single$color
      }
      if(material_properties[[i]] == "water") {
        water_color = material_properties_single$color
        water_alpha = material_properties_single$alpha
      }
      if(material_properties[[i]] == "lines") {
        line_color = material_properties_single$color
      }
      if(material_properties[[i]] == "waterlines") {
        waterline_color = material_properties_single$color
      }
      if(material_properties[[i]] == "shadow") {
        shadow_texture_file = material_properties_single$texture
      }
    } else {
      material_properties[[i]] = ambient_encoder[material_properties_single$ambient]
      if(material_properties[[i]] == "surface") {
        texture_file = material_properties_single$texture
      }
      if(material_properties[[i]] == "base") {
        base_color = material_properties_single$color
      }
      if(material_properties[[i]] == "water") {
        water_color = material_properties_single$color
        water_alpha = material_properties_single$alpha
      }
      if(material_properties[[i]] == "shadow") {
        shadow_texture_file = material_properties_single$texture
      }
    }
    if(idvals$type[i] != "text") {
      material_properties[[i]] = ambient_encoder[material_properties_single$ambient]
    } else {
      material_properties[[i]] = "text"
    }
  }
  
  idvals$raytype = unlist(material_properties)
  idvals
}