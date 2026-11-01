#' Build rayrender instances from cached lightweight models
#'
#' @param instance_layers Cached model transform layers.
#' @param bbox_center Scene bounding-box center.
#' @param override_material Whether the global high-quality material overrides
#' model materials.
#' @param material Global high-quality material.
#' @param rgl_materials Named rgl material overrides.
#' @param calculate_consistent_normals Whether rayrender should make model
#' normals consistent.
#'
#' @param rgl_tag Default `"objtree"`. Material override tag for these models.
#' @return A list of rayrender instance objects.
#' @keywords internal
make_render_highquality_cached_model_instances = function(
  instance_layers,
  bbox_center,
  override_material,
  material,
  rgl_materials,
  calculate_consistent_normals,
  rgl_tag = "objtree"
) {
  if (!length(instance_layers)) {
    return(list())
  }
  required_columns = c(
    "model_file",
    "load_material",
    "load_normals",
    "color",
    "x",
    "y",
    "z",
    "angle_x",
    "angle_y",
    "angle_z",
    "scale_x",
    "scale_y",
    "scale_z",
    "base_scale_x",
    "base_scale_y",
    "base_scale_z"
  )
  valid_layers = Filter(
    function(layer) {
      is.data.frame(layer) &&
        nrow(layer) > 0L &&
        all(required_columns %in% names(layer))
    },
    instance_layers
  )
  if (!length(valid_layers)) {
    return(list())
  }
  components = do.call(rbind, valid_layers)
  row.names(components) = NULL
  proportion_value = function(value) {
    formatC(signif(value, 12L), digits = 12L, format = "fg", flag = "#")
  }
  group_key = paste(
    components$model_file,
    components$load_material,
    components$load_normals,
    components$color,
    proportion_value(components$base_scale_x),
    proportion_value(components$base_scale_y),
    proportion_value(components$base_scale_z),
    sep = "\r"
  )
  component_groups = split(seq_len(nrow(components)), group_key)
  lapply(component_groups, function(group_rows) {
    group = components[group_rows, , drop = FALSE]
    color = convert_color(group$color[[1L]], linear = TRUE)
    component_material = NULL
    if (isTRUE(override_material)) {
      component_material = material
    } else {
      component_material = resolve_render_highquality_rgl_material(
        rgl_materials = rgl_materials,
        id = NA_integer_,
        tag = rgl_tag,
        color = color
      )
    }
    load_material = isTRUE(group$load_material[[1L]]) &&
      is.null(component_material)
    if (is.null(component_material)) {
      component_material = rayrender::diffuse(color = color)
    }
    base_model = rayrender::obj_model(
      filename = group$model_file[[1L]],
      load_material = load_material,
      load_normals = isTRUE(group$load_normals[[1L]]),
      calculate_consistent_normals = calculate_consistent_normals,
      material = component_material
    )
    rayrender::create_instances(
      ray_scene = base_model,
      x = group$x - bbox_center[[1L]],
      y = group$y - bbox_center[[2L]],
      z = group$z - bbox_center[[3L]],
      angle_x = group$angle_x,
      angle_y = group$angle_y,
      angle_z = group$angle_z,
      scale_x = group$scale_x,
      scale_y = group$scale_y,
      scale_z = group$scale_z
    )
  })
}
