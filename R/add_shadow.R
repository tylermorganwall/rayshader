#'@title Add Shadow
#'
#'@description Multiplies a texture array or shadow map by a shadow map.
#'
#'@param hillshade A three-dimensional RGB array or 2D matrix of shadow intensities.
#'@param shadow_map A matrix that incidates the intensity of the shadow at that point. 0 is full darkness, 1 is full light.
#'@param max_darken Default `0.7`. The lower limit for how much the image will be darkened. 0 is completely black,
#'1 means the shadow map will have no effect.
#'@param rescale_original Default `FALSE`. If `TRUE`, `hillshade` will be scaled to match the dimensions of `shadow_map` (instead of
#'the other way around).
#'@return Shaded texture map.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#First we plot the sphere_shade() hillshade of `montereybay_spatial` with no shadows
#'
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration=20) |>
#'  plot_map()
#'
#'#Raytrace the `montereybay_spatial` elevation map and add that shadow to the output of sphere_shade()
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration=20) |>
#'  add_shadow(ray_shade(sun_altitude=20, vertical_exaggeration = 4),max_darken=0.3) |>
#'  plot_map()
#'
#'#Increase the intensity of the shadow map with the max_darken argument.
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration=20) |>
#'  add_shadow(ray_shade(sun_altitude=20, vertical_exaggeration = 4),max_darken=0.1) |>
#'  plot_map()
#'
#'#Decrease the intensity of the shadow map.
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration=20) |>
#'  add_shadow(ray_shade(sun_altitude=20, vertical_exaggeration = 4),max_darken=0.7) |>
#'  plot_map()
add_shadow = function(
  hillshade,
  shadow_map,
  max_darken = 0.7,
  rescale_original = FALSE
) {
  force(hillshade)
  hillshade_cache_label = get_hillshade_map_label(
    default = format_scene_cache_label(deparse(substitute(hillshade)))
  )
  if (length(dim(shadow_map)) == 3 && length(dim(hillshade)) == 2) {
    tempstore = hillshade
    hillshade = shadow_map
    shadow_map = tempstore
  }
  shadow_map = scales::rescale(
    shadow_map,
    to = c(max_darken, 1),
    from = c(0, 1)
  )
  shadow_array = array(0, dim = c(dim(shadow_map), 4))
  shadow_array[,, 4] = 1 - shadow_map
  shadow_array = rayimage::ray_read_image(
    shadow_array,
    assume_colorspace = rayimage::CS_SRGB,
    assume_white = "D65",
    source_linear = TRUE
  )
  hillshade = rayimage::render_image_overlay(
    hillshade,
    image_overlay = rayimage::render_reorient(
      shadow_array,
      transpose = FALSE,
      flipy = FALSE
    )
  )
  cache_hillshade_map(hillshade, label = hillshade_cache_label)
  return(hillshade)
}
