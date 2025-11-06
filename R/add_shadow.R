#'@title Add Shadow
#'
#'@description Multiplies a texture array or shadow map by a shadow map.
#'
#'@param hillshade A three-dimensional RGB array or 2D matrix of shadow intensities.
#'@param shadowmap A matrix that incidates the intensity of the shadow at that point. 0 is full darkness, 1 is full light.
#'@param max_darken Default `0.7`. The lower limit for how much the image will be darkened. 0 is completely black,
#'1 means the shadow map will have no effect.
#'@param rescale_original Default `FALSE`. If `TRUE`, `hillshade` will be scaled to match the dimensions of `shadowmap` (instead of
#'the other way around).
#'@return Shaded texture map.
#'@export
#'@examples
#'#First we plot the sphere_shade() hillshade of `montereybay` with no shadows
#'
#'if(run_documentation()) {
#'montereybay |>
#'  sphere_shade(colorintensity=0.5) |>
#'  plot_map()
#'}
#'
#'#Raytrace the `montereybay` elevation map and add that shadow to the output of sphere_shade()
#'if(run_documentation()) {
#'montereybay |>
#'  sphere_shade(colorintensity=0.5) |>
#'  add_shadow(ray_shade(montereybay,sunaltitude=20,zscale=50),max_darken=0.3) |>
#'  plot_map()
#'}
#'
#'#Increase the intensity of the shadow map with the max_darken argument.
#'if(run_documentation()) {
#'montereybay |>
#'  sphere_shade(colorintensity=0.5) |>
#'  add_shadow(ray_shade(montereybay,sunaltitude=20,zscale=50),max_darken=0.1) |>
#'  plot_map()
#'}
#'
#'#Decrease the intensity of the shadow map.
#'if(run_documentation()) {
#'montereybay |>
#'  sphere_shade(colorintensity=0.5) |>
#'  add_shadow(ray_shade(montereybay,sunaltitude=20,zscale=50),max_darken=0.7) |>
#'  plot_map()
#'}
add_shadow = function(
	hillshade,
	shadowmap,
	max_darken = 0.7,
	rescale_original = FALSE
) {
	if (length(dim(shadowmap)) == 3 && length(dim(hillshade)) == 2) {
		tempstore = hillshade
		hillshade = shadowmap
		shadowmap = tempstore
	}
	shadowmap = scales::rescale(shadowmap, to = c(max_darken, 1), from = c(0, 1))
	shadow_array = array(0, dim = c(dim(shadowmap), 4))
	shadow_array[,, 4] = 1 - shadowmap
	shadow_array = rayimage::ray_read_image(
		shadow_array,
		assume_colorspace = rayimage::CS_SRGB,
		assume_white = "D65",
		source_linear = TRUE
	)
	return(rayimage::render_image_overlay(
		hillshade,
		image_overlay = rayimage::render_reorient(
			shadow_array,
			transpose = FALSE,
			flipy = FALSE
		)
	))
}
