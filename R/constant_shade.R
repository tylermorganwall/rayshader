#'@title Calculate Constant Color Map
#'
#'@description Generates a constant color layer.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'@param color Default `"white"`. Color for the constant layer.
#'@param alpha Default `1`, the alpha transparency.
#'@return RGB array of a single color layer.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Shade a red map
#'montereybay |>
#'  constant_shade("red") |>
#'  add_shadow(lamb_shade(montereybay),0) |>
#'  plot_map()
#'#Shade a green map
#'montereybay |>
#'  constant_shade("green") |>
#'  add_shadow(lamb_shade(montereybay),0) |>
#'  plot_map()
#'#Add a blue tint
#'montereybay |>
#'  height_shade() |>
#'  add_overlay(constant_shade(montereybay, "dodgerblue", alpha=0.25)) |>
#'  add_shadow(lamb_shade(montereybay,zscale=50),0) |>
#'  plot_map()
#'#Use a blank map on which to draw other data
#'montereybay |>
#'  constant_shade() |>
#'  add_overlay(generate_line_overlay(monterey_roads_sf, linewidth=5, color="black",
#'                                    attr(montereybay,"extent"), width = 1080, height = 1080),
#'                                    alphalayer=0.8)  |>
#'  add_water(detect_water(montereybay < 0), "dodgerblue") |>
#'  plot_map()
constant_shade = function(heightmap, color = "white", alpha = 1) {
	heightmap_missing = missing(heightmap)
	heightmap_cache_label = format_scene_cache_label(deparse(substitute(heightmap)))
	if (heightmap_missing) {
		resolved_heightmap = resolve_hillshade_heightmap(
			heightmap_missing = TRUE,
			caller = "constant_shade"
		)
		heightmap = resolved_heightmap$heightmap
	} else {
		heightmap_info = coerce_plot_3d_heightmap(heightmap)
		heightmap = heightmap_info$heightmap
		cache_hillshade_input_context(heightmap_info, label = heightmap_cache_label)
	}
	hillshade_cache_label = if (heightmap_missing) {
		resolved_heightmap$label
	} else {
		heightmap_cache_label
	}
	stopifnot(is.matrix(heightmap))
	return_array = array(alpha, dim = c(ncol(heightmap), nrow(heightmap), 4))
	const_col = convert_color(color)
	return_array[,, 1] = const_col[1]
	return_array[,, 2] = const_col[2]
	return_array[,, 3] = const_col[3]
	return_array = rayimage::ray_read_image(
		return_array,
		assume_colorspace = rayimage::CS_SRGB,
		assume_white = "D65"
	)
	cache_hillshade_map(return_array, label = hillshade_cache_label)
	return(return_array)
}
