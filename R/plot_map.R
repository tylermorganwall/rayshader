#'@title Plot Map
#'
#'@description Displays the map in the current device.
#'
#'@param hillshade Hillshade to be plotted.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using `magick::image_annotate()`.
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica",
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_style Default `normal`. Font style (e.g. `italic`).
#'@param title_bar_color Default `NA`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param title_just Default `left`. Justification of the title.
#'@param ... Additional arguments to pass to the `raster::plotRGB` function that displays the map.
#'@export
#'@examples
#'#Plotting the Monterey Bay dataset with bathymetry data
#'if(run_documentation()) {
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'#For compass text
#'par(family = "Arial")
#'
#'#Set everything below 0m to water palette
#'montereybay |>
#'  sphere_shade(zscale=10) |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  |>
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) |>
#'  plot_map()
#'}
plot_map = function(
	hillshade,
	title_text = NA,
	title_offset = c(20, 20),
	title_color = "black",
	title_size = 30,
	title_font = "sans",
	title_style = "normal",
	title_bar_color = NA,
	title_bar_alpha = 0.5,
	title_just = "left",
	...
) {
	has_title = !is.na(title_text)
	hillshade_img = rayimage::ray_read_image(
		hillshade
	)
	if (has_title) {
		hillshade_img = rayimage::render_title(
			hillshade_img,
			title_text = title_text,
			title_offset = title_offset,
			title_color = title_color,
			title_size = title_size,
			title_font = title_font,
			title_style = title_style,
			title_bar_color = title_bar_color,
			title_bar_alpha = title_bar_alpha,
			title_just = title_just
		)
	}
	rayimage::plot_image(hillshade_img, ...)
}
