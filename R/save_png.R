#'@title Save PNG
#'
#'@description A wrapper around [rayimage::ray_write_image()] to write an image to file.
#'
#'@param hillshade Array (or matrix) of hillshade to be written.
#'@param filename String with the filename. If `.png` is not at the end of the string, it will be appended automatically.
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
#'@param title_just Default `left`. Position of the title.
#'@export
#'@examples
#'filename_map = tempfile()
#'
#'#Save the map into `filename_map`
#'montereybay |>
#'  sphere_shade() |>
#'  save_png(filename_map)
save_png = function(
	hillshade,
	filename,
	title_text = NA,
	title_offset = c(20, 20),
	title_color = "black",
	title_size = 30,
	title_font = "sans",
	title_style = "normal",
	title_bar_color = NA,
	title_bar_alpha = 0.5,
	title_just = "left"
) {
	if (is.null(filename)) {
		stop("save_png requires a filename")
	}
	has_title = !is.na(title_text)
	if (!(length(find.package("rayimage", quiet = TRUE)) > 0) && has_title) {
		warning("`rayimage` package required for title text")
		has_title = FALSE
	}
	if (substring(filename, nchar(filename) - 3, nchar(filename)) != ".png") {
		filename = paste0(filename, ".png")
	}
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
	rayimage::ray_write_image(hillshade_img, filename, clamp = TRUE)
}
