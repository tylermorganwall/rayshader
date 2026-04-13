#'@title Calculate Surface Color Map
#'
#'@description Calculates a color for each point on the surface using the surface normals and
#' hemispherical UV mapping. This uses either a texture map provided by the user (as an RGB array),
#' or a built-in color texture.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param sunangle Default `315` (NW). The direction of the main highlight color (derived from the built-in palettes or the [create_texture()] function).
#'@param texture Default `imhof1`. Either a square matrix indicating the spherical texture mapping, or a string indicating one
#'of the built-in palettes (`imhof1`,`imhof2`,`imhof3`,`imhof4`,`desert`, `bw`, and `unicorn`).
#'@param normalvectors Default `NULL`. Cache of the normal vectors (from [calculate_normal()] function). Supply this to speed up texture mapping.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis.
#'If supplied, this also updates the cached hillshade `zscale` for downstream rayshader calls.
#'@param vertical_exaggeration Default `1`. One-off multiplier applied to the
#'effective visual relief for this call. Values greater than `1` increase
#'apparent relief and values between `0` and `1` flatten it. This does not
#'update cached `zscale` metadata.
#'@param colorintensity Deprecated alias for `vertical_exaggeration`.
#'
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar.
#'@return RGB array of hillshaded texture mappings.
#'@export
#'@examples
#'#Basic example:
#'montereybay |>
#'  sphere_shade() |>
#'  plot_map()
#'
#'#Decrease the color intensity:
#'montereybay |>
#'  sphere_shade(vertical_exaggeration=0.1) |>
#'  plot_map()
#'
#'#Change to a built-in color texture:
#'montereybay |>
#'  sphere_shade(texture="desert") |>
#'  plot_map()
#'
#'#Change the highlight angle:
#'montereybay |>
#'  sphere_shade(texture="desert", sunangle = 45) |>
#'  plot_map()
#'
#'#Create our own texture using the `create_texture` function:
#'montereybay |>
#'  sphere_shade(zscale=10,texture=create_texture("#E9C68D","#AF7F38",
#'                                                "#674F30","#494D30",
#'                                                "#B3BEA3")) |>
#'  plot_map()
sphere_shade = function(
	heightmap,
	sunangle = 315,
	texture = "imhof1",
	normalvectors = NULL,
	colorintensity = 1,
	zscale = 1,
	vertical_exaggeration = 1,
	progbar = interactive()
) {
	heightmap_missing = missing(heightmap)
	heightmap_cache_label = format_scene_cache_label(deparse(substitute(
		heightmap
	)))
	zscale_cache_input_label = format_scene_cache_label(deparse(substitute(
		zscale
	)))
	vertical_exaggeration_missing = missing(vertical_exaggeration)
	if (!missing(colorintensity)) {
		.Deprecated(
			msg = paste(
				"`colorintensity` is deprecated in `sphere_shade()`.",
				"Use `vertical_exaggeration` instead."
			)
		)
		if (vertical_exaggeration_missing) {
			vertical_exaggeration = colorintensity
		}
	}
	if (heightmap_missing) {
		resolved_heightmap = resolve_hillshade_heightmap(
			heightmap_missing = TRUE,
			caller = "sphere_shade"
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
	if (!missing(zscale)) {
		zscale = suppressWarnings(as.numeric(zscale)[1])
		if (!is.finite(zscale) || zscale <= 0) {
			stop("`zscale` should be a positive number.")
		}
		cache_hillshade_zscale(zscale, label = zscale_cache_input_label)
	} else {
		zscale = 1
	}
	zscale = apply_vertical_exaggeration(
		zscale = zscale,
		vertical_exaggeration = vertical_exaggeration,
		caller = "sphere_shade"
	)
	sunangle = sunangle / 180 * pi
	if (is.null(normalvectors)) {
		normalvectors = calculate_normal(
			heightmap = heightmap,
			zscale = zscale,
			progbar = progbar
		)
	}
	heightmap = add_padding(heightmap)
	if (methods::is(texture, "character")) {
		if (
			texture %in%
				c("imhof1", "imhof2", "imhof3", "imhof4", "desert", "bw", "unicorn")
		) {
			if (texture == "imhof1") {
				texture = create_texture(
					"#fff673",
					"#55967a",
					"#8fb28a",
					"#55967a",
					"#cfe0a9"
				)
			} else if (texture == "imhof2") {
				texture = create_texture(
					"#f5dfca",
					"#63372c",
					"#dfa283",
					"#195f67",
					"#83a6a0"
				)
			} else if (texture == "imhof3") {
				texture = create_texture(
					"#e9e671",
					"#7f3231",
					"#cbb387",
					"#607080",
					"#7c9695"
				)
			} else if (texture == "imhof4") {
				texture = create_texture(
					"#ffe3b3",
					"#66615e",
					"#f1c3a9",
					"#ac9988",
					"#abaf98"
				)
			} else if (texture == "bw") {
				texture = create_texture("white", "black", "grey75", "grey25", "grey50")
			} else if (texture == "desert") {
				texture = create_texture(
					"#ffe3b3",
					"#6a463a",
					"#dbaf70",
					"#9c9988",
					"#c09c7c"
				)
			} else if (texture == "unicorn") {
				texture = create_texture("red", "green", "blue", "yellow", "white")
			}
		} else {
			stop(
				"Built-in texture palette not recognized: possible choices are `imhof1`,`imhof2`,`imhof3`,`imhof4`,`bw`,`desert`, and `unicorn`"
			)
		}
	}
	center = dim(texture)[1:2] / 2
	heightmap = flipud(t(heightmap)) / zscale
	distancemat = (1 - normalvectors[["z"]]) * center[1]
	lengthmat = sqrt(1 - (normalvectors[["z"]])^2)
	image_x_nocenter = ((-normalvectors[["x"]] / lengthmat * distancemat))
	image_y_nocenter = ((normalvectors[["y"]] / lengthmat * distancemat))
	image_x = floor(
		cos(sunangle) * image_x_nocenter - sin(sunangle) * image_y_nocenter
	) +
		center[1]
	image_y = floor(
		sin(sunangle) * image_x_nocenter + cos(sunangle) * image_y_nocenter
	) +
		center[2]
	image_x[is.na(image_x)] = center[1]
	image_y[is.na(image_y)] = center[2]
	image_x[is.nan(image_x)] = center[1]
	image_y[is.nan(image_y)] = center[2]
	image_x[is.infinite(image_x)] = center[1]
	image_y[is.infinite(image_y)] = center[2]
	image_x[image_x > dim(texture)[1]] = dim(texture)[1]
	image_y[image_y > dim(texture)[2]] = dim(texture)[2]
	image_x[image_x == 0] = 1
	image_y[image_y == 0] = 1
	returnimage = array(dim = c(nrow(heightmap), ncol(heightmap), 3))
	returnimage[,, 1] = construct_matrix(
		texture[,, 1],
		nrow(heightmap),
		ncol(heightmap),
		image_x,
		image_y
	)
	returnimage[,, 2] = construct_matrix(
		texture[,, 2],
		nrow(heightmap),
		ncol(heightmap),
		image_x,
		image_y
	)
	returnimage[,, 3] = construct_matrix(
		texture[,, 3],
		nrow(heightmap),
		ncol(heightmap),
		image_x,
		image_y
	)
	returnimageslice = array(dim = c(nrow(heightmap) - 2, ncol(heightmap) - 2, 3))
	returnimageslice[,, 1] = returnimage[
		c(-1, -nrow(heightmap)),
		c(-1, -ncol(heightmap)),
		1
	]
	returnimageslice[,, 2] = returnimage[
		c(-1, -nrow(heightmap)),
		c(-1, -ncol(heightmap)),
		2
	]
	returnimageslice[,, 3] = returnimage[
		c(-1, -nrow(heightmap)),
		c(-1, -ncol(heightmap)),
		3
	]
	returnimage = rayimage::ray_read_image(
		returnimageslice,
		source_linear = TRUE,
		assume_colorspace = rayimage::CS_SRGB,
		assume_white = "D65"
	)
	cache_hillshade_map(returnimage, label = hillshade_cache_label)
	return(returnimage)
}
