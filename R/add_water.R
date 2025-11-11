#'@title Add Water
#'
#'@description Adds a layer of water to a map.
#'
#'@param hillshade A three-dimensional RGB array.
#'@param watermap Matrix indicating whether water was detected at that point. 1 indicates water, 0 indicates no water.
#'@param color Default `imhof1`. The water fill color. A hexcode or recognized color string.
#'Also includes built-in colors to match the palettes included in [sphere_shade()]:
#'(`imhof1`,`imhof2`,`imhof3`,`imhof4`, `desert`, `bw`, and `unicorn`).
#'@importFrom grDevices col2rgb rainbow
#'@export
#'@examples
#'#Here we even out a portion of the volcano dataset to simulate water:
#'island_volcano = volcano
#'island_volcano[island_volcano < mean(island_volcano)] = mean(island_volcano)
#'
#'#Setting a minimum area avoids classifying small flat areas as water:
#'if(run_documentation()) {
#'island_volcano |>
#'  sphere_shade(texture="imhof3") |>
#'  add_water(detect_water(island_volcano, min_area = 400),color="imhof3") |>
#'  plot_map()
#'}
#'
#'#We'll do the same thing with the Monterey Bay dataset to fill in the ocean:
#'
#'montbay_water = montereybay
#'montbay_water[montbay_water < 0] = 0
#'
#'if(run_documentation()) {
#'montereybay |>
#'  sphere_shade(texture="imhof4") |>
#'  add_water(detect_water(montbay_water),color="imhof4") |>
#'  plot_map()
#'}
add_water = function(hillshade, watermap, color = "imhof1") {
	hillshade = rayimage::ray_read_image(hillshade)
	colorspace = attr(hillshade, "colorspace")
	white_point = attr(hillshade, "white_current")
	watermap = t(flipud(watermap))
	if (color == "imhof1") {
		color = col2rgb_linear("#e9f9ee")
	} else if (color == "imhof2") {
		color = col2rgb_linear("#337c73")
	} else if (color == "imhof3") {
		color = col2rgb_linear("#4e7982")
	} else if (color == "imhof4") {
		color = col2rgb_linear("#638d99")
	} else if (color == "desert") {
		color = col2rgb_linear("#b2f4ff")
	} else if (color == "bw") {
		color = col2rgb_linear("#ffffff")
	} else if (color != "unicorn") {
		color = col2rgb_linear(color)
	}
	if (length(dim(hillshade)) != 3) {
		if (length(dim(hillshade)) == 2) {
			if (any(hillshade > 1 | hillshade < 0)) {
				stop(
					"Error: Not a shadow matrix. Intensities must be between 0 and 1. Pass your elevation matrix to ray_shade/lamb_shade/ambient_shade/sphere_shade first."
				)
			}
			temp = array(0, dim = c(ncol(hillshade), nrow(hillshade), 3))
			temp[,, 1] = t(flipud(hillshade))
			temp[,, 2] = t(flipud(hillshade))
			temp[,, 3] = t(flipud(hillshade))
			hillshade = temp
		}
	}
	if (missing(watermap)) {
		stop("User must provide matrix indicating locations of bodies of water")
	}
	if (all(dim(watermap) != dim(hillshade)[1:2])) {
		stop(
			"`hillshade` and `watermap` dimensions must be the same; hillshade is ",
			paste0(dim(hillshade)[1:2], collapse = "x"),
			", watermap is ",
			paste0(dim(watermap)[1:2], collapse = "x")
		)
	}
	for (i in 1:3) {
		tempmat = hillshade[,, i]
		if (color[1] != "unicorn") {
			tempmat[watermap >= 1] = color[i]
		} else {
			unicolors = t(col2rgb_linear(rainbow(256)))
			for (row in seq_len(nrow(watermap))) {
				for (col in seq_len(ncol(watermap))) {
					if (watermap[row, col] >= 1) {
						tempmat[row, col] = unicolors[i, (col - 1) %% 256 + 1]
					}
				}
			}
		}
		hillshade[,, i] = tempmat
	}
	return(rayimage::ray_read_image(
		hillshade,
		assume_colorspace = colorspace,
		assume_white = white_point,
		source_linear = TRUE
	))
}
