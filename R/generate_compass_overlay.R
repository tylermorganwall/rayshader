#'@title Generate Compass Overlay
#'
#'@description This adds the compass
#'
#'Based on code from "Auxiliary Cartographic Functions in R: North Arrow, Scale Bar, and Label with a Leader Arrow"
#'
#'@param x Default `NULL`. The horizontal percentage across the map (measured from the bottom-left corner) where
#'the compass is located.
#'@param y Default `NULL`. The vertical percentage across the map (measured from the bottom-left corner) where
#'the compass is located.
#'@param size Default `0.05`. Size of the compass, in percentage of the map size..
#'@param text_size Default `1`. Text size.
#'@param bearing Default `0`. Angle (in degrees) of north.
#'@param color1 Default `NA`, `white` except for `compass_type == "triangle_circle"` . Primary color of the compass.
#'@param color2 Default `NA`, `black`. Secondary color of the symcompass.
#'@param text_color Default `black`. Text color.
#'@param border_color Default `black`. Border color of the scale bar.
#'@param border_width Default `1`. Width of the scale bar border.
#'@param compass_type Default `"classic"`. Set to `"split_arrow"`, `"triangle_circle"`, or
#'`"split_arrow_ring"` to draw simple polygon north arrows that match the styles shown above.
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting
#'RGB image array automatically.
#'@param width Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param resolution_multiply Default `1`. If passing in `heightmap` instead of width/height, amount to
#'increase the resolution of the overlay, which should make lines/polygons finer.
#'Should be combined with \link[=add_overlay]{\code{add_overlay(rescale_original = TRUE)}} to ensure those added details are captured
#'in the final map.
#'@param halo_color Default `NA`, no halo. If a color is specified, the compass will be surrounded by a halo
#'of this color.
#'@param halo_expand Default `1`. Number of pixels to expand the halo.
#'@param halo_alpha Default `1`. Transparency of the halo.
#'@param halo_offset Default `c(0,0)`. Horizontal and vertical offset to apply to the halo, in percentage of the image.
#'@param halo_blur Default `1`. Amount of blur to apply to the halo. Values greater than `30` won't result in further blurring.
#'@param halo_edge_softness Default `0.1`. Width of the softened halo edge transition, in pixels.
#'@return Semi-transparent overlay with a compass.
#'@export
#'@examples
#'if(run_documentation()) {
#'#Create the water palette
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'
#'#Generate flat water heightmap
#'mbay = montereybay
#'mbay[mbay < 0] = 0
#'
#'base_map = mbay |>
#'  height_shade() |>
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  |>
#'  add_shadow(lamb_shade(montereybay,zscale=50),0.3)
#'
#'#Plot a compass
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay)) |>
#'  plot_map()
#'}
#'
#'if(run_documentation()) {
#'#Change the position to be over the water
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15)) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Change the type
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15,
#'                                       compass_type = "split_arrow")) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Change the type
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15,
#'                                       compass_type = "split_arrow_ring")) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Change the type
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15,
#'                                       compass_type = "triangle_circle")) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Change the text color for visibility
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, compass_type = "split_arrow",
#' x = 0.15, text_color="white")) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Alternatively, add a halo color to improve contrast
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              compass_type = "split_arrow", halo_color="white", halo_expand = 2)) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Change the color scheme
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              compass_type = "split_arrow", halo_color="white",
#'              halo_expand = 2, color1 = "purple", color2 = "red")) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Remove the inner border
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              border_color=NA,compass_type = "split_arrow",
#'              halo_color="white", halo_expand = 2,
#'              color1 = "darkolivegreen4", color2 = "burlywood3")) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Change the size of the compass and text
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.75, y=0.75,
#'              halo_color="white", halo_expand = 2,compass_type = "classic",
#'              size=0.075*2, text_size = 1.25)) |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.45, y=0.45,
#'              halo_color="white", halo_expand = 2,compass_type = "split_arrow_ring",
#'              size=0.075)) |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              halo_color="white", halo_expand = 2,compass_type = "split_arrow",
#'              size=0.075/2, text_size = 0.75)) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Change the bearing of the compass
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              halo_color="white", halo_expand = 2, bearing=30, compass_type = "classic",
#'              size=0.075)) |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.35, y=0.15,
#'              halo_color="white", halo_expand = 2, bearing=15, compass_type = "triangle_circle",
#'              size=0.075)) |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.35,
#'              halo_color="white", halo_expand = 2, bearing=-45, compass_type = "split_arrow_ring",
#'              size=0.075)) |>
#'  plot_map()
#'}
#'if(run_documentation()) {
#'#Create a drop shadow effect
#'base_map |>
#'  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.15,
#'              text_color="white", halo_alpha=0.7, halo_blur=3,
#'              halo_color="black", halo_expand = 2, halo_offset = c(0.002,-0.002))) |>
#'	add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.35, y=0.15,
#'              text_color="white", halo_alpha=0.7, halo_blur=3,
#'              compass_type = "split_arrow",
#'              halo_color="black", halo_expand = 2, halo_offset = c(0.002,-0.002))) |>
#'	add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, y=0.35,
#'              text_color="white", halo_alpha=0.2, halo_blur=8,
#'              compass_type = "split_arrow_ring",
#'              halo_color="white", halo_expand = 2)) |>
#'  plot_map()
#'}
generate_compass_overlay = function(
	x = 0.85,
	y = 0.15,
	size = 0.05,
	text_size = 1,
	bearing = 0,
	heightmap = NULL,
	width = NA,
	height = NA,
	resolution_multiply = 1,
	color1 = NA,
	color2 = NA,
	text_color = NA,
	border_color = "black",
	border_width = 1,
	compass_type = c(
		"classic",
		"split_arrow",
		"triangle_circle",
		"split_arrow_ring"
	),
	halo_color = NA,
	halo_expand = 2,
	halo_alpha = 1,
	halo_offset = c(0, 0),
	halo_blur = 0,
	halo_edge_softness = 0.1
) {
	compass_type = match.arg(compass_type)
	if (!(length(find.package("ragg", quiet = TRUE)) > 0)) {
		png_device = grDevices::png
	} else {
		png_device = ragg::agg_png
	}
	loc = rep(0, 2)
	loc[1] = x
	loc[2] = y
	if (is.na(height)) {
		height = ncol(heightmap)
	}
	if (is.na(width)) {
		width = nrow(heightmap)
	}
	height = height * resolution_multiply
	width = width * resolution_multiply

	if (is.na(color1)) {
		if (compass_type %in% c("classic", "split_arrow", "split_arrow_ring")) {
			color1 = "white"
		} else {
			color1 = "black"
		}
	}
	if (is.na(color2)) {
		color2 = "black"
	}
	if (is.na(text_color)) {
		if (compass_type %in% c("split_arrow_ring", "triangle_circle")) {
			text_color = "white"
		} else {
			text_color = "black"
		}
	}

	bearing = -bearing * pi / 180
	# default colors are white and black
	if (compass_type == "classic") {
		cols <- rep(c(color1, color2), 8)
		radii <- rep(size / c(1, 4, 2, 4), 4)
		x <- radii[(0:15) + 1] * cos((0:15) * pi / 8 + bearing) + loc[1]
		y <- radii[(0:15) + 1] * sin((0:15) * pi / 8 + bearing) + loc[2]
	}
	cos_b = cos(bearing)
	sin_b = sin(bearing)
	transform_points = function(points, offset = c(0, 0)) {
		if (is.null(dim(points))) {
			points = matrix(points, ncol = 2)
		}
		rot = matrix(c(cos_b, -sin_b, sin_b, cos_b), ncol = 2, byrow = TRUE)
		transformed = points %*% t(rot)
		transformed[, 1] = transformed[, 1] + loc[1] + offset[1]
		transformed[, 2] = transformed[, 2] + loc[2] + offset[2]
		return(transformed)
	}
	draw_circle = function(center, radius, fill, border, offset = c(0, 0)) {
		if (radius <= 0) {
			return(invisible())
		}
		theta = seq(0, 2 * pi, length.out = 200)
		center_pt = transform_points(center, offset)
		xvals = center_pt[1, 1] + radius * cos(theta)
		yvals = center_pt[1, 2] + radius * sin(theta)
		graphics::polygon(
			xvals,
			yvals,
			col = fill,
			border = border,
			lwd = border_width * 2
		)
	}
	draw_compass = function(offset = c(0, 0)) {
		if (compass_type == "classic") {
			for (i in 1:15) {
				x1 <- c(x[i], x[i + 1], loc[1]) + offset[1]
				y1 <- c(y[i], y[i + 1], loc[2]) + offset[2]
				graphics::polygon(
					x1,
					y1,
					col = cols[i],
					border = border_color,
					lwd = border_width
				)
			}
			graphics::polygon(
				c(x[16], x[1], loc[1]) + offset[1],
				c(y[16], y[1], loc[2]) + offset[2],
				col = cols[16],
				border = border_color,
				lwd = border_width
			)
			b <- c("E", "N", "W", "S")
			for (i in 0:3) {
				graphics::text(
					(size + graphics::par("cxy")[1] * resolution_multiply) *
						cos(bearing + i * pi / 2) +
						loc[1] +
						offset[1],
					(size + graphics::par("cxy")[2] * resolution_multiply) *
						sin(bearing + i * pi / 2) +
						loc[2] +
						offset[2],
					b[i + 1],
					cex = text_size,
					col = text_color,
					font = 2
				)
			}
			return(invisible())
		}
		if (compass_type %in% c("split_arrow", "split_arrow_ring")) {
			mult = if (compass_type == "split_arrow_ring") 0.8 else 0.8
			mult_txt = if (compass_type == "split_arrow_ring") 0.8 else 1
			mult_width = if (compass_type == "split_arrow_ring") 1 else 0.8

			tip_y = size * mult
			base_y = -size * 1.3 * mult / 1.5
			center_y = -size * 0.5 * mult / 1.5
			half_width = size * mult_width * mult / 1.5
			text_size = text_size * mult_txt
			left_poly = rbind(
				c(0, tip_y),
				c(-half_width, base_y),
				c(0, center_y)
			)
			right_poly = rbind(
				c(0, tip_y),
				c(0, center_y),
				c(half_width, base_y)
			)
			left_poly = transform_points(left_poly, offset)
			right_poly = transform_points(right_poly, offset)
			graphics::polygon(
				left_poly[, 1],
				left_poly[, 2],
				col = color1,
				border = border_color,
				lwd = border_width
			)
			graphics::polygon(
				right_poly[, 1],
				right_poly[, 2],
				col = color2,
				border = border_color,
				lwd = border_width
			)
			if (compass_type == "split_arrow_ring") {
				ring_radius = size * 1.1
				draw_circle(
					c(0, 0),
					ring_radius,
					fill = NA,
					border = border_color,
					offset = offset
				)
				top_circle_center = c(0, ring_radius)
				top_circle_radius = size * 0.2
				draw_circle(
					top_circle_center,
					top_circle_radius,
					fill = border_color,
					border = border_color,
					offset = offset
				)
				text_center = transform_points(top_circle_center, offset)
				graphics::text(
					text_center[1, 1],
					text_center[1, 2],
					"N",
					cex = text_size,
					col = text_color,
					font = 2
				)
			} else {
				text_loc = transform_points(c(0, tip_y + size * 0.4), offset * 0.8)
				graphics::text(
					text_loc[1, 1],
					text_loc[1, 2],
					"N",
					cex = text_size,
					col = text_color,
					font = 2
				)
			}
			return(invisible())
		}
		if (compass_type == "triangle_circle") {
			size = size * 0.7
			tip_y = size * 1.5
			base_y = -size * 0.7
			half_width = size * 0.7
			center_bottom = -size * 0.3
			text_size = text_size * 0.8
			arrow_poly = rbind(
				c(0, tip_y),
				c(-half_width, base_y),
				c(0, center_bottom),
				c(half_width, base_y)
			)
			arrow_poly = transform_points(arrow_poly, offset)
			graphics::polygon(
				arrow_poly[, 1],
				arrow_poly[, 2],
				col = color1,
				border = border_color,
				lwd = border_width
			)
			circle_center = c(0, base_y - size * 0.2)
			circle_radius = size * 0.35
			draw_circle(
				circle_center,
				circle_radius,
				fill = color2,
				border = border_color,
				offset = offset
			)
			text_center = transform_points(circle_center, offset)
			graphics::text(
				text_center[1, 1],
				text_center[1, 2],
				"N",
				cex = text_size,
				col = text_color,
				font = 2
			)
			return(invisible())
		}
	}
	# drawing polygons
	tempoverlay = tempfile(fileext = ".png")
	png_device(
		filename = tempoverlay,
		width = width,
		height = height,
		units = "px",
		bg = "transparent"
	)
	graphics::par(mar = c(0, 0, 0, 0))
	graphics::plot(
		x = c(0, 1),
		y = c(0, 1),
		xlim = c(0, 1),
		ylim = c(0, 1),
		asp = 1,
		pch = 0,
		bty = "n",
		axes = FALSE,
		xaxs = "i",
		yaxs = "i",
		cex = 0,
		col = NA
	)
	draw_compass(c(0, 0))
	grDevices::dev.off() #resets par
	overlay_temp = rayimage::ray_read_image(tempoverlay)
	if (!is.na(halo_color)) {
		tempoverlay = tempfile(fileext = ".png")
		png_device(
			filename = tempoverlay,
			width = width,
			height = height,
			units = "px",
			bg = "transparent"
		)
		graphics::par(mar = c(0, 0, 0, 0))
		graphics::plot(
			x = c(0, 1),
			y = c(0, 1),
			xlim = c(0, 1),
			ylim = c(0, 1),
			asp = 1,
			pch = 0,
			bty = "n",
			axes = FALSE,
			xaxs = "i",
			yaxs = "i",
			cex = 0,
			col = NA
		)
		draw_compass(halo_offset)
		grDevices::dev.off() #resets par
		overlay_temp_under = rayimage::ray_read_image(tempoverlay)
		overlay_temp_under = generate_halo_underlay(
			overlay_temp_under,
			halo_expand,
			halo_offset,
			halo_color,
			halo_alpha,
			halo_blur,
			halo_edge_softness
		)
		overlay_temp = rayimage::render_image_overlay(
			overlay_temp_under,
			overlay_temp
		)
	}
	return(overlay_temp)
}
