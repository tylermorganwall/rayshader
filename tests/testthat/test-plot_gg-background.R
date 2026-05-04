library(ggplot2)

plot_gg_background_regression_plot = function() {
	grid_data = expand.grid(x = seq_len(3), y = seq_len(9))
	grid_data$z = grid_data$y
	ggplot(grid_data) +
		geom_tile(aes(x = x, y = y, fill = z)) +
		scale_x_continuous(expand = c(0, 0)) +
		scale_y_continuous(expand = c(0, 0)) +
		scale_fill_gradient(low = "green", high = "green") +
		coord_fixed() +
		theme_void() +
		theme(
			legend.position = "none",
			plot.background = element_rect(fill = "red", color = "white", linewidth = 4),
			panel.background = element_rect(fill = "red", color = NA),
			panel.border = element_blank()
		)
}

local_discard_plot_output = function(env = parent.frame()) {
	grDevices::pdf(NULL)
	withr::defer(grDevices::dev.off(), envir = env)
	invisible(NULL)
}

test_that("plot_gg() fills fixed-aspect texture margins with the plot background", {
	local_discard_plot_output()
	texture = suppressWarnings(plot_gg_test(
		plot_gg_background_regression_plot(),
		width = 4,
		height = 2,
		raytrace = FALSE,
		shadow = FALSE,
		preview = TRUE,
		plot = FALSE
	))

	left_edge = sapply(seq_len(3), function(channel) mean(texture[, 1, channel]))
	right_edge = sapply(seq_len(3), function(channel) {
		mean(texture[, dim(texture)[2], channel])
	})

	expect_equal(left_edge, c(1, 0, 0), tolerance = 0.01)
	expect_equal(right_edge, c(1, 0, 0), tolerance = 0.01)

	white_pixels = texture[, , 1] > 0.95 &
		texture[, , 2] > 0.95 &
		texture[, , 3] > 0.95
	expect_false(any(white_pixels))
})

test_that("plot_gg() keeps the scene background independent of the plot background", {
	local_rgl_use_null()
	local_discard_plot_output()
	withr::defer(rgl::close3d())

	expect_no_condition(suppressWarnings(plot_gg_test(
		plot_gg_background_regression_plot(),
		width = 2,
		height = 2,
		raytrace = FALSE,
		shadow = FALSE,
		vertical_exaggeration = 1,
		windowsize = c(100, 100),
		zoom = 0.5,
		solid = FALSE
	)))
	background_id = tail(rgl::ids3d("background")$id, 1)
	background_color = rgl::rgl.attrib(background_id, "colors")
	expect_equal(unname(background_color[1, 1:3]), c(1, 1, 1), tolerance = 0.01)

	expect_no_condition(suppressWarnings(plot_gg_test(
		plot_gg_background_regression_plot(),
		width = 2,
		height = 2,
		raytrace = FALSE,
		shadow = FALSE,
		background = "blue",
		vertical_exaggeration = 1,
		windowsize = c(100, 100),
		zoom = 0.5,
		solid = FALSE
	)))
	background_id = tail(rgl::ids3d("background")$id, 1)
	background_color = rgl::rgl.attrib(background_id, "colors")
	expect_equal(unname(background_color[1, 1:3]), c(0, 0, 1), tolerance = 0.01)
})
