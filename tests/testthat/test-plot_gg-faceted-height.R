library(ggplot2)

test_that("plot_gg() preserves height data in faceted panels", {
	local_rgl_use_null()
	withr::defer(rgl::close3d())

	plot_data = expand.grid(
		x = seq_len(8),
		y = seq_len(8),
		facet = c("a", "b")
	)
	plot_data$value = with(plot_data, ifelse(facet == "a", x + y, x + 2 * y))

	faceted_plot = ggplot(plot_data, aes(x, y, fill = value)) +
		geom_raster() +
		facet_wrap(~facet) +
		scale_x_continuous(expand = c(0, 0)) +
		scale_y_continuous(expand = c(0, 0)) +
		scale_fill_gradient(low = "white", high = "black") +
		coord_fixed()

	height_matrix = suppressWarnings(plot_gg_test(
		faceted_plot,
		width = 4,
		height = 2,
		windowsize = c(400, 250),
		shadow = FALSE,
		solid = FALSE,
		save_height_matrix = TRUE
	))
	panel_info = attr(height_matrix, "ggplot_panel_info", exact = TRUE)

	panel_ranges = vapply(
		seq_len(nrow(panel_info)),
		function(i) {
			panel_rows = floor(panel_info$panel_xmin[i]):ceiling(panel_info$panel_xmax[i])
			panel_cols = floor(panel_info$panel_ymin[i]):ceiling(panel_info$panel_ymax[i])
			panel_rows = pmax(1, pmin(nrow(height_matrix), panel_rows))
			panel_cols = pmax(1, pmin(ncol(height_matrix), panel_cols))
			range(height_matrix[panel_rows, panel_cols], na.rm = TRUE)
		},
		numeric(2)
	)

	expect_true(all(panel_ranges[2, ] > panel_ranges[1, ]))
	expect_true(all(panel_ranges[2, ] > 0.1))
})
