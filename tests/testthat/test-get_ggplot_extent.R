library(ggplot2)

test_that("get_ggplot_extent() maps a single ggplot panel into scene coordinates", {
	on.exit(rgl::close3d(), add = TRUE)

	gg = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg))

	height_matrix = plot_gg(
		gg,
		width = 3,
		height = 3,
		windowsize = c(800, 800),
		raytrace = FALSE,
		shadow = FALSE,
		save_height_matrix = TRUE
	)
	gg_extent = get_ggplot_extent()
	panel_info = attr(gg_extent, "panel_info")

	expect_equal(names(gg_extent), c("xmin", "xmax", "ymin", "ymax"))
	expect_equal(nrow(panel_info), 1)
	expect_equal(as.numeric(attr(height_matrix, "extent")), as.numeric(gg_extent))
	expect_equal(names(attr(height_matrix, "extent")), names(gg_extent))

	nrow_map = nrow(height_matrix) - 1
	ncol_map = ncol(height_matrix) - 1
	x_coords = transform_into_heightmap_coords(
		extent = gg_extent,
		heightmap = height_matrix,
		lat = rep(mean(c(panel_info$data_ymin, panel_info$data_ymax)), 2),
		long = c(panel_info$data_xmin, panel_info$data_xmax),
		altitude = 0,
		use_altitude = TRUE
	)
	y_coords = transform_into_heightmap_coords(
		extent = gg_extent,
		heightmap = height_matrix,
		lat = c(panel_info$data_ymax, panel_info$data_ymin),
		long = rep(mean(c(panel_info$data_xmin, panel_info$data_xmax)), 2),
		altitude = 0,
		use_altitude = TRUE
	)

	expect_equal(
		as.numeric(x_coords[, 1] + nrow_map / 2 + 1),
		c(panel_info$panel_xmin, panel_info$panel_xmax),
		tolerance = 2
	)
	expect_equal(
		as.numeric(y_coords[, 3] + ncol_map / 2 + 1),
		c(panel_info$panel_ymin, panel_info$panel_ymax),
		tolerance = 2
	)
})

test_that("get_ggplot_extent() returns one extent per facet panel", {
	on.exit(rgl::close3d(), add = TRUE)

	gg = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg)) +
		facet_wrap(~cyl)

	expect_no_condition(plot_gg(
		gg,
		width = 5,
		height = 3,
		windowsize = c(1000, 800),
		raytrace = FALSE,
		shadow = FALSE
	))

	all_extents = get_ggplot_extent()
	panel_two_extent = get_ggplot_extent(panel = 2)

	expect_type(all_extents, "list")
	expect_named(all_extents, c("panel_1", "panel_2", "panel_3"))
	expect_equal(length(all_extents), 3)
	expect_equal(panel_two_extent, all_extents[[2]])
	expect_equal(attr(panel_two_extent, "panel_info")$panel, 2L)
})
