test_that("render_points() can add a styled z-axis outside a specified corner", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(300, 300)
	))

	extent = c(
		xmin = 0,
		xmax = nrow(heightmap),
		ymin = 0,
		ymax = ncol(heightmap)
	)
	expect_no_condition(render_points(
		lat = c(10, 20),
		long = c(10, 20),
		extent = extent,
		heightmap = heightmap,
		zscale = 10,
		zaxis = TRUE,
		zaxis_location = "bottomleft",
		zaxis_breaks = c(0, 25, 50),
		zaxis_labels = c("0", "25", "50"),
		zaxis_color = "red",
		zaxis_linewidth = 5,
		zaxis_text_offset = 4,
		zaxis_tick_size = 9
	))

	ids = get_ids_with_labels()
	expect_true(any(ids$tag == "zaxis_axis"))
	expect_true(any(ids$tag == "zaxis_ticks"))
	expect_true(any(ids$tag == "zaxis_labels"))
	axis_id = ids$id[ids$tag == "zaxis_axis"][1]
	tick_id = ids$id[ids$tag == "zaxis_ticks"][1]

	axis_material = rgl::material3d(id = axis_id)
	expect_equal(axis_material$color, "#FF0000")
	expect_equal(axis_material$lwd, 5)
	tick_material = rgl::material3d(id = tick_id)
	expect_equal(tick_material$size, 9)
	label_id = ids$id[ids$tag == "zaxis_labels"][1]
	label_adj_left = rgl::rgl.attrib(label_id, "adj")
	expect_equal(unname(label_adj_left[1]), 1)
	label_text_left = rgl::rgl.attrib(label_id, "texts")[1]
	expect_true(grepl("\\s$", label_text_left))

	tick_verts = rgl::rgl.attrib(tick_id, "vertices")
	axis_verts = rgl::rgl.attrib(axis_id, "vertices")
	expect_gte(nrow(tick_verts), 2)
	expect_equal(unname(tick_verts[1, 1]), unname(axis_verts[1, 1]), tolerance = 1e-6)
	expect_equal(unname(tick_verts[1, 3]), unname(axis_verts[1, 3]), tolerance = 1e-6)
	expect_true(all(abs(tick_verts[, 2] - axis_verts[1, 2]) > 1e-6))

	expect_no_condition(render_points(
		extent = extent,
		heightmap = heightmap,
		zscale = 10,
		clear_previous = TRUE,
		zaxis = TRUE,
		zaxis_location = "bottomright",
		zaxis_breaks = c(0, 25, 50),
		zaxis_labels = c("0", "25", "50")
	))
	ids = get_ids_with_labels()
	label_id_right = ids$id[ids$tag == "zaxis_labels"][1]
	label_adj_right = rgl::rgl.attrib(label_id_right, "adj")
	expect_equal(unname(label_adj_right[1]), 0)
	label_text_right = rgl::rgl.attrib(label_id_right, "texts")[1]
	expect_true(grepl("^\\s", label_text_right))
})

test_that("ggplot z-axis defaults to panel placement", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)
	skip_if_not_installed("ggplot2")

	p = ggplot2::ggplot(mtcars) +
		ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg))
	expect_no_condition(suppressWarnings(plot_gg(p, windowsize = c(300, 300))))

	pts = transform_ggplot_coords(x = c(2, 4), y = c(15, 30))
	ext = attr(pts, "extent")
	panel_info = attr(ext, "panel_info")
	expect_true(is.data.frame(panel_info))
	expect_equal(nrow(panel_info), 1)

	expect_no_condition(render_points(
		long = pts$long,
		lat = pts$lat,
		extent = ext,
		altitude = 100,
		zaxis = TRUE,
		zaxis_breaks = c(0, 50, 100)
	))

	ids = get_ids_with_labels()
	axis_rows = ids$tag == "zaxis_axis"
	expect_true(any(axis_rows))
	axis_id = ids$id[which(axis_rows)[1]]
	axis_verts = rgl::rgl.attrib(axis_id, "vertices")

	anchor_long = panel_info$data_xmin
	anchor_lat = panel_info$data_ymin
	expected_anchor = transform_into_heightmap_coords(
		extent = get_extent(ext),
		heightmap = matrix(0, nrow = 2, ncol = 2),
		lat = anchor_lat,
		long = anchor_long,
		altitude = 0,
		use_altitude = FALSE,
		zscale = 1
	)[1, ]

	expect_equal(unname(axis_verts[1, 1]), unname(expected_anchor[1]), tolerance = 1e-6)
	expect_equal(unname(axis_verts[1, 3]), unname(expected_anchor[3]), tolerance = 1e-6)
	if (all(c("extent_xmin", "extent_ymin") %in% names(panel_info))) {
		extent_anchor = transform_into_heightmap_coords(
			extent = get_extent(ext),
			heightmap = matrix(0, nrow = 2, ncol = 2),
			lat = panel_info$extent_ymin,
			long = panel_info$extent_xmin,
			altitude = 0,
			use_altitude = FALSE,
			zscale = 1
		)[1, ]
		expect_gt(abs(unname(axis_verts[1, 1] - extent_anchor[1])) +
			abs(unname(axis_verts[1, 3] - extent_anchor[3])), 1e-6)
	}
})

test_that("ggplot z-axis supports explicit panel corners", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)
	skip_if_not_installed("ggplot2")

	p = ggplot2::ggplot(mtcars) +
		ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg))
	expect_no_condition(suppressWarnings(plot_gg(p, windowsize = c(300, 300))))

	pts = transform_ggplot_coords(x = c(2, 4), y = c(15, 30))
	ext = attr(pts, "extent")
	panel_info = attr(ext, "panel_info")
	expect_true(is.data.frame(panel_info))
	expect_equal(nrow(panel_info), 1)

	expect_no_condition(render_points(
		long = pts$long,
		lat = pts$lat,
		extent = ext,
		altitude = 100,
		zaxis = TRUE,
		zaxis_location = "paneltopright",
		zaxis_breaks = c(0, 50, 100)
	))

	ids = get_ids_with_labels()
	axis_id = ids$id[ids$tag == "zaxis_axis"][1]
	axis_verts = rgl::rgl.attrib(axis_id, "vertices")

	expected_anchor = transform_into_heightmap_coords(
		extent = get_extent(ext),
		heightmap = matrix(0, nrow = 2, ncol = 2),
		lat = panel_info$data_ymax,
		long = panel_info$data_xmax,
		altitude = 0,
		use_altitude = FALSE,
		zscale = 1
	)[1, ]

	expect_equal(unname(axis_verts[1, 1]), unname(expected_anchor[1]), tolerance = 1e-6)
	expect_equal(unname(axis_verts[1, 3]), unname(expected_anchor[3]), tolerance = 1e-6)
})

test_that("render_points() validates z-axis labels length", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(300, 300)
	))

	extent = c(
		xmin = 0,
		xmax = nrow(heightmap),
		ymin = 0,
		ymax = ncol(heightmap)
	)
	expect_error(
		render_points(
			lat = c(10, 20),
			long = c(10, 20),
			extent = extent,
			heightmap = heightmap,
			zscale = 10,
			zaxis = TRUE,
			zaxis_breaks = c(0, 25, 50),
			zaxis_labels = c("0", "25")
		),
		"`zaxis_labels` must be the same length as `zaxis_breaks`."
	)
})

test_that("render_contours() forwards z-axis options", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(300, 300)
	))

	expect_no_condition(render_contours(
		heightmap = heightmap,
		zscale = 10,
		nlevels = 5,
		zaxis = TRUE,
		zaxis_location = "topright",
		zaxis_breaks = c(0, 50, 100),
		zaxis_labels = c("sea", "mid", "high")
	))

	ids = get_ids_with_labels()
	expect_true(any(ids$tag == "zaxis_axis"))
	expect_true(any(ids$tag == "zaxis_ticks"))
	expect_true(any(ids$tag == "zaxis_labels"))
})
