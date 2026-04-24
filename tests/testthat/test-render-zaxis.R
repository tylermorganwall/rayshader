test_that("render_points() can add a styled z-axis outside a specified corner", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
		y = c(10, 20),
		x = c(10, 20),
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
	label_texts_left = trimws(as.character(rgl::rgl.attrib(label_id, "texts")))
	label_text_left = rgl::rgl.attrib(label_id, "texts")[1]
	expect_true(grepl("\\s$", label_text_left))
	expect_true(any(label_texts_left == "0"))

	tick_verts = rgl::rgl.attrib(tick_id, "vertices")
	axis_verts = rgl::rgl.attrib(axis_id, "vertices")
	label_verts_left = rgl::rgl.attrib(label_id, "vertices")
	expect_gt(abs(unname(label_verts_left[1, 1] - axis_verts[1, 1])), 1e-6)
	expect_gt(abs(unname(label_verts_left[1, 3] - axis_verts[1, 3])), 1e-6)
	expect_gte(nrow(tick_verts), 2)
	expect_equal(unname(tick_verts[1, 1]), unname(axis_verts[1, 1]), tolerance = 1e-6)
	expect_equal(unname(tick_verts[1, 3]), unname(axis_verts[1, 3]), tolerance = 1e-6)
	expect_true(any(abs(tick_verts[, 2] - axis_verts[1, 2]) <= 1e-6))

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

test_that("render_zaxis() accepts fractional text offsets", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
	expect_no_condition(render_zaxis(
		extent = extent,
		zscale = 10,
		heightmap = heightmap,
		zaxis_location = "bottomleft",
		zaxis_breaks = c(0, 25, 50),
		zaxis_text_offset = 0.25
	))
})

test_that("render_zaxis() works as a standalone entry point", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
	expect_no_condition(render_zaxis(
		extent = extent,
		zscale = 10,
		heightmap = heightmap,
		zaxis_breaks = c(0, 50, 100),
		zaxis_tick_size = 5
	))

	ids = get_ids_with_labels()
	expect_true(any(ids$tag == "zaxis_axis"))
	expect_true(any(ids$tag == "zaxis_ticks"))
	expect_true(any(ids$tag == "zaxis_labels"))
})

test_that("render_zaxis() infers ggplot panel extent when omitted", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	skip_if_not_installed("ggplot2")

	p = ggplot2::ggplot(mtcars) +
		ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg))
	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		windowsize = c(300, 300),
		raytrace = FALSE,
		multicore = FALSE
	)))

	expect_no_condition(render_zaxis(
		zaxis_location = "panel_bottomleft",
		zaxis_breaks = c(0, 50, 100)
	))

	ids = get_ids_with_labels()
	expect_true(any(ids$tag == "zaxis_axis"))
	expect_true(any(ids$tag == "zaxis_ticks"))
	expect_true(any(ids$tag == "zaxis_labels"))
})

test_that("render_zaxis() uses cached coord_sf scene metadata when omitted", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	skip_if_not_installed("ggplot2")
	skip_if_not_installed("sf")

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	p = ggplot2::ggplot(nc) +
		ggplot2::geom_sf() +
		ggplot2::coord_sf(crs = sf::st_crs(32119))
	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 3,
		height = 3,
		windowsize = c(600, 600),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	expect_no_condition(render_zaxis(
		zaxis_breaks = c(0, 50, 100)
	))

	ids = get_ids_with_labels()
	expect_true(any(ids$tag == "zaxis_axis"))
	expect_true(any(ids$tag == "zaxis_ticks"))
	expect_true(any(ids$tag == "zaxis_labels"))
})

test_that("render_zaxis() uses cached user extent when omitted", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
	expect_no_condition(render_zaxis(
		extent = extent,
		zscale = 10,
		heightmap = heightmap,
		zaxis_breaks = c(0, 50, 100)
	))
	expect_no_condition(render_zaxis(
		zaxis_breaks = c(0, 50, 100)
	))
})

test_that("render_zaxis() uses extent cached by plot_3d()", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	extent = c(
		xmin = 0,
		xmax = nrow(heightmap),
		ymin = 0,
		ymax = ncol(heightmap)
	)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		extent = extent,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(300, 300)
	))

	expect_no_condition(render_zaxis(
		zaxis_breaks = c(0, 50, 100)
	))
})

test_that("render_zaxis() errors without extent metadata on plain terrain matrices", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(300, 300)
	))

	expect_error(
		render_zaxis(zaxis_breaks = c(0, 50, 100)),
		"Could not determine `extent`"
	)
})

test_that("render_zaxis() does not infer extent from heightmap attributes", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	attr(heightmap, "extent") = c(
		xmin = 0,
		xmax = nrow(heightmap),
		ymin = 0,
		ymax = ncol(heightmap)
	)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(300, 300)
	))

	expect_error(
		render_zaxis(zaxis_breaks = c(0, 50, 100)),
		"Could not determine `extent`"
	)
})

test_that("plot_3d() and plot_gg() return invisibly by default", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	vis_3d = withVisible(plot_3d_test(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(300, 300)
	))
	expect_false(vis_3d$visible)

	skip_if_not_installed("ggplot2")
	p = ggplot2::ggplot(mtcars) +
		ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg))
	vis_gg = withVisible(suppressWarnings(plot_gg_test(
		p,
		windowsize = c(300, 300),
		raytrace = FALSE,
		multicore = FALSE
	)))
	expect_false(vis_gg$visible)
})

test_that("render_zaxis() infers cached terrain zscale for default breaks", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
	expect_no_condition(render_zaxis(
		extent = extent,
		zaxis_location = "bottomleft"
	))

	ids = get_ids_with_labels()
	label_ids = ids$id[ids$tag == "zaxis_labels"]
	label_texts = unlist(lapply(label_ids, function(id) {
		trimws(as.character(rgl::rgl.attrib(id, "texts")))
	}))
	label_vals = suppressWarnings(as.numeric(label_texts))
	label_vals = label_vals[is.finite(label_vals)]

	expect_true(length(label_vals) >= 1)
	expect_gte(max(label_vals), 20)
})

test_that("render_zaxis() default breaks span negative and positive terrain", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano - mean(range(volcano))
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
	expect_no_condition(render_zaxis(
		extent = extent,
		zaxis_location = "bottomleft"
	))

	ids = get_ids_with_labels()
	label_ids = ids$id[ids$tag == "zaxis_labels"]
	label_texts = unlist(lapply(label_ids, function(id) {
		trimws(as.character(rgl::rgl.attrib(id, "texts")))
	}))
	label_vals = suppressWarnings(as.numeric(label_texts))
	label_vals = label_vals[is.finite(label_vals)]

	expect_true(any(label_vals < 0))
	expect_true(any(label_vals > 0))
})

test_that("render_zaxis() corner offset is user-configurable", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
	corner_xyz = transform_into_heightmap_coords(
		extent = extent,
		heightmap = matrix(0, nrow = 2, ncol = 2),
		lat = extent["ymin"],
		long = extent["xmin"],
		altitude = 0,
		use_altitude = FALSE,
		zscale = 10
	)[1, ]

	expect_no_condition(render_zaxis(
		extent = extent,
		zscale = 10,
		heightmap = heightmap,
		zaxis_location = "bottomleft",
		zaxis_breaks = c(0, 25, 50),
		zaxis_corner_offset = 0
	))
	ids = get_ids_with_labels()
	axis_id = ids$id[ids$tag == "zaxis_axis"][1]
	axis_verts_near = rgl::rgl.attrib(axis_id, "vertices")
	dist_near = sqrt(
		(axis_verts_near[1, 1] - corner_xyz[1])^2 +
			(axis_verts_near[1, 3] - corner_xyz[3])^2
	)

	expect_no_condition(render_zaxis(
		extent = extent,
		zscale = 10,
		heightmap = heightmap,
		zaxis_location = "bottomleft",
		zaxis_breaks = c(0, 25, 50),
		zaxis_corner_offset = 0.2
	))
	ids = get_ids_with_labels()
	axis_id = ids$id[ids$tag == "zaxis_axis"][1]
	axis_verts_far = rgl::rgl.attrib(axis_id, "vertices")
	dist_far = sqrt(
		(axis_verts_far[1, 1] - corner_xyz[1])^2 +
			(axis_verts_far[1, 3] - corner_xyz[3])^2
	)

	expect_gt(dist_far, dist_near)
})

test_that("ggplot z-axis defaults to panel placement", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	skip_if_not_installed("ggplot2")

	p = ggplot2::ggplot(mtcars) +
		ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg))
	expect_no_condition(suppressWarnings(plot_gg_test(p, windowsize = c(300, 300))))

	ext = rayshader:::get_ggplot_extent()
	panel_info = attr(ext, "panel_info")
	expect_true(is.data.frame(panel_info))
	expect_equal(nrow(panel_info), 1)

	expect_no_condition(render_points(
		x = c(2, 4),
		y = c(15, 30),
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
	expected_center = transform_into_heightmap_coords(
		extent = get_extent(ext),
		heightmap = matrix(0, nrow = 2, ncol = 2),
		lat = (panel_info$data_ymin + panel_info$data_ymax) / 2,
		long = (panel_info$data_xmin + panel_info$data_xmax) / 2,
		altitude = 0,
		use_altitude = FALSE,
		zscale = 1
	)[1, ]
	dist_axis = sqrt(
		(axis_verts[1, 1] - expected_center[1])^2 +
			(axis_verts[1, 3] - expected_center[3])^2
	)
	dist_corner = sqrt(
		(expected_anchor[1] - expected_center[1])^2 +
			(expected_anchor[3] - expected_center[3])^2
	)
	expect_equal(unname(dist_axis), unname(dist_corner), tolerance = 1e-6)
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
	local_rgl_use_null()
	skip_if_not_installed("ggplot2")

	p = ggplot2::ggplot(mtcars) +
		ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg))
	expect_no_condition(suppressWarnings(plot_gg_test(p, windowsize = c(300, 300))))

	ext = rayshader:::get_ggplot_extent()
	panel_info = attr(ext, "panel_info")
	expect_true(is.data.frame(panel_info))
	expect_equal(nrow(panel_info), 1)

	expect_no_condition(render_points(
		x = c(2, 4),
		y = c(15, 30),
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
	expected_center = transform_into_heightmap_coords(
		extent = get_extent(ext),
		heightmap = matrix(0, nrow = 2, ncol = 2),
		lat = (panel_info$data_ymin + panel_info$data_ymax) / 2,
		long = (panel_info$data_xmin + panel_info$data_xmax) / 2,
		altitude = 0,
		use_altitude = FALSE,
		zscale = 1
	)[1, ]
	dist_axis = sqrt(
		(axis_verts[1, 1] - expected_center[1])^2 +
			(axis_verts[1, 3] - expected_center[3])^2
	)
	dist_corner = sqrt(
		(expected_anchor[1] - expected_center[1])^2 +
			(expected_anchor[3] - expected_center[3])^2
	)
	expect_equal(unname(dist_axis), unname(dist_corner), tolerance = 1e-6)
})

test_that("ggplot panel inset omits zero marker", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	skip_if_not_installed("ggplot2")

	p = ggplot2::ggplot(mtcars) +
		ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg))
	expect_no_condition(suppressWarnings(plot_gg_test(p, windowsize = c(300, 300))))

	ext = rayshader:::get_ggplot_extent()
	expect_no_condition(render_points(
		x = c(2, 4),
		y = c(15, 30),
		extent = ext,
		altitude = 100,
		zaxis = TRUE,
		zaxis_location = "panelbottomleft",
		zaxis_breaks = c(0, 50, 100)
	))

	ids = get_ids_with_labels()
	tick_id = ids$id[ids$tag == "zaxis_ticks"][1]
	axis_id = ids$id[ids$tag == "zaxis_axis"][1]
	label_id = ids$id[ids$tag == "zaxis_labels"][1]
	tick_verts = rgl::rgl.attrib(tick_id, "vertices")
	axis_verts = rgl::rgl.attrib(axis_id, "vertices")
	label_texts = trimws(as.character(rgl::rgl.attrib(label_id, "texts")))
	base_y = min(axis_verts[, 2])

	expect_equal(nrow(tick_verts), 2)
	expect_gt(min(tick_verts[, 2]) - base_y, 1e-8)
	expect_false(any(label_texts == "0"))
})

test_that("render_points() validates z-axis labels length", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
			y = c(10, 20),
			x = c(10, 20),
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
	local_rgl_use_null()

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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
