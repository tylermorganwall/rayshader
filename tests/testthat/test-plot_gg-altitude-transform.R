library(ggplot2)

test_that("ggplot scenes transform mapped overlay altitudes into scene units", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	p = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg))

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 2,
		raytrace = FALSE,
		windowsize = c(300, 300)
	)))

	gg_extent = rayshader:::get_ggplot_extent()
	scene_heightmap = get_scene_heightmap()
	scene_zscale = get_scene_effective_zscale()
	altitude_vals = c(min(mtcars$disp), max(mtcars$disp))
	scene_height_range = range(scene_heightmap[is.finite(scene_heightmap)])

	xyz = transform_into_heightmap_coords(
		extent = gg_extent,
		heightmap = scene_heightmap,
		lat = c(min(mtcars$mpg), max(mtcars$mpg)),
		long = c(min(mtcars$wt), max(mtcars$wt)),
		altitude = altitude_vals,
		offset = 0,
		zscale = scene_zscale
	)

	expected_y = scales::rescale(
		altitude_vals,
		to = scene_height_range,
		from = range(altitude_vals)
	) / scene_zscale

	expect_equal(xyz[, 2], expected_y, tolerance = 1e-6)
})

test_that("ggplot scenes without mapped height keep raw overlay altitudes", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	p = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg))

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 2,
		raytrace = FALSE,
		windowsize = c(300, 300)
	)))

	gg_extent = rayshader:::get_ggplot_extent()
	scene_zscale = get_scene_effective_zscale()
	altitude_vals = c(100, 200)

	xyz = transform_into_heightmap_coords(
		extent = gg_extent,
		heightmap = get_scene_heightmap(),
		lat = c(15, 30),
		long = c(2, 4),
		altitude = altitude_vals,
		offset = 0,
		zscale = scene_zscale
	)

	expect_equal(xyz[, 2], altitude_vals / scene_zscale, tolerance = 1e-6)
})

test_that("ggplot z-axis breaks use mapped height positions but keep raw labels", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	p = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg)) +
		labs(color = "Miles per gallon")

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 2,
		raytrace = FALSE,
		windowsize = c(300, 300)
	)))

	gg_extent = rayshader:::get_ggplot_extent()
	scene_heightmap = get_scene_heightmap()
	scene_zscale = get_scene_effective_zscale()
	altitude_vals = c(min(mtcars$disp), max(mtcars$disp))
	breaks = altitude_vals + c(10, -10)
	labels = c("low", "high")
	scene_height_range = range(scene_heightmap[is.finite(scene_heightmap)])

	expect_no_condition(render_points(
		x = c(min(mtcars$wt), max(mtcars$wt)),
		y = c(min(mtcars$mpg), max(mtcars$mpg)),
		extent = gg_extent,
		altitude = altitude_vals,
		color = "red",
		size = 4,
		clear_previous = TRUE,
		zaxis = TRUE,
		zaxis_breaks = breaks,
		zaxis_labels = labels
	))

	ids = get_ids_with_labels()
	tick_id = ids$id[ids$tag == "zaxis_ticks"][1]
	tick_verts = rgl::rgl.attrib(tick_id, "vertices")
	label_ids = ids$id[ids$tag == "zaxis_labels"]
	label_texts = unlist(lapply(
		label_ids,
		function(id) trimws(as.character(rgl::rgl.attrib(id, "texts")))
	))

	expected_y = sort(scales::rescale(
		breaks,
		to = scene_height_range,
		from = range(altitude_vals)
	) / scene_zscale)

	expect_equal(sort(tick_verts[, 2]), expected_y, tolerance = 1e-6)
	expect_true(all(labels %in% label_texts))
})

test_that("standalone ggplot z-axis defaults use mapped height scale labels", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	p = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg)) +
		labs(color = "Miles per gallon")

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 2,
		raytrace = FALSE,
		windowsize = c(300, 300)
	)))

	gg_extent = get_ggplot_extent()
	scene_heightmap = get_scene_heightmap()
	scene_zscale = get_scene_effective_zscale()
	height_transform = get_scene_height_transform(
		heightmap = scene_heightmap,
		extent = gg_extent
	)
	raw_range = range(as.numeric(height_transform$height_range))
	raw_breaks = pretty(raw_range, n = 4)
	raw_breaks = raw_breaks[is.finite(raw_breaks)]
	scene_breaks = map_scene_altitudes(
		raw_breaks,
		height_transform = height_transform,
		reference_values = raw_range
	)
	draw_idx = abs(scene_breaks) > .Machine$double.eps^0.5
	expected_y = sort(scene_breaks[draw_idx] / scene_zscale)
	expected_labels = format(
		raw_breaks,
		trim = TRUE,
		scientific = FALSE
	)[draw_idx]

	expect_no_condition(render_zaxis(zaxis_location = "panel_bottomleft"))

	ids = get_ids_with_labels()
	tick_id = ids$id[ids$tag == "zaxis_ticks"][1]
	tick_verts = rgl::rgl.attrib(tick_id, "vertices")
	label_ids = ids$id[ids$tag == "zaxis_labels"]
	label_texts = unlist(lapply(
		label_ids,
		function(id) trimws(as.character(rgl::rgl.attrib(id, "texts")))
	))
	title_id = ids$id[ids$tag == "zaxis_title"][1]
	title_text = trimws(as.character(rgl::rgl.attrib(title_id, "texts")))

	expect_equal(sort(tick_verts[, 2]), expected_y, tolerance = 1e-6)
	expect_true(all(expected_labels %in% label_texts))
	expect_gt(max(suppressWarnings(as.numeric(label_texts)), na.rm = TRUE), 1)
	expect_equal(title_text, "Miles per gallon")
})

test_that("standalone ggplot z-axis auto-title uses implicit mapped height label", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	p = ggplot(mtcars) +
		geom_point(aes(x = mpg, y = disp, color = cyl)) +
		scale_color_continuous(limits = c(0, 8))

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 2,
		raytrace = FALSE,
		windowsize = c(300, 300)
	)))

	transform_info = get_cached_plot_gg_transform_info(default = NULL)
	expect_equal(transform_info$height_label, "cyl")

	expect_no_condition(render_zaxis(zaxis_location = "panel_bottomleft"))

	ids = get_ids_with_labels()
	title_id = ids$id[ids$tag == "zaxis_title"][1]
	title_text = trimws(as.character(rgl::rgl.attrib(title_id, "texts")))

	expect_equal(title_text, "cyl")
})

test_that("plot_3d scenes keep raw altitude values in transform_into_heightmap_coords", {
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
	altitude_vals = c(100, 200)
	xyz = transform_into_heightmap_coords(
		extent = extent,
		heightmap = heightmap,
		lat = c(10, 20),
		long = c(10, 20),
		altitude = altitude_vals,
		offset = 0,
		zscale = 10
	)

	expect_equal(xyz[, 2], altitude_vals / 10, tolerance = 1e-6)
})

test_that("transform_into_heightmap_coords() does not warn about derived altitude when altitude is explicit", {
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
	expect_warning(
		transform_into_heightmap_coords(
			extent = extent,
			heightmap = heightmap,
			lat = -100,
			long = -100,
			offset = 0,
			zscale = 10,
			filter_bounds = TRUE
		),
		"altitude of those points"
	)
	expect_no_warning(transform_into_heightmap_coords(
		extent = extent,
		heightmap = heightmap,
		lat = -100,
		long = -100,
		altitude = 1000,
		offset = 0,
		zscale = 10,
		filter_bounds = TRUE
	))
})
