library(ggplot2)

test_that("ggplot scenes transform mapped overlay altitudes into scene units", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)

	p = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg))

	expect_no_condition(suppressWarnings(plot_gg(
		p,
		width = 2,
		raytrace = FALSE,
		windowsize = c(300, 300)
	)))

	gg_extent = get_ggplot_extent()
	scene_heightmap = get_scene_heightmap()
	scene_zscale = get_scene_zscale()
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
	options(rgl.useNULL = TRUE)

	p = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg))

	expect_no_condition(suppressWarnings(plot_gg(
		p,
		width = 2,
		raytrace = FALSE,
		windowsize = c(300, 300)
	)))

	gg_extent = get_ggplot_extent()
	scene_zscale = get_scene_zscale()
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
	options(rgl.useNULL = TRUE)

	p = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg))

	expect_no_condition(suppressWarnings(plot_gg(
		p,
		width = 2,
		raytrace = FALSE,
		windowsize = c(300, 300)
	)))

	gg_extent = get_ggplot_extent()
	scene_heightmap = get_scene_heightmap()
	scene_zscale = get_scene_zscale()
	altitude_vals = c(min(mtcars$disp), max(mtcars$disp))
	breaks = altitude_vals + c(10, -10)
	labels = c("low", "high")
	scene_height_range = range(scene_heightmap[is.finite(scene_heightmap)])

	expect_no_condition(render_points(
		long = c(min(mtcars$wt), max(mtcars$wt)),
		lat = c(min(mtcars$mpg), max(mtcars$mpg)),
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

test_that("plot_3d scenes keep raw altitude values in transform_into_heightmap_coords", {
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
