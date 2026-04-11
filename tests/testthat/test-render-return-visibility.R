test_that("side-effect render helpers return invisibly", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = matrix(0, nrow = 20, ncol = 20)
	texture = sphere_shade(heightmap)
	extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	camera_vis = withVisible(render_camera(theta = 20))
	expect_false(camera_vis$visible)

	label_vis = withVisible(render_label(
		text = "A",
		x = 10,
		y = 10,
		z = 10,
		extent = extent
	))
	expect_false(label_vis$visible)

	points_vis = withVisible(render_points(
		x = 10,
		y = 10,
		offset = 5,
		extent = extent
	))
	expect_false(points_vis$visible)

	compass_vis = withVisible(render_compass())
	expect_false(compass_vis$visible)

	skip_if_not_installed("sf")
	skip_if_not_installed("rayrender")
	polygon = sf::st_sf(geometry = sf::st_sfc(
		sf::st_polygon(list(matrix(
			c(2, 2, 10, 2, 10, 10, 2, 10, 2, 2),
			ncol = 2,
			byrow = TRUE
		)))
	))
	polygon_vis = withVisible(render_polygons(
		polygon,
		extent = extent,
		top = 2,
		bottom = 1,
		parallel = FALSE
	))
	expect_false(polygon_vis$visible)
})

test_that("render_camera returns camera values visibly when queried", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = matrix(0, nrow = 20, ncol = 20)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	camera_vis = withVisible(render_camera())
	expect_true(camera_vis$visible)
	expect_named(camera_vis$value, c("theta", "phi", "zoom", "fov"))
})
