test_that("render_points() uses cached scene heightmap and zscale", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)

	heightmap = matrix(0, nrow = 20, ncol = 20)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	expect_no_condition(render_points(
		lat = 10,
		long = 10,
		extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
		offset = 10
	))

	ids = get_ids_with_labels()
	point_id = ids$id[ids$tag == "points3d"][1]
	point_verts = rgl::rgl.attrib(point_id, "vertices")
	expect_equal(unname(point_verts[1, 2]), 1, tolerance = 1e-6)
})

test_that("render_water() uses cached scene heightmap and zscale", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)

	heightmap = matrix(0, nrow = 20, ncol = 20)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	expect_no_condition(render_water(
		waterdepth = 100,
		watercolor = "lightblue"
	))

	ids = get_ids_with_labels()
	water_id = ids$id[ids$tag == "water"][1]
	water_verts = rgl::rgl.attrib(water_id, "vertices")
	expect_equal(unname(range(water_verts[, 2])), c(10, 10), tolerance = 1e-6)
})

test_that("render_contours() uses cached scene heightmap", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)
	skip_if_not_installed("sf")
	skip_if_not_installed("isoband")

	heightmap = outer(1:30, 1:30, `+`)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	expect_no_condition(render_contours(nlevels = 5))
	ids = get_ids_with_labels(typeval = "contour3d")
	expect_gt(nrow(ids), 0)
})

test_that("render_label() uses cached scene heightmap and zscale", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)

	heightmap = matrix(0, nrow = 20, ncol = 20)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	expect_no_condition(render_label(
		text = "A",
		x = 10,
		y = 10,
		z = 10
	))

	ids = get_ids_with_labels(typeval = c("raytext", "textline"))
	expect_gt(nrow(ids), 0)
})

test_that("cached scene messages include cached symbol labels", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)

	elmat = matrix(0, nrow = 20, ncol = 20)
	zs = 10
	texture = sphere_shade(elmat)
	expect_no_condition(plot_3d(
		texture,
		elmat,
		zscale = zs,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	out = capture.output(
		render_points(
			lat = 10,
			long = 10,
			extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
			offset = 10
		)
	)
	expect_true(any(grepl("scene_heightmap", out, fixed = TRUE)))
	expect_true(any(grepl("elmat", out, fixed = TRUE)))
	expect_true(any(grepl("scene_zscale", out, fixed = TRUE)))
	expect_true(any(grepl("zs", out, fixed = TRUE)))
})
