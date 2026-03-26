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

test_that("plot_3d() accepts raster input and caches spatial metadata", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)
	skip_if_not_installed("raster")

	elev_raster = raster::raster(
		nrows = 20,
		ncols = 20,
		xmn = 100,
		xmx = 500,
		ymn = 1000,
		ymx = 1800,
		crs = "+proj=longlat +datum=WGS84 +no_defs"
	)
	raster::values(elev_raster) = seq_len(raster::ncell(elev_raster))
	texture = sphere_shade(raster_to_matrix(elev_raster))

	expect_no_condition(plot_3d(
		texture,
		elev_raster,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	expect_equal(get_scene_zscale(), mean(raster::res(elev_raster)), tolerance = 1e-8)
	expect_equal(
		get_extent(get_scene_extent()),
		c(xmin = 100, xmax = 500, ymin = 1000, ymax = 1800)
	)
	expect_false(is.null(get_scene_crs(default = NULL)))
	expect_true(nzchar(as.character(get_scene_crs())))

	expect_no_condition(render_points(
		lat = 1400,
		long = 200,
		offset = 30,
		size = 1
	))
})

test_that("transform_into_heightmap_coords() can use cached scene extent", {
	on.exit(rgl::close3d(), add = TRUE)
	options(rgl.useNULL = TRUE)
	skip_if_not_installed("raster")

	elev_raster = raster::raster(
		nrows = 10,
		ncols = 10,
		xmn = 0,
		xmx = 100,
		ymn = 0,
		ymx = 200
	)
	raster::values(elev_raster) = seq_len(raster::ncell(elev_raster))
	texture = sphere_shade(raster_to_matrix(elev_raster))

	expect_no_condition(plot_3d(
		texture,
		elev_raster,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	coords = transform_into_heightmap_coords(
		extent = NULL,
		heightmap = NULL,
		lat = 100,
		long = 50,
		altitude = 50,
		zscale = get_scene_zscale()
	)
	expect_equal(dim(coords), c(1, 3))
	expect_true(all(is.finite(coords)))
})
