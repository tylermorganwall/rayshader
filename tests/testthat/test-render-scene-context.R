test_that("render_points() uses cached scene heightmap and zscale", {
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

	expect_no_condition(render_points(
		y = 10,
		x = 10,
		extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
		offset = 10
	))

	ids = get_ids_with_labels()
	point_id = ids$id[ids$tag == "points3d"][1]
	point_verts = rgl::rgl.attrib(point_id, "vertices")
	expect_equal(unname(point_verts[1, 2]), 1, tolerance = 1e-6)
})

test_that("scene cache is rejected after switching to a different open scene", {
	local_rgl_use_null()
	withr::defer({
		while (rgl::cur3d() != 0) {
			rgl::close3d()
		}
	})

	heightmap1 = matrix(0, nrow = 20, ncol = 20)
	expect_no_condition(plot_3d_test(
		sphere_shade(heightmap1),
		heightmap1,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250),
		close_previous = TRUE
	))
	scene1 = rgl::cur3d()

	heightmap2 = matrix(1, nrow = 12, ncol = 12)
	expect_no_condition(plot_3d_test(
		sphere_shade(heightmap2),
		heightmap2,
		zscale = 5,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250),
		close_previous = FALSE
	))
	scene2 = rgl::cur3d()

	expect_false(identical(scene1, scene2))
	expect_equal(get_scene_context_token(default = NULL), unname(scene2))

	rgl::set3d(scene1)
	expect_null(get_scene_heightmap(default = NULL))
	expect_null(get_scene_zscale(default = NULL))
	expect_error(
		render_water(waterdepth = 1),
		"No heightmap found"
	)

	rgl::set3d(scene2)
	expect_equal(get_scene_heightmap(default = NULL), heightmap2)
	expect_equal(get_scene_zscale(default = NULL), 5)
	expect_no_condition(render_water(waterdepth = 2, watercolor = "lightblue"))
})

test_that("render_water() uses cached scene heightmap and zscale", {
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
	local_rgl_use_null()
	skip_if_not_installed("sf")
	skip_if_not_installed("isoband")

	heightmap = outer(1:30, 1:30, `+`)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
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

	expect_no_condition(render_label(
		text = "A",
		x = 10,
		y = 10,
		z = 10
	))

	ids = get_ids_with_labels(typeval = c("raytext", "textline"))
	expect_gt(nrow(ids), 0)
})

test_that("render_label() accepts x/y names and lat/long aliases", {
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

	expect_no_condition(render_label(
		text = "A",
		y = 10,
		x = 10,
		z = 10,
		extent = extent,
		clear_previous = TRUE
	))
	expect_no_condition(render_label(
		text = "A",
		lat = 10,
		long = 10,
		z = 10,
		extent = extent,
		clear_previous = TRUE
	))
	expect_error(
		render_label(
			text = "A",
			y = 10,
			x = 10,
			lat = 10,
			z = 10,
			extent = extent
		),
		"Use only one of `y` or `lat`"
	)
	expect_error(
		render_label(
			text = "A",
			y = 10,
			x = 10,
			long = 10,
			z = 10,
			extent = extent
		),
		"Use only one of `x` or `long`"
	)
})

test_that("render_label() matrix fallback extent preserves 1-based indexing", {
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

	expect_no_condition(render_label(
		text = "A",
		x = 1,
		y = 1,
		z = 10,
		clear_previous = TRUE
	))

	ids = get_ids_with_labels(typeval = "textline")
	line_id = ids$id[ids$tag == "textline"][1]
	line_verts = rgl::rgl.attrib(line_id, "vertices")
	expect_equal(unname(line_verts[1, 1]), -(nrow(heightmap) - 1) / 2, tolerance = 1e-6)
	expect_equal(unname(line_verts[1, 3]), (ncol(heightmap) - 1) / 2, tolerance = 1e-6)
})

test_that("render_points() accepts x/y names and lat/long aliases", {
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

	expect_no_condition(render_points(
		y = 10,
		x = 10,
		extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
		offset = 10,
		clear_previous = TRUE
	))
	expect_no_condition(render_points(
		lat = 10,
		long = 10,
		extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
		offset = 10,
		clear_previous = TRUE
	))
	expect_error(
		render_points(
			y = 10,
			x = 10,
			lat = 10,
			extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
		),
		"Use only one of `y` or `lat`"
	)
	expect_error(
		render_points(
			y = 10,
			x = 10,
			long = 10,
			extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
		),
		"Use only one of `x` or `long`"
	)
})

test_that("render_obj() and render_tree() accept x/y coordinates", {
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

	expect_no_condition(render_obj(
		flag_pole_obj(),
		y = 10,
		x = 10,
		extent = extent,
		heightmap = heightmap,
		scale = c(1, 1, 1),
		clear_previous = TRUE
	))
	expect_true(any(get_ids_with_labels()$tag == "obj"))

	expect_no_condition(render_tree(
		y = 12,
		x = 12,
		extent = extent,
		heightmap = heightmap,
		tree_height = 5,
		clear_previous = TRUE
	))
	expect_true(any(get_ids_with_labels()$tag == "objtree"))
})

test_that("render_path() and render_raymesh() accept x/y coordinates", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	skip_if_not_installed("rayvertex")

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

	expect_no_condition(render_path(
		y = c(5, 15),
		x = c(5, 15),
		extent = extent,
		heightmap = heightmap,
		clear_previous = TRUE
	))
	expect_true(any(get_ids_with_labels()$tag == "path3d"))

	expect_no_condition(render_path(
		lat = c(6, 16),
		long = c(6, 16),
		extent = extent,
		heightmap = heightmap,
		clear_previous = TRUE
	))
	expect_error(
		render_path(
			y = c(5, 15),
			x = c(5, 15),
			lat = c(5, 15),
			extent = extent,
			heightmap = heightmap
		),
		"Use only one of `y` or `lat`"
	)

	mesh = rayvertex::sphere_mesh(radius = 1)
	expect_no_condition(render_raymesh(
		mesh,
		y = 10,
		x = 10,
		extent = extent,
		heightmap = heightmap,
		clear_previous = TRUE
	))
	expect_true(any(get_ids_with_labels()$tag == "obj_raymesh"))

	expect_no_condition(render_raymesh(
		mesh,
		lat = 12,
		long = 12,
		extent = extent,
		heightmap = heightmap,
		clear_previous = TRUE
	))
	expect_error(
		render_raymesh(
			mesh,
			y = 10,
			x = 10,
			long = 10,
			extent = extent,
			heightmap = heightmap
		),
		"Use only one of `x` or `long`"
	)
})

test_that("cached scene messages include cached symbol labels", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	withr::local_options(list(rayshader.verbose_scene_cache = TRUE))

	elmat = matrix(0, nrow = 20, ncol = 20)
	zs = 10
	texture = sphere_shade(elmat)
	expect_no_condition(plot_3d_test(
		texture,
		elmat,
		zscale = zs,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	out = character()
	expect_no_error(withCallingHandlers(
		render_points(
			y = 10,
			x = 10,
			extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
			offset = 10
		),
		message = function(cnd) {
			out <<- c(out, conditionMessage(cnd))
			invokeRestart("muffleMessage")
		}
	))
	expect_true(any(grepl("scene_heightmap", out, fixed = TRUE)))
	expect_true(any(grepl("elmat", out, fixed = TRUE)))
	expect_true(any(grepl("scene_zscale", out, fixed = TRUE)))
	expect_true(any(grepl("zs", out, fixed = TRUE)))
})

test_that("plot_3d() accepts raster input and caches spatial metadata", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
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

	expect_no_condition(plot_3d_test(
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
		y = 1400,
		x = 200,
		offset = 30,
		size = 1
	))
})

test_that("transform_into_heightmap_coords() can use cached scene extent", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
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

	expect_no_condition(plot_3d_test(
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
