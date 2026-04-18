clear_spatialize_image_test_cache = function() {
	clear_hillshade_cache()
	reset_scene_context(
		clear_scene_metadata = TRUE,
		clear_scene_cache = TRUE
	)
	while (rgl::cur3d() != 0) {
		rgl::close3d()
	}
	invisible(NULL)
}

spatialize_image_extent_values = function(x) {
	c(
		xmin = terra::xmin(x),
		xmax = terra::xmax(x),
		ymin = terra::ymin(x),
		ymax = terra::ymax(x)
	)
}

spatialize_image_corner_values = function(x) {
	points = data.frame(
		x = c(0.5, 1.5, 0.5, 1.5),
		y = c(1.5, 1.5, 0.5, 0.5)
	)
	vals = terra::extract(x, points)
	stats::setNames(vals[, 2], c("top_left", "top_right", "bottom_left", "bottom_right"))
}

test_that("spatialize_image() converts a matrix into a one-layer SpatRaster", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	mat = matrix(1:6, nrow = 2, byrow = TRUE)
	rast = spatialize_image(
		mat,
		extent = c(xmin = 0, xmax = 3, ymin = 10, ymax = 12),
		toRGB = FALSE
	)

	expect_s4_class(rast, "SpatRaster")
	expect_equal(terra::nlyr(rast), 1)
	expect_equal(names(rast), "value")
	expect_equal(spatialize_image_extent_values(rast), c(xmin = 0, xmax = 3, ymin = 10, ymax = 12))
	expect_equal(terra::as.matrix(rast, wide = TRUE), mat)
	expect_error(
		spatialize_image(
			mat,
			extent = c(0, 1, 0, 1),
			toRGB = FALSE,
			layer_names = c("a", "b")
		),
		"`layer_names` must be a character vector of length 1"
	)
})

test_that("spatialize_image() converts RGB and RGBA arrays into SpatRasters", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	rgb = array(seq_len(18), dim = c(2, 3, 3))
	rgba = array(seq_len(24), dim = c(2, 3, 4))

	rgb_rast = spatialize_image(rgb, extent = c(0, 3, 0, 2), toRGB = FALSE)
	rgba_rgb_rast = spatialize_image(rgba, extent = c(0, 3, 0, 2), toRGB = FALSE)
	rgba_full_rast = spatialize_image(
		rgba,
		extent = c(0, 3, 0, 2),
		include_alpha = TRUE,
		toRGB = FALSE
	)

	expect_equal(terra::nlyr(rgb_rast), 3)
	expect_equal(names(rgb_rast), c("red", "green", "blue"))
	expect_equal(terra::nlyr(rgba_rgb_rast), 3)
	expect_equal(names(rgba_rgb_rast), c("red", "green", "blue"))
	expect_equal(terra::nlyr(rgba_full_rast), 4)
	expect_equal(names(rgba_full_rast), c("red", "green", "blue", "alpha"))
	expect_equal(
		terra::as.matrix(rgba_full_rast[[4]], wide = TRUE),
		rgba[, , 4]
	)
})

test_that("spatialize_image() can append an explicit height layer", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	image_mat = matrix(1:6, nrow = 2, byrow = TRUE)
	height_mat = matrix(c(10, 20, 30, 40, 50, 60), nrow = 3, byrow = TRUE)
	rast = spatialize_image(
		image_mat,
		extent = c(0, 3, 0, 2),
		toRGB = FALSE,
		include_height = TRUE,
		heightmap = height_mat
	)

	expect_equal(terra::nlyr(rast), 2)
	expect_equal(names(rast), c("value", "height"))
	expect_equal(terra::as.matrix(rast[[2]], wide = TRUE), t(height_mat))
})

test_that("spatialize_image() can append a cached height layer", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	hillshade_img = sphere_shade(volcano)
	rast = spatialize_image(
		hillshade_img,
		extent = c(0, ncol(hillshade_img), 0, nrow(hillshade_img)),
		toRGB = FALSE,
		include_height = TRUE
	)

	expect_equal(terra::nlyr(rast), 4)
	expect_equal(names(rast), c("red", "green", "blue", "height"))
	expect_equal(terra::as.matrix(rast[[4]], wide = TRUE), t(volcano))
})

test_that("spatialize_image() bilinearly resamples height onto the output grid", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	height_mat = matrix(c(0, 10, 20, 30, 40, 50), nrow = 3, byrow = TRUE)
	rast = spatialize_image(
		matrix(0, nrow = 4, ncol = 6),
		extent = c(0, 6, 0, 4),
		toRGB = FALSE,
		include_height = TRUE,
		heightmap = height_mat
	)
	expected = matrix(c(
		0, 8, 16, 24, 32, 40,
		3.3333333333, 11.3333333333, 19.3333333333, 27.3333333333, 35.3333333333, 43.3333333333,
		6.6666666667, 14.6666666667, 22.6666666667, 30.6666666667, 38.6666666667, 46.6666666667,
		10, 18, 26, 34, 42, 50
	), nrow = 4, byrow = TRUE)

	expect_equal(
		terra::as.matrix(rast[[2]], wide = TRUE),
		expected,
		tolerance = 1e-8
	)
})

test_that("spatialize_image() defaults to plotting-friendly RGB conversion", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	mat = matrix(c(0, 0.5, 0.75, 1), nrow = 2, byrow = TRUE)
	rast = spatialize_image(mat, extent = c(0, 2, 0, 2))
	expected = pmax(pmin(
		rayimage::render_gamma_linear(mat, srgb_to_linear = FALSE) * 255,
		255
	), 0)

	expect_equal(terra::as.matrix(rast, wide = TRUE), expected, tolerance = 1e-8)
})

test_that("spatialize_image() can convert image values to sRGB-scaled 0-255 output", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	rgb = array(c(
		0.25, 0.5,
		0.75, 1.0,
		0.1, 0.2,
		0.3, 0.4,
		0.6, 0.7,
		0.8, 0.9
	), dim = c(2, 2, 3))
	rast = spatialize_image(rgb, extent = c(0, 2, 0, 2), toRGB = TRUE)
	expected = pmax(pmin(
		rayimage::render_gamma_linear(rgb, srgb_to_linear = FALSE) * 255,
		255
	), 0)

	expect_equal(
		terra::as.matrix(rast[[1]], wide = TRUE),
		expected[, , 1],
		tolerance = 1e-8
	)
	expect_equal(
		terra::as.matrix(rast[[2]], wide = TRUE),
		expected[, , 2],
		tolerance = 1e-8
	)
	expect_equal(
		terra::as.matrix(rast[[3]], wide = TRUE),
		expected[, , 3],
		tolerance = 1e-8
	)
})

test_that("spatialize_image() scales alpha without gamma-adjusting it when toRGB is TRUE", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	rgba = array(c(
		0.25, 0.5,
		0.75, 1.0,
		0.1, 0.2,
		0.3, 0.4,
		0.6, 0.7,
		0.8, 0.9,
		0.0, 0.25,
		0.5, 1.0
	), dim = c(2, 2, 4))
	rast = spatialize_image(
		rgba,
		extent = c(0, 2, 0, 2),
		include_alpha = TRUE,
		toRGB = TRUE
	)
	expected_rgb = pmax(pmin(
		rayimage::render_gamma_linear(rgba[, , 1:3, drop = FALSE], srgb_to_linear = FALSE) * 255,
		255
	), 0)
	expected_alpha = rgba[, , 4] * 255

	expect_equal(
		terra::as.matrix(rast[[1]], wide = TRUE),
		expected_rgb[, , 1],
		tolerance = 1e-8
	)
	expect_equal(
		terra::as.matrix(rast[[4]], wide = TRUE),
		expected_alpha,
		tolerance = 1e-8
	)
})

test_that("spatialize_image() derives extent and CRS from sf extent-like objects", {
	skip_if_not_installed("terra")
	skip_if_not_installed("sf")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	sf_extent = sf::st_as_sfc(sf::st_bbox(
		c(xmin = -1, xmax = 1, ymin = 40, ymax = 41),
		crs = sf::st_crs(4326)
	))
	rast = spatialize_image(matrix(1:4, nrow = 2, byrow = TRUE), extent = sf_extent)

	expect_equal(
		spatialize_image_extent_values(rast),
		c(xmin = -1, xmax = 1, ymin = 40, ymax = 41)
	)
	expect_equal(rayshader:::try_parse_scene_crs(terra::crs(rast))$epsg, 4326)
})

test_that("spatialize_image() lets explicit CRS override inferred CRS from extent", {
	skip_if_not_installed("terra")
	skip_if_not_installed("sf")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	sf_extent = sf::st_as_sfc(sf::st_bbox(
		c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
		crs = sf::st_crs(3857)
	))
	rast = spatialize_image(
		matrix(1:4, nrow = 2, byrow = TRUE),
		extent = sf_extent,
		crs = "EPSG:4326"
	)

	expect_equal(rayshader:::try_parse_scene_crs(terra::crs(rast))$epsg, 4326)
})

test_that("spatialize_image() uses cached scene extent and CRS when explicit values are omitted", {
	skip_if_not_installed("terra")
	skip_if_not_installed("sf")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())
	local_rgl_use_null()

	heightmap = raster::raster(volcano)
	raster::extent(heightmap) = c(
		10,
		10 + ncol(volcano) * 30,
		30,
		30 + nrow(volcano) * 30
	)
	raster::crs(heightmap) = sf::st_crs(3857)$wkt

	expect_no_condition(plot_3d_test(
		sphere_shade(raster_to_matrix(heightmap)),
		heightmap,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(200, 200)
	))

	rast = spatialize_image(matrix(1:4, nrow = 2, byrow = TRUE))

	expect_equal(
		spatialize_image_extent_values(rast),
		get_extent(get_scene_extent())
	)
	expect_equal(rayshader:::try_parse_scene_crs(terra::crs(rast))$epsg, 3857)
})

test_that("spatialize_image() lets explicit extent and CRS override cached scene metadata", {
	skip_if_not_installed("terra")
	skip_if_not_installed("sf")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())
	local_rgl_use_null()

	heightmap = raster::raster(volcano)
	raster::extent(heightmap) = c(
		10,
		10 + ncol(volcano) * 30,
		30,
		30 + nrow(volcano) * 30
	)
	raster::crs(heightmap) = sf::st_crs(3857)$wkt

	expect_no_condition(plot_3d_test(
		sphere_shade(raster_to_matrix(heightmap)),
		heightmap,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(200, 200)
	))

	rast = spatialize_image(
		matrix(1:4, nrow = 2, byrow = TRUE),
		extent = c(xmin = 0, xmax = 2, ymin = 0, ymax = 2),
		crs = "EPSG:4326"
	)

	expect_equal(
		spatialize_image_extent_values(rast),
		c(xmin = 0, xmax = 2, ymin = 0, ymax = 2)
	)
	expect_equal(rayshader:::try_parse_scene_crs(terra::crs(rast))$epsg, 4326)
})

test_that("spatialize_image() resolves panel-specific cached extents for faceted plot_gg scenes", {
	skip_if_not_installed("terra")
	skip_if_not_installed("ggplot2")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())
	local_rgl_use_null()

	gg = ggplot2::ggplot(mtcars) +
		ggplot2::geom_point(ggplot2::aes(wt, mpg, color = mpg)) +
		ggplot2::facet_wrap(~cyl)

	expect_no_condition(suppressWarnings(plot_gg_test(
		gg,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE
	)))

	rast = spatialize_image(matrix(1:4, nrow = 2, byrow = TRUE), panel = 2)

	expect_equal(
		spatialize_image_extent_values(rast),
		get_extent(get_ggplot_extent(panel = 2))
	)
	expect_error(
		spatialize_image(matrix(1:4, nrow = 2, byrow = TRUE)),
		"Supply `panel = <panel>`"
	)
	expect_error(
		spatialize_image(
			matrix(1:4, nrow = 2, byrow = TRUE),
			panel = 2,
			include_height = TRUE,
			heightmap = NULL
		),
		"Cached height export is not supported for faceted `plot_gg\\(\\)` scenes"
	)
})

test_that("spatialize_image() preserves spatial orientation and supports flips", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	mat = matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)

	rast = spatialize_image(mat, extent = c(0, 2, 0, 2), toRGB = FALSE)
	expect_equal(
		spatialize_image_corner_values(rast),
		c(top_left = 1, top_right = 2, bottom_left = 3, bottom_right = 4)
	)

	rast_flip_v = spatialize_image(
		mat,
		extent = c(0, 2, 0, 2),
		toRGB = FALSE,
		flip_vertical = TRUE
	)
	expect_equal(
		spatialize_image_corner_values(rast_flip_v),
		c(top_left = 3, top_right = 4, bottom_left = 1, bottom_right = 2)
	)

	rast_flip_h = spatialize_image(
		mat,
		extent = c(0, 2, 0, 2),
		toRGB = FALSE,
		flip_horizontal = TRUE
	)
	expect_equal(
		spatialize_image_corner_values(rast_flip_h),
		c(top_left = 2, top_right = 1, bottom_left = 4, bottom_right = 3)
	)
})

test_that("spatialize_image() errors clearly when no extent is supplied and no cache exists", {
	skip_if_not_installed("terra")
	clear_spatialize_image_test_cache()
	withr::defer(clear_spatialize_image_test_cache())

	expect_error(
		spatialize_image(matrix(1:4, nrow = 2, byrow = TRUE)),
		"Could not determine `extent`"
	)
})
