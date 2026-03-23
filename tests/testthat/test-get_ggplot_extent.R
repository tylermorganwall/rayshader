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

test_that("transform_ggplot_coords() respects ggplot scale and coord transforms", {
	on.exit(rgl::close3d(), add = TRUE)

	base_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg, color = mpg)) +
		scale_x_log10()
	test_df = data.frame(x = c(2, 4), y = c(15, 30))

	expect_no_condition(plot_gg(
		base_plot,
		width = 4,
		height = 3,
		windowsize = c(1000, 800),
		raytrace = FALSE,
		shadow = FALSE
	))
	coords = transform_ggplot_coords(x = test_df$x, y = test_df$y)
	expect_equal(names(coords), c("long", "lat"))
	expect_true(!is.null(attr(coords, "extent")))

	expected_plot = base_plot +
		geom_point(data = test_df, aes(x, y), inherit.aes = FALSE, color = "red")
	expected_build = ggplot_build(expected_plot)
	expected_layer = expected_build$data[[2]]
	expected_npc = expected_build$layout$coord$transform(
		expected_layer,
		expected_build$layout$panel_params[[1]]
	)
	x_range = tryCatch(
		rayshader:::get_ggplot_panel_range(expected_build$layout$panel_params[[1]], "x"),
		error = function(e) c(0, 1)
	)
	y_range = tryCatch(
		rayshader:::get_ggplot_panel_range(expected_build$layout$panel_params[[1]], "y"),
		error = function(e) c(0, 1)
	)
	expected_long = x_range[1] + as.numeric(expected_npc$x) * diff(x_range)
	expected_lat = y_range[1] + as.numeric(expected_npc$y) * diff(y_range)
	expect_equal(coords$long, expected_long, tolerance = 1e-7)
	expect_equal(coords$lat, expected_lat, tolerance = 1e-7)
	expect_true(all(coords$long >= attr(coords, "extent")["xmin"]))
	expect_true(all(coords$long <= attr(coords, "extent")["xmax"]))
	expect_true(all(coords$lat >= attr(coords, "extent")["ymin"]))
	expect_true(all(coords$lat <= attr(coords, "extent")["ymax"]))
})

test_that("transform_ggplot_coords() handles polar coordinates", {
	on.exit(rgl::close3d(), add = TRUE)

	polar_plot = ggplot(
		data.frame(cat = c("a", "b", "c"), val = c(2, 5, 3)),
		aes(cat, val)
	) +
		geom_point() +
		coord_polar()
	test_df = data.frame(x = c("a", "c"), y = c(2, 3))

	expect_no_condition(suppressWarnings(plot_gg(
		polar_plot,
		width = 4,
		height = 4,
		windowsize = c(900, 900),
		raytrace = FALSE,
		shadow = FALSE
	)))
	coords = transform_ggplot_coords(x = test_df$x, y = test_df$y)
	expect_true(all(coords$long >= 0 & coords$long <= 1))
	expect_true(all(coords$lat >= 0 & coords$lat <= 1))

	expected_plot = polar_plot +
		geom_point(data = test_df, aes(x, y), inherit.aes = FALSE, color = "red")
	expected_build = ggplot_build(expected_plot)
	expected_layer = expected_build$data[[2]]
	expected_npc = expected_build$layout$coord$transform(
		expected_layer,
		expected_build$layout$panel_params[[1]]
	)
	x_range = tryCatch(
		rayshader:::get_ggplot_panel_range(expected_build$layout$panel_params[[1]], "x"),
		error = function(e) c(0, 1)
	)
	y_range = tryCatch(
		rayshader:::get_ggplot_panel_range(expected_build$layout$panel_params[[1]], "y"),
		error = function(e) c(0, 1)
	)
	expected_long = x_range[1] + as.numeric(expected_npc$x) * diff(x_range)
	expected_lat = y_range[1] + as.numeric(expected_npc$y) * diff(y_range)
	expect_equal(coords$long, expected_long, tolerance = 1e-7)
	expect_equal(coords$lat, expected_lat, tolerance = 1e-7)
})

test_that("transform_ggplot_coords() supports coord_sf CRS handling", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	pt = suppressWarnings(sf::st_coordinates(sf::st_centroid(nc[1, ]))[1, ])
	point_df = data.frame(x = pt[1], y = pt[2])
	sf_plot = ggplot(nc) +
		geom_sf() +
		coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(plot_gg(
		sf_plot,
		width = 4,
		height = 3,
		windowsize = c(1000, 800),
		raytrace = FALSE,
		shadow = FALSE
	))
	coords = transform_ggplot_coords(
		x = point_df$x,
		y = point_df$y,
		crs = sf::st_crs(4269)
	)
	expect_equal(nrow(coords), 1)
	expect_true(!is.na(coords$long))
	expect_true(!is.na(coords$lat))

	expected_build = ggplot_build(sf_plot)
	target_crs = expected_build$layout$panel_params[[1]]$crs
	expected_sf = sf::st_as_sf(point_df, coords = c("x", "y"), crs = sf::st_crs(4269))
	expected_sf = sf::st_transform(expected_sf, target_crs)
	expected_xy = sf::st_coordinates(expected_sf)
	expected_npc = expected_build$layout$coord$transform(
		data.frame(x = expected_xy[, 1], y = expected_xy[, 2]),
		expected_build$layout$panel_params[[1]]
	)
	x_range = rayshader:::get_ggplot_panel_range(expected_build$layout$panel_params[[1]], "x")
	y_range = rayshader:::get_ggplot_panel_range(expected_build$layout$panel_params[[1]], "y")
	expected_long = x_range[1] + as.numeric(expected_npc$x) * diff(x_range)
	expected_lat = y_range[1] + as.numeric(expected_npc$y) * diff(y_range)
	expect_equal(coords$long, expected_long, tolerance = 1e-7)
	expect_equal(coords$lat, expected_lat, tolerance = 1e-7)
})

test_that("transform_ggplot_sf() matches vertexwise coordinate transforms", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	sf_plot = ggplot(nc) +
		geom_sf() +
		coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(plot_gg(
		sf_plot,
		width = 4,
		height = 3,
		windowsize = c(1000, 800),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	))
	poly_in = nc[1, ]
	poly_out = transform_ggplot_sf(poly_in)
	poly_coords = sf::st_coordinates(poly_out)
	orig_coords = sf::st_coordinates(poly_in)
	coords_out = transform_ggplot_coords(
		x = orig_coords[, 1],
		y = orig_coords[, 2],
		crs = sf::st_crs(poly_in)
	)
	expect_s3_class(poly_out, "sf")
	expect_equal(poly_coords[, 1], coords_out$long, tolerance = 1e-6)
	expect_equal(poly_coords[, 2], coords_out$lat, tolerance = 1e-6)

	extent_out = attr(poly_out, "extent")
	expect_true(all(poly_coords[, 1] >= extent_out["xmin"]))
	expect_true(all(poly_coords[, 1] <= extent_out["xmax"]))
	expect_true(all(poly_coords[, 2] >= extent_out["ymin"]))
	expect_true(all(poly_coords[, 2] <= extent_out["ymax"]))
})

test_that("transform_ggplot_sf() supports segmentization and geometry class passthrough", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)

	polar_plot = ggplot(
		data.frame(theta = seq(0, 350, by = 10), r = rep(1, 36)),
		aes(theta, r)
	) +
		geom_col(width = 10) +
		coord_polar()
	expect_no_condition(suppressWarnings(plot_gg(
		polar_plot,
		width = 4,
		height = 4,
		windowsize = c(900, 900),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	triangle = sf::st_polygon(list(matrix(
		c(0, 0.30, 120, 0.75, 240, 0.40, 0, 0.30),
		ncol = 2,
		byrow = TRUE
	)))
	poly_sf = sf::st_sf(geometry = sf::st_sfc(triangle))
	plain_poly = transform_ggplot_sf(poly_sf)
	dense_poly = transform_ggplot_sf(
		poly_sf,
		segmentize_df_max_length = 20
	)
	expect_gt(
		nrow(sf::st_coordinates(dense_poly)),
		nrow(sf::st_coordinates(plain_poly))
	)
	poly_sfc = transform_ggplot_sf(sf::st_geometry(poly_sf))
	poly_sfg = transform_ggplot_sf(sf::st_geometry(poly_sf)[[1]])
	expect_s3_class(poly_sfc, "sfc")
	expect_s3_class(poly_sfg, "POLYGON")

	extent_out = attr(dense_poly, "extent")
	dense_coords = sf::st_coordinates(dense_poly)
	expect_true(all(dense_coords[, 1] >= extent_out["xmin"]))
	expect_true(all(dense_coords[, 1] <= extent_out["xmax"]))
	expect_true(all(dense_coords[, 2] >= extent_out["ymin"]))
	expect_true(all(dense_coords[, 2] <= extent_out["ymax"]))
})
