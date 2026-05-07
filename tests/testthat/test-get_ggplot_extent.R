library(ggplot2)

test_that("scene extent resolution infers extent from spatial heightmaps", {
	testthat::skip_if_not_installed("terra")
	clear_hillshade_cache()
	reset_scene_context(
		clear_scene_metadata = TRUE,
		clear_scene_cache = TRUE
	)
	withr::defer({
		clear_hillshade_cache()
		reset_scene_context(
			clear_scene_metadata = TRUE,
			clear_scene_cache = TRUE
		)
	})

	cache_scene_extent(c(xmin = 0, xmax = 1, ymin = 0, ymax = 1), label = "stale")
	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		xmin = 10,
		xmax = 13,
		ymin = 20,
		ymax = 22,
		crs = "EPSG:4326"
	)
	terra::values(rast) = seq_len(terra::ncell(rast))

	expect_equal(
		resolve_scene_render_extent(heightmap = rast, caller = "test"),
		c(xmin = 10, xmax = 13, ymin = 20, ymax = 22)
	)
})

test_that("scene render heightmap resolution coerces spatial heightmaps", {
	testthat::skip_if_not_installed("terra")

	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		xmin = 10,
		xmax = 13,
		ymin = 20,
		ymax = 22,
		crs = "EPSG:4326"
	)
	terra::values(rast) = seq_len(terra::ncell(rast))

	heightmap = rayshader:::resolve_scene_render_heightmap(
		rast,
		caller = "test"
	)

	expect_true(is.matrix(heightmap))
	expect_equal(
		attr(heightmap, "extent", exact = TRUE),
		c(xmin = 10, xmax = 13, ymin = 20, ymax = 22)
	)
	expect_false(is.null(attr(heightmap, "crs", exact = TRUE)))
})

test_that("internal ggplot scene extent maps a single panel into scene coordinates", {
	on.exit(rgl::close3d(), add = TRUE)

	gg = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg))

	height_matrix = plot_gg_test(
		gg,
		width = 3,
		height = 3,
		windowsize = c(800, 800),
		raytrace = FALSE,
		shadow = FALSE,
		save_height_matrix = TRUE
	)
	gg_extent = rayshader:::get_ggplot_extent()
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

test_that("internal ggplot scene extent returns one extent per facet panel", {
	on.exit(rgl::close3d(), add = TRUE)

	gg = ggplot(mtcars) +
		geom_point(aes(x = wt, y = mpg, color = mpg)) +
		facet_wrap(~cyl)

	expect_no_condition(plot_gg_test(
		gg,
		width = 5,
		height = 3,
		windowsize = c(1000, 800),
		raytrace = FALSE,
		shadow = FALSE
	))

	all_extents = rayshader:::get_ggplot_extent()
	panel_two_extent = rayshader:::get_ggplot_extent(panel = 2)

	expect_type(all_extents, "list")
	expect_named(all_extents, c("panel_1", "panel_2", "panel_3"))
	expect_equal(length(all_extents), 3)
	expect_equal(panel_two_extent, all_extents[[2]])
	expect_equal(attr(panel_two_extent, "panel_info")$panel, 2L)
})

test_that("internal ggplot coordinate transform respects ggplot scale and coord transforms", {
	on.exit(rgl::close3d(), add = TRUE)

	base_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg, color = mpg)) +
		scale_x_log10()
	test_df = data.frame(x = c(2, 4), y = c(15, 30))

	expect_no_condition(plot_gg_test(
		base_plot,
		width = 4,
		height = 3,
		windowsize = c(1000, 800),
		raytrace = FALSE,
		shadow = FALSE
	))
	coords = rayshader:::transform_ggplot_coords(x = test_df$x, y = test_df$y)
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

test_that("internal ggplot coordinate transform handles polar coordinates", {
	on.exit(rgl::close3d(), add = TRUE)

	polar_plot = ggplot(
		data.frame(cat = c("a", "b", "c"), val = c(2, 5, 3)),
		aes(cat, val)
	) +
		geom_point() +
		coord_polar()
	test_df = data.frame(x = c("a", "c"), y = c(2, 3))

	expect_no_condition(suppressWarnings(plot_gg_test(
		polar_plot,
		width = 4,
		height = 4,
		windowsize = c(900, 900),
		raytrace = FALSE,
		shadow = FALSE
	)))
	coords = rayshader:::transform_ggplot_coords(x = test_df$x, y = test_df$y)
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

test_that("internal ggplot coordinate transform supports coord_sf CRS handling", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	pt = suppressWarnings(sf::st_coordinates(sf::st_centroid(nc[1, ]))[1, ])
	point_df = data.frame(x = pt[1], y = pt[2])
	sf_plot = ggplot(nc) +
		geom_sf() +
		coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(plot_gg_test(
		sf_plot,
		width = 4,
		height = 3,
		windowsize = c(1000, 800),
		raytrace = FALSE,
		shadow = FALSE
	))
	coords = rayshader:::transform_ggplot_coords(
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

test_that("internal ggplot sf transform matches vertexwise coordinate transforms", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	sf_plot = ggplot(nc) +
		geom_sf() +
		coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(plot_gg_test(
		sf_plot,
		width = 4,
		height = 3,
		windowsize = c(1000, 800),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	))
	poly_in = nc[1, ]
	poly_out = rayshader:::transform_ggplot_sf(poly_in)
	poly_coords = sf::st_coordinates(poly_out)
	orig_coords = sf::st_coordinates(poly_in)
	coords_out = rayshader:::transform_ggplot_coords(
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

test_that("internal ggplot sf transform supports segmentization and geometry class passthrough", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)

	polar_plot = ggplot(
		data.frame(theta = seq(0, 350, by = 10), r = rep(1, 36)),
		aes(theta, r)
	) +
		geom_col(width = 10) +
		coord_polar()
	expect_no_condition(suppressWarnings(plot_gg_test(
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
	plain_poly = rayshader:::transform_ggplot_sf(poly_sf)
	dense_poly = rayshader:::transform_ggplot_sf(
		poly_sf,
		segmentize_df_max_length = 20
	)
	expect_gt(
		nrow(sf::st_coordinates(dense_poly)),
		nrow(sf::st_coordinates(plain_poly))
	)
	poly_sfc = rayshader:::transform_ggplot_sf(sf::st_geometry(poly_sf))
	poly_sfg = rayshader:::transform_ggplot_sf(sf::st_geometry(poly_sf)[[1]])
	expect_s3_class(poly_sfc, "sfc")
	expect_s3_class(poly_sfg, "POLYGON")

	extent_out = attr(dense_poly, "extent")
	dense_coords = sf::st_coordinates(dense_poly)
	expect_true(all(dense_coords[, 1] >= extent_out["xmin"]))
	expect_true(all(dense_coords[, 1] <= extent_out["xmax"]))
	expect_true(all(dense_coords[, 2] >= extent_out["ymin"]))
	expect_true(all(dense_coords[, 2] <= extent_out["ymax"]))
})

test_that("faceted ggplot cached extent resolution requires explicit panel disambiguation", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	faceted_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		facet_wrap(~cyl)

	expect_no_condition(suppressWarnings(plot_gg_test(
		faceted_plot,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	expect_error(
		rayshader:::resolve_scene_render_extent(
			heightmap = rayshader:::get_scene_heightmap(),
			caller = "render_path"
		),
		"Supply `panel = <panel>`"
	)
	expect_error(
		rayshader:::resolve_scene_render_extent(
			extent = rayshader:::get_ggplot_extent(panel = 1),
			heightmap = rayshader:::get_scene_heightmap(),
			panel = 2,
			caller = "render_path"
		),
		"refer to different facet panels"
	)
})

test_that("faceted ggplot transforms support explicit panel routing", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	faceted_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		facet_wrap(~cyl, scales = "free_x")

	expect_no_condition(suppressWarnings(plot_gg_test(
		faceted_plot,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	panel_two_extent = rayshader:::get_ggplot_extent(panel = 2)
	panel_two_coords = rayshader:::transform_ggplot_coords(
		x = c(3.0, 3.2),
		y = c(20, 19),
		panel = 2
	)

	expect_equal(attr(panel_two_extent, "panel_info")$panel, 2L)
	expect_equal(attr(panel_two_coords, "panel"), 2L)
	expect_equal(as.numeric(attr(panel_two_coords, "extent")), as.numeric(panel_two_extent))
	expect_true(all(panel_two_coords$long >= panel_two_extent["xmin"]))
	expect_true(all(panel_two_coords$long <= panel_two_extent["xmax"]))
	expect_true(all(panel_two_coords$lat >= panel_two_extent["ymin"]))
	expect_true(all(panel_two_coords$lat <= panel_two_extent["ymax"]))
})

test_that("render_path() uses public panel routing for faceted ggplot scenes", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	faceted_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		facet_wrap(~cyl, scales = "free_x")

	expect_no_condition(suppressWarnings(plot_gg_test(
		faceted_plot,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	expect_error(
		render_path(
			x = c(3.0, 3.2),
			y = c(20, 19),
			altitude = 0,
			return_coords = TRUE
		),
		"Supply `panel = <panel>`"
	)

	coords = render_path(
		x = c(3.0, 3.2),
		y = c(20, 19),
		altitude = 0,
		panel = 2,
		return_coords = TRUE
	)
	expected_scene_xy = rayshader:::transform_ggplot_coords(
		x = c(3.0, 3.2),
		y = c(20, 19),
		panel = 2
	)
	expected_coords = rayshader:::transform_into_heightmap_coords(
		extent = attr(expected_scene_xy, "extent"),
		heightmap = rayshader:::get_scene_heightmap(),
		lat = expected_scene_xy$lat,
		long = expected_scene_xy$long,
		altitude = c(0, 0),
		zscale = rayshader:::get_scene_effective_zscale(),
		transform_scene = FALSE,
		caller = "test"
	)

	expect_equal(length(coords), 1)
	expect_equal(coords[[1]], expected_coords, tolerance = 1e-6)
})

test_that("render_raymesh() uses public panel routing for faceted ggplot scenes", {
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	faceted_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		facet_wrap(~cyl, scales = "free_x")

	expect_no_condition(suppressWarnings(plot_gg_test(
		faceted_plot,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	mesh = rayvertex::sphere_mesh(radius = 0.1)
	expect_error(
		render_raymesh(
			mesh,
			x = 3.1,
			y = 20,
			altitude = 0,
			clear_previous = TRUE
		),
		"Supply `panel = <panel>`"
	)

	expect_no_condition(render_raymesh(
		mesh,
		x = 3.1,
		y = 20,
		altitude = 0,
		panel = 2,
		clear_previous = TRUE
	))
	obj_ids = get_ids_with_labels(typeval = "obj")
	obj_id = obj_ids$id[grepl("^obj_raymesh", obj_ids$tag)][1]
	obj_verts = rgl::rgl.attrib(obj_id, "vertices")
	obj_center = c(
		mean(range(obj_verts[, 1])),
		mean(range(obj_verts[, 2])),
		mean(range(obj_verts[, 3]))
	)
	expected_scene_xy = rayshader:::transform_ggplot_coords(x = 3.1, y = 20, panel = 2)
	expected_center = rayshader:::transform_into_heightmap_coords(
		extent = attr(expected_scene_xy, "extent"),
		heightmap = rayshader:::get_scene_heightmap(),
		lat = expected_scene_xy$lat,
		long = expected_scene_xy$long,
		altitude = 0,
		zscale = rayshader:::get_scene_effective_zscale(),
		transform_scene = FALSE,
		caller = "test"
	)[1, ]

	expect_equal(obj_center, as.numeric(expected_center), tolerance = 1e-6)
})

test_that("render_polygons() requires panel for faceted cached ggplot scenes", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	faceted_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		facet_wrap(~cyl, scales = "free_x")

	expect_no_condition(suppressWarnings(plot_gg_test(
		faceted_plot,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	poly = sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
		c(
			2.8, 18.5,
			3.2, 18.5,
			3.2, 19.5,
			2.8, 19.5,
			2.8, 18.5
		),
		ncol = 2,
		byrow = TRUE
	)))))

	expect_error(
		render_polygons(poly, top = 1, bottom = 0, clear_previous = TRUE),
		"Supply `panel = <panel>`"
	)
	expect_no_condition(render_polygons(
		poly,
		top = 1,
		bottom = 0,
		panel = 2,
		clear_previous = TRUE
	))
	expect_true(any(get_ids_with_labels()$tag == "polygon3d"))
})

test_that("render_zaxis() requires panel for faceted cached ggplot scenes", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	faceted_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		facet_wrap(~cyl)

	expect_no_condition(suppressWarnings(plot_gg_test(
		faceted_plot,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	expect_error(
		render_zaxis(zaxis_breaks = c(0, 50, 100)),
		"Supply `panel = <panel>`"
	)
	expect_error(
		render_zaxis(
			zaxis_location = "panel_bottomleft",
			zaxis_breaks = c(0, 50, 100)
		),
		"Supply `panel = <panel>`"
	)
	expect_no_condition(render_zaxis(
		zaxis_location = "topleft",
		zaxis_breaks = c(0, 50, 100),
		zaxis_title_offset = 6
	))
	expect_true(any(get_ids_with_labels()$tag == "zaxis_axis"))

	expect_no_condition(render_zaxis(
		panel = 2,
		zaxis_breaks = c(0, 50, 100)
	))
	expect_true(any(get_ids_with_labels()$tag == "zaxis_axis"))
})

test_that("coord_sf numeric x/y inputs require explicit CRS metadata", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	pt_ll = suppressWarnings(sf::st_coordinates(
		sf::st_transform(sf::st_centroid(nc[1, ]), 4326)
	)[1, ])
	pt_projected = suppressWarnings(sf::st_coordinates(
		sf::st_transform(sf::st_centroid(nc[1, ]), 32119)
	)[1, ])
	sf_plot = ggplot(nc) +
		geom_sf() +
		coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(plot_gg_test(
		sf_plot,
		width = 4,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	))

	expect_error(
		rayshader:::transform_ggplot_coords(
			x = pt_ll[1],
			y = pt_ll[2]
		),
		"must include `crs`"
	)
	expect_s3_class(rayshader:::transform_ggplot_coords(
		x = pt_ll[1],
		y = pt_ll[2],
		crs = 4326
	), "data.frame")

	expect_error(
		rayshader:::transform_ggplot_coords(
			x = pt_projected[1],
			y = pt_projected[2]
		),
		"must include `crs`"
	)
})

test_that("coord_sf sf inputs require CRS metadata or an explicit crs argument", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	sf_plot = ggplot(nc) +
		geom_sf() +
		coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(plot_gg_test(
		sf_plot,
		width = 4,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	))

	nc_no_crs = suppressWarnings(sf::st_set_crs(nc[1, ], NA))
	expect_error(
		rayshader:::transform_ggplot_sf(nc_no_crs),
		"must carry a CRS or `crs` must be supplied"
	)
	expect_s3_class(
		rayshader:::transform_ggplot_sf(nc_no_crs, crs = 4269),
		"sf"
	)
})

test_that("faceted reversed-scale transforms remain panel-aware", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	reversed_plot = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		facet_wrap(~cyl, scales = "free_y") +
		scale_y_reverse()

	expect_no_condition(suppressWarnings(plot_gg_test(
		reversed_plot,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	coords = rayshader:::transform_ggplot_coords(
		x = c(3.0, 3.2),
		y = c(18, 20),
		panel = 2
	)
	panel_two_extent = rayshader:::get_ggplot_extent(panel = 2)

	expect_true(all(is.finite(as.matrix(coords))))
	expect_true(all(coords$long >= panel_two_extent["xmin"]))
	expect_true(all(coords$long <= panel_two_extent["xmax"]))
	expect_true(all(coords$lat >= panel_two_extent["ymin"]))
	expect_true(all(coords$lat <= panel_two_extent["ymax"]))
})
