compare_location_snapshot_image_test = function(path1, path2) {
	image1 = png::readPNG(path1)
	image2 = png::readPNG(path2)
	identical(image1, image2)
}

flatten_point_location_coords_test = function(location) {
	sf_data = rayshader:::coerce_scene_sf_input(location)$sf_data
	point_sf = suppressWarnings(sf::st_cast(sf_data, "POINT", warn = FALSE))
	sf::st_coordinates(point_sf)[, 1:2, drop = FALSE]
}

monterey_point_location_fixtures_test = function() {
	county_points = suppressWarnings(sf::st_centroid(monterey_counties_sf[1:3, ]))
	county_points_3857 = sf::st_transform(county_points, 3857)
	single_multipoint_sfc = sf::st_sfc(
		sf::st_multipoint(flatten_point_location_coords_test(county_points_3857[1, ])),
		crs = sf::st_crs(county_points_3857)
	)
	multipoint_sfc = sf::st_sfc(
		sf::st_multipoint(flatten_point_location_coords_test(county_points_3857)),
		crs = sf::st_crs(county_points_3857)
	)
	single_sfc = sf::st_geometry(county_points_3857[1, ])
	list(
		single_sf = county_points_3857[1, ],
		single_sfc = single_sfc,
		single_sfg = single_sfc[[1]],
		single_sp = if (requireNamespace("sp", quietly = TRUE)) {
			sf::as_Spatial(county_points_3857[1, ])
		} else {
			NULL
		},
		single_multipoint_sfc = single_multipoint_sfc,
		multi_sf = county_points_3857,
		multipoint_sfc = multipoint_sfc,
		no_crs_single_sf = suppressWarnings(sf::st_set_crs(county_points_3857[1, ], NA)),
		no_crs_single_sfc = suppressWarnings(sf::st_set_crs(single_sfc, NA)),
		no_crs_single_multipoint_sfc = suppressWarnings(
			sf::st_set_crs(single_multipoint_sfc, NA)
		),
		no_crs_multipoint_sfc = suppressWarnings(sf::st_set_crs(multipoint_sfc, NA))
	)
}

monterey_spatial_raster_test = function() {
	scene_extent = attr(montereybay, "extent")
	raster::raster(
		t(montereybay),
		xmn = scene_extent@xmin,
		xmx = scene_extent@xmax,
		ymn = scene_extent@ymin,
		ymx = scene_extent@ymax,
		crs = attr(montereybay, "crs")
	)
}

setup_plot3d_location_scene_test = function() {
	monterey_raster = monterey_spatial_raster_test()
	expect_no_condition(plot_3d_test(
		sphere_shade(montereybay),
		monterey_raster,
		zscale = 50,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(320, 320)
	))
	render_camera(theta = 220, phi = 35, zoom = 0.55, fov = 60)
}

setup_plotgg_location_scene_test = function() {
	p = ggplot2::ggplot(monterey_counties_sf[1:6, ]) +
		ggplot2::geom_sf(fill = "grey90", color = "grey25", linewidth = 0.2) +
		ggplot2::coord_sf(crs = sf::st_crs(3310))
	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 4,
		height = 4,
		windowsize = c(500, 500),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))
	render_camera(theta = 0, phi = 35, zoom = 0.8, fov = 60)
}

setup_plotgg_location_scene_faceted_test = function() {
	faceted_counties = monterey_counties_sf[1:6, ]
	faceted_counties$facet = factor(rep(c("a", "b"), each = 3), levels = c("a", "b"))
	p = ggplot2::ggplot(faceted_counties) +
		ggplot2::geom_sf(fill = "grey90", color = "grey25", linewidth = 0.2) +
		ggplot2::facet_wrap(~facet) +
		ggplot2::coord_sf(crs = sf::st_crs(3310))
	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 6,
		height = 3.5,
		windowsize = c(700, 450),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))
	render_camera(theta = 0, phi = 35, zoom = 0.8, fov = 60)
	invisible(faceted_counties)
}

expected_plot3d_location_xy_test = function(location, crs = NULL) {
	target_crs = sf::st_crs(attr(montereybay, "crs"))
	scene_points = sf::st_transform(
		rayshader:::coerce_scene_point_input(location, crs = crs, caller = "test")$sf_data,
		target_crs
	)
	unname(flatten_point_location_coords_test(scene_points))
}

expected_plot3d_location_xy_from_explicit_crs_test = function(location, crs) {
	sf_data = rayshader:::coerce_scene_sf_input(location)$sf_data
	scene_points = sf::st_transform(
		sf::st_set_crs(sf_data, crs),
		sf::st_crs(attr(montereybay, "crs"))
	)
	unname(flatten_point_location_coords_test(scene_points))
}

expected_plotgg_location_xy_test = function(location, crs = NULL, panel = NULL) {
	target_crs = sf::st_crs(3310)
	target_points = sf::st_transform(
		rayshader:::coerce_scene_point_input(location, crs = crs, caller = "test")$sf_data,
		target_crs
	)
	target_coords = flatten_point_location_coords_test(target_points)
	scene_xy = rayshader:::transform_ggplot_coords(
		x = target_coords[, 1],
		y = target_coords[, 2],
		crs = target_crs,
		panel = panel
	)
	unname(as.matrix(scene_xy[, c("long", "lat")]))
}

expected_plotgg_location_xy_from_explicit_crs_test = function(location, crs, panel = NULL) {
	target_crs = sf::st_crs(3310)
	sf_data = rayshader:::coerce_scene_sf_input(location)$sf_data
	target_points = sf::st_transform(sf::st_set_crs(sf_data, crs), target_crs)
	target_coords = flatten_point_location_coords_test(target_points)
	scene_xy = rayshader:::transform_ggplot_coords(
		x = target_coords[, 1],
		y = target_coords[, 2],
		crs = target_crs,
		panel = panel
	)
	unname(as.matrix(scene_xy[, c("long", "lat")]))
}

extract_scene_point_xy_test = function(location, crs = NULL, panel = NULL) {
	scene_xy = rayshader:::extract_scene_point_xy(
		location = location,
		heightmap = rayshader:::get_scene_heightmap(),
		crs = crs,
		panel = panel,
		caller = "test"
	)
	unname(cbind(scene_xy$x, scene_xy$y))
}

renderer_location_cases_test = function(fixtures, scene = c("plot3d", "plotgg")) {
	scene = match.arg(scene)
	mesh = rayvertex::sphere_mesh(radius = 0.08)
	label_z = if (scene == "plot3d") 2000 else 100
	points_altitude = if (scene == "plot3d") 500 else 10
	obj_scale = if (scene == "plot3d") c(3, 3, 3) else c(1.5, 1.5, 1.5)
	raymesh_scale = if (scene == "plot3d") c(6, 6, 6) else c(2.5, 2.5, 2.5)
	tree_height = if (scene == "plot3d") 1200 else 8
	list(
		render_label = list(
			location = fixtures$single_sfg,
			crs = sf::st_crs(fixtures$single_sf),
			render = function(location) {
				render_label(
					text = "A",
					location = location,
					crs = sf::st_crs(fixtures$single_sf),
					z = label_z,
					linecolor = "firebrick",
					textalpha = 0,
					linewidth = 4,
					clear_previous = TRUE
				)
			},
			check = function() {
				ids = get_ids_with_labels(typeval = c("raytext", "textline"))
				expect_true(any(ids$tag == "textline"))
			}
		),
		render_points = list(
			location = fixtures$multipoint_sfc,
			crs = NULL,
			render = function(location) {
				render_points(
					location = location,
					altitude = points_altitude,
					offset = 0,
					size = 6,
					color = "orange",
					clear_previous = TRUE
				)
			},
			check = function() {
				expect_true(any(get_ids_with_labels()$tag == "points3d"))
			}
		),
		render_obj = list(
			location = fixtures$multi_sf,
			crs = NULL,
			render = function(location) {
				render_obj(
					flag_pole_obj(),
					location = location,
					altitude = 0,
					offset = 0,
					scale = obj_scale,
					color = "white",
					clear_previous = TRUE
				)
			},
			check = function() {
				expect_true(any(get_ids_with_labels()$tag == "obj"))
			}
		),
		render_raymesh = list(
			location = fixtures$multi_sf,
			crs = NULL,
			render = function(location) {
				render_raymesh(
					mesh,
					location = location,
					altitude = 0,
					offset = 0,
					scale = raymesh_scale,
					change_material = FALSE,
					color = "#4fa3ff",
					clear_previous = TRUE
				)
			},
			check = function() {
				expect_true(any(get_ids_with_labels()$tag == "obj_raymesh"))
			}
		),
		render_tree = list(
			location = fixtures$multi_sf,
			crs = NULL,
			render = function(location) {
				render_tree(
					location = location,
					tree_height = tree_height,
					tree_zscale = FALSE,
					crown_color = "#2d6a4f",
					trunk_color = "#6f4e37",
					clear_previous = TRUE
				)
			},
			check = function() {
				expect_true(any(get_ids_with_labels()$tag == "objtree"))
			}
		)
	)
}

expect_location_snapshot_test = function(snapshot_name, setup_scene, render_call) {
	if (rgl::cur3d() != 0) {
		rgl::close3d()
	}
	setup_scene()
	render_call()
	path = tempfile(fileext = ".png")
	render_snapshot(
		filename = path,
		software_render = TRUE,
		cache_scene = TRUE,
		width = 400,
		height = 400,
		point_radius = 6,
		line_radius = 4,
		fsaa = 1
	)
	expect_snapshot_file(
		path,
		name = snapshot_name,
		compare = compare_location_snapshot_image_test
	)
	rgl::close3d()
}

test_that("coerce_scene_point_input() accepts supported point input classes", {
	skip_if_not_installed("sf")

	fixtures = monterey_point_location_fixtures_test()
	inputs = list(
		sf = fixtures$single_sf,
		sfc = fixtures$single_sfc,
		sfg = fixtures$single_sfg
	)
	if (!is.null(fixtures$single_sp)) {
		inputs$sp = fixtures$single_sp
	}

	for (input_name in names(inputs)) {
		coerced = rayshader:::coerce_scene_point_input(
			inputs[[input_name]],
			caller = "test"
		)
		expect_equal(coerced$feature_count, 1, label = input_name)
		expect_equal(coerced$geometry_count, 1, label = input_name)
	}
})

test_that("coerce_scene_point_input() assigns explicit crs to CRS-less point inputs", {
	skip_if_not_installed("sf")

	fixtures = monterey_point_location_fixtures_test()
	coerced = rayshader:::coerce_scene_point_input(
		fixtures$no_crs_multipoint_sfc,
		crs = sf::st_crs(3857),
		caller = "test"
	)

	expect_true(rayshader:::scene_crs_equal(coerced$source_crs, sf::st_crs(3857)))
	expect_equal(coerced$feature_count, 1)
	expect_equal(coerced$geometry_count, 3)
	expect_equal(
		unname(cbind(coerced$x, coerced$y)),
		unname(flatten_point_location_coords_test(fixtures$multipoint_sfc)),
		tolerance = 1e-6
	)
})

test_that("CRS-less location inputs transform with explicit crs in plot_3d scenes", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raster")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	fixtures = monterey_point_location_fixtures_test()
	setup_plot3d_location_scene_test()

	expected_points = expected_plot3d_location_xy_from_explicit_crs_test(
		fixtures$no_crs_multipoint_sfc,
		crs = sf::st_crs(3857)
	)
	actual_points = extract_scene_point_xy_test(
		fixtures$no_crs_multipoint_sfc,
		crs = sf::st_crs(3857)
	)
	expect_equal(actual_points, expected_points, tolerance = 1e-6)

	expect_no_condition(render_points(
		location = fixtures$no_crs_multipoint_sfc,
		crs = sf::st_crs(3857),
		altitude = 0,
		offset = 0,
		size = 5,
		color = "orange",
		clear_previous = TRUE
	))
	point_id = get_ids_with_labels()$id[get_ids_with_labels()$tag == "points3d"][1]
	expect_equal(nrow(rgl::rgl.attrib(point_id, "vertices")), nrow(expected_points))

	expected_label = expected_plot3d_location_xy_from_explicit_crs_test(
		fixtures$no_crs_single_sfc,
		crs = sf::st_crs(3857)
	)
	actual_label = extract_scene_point_xy_test(
		fixtures$no_crs_single_sfc,
		crs = sf::st_crs(3857)
	)
	expect_equal(actual_label, expected_label, tolerance = 1e-6)
	expect_no_condition(render_label(
		text = "A",
		location = fixtures$no_crs_single_sfc,
		crs = sf::st_crs(3857),
		z = 1500,
		clear_previous = TRUE
	))
})

test_that("MULTIPOINT locations are flattened for vectorized renderers", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raster")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	fixtures = monterey_point_location_fixtures_test()
	setup_plot3d_location_scene_test()

	expected_xy = expected_plot3d_location_xy_test(fixtures$multipoint_sfc)
	actual_xy = extract_scene_point_xy_test(fixtures$multipoint_sfc)
	expect_equal(actual_xy, expected_xy, tolerance = 1e-6)
	expect_equal(nrow(actual_xy), 3)

	expect_no_condition(render_points(
		location = fixtures$multipoint_sfc,
		altitude = 0,
		offset = 0,
		size = 4,
		color = "orange",
		clear_previous = TRUE
	))
	point_id = get_ids_with_labels()$id[get_ids_with_labels()$tag == "points3d"][1]
	expect_equal(nrow(rgl::rgl.attrib(point_id, "vertices")), nrow(expected_xy))
})

test_that("render_label() requires exactly one point after MULTIPOINT flattening", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raster")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	fixtures = monterey_point_location_fixtures_test()
	setup_plot3d_location_scene_test()

	expect_no_condition(render_label(
		text = "A",
		location = fixtures$single_multipoint_sfc,
		z = 1500,
		clear_previous = TRUE
	))
	ids = get_ids_with_labels(typeval = c("raytext", "textline"))
	expect_true(any(ids$tag == "textline"))

	expect_error(
		render_label(
			text = "A",
			location = fixtures$multipoint_sfc,
			z = 1500,
			clear_previous = TRUE
		),
		"exactly one point"
	)
})

test_that("plot_3d scenes transform spatial point locations for point-anchored renderers", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raster")
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	fixtures = monterey_point_location_fixtures_test()
	cases = renderer_location_cases_test(fixtures, scene = "plot3d")

	for (case_name in names(cases)) {
		setup_plot3d_location_scene_test()
		expected_xy = expected_plot3d_location_xy_test(
			cases[[case_name]]$location,
			crs = cases[[case_name]]$crs
		)
		actual_xy = extract_scene_point_xy_test(
			cases[[case_name]]$location,
			crs = cases[[case_name]]$crs
		)
		expect_equal(actual_xy, expected_xy, tolerance = 1e-6, label = case_name)
		expect_no_condition(cases[[case_name]]$render(cases[[case_name]]$location))
		cases[[case_name]]$check()
		rgl::close3d()
	}
})

test_that("faceted coord_sf scenes use panel-aware location transforms", {
	skip_if_not_installed("sf")
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	fixtures = monterey_point_location_fixtures_test()
	setup_plotgg_location_scene_faceted_test()

	expect_error(
		extract_scene_point_xy_test(fixtures$single_sf),
		"Supply `panel = <panel>`"
	)
	expect_error(
		render_points(
			location = fixtures$single_sf,
			altitude = 0,
			offset = 0,
			clear_previous = TRUE
		),
		"Supply `panel = <panel>`"
	)

	scene_panel_one = rayshader:::extract_scene_point_xy(
		location = fixtures$single_sf,
		heightmap = rayshader:::get_scene_heightmap(),
		panel = 1,
		caller = "test"
	)
	scene_panel_two = rayshader:::extract_scene_point_xy(
		location = fixtures$single_sf,
		heightmap = rayshader:::get_scene_heightmap(),
		panel = 2,
		caller = "test"
	)
	actual_panel_one = unname(cbind(scene_panel_one$x, scene_panel_one$y))
	actual_panel_two = unname(cbind(scene_panel_two$x, scene_panel_two$y))
	expected_panel_one = expected_plotgg_location_xy_test(
		fixtures$single_sf,
		panel = 1
	)
	expected_panel_two = expected_plotgg_location_xy_test(
		fixtures$single_sf,
		panel = 2
	)
	expect_identical(scene_panel_one$panel, 1)
	expect_identical(scene_panel_two$panel, 2)
	expect_equal(
		rayshader:::get_extent(scene_panel_one$extent),
		rayshader:::get_extent(rayshader:::get_ggplot_extent(panel = 1))
	)
	expect_equal(
		rayshader:::get_extent(scene_panel_two$extent),
		rayshader:::get_extent(rayshader:::get_ggplot_extent(panel = 2))
	)
	expect_equal(actual_panel_one, expected_panel_one, tolerance = 1e-6)
	expect_equal(actual_panel_two, expected_panel_two, tolerance = 1e-6)

	expect_no_condition(render_points(
		location = fixtures$single_sf,
		panel = 1,
		altitude = 0,
		offset = 0,
		size = 6,
		color = "orange",
		clear_previous = TRUE
	))
	expect_true(any(get_ids_with_labels()$tag == "points3d"))
})

test_that("CRS-less location inputs transform with explicit crs in coord_sf scenes", {
	skip_if_not_installed("sf")
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	fixtures = monterey_point_location_fixtures_test()
	setup_plotgg_location_scene_test()

	expected_points = expected_plotgg_location_xy_from_explicit_crs_test(
		fixtures$no_crs_multipoint_sfc,
		crs = sf::st_crs(3857)
	)
	actual_points = extract_scene_point_xy_test(
		fixtures$no_crs_multipoint_sfc,
		crs = sf::st_crs(3857)
	)
	expect_equal(actual_points, expected_points, tolerance = 1e-6)

	expect_no_condition(render_points(
		location = fixtures$no_crs_multipoint_sfc,
		crs = sf::st_crs(3857),
		altitude = 0,
		offset = 0,
		size = 5,
		color = "orange",
		clear_previous = TRUE
	))
	expect_true(any(get_ids_with_labels()$tag == "points3d"))
})

test_that("faceted coord_sf location snapshots match goldens", {
	testthat::skip_on_cran()
	skip_if_not_installed("sf")
	skip_if_not_installed("rayvertex")
	skip_if(
		rgl::rgl.useNULL(),
		message = "Software golden snapshots require a live rgl device."
	)

	fixtures = monterey_point_location_fixtures_test()
	expect_location_snapshot_test(
		"plotgg_faceted_render_points_panel1.png",
		setup_scene = setup_plotgg_location_scene_faceted_test,
		render_call = function() {
			render_points(
				location = fixtures$single_sf,
				panel = 1,
				altitude = 0,
				offset = 0,
				size = 6,
				color = "orange",
				clear_previous = TRUE
			)
		}
	)
})

test_that("coord_sf scenes transform spatial point locations for point-anchored renderers", {
	skip_if_not_installed("sf")
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	fixtures = monterey_point_location_fixtures_test()
	cases = renderer_location_cases_test(fixtures, scene = "plotgg")

	for (case_name in names(cases)) {
		setup_plotgg_location_scene_test()
		expected_xy = expected_plotgg_location_xy_test(
			cases[[case_name]]$location,
			crs = cases[[case_name]]$crs
		)
		actual_xy = extract_scene_point_xy_test(
			cases[[case_name]]$location,
			crs = cases[[case_name]]$crs
		)
		expect_equal(actual_xy, expected_xy, tolerance = 1e-6, label = case_name)
		expect_no_condition(cases[[case_name]]$render(cases[[case_name]]$location))
		cases[[case_name]]$check()
		rgl::close3d()
	}
})

test_that("point-anchored renderers validate location inputs", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raster")
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	fixtures = monterey_point_location_fixtures_test()
	setup_plot3d_location_scene_test()

	expect_error(
		render_points(
			location = fixtures$single_sf,
			crs = sf::st_crs(4326),
			altitude = 0,
			clear_previous = TRUE
		),
		"conflicts with `crs`"
	)
	expect_error(
		render_points(
			location = monterey_roads_sf[1, ],
			altitude = 0,
			clear_previous = TRUE
		),
		"POINT or MULTIPOINT"
	)
	expect_error(
		render_label(
			text = "A",
			location = fixtures$multipoint_sfc,
			z = 1000,
			clear_previous = TRUE
		),
		"exactly one point"
	)
	expect_error(
		render_points(
			location = fixtures$single_sf,
			x = -122,
			altitude = 0,
			clear_previous = TRUE
		),
		"cannot be combined with `x`"
	)
})

test_that("plot_3d location snapshots match goldens", {
	testthat::skip_on_cran()
	skip_if_not_installed("sf")
	skip_if_not_installed("raster")
	skip_if_not_installed("rayvertex")
	skip_if(
		rgl::rgl.useNULL(),
		message = "Software golden snapshots require a live rgl device."
	)

	fixtures = monterey_point_location_fixtures_test()
	cases = renderer_location_cases_test(fixtures, scene = "plot3d")

	for (case_name in names(cases)) {
		expect_location_snapshot_test(
			sprintf("plot3d_%s.png", case_name),
			setup_scene = setup_plot3d_location_scene_test,
			render_call = function() cases[[case_name]]$render(cases[[case_name]]$location)
		)
	}
})

test_that("coord_sf location snapshots match goldens", {
	testthat::skip_on_cran()
	skip_if_not_installed("sf")
	skip_if_not_installed("rayvertex")
	skip_if(
		rgl::rgl.useNULL(),
		message = "Software golden snapshots require a live rgl device."
	)

	fixtures = monterey_point_location_fixtures_test()
	cases = renderer_location_cases_test(fixtures, scene = "plotgg")

	for (case_name in names(cases)) {
		expect_location_snapshot_test(
			sprintf("plotgg_%s.png", case_name),
			setup_scene = setup_plotgg_location_scene_test,
			render_call = function() cases[[case_name]]$render(cases[[case_name]]$location)
		)
	}
})
