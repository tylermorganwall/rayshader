test_that("render_tree() transforms DC WGS84 points into the projected scene CRS", {
	skip_if_not_installed("sf")
	skip_if_not_installed("terra")
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	trees_sf = read_dc_tree_points_test()
	expect_true(rayshader:::scene_crs_equal(
		sf::st_crs(trees_sf),
		sf::st_crs(4326)
	))

	setup_dc_tree_plot3d_scene()

	expect_false(isTRUE(sf::st_is_longlat(get_scene_crs())))
	expect_true(grepl("zone 18", get_scene_crs()$input, fixed = TRUE))
	scene_extent = rayshader:::get_extent(get_scene_extent())
	expect_gt(scene_extent["xmax"] - scene_extent["xmin"], 3500)
	expect_gt(scene_extent["ymax"] - scene_extent["ymin"], 3500)

	point_input = rayshader:::resolve_render_location_input(
		location = trees_sf,
		extent = get_scene_extent(),
		heightmap = get_scene_heightmap(),
		caller = "test"
	)
	expected_projected_xy = expected_dc_tree_projected_xy_test(trees_sf)
	actual_projected_xy = unname(cbind(point_input$x, point_input$y))
	expect_true(isTRUE(point_input$location_supplied))
	expect_true(isTRUE(point_input$transformed))
	expect_equal(actual_projected_xy, expected_projected_xy, tolerance = 1e-8)

	actual_scene_xy = extract_dc_tree_scene_xy_test(trees_sf)
	expect_equal(actual_scene_xy, expected_projected_xy, tolerance = 1e-8)

	expected_scene_xyz = expected_dc_tree_scene_xyz_test(trees_sf)
	actual_scene_xyz = transform_into_heightmap_coords(
		extent = point_input$extent,
		heightmap = get_scene_heightmap(),
		lat = point_input$y,
		long = point_input$x,
		altitude = rep(0, nrow(actual_projected_xy)),
		zscale = get_scene_zscale(),
		transform_scene = FALSE,
		caller = "test"
	)
	expect_equal(actual_scene_xyz, expected_scene_xyz, tolerance = 1e-6)

	expect_no_condition(render_tree(
		location = trees_sf,
		tree_height = dc_tree_render_heights_test(trees_sf),
		tree_zscale = TRUE,
		zscale = get_scene_zscale(),
		crown_width_ratio = 0.8,
		crown_color = "#52734d",
		trunk_color = "#714f32",
		clear_previous = TRUE
	))
	expect_true(any(get_ids_with_labels()$tag == "objtree"))
})

test_that("render_tree() DC tree scene software snapshot matches the golden", {
	testthat::skip_on_cran()
	skip_if_not_installed("sf")
	skip_if_not_installed("terra")
	skip_if_not_installed("rayvertex")
	skip_if(
		rgl::rgl.useNULL(),
		message = "Software golden snapshots require a live rgl device."
	)

	trees_sf = read_dc_tree_points_test()
	expect_dc_tree_snapshot_test(
		"plot3d_render_tree_dc.png",
		render_call = function() {
			render_dc_tree_monument_reference_test()
			expect_true(any(get_ids_with_labels()$tag == "obj_multipolygon"))
			render_tree(
				location = trees_sf,
				tree_height = dc_tree_render_heights_test(trees_sf),
				tree_zscale = TRUE,
				zscale = get_scene_zscale(),
				crown_width_ratio = 0.8,
				crown_color = "#52734d",
				trunk_color = "#714f32",
				clear_previous = TRUE
			)
			expect_true(any(get_ids_with_labels()$tag == "objtree"))
		}
	)
})
