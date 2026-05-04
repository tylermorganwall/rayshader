raybevel_backend_available = function() {
	if (
		!requireNamespace("raybevel", quietly = TRUE) ||
			!requireNamespace("rayvertex", quietly = TRUE)
	) {
		return(FALSE)
	}
	test_square = matrix(
		c(0, 0, 10, 0, 10, 10, 0, 10),
		ncol = 2,
		byrow = TRUE
	)
	tryCatch(
		{
			sk = raybevel::skeletonize(test_square)
			mat = rayvertex::material_list(diffuse = "grey50")
			raybevel::generate_roof(
				sk,
				max_height = 1,
				base_height = 0,
				vertical_offset = 1,
				material = mat,
				roof_material = mat,
				sides = TRUE,
				base = TRUE
			)
			TRUE
		},
		error = function(e) FALSE
	)
}

test_that("get_skeleton_source_indices() handles single and multi skeleton outputs", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raybevel")

	single_square = matrix(
		c(0, 0, 10, 0, 10, 10, 0, 10),
		ncol = 2,
		byrow = TRUE
	)
	single_sk = raybevel::skeletonize(single_square)
	expect_equal(get_skeleton_source_indices(single_sk), 1L)

	poly_sf = sf::st_sf(
		geometry = sf::st_sfc(
			sf::st_polygon(list(rbind(
				c(0, 0),
				c(10, 0),
				c(10, 10),
				c(0, 10),
				c(0, 0)
			))),
			sf::st_polygon(list(rbind(
				c(20, 20),
				c(30, 20),
				c(30, 30),
				c(20, 30),
				c(20, 20)
			)))
		)
	)
	list_sk = raybevel::skeletonize(poly_sf)
	expect_equal(get_skeleton_source_indices(list_sk), c(1L, 2L))
})

test_that("render_beveled_polygons() and render_buildings() work for single polygons", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raybevel")
	skip_if_not(raybevel_backend_available())
	on.exit(rgl::close3d(), add = TRUE)

	heightmap = volcano
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		zscale = 1,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(400, 400)
	))
	extent = c(
		xmin = 0,
		xmax = nrow(heightmap),
		ymin = 0,
		ymax = ncol(heightmap)
	)
	polygon = sf::st_sf(
		geometry = sf::st_sfc(sf::st_polygon(list(matrix(
			c(10, 10, 20, 10, 20, 20, 10, 20, 10, 10),
			ncol = 2,
			byrow = TRUE
		))))
	)

	expect_no_condition(render_beveled_polygons(
		polygon = polygon,
		extent = extent,
		material = "grey40",
		bevel_material = "orange",
		bevel_height = 10,
		base_height = 0,
		bevel_width = 0.2,
		width_raw_units = TRUE,
		angle = 45,
		heights_relative_to_centroid = FALSE,
		clear_previous = TRUE
	))

	expect_no_condition(render_buildings(
		polygon = polygon,
		extent = extent,
		material = "grey40",
		roof_material = "orange",
		roof_height = 10,
		base_height = 0,
		relative_heights = TRUE,
		heightmap = volcano,
		heights_relative_to_centroid = TRUE,
		clear_previous = TRUE
	))
})

test_that("render_polygons() warns and combines scale_data with vertical_exaggeration", {
	skip_if_not_installed("sf")
	skip_if_not_installed("rayrender")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = matrix(0, nrow = 10, ncol = 10)
	expect_no_condition(plot_3d_test(
		sphere_shade(heightmap),
		heightmap,
		zscale = 1,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(100, 100)
	))
	polygon = sf::st_sf(
		value = 10,
		geometry = sf::st_sfc(sf::st_polygon(list(matrix(
			c(2, 2, 4, 2, 4, 4, 2, 4, 2, 2),
			ncol = 2,
			byrow = TRUE
		))))
	)

	expect_warning(
		render_polygons(
			polygon = polygon,
			extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
			data_column_top = "value",
			bottom = 0,
			zscale = 10,
			vertical_exaggeration = 2,
			scale_data = 3,
			clear_previous = TRUE
		),
		"both scale vertical data values"
	)

	polygon_ids = get_ids_with_labels()$id[get_ids_with_labels()$tag == "polygon3d"]
	polygon_vertices = rgl::rgl.attrib(polygon_ids[length(polygon_ids)], "vertices")
	expect_equal(range(polygon_vertices[, 2]), c(0, 6), tolerance = 1e-8)
})

test_that("raybevel polygon renderers warn when scale_data and vertical_exaggeration are both supplied", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raybevel")
	skip_if_not(raybevel_backend_available())
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = matrix(0, nrow = 10, ncol = 10)
	expect_no_condition(plot_3d_test(
		sphere_shade(heightmap),
		heightmap,
		zscale = 1,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(100, 100)
	))
	polygon = sf::st_sf(
		value = 10,
		geometry = sf::st_sfc(sf::st_polygon(list(matrix(
			c(2, 2, 4, 2, 4, 4, 2, 4, 2, 2),
			ncol = 2,
			byrow = TRUE
		))))
	)

	expect_warning(
		render_buildings(
			polygon = polygon,
			extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
			data_column_top = "value",
			base_height = 0,
			zscale = 10,
			vertical_exaggeration = 2,
			scale_data = 3,
			heights_relative_to_centroid = FALSE,
			clear_previous = TRUE
		),
		"both scale vertical data values"
	)
	expect_warning(
		render_beveled_polygons(
			polygon = polygon,
			extent = c(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
			data_column_top = "value",
			base_height = 0,
			bevel_width = 0.2,
			width_raw_units = TRUE,
			zscale = 10,
			vertical_exaggeration = 2,
			scale_data = 3,
			heights_relative_to_centroid = FALSE,
			clear_previous = TRUE
		),
		"both scale vertical data values"
	)
})
