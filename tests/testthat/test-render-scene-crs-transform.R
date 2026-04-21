scene_xy_to_rgl_test = function(long, lat, altitude, extent, zscale) {
	vertex_info = get_ids_with_labels(typeval = "surface_tris")
	ncol_map = vertex_info$ncol[[1]] - 1
	nrow_map = vertex_info$nrow[[1]] - 1
	e = get_extent(extent)
	distances_x = (long - e["xmin"]) / (e["xmax"] - e["xmin"]) * nrow_map + 1
	distances_y = 1 + ncol_map - (lat - e["ymin"]) / (e["ymax"] - e["ymin"]) * ncol_map
	cbind(
		distances_x - nrow_map / 2 - 1,
		as.numeric(altitude) / zscale,
		distances_y - ncol_map / 2 - 1
	)
}

transform_xy_crs_test = function(x, y, source_crs, target_crs) {
	point_sf = sf::st_as_sf(
		data.frame(x = x, y = y),
		coords = c("x", "y"),
		crs = source_crs
	)
	sf::st_coordinates(sf::st_transform(point_sf, target_crs))
}

setup_spatial_plot3d_scene_test = function() {
	elev_raster = suppressWarnings(raster::raster(
		nrows = 20,
		ncols = 20,
		xmn = 0,
		xmx = 1000,
		ymn = 0,
		ymx = 1000,
		crs = "EPSG:3857"
	))
	raster::values(elev_raster) = seq_len(raster::ncell(elev_raster))
	texture = sphere_shade(raster_to_matrix(elev_raster))
	expect_no_condition(plot_3d_test(
		texture,
		elev_raster,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))
	invisible(elev_raster)
}

raybevel_backend_available_test = function() {
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

test_that("plot_3d spatial scenes transform numeric renderers with explicit crs", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raster")
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	setup_spatial_plot3d_scene_test()

	input_x = 0.0045
	input_y = 0.0060
	scene_xy = transform_xy_crs_test(
		x = input_x,
		y = input_y,
		source_crs = sf::st_crs(4326),
		target_crs = get_scene_crs()
	)
	expected_xyz = transform_into_heightmap_coords(
		extent = get_scene_extent(),
		heightmap = get_scene_heightmap(),
		lat = scene_xy[, 2],
		long = scene_xy[, 1],
		altitude = 0,
		zscale = get_scene_zscale(),
		transform_scene = FALSE,
		caller = "test"
	)[1, ]

	expect_no_condition(render_points(
		x = input_x,
		y = input_y,
		crs = sf::st_crs(4326),
		altitude = 0,
		offset = 0,
		size = 4,
		clear_previous = TRUE
	))
	point_ids = get_ids_with_labels()
	point_id = point_ids$id[point_ids$tag == "points3d"][1]
	point_verts = rgl::rgl.attrib(point_id, "vertices")
	expect_equal(unname(point_verts), matrix(expected_xyz, nrow = 1), tolerance = 1e-6)

	expect_no_condition(render_label(
		text = "A",
		x = input_x,
		y = input_y,
		crs = sf::st_crs(4326),
		z = 50,
		clear_previous = TRUE
	))

	mesh = rayvertex::sphere_mesh(radius = 0.1)
	expect_no_condition(render_raymesh(
		mesh,
		x = input_x,
		y = input_y,
		crs = sf::st_crs(4326),
		altitude = 0,
		clear_previous = TRUE
	))
	obj_ids = get_ids_with_labels(typeval = "obj")
	mesh_id = obj_ids$id[grepl("^obj_raymesh", obj_ids$tag)][1]
	mesh_verts = rgl::rgl.attrib(mesh_id, "vertices")
	mesh_center = c(
		mean(range(mesh_verts[, 1])),
		mean(range(mesh_verts[, 2])),
		mean(range(mesh_verts[, 3]))
	)
	expect_equal(mesh_center, as.numeric(expected_xyz), tolerance = 1e-6)

	expect_no_condition(render_obj(
		flag_pole_obj(),
		x = input_x,
		y = input_y,
		crs = sf::st_crs(4326),
		heightmap = get_scene_heightmap(),
		altitude = 0,
		clear_previous = TRUE
	))

	expect_no_condition(render_tree(
		x = input_x,
		y = input_y,
		crs = sf::st_crs(4326),
		heightmap = get_scene_heightmap(),
		tree_height = 5,
		clear_previous = TRUE
	))
})

test_that("plot_3d spatial scenes auto-transform sf line and polygon inputs", {
	skip_if_not_installed("sf")
	skip_if_not_installed("raster")
	skip_if_not_installed("rayrender")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	setup_spatial_plot3d_scene_test()

	line = sf::st_sfc(
		sf::st_linestring(matrix(
			c(
				0.0020, 0.0020,
				0.0075, 0.0080
			),
			ncol = 2,
			byrow = TRUE
		)),
		crs = 4326
	)
	coords = render_path(
		lat = line,
		altitude = 0,
		return_coords = TRUE
	)
	line_scene = sf::st_coordinates(sf::st_transform(line, get_scene_crs()))
	expected_line = transform_into_heightmap_coords(
		extent = get_scene_extent(),
		heightmap = get_scene_heightmap(),
		lat = line_scene[, 2],
		long = line_scene[, 1],
		altitude = c(0, 0),
		zscale = get_scene_zscale(),
		transform_scene = FALSE,
		caller = "test"
	)
	expect_equal(length(coords), 1)
	expect_equal(coords[[1]], expected_line, tolerance = 1e-6)

	polygon = sf::st_sf(
		geometry = sf::st_sfc(
			sf::st_polygon(list(matrix(
				c(
					0.0020, 0.0020,
					0.0055, 0.0020,
					0.0055, 0.0055,
					0.0020, 0.0055,
					0.0020, 0.0020
				),
				ncol = 2,
				byrow = TRUE
			))),
			crs = 4326
		)
	)
	polygon_raycoords = rayshader:::transform_polygon_into_raycoords(
		polygon = polygon,
		heightmap = get_scene_heightmap(),
		e = get_scene_extent(),
		top = 1,
		bottom = 0,
		caller = "test"
	)
	polygon_scene = sf::st_transform(polygon, get_scene_crs())
	vertex_info = get_ids_with_labels(typeval = "surface_tris")
	new_extent = c(
		vertex_info$nrow[[1]] / 2 - 0.5,
		-vertex_info$nrow[[1]] / 2 + 0.5,
		vertex_info$ncol[[1]] / 2 - 0.5,
		-vertex_info$ncol[[1]] / 2 + 0.5
	)
	expected_polygon = rayshader:::transform_polygon_custom_crs(
		polygon_scene,
		get_scene_extent(),
		new_extent
	)
	actual_polygon_coords = as.data.frame(sf::st_coordinates(polygon_raycoords))
	expected_polygon_coords = as.data.frame(sf::st_coordinates(expected_polygon))
	actual_polygon_coords = actual_polygon_coords[order(
		actual_polygon_coords$L2,
		actual_polygon_coords$L1,
		actual_polygon_coords$X,
		actual_polygon_coords$Y
	), ]
	expected_polygon_coords = expected_polygon_coords[order(
		expected_polygon_coords$L2,
		expected_polygon_coords$L1,
		expected_polygon_coords$X,
		expected_polygon_coords$Y
	), ]
	rownames(actual_polygon_coords) = NULL
	rownames(expected_polygon_coords) = NULL
	expect_equal(actual_polygon_coords, expected_polygon_coords, tolerance = 1e-6)

	expect_no_condition(render_polygons(
		polygon,
		top = 1,
		bottom = 0,
		heightmap = get_scene_heightmap(),
		clear_previous = TRUE
	))

	if (
		requireNamespace("raybevel", quietly = TRUE) &&
			requireNamespace("rayvertex", quietly = TRUE) &&
			raybevel_backend_available_test()
	) {
		expect_no_condition(render_buildings(
			polygon = polygon,
			heightmap = get_scene_heightmap(),
			roof_height = 10,
			base_height = 0,
			relative_heights = TRUE,
			heights_relative_to_centroid = TRUE,
			clear_previous = TRUE
		))
		expect_no_condition(render_beveled_polygons(
			polygon = polygon,
			heightmap = get_scene_heightmap(),
			bevel_height = 10,
			base_height = 0,
			heights_relative_to_centroid = TRUE,
			clear_previous = TRUE
		))
	}

	multipolygonz = sf::st_sf(
		geometry = sf::st_sfc(
			sf::st_multipolygon(list(list(matrix(
				c(
					0.0030, 0.0030, 15,
					0.0040, 0.0030, 15,
					0.0040, 0.0040, 15,
					0.0030, 0.0040, 15,
					0.0030, 0.0030, 15
				),
				ncol = 3,
				byrow = TRUE
			)))),
			crs = 4326
		)
	)
	expect_no_condition(render_multipolygonz(
		multipolygonz,
		heightmap = get_scene_heightmap(),
		clear_previous = TRUE
	))
})

test_that("coord_sf scenes transform numeric renderer coordinates with explicit crs", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	centroid = suppressWarnings(sf::st_coordinates(sf::st_centroid(nc[1, ])))
	p = ggplot2::ggplot(nc) +
		ggplot2::geom_sf() +
		ggplot2::coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 3,
		height = 3,
		windowsize = c(600, 600),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	expect_no_condition(render_points(
		x = centroid[1, 1],
		y = centroid[1, 2],
		crs = sf::st_crs(nc),
		altitude = 0,
		offset = 0,
		size = 4,
		clear_previous = TRUE
	))
	point_ids = get_ids_with_labels()
	point_id = point_ids$id[point_ids$tag == "points3d"][1]
	point_verts = rgl::rgl.attrib(point_id, "vertices")
	expected_scene_xy = rayshader:::transform_ggplot_coords(
		x = centroid[1, 1],
		y = centroid[1, 2],
		crs = sf::st_crs(nc)
	)
	expected_xyz = scene_xy_to_rgl_test(
		long = expected_scene_xy$long,
		lat = expected_scene_xy$lat,
		altitude = 0,
		extent = attr(expected_scene_xy, "extent"),
		zscale = get_scene_effective_zscale()
	)
	expect_equal(unname(point_verts), unname(expected_xyz), tolerance = 1e-6)

	centroid_ll = suppressWarnings(sf::st_coordinates(
		sf::st_transform(sf::st_centroid(nc[1, ]), 4326)
	))
	expect_no_condition(render_points(
		long = centroid_ll[1, 1],
		lat = centroid_ll[1, 2],
		altitude = 0,
		offset = 0,
		size = 4,
		clear_previous = TRUE
	))
	point_ids = get_ids_with_labels()
	point_id = point_ids$id[point_ids$tag == "points3d"][1]
	point_verts = rgl::rgl.attrib(point_id, "vertices")
	expected_scene_xy = rayshader:::transform_ggplot_coords(
		x = centroid_ll[1, 1],
		y = centroid_ll[1, 2],
		crs = 4326
	)
	expected_xyz = scene_xy_to_rgl_test(
		long = expected_scene_xy$long,
		lat = expected_scene_xy$lat,
		altitude = 0,
		extent = attr(expected_scene_xy, "extent"),
		zscale = get_scene_effective_zscale()
	)
	expect_equal(unname(point_verts), unname(expected_xyz), tolerance = 1e-6)

	expect_no_condition(render_label(
		text = "A",
		x = centroid[1, 1],
		y = centroid[1, 2],
		crs = sf::st_crs(nc),
		z = 100,
		clear_previous = TRUE
	))
})

test_that("coord_sf faceted scenes still require panel and reject conflicting crs", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:4, ]
	nc$facet = rep(c("a", "b"), each = 2)
	panel_line = suppressWarnings(sf::st_coordinates(sf::st_centroid(
		nc[nc$facet == "a", ]
	)))
	line = sf::st_sf(
		geometry = sf::st_sfc(
			sf::st_linestring(panel_line[, 1:2]),
			crs = sf::st_crs(nc)
		)
	)
	p = ggplot2::ggplot(nc) +
		ggplot2::geom_sf() +
		ggplot2::facet_wrap(~facet) +
		ggplot2::coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 5,
		height = 3,
		windowsize = c(900, 700),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	expect_error(
		render_path(
			x = panel_line[, 1],
			y = panel_line[, 2],
			crs = sf::st_crs(nc),
			altitude = 0,
			return_coords = TRUE
		),
		"Supply `panel = <panel>`"
	)

	panel_coords = render_path(
		x = panel_line[, 1],
		y = panel_line[, 2],
		crs = sf::st_crs(nc),
		altitude = 0,
		panel = 1,
		return_coords = TRUE
	)
	expected_scene_xy = rayshader:::transform_ggplot_coords(
		x = panel_line[, 1],
		y = panel_line[, 2],
		crs = sf::st_crs(nc),
		panel = 1
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
	expect_equal(panel_coords[[1]], expected_coords, tolerance = 1e-6)

	expect_error(
		render_path(
			lat = line,
			crs = sf::st_crs(3857),
			panel = 1,
			altitude = 0,
			return_coords = TRUE
		),
		"conflicts with `crs`"
	)
})

test_that("scenes without CRS metadata leave inputs unchanged", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = matrix(1, nrow = 20, ncol = 20)
	texture = sphere_shade(heightmap)
	scene_extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)

	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		extent = scene_extent,
		zscale = 1,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(250, 250)
	))

	expected_numeric = transform_into_heightmap_coords(
		extent = scene_extent,
		heightmap = heightmap,
		lat = 10,
		long = 5,
		altitude = 0,
		zscale = get_scene_zscale(),
		transform_scene = FALSE,
		caller = "test"
	)
	actual_numeric = transform_into_heightmap_coords(
		extent = scene_extent,
		heightmap = heightmap,
		lat = 10,
		long = 5,
		altitude = 0,
		crs = sf::st_crs(4326),
		zscale = get_scene_zscale(),
		caller = "test"
	)
	expect_equal(actual_numeric, expected_numeric, tolerance = 1e-6)

	line = sf::st_sfc(
		sf::st_linestring(matrix(
			c(
				2, 2,
				5, 5
			),
			ncol = 2,
			byrow = TRUE
		)),
		crs = 4326
	)
	coords = render_path(
		lat = line,
		altitude = 0,
		extent = scene_extent,
		heightmap = heightmap,
		return_coords = TRUE,
		clear_previous = TRUE
	)
	expected_line = transform_into_heightmap_coords(
		extent = scene_extent,
		heightmap = heightmap,
		lat = c(2, 5),
		long = c(2, 5),
		altitude = c(0, 0),
		zscale = get_scene_zscale(),
		transform_scene = FALSE,
		caller = "test"
	)
	expect_equal(coords[[1]], expected_line, tolerance = 1e-6)

	expect_no_condition(render_points(
		x = 5,
		y = 10,
		crs = sf::st_crs(4326),
		altitude = 0,
		extent = scene_extent,
		heightmap = heightmap,
		clear_previous = TRUE
	))
})
