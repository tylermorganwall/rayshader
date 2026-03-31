library(ggplot2)

scene_xy_to_rgl = function(long, lat, altitude, extent, zscale) {
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

test_that("render_points() auto-transforms ggplot scene coordinates", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	p = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		scale_x_log10()

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 3,
		height = 3,
		windowsize = c(600, 600),
		raytrace = FALSE,
		shadow = FALSE
	)))

	expect_no_condition(render_points(
		x = c(2, 4),
		y = c(15, 30),
		altitude = c(100, 200),
		offset = 0,
		color = "red",
		size = 4,
		clear_previous = TRUE
	))

	ids = get_ids_with_labels()
	point_id = ids$id[ids$tag == "points3d"][1]
	point_verts = rgl::rgl.attrib(point_id, "vertices")
	pts = rayshader:::transform_ggplot_coords(x = c(2, 4), y = c(15, 30))
	expected_xyz = scene_xy_to_rgl(
		long = pts$long,
		lat = pts$lat,
		altitude = c(100, 200),
		extent = attr(pts, "extent"),
		zscale = get_scene_zscale()
	)

	expect_equal(unname(point_verts), expected_xyz, tolerance = 1e-6)
})

test_that("render_label() auto-transforms ggplot scene coordinates", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	p = ggplot(mtcars) +
		geom_point(aes(wt, mpg)) +
		scale_x_log10()

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 3,
		height = 3,
		windowsize = c(600, 600),
		raytrace = FALSE,
		shadow = FALSE
	)))

	expect_no_condition(render_label(
		text = "A",
		x = 2,
		y = 15,
		z = 100,
		clear_previous = TRUE
	))

	ids = get_ids_with_labels(typeval = c("raytext", "textline"))
	expect_gt(nrow(ids), 0)
})

test_that("render_path() auto-transforms sf input through coord_sf()", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	centroids = suppressWarnings(sf::st_coordinates(sf::st_centroid(nc[1:2, ])))
	line = sf::st_sf(
		geometry = sf::st_sfc(
			sf::st_linestring(centroids[, 1:2]),
			crs = sf::st_crs(nc)
		)
	)

	p = ggplot(nc) +
		geom_sf() +
		coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 3,
		height = 3,
		windowsize = c(600, 600),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	coords = render_path(
		lat = line,
		altitude = 0,
		return_coords = TRUE
	)

	line_coords = sf::st_coordinates(line)
	pts = rayshader:::transform_ggplot_coords(
		x = line_coords[, 1],
		y = line_coords[, 2],
		crs = sf::st_crs(line)
	)
	expected_xyz = scene_xy_to_rgl(
		long = pts$long,
		lat = pts$lat,
		altitude = c(0, 0),
		extent = attr(pts, "extent"),
		zscale = get_scene_zscale()
	)

	expect_equal(length(coords), 1)
	expect_equal(coords[[1]], expected_xyz, tolerance = 1e-6)
})

test_that("polygon raycoords auto-transform ggplot scene sf input", {
	skip_if_not_installed("sf")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	nc = sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
	p = ggplot(nc) +
		geom_sf() +
		coord_sf(crs = sf::st_crs(32119))

	expect_no_condition(suppressWarnings(plot_gg_test(
		p,
		width = 3,
		height = 3,
		windowsize = c(600, 600),
		raytrace = FALSE,
		shadow = FALSE,
		multicore = FALSE
	)))

	poly_in = nc[1, ]
	scene_extent = rayshader:::get_ggplot_extent(panel = 1)
	poly_out = rayshader:::transform_polygon_into_raycoords(
		polygon = poly_in,
		heightmap = get_scene_heightmap(),
		e = scene_extent,
		top = 1,
		bottom = 0
	)

	poly_manual = rayshader:::transform_ggplot_sf(poly_in, panel = 1)
	vertex_info = get_ids_with_labels(typeval = "surface_tris")
	new_extent = c(
		vertex_info$nrow[[1]] / 2 - 0.5,
		-vertex_info$nrow[[1]] / 2 + 0.5,
		vertex_info$ncol[[1]] / 2 - 0.5,
		-vertex_info$ncol[[1]] / 2 + 0.5
	)
	expected_poly = rayshader:::transform_polygon_custom_crs(
		poly_manual,
		attr(poly_manual, "extent"),
		new_extent
	)

	expect_equal(
		sf::st_coordinates(poly_out),
		sf::st_coordinates(expected_poly),
		tolerance = 1e-6
	)
	expect_equal(poly_out$top, 1)
	expect_equal(poly_out$bottom, 0)
})
