dc_tree_fixture_paths_test = function() {
	fixture_dir = testthat::test_path("fixtures", "spatial")
	list(
		points = file.path(fixture_dir, "dc_tree_points_4326.gpkg"),
		dem = file.path(fixture_dir, "dc_trees_dem_32618.tif")
	)
}

read_dc_tree_points_test = function() {
	sf::st_read(dc_tree_fixture_paths_test()$points, quiet = TRUE)
}

read_dc_tree_dem_test = function() {
	terra::rast(dc_tree_fixture_paths_test()$dem)
}

dc_tree_render_heights_test = function(trees_sf) {
	if (!"HEIGHT" %in% names(trees_sf)) {
		return(rep(12, nrow(trees_sf)))
	}
	heights_m = as.numeric(trees_sf$HEIGHT) * 0.3048
	heights_m[!is.finite(heights_m) | heights_m <= 0] = 6
	heights_m
}

setup_dc_tree_plot3d_scene = function() {
	dem = read_dc_tree_dem_test()
	dem_matrix = raster_to_matrix(dem)
	dem_matrix[dem_matrix < 0] = 0
	scene_zscale = mean(terra::res(dem))
	texture = constant_shade(dem_matrix, color = "#49654b") |>
		add_shadow(lamb_shade(dem_matrix, zscale = 1 / 100), 0.55)
	expect_no_condition(plot_3d_test(
		texture,
		dem,
		zscale = scene_zscale,
		shadow = FALSE,
		water = FALSE,
		solid = TRUE,
		soliddepth = -120,
		solidcolor = "#d9d3c7",
		background = "grey95",
		windowsize = c(500, 420)
	))
	rgl::par3d(ignoreExtent = TRUE)
	render_camera(theta = 58, phi = 28, zoom = 0.10, fov = 48)
	invisible(dem)
}

render_dc_tree_monument_reference_test = function() {
	expect_no_condition(render_multipolygonz(
		washington_monument_multipolygonz,
		heightmap = get_scene_heightmap(),
		zscale = get_scene_zscale(),
		color = "#d8d4ce",
		clear_previous = TRUE
	))
	invisible(NULL)
}

extract_dc_tree_scene_xy_test = function(trees_sf) {
	scene_xy = rayshader:::extract_scene_point_xy(
		location = trees_sf,
		extent = get_scene_extent(),
		heightmap = get_scene_heightmap(),
		caller = "test"
	)
	unname(cbind(scene_xy$x, scene_xy$y))
}

expected_dc_tree_projected_xy_test = function(trees_sf) {
	projected = sf::st_transform(trees_sf, 32618)
	unname(sf::st_coordinates(projected)[, 1:2, drop = FALSE])
}

expected_dc_tree_scene_xyz_test = function(trees_sf) {
	projected_xy = expected_dc_tree_projected_xy_test(trees_sf)
	transform_into_heightmap_coords(
		extent = get_scene_extent(),
		heightmap = get_scene_heightmap(),
		lat = projected_xy[, 2],
		long = projected_xy[, 1],
		altitude = rep(0, nrow(projected_xy)),
		zscale = get_scene_zscale(),
		transform_scene = FALSE,
		caller = "test"
	)
}

compare_dc_tree_snapshot_image_test = function(path1, path2) {
	image1 = png::readPNG(path1)
	image2 = png::readPNG(path2)
	identical(image1, image2)
}

expect_dc_tree_snapshot_test = function(snapshot_name, render_call) {
	if (rgl::cur3d() != 0) {
		rgl::close3d()
	}
	setup_dc_tree_plot3d_scene()
	render_call()
	path = tempfile(fileext = ".png")
	render_snapshot(
		filename = path,
		software_render = TRUE,
		cache_scene = TRUE,
		width = 640,
		height = 420,
		point_radius = 6,
		line_radius = 4,
		fsaa = 1
	)
	expect_snapshot_file(
		path,
		name = snapshot_name,
		compare = compare_dc_tree_snapshot_image_test
	)
	rgl::close3d()
}
