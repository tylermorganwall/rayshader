compare_bryce_snapshot_image_test = function(path1, path2) {
	image1 = png::readPNG(path1)
	image2 = png::readPNG(path2)
	identical(image1, image2)
}

bryce_raybevel_backend_available_test = function() {
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

load_bryce_building_scene_fixtures_test = function(building_count = 20, buffer_m = 40) {
	zip_path = testthat::test_path("fixtures/spatial/Bryce_Canyon_GeoTIFF.zip")
	buildings_path = testthat::test_path("fixtures/spatial/bryce_buildings_4326.gpkg")
	zip_info = utils::unzip(
		zip_path,
		list = TRUE
	)
	tif_name = zip_info$Name[grepl("\\.tif$", zip_info$Name)][1]
	tif_path = utils::unzip(
		zip_path,
		files = tif_name,
		exdir = tempdir(),
		overwrite = TRUE
	)
	bryce_raster = raster::raster(tif_path)
	raster_crs = sf::st_crs(raster::crs(bryce_raster))
	buildings = sf::st_read(buildings_path, quiet = TRUE)
	buildings_utm = sf::st_transform(buildings, raster_crs)
	raster_bbox = sf::st_as_sfc(sf::st_bbox(
		c(
			xmin = raster::xmin(bryce_raster),
			xmax = raster::xmax(bryce_raster),
			ymin = raster::ymin(bryce_raster),
			ymax = raster::ymax(bryce_raster)
		),
		crs = raster_crs
	))
	inside_raster = lengths(sf::st_intersects(buildings_utm, raster_bbox)) > 0
	buildings_utm = buildings_utm[inside_raster & !is.na(buildings_utm$height_m), ]
	centroids = suppressWarnings(sf::st_coordinates(sf::st_centroid(buildings_utm)))
	raster_center = c(
		mean(c(raster::xmin(bryce_raster), raster::xmax(bryce_raster))),
		mean(c(raster::ymin(bryce_raster), raster::ymax(bryce_raster)))
	)
	center_distance = (centroids[, 1] - raster_center[1])^2 +
		(centroids[, 2] - raster_center[2])^2
	selected_order = order(center_distance, centroids[, 1], centroids[, 2])
	buildings_utm = buildings_utm[selected_order[seq_len(building_count)], ]
	aoi = sf::st_buffer(sf::st_as_sfc(sf::st_bbox(buildings_utm)), buffer_m)
	raster_crop = raster::crop(bryce_raster, methods::as(aoi, "Spatial"))
	list(
		raster_crop = raster_crop,
		heightmap = raster_to_matrix(raster_crop, verbose = FALSE),
		buildings_4326 = sf::st_transform(buildings_utm, 4326),
		buildings_utm = buildings_utm
	)
}

bryce_building_hillshade_test = function(fixtures) {
	base_map = sphere_shade(fixtures$heightmap)
	footprint_outline = generate_polygon_overlay(
		geometry = fixtures$buildings_utm,
		extent = fixtures$raster_crop,
		heightmap = fixtures$heightmap,
		palette = NA,
		linecolor = "#c1121f",
		linewidth = 18
	)
	add_overlay(base_map, footprint_outline, rescale_original = TRUE)
}

setup_bryce_buildings_plot3d_scene_test = function(fixtures = NULL) {
	if (is.null(fixtures)) {
		fixtures = load_bryce_building_scene_fixtures_test()
	}
	expect_no_condition(plot_3d_test(
		bryce_building_hillshade_test(fixtures),
		fixtures$raster_crop,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(420, 420)
	))
	render_camera(theta = 35, phi = 40, zoom = 0.7, fov = 0)
	invisible(fixtures)
}

expect_bryce_buildings_snapshot_test = function(snapshot_name, render_call) {
	if (rgl::cur3d() != 0) {
		rgl::close3d()
	}
	fixtures = setup_bryce_buildings_plot3d_scene_test()
	render_call(fixtures)
	path = tempfile(fileext = ".png")
	render_snapshot(
		filename = path,
		software_render = TRUE,
		cache_scene = TRUE,
		width = 420,
		height = 420,
		point_radius = 6,
		line_radius = 4,
		fsaa = 1
	)
	testthat::expect_snapshot_file(
		path,
		name = snapshot_name,
		compare = compare_bryce_snapshot_image_test
	)
	rgl::close3d()
}
