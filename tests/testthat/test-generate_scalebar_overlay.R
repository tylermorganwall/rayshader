test_that("generate_scalebar_overlay() infers latlong from spatial heightmaps", {
	testthat::skip_if_not_installed("terra")

	lonlat_rast = terra::rast(
		nrows = 2,
		ncols = 2,
		xmin = -1,
		xmax = 1,
		ymin = 35,
		ymax = 37,
		crs = "EPSG:4326"
	)
	projected_rast = terra::rast(
		nrows = 2,
		ncols = 2,
		xmin = 0,
		xmax = 1000,
		ymin = 0,
		ymax = 1000,
		crs = "EPSG:3857"
	)

	expect_true(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = FALSE,
		heightmap = lonlat_rast,
		caller = "generate_scalebar_overlay"
	))
	expect_false(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = TRUE,
		heightmap = projected_rast,
		caller = "generate_scalebar_overlay"
	))
})

test_that("generate_scalebar_overlay() infers latlong from explicit spatial extent", {
	testthat::skip_if_not_installed("sf")

	heightmap = matrix(1, nrow = 2, ncol = 2)
	lonlat_extent = structure(
		c(xmin = -1, ymin = 35, xmax = 1, ymax = 37),
		crs = sf::st_crs(4326),
		class = "bbox"
	)
	projected_extent = structure(
		c(xmin = 0, ymin = 0, xmax = 1000, ymax = 1000),
		crs = sf::st_crs(3857),
		class = "bbox"
	)

	expect_true(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = FALSE,
		extent = lonlat_extent,
		heightmap = heightmap,
		caller = "generate_scalebar_overlay"
	))
	expect_false(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = TRUE,
		extent = projected_extent,
		heightmap = heightmap,
		caller = "generate_scalebar_overlay"
	))
})

test_that("generate_scalebar_overlay() uses cached spatial metadata before latlong", {
	testthat::skip_if_not_installed("sf")
	rayshader:::clear_hillshade_cache()
	rayshader:::reset_scene_context(
		clear_scene_metadata = TRUE,
		clear_scene_cache = TRUE
	)
	withr::defer({
		rayshader:::clear_hillshade_cache()
		rayshader:::reset_scene_context(
			clear_scene_metadata = TRUE,
			clear_scene_cache = TRUE
		)
	})

	heightmap = matrix(1, nrow = 2, ncol = 2)

	rayshader:::cache_hillshade_crs(sf::st_crs(4326), label = "test")
	expect_true(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = FALSE,
		heightmap = heightmap,
		caller = "generate_scalebar_overlay"
	))

	rayshader:::cache_hillshade_crs(sf::st_crs(3857), label = "test")
	expect_false(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = TRUE,
		heightmap = heightmap,
		caller = "generate_scalebar_overlay"
	))
})

test_that("generate_scalebar_overlay() only falls back to latlong without spatial metadata", {
	rayshader:::clear_hillshade_cache()
	rayshader:::reset_scene_context(
		clear_scene_metadata = TRUE,
		clear_scene_cache = TRUE
	)
	withr::defer({
		rayshader:::clear_hillshade_cache()
		rayshader:::reset_scene_context(
			clear_scene_metadata = TRUE,
			clear_scene_cache = TRUE
		)
	})

	heightmap = matrix(1, nrow = 2, ncol = 2)
	expect_true(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = TRUE,
		heightmap = heightmap,
		caller = "generate_scalebar_overlay"
	))
	expect_false(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = NA,
		heightmap = heightmap,
		caller = "generate_scalebar_overlay"
	))
	expect_false(rayshader:::resolve_scalebar_overlay_latlong(
		latlong = TRUE,
		heightmap = NULL,
		caller = "generate_scalebar_overlay"
	))
})
