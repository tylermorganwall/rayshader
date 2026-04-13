clear_hillshade_test_cache = function() {
	clear_hillshade_cache()
	reset_scene_context(
		clear_scene_metadata = TRUE,
		clear_scene_cache = TRUE
	)
	invisible(NULL)
}

test_that("hillshade functions reuse cached heightmap", {
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	sphere_shade(volcano, vertical_exaggeration = 50)

	expect_equal(get_hillshade_heightmap(), volcano)
	expect_null(get_hillshade_zscale(default = NULL))
	expect_equal(
		height_shade(),
		height_shade(volcano)
	)
	expect_equal(
		constant_shade(color = "red", alpha = 0.5),
		constant_shade(volcano, color = "red", alpha = 0.5)
	)
	expect_equal(
		texture_shade(detail = 0.25, contrast = 2, brightness = 1),
		texture_shade(volcano, detail = 0.25, contrast = 2, brightness = 1)
	)
})

test_that("hillshade map functions cache the latest map texture", {
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	sphere_map = sphere_shade(volcano, vertical_exaggeration = 50)
	expect_equal(get_hillshade_map(), sphere_map)

	height_map = height_shade(volcano)
	expect_equal(get_hillshade_map(), height_map)

	constant_map = constant_shade(volcano, color = "red", alpha = 0.5)
	expect_equal(get_hillshade_map(), constant_map)

	overlay_map = add_overlay(
		sphere_map,
		height_shade(volcano),
		alphalayer = 0.5
	)
	expect_equal(get_hillshade_map(), overlay_map)

	shadow_map = add_shadow(
		sphere_map,
		ray_shade(volcano, zscale = 50, maxsearch = 10, sunaltitude = 25),
		max_darken = 0.5
	)
	expect_equal(get_hillshade_map(), shadow_map)

	volcano_water = volcano
	volcano_water[volcano_water < mean(volcano_water)] = mean(volcano_water)
	water_map = add_water(
		sphere_map,
		detect_water(volcano_water, min_area = 25),
		color = "desert"
	)
	expect_equal(get_hillshade_map(), water_map)
})

test_that("new explicit 2D heightmap invalidates stale cached hillshade zscale", {
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	ray_shade(volcano, zscale = 50, sunaltitude = 25, sunangle = 225)
	expect_equal(get_hillshade_zscale(), 50)

	height_shade(volcano)
	expect_null(get_hillshade_zscale(default = NULL))

	texture_shade(volcano, detail = 0.25, contrast = 2, brightness = 1)
	expect_null(get_hillshade_zscale(default = NULL))

	raster_heightmap = raster::raster(volcano)
	raster::res(raster_heightmap) = c(30, 30)
	sphere_shade(raster_heightmap)
	expect_equal(get_hillshade_zscale(), 30)
})

test_that("sphere_shade explicit zscale overrides and caches hillshade zscale", {
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	sphere_shade(volcano, zscale = 12)
	expect_equal(get_hillshade_zscale(), 12)

	sphere_shade(vertical_exaggeration = 5)
	expect_equal(get_hillshade_zscale(), 12)

	sphere_shade(zscale = 8)
	expect_equal(get_hillshade_zscale(), 8)
})

test_that("vertical_exaggeration is one-off for cached hillshade and scene zscale", {
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	expect_equal(
		sphere_shade(volcano, zscale = 12, vertical_exaggeration = 2),
		sphere_shade(volcano, zscale = 6)
	)
	expect_equal(get_hillshade_zscale(), 6)

	clear_hillshade_test_cache()
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	hillshade = sphere_shade(volcano)
	expect_no_condition(plot_3d_test(
		hillshade,
		heightmap = volcano,
		zscale = 50,
		vertical_exaggeration = 2,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(200, 200)
	))
	expect_equal(get_hillshade_zscale(), 50)
	expect_equal(get_scene_zscale(), 25)
})

test_that("ray based hillshade functions reuse cached zscale", {
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	sphere_shade(volcano)

	expect_equal(
		ray_shade(
			zscale = 50,
			sunaltitude = 25,
			sunangle = 225,
			maxsearch = 10
		),
		ray_shade(
			volcano,
			zscale = 50,
			sunaltitude = 25,
			sunangle = 225,
			maxsearch = 10
		)
	)
	expect_equal(get_hillshade_zscale(), 50)
	expect_equal(
		lamb_shade(sunaltitude = 25, sunangle = 225),
		lamb_shade(volcano, sunaltitude = 25, sunangle = 225, zscale = 50)
	)
	expect_equal(
		ambient_shade(
			sunbreaks = 3,
			maxsearch = 10,
			anglebreaks = seq(10, 20, by = 5)
		),
		ambient_shade(
			volcano,
			sunbreaks = 3,
			maxsearch = 10,
			anglebreaks = seq(10, 20, by = 5),
			zscale = 50
		)
	)
})

test_that("plot_3d uses cached hillshade heightmap and zscale", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	withr::local_options(list(rayshader.verbose_scene_cache = TRUE))
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	hillshade = volcano |>
		sphere_shade(vertical_exaggeration = 50) |>
		add_overlay(height_shade(), 0.5) |>
		add_shadow(
			ray_shade(zscale = 50, maxsearch = 10, sunaltitude = 25),
			0.5
		) |>
		add_shadow(
			ambient_shade(
				sunbreaks = 3,
				maxsearch = 10,
				anglebreaks = seq(10, 20, by = 5)
			),
			0.2
		)

	out = character()
	expect_no_error(withCallingHandlers(
		plot_3d_test(
			hillshade,
			shadow = FALSE,
			water = FALSE,
			windowsize = c(200, 200)
		),
		message = function(cnd) {
			out <<- c(out, conditionMessage(cnd))
			invokeRestart("muffleMessage")
		}
	))

	expect_true(any(grepl("hillshade_heightmap", out, fixed = TRUE)))
	expect_true(any(grepl("hillshade_zscale", out, fixed = TRUE)))
	expect_equal(get_scene_heightmap(), volcano)
	expect_equal(get_scene_zscale(), 50)
	expect_equal(get_hillshade_heightmap(), volcano)
	expect_equal(get_hillshade_zscale(), 50)
})

test_that("plot_3d explicit matrix heightmap does not reuse stale cached zscale", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	hillshade = sphere_shade(volcano)
	expect_no_condition(plot_3d_test(
		hillshade,
		heightmap = volcano,
		zscale = 3,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(200, 200)
	))
	expect_equal(get_hillshade_zscale(), 3)

	rgl::close3d()

	expect_no_condition(plot_3d_test(
		hillshade,
		heightmap = volcano,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(200, 200)
	))
	expect_equal(get_scene_zscale(), 1)
	expect_equal(get_hillshade_zscale(), 1)
})

test_that("radiance_shade reuses cached 2D hillshade state without a scene", {
	skip_if_not_installed("rayrender")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	withr::local_options(list(rayshader.verbose_scene_cache = TRUE))
	clear_hillshade_test_cache()
	withr::defer(clear_hillshade_test_cache())

	sphere_map = sphere_shade(volcano, vertical_exaggeration = 50)
	rgl::close3d()

	out = character()
	radiance_error = NULL
	expect_no_error(withCallingHandlers(
		tryCatch(
			radiance_shade(samples = 1, light = FALSE, shadow = FALSE),
			error = function(e) {
				radiance_error <<- conditionMessage(e)
				NULL
			}
		),
		message = function(cnd) {
			out <<- c(out, conditionMessage(cnd))
			invokeRestart("muffleMessage")
		}
	))
	expect_true(any(grepl("hillshade_heightmap", out, fixed = TRUE)))
	expect_true(any(grepl("hillshade_map", out, fixed = TRUE)))
	if (!is.null(radiance_error)) {
		expect_false(grepl(
			"No rgl window currently open and no `heightmap` supplied",
			radiance_error,
			fixed = TRUE
		))
	}

	out = character()
	radiance_error = NULL
	expect_no_error(withCallingHandlers(
		tryCatch(
			radiance_shade(
				texture = sphere_map,
				samples = 1,
				light = FALSE,
				shadow = FALSE
			),
			error = function(e) {
				radiance_error <<- conditionMessage(e)
				NULL
			}
		),
		message = function(cnd) {
			out <<- c(out, conditionMessage(cnd))
			invokeRestart("muffleMessage")
		}
	))
	expect_true(any(grepl("hillshade_heightmap", out, fixed = TRUE)))
	expect_false(any(grepl("hillshade_map", out, fixed = TRUE)))
	if (!is.null(radiance_error)) {
		expect_false(grepl(
			"No rgl window currently open and no `heightmap` supplied",
			radiance_error,
			fixed = TRUE
		))
	}
})
