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

	sphere_shade(volcano, colorintensity = 50)

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
		sphere_shade(colorintensity = 50) |>
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
