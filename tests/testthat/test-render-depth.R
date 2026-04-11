test_that("render_depth is quiet by default when focus is auto-derived", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	testthat::local_mocked_bindings(
		render_snapshot_software = function(...) stop("mock stop"),
		.package = "rayshader"
	)

	heightmap = matrix(0, nrow = 20, ncol = 20)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(200, 200)
	))

	messages = character()
	expect_error(withCallingHandlers(
		render_depth(
			filename = tempfile(fileext = ".png"),
			instant_capture = TRUE
		),
		message = function(cnd) {
			messages <<- c(messages, conditionMessage(cnd))
			invokeRestart("muffleMessage")
		}
	), "mock stop")
	expect_false(any(grepl("Focus distance:", messages, fixed = TRUE)))
})

test_that("render_depth emits focus diagnostics only when verbose is TRUE", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()
	testthat::local_mocked_bindings(
		render_snapshot_software = function(...) stop("mock stop"),
		.package = "rayshader"
	)

	heightmap = matrix(0, nrow = 20, ncol = 20)
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = FALSE,
		windowsize = c(200, 200)
	))

	messages = character()
	expect_error(withCallingHandlers(
		render_depth(
			filename = tempfile(fileext = ".png"),
			instant_capture = TRUE,
			verbose = TRUE
		),
		message = function(cnd) {
			messages <<- c(messages, conditionMessage(cnd))
			invokeRestart("muffleMessage")
		}
	), "mock stop")
	expect_true(any(grepl("Focus distance:", messages, fixed = TRUE)))
})
