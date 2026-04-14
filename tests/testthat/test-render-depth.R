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

test_that("render_depth transparent_water hides water via subscene membership and restores it", {
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	heightmap = matrix(0, nrow = 20, ncol = 20)
	heightmap[8:12, 8:12] = -1
	texture = sphere_shade(heightmap)
	expect_no_condition(plot_3d_test(
		texture,
		heightmap,
		zscale = 10,
		shadow = FALSE,
		water = TRUE,
		waterlinecolor = "white",
		windowsize = c(200, 200)
	))

	visible_tags = list()
	testthat::local_mocked_bindings(
		render_snapshot_software = function(filename, debug = "all", ...) {
			visible_tags[[length(visible_tags) + 1]] <<- get_ids_with_labels(
				typeval = c("water", "waterlines")
			)$tag
			if (identical(debug, "all")) {
				return(list(
					r = matrix(1, 4, 4),
					g = matrix(1, 4, 4),
					b = matrix(1, 4, 4),
					linear_depth = matrix(1, 4, 4)
				))
			}
			stop("depth capture stop", call. = FALSE)
		},
		.package = "rayshader"
	)

	expect_error(
		render_depth(
			filename = tempfile(fileext = ".png"),
			preview_focus = TRUE,
			transparent_water = TRUE,
			focus = 1,
			instant_capture = TRUE
		),
		"depth capture stop"
	)

	initial_has_waterlines = any(visible_tags[[1]] == "waterlines")
	expect_true(any(visible_tags[[1]] == "water"))
	expect_false(any(visible_tags[[2]] == "water"))
	if (initial_has_waterlines) {
		expect_false(any(visible_tags[[2]] == "waterlines"))
	}

	restored_tags = get_ids_with_labels(typeval = c("water", "waterlines"))$tag
	expect_true(any(restored_tags == "water"))
	if (initial_has_waterlines) {
		expect_true(any(restored_tags == "waterlines"))
	}
})
