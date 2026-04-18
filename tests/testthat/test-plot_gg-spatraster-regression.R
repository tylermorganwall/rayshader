library(ggplot2)

expected_rendered_scene_zscale = function(height_matrix) {
	scene_extent = attr(height_matrix, "extent", exact = TRUE)
	expect_true(!is.null(scene_extent))
	mean(c(
		(scene_extent["xmax"] - scene_extent["xmin"]) / (nrow(height_matrix) - 1),
		(scene_extent["ymax"] - scene_extent["ymin"]) / (ncol(height_matrix) - 1)
	))
}

expected_plot_3d_shadowdepth = function(
	height_matrix,
	zscale,
	solid = TRUE,
	soliddepth = "auto"
) {
	min_height = min(height_matrix, na.rm = TRUE)
	max_height = max(height_matrix, na.rm = TRUE)
	if (identical(soliddepth, "auto")) {
		if (min_height != max_height) {
			soliddepth = min_height / zscale -
				(max_height / zscale - min_height / zscale) / 5
		} else {
			max_dim = max(dim(height_matrix))
			soliddepth = min_height / zscale - max_dim / 25
		}
	} else if (soliddepth > min_height) {
		soliddepth = min_height / zscale
	} else {
		soliddepth = soliddepth / zscale
	}
	min_height_shadow = if (solid) {
		min(c(min_height, soliddepth * zscale))
	} else {
		min_height
	}
	if (min_height_shadow != max_height) {
		if (solid) {
			soliddepth - (max_height / zscale - min_height_shadow / zscale) / 5
		} else {
			min_height_shadow / zscale -
				(max_height / zscale - min_height_shadow / zscale) / 5
		}
	} else {
		max_dim = max(dim(height_matrix))
		if (solid) {
			soliddepth - max_dim / 25
		} else {
			min_height - max_dim / 25
		}
	}
}

test_that("plot_gg() handles tidyterra SpatRaster layers without invalidating pointers", {
	skip_if_not_installed("terra")
	skip_if_not_installed("tidyterra")
	local_rgl_use_null()
	on.exit(rgl::close3d(), add = TRUE)

	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		nlyrs = 4,
		xmin = 0,
		xmax = 30,
		ymin = 0,
		ymax = 20
	)
	terra::values(rast) = cbind(
		red = c(255, 0, 0, 255, 255, 0),
		green = c(0, 255, 0, 255, 0, 255),
		blue = c(0, 0, 255, 0, 255, 255),
		height = c(1, 2, 3, 4, 5, 6)
	)
	names(rast) = c("red", "green", "blue", "height")

	ggval = ggplot() +
		tidyterra::geom_spatraster(
			data = rast,
			aes(fill = height)
		) +
		tidyterra::geom_spatraster_rgb(
			data = rast
		) +
		coord_sf()

	ggheight = ggplot() +
		tidyterra::geom_spatraster_rgb(
			data = rast
		) +
		tidyterra::geom_spatraster(
			data = rast,
			aes(fill = height)
		) +
		coord_sf()

	expect_no_condition(suppressWarnings(ggplot2::ggplotGrob(ggval)))
	expect_no_condition(suppressWarnings(ggplot2::ggplotGrob(ggheight)))

	expect_no_condition(suppressWarnings(plot_gg_test(
		ggobj = ggval,
		ggobj_height = ggheight,
		width = 2,
		height = 2,
		multicore = FALSE,
		raytrace = FALSE,
		shadow = FALSE,
		plot = FALSE
	)))

	expect_no_condition(suppressWarnings(ggplot2::ggplotGrob(ggval)))
	expect_no_condition(suppressWarnings(ggplot2::ggplotGrob(ggheight)))
})

test_that("plot_gg() preserves mapped data scale for tidyterra height rasters", {
	skip_if_not_installed("terra")
	skip_if_not_installed("tidyterra")
	local_rgl_use_null()
	on.exit(rgl::close3d(), add = TRUE)

	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		nlyrs = 4,
		xmin = 0,
		xmax = 30,
		ymin = 0,
		ymax = 20
	)
	terra::values(rast) = cbind(
		red = c(255, 0, 0, 255, 255, 0),
		green = c(0, 255, 0, 255, 0, 255),
		blue = c(0, 0, 255, 0, 255, 255),
		height = c(10, 20, 30, 40, 50, 60)
	)
	names(rast) = c("red", "green", "blue", "height")

	p = ggplot() +
		tidyterra::geom_spatraster(
			data = rast,
			aes(fill = height)
		) +
		tidyterra::geom_spatraster_rgb(
			data = rast
		) +
		coord_sf()

	height_matrix = suppressWarnings(plot_gg_test(
		ggobj = p,
		ggobj_height = p,
		width = 2,
		height = 2,
		multicore = FALSE,
		raytrace = FALSE,
		shadow = FALSE,
		save_height_matrix = TRUE
	))

	expect_true(is.matrix(height_matrix))
	expect_equal(range(height_matrix, na.rm = TRUE), c(10, 60), tolerance = 1e-6)
	expect_equal(
		get_scene_zscale(),
		expected_rendered_scene_zscale(height_matrix),
		tolerance = 1e-8
	)
})

test_that("plot_gg() recomputes auto shadowdepth from the rendered scene zscale", {
	skip_if_not_installed("terra")
	skip_if_not_installed("tidyterra")
	local_rgl_use_null()
	on.exit(rgl::close3d(), add = TRUE)

	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		nlyrs = 4,
		xmin = 0,
		xmax = 30,
		ymin = 0,
		ymax = 20
	)
	terra::values(rast) = cbind(
		red = c(255, 0, 0, 255, 255, 0),
		green = c(0, 255, 0, 255, 0, 255),
		blue = c(0, 0, 255, 0, 255, 255),
		height = c(10, 20, 30, 40, 50, 60)
	)
	names(rast) = c("red", "green", "blue", "height")

	p = ggplot() +
		tidyterra::geom_spatraster(
			data = rast,
			aes(fill = height)
		) +
		tidyterra::geom_spatraster_rgb(
			data = rast
		) +
		coord_sf()

	height_matrix = suppressWarnings(plot_gg_test(
		ggobj = p,
		ggobj_height = p,
		width = 2,
		height = 2,
		multicore = FALSE,
		raytrace = FALSE,
		shadow = FALSE,
		save_height_matrix = TRUE
	))
	expected_zscale = expected_rendered_scene_zscale(height_matrix)
	expected_shadowdepth = expected_plot_3d_shadowdepth(
		height_matrix = height_matrix,
		zscale = expected_zscale
	)

	expect_no_condition(suppressWarnings(plot_gg_test(
		ggobj = p,
		ggobj_height = p,
		width = 2,
		height = 2,
		multicore = FALSE,
		raytrace = FALSE,
		shadow = TRUE,
		plot = FALSE
	)))

	shadow_id = get_ids_with_labels("shadow")$id
	expect_length(shadow_id, 1)
	shadow_vertices = rgl::rgl.attrib(shadow_id[[1]], "vertices")
	expect_equal(length(unique(shadow_vertices[, 2])), 1)
	expect_equal(
		as.numeric(unique(shadow_vertices[, 2])),
		expected_shadowdepth,
		tolerance = 1e-6
	)
})

test_that("plot_gg() lets zscale override and vertical_exaggeration modify SpatRaster height scaling", {
	skip_if_not_installed("terra")
	skip_if_not_installed("tidyterra")
	local_rgl_use_null()
	on.exit(rgl::close3d(), add = TRUE)

	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		nlyrs = 4,
		xmin = 0,
		xmax = 30,
		ymin = 0,
		ymax = 20
	)
	terra::values(rast) = cbind(
		red = c(255, 0, 0, 255, 255, 0),
		green = c(0, 255, 0, 255, 0, 255),
		blue = c(0, 0, 255, 0, 255, 255),
		height = c(10, 20, 30, 40, 50, 60)
	)
	names(rast) = c("red", "green", "blue", "height")

	p = ggplot() +
		tidyterra::geom_spatraster(
			data = rast,
			aes(fill = height)
		) +
		tidyterra::geom_spatraster_rgb(
			data = rast
		) +
		coord_sf()

	expect_no_condition(suppressWarnings(plot_gg_test(
		ggobj = p,
		ggobj_height = p,
		width = 2,
		height = 2,
		zscale = 5,
		vertical_exaggeration = 2,
		multicore = FALSE,
		raytrace = FALSE,
		shadow = FALSE,
		plot = FALSE
	)))

	expect_equal(get_scene_zscale(), 2.5, tolerance = 1e-8)
})

test_that("plot_gg() uses explicit zscale directly when no exaggeration is supplied", {
	skip_if_not_installed("terra")
	skip_if_not_installed("tidyterra")
	local_rgl_use_null()
	on.exit(rgl::close3d(), add = TRUE)

	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		nlyrs = 4,
		xmin = 0,
		xmax = 30,
		ymin = 0,
		ymax = 20
	)
	terra::values(rast) = cbind(
		red = c(255, 0, 0, 255, 255, 0),
		green = c(0, 255, 0, 255, 0, 255),
		blue = c(0, 0, 255, 0, 255, 255),
		height = c(10, 20, 30, 40, 50, 60)
	)
	names(rast) = c("red", "green", "blue", "height")

	p = ggplot() +
		tidyterra::geom_spatraster(
			data = rast,
			aes(fill = height)
		) +
		tidyterra::geom_spatraster_rgb(
			data = rast
		) +
		coord_sf()

	height_matrix = suppressWarnings(plot_gg_test(
		ggobj = p,
		ggobj_height = p,
		width = 2,
		height = 2,
		zscale = 5,
		multicore = FALSE,
		raytrace = FALSE,
		shadow = FALSE,
		save_height_matrix = TRUE
	))

	expect_equal(get_scene_zscale(), 5, tolerance = 1e-8)
	expect_true(is.matrix(height_matrix))
})

test_that("plot_gg() maps deprecated scale onto vertical_exaggeration for SpatRaster heights", {
	skip_if_not_installed("terra")
	skip_if_not_installed("tidyterra")
	local_rgl_use_null()
	on.exit(rgl::close3d(), add = TRUE)

	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		nlyrs = 4,
		xmin = 0,
		xmax = 30,
		ymin = 0,
		ymax = 20
	)
	terra::values(rast) = cbind(
		red = c(255, 0, 0, 255, 255, 0),
		green = c(0, 255, 0, 255, 0, 255),
		blue = c(0, 0, 255, 0, 255, 255),
		height = c(10, 20, 30, 40, 50, 60)
	)
	names(rast) = c("red", "green", "blue", "height")

	p = ggplot() +
		tidyterra::geom_spatraster(
			data = rast,
			aes(fill = height)
		) +
		tidyterra::geom_spatraster_rgb(
			data = rast
		) +
		coord_sf()

	height_matrix = suppressWarnings(plot_gg_test(
		ggobj = p,
		ggobj_height = p,
		width = 2,
		height = 2,
		scale = 2,
		multicore = FALSE,
		raytrace = FALSE,
		shadow = FALSE,
		save_height_matrix = TRUE
	))

	expect_equal(
		get_scene_zscale(),
		expected_rendered_scene_zscale(height_matrix) / 2,
		tolerance = 1e-8
	)
})

test_that("plot_gg() rejects simultaneous scale and vertical_exaggeration", {
	skip_if_not_installed("terra")
	skip_if_not_installed("tidyterra")
	local_rgl_use_null()
	on.exit(rgl::close3d(), add = TRUE)

	rast = terra::rast(
		nrows = 2,
		ncols = 3,
		nlyrs = 4,
		xmin = 0,
		xmax = 30,
		ymin = 0,
		ymax = 20
	)
	terra::values(rast) = cbind(
		red = c(255, 0, 0, 255, 255, 0),
		green = c(0, 255, 0, 255, 0, 255),
		blue = c(0, 0, 255, 0, 255, 255),
		height = c(10, 20, 30, 40, 50, 60)
	)
	names(rast) = c("red", "green", "blue", "height")

	p = ggplot() +
		tidyterra::geom_spatraster(
			data = rast,
			aes(fill = height)
		) +
		tidyterra::geom_spatraster_rgb(
			data = rast
		) +
		coord_sf()

	expect_error(
		suppressWarnings(plot_gg_test(
			ggobj = p,
			ggobj_height = p,
			width = 2,
			height = 2,
			scale = 2,
			vertical_exaggeration = 2,
			multicore = FALSE,
			raytrace = FALSE,
			shadow = FALSE,
			plot = FALSE
		)),
		"`scale` is deprecated"
	)
})
