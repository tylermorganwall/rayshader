library(ggplot2)

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
		xmax = 3,
		ymin = 0,
		ymax = 2
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
