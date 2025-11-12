library(ggplot2)

test_that("plot_gg() doesn't error", {
	ggdiamonds = ggplot(diamonds, aes(x, depth)) +
		stat_density_2d(
			aes(fill = after_stat(nlevel), color = after_stat(nlevel)),
			geom = "polygon",
			n = 200,
			bins = 50,
			contour = TRUE
		) +
		facet_wrap(clarity ~ .) +
		scale_fill_viridis_c(option = "A") +
		scale_color_viridis_c(option = "A")

	expect_no_condition(plot_gg(
		ggdiamonds,
		multicore = TRUE,
		width = 5,
		height = 5,
		scale = 250,
		windowsize = c(1400, 866),
		zoom = 0.55,
		phi = 30
	))
	expect_no_condition(render_snapshot(software_render = TRUE))
	#Contours and other lines will automatically be ignored. Here is the volcano dataset:

	ggvolcano = volcano |>
		reshape2::melt() |>
		ggplot() +
		geom_tile(aes(x = Var1, y = Var2, fill = value)) +
		geom_contour(aes(x = Var1, y = Var2, z = value), color = "black") +
		scale_x_continuous("X", expand = c(0, 0)) +
		scale_y_continuous("Y", expand = c(0, 0)) +
		scale_fill_gradientn("Z", colours = terrain.colors(10, alpha = 1)) +
		coord_fixed()

	expect_warning(
		plot_gg(
			ggvolcano,
			multicore = TRUE,
			raytrace = TRUE,
			width = 7,
			height = 4,
			scale = 300,
			windowsize = c(1400, 866),
			zoom = 0.6,
			phi = 30,
			theta = 30
		),
		"missing values"
	)
	expect_no_condition(render_snapshot(software_render = TRUE))

	#You can specify the color and height separately using the `ggobj_height()` argument.
	ggvolcano_surface = volcano |>
		reshape2::melt() |>
		ggplot() +
		geom_tile(aes(x = Var1, y = Var2, fill = value), alpha = 0) +
		geom_contour(aes(x = Var1, y = Var2, z = value), color = "black") +
		scale_x_continuous("X", expand = c(0, 0)) +
		scale_y_continuous("Y", expand = c(0, 0)) +
		scale_fill_gradientn("Z", colours = terrain.colors(10, alpha = 1)) +
		coord_fixed()

	expect_warning(
		plot_gg(
			ggvolcano_surface,
			ggobj_height = ggvolcano,
			multicore = TRUE,
			raytrace = TRUE,
			width = 7,
			height = 4,
			scale = 300,
			windowsize = c(1400, 866),
			zoom = 0.6,
			phi = 30,
			theta = 30
		),
		"missing values"
	)
	expect_no_condition(render_snapshot(software_render = TRUE))

	#Here, we will create a 3D plot of the mtcars dataset. This automatically detects
	#that the user used the `color` aesthetic instead of the `fill`.
	mtplot = ggplot(mtcars) +
		geom_point(aes(x = mpg, y = disp, color = cyl)) +
		scale_color_continuous(limits = c(0, 8))

	#Preview how the plot will look by setting `preview = TRUE`: We also adjust the angle of the light.

	expect_no_condition(plot_gg(
		mtplot,
		width = 3.5,
		sunangle = 225,
		preview = TRUE
	))

	plot_gg(
		mtplot,
		width = 3.5,
		multicore = TRUE,
		windowsize = c(1400, 866),
		sunangle = 225,
		zoom = 0.60,
		phi = 30,
		theta = 45
	)
	expect_no_condition(render_snapshot(software_render = TRUE))

	#Now let's plot a density plot in 3D.
	mtplot_density = ggplot(mtcars) +
		stat_density_2d(
			aes(x = mpg, y = disp, fill = after_stat(!!str2lang("density"))),
			geom = "raster",
			contour = FALSE
		) +
		scale_x_continuous(expand = c(0, 0)) +
		scale_y_continuous(expand = c(0, 0)) +
		scale_fill_gradient(low = "pink", high = "red")

	expect_no_condition(plot_gg(
		mtplot_density,
		width = 4,
		zoom = 0.60,
		theta = -45,
		phi = 30,
		windowsize = c(1400, 866)
	))
	expect_no_condition(render_snapshot(software_render = TRUE))

	#This also works facetted.
	mtplot_density_facet = mtplot_density + facet_wrap(~cyl)

	#Preview this plot in 2D:

	expect_no_condition(plot_gg(mtplot_density_facet, preview = TRUE))

	expect_no_condition(plot_gg(
		mtplot_density_facet,
		windowsize = c(1400, 866),
		zoom = 0.55,
		theta = -10,
		phi = 25
	))
	expect_no_condition(render_snapshot(software_render = TRUE))

	#That is a little cramped. Specifying a larger width will improve the readability of this plot.

	expect_no_condition(plot_gg(mtplot_density_facet, width = 6, preview = TRUE))

	#That's better. Let's plot it in 3D, and increase the scale.

	expect_no_condition(plot_gg(
		mtplot_density_facet,
		width = 6,
		windowsize = c(1400, 866),
		zoom = 0.55,
		theta = -10,
		phi = 25,
		scale = 300
	))

	#We can also render a flat version of the plot alongside (or above/below) the 3D version.

	expect_no_condition(plot_gg(
		mtplot_density_facet,
		width = 6,
		windowsize = c(1400, 866),
		zoom = 0.65,
		theta = -25,
		phi = 35,
		scale = 300,
		flat_plot_render = TRUE,
		flat_direction = "x"
	))
	rgl::close3d()
})
