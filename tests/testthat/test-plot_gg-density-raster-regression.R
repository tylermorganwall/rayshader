library(ggplot2)

test_that("plot_gg() handles raster density plots with extra non-height scales", {
	mtplot_density = ggplot(mtcars) +
		stat_density_2d(
			aes(x = mpg, y = disp, fill = after_stat(!!str2lang("density"))),
			geom = "raster",
			contour = FALSE
		) +
		scale_x_continuous(expand = c(0, 0)) +
		scale_y_continuous(expand = c(0, 0)) +
		scale_fill_gradient(low = "pink", high = "red")

	rayimg = plot_gg(
		mtplot_density,
		width = 4,
		preview = TRUE,
		plot = FALSE,
		raytrace = FALSE
	)

	expect_equal(length(dim(rayimg)), 3)
	expect_true(all(dim(rayimg)[1:2] > 0))
})
