library(ggplot2)

test_that("plot_gg() accepts cached radiance overlays", {
	mtplot = ggplot(mtcars) +
		geom_point(aes(x = mpg, y = disp, color = cyl)) +
		scale_color_continuous(limits = c(0, 8))

	cached_radiance = array(1, dim = c(16, 16, 4))
	cached_radiance[,, 4] = 1

	rayimg = plot_gg(
		mtplot,
		width = 2,
		raytrace = "radiance",
		saved_shadow_matrix = cached_radiance,
		preview = TRUE,
		plot = FALSE
	)

	expect_equal(length(dim(rayimg)), 3)
	expect_true(all(dim(rayimg)[1:2] > 0))
})

test_that("plot_gg() aligns radiance shadow_intensity with raytrace semantics", {
	mtplot = ggplot(mtcars) +
		geom_point(aes(x = mpg, y = disp, color = cyl)) +
		scale_color_continuous(limits = c(0, 8))

	cached_radiance = array(0, dim = c(16, 16, 4))
	cached_radiance[,, 1] = 1
	cached_radiance[,, 4] = 1

	base_img = plot_gg(
		mtplot,
		width = 2,
		raytrace = FALSE,
		preview = TRUE,
		plot = FALSE
	)

	no_effect_img = plot_gg(
		mtplot,
		width = 2,
		raytrace = "radiance",
		saved_shadow_matrix = cached_radiance,
		shadow_intensity = 1,
		preview = TRUE,
		plot = FALSE
	)

	full_effect_img = plot_gg(
		mtplot,
		width = 2,
		raytrace = "radiance",
		saved_shadow_matrix = cached_radiance,
		shadow_intensity = 0,
		preview = TRUE,
		plot = FALSE
	)

	expect_equal(dim(no_effect_img)[1:2], dim(base_img)[1:2])
	expect_equal(dim(no_effect_img)[3], 4)
	expect_equal(dim(base_img)[3], 3)
	expect_equal(as.numeric(no_effect_img[,,1:3]), as.numeric(base_img))
	expect_false(isTRUE(all.equal(
		as.numeric(full_effect_img[,,1:3]),
		as.numeric(base_img)
	)))
})

test_that("plot_gg() can render ggplots with radiance shading", {
	skip_if_not_installed("callr")
	options(rgl.useNULL = TRUE)

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
		width = 1,
		raytrace = "radiance",
		radiance_args = list(
			samples = 1,
			width = 64,
			height = 64,
			lightdirection = 225,
			lightaltitude = 30
		),
		preview = TRUE,
		plot = FALSE
	)

	expect_equal(length(dim(rayimg)), 3)
	expect_true(all(dim(rayimg)[1:2] > 0))
})
