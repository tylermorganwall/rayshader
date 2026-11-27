test_that("resolve_render_label_text_angle_rayvertex() matches software label rendering conventions", {
	expect_equal(
		resolve_render_label_text_angle_rayvertex(
			text_angle = NULL,
			theta = 120,
			rotmat = c(35, 0, 0)
		),
		c(35, -120, 0)
	)
	expect_equal(
		resolve_render_label_text_angle_rayvertex(
			text_angle = 30,
			theta = 120,
			rotmat = c(35, 0, 0)
		),
		c(0, 30, 0)
	)
	expect_equal(
		resolve_render_label_text_angle_rayvertex(
			text_angle = c(1, 2, 3),
			theta = 120,
			rotmat = c(35, 0, 0)
		),
		c(1, 2, 3)
	)
})

test_that("resolve_render_label_text_angle_rayrender() matches high quality label rendering conventions", {
	expect_equal(
		resolve_render_label_text_angle_rayrender(
			text_angle = NULL,
			phi = 35,
			theta = 120
		),
		c(-35, 300, 0)
	)
	expect_equal(
		resolve_render_label_text_angle_rayrender(
			text_angle = 30,
			phi = 35,
			theta = 120
		),
		c(0, 30, 0)
	)
	expect_equal(
		resolve_render_label_text_angle_rayrender(
			text_angle = c(1, 2, 3),
			phi = 35,
			theta = 120
		),
		c(1, 2, 3)
	)
})
