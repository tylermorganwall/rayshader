test_that("generate_halo_underlay uses rayimage alpha outline gap controls", {
	overlay = array(0, dim = c(1, 6, 4))
	overlay[1, c(1, 6), 4] = 1

	one_pixel_fill = generate_halo_underlay(
		overlay,
		halo_expand = 1,
		halo_offset = c(0, 0),
		halo_color = "white",
		halo_alpha = 1,
		halo_blur = 0,
		halo_edge_softness = 0.1,
		halo_gap_fill = 1,
		halo_gap_fill_alpha_threshold = 0.25
	)
	two_pixel_fill = generate_halo_underlay(
		overlay,
		halo_expand = 1,
		halo_offset = c(0, 0),
		halo_color = "white",
		halo_alpha = 1,
		halo_blur = 0,
		halo_edge_softness = 0.1,
		halo_gap_fill = 2,
		halo_gap_fill_alpha_threshold = 0.25
	)

	expect_equal(unclass(one_pixel_fill)[,, 4], c(1, 0.5, 0, 0, 0.5, 1))
	expect_equal(
		unclass(two_pixel_fill)[,, 4],
		c(1, 0.5, 0.5, 0.5, 0.5, 1)
	)
})
