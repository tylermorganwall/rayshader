test_that("save_multipolygonz_to_obj writes stable MULTIPOLYGON Z OBJ output", {
	skip_if_not_installed("sf")
	skip_if_not_installed("rayvertex")

	sfobj = sf::st_sf(
		geometry = sf::st_sfc(
			sf::st_multipolygon(list(list(matrix(
				c(
					0, 0, 1,
					1, 0, 2,
					1, 1, 3,
					0, 0, 1
				),
				ncol = 3,
				byrow = TRUE
			)))),
			sf::st_multipolygon(list(list(matrix(
				c(
					2, 2, 4,
					3, 2, 5,
					3, 3, 6,
					2, 2, 4
				),
				ncol = 3,
				byrow = TRUE
			)))),
			crs = 4326
		)
	)

	obj_file = tempfile(fileext = ".obj")
	save_multipolygonz_to_obj(sfobj, obj_file)
	expect_equal(
		readLines(obj_file),
		c(
			"v 1.0000 0.0000 2.0000",
			"v 1.0000 1.0000 3.0000",
			"v 0.0000 0.0000 1.0000",
			"f 1 2 3",
			"v 3.0000 2.0000 5.0000",
			"v 3.0000 3.0000 6.0000",
			"v 2.0000 2.0000 4.0000",
			"f 4 5 6"
		)
	)

	save_multipolygonz_to_obj(sfobj, obj_file, swap_yz = TRUE)
	expect_equal(
		readLines(obj_file),
		c(
			"v 1.0000 2.0000 0.0000",
			"v 1.0000 3.0000 1.0000",
			"v 0.0000 1.0000 0.0000",
			"f 3 2 1",
			"v 3.0000 5.0000 2.0000",
			"v 3.0000 6.0000 3.0000",
			"v 2.0000 4.0000 2.0000",
			"f 6 5 4"
		)
	)

	mesh = rayshader:::multipolygonz_to_raymesh(sfobj)
	expect_s3_class(mesh, "ray_mesh")
	expect_equal(
		unname(mesh$vertices[[1]]),
		matrix(
			c(
				1, 0, 2,
				1, 1, 3,
				0, 0, 1,
				3, 2, 5,
				3, 3, 6,
				2, 2, 4
			),
			ncol = 3,
			byrow = TRUE
		)
	)
	expect_equal(
		mesh$shapes[[1]]$indices,
		matrix(c(0, 1, 2, 3, 4, 5), ncol = 3, byrow = TRUE)
	)
})

test_that("multipolygonz_to_raymesh triangulates polygon faces directly", {
	skip_if_not_installed("sf")
	skip_if_not_installed("rayvertex")

	sfobj = sf::st_sf(
		geometry = sf::st_sfc(
			sf::st_multipolygon(list(list(matrix(
				c(
					0, 0, 1,
					1, 0, 1,
					1, 1, 1,
					0, 1, 1,
					-1, 1, 1,
					0, 0, 1
				),
				ncol = 3,
				byrow = TRUE
			)))),
			crs = 4326
		)
	)

	expect_no_warning(mesh <- rayshader:::multipolygonz_to_raymesh(sfobj))
	expect_equal(nrow(mesh$vertices[[1]]), 5)
	expect_equal(nrow(mesh$shapes[[1]]$indices), 3)
	expect_true(all(mesh$shapes[[1]]$indices >= 0))
	expect_true(all(mesh$shapes[[1]]$indices < 5))
})
