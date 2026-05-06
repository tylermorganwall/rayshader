test_that("render_highquality() resolves rgl material overrides", {
	skip_if_not_installed("rayrender")

	expect_false("point_material" %in% names(formals(render_highquality)))
	expect_false("path_material" %in% names(formals(render_highquality)))

	material = make_render_highquality_rgl_material(
		rayrender::diffuse,
		color = c(0.2, 0.3, 0.4),
		name = "points3d"
	)
	expect_true(is_rayrender_material(material))
	expect_equal(material[[1]]$properties[[1]], c(0.2, 0.3, 0.4))

	material_lookup = setNames(
		list(rayrender::metal()),
		"123"
	)
	expect_true(is_rayrender_material(resolve_render_highquality_rgl_material(
		rgl_materials = material_lookup,
		id = 123,
		tag = "points3d",
		color = c(1, 0, 0)
	)))
	expect_null(resolve_render_highquality_rgl_material(
		rgl_materials = material_lookup,
		id = 456,
		tag = "points3d",
		color = c(1, 0, 0)
	))
})

test_that("render_highquality() applies rgl material overrides by tag and id", {
	skip_if_not_installed("rayrender")
	skip_if_not_installed("rayvertex")
	on.exit(rgl::close3d(), add = TRUE)
	local_rgl_use_null()

	matrix(0, 3, 3) |>
		sphere_shade() |>
		plot_3d(zscale = 1, solid = FALSE)
	render_points(
		x = c(1, 2),
		y = c(1, 2),
		z = c(1, 1),
		color = c("red", "blue"),
		size = 2
	)
	point_ids = get_ids_with_labels(typeval = "points3d")$id
	surface_id = get_ids_with_labels(typeval = "surface")$id[[1]]

	expect_error(
		render_highquality(point_material = rayrender::metal),
		"Use `rgl_materials` instead"
	)

	scene_tag = render_highquality(
		return_scene = TRUE,
		light = FALSE,
		rgl_materials = list(points3d = rayrender::metal)
	)
	sphere_materials = scene_tag$material[scene_tag$shape == "sphere"]
	expect_equal(
		vapply(sphere_materials, function(material) material$type, integer(1)),
		rep(rayrender::metal()[[1]]$type, length(sphere_materials))
	)

	scene_id = render_highquality(
		return_scene = TRUE,
		light = FALSE,
		rgl_materials = setNames(
			list(rayrender::metal()),
			as.character(point_ids[[1]])
		)
	)
	sphere_materials = scene_id$material[scene_id$shape == "sphere"]
	expect_equal(
		vapply(sphere_materials, function(material) material$type, integer(1)),
		rep(rayrender::metal()[[1]]$type, length(sphere_materials))
	)

	scene_mesh_id = render_highquality(
		return_scene = TRUE,
		light = FALSE,
		rgl_materials = setNames(
			list(rayrender::metal()),
			as.character(surface_id)
		)
	)
	mesh_materials = scene_mesh_id$material[scene_mesh_id$shape == "raymesh"]
	expect_equal(mesh_materials[[1]]$type, rayrender::metal()[[1]]$type)
})
