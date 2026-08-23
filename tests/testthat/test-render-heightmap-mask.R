test_that("floating overlays orient the cached heightmap NA mask", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  heightmap = matrix(seq_len(216), nrow = 12, ncol = 18)
  plot_3d(
    height_shade(heightmap),
    heightmap = heightmap,
    baseshape = "hex",
    shadow = FALSE
  )

  cached_heightmap = get_scene_heightmap()
  overlay = array(1, dim = c(18, 12, 4))
  overlay_id = render_floating_overlay(overlay, altitude = 150)
  overlay_texture = rgl::material3d(id = overlay_id)$texture
  overlay_alpha = png::readPNG(overlay_texture)[,, 4]

  expect_identical(overlay_alpha == 0, t(is.na(cached_heightmap)))
})

test_that("cloud layers follow the oriented cached heightmap mask", {
  skip_if_not_installed("ambient")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)

  heightmap = matrix(seq_len(216), nrow = 12, ncol = 18)
  plot_3d(
    height_shade(heightmap),
    heightmap = heightmap,
    baseshape = "hex",
    shadow = FALSE
  )
  render_clouds(
    start_altitude = 250,
    end_altitude = 251,
    layers = 1,
    fractal_levels = 1,
    seed = 3
  )

  cloud_ids = get_ids_with_labels(typeval = "floating_overlay_tris")
  cloud_texture = rgl::material3d(id = tail(cloud_ids$id, 1))$texture
  cloud_alpha = png::readPNG(cloud_texture)[,, 4]
  heightmap_na_mask = t(is.na(get_scene_heightmap()))

  expect_true(all(cloud_alpha[heightmap_na_mask] == 0))
  expect_true(any(cloud_alpha[!heightmap_na_mask] > 0))
})
