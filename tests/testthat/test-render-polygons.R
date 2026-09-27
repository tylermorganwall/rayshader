test_that("polygon meshes preserve holes, multipart caps, and parallel geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rayvertex", "0.16.0")
  local_rgl_use_null()
  withr::local_options(cores = 2)
  on.exit(foreach::registerDoSEQ(), add = TRUE)
  on.exit(rgl::close3d(), add = TRUE)

  heightmap = matrix(0, 10, 10)
  plot_3d_test(
    sphere_shade(heightmap),
    heightmap,
    shadow = FALSE,
    water = FALSE
  )
  outer = rbind(c(1, 1), c(5, 1), c(5, 5), c(1, 5), c(1, 1))
  hole = rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2))
  square = rbind(c(6, 1), c(7, 1), c(7, 2), c(6, 2), c(6, 1))
  polygon = sf::st_sf(
    upper = c(6, 4),
    lower = c(2, 4),
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(list(outer, hole))),
      sf::st_multipolygon(list(
        list(square),
        list(sweep(square, 2, c(0, 3), "+"))
      ))
    )
  )
  serial_vertices = NULL
  for (parallel in c(FALSE, TRUE)) {
    render_polygons(
      polygon,
      data_column_top = "upper",
      data_column_bottom = "lower",
      bottom = 0,
      scale_data = 2,
      zscale = 2,
      extent = c(0, 10, 0, 10),
      heightmap = heightmap,
      parallel = parallel,
      lit = FALSE,
      clear_previous = TRUE
    )
    ids = get_ids_with_labels(typeval = "polygon3d")$id
    expect_length(ids, 2)
    vertices = lapply(ids, function(id) rgl::rgl.attrib(id, "vertices"))
    expect_equal(range(vertices[[1]][, 2]), c(2, 6))
    expect_equal(range(vertices[[2]][, 2]), c(4, 4))
    expect_equal(range(vertices[[1]][, 1]), c(-4, 0))
    expect_equal(range(vertices[[1]][, 3]), c(0, 4))

    # Cap area excludes the hole and includes both multipart components.
    for (i in seq_along(vertices)) {
      v = vertices[[i]]
      a = v[seq(1, nrow(v), 3), , drop = FALSE]
      b = v[seq(2, nrow(v), 3), , drop = FALSE]
      c = v[seq(3, nrow(v), 3), , drop = FALSE]
      cap = a[, 2] == polygon$upper[i] &
        b[, 2] == polygon$upper[i] &
        c[, 2] == polygon$upper[i]
      area = abs(
        (b[, 1] - a[, 1]) *
          (c[, 3] - a[, 3]) -
          (b[, 3] - a[, 3]) * (c[, 1] - a[, 1])
      ) /
        2
      expect_equal(sum(area[cap]), c(15, 2)[i])
    }
    if (parallel) {
      expect_equal(vertices, serial_vertices)
    } else {
      serial_vertices = vertices
    }
  }
})

test_that("automatic polygon bottoms stay within the base under cached scaling", {
  skip_if_not_installed("sf")
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(0, 10, 10)
  polygon = sf::st_sf(
    upper = 100,
    lower = 5,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(2, 2),
      c(4, 2),
      c(4, 4),
      c(2, 4),
      c(2, 2)
    ))))
  )

  for (scene_zscale in c(0.1, 2)) {
    plot_3d_test(
      constant_shade(heightmap),
      heightmap,
      zscale = scene_zscale,
      vertical_exaggeration = 2,
      solid_depth = -2,
      shadow = FALSE
    )
    base_id = get_ids_with_labels(typeval = "base")$id[1]
    base_bottom = min(rgl::rgl.attrib(base_id, "vertices")[, 2])
    render_polygons(
      polygon,
      extent = c(0, 10, 0, 10),
      data_column_top = "upper",
      scale_data = 0.1,
      lit = FALSE
    )
    id = get_ids_with_labels(typeval = "polygon3d")$id[1]
    heights = range(rgl::rgl.attrib(id, "vertices")[, 2])
    expect_equal(heights, c(base_bottom / 2, 10 / (scene_zscale / 2)))

    # Explicit heights retain their data-unit interpretation.
    render_polygons(
      polygon,
      extent = c(0, 10, 0, 10),
      top = 10,
      bottom = 3,
      lit = FALSE,
      clear_previous = TRUE
    )
    id = get_ids_with_labels(typeval = "polygon3d")$id[1]
    expect_equal(
      range(rgl::rgl.attrib(id, "vertices")[, 2]),
      c(3, 10) / (scene_zscale / 2)
    )

    render_polygons(
      polygon,
      extent = c(0, 10, 0, 10),
      data_column_top = "upper",
      data_column_bottom = "lower",
      scale_data = 0.1,
      lit = FALSE,
      clear_previous = TRUE
    )
    id = get_ids_with_labels(typeval = "polygon3d")$id[1]
    expect_equal(
      range(rgl::rgl.attrib(id, "vertices")[, 2]),
      c(0.5, 10) / (scene_zscale / 2)
    )
  }
})
