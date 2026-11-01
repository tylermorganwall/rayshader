test_that('native instances preserve mesh slots, materials and scaled normals', {
  skip_if_not_installed('rayvertex')
  obj = rayvertex::sphere_mesh()
  before = serialize(obj, NULL)
  xyz = rbind(c(1, 2, 3), c(-2, .5, 0), c(4, 0, -1), c(0, 0, 0))
  scale = rbind(c(1, 1, 1), c(2, 3, .5), c(-1, 2, 1), c(0, 1, 2))
  colors = c('red', 'blue', 'red', 'blue')
  for (retain in c(FALSE, TRUE)) {
    reference = lapply(seq_len(nrow(xyz)), function(i) {
      mesh = obj
      if (any(scale[i, ] != 1)) {
        mesh = rayvertex::scale_mesh(mesh, scale[i, ])
      }
      if (!retain) {
        mesh = rayvertex::set_material(mesh, diffuse = colors[i])
      }
      rayvertex::translate_mesh(mesh, xyz[i, ])
    })
    expected = rayvertex::scene_from_list(reference)
    actual = render_obj_instances(obj, xyz, scale, colors, retain)
    expect_equal(actual, expected, tolerance = 1e-14)
    expect_identical(actual$shapes, expected$shapes)
    expect_identical(actual$texcoords, expected$texcoords)
    expect_identical(actual$materials, expected$materials)
    expect_identical(
      attr(actual, 'material_hashes'),
      attr(expected, 'material_hashes')
    )
  }
  expect_identical(serialize(obj, NULL), before)
  # Missing normal/texture arrays must stay empty and must not be indexed.
  obj$normals[[1]] = matrix(numeric(), 0, 3)
  obj$texcoords[[1]] = matrix(numeric(), 0, 2)
  actual = render_obj_instances(obj, xyz, scale, colors, TRUE)
  expect_true(all(vapply(actual$normals, nrow, integer(1)) == 0L))
  expect_true(all(vapply(actual$texcoords, nrow, integer(1)) == 0L))
  template = list(rayvertex::scene_from_list(list(obj)))
  expect_error(
    instance_obj_cpp(template, rep(NA_integer_, 4), xyz, scale),
    'template index'
  )
  expect_error(
    instance_obj_cpp(template, rep(1L, 4), xyz[, 1:2], scale),
    'dimensions'
  )
})

test_that('native terrain rings and shoreline samples preserve geometric boundaries', {
  skip_if_not_installed('sf')
  a = rbind(c(-3, 1, -3), c(-3, 1, -3))
  b = rbind(c(3, 4, -3), c(3, 7, 3))
  c = rbind(c(3, 7, 3), c(-3, 4, 3))
  ids = c(2L, 1L)
  expected = lapply(ids, function(i) {
    sf::st_polygon(list(rbind(
      a[i, c(1, 3)],
      b[i, c(1, 3)],
      c[i, c(1, 3)],
      a[i, c(1, 3)]
    )))
  })
  expect_identical(water_triangle_rings_cpp(a, b, c, ids), expected)
  expect_error(water_triangle_rings_cpp(a, b, c, NA_integer_), 'face index')
  exterior = rbind(c(-1, -1), c(1, -1), c(1, -1), c(1, 1), c(-1, 1), c(-1, -1))
  withr::local_seed(84)
  points = rbind(
    matrix(runif(2000, -2, 2), ncol = 2),
    cbind(seq(-1, 1, length.out = 101), -1),
    cbind(1 + c(-1, 0, 1) * 1e-8, 0)
  )
  coordinates = cbind(points, 1)
  for (ring in list(exterior, exterior[nrow(exterior):1, ])) {
    for (tol in c(0, 1e-8, .3)) {
      actual = water_support_samples_cpp(coordinates, 1L, a, b, c, ring, tol)
      on_shore = lengths(sf::st_is_within_distance(
        sf::st_as_sf(
          data.frame(x = points[, 1], z = points[, 2]),
          coords = c('x', 'z')
        ),
        sf::st_sfc(sf::st_linestring(ring)),
        dist = tol
      )) >
        0
      expect_identical(actual$shore, actual$heights[on_shore])
      expect_equal(
        actual$heights,
        1 + (points[, 1] + 3) / 2 + (points[, 2] + 3) / 2,
        tolerance = 1e-14
      )
    }
  }
  expect_error(
    water_support_samples_cpp(coordinates, NA_integer_, a, b, c, exterior, 0),
    'face index'
  )
})
