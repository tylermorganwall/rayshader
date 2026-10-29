test_that("save_multipolygonz_to_obj writes stable MULTIPOLYGON Z OBJ output", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rayvertex")
  skip_if_not_installed("decido")

  sf_obj = sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(list(matrix(
        c(
          0,
          0,
          1,
          1,
          0,
          2,
          1,
          1,
          3,
          0,
          0,
          1
        ),
        ncol = 3,
        byrow = TRUE
      )))),
      sf::st_multipolygon(list(list(matrix(
        c(
          2,
          2,
          4,
          3,
          2,
          5,
          3,
          3,
          6,
          2,
          2,
          4
        ),
        ncol = 3,
        byrow = TRUE
      )))),
      crs = 4326
    )
  )

  obj_file = tempfile(fileext = ".obj")
  save_multipolygonz_to_obj(sf_obj, obj_file)
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

  save_multipolygonz_to_obj(sf_obj, obj_file, swap_yz = TRUE)
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

  mesh = rayshader:::multipolygonz_to_raymesh(sf_obj)
  expect_s3_class(mesh, "ray_mesh")
  expect_equal(
    unname(mesh$vertices[[1]]),
    matrix(
      c(
        1,
        0,
        2,
        1,
        1,
        3,
        0,
        0,
        1,
        3,
        2,
        5,
        3,
        3,
        6,
        2,
        2,
        4
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
  skip_if_not_installed("decido")

  sf_obj = sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(list(matrix(
        c(
          0,
          0,
          1,
          1,
          0,
          1,
          1,
          1,
          1,
          0,
          1,
          1,
          -1,
          1,
          1,
          0,
          0,
          1
        ),
        ncol = 3,
        byrow = TRUE
      )))),
      crs = 4326
    )
  )

  expect_no_warning(mesh <- rayshader:::multipolygonz_to_raymesh(sf_obj))
  expect_equal(nrow(mesh$vertices[[1]]), 5)
  expect_equal(nrow(mesh$shapes[[1]]$indices), 3)
  expect_true(all(mesh$shapes[[1]]$indices >= 0))
  expect_true(all(mesh$shapes[[1]]$indices < 5))
})

test_that("MULTIPOLYGON Z triangulation preserves concavity, holes, and winding", {
  skip_if_not_installed("sf")
  skip_if_not_installed("decido")
  skip_if_not_installed("rayvertex")
  concave = list(matrix(
    c(0, 0, 4, 0, 4, 4, 3, 4, 3, 1, 1, 1, 1, 4, 0, 4, 0, 0),
    ncol = 2,
    byrow = TRUE
  ))
  holes = list(
    matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE),
    matrix(c(2, 2, 4, 2, 4, 4, 2, 4, 2, 2), ncol = 2, byrow = TRUE),
    matrix(c(6, 6, 8, 6, 8, 8, 6, 8, 6, 6), ncol = 2, byrow = TRUE)
  )
  # These include both vertical wall orientations and a sloping roof. All
  # holes intentionally share the exterior's winding, which sf can represent.
  planes = list(
    function(x) cbind(x, 10),
    function(x) cbind(10, x),
    function(x) cbind(x[, 1], 10, x[, 2]),
    function(x) cbind(x, 10 + .3 * x[, 1] + .7 * x[, 2])
  )
  axes = list(c(1, 2), c(2, 3), c(1, 3), c(1, 2))
  origin = c(4000000.1234567, 6000000.2345678, 100.3456789)
  for (rings in list(concave, holes)) {
    reference = sf::st_sfc(sf::st_polygon(rings))
    expected_area = as.numeric(sf::st_area(reference))
    for (plane in seq_along(planes)) {
      for (reverse in c(FALSE, TRUE)) {
        rings3d = lapply(rings, function(ring) {
          coords = sweep(planes[[plane]](ring), 2, origin, "+")
          if (reverse) {
            coords = coords[nrow(coords):1, , drop = FALSE]
          }
          coords
        })
        input = sf::st_sfc(sf::st_multipolygon(list(rings3d)))
        for (swap in c(FALSE, TRUE)) {
          mesh = multipolygonz_to_raymesh(input, swap_yz = swap)
          vertices = mesh$vertices[[1]]
          triangles = mesh$shapes[[1]]$indices + 1L
          if (swap) {
            vertices = vertices[, c(1, 3, 2), drop = FALSE]
          }
          # No invented vertices or coordinate rounding during triangulation.
          original = do.call(
            rbind,
            lapply(rings3d, function(r) r[-1, , drop = FALSE])
          )
          expect_equal(unname(vertices), unname(original), tolerance = 0)
          xy = sweep(vertices, 2, origin)[, axes[[plane]], drop = FALSE]
          regions = sf::st_sfc(lapply(seq_len(nrow(triangles)), function(i) {
            sf::st_polygon(list(xy[
              triangles[i, c(1, 2, 3, 1)],
              ,
              drop = FALSE
            ]))
          }))
          expect_equal(
            sum(as.numeric(sf::st_area(regions))),
            expected_area,
            tolerance = 1e-8
          )
          difference = suppressWarnings(sf::st_sym_difference(
            sf::st_union(regions),
            reference
          ))
          expect_lt(sum(as.numeric(sf::st_area(difference))), 1e-8)
          a = xy[triangles[, 2], , drop = FALSE] -
            xy[triangles[, 1], , drop = FALSE]
          b = xy[triangles[, 3], , drop = FALSE] -
            xy[triangles[, 1], , drop = FALSE]
          signed_area = a[, 1] * b[, 2] - a[, 2] * b[, 1]
          expected_sign = if (xor(reverse, swap)) -1 else 1
          expect_true(all(sign(signed_area) == expected_sign))
        }
      }
    }
  }
})

test_that("OBJ exports use the same hole-preserving triangles as rendering", {
  skip_if_not_installed("sf")
  skip_if_not_installed("decido")
  skip_if_not_installed("rayvertex")
  outer = cbind(
    matrix(c(0, 0, 6, 0, 6, 6, 0, 6, 0, 0), ncol = 2, byrow = TRUE),
    3
  )
  hole = cbind(
    matrix(c(2, 2, 4, 2, 4, 4, 2, 4, 2, 2), ncol = 2, byrow = TRUE),
    3
  )
  input = sf::st_sfc(sf::st_multipolygon(list(
    list(outer, hole),
    list(sweep(outer, 2, c(20, 0, 0), "+"))
  )))
  for (swap in c(FALSE, TRUE)) {
    file = tempfile(fileext = ".obj")
    save_multipolygonz_to_obj(input, file, swap_yz = swap)
    lines = readLines(file)
    parse_rows = function(prefix) {
      rows = sub(
        paste0("^", prefix, " "),
        "",
        lines[startsWith(lines, paste0(prefix, " "))]
      )
      do.call(rbind, lapply(strsplit(rows, " +"), as.numeric))
    }
    mesh = multipolygonz_to_raymesh(input, swap_yz = swap)
    expect_equal(parse_rows("v"), unname(mesh$vertices[[1]]))
    expect_equal(parse_rows("f") - 1L, mesh$shapes[[1]]$indices)
  }
})

test_that("MULTIPOLYGON Z conversion rejects untriangulatable faces clearly", {
  skip_if_not_installed("sf")
  collinear = matrix(
    c(0, 0, 1, 1, 0, 1, 2, 0, 1, 0, 0, 1),
    ncol = 3,
    byrow = TRUE
  )
  input = sf::st_sfc(sf::st_multipolygon(list(list(collinear))))
  expect_error(multipolygonz_to_raymesh(input), "face 1:.*zero area")
  file = tempfile(fileext = ".obj")
  writeLines("keep existing file", file)
  expect_error(save_multipolygonz_to_obj(input, file), "zero area")
  expect_identical(readLines(file), "keep existing file")
})
