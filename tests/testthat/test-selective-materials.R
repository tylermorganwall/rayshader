test_that("tag filtering preserves material values, prefixes and empty schemas", {
  skip_if_not_installed("rgl")
  rgl::open3d(useNULL = TRUE)
  on.exit(rgl::close3d())
  texture = tempfile(fileext = ".png")
  on.exit(unlink(texture), add = TRUE)
  png::writePNG(array(1, c(2, 2, 3)), texture)
  tags = c(
    "road_path",
    "base",
    "water",
    "obj-example",
    "surface-dim_2_2",
    "surface_tris-dim_2_2"
  )
  for (tag in tags) {
    rgl::triangles3d(
      c(0, 1, 0),
      c(0, 0, 1),
      c(0, 0, 0),
      tag = tag,
      texture = texture,
      color = "white"
    )
  }
  all = get_ids_with_labels()
  for (tag in c("water", "obj", "surface_tris", "raytext")) {
    matched = if (tag == "obj") {
      startsWith(all$tag, "obj")
    } else if (tag == "surface_tris") {
      startsWith(all$tag, tag)
    } else {
      all$tag == tag
    }
    selected = get_ids_with_labels(tag)
    expected = all[matched, ]
    expect_identical(names(selected), names(expected))
    expect_identical(selected$id, expected$id)
    for (column in names(expected)) {
      expect_identical(unname(selected[[column]]), unname(expected[[column]]))
    }
  }
  expect_equal(nrow(get_ids_with_labels(c("surface", "surface_tris"))), 2)
  expect_identical(get_ids_with_labels("unrecognized"), all)
  expect_identical(get_ids_with_labels()$water_color, all$water_color)
})

test_that("a selective query does not fetch unrelated materials", {
  skip_if_not_installed("rgl")
  rgl::open3d(useNULL = TRUE)
  on.exit(rgl::close3d())
  for (i in 1:20) {
    rgl::lines3d(c(0, 1), c(0, 1), c(0, 1), tag = "road_path")
  }
  water = rgl::triangles3d(c(0, 1, 0), c(0, 0, 1), c(0, 0, 0), tag = "water")
  original = rgl::material3d
  queried = integer()
  local_mocked_bindings(
    material3d = function(..., id) {
      queried <<- c(queried, id)
      original(..., id = id)
    },
    .package = "rgl"
  )
  expect_identical(get_ids_with_labels("water")$id, as.integer(water))
  expect_identical(queried, as.integer(water))
  queried = integer()
  expect_equal(nrow(get_ids_with_labels("raytext")), 0)
  expect_length(queried, 0)
})
