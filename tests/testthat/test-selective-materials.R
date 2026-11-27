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


test_that("ID snapshots and ID filters avoid unrelated material queries", {
  rgl::open3d(useNULL = TRUE)
  on.exit(rgl::close3d())
  road = rgl::lines3d(c(0, 1), c(0, 1), c(0, 1), tag = "road_path")
  water = rgl::triangles3d(c(0, 1, 0), c(0, 0, 1), c(0, 0, 0), tag = "water")
  ids = rgl::ids3d(tags = TRUE)
  expected = get_ids_with_labels("water")
  original = rgl::material3d
  queried = integer()
  local_mocked_bindings(
    material3d = function(..., id) {
      queried <<- c(queried, id)
      original(..., id = id)
    },
    .package = "rgl"
  )
  actual = get_ids_with_labels(id_table = ids, exclude_ids = road)
  expect_identical(actual, expected)
  expect_identical(queried, as.integer(water))
  queried = integer()
  expect_equal(nrow(get_ids_with_labels(id_table = ids, include_ids = -1L)), 0)
  expect_length(queried, 0)
  expect_identical(
    get_render_highquality_rgl_material_info(
      list(water = rayrender::diffuse),
      id_table = ids
    ),
    ids[ids$id == water, c("id", "tag")]
  )
  expect_length(queried, 0)
})

test_that("mesh conversion skips preview materials and accepts untextured surfaces", {
  rgl::open3d(useNULL = TRUE)
  on.exit(rgl::close3d())
  surface = rgl::triangles3d(
    c(0, 1, 0),
    c(0, 0, 1),
    c(0, 0, 0),
    tag = "surface_tris-dim_2_2",
    texture = NULL
  )
  rgl::lines3d(c(0, 1), c(0, 1), c(0, 1), tag = "road_path")
  ids = rgl::ids3d(tags = TRUE)
  original = rgl::material3d
  queried = integer()
  local_mocked_bindings(
    material3d = function(..., id) {
      queried <<- c(queried, id)
      original(..., id = id)
    },
    .package = "rgl"
  )
  mesh = convert_rgl_to_raymesh(id_table = ids)
  expect_true(inherits(mesh, "ray_mesh"))
  expect_true(all(queried == surface))
})

test_that("verbose high-quality conversion reports phases and reaches rayrender", {
  h = matrix(0, 3, 3)
  plot_3d(height_shade(h), h, shadow = FALSE, solid = FALSE)
  on.exit(rgl::close3d())
  received = NULL
  original = rayrender::render_scene
  fake = original
  body(fake) = quote({
    received <<- verbose
    array(0, c(2, 2, 3))
  })
  environment(fake) = environment()
  local_mocked_bindings(render_scene = fake, .package = "rayrender")
  logs = capture.output(
    render_highquality(light = FALSE, verbose = TRUE, plot = FALSE),
    type = "message"
  )
  expect_true(received)
  expect_true(any(grepl("scene assembly finished", logs)))
  expect_true(any(grepl("renderer timings follow", logs)))
})
