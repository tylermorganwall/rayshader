test_that("plot_3d preserves default normals and accepts precomputed normals", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(c(0, 1, 0, 2, 4, 1, 0, 3, 1), 3, 3)
  texture = constant_shade(heightmap)
  plot_3d_test(texture, heightmap, solid = FALSE, shadow = FALSE)
  id = get_ids_with_labels(typeval = "surface_tris")$id[1]
  expected = rgl::rgl.attrib(id, "normals")
  expect_gt(nrow(expected), 0)
  expect_true(rgl::material3d(id = id)$smooth)

  normal_data = calculate_normal(heightmap)
  testthat::local_mocked_bindings(
    calculate_normal = function(...) stop("unexpected normal calculation"),
    .package = "rayshader"
  )
  plot_3d_test(
    texture,
    heightmap,
    solid = FALSE,
    shadow = FALSE,
    normals = TRUE,
    precomputed_normals = normal_data
  )
  id = get_ids_with_labels(typeval = "surface_tris")$id[1]
  expect_equal(rgl::rgl.attrib(id, "normals"), expected)
  mesh = convert_rgl_to_raymesh(include_ids = id)
  expect_true(all(mesh$shapes[[1]]$has_vertex_normals))
})

test_that("disabled DEM normals skip calculation and stay disabled in mesh and OBJ exports", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(seq_len(25)^2, 5, 5)
  texture = constant_shade(heightmap)
  testthat::local_mocked_bindings(
    calculate_normal = function(...) stop("unexpected normal calculation"),
    .package = "rayshader"
  )
  directory = tempfile("dem-normals-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (triangulate in c(FALSE, TRUE)) {
    plot_3d_test(
      texture,
      heightmap,
      solid = FALSE,
      shadow = FALSE,
      normals = FALSE,
      triangulate = triangulate,
      precomputed_normals = stop("disabled precomputed normals were evaluated")
    )
    surface = get_ids_with_labels(typeval = "surface_tris")
    expect_false(rgl::material3d(id = surface$id[1])$smooth)
    expect_false(surface$load_normals[[1]])
    mesh = convert_rgl_to_raymesh(include_ids = surface$id[1])
    expect_equal(nrow(mesh$normals[[1]]), 0)
    expect_false(any(mesh$shapes[[1]]$has_vertex_normals))
    expect_true(all(mesh$shapes[[1]]$norm_indices == -1))

    filename = file.path(directory, paste0("terrain-", triangulate, ".obj"))
    save_obj(filename, save_texture = FALSE)
    obj = readLines(filename)
    expect_true(any(startsWith(obj, "v ")))
    expect_true(any(startsWith(obj, "f ")))
    expect_false(any(startsWith(obj, "vn ")))
    expect_false(any(grepl("[0-9]+/[0-9]*/[0-9]+", obj[startsWith(obj, "f ")])))
  }
})

test_that("normals are controlled per DEM without changing terrain geometry or other objects", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  heightmap = matrix(c(0, 1, 0, 2, 4, 1, 0, 3, 1), 3, 3)
  texture = constant_shade(heightmap)
  plot_3d_test(texture, heightmap, shadow = FALSE, normals = TRUE)
  first = get_ids_with_labels(typeval = "surface_tris")$id[1]
  plot_3d_test(
    texture,
    heightmap,
    shadow = FALSE,
    solid = FALSE,
    normals = FALSE,
    plot_new = FALSE,
    close_previous = FALSE,
    clear_previous = FALSE
  )
  ids = get_ids_with_labels()
  second = setdiff(ids$id[startsWith(ids$tag, "surface")], first)
  expect_length(second, 1)
  for (attribute in c("vertices", "indices", "texcoords")) {
    expect_equal(
      rgl::rgl.attrib(first, attribute),
      rgl::rgl.attrib(second, attribute)
    )
  }
  expect_true(all(
    convert_rgl_to_raymesh(include_ids = first)$shapes[[1]]$has_vertex_normals
  ))
  expect_false(any(
    convert_rgl_to_raymesh(include_ids = second)$shapes[[1]]$has_vertex_normals
  ))
  base_ids = ids$id[ids$tag %in% c("base", "basebottom")]
  expect_gt(length(base_ids), 0)
  base_mesh = convert_rgl_to_raymesh(include_ids = base_ids)
  expect_true(any(vapply(base_mesh$normals, nrow, integer(1)) > 0))
})

test_that("plot_3d rejects invalid normals flags before modifying a scene", {
  for (value in list(NULL, NA, 0, "false", c(TRUE, FALSE))) {
    expect_error(
      plot_3d(NULL, NULL, normals = value),
      "`normals` must be a single TRUE/FALSE value.",
      fixed = TRUE
    )
  }
})
