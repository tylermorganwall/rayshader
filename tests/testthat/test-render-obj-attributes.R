test_that('OBJ attribute batching retains last valid corner values and missing indices', {
  skip_if_not_installed('rayvertex')
  local_rgl_use_null()
  withr::defer(if (rgl::cur3d() != 0) rgl::close3d())
  withr::defer(reset_scene_context())
  directory = withr::local_tempdir()
  png::writePNG(array(1, c(2, 2, 3)), file.path(directory, 'texture.png'))
  writeLines(
    c('newmtl textured', 'Kd 1 1 1', 'map_Kd texture.png'),
    file.path(directory, 'mesh.mtl')
  )
  writeLines(
    c(
      'mtllib mesh.mtl',
      'v 0 0 0',
      'v 1 0 0',
      'v 0 1 0',
      'v 0 0 1',
      'vt 0 0',
      'vt 1 0',
      'vt 0 1',
      'vt 1 1',
      'vn 0 0 1',
      'vn 1 0 0',
      'usemtl textured',
      'f 1/1/1 2/2/1 3/3/1',
      'f 1/4/2 3/3/2 4/2/2',
      'f 2 4 3'
    ),
    file.path(directory, 'mesh.obj')
  )
  rgl::open3d()
  cache_scene_context_token()
  render_obj(
    file.path(directory, 'mesh.obj'),
    xyz = matrix(c(0, 0, 0), ncol = 3),
    load_material = TRUE,
    load_normals = TRUE,
    rgl_tag = 'attributes'
  )
  ids = rgl::ids3d(tags = TRUE)
  id = ids$id[ids$tag == 'objattributes']
  expect_length(id, 1L)
  vertices = rgl::rgl.attrib(id, 'vertices')
  normals = rgl::rgl.attrib(id, 'normals')
  texture = rgl::rgl.attrib(id, 'texcoords')
  # Vertices 1 and 3 are revisited with a different normal. The later face
  # without attributes must not overwrite their last valid assignments.
  expected_vertices = rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  expected_normals = rbind(c(1, 0, 0), c(0, 0, 1), c(1, 0, 0), c(1, 0, 0))
  expected_texture = rbind(c(1, 1), c(1, 0), c(0, 1), c(1, 0))
  expect_equal(unname(vertices), expected_vertices)
  expect_equal(unname(normals), expected_normals)
  expect_equal(unname(texture), expected_texture)
})
