save_3d_test_png = function(code) {
  path = tempfile(fileext = ".png")
  code
  render_snapshot(path)
  path
}

compare_image = function(path1, path2) {
  image1 = png::readPNG(path1)
  image2 = png::readPNG(path2)
  return(identical(image1, image2))
}

run_tests = function(func, argument_grid, plot_prefix = "", ...) {
  stopifnot(inherits(argument_grid, "data.frame"))

  for (i in seq_len(nrow(argument_grid))) {
    args = unlist(argument_grid[i, ], recursive = FALSE)
    # test_filename = paste0(sprintf("%s-%s", substr(names(args),start_name,start_name + name_len), args),collapse="_")
    test_filename = sprintf("%s_test%i.png", plot_prefix, i)
    args = append(args, ...)
    save_3d_test_png(do.call(func, args = args)) |>
      suppressMessages() |>
      suppressWarnings() |>
      expect_snapshot_file(name = test_filename, compare = compare_image)
    Sys.sleep(0.1)
  }
  rgl::close3d()
}

test_that("plot_3d resizes rgl texture with rayshader.max_texture_size", {
  texture = array(1, dim = c(3, 5, 3))

  withr::local_options(list(rayshader.max_texture_size = 4))
  expect_message(
    resized_texture <- check_plot_3d_texture_size(texture),
    "rayshader.max_texture_size"
  )
  expect_equal(dim(resized_texture), c(2, 4, 3))

  withr::local_options(list(rayshader.max_texture_size = 5))
  expect_identical(check_plot_3d_texture_size(texture), texture)

  withr::local_options(list(rayshader.max_texture_size = Inf))
  expect_identical(check_plot_3d_texture_size(texture), texture)
})

test_that("plot_3d preserves full texture for rayrender conversion", {
  heightmap = matrix(0, nrow = 6, ncol = 10)
  hillshade = array(seq(0, 1, length.out = 6 * 10 * 3), dim = c(6, 10, 3))

  withr::local_options(list(rayshader.max_texture_size = 5))
  suppressMessages(plot_3d(
    hillshade,
    heightmap = heightmap,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    plot_new = TRUE,
    close_previous = TRUE
  ))
  withr::defer(rgl::close3d())

  surface = get_ids_with_labels(typeval = "surface_tris")
  rgl_texture = rgl::material3d(id = surface$id[1])$texture
  full_texture = surface$texture_file[[1]]

  expect_equal(dim(png::readPNG(rgl_texture)), c(3, 5, 3))
  expect_equal(dim(png::readPNG(full_texture)), c(6, 10, 3))

  texture_env = get(
    "ray_surface_texture_envir",
    envir = asNamespace("rayshader")
  )
  expect_gt(length(ls(texture_env)), 0)
  rgl::clear3d()
  expect_equal(nrow(get_ids_with_labels(typeval = "surface_tris")), 0)
  expect_equal(length(ls(texture_env)), 0)
})

test_that("plot_3d caps shadow texture resolution without changing shadow extent", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 80, ncol = 120)
  texture = constant_shade(heightmap)

  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    solid = FALSE,
    shadow = TRUE,
    shadowwidth = 12,
    shadow_texture_size = 40,
    windowsize = c(100, 100)
  ))

  shadow = get_ids_with_labels(typeval = "shadow")
  shadow_texture = rgl::material3d(id = shadow$id[1])$texture
  shadow_image = png::readPNG(shadow_texture)
  shadow_vertices = rgl::rgl.attrib(shadow$id[1], "vertices")

  expect_lte(max(dim(shadow_image)[1:2]), 40)
  expect_equal(range(shadow_vertices[, 1]), c(-51, 52), tolerance = 1e-8)
  expect_equal(range(shadow_vertices[, 3]), c(-71, 72), tolerance = 1e-8)
})

test_that("full resolution shadow textures can be requested", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  rgl::open3d(useNULL = TRUE)

  make_shadow(
    matrix(0, nrow = 20, ncol = 30),
    basedepth = -1,
    shadowwidth = 5,
    color = "white",
    shadowcolor = "grey50",
    shadow_texture_size = Inf
  )

  shadow = get_ids_with_labels(typeval = "shadow")
  shadow_texture = rgl::material3d(id = shadow$id[1])$texture
  shadow_image = png::readPNG(shadow_texture)

  expect_equal(dim(shadow_image)[1:2], c(40, 30))
  expect_identical(validate_shadow_texture_size(FALSE), Inf)
  expect_error(validate_shadow_texture_size(15), ">= 16")
})

test_that("colored shadow texture background matches scene background", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  rgl::open3d(useNULL = TRUE)

  background = "brown"
  make_shadow(
    matrix(0, nrow = 20, ncol = 30),
    basedepth = -1,
    shadowwidth = 5,
    color = background,
    shadowcolor = "grey25",
    shadow_texture_size = Inf
  )

  shadow = get_ids_with_labels(typeval = "shadow")
  shadow_texture = rgl::material3d(id = shadow$id[1])$texture
  shadow_image = png::readPNG(shadow_texture)

  expect_equal(
    unname(shadow_image[1, 1, ]),
    as.vector(col2rgb(background)) / 255,
    tolerance = 1 / 255
  )
})

test_that("plot_3d plots basic options", {
  skip_if(
    rgl::rgl.useNULL(),
    message = "rgl.useNULL is TRUE--not testing raw rgl snapshots"
  )

  hillshade = sphere_shade(volcano)
  volcano_na = volcano
  volcano_na[60:80, 30:40] = NA

  plot_3d_args_meshing = expand.grid(
    zscale = list(1, 3),
    baseshape = list("rectangle", "circle", "hex"),
    solid = list(TRUE, FALSE),
    soliddepth = list("auto", -100, 200),
    shadow = list(TRUE, FALSE)
  )

  plot_3d_args_meshing_subset = expand.grid(
    water = list(TRUE, FALSE),
    waterdepth = list(150, 300),
    solid = list(TRUE, FALSE),
    shadow = list(TRUE, FALSE),
    shadowdepth = list(-100, 0, 200),
    soliddepth = list("auto", -100, 200)
  )

  run_tests(
    "plot_3d_test",
    plot_3d_args_meshing,
    plot_prefix = "basic",
    list(
      hillshade = hillshade,
      heightmap = volcano,
      windowsize = 200,
      close_previous = FALSE,
      clear_previous = TRUE,
      plot_new = FALSE
    )
  )
  run_tests(
    "plot_3d_test",
    plot_3d_args_meshing_subset,
    plot_prefix = "na",
    list(
      hillshade = hillshade,
      heightmap = volcano_na,
      windowsize = 200,
      close_previous = FALSE,
      clear_previous = TRUE,
      plot_new = FALSE
    )
  )
})

test_that("plot_3d plots color options", {
  skip_if(
    rgl::rgl.useNULL(),
    message = "rgl.useNULL is TRUE--not testing raw rgl snapshots"
  )

  hillshade = sphere_shade(volcano)
  plot_3d_args_colors = expand.grid(
    solidlinecolor = list(NULL, "#d8b8c8"),
    solidcolor = list("grey20", "#28b8d8"),
    shadowcolor = list("auto", "red"),
    shadow_darkness = list(0.5, 0.2),
    background = list("white", "purple")
  )

  plot_3d_args_water_colors = expand.grid(
    water = list(TRUE),
    waterdepth = list(150),
    watercolor = list("lightblue", "green"),
    wateralpha = list(0.5, 1)
  )

  run_tests(
    "plot_3d_test",
    plot_3d_args_colors,
    plot_prefix = "color",
    list(
      hillshade = hillshade,
      heightmap = volcano,
      windowsize = 200,
      close_previous = FALSE,
      clear_previous = TRUE,
      plot_new = FALSE
    )
  )
  run_tests(
    "plot_3d_test",
    plot_3d_args_water_colors,
    plot_prefix = "wcolor",
    list(
      hillshade = hillshade,
      heightmap = volcano,
      windowsize = 200,
      close_previous = FALSE,
      clear_previous = TRUE,
      plot_new = FALSE
    )
  )
})

test_that("plot_3d triangulation", {
  skip_if(
    rgl::rgl.useNULL(),
    message = "rgl.useNULL is TRUE--not testing raw rgl snapshots"
  )

  hillshade = sphere_shade(volcano)
  plot_3d_args_triangulation = expand.grid(
    triangulate = list(TRUE),
    max_error = list(0, 0.001, 1, 10),
    max_tri = list(0, 100, 1000)
  )
  run_tests(
    "plot_3d_test",
    plot_3d_args_triangulation,
    plot_prefix = "triangl",
    list(
      hillshade = hillshade,
      heightmap = volcano,
      windowsize = 200,
      close_previous = FALSE,
      clear_previous = TRUE,
      plot_new = FALSE
    )
  )
})

test_that("plot_3d plots line options", {
  skip_if(
    rgl::rgl.useNULL(),
    message = "rgl.useNULL is TRUE--not testing raw rgl snapshots"
  )

  hillshade = sphere_shade(volcano)
  plot_3d_args_lines = expand.grid(
    water = list(TRUE, FALSE),
    waterdepth = list(0, 150),
    waterlinecolor = list(NULL, "red"),
    waterlinealpha = list(0.5, 1),
    linewidth = list(2, 5),
    lineantialias = list(FALSE, TRUE)
  )

  run_tests(
    "plot_3d_test",
    plot_3d_args_lines,
    plot_prefix = "line",
    list(
      hillshade = hillshade,
      heightmap = volcano,
      windowsize = 200,
      close_previous = FALSE,
      clear_previous = TRUE,
      plot_new = FALSE
    )
  )
})

test_that("plot_3d plots soil options", {
  skip_if(
    rgl::rgl.useNULL(),
    message = "rgl.useNULL is TRUE--not testing raw rgl snapshots"
  )

  hillshade = sphere_shade(volcano)
  plot_3d_args_soil = expand.grid(
    soil = list(TRUE),
    soil_freq = list(0.1, 0.5),
    soil_levels = list(1, 32),
    soil_color_light = list("#b39474", "#ffdfd4"),
    soil_color_dark = list("#8a623b", "grey20"),
    soil_gradient = list(0, 2, 8),
    soil_gradient_darken = list(1, 4, 8)
  )

  set.seed(1)
  run_tests(
    "plot_3d_test",
    plot_3d_args_soil,
    plot_prefix = "soil",
    list(
      hillshade = hillshade,
      heightmap = volcano,
      windowsize = 200,
      close_previous = FALSE,
      clear_previous = TRUE,
      plot_new = FALSE
    )
  )
})

test_that("test raymesh conversion", {
  volcano |>
    sphere_shade() |>
    plot_3d_test(volcano, zscale = 2)
  raymesh = convert_rgl_to_raymesh()
  rgl::close3d()
  raymesh$material_hashes = as.character(c(1, 2, 3))
  raymesh$materials[[1]][[1]]$diffuse_texname = "texture_location"
  raymesh$materials[[2]][[1]]$diffuse_texname = "texture_location"
  raymesh$materials[[3]][[1]]$diffuse_texname = "texture_location"
  raymesh$materials[[2]][[1]]$diffuse = round(
    raymesh$materials[[2]][[1]]$diffuse,
    digits = 8
  )
  class(raymesh) = "list"
  attr(raymesh, "material_hashes") = c(1, 2, 3)
  expect_snapshot_value(as.list(raymesh), style = "json2")
})
