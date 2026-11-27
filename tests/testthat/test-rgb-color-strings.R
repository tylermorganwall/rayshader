test_that("RGB strings use byte-scale channels while numeric vectors stay normalized", {
  expected = c(129, 123, 201) / 255
  for (color in c(
    "rgb(129, 123, 201)",
    "rgb(129,123,201)",
    " RGB ( 129 , 123 , 201 ) ",
    "rgb(129.0,\t123.0,201)"
  )) {
    expect_equal(convert_color(color), expected)
    expect_identical(toupper(convert_color(color, as_hex = TRUE)), "#817BC9")
  }
  expect_equal(convert_color(expected), expected)
  expect_equal(convert_color("rgb(0, 0, 0)"), c(0, 0, 0))
  expect_equal(convert_color("rgb(255, 255, 255)"), c(1, 1, 1))
  expect_equal(convert_color("rgb(1, 0, 0)"), c(1 / 255, 0, 0))
  expect_equal(
    convert_color("rgb(12.5, 0, 255)"),
    convert_color(grDevices::rgb(12.5, 0, 255, maxColorValue = 255))
  )
})

test_that("RGB normalization preserves mixed palettes and non-string inputs", {
  colors = c(
    css = "rgb(129, 123, 201)",
    named = "red",
    hex = "#33d",
    alpha = "#11223380",
    missing = NA_character_,
    clear = "transparent"
  )
  expected = colors
  expected[1] = "#817BC9"
  expect_identical(normalize_rgb_color(colors), expected)
  expect_identical(normalize_rgb_color(character()), character())
  for (value in list(NULL, NA, TRUE, c(0.1, 0.2, 0.3), matrix(0.5, 2, 3))) {
    expect_identical(normalize_rgb_color(value), value)
  }
  expect_equal(convert_color(c("rgb(255, 0, 0)", "blue")), c(1, 0, 0, 0, 0, 1))
  expect_error(convert_color("not-a-color"), "invalid color")
})

test_that("invalid RGB strings fail without evaluating their contents", {
  for (color in c(
    "rgb(256, 0, 0)",
    "rgb(-1, 0, 0)",
    "rgb(1, 2)",
    "rgb(1, 2, 3, 4)",
    "rgb(1, 2,)",
    "rgb(NA, 2, 3)",
    "rgb(Inf, 2, 3)",
    "rgb(1%, 2%, 3%)",
    "rgb(1, 2, 3) trailing",
    "rgb(stop('evaluated'), 2, 3)"
  )) {
    expect_error(convert_color(color), "RGB color")
  }
})

test_that("RGB strings preserve linear color conversion and terrain shading", {
  color = "rgb(129, 123, 201)"
  hex = "#817BC9"
  heightmap = matrix(seq_len(100), 10, 10)
  expect_equal(
    convert_color(color, linear = TRUE),
    convert_color(hex, linear = TRUE)
  )
  expect_equal(col2rgb_linear(color), col2rgb_linear(hex))
  expect_equal(constant_shade(heightmap, color), constant_shade(heightmap, hex))
  expect_equal(
    height_shade(heightmap, texture = c("black", color)),
    height_shade(heightmap, texture = c("black", hex))
  )
  for (old_method in c(FALSE, TRUE)) {
    expect_equal(
      create_texture(
        color,
        color,
        color,
        color,
        color,
        corner_colors = rep(color, 4),
        old_method = old_method
      ),
      create_texture(
        hex,
        hex,
        hex,
        hex,
        hex,
        corner_colors = rep(hex, 4),
        old_method = old_method
      )
    )
  }
})

test_that("RGB strings work in rgl scenes and the high-quality ground material", {
  local_rgl_use_null()
  on.exit(rgl::close3d(), add = TRUE)
  color = "rgb(129, 123, 201)"
  heightmap = matrix(1:100, 10, 10)
  plot_3d_test(
    constant_shade(heightmap, color),
    heightmap,
    background = color,
    solid_color = color,
    solid_line_color = NULL,
    shadow = TRUE,
    shadow_color = color,
    zscale = 10
  )
  background = rgl::ids3d(type = "background")$id[1]
  expect_equal(
    unname(rgl::material3d(id = background)$color),
    "#817BC9",
    ignore_attr = TRUE
  )
  render_zaxis(color = color, breaks = c(0, 50, 100))
  ids = rgl::ids3d(tags = TRUE)
  axis = ids$id[ids$tag == "zaxis_axis"][1]
  expect_equal(
    unname(rgl::rgl.attrib(axis, "colors")[1, 1:3]),
    c(129, 123, 201) / 255,
    tolerance = 1e-6
  )
  render_points(
    x = 1,
    y = 1,
    altitude = 50,
    color = color,
    extent = c(0, 10, 0, 10)
  )
  points = get_ids_with_labels(typeval = "points3d")$id
  expect_true(length(points) > 0)
  expected = rayrender::diffuse(color = "#817BC9")
  ground = rayrender::diffuse(color = color)
  expect_equal(ground, expected)
  scene = render_highquality(
    point_radius = 5,
    light = FALSE,
    iso = 1,
    ground_material = ground,
    return_scene = TRUE
  )
  expect_s3_class(scene, "data.frame")
  expect_true(any(vapply(
    scene$material,
    function(material) identical(material$properties, ground[[1]]$properties),
    logical(1)
  )))
})
