test_that("darkening preserves the chromaticity of saturated colors", {
  for (color in c(
    "#33d",
    "red",
    "green",
    "blue",
    "yellow",
    "magenta",
    "#355d44"
  )) {
    original = as.vector(grDevices::col2rgb(color)) / 255
    original_xyz = grDevices::convertColor(original, "sRGB", "XYZ")
    original_lightness = grDevices::convertColor(original, "sRGB", "Luv")[1]

    for (darkness in c(0.25, 0.5, 0.8)) {
      darker = darken_color(color, darken = darkness)
      darker_xyz = grDevices::convertColor(darker, "sRGB", "XYZ")
      darker_lightness = grDevices::convertColor(darker, "sRGB", "Luv")[1]

      expect_true(all(
        is.finite(darker) & darker >= 0 & darker <= original + 1e-8
      ))
      expect_equal(
        darker_xyz / sum(darker_xyz),
        original_xyz / sum(original_xyz),
        tolerance = 1e-4
      )
      expect_equal(
        darker_lightness,
        original_lightness * darkness,
        tolerance = 1e-4
      )
    }
  }
})

test_that("darkening handles black, neutral colors, and endpoint factors", {
  expect_equal(darken_color("black", darken = 0.5), c(0, 0, 0))
  expect_equal(darken_color("#33d", darken = 0), c(0, 0, 0))
  expect_equal(
    darken_color("#33d", darken = 1),
    c(51, 51, 221) / 255,
    tolerance = 1e-4
  )
  expect_equal(darken_color("#33d"), darken_color("#3333dd"))
  expect_equal(darken_color("#33d"), darken_color(c(51, 51, 221) / 255))
  expect_identical(
    convert_color(darken_color("white", 0.5), as_hex = TRUE),
    "#777777"
  )
})

test_that("plot_3d derives a dark blue rgl shadow from a blue background", {
  local_rgl_use_null()
  withr::defer(rgl::close3d())

  terrain = volcano_spatial()
  texture = height_shade(
    terrain,
    texture = grDevices::colorRampPalette(
      c("#355d44", "#7d8857", "#ae986b", "#dfd1b3")
    )(256)
  )
  plot_3d_test(
    texture,
    zscale = 10,
    theta = -35,
    phi = 30,
    fov = 0,
    zoom = 0.6,
    windowsize = c(600, 500),
    background = "#33d"
  )

  shadow = get_ids_with_labels(typeval = "shadow")
  shadow_material = rgl::material3d(id = shadow$id[1])
  shadow_image = png::readPNG(shadow_material$texture)
  center = ceiling(dim(shadow_image)[1:2] / 2)

  expect_false(shadow_material$lit)
  expect_equal(
    unname(shadow_image[1, 1, ]),
    c(51, 51, 221) / 255,
    tolerance = 1 / 255
  )
  expect_equal(
    unname(shadow_image[center[1], center[2], ]),
    c(25, 25, 125) / 255,
    tolerance = 1 / 255
  )
})

test_that("plot_3d honors an explicit shadow color on a blue background", {
  local_rgl_use_null()
  withr::defer(rgl::close3d())

  heightmap = matrix(1, nrow = 20, ncol = 30)
  plot_3d_test(
    constant_shade(heightmap),
    heightmap,
    background = "#33d",
    shadowcolor = "#551122",
    shadow_darkness = 0,
    shadowwidth = 5
  )

  shadow = get_ids_with_labels(typeval = "shadow")
  shadow_image = png::readPNG(rgl::material3d(id = shadow$id[1])$texture)
  center = ceiling(dim(shadow_image)[1:2] / 2)
  expect_equal(
    unname(shadow_image[center[1], center[2], ]),
    c(85, 17, 34) / 255,
    tolerance = 1 / 255
  )
})
