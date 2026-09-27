#'@title Create Texture
#'
#'@description Creates a texture map based on 5 user-supplied colors.
#'
#'@param light_color The main highlight color. Corresponds to the top center of the texture map.
#'@param shadow_color The main shadow color. Corresponds to the bottom center of the texture map. This color represents slopes directed
#'directly opposite to the main highlight color.
#'@param left_color The left fill color. Corresponds to the left center of the texture map. This color represents slopes directed
#'90 degrees to the left of the main highlight color.
#'@param right_color The right fill color. Corresponds to the right center of the texture map. This color represents slopes directed
#'90 degrees to the right of the main highlight color.
#'@param center_color The center color. Corresponds to the center of the texture map. This color represents flat areas.
#'@param corner_colors Default `NULL`. The colors at the corners, in this order: NW, NE, SW, SE. If this vector isn't present (or
#'all corners are specified), the mid-points will just be interpolated from the main colors.
#'@param old_method Default `FALSE`. Whether to use the original method rayshader used for computing textures,
#'which had non-standard color handling.
#'@param darken Default `1`. Amount to darken the colors.
#'@export
#'@examples
#'#Here is the `imhof1` palette:
#'create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9") |>
#'  plot_map()
#'
#'#Here is the `unicorn` palette:
#'create_texture("red","green","blue","yellow","white") |>
#'  plot_map()
create_texture = function(
  light_color,
  shadow_color,
  left_color,
  right_color,
  center_color,
  corner_colors = NULL,
  old_method = FALSE,
  darken = 1
) {
  if (old_method) {
    lightrgb = col2rgb(light_color)
    shadowrgb = col2rgb(shadow_color)
    leftrgb = col2rgb(left_color)
    rightrgb = col2rgb(right_color)
    centerrgb = col2rgb(center_color)
    if (is.null(corner_colors) || length(corner_colors) != 4) {
      nw_corner = (lightrgb + leftrgb) / 2
      ne_corner = (lightrgb + rightrgb) / 2
      sw_corner = (shadowrgb + leftrgb) / 2
      se_corner = (shadowrgb + rightrgb) / 2
    } else {
      cornercolorsrgb = lapply(corner_colors, col2rgb)
      nw_corner = cornercolorsrgb[[1]]
      ne_corner = cornercolorsrgb[[2]]
      se_corner = cornercolorsrgb[[3]]
      sw_corner = cornercolorsrgb[[4]]
    }
    colorarray = array(0, dim = c(3, 3, 3))
    for (i in 1:3) {
      #center
      colorarray[2, 2, i] = centerrgb[i]

      #edges
      colorarray[1, 2, i] = lightrgb[i]
      colorarray[2, 1, i] = leftrgb[i]
      colorarray[2, 3, i] = rightrgb[i]
      colorarray[3, 2, i] = shadowrgb[i]

      #corners
      colorarray[1, 1, i] = nw_corner[i]
      colorarray[1, 3, i] = ne_corner[i]
      colorarray[3, 1, i] = se_corner[i]
      colorarray[3, 3, i] = sw_corner[i]
    }
    returnarray = array(0, dim = c(512, 512, 3))
    for (i in 1:3) {
      returnarray[,, i] = bilineargrid(colorarray[,, i] / 256)
    }
    returnarray
  } else {
    # Get sRGB bytes
    to01 = function(col) {
      as.numeric(col2rgb_linear(col))
    }

    l = to01(light_color)
    s = to01(shadow_color)
    lf = to01(left_color)
    rf = to01(right_color)
    c0 = to01(center_color)

    # Optional corners
    if (is.null(corner_colors) || length(corner_colors) != 4) {
      nw = (l + lf) / 2
      ne = (l + rf) / 2
      sw = (s + lf) / 2
      se = (s + rf) / 2
    } else {
      corners = lapply(corner_colors, to01)
      nw = corners[[1]]
      ne = corners[[2]]
      sw = corners[[3]]
      se = corners[[4]]
    }

    # Build control grid in sRGB, then decode to linear
    grid_srgb = array(0, dim = c(3, 3, 3))
    for (i in seq_len(3)) {
      grid_srgb[2, 2, i] = c0[i]
      grid_srgb[1, 2, i] = l[i]
      grid_srgb[3, 2, i] = s[i]
      grid_srgb[2, 1, i] = lf[i]
      grid_srgb[2, 3, i] = rf[i]
      grid_srgb[1, 1, i] = nw[i]
      grid_srgb[1, 3, i] = ne[i]
      grid_srgb[3, 1, i] = sw[i]
      grid_srgb[3, 3, i] = se[i]
    }

    grid_lin = #rayimage::render_convert_colorspace(
      rayimage::ray_read_image(
        grid_srgb,
        assume_colorspace = rayimage::CS_SRGB,
        assume_white = "D65"
      )
    # to_mats = rayimage::CS_ACESCG,
    # to_white = "D65"
    # )

    # Bilinear interpolation in linear
    # out = array(0, dim = c(512, 512, 3))
    # for (i in 1:3) {
    # 	out[,, i] = rayimage::render(grid_lin[,, i])
    # }
    # linear, rayimage::plot_image will do the sRGB encode
    return(rayimage::render_resized(
      grid_lin,
      dims = c(512, 512),
      method = "bilinear"
    ))
    # return(rayimage::ray_read_image(
    # 	out,
    # 	assume_colorspace = rayimage::CS_ACESCG,
    # 	assume_white = "D65"
    # ))
  }
}
