#' @title make_shadow
#'
#' @description Makes the base below the 3D elevation map.
#'
#' @param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#' @param basedepth Depth of the shadow layer.
#' @param shadowwidth Width of the shadow in matrix units.
#' @param color Background color.
#' @param shadowcolor Shadow color.
#' @param offset Default `c(0, 0)`. Shadow plane offset.
#' @param shadow_texture_size Default `getOption("rayshader.max_shadow_texture_size", 1024)`. Maximum width or height, in pixels, of the blurred shadow texture.
#' @keywords internal
make_shadow = function(
  heightmap,
  basedepth,
  shadowwidth,
  color,
  shadowcolor,
  offset = c(0, 0),
  shadow_texture_size = getOption("rayshader.max_shadow_texture_size", 1024)
) {
  rows = nrow(heightmap)
  cols = ncol(heightmap)
  shadowwidth = as.integer(round(shadowwidth))
  shadowwidth = max(shadowwidth, 0L)
  shadow_texture_size = validate_shadow_texture_size(shadow_texture_size)
  colors = col2rgb(color) / 255
  shadowcolors = col2rgb(shadowcolor) / 255
  shadow_rows = rows + shadowwidth * 2L
  shadow_cols = cols + shadowwidth * 2L
  na_depth = matrix(FALSE, nrow = shadow_rows, ncol = shadow_cols)
  na_depth[
    (shadowwidth + 1):(rows + shadowwidth),
    (shadowwidth + 1):(cols + shadowwidth)
  ] = is.na(heightmap)

  shadow_mask = matrix(0, nrow = shadow_rows, ncol = shadow_cols)
  shadow_mask[
    (shadowwidth + 1):(rows + shadowwidth),
    (shadowwidth + 1):(cols + shadowwidth)
  ] = 1
  shadow_mask[fliplr(na_depth)] = 0
  shadow_mask = t(shadow_mask)

  resized_shadow = resize_shadow_mask(
    shadow_mask,
    shadowwidth = shadowwidth,
    shadow_texture_size = shadow_texture_size
  )
  shadow_mask = resized_shadow$mask
  shadowwidth_texture = resized_shadow$shadowwidth

  tempmap = tempfile(fileext = ".png")
  has_rayimage = length(find.package("rayimage", quiet = TRUE)) > 0
  if (has_rayimage) {
    blurred_shadow = rayimage::render_convolution_fft(
      shadow_mask,
      kernel = rayimage::generate_2d_gaussian(
        dim = rep(max(1L, as.integer(round(shadowwidth_texture / 2))), 2L)
      )
    )
  } else {
    warning(
      "`rayimage` package required for smooth shadow--using basic shadow instead."
    )
    blurred_shadow = shadow_mask
  }
  shadowarray = colorize_shadow_mask(
    blurred_shadow,
    colors = colors,
    shadowcolors = shadowcolors
  )
  png::writePNG(shadowarray, tempmap)

  rowmin = min((-shadowwidth + 1):(rows + shadowwidth) - rows / 2) + offset[1]
  rowmax = max((-shadowwidth + 1):(rows + shadowwidth) - rows / 2) + offset[1]
  colmin = min(-(-shadowwidth + 1):-(cols + shadowwidth) + cols / 2 + 1) +
    offset[2]
  colmax = max(-(-shadowwidth + 1):-(cols + shadowwidth) + cols / 2 + 1) +
    offset[2]

  tri1 = matrix(
    c(
      rowmax,
      rowmax,
      rowmin,
      basedepth,
      basedepth,
      basedepth,
      colmax,
      colmin,
      colmin
    ),
    nrow = 3,
    ncol = 3
  )
  tri2 = matrix(
    c(
      rowmin,
      rowmax,
      rowmin,
      basedepth,
      basedepth,
      basedepth,
      colmax,
      colmax,
      colmin
    ),
    nrow = 3,
    ncol = 3
  )

  rgl::triangles3d(
    x = rbind(tri1, tri2),
    texcoords = matrix(
      c(1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0),
      nrow = 6,
      ncol = 2
    ),
    texture = tempmap,
    color = "white",
    lit = FALSE,
    back = "culled",
    tag = "shadow"
  )
}

#'@keywords internal
validate_shadow_texture_size = function(shadow_texture_size) {
  if (isFALSE(shadow_texture_size) || identical(shadow_texture_size, Inf)) {
    return(Inf)
  }
  if (
    !is.numeric(shadow_texture_size) ||
      length(shadow_texture_size) != 1 ||
      is.na(shadow_texture_size) ||
      shadow_texture_size < 16
  ) {
    stop(
      "`shadow_texture_size` must be a single number >= 16, `Inf`, or `FALSE`.",
      call. = FALSE
    )
  }
  shadow_texture_size
}

#'@keywords internal
resize_shadow_mask = function(
  shadow_mask,
  shadowwidth,
  shadow_texture_size = Inf
) {
  shadow_scale = 1
  if (is.finite(shadow_texture_size)) {
    shadow_scale = min(1, shadow_texture_size / max(dim(shadow_mask)))
  }
  if (shadow_scale < 1) {
    resized_dims = pmax(1L, as.integer(round(dim(shadow_mask) * shadow_scale)))
    shadow_mask = rayimage::render_resized(
      shadow_mask,
      dims = resized_dims,
      method = "tri"
    )
  }
  list(
    mask = shadow_mask,
    shadowwidth = max(1, as.integer(round(shadowwidth * shadow_scale)))
  )
}

#'@keywords internal
colorize_shadow_mask = function(
  shadow_mask,
  colors,
  shadowcolors
) {
  shadow_mask = pmin(pmax(shadow_mask, 0), 1)
  shadowarray = array(1, dim = c(dim(shadow_mask), 3L))
  for (channel in seq_len(3L)) {
    shadowarray[,, channel] = colors[channel] +
      shadow_mask * (shadowcolors[channel] - colors[channel])
  }
  shadowarray
}
