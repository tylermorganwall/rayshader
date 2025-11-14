#'@title Calculate Terrain Color Map
#'
#'@description Calculates a color for each point on the surface using a direct elevation-to-color mapping.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'@param texture Default `terrain.colors(256)`. A color palette for the plot.
#'@param range Default `NULL`, the full range of the heightmap. A length-2 vector specifying the maximum
#'and minimum values to map the color palette to.
#'@return RGB array of hillshaded texture mappings.
#'@export
#'@examples
#'#Create a direct mapping of elevation to color:
#'montereybay |>
#'  height_shade() |>
#'  plot_map()
#'
#'#Add a shadow:
#'if(run_documentation()) {
#'montereybay |>
#'  height_shade() |>
#'  add_shadow(ray_shade(montereybay,zscale=50),0.1) |>
#'  plot_map()
#'}
#'
#'#Change the palette:
#'if(run_documentation()) {
#'montereybay |>
#'  height_shade(texture = topo.colors(256)) |>
#'  add_shadow(ray_shade(montereybay,zscale=50),0.1) |>
#'  plot_map()
#'}
#'
#'#Really change the palette (warning: gratuitous use of rainbow palette):
#'if(run_documentation()) {
#'montereybay |>
#'  height_shade(texture = rainbow(256)) |>
#'  add_shadow(ray_shade(montereybay,zscale=50),0.1) |>
#'  plot_map()
#'}
height_shade = function(
  heightmap,
  texture = grDevices::colorRampPalette(c("#6AA85B", "#D9CC9A", "#FFFFFF"))(
    256
  ),
  range = NULL
) {
  if (!is.null(range)) {
    range = base::sort(range[1:2])
  } else {
    range = range(heightmap, na.rm = TRUE)
  }
  if (any(!is.finite(range))) {
    stop(
      "`heightmap` must contain at least one finite value to determine `range`."
    )
  }
  height_values = t(heightmap)
  na_mask = !is.finite(height_values)
  range_diff = range[2] - range[1]
  if (range_diff <= 0) {
    scaled = matrix(
      0,
      nrow = nrow(height_values),
      ncol = ncol(height_values)
    )
  } else {
    scaled = (height_values - range[1]) / range_diff
    scaled = pmin(pmax(scaled, 0), 1)
  }
  scaled[na_mask] = 0
  color_table = convert_color(grDevices::col2rgb(texture) / 255, linear = TRUE)
  if (ncol(color_table) == 0) {
    stop("`texture` must contain at least one color.")
  }
  color_index = floor(scaled * (ncol(color_table) - 1)) + 1L
  color_index[!is.finite(color_index)] = 1L
  return_array = array(1, dim = c(nrow(height_values), ncol(height_values), 4))
  set_channel = function(channel) {
    matrix(
      color_table[channel, ][color_index],
      nrow = nrow(height_values),
      ncol = ncol(height_values)
    )
  }
  return_array[,, 1] = set_channel(1)
  return_array[,, 2] = set_channel(2)
  return_array[,, 3] = set_channel(3)
  if (any(na_mask)) {
    bg_color = convert_color("white", linear = TRUE)
    for (i in 1:3) {
      channel = return_array[,, i]
      channel[na_mask] = bg_color[i]
      return_array[,, i] = channel
    }
  }
  return_array[,, 4] = 1
  return(rayimage::ray_read_image(
    return_array,
    assume_colorspace = rayimage::CS_SRGB,
    assume_white = "D65"
  ))
}
