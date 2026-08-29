#'@title Calculate Normal
#'
#'@description Calculates the normal unit vector for every point on the grid.
#'
#' @param heightmap A two-dimensional matrix or spatial raster containing
#' elevation values.
#' @param zscale Default `1`. Ratio of horizontal spacing to elevation units.
#' @param progbar Default `FALSE`. If `TRUE`, turns on progress bar.
#' @param geographic_aspect Default `TRUE`. Correct unequal metric x/y cell
#' spacing using the input extent and CRS.
#' @param extent Default `NULL`. Spatial extent for a matrix heightmap.
#' @param crs Default `NULL`. CRS to assign to the input before calculating
#' metric cell spacing.
#'@return Matrix of light intensities at each point.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Here we produce a light intensity map of the `volcano` elevation map.
#'
#'#Cache the normal vectors of the volcano dataset
#'volcanocache = calculate_normal(volcano)
#'
#'#Use the cached vectors to speed up calculation of `sphere_shade()` on a map.
#'sphere_shade(volcano,normalvectors = volcanocache) |>
#'  plot_map()
calculate_normal = function(
  heightmap,
  zscale = 1,
  progbar = FALSE,
  geographic_aspect = TRUE,
  extent = NULL,
  crs = NULL
) {
  zscale_missing = missing(zscale)
  heightmap_info = coerce_plot_3d_heightmap(
    heightmap,
    extent = extent,
    crs = crs,
    geographic_aspect = geographic_aspect
  )
  heightmap = heightmap_info$heightmap
  if (
    zscale_missing &&
      is.finite(heightmap_info$zscale) &&
      heightmap_info$zscale > 0
  ) {
    zscale = heightmap_info$zscale
  }
  aspect = heightmap_info$geographic_aspect
  heightmap = add_padding(heightmap)
  heightmap = heightmap / zscale
  matrices = calculate_normal_cpp(
    heightmap = heightmap,
    progbar = progbar,
    column_scale = aspect$scale[["z"]],
    row_scale = aspect$scale[["x"]]
  )
  matrices$x[is.na(heightmap)] = NA
  matrices$y[is.na(heightmap)] = NA
  matrices$z[is.na(heightmap)] = NA

  returnnormal = list()
  returnnormal[["x"]] = t(matrices$x)
  returnnormal[["y"]] = t(matrices$y)
  returnnormal[["z"]] = t(matrices$z)
  attr(returnnormal, "geographic_aspect") = aspect
  return(returnnormal)
}
