#'@title Construct the Georeferenced Volcano Raster
#'
#'@description Constructs the georeferenced volcano raster cached when the
#'rayshader namespace loads.
#'
#'@return A one-layer `terra::SpatRaster`.
#'@keywords internal
#'@noRd
make_volcano_spatial = function() {
  volcano_matrix = datasets::volcano

  volcano_matrix = volcano_matrix[
    rev(seq_len(nrow(volcano_matrix))),
    rev(seq_len(ncol(volcano_matrix))),
    drop = FALSE
  ]

  volcano_raster = terra::rast(
    nrows = nrow(volcano_matrix),
    ncols = ncol(volcano_matrix),
    xmin = 2667400,
    xmax = 2668010,
    ymin = 6478700,
    ymax = 6479570,
    crs = "EPSG:27200",
    vals = as.vector(t(volcano_matrix)),
    names = "elevation"
  )
  terra::units(volcano_raster) = "m"
  volcano_raster
}

#'@title Georeferenced Maungawhau Volcano Elevation Raster
#'
#'@description Returns the base R `datasets::volcano` elevation model as a
#'georeferenced, one-layer `terra::SpatRaster`. Elevation is measured in meters,
#'and horizontal coordinates use EPSG:27200, NZGD49 / New Zealand Map Grid, on
#'a 10-meter grid.
#'
#'The elevation data were digitized from a topographic map and should not be
#'treated as accurate survey data. A new independent copy of the raster is
#'returned on every call.
#'
#'@return A one-layer `terra::SpatRaster` containing elevation in meters.
#'@source The base R [datasets::volcano] data and the Geomorphometry.org
#'["Volcano Maungawhau" reconstruction](https://www.geomorphometry.org/2009/08/20/volcano-maungawhau/).
#'@seealso [raster_to_matrix()]
#'@export
#'@examples
#' volcano_dem = volcano_spatial()
#' terra::ext(volcano_dem)
#' terra::res(volcano_dem)
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' volcano_dem |>
#'   height_shade() |>
#'   add_shadow(ray_shade(),0) |>
#'   plot_3d()
volcano_spatial = function() {
  terra::deepcopy(ray_volcano_envir$volcano)
}
