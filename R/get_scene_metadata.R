#' Get Cached Scene Metadata
#'
#' @description Returns the spatial and 3D coordinate metadata cached for the
#' active scene created by [plot_3d()] or [plot_gg()]. This provides the
#' information needed to transform user data or size procedural geometry to the
#' rendered terrain without reaching into rayshader's internal cache.
#'
#' The returned `extent_3d` uses conventional spatial axes: x, y, and elevation
#' z in the cached heightmap's units. For [plot_3d()] these are the input
#' elevation units; [plot_gg()] may use a transformed height scale.
#' `scene_bounds` uses rgl axes: x, vertical y, and horizontal z. Spatial y
#' increases toward negative scene z, and cached heightmap elevations are divided
#' by `effective_zscale` to obtain scene y coordinates.
#'
#' @param include_heightmap Default `FALSE`. Whether to include the cached
#' height matrix in the returned list. The heightmap can be large; its dimensions
#' and elevation range are always returned.
#'
#' @return A named list containing:
#' * `scene_id`: The active rgl scene identifier.
#' * `plot_type`: Either `"plot_3d"` or `"plot_gg"`.
#' * `extent`: The cached x/y extent, or `NULL` when there is no single extent.
#' * `extent_3d`: The x/y extent plus `zmin` and `zmax` elevation bounds.
#' * `crs`: The scene coordinate reference system, when available.
#' * `zscale`, `vertical_exaggeration`, and `effective_zscale`: The cached base
#'   elevation scale, relief multiplier, and their effective ratio.
#' * `heightmap_dimensions`, `elevation_range`, and `scene_elevation_range`:
#'   Terrain matrix dimensions and vertical bounds in input and scene units.
#' * `scene_bounds`, `scene_center`, and `scene_dimensions`: Terrain bounds,
#'   center, and size in rgl x/y/z coordinates.
#' * `geographic_aspect`: Cached horizontal aspect and physical cell metadata.
#' * `meters_per_scene_unit` and `map_units_per_scene_unit`: Available physical
#'   and coordinate-unit conversions for horizontal scene distances.
#' * `scene_axis_mapping`: Mapping from spatial axes to rgl scene axes.
#' * `triangulate`: Whether the terrain used mesh simplification.
#' * `panel_extents` and `panel_info`: Cached [plot_gg()] panel metadata, when
#'   applicable.
#' * `sources`: Labels describing the expressions that populated core cache
#'   values.
#' * `heightmap`: The cached matrix when `include_heightmap = TRUE`, otherwise
#'   `NULL`.
#'
#' @export
#'
#' @examplesIf interactive()
#' volcano_spatial() |>
#'   height_shade() |>
#'   plot_3d()
#'
#' scene_info = get_scene_metadata()
#' scene_info$extent_3d
#' scene_info$scene_bounds
get_scene_metadata = function(include_heightmap = FALSE) {
  if (
    !is.logical(include_heightmap) ||
      length(include_heightmap) != 1L ||
      is.na(include_heightmap)
  ) {
    stop(
      "`include_heightmap` must be a single TRUE/FALSE value.",
      call. = FALSE
    )
  }
  if (!is_current_scene_context()) {
    stop(
      "No cached scene metadata found. Call `plot_3d()` or `plot_gg()` first.",
      call. = FALSE
    )
  }

  heightmap = get_scene_heightmap(default = NULL)
  zscale = get_scene_zscale(default = NA_real_)
  vertical_exaggeration = get_scene_vertical_exaggeration(default = 1)
  effective_zscale = get_scene_effective_zscale(default = NA_real_)
  if (
    !is.matrix(heightmap) ||
      !is.finite(zscale) ||
      !is.finite(vertical_exaggeration) ||
      !is.finite(effective_zscale)
  ) {
    stop(
      "The active scene does not have a complete metadata cache.",
      call. = FALSE
    )
  }

  extent = get_scene_extent(default = NULL)
  if (!is.null(extent)) {
    extent = tryCatch(
      get_extent(extent),
      error = function(error) NULL
    )
  }

  panel_info = get_scene_context_value("plot_gg_panel_info", default = NULL)
  panel_extents = NULL
  if (is.data.frame(panel_info) && nrow(panel_info)) {
    panel_extents = lapply(seq_len(nrow(panel_info)), function(panel_index) {
      c(
        xmin = panel_info$extent_xmin[[panel_index]],
        xmax = panel_info$extent_xmax[[panel_index]],
        ymin = panel_info$extent_ymin[[panel_index]],
        ymax = panel_info$extent_ymax[[panel_index]]
      )
    })
    names(panel_extents) = paste0("panel_", panel_info$panel)
  }

  crs = get_scene_crs(default = NULL)
  if (is.null(crs) && is.data.frame(panel_info) && nrow(panel_info)) {
    crs = tryCatch(
      get_scene_target_crs(
        extent = panel_extents[[1L]],
        heightmap = heightmap,
        panel = panel_info$panel[[1L]],
        caller = "get_scene_metadata"
      ),
      error = function(error) NULL
    )
  }

  finite_elevations = heightmap[is.finite(heightmap)]
  elevation_range = if (length(finite_elevations)) {
    stats::setNames(range(finite_elevations), c("min", "max"))
  } else {
    c(min = NA_real_, max = NA_real_)
  }
  scene_elevation_range = elevation_range / effective_zscale

  geographic_aspect = get_scene_geographic_aspect()
  half_x = (nrow(heightmap) - 1) / 2 * geographic_aspect$scale[["x"]]
  half_z = (ncol(heightmap) - 1) / 2 * geographic_aspect$scale[["z"]]
  scene_bounds = matrix(
    c(
      -half_x,
      half_x,
      scene_elevation_range[["min"]],
      scene_elevation_range[["max"]],
      -half_z,
      half_z
    ),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(c("x", "y", "z"), c("min", "max"))
  )
  scene_center = stats::setNames(rowMeans(scene_bounds), c("x", "y", "z"))
  scene_dimensions = stats::setNames(
    scene_bounds[, "max"] - scene_bounds[, "min"],
    c("x", "y", "z")
  )

  extent_3d = NULL
  map_units_per_scene_unit = c(x = NA_real_, y = NA_real_)
  if (!is.null(extent)) {
    extent_3d = c(
      extent[c("xmin", "xmax", "ymin", "ymax")],
      zmin = elevation_range[["min"]],
      zmax = elevation_range[["max"]]
    )
    if (scene_dimensions[["x"]] > 0 && scene_dimensions[["z"]] > 0) {
      map_units_per_scene_unit = c(
        x = unname(diff(extent[c("xmin", "xmax")])) /
          scene_dimensions[["x"]],
        y = unname(diff(extent[c("ymin", "ymax")])) /
          scene_dimensions[["z"]]
      )
    }
  }

  meters_per_scene_unit = geographic_aspect$mean_cell_meters
  if (!is.finite(meters_per_scene_unit)) {
    meters_per_scene_unit = NA_real_
  }

  list(
    scene_id = get_scene_context_token(default = NULL),
    plot_type = if (is.data.frame(panel_info) && nrow(panel_info)) {
      "plot_gg"
    } else {
      "plot_3d"
    },
    extent = extent,
    extent_3d = extent_3d,
    crs = crs,
    zscale = zscale,
    vertical_exaggeration = vertical_exaggeration,
    effective_zscale = effective_zscale,
    heightmap_dimensions = c(
      rows = nrow(heightmap),
      columns = ncol(heightmap)
    ),
    elevation_range = elevation_range,
    scene_elevation_range = scene_elevation_range,
    scene_bounds = scene_bounds,
    scene_center = scene_center,
    scene_dimensions = scene_dimensions,
    geographic_aspect = geographic_aspect,
    meters_per_scene_unit = meters_per_scene_unit,
    map_units_per_scene_unit = map_units_per_scene_unit,
    scene_axis_mapping = c(x = "x", y = "-z", elevation = "y"),
    triangulate = get_scene_triangulate(default = FALSE),
    panel_extents = panel_extents,
    panel_info = panel_info,
    sources = list(
      heightmap = get_scene_heightmap_label(default = NULL),
      extent = get_scene_extent_label(default = NULL),
      crs = get_scene_crs_label(default = NULL),
      zscale = get_scene_zscale_label(default = NULL),
      vertical_exaggeration = get_scene_vertical_exaggeration_label(
        default = NULL
      )
    ),
    heightmap = if (isTRUE(include_heightmap)) heightmap else NULL
  )
}
