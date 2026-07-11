#'@title Render Water Layer
#'
#'@description Adds water layer to the scene, removing the previous water layer if desired.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap Default `NULL`. Height matrix or spatial raster for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param waterdepth Default `0`. Water level. Either a scalar, a matrix with the same dimensions as `heightmap`, or a spatial raster that can be projected/resampled to the heightmap grid. For spatial rasters, finite cells define the water footprint.
#'@param watercolor Default `lightblue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'If `zscale` is omitted and `heightmap` is a spatial raster, rayshader uses the raster cell resolution.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency.
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param water_render_method Default `"contour"`. Water meshing method. `"contour"` clips scalar/matrix water meshes and uses the raster-cell renderer for spatial water rasters; `"raster"` explicitly uses spatial water raster cells; `"polygon"` fits each spatial water component to a DEM contour by matching contour area to raster footprint area, initialized from covered DEM values; `"legacy"` uses the previous box/grid renderer.
#'@param water_edge_extension Default `0.5`. For spatial `waterdepth` inputs, amount in grid cells to expand finite water cells at boundary edges, up to a maximum of half a cell.
#'@param water_edge_clamp Default `FALSE`. For spatial `waterdepth` inputs, if `TRUE`, resolves each connected water footprint to a single level, then lowers it by the largest finite exterior sidewall height after edge expansion. Heightmap-boundary and NA-slice edges are ignored when computing the lowering amount.
#'@param parallel Default `FALSE`. If `TRUE`, spatial polygon water components are fit in parallel using `mirai`. A positive numeric value sets the worker count.
#'@param clear_previous Default `TRUE`. If `TRUE`, will remove existing water layer and replace it with new layer.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'montereybay |>
#'  sphere_shade(vertical_exaggeration = 20) |>
#'  plot_3d(vertical_exaggeration = 4)
#'render_snapshot()
#'
#'#We want to add a layer of water after the initial render.
#'render_water()
#'render_snapshot()
#'
#'#Call it again to change the water depth
#'render_water(waterdepth=-1000, watercolor = "dodgerblue3")
#'render_snapshot()
#'
#'#Slice the water out to the edge
#'water_levels = matrix(
#'  0,
#'  nrow = nrow(montereybay),
#'  ncol = ncol(montereybay)
#')
#'water_levels[col(water_levels) > ncol(water_levels) / 2 + 20 |
#' col(water_levels) < ncol(water_levels) / 2-20] = -8000
#'render_water(waterdepth = water_levels, watercolor = "dodgerblue4")
#'render_snapshot()
#'
#'#Use a matrix to vary the water level across the scene
#'water_ramp = matrix(
#'  seq(-1200, -300, length.out = length(montereybay)),
#'  nrow = nrow(montereybay),
#'  ncol = ncol(montereybay)
#')
#'render_water(waterdepth = water_ramp, watercolor = "dodgerblue3")
#'render_highquality()
#'
#'#Add waterlines
#'render_camera(theta=-45)
#'render_water(waterlinecolor="white", watercolor = "dodgerblue4")
#'render_snapshot()
render_water = function(
  heightmap = NULL,
  waterdepth = 0,
  watercolor = "lightblue",
  zscale = 1,
  vertical_exaggeration = 1,
  wateralpha = 0.5,
  waterlinecolor = NULL,
  waterlinealpha = 1,
  linewidth = 2,
  water_render_method = c("contour", "raster", "polygon", "legacy"),
  water_edge_extension = 0.5,
  water_edge_clamp = FALSE,
  parallel = FALSE,
  clear_previous = TRUE
) {
  water_render_method = match.arg(water_render_method)
  heightmap = resolve_render_water_heightmap(
    heightmap,
    heightmap_missing = missing(heightmap),
    caller = "render_water"
  )
  if (is.null(heightmap)) {
    stop(
      "No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
    )
  }
  zscale = resolve_render_water_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    heightmap = heightmap,
    caller = "render_water"
  )
  water_render_method_current = resolve_polygon_water_render_method_for_terrain(
    water_render_method = water_render_method,
    triangulate = get_scene_triangulate(default = FALSE),
    caller = "render_water"
  )
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  heightmap_extent = NULL
  heightmap_crs = NULL
  if (is_spatial_heightmap_input(waterdepth)) {
    heightmap_extent = resolve_scene_render_extent(
      heightmap = heightmap,
      caller = "render_water",
      error_if_missing = FALSE
    )
    heightmap_crs = attr(heightmap, "crs", exact = TRUE)
    if (is.null(heightmap_crs)) {
      heightmap_crs = tryCatch(
        get_scene_target_crs(
          extent = heightmap_extent,
          heightmap = heightmap,
          caller = "render_water"
        ),
        error = function(e) NULL
      )
    }
  }
  if (clear_previous) {
    rgl::pop3d(tag = c("waterlines", "water"))
  }
  water_mesh = list(
    vertices = list(),
    lines = matrix(nrow = 0, ncol = 3)
  )
  if (!is.null(waterdepth)) {
    water_mesh = make_water(
      heightmap,
      waterheight = waterdepth,
      wateralpha = wateralpha,
      watercolor = watercolor,
      zscale = zscale,
      water_render_method = water_render_method_current,
      water_edge_extension = water_edge_extension,
      water_edge_clamp = water_edge_clamp,
      parallel = parallel,
      heightmap_extent = heightmap_extent,
      heightmap_crs = heightmap_crs
    )
  }
  if (!is.null(waterlinecolor)) {
    if (!identical(water_render_method_current, "legacy")) {
      make_waterlines_from_mesh(
        water_mesh,
        linecolor = waterlinecolor,
        alpha = waterlinealpha,
        linewidth = linewidth
      )
    } else {
      if (all(!is.na(heightmap))) {
        make_lines(
          fliplr(heightmap),
          basedepth = waterdepth,
          linecolor = waterlinecolor,
          zscale = zscale,
          linewidth = linewidth,
          alpha = waterlinealpha,
          solid = FALSE
        )
      }
      make_waterlines(
        heightmap,
        waterdepth = waterdepth,
        linecolor = waterlinecolor,
        zscale = zscale,
        alpha = waterlinealpha,
        linewidth = linewidth
      )
    }
  }
  invisible(NULL)
}

#' Resolve render_water heightmap
#'
#' @param heightmap Default `NULL`. Heightmap input.
#' @param heightmap_missing Default `FALSE`. Whether `heightmap` was omitted.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Heightmap matrix or `NULL`.
#' @keywords internal
resolve_render_water_heightmap = function(
  heightmap = NULL,
  heightmap_missing = FALSE,
  caller = NULL
) {
  if (
    !isTRUE(heightmap_missing) &&
      !is.null(heightmap) &&
      is_spatial_heightmap_input(heightmap)
  ) {
    heightmap_info = coerce_plot_3d_heightmap(heightmap)
    heightmap = heightmap_info$heightmap
    if (!is.null(heightmap_info$extent)) {
      attr(heightmap, "extent") = heightmap_info$extent
    }
    if (!is.null(heightmap_info$crs)) {
      attr(heightmap, "crs") = heightmap_info$crs
    }
    if (is.finite(heightmap_info$zscale) && heightmap_info$zscale > 0) {
      attr(heightmap, "zscale") = heightmap_info$zscale
    }
    return(heightmap)
  }
  resolve_scene_render_heightmap(
    heightmap,
    caller = caller
  )
}

#' Resolve render_water zscale
#'
#' @param zscale Default `1`. Requested zscale.
#' @param zscale_missing Default `FALSE`. Whether `zscale` was omitted.
#' @param vertical_exaggeration Default `1`. Requested vertical exaggeration.
#' @param vertical_exaggeration_missing Default `FALSE`. Whether `vertical_exaggeration` was omitted.
#' @param heightmap Default `NULL`. Resolved heightmap.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Effective zscale.
#' @keywords internal
resolve_render_water_effective_zscale = function(
  zscale = 1,
  zscale_missing = FALSE,
  vertical_exaggeration = 1,
  vertical_exaggeration_missing = FALSE,
  heightmap = NULL,
  caller = NULL
) {
  heightmap_zscale = suppressWarnings(
    as.numeric(attr(heightmap, "zscale", exact = TRUE))[1]
  )
  if (
    isTRUE(zscale_missing) &&
      is.finite(heightmap_zscale) &&
      heightmap_zscale > 0
  ) {
    zscale = heightmap_zscale
  } else {
    zscale = resolve_scene_render_zscale(
      zscale = zscale,
      zscale_missing = zscale_missing,
      caller = caller
    )
  }
  vertical_exaggeration = resolve_scene_render_vertical_exaggeration(
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = vertical_exaggeration_missing,
    caller = caller
  )
  apply_vertical_exaggeration(
    zscale = zscale,
    vertical_exaggeration = vertical_exaggeration,
    caller = caller
  )
}
