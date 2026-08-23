#'@title Render Water Layer
#'
#'@description Adds water layer to the scene, removing the previous water layer if desired.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param waterdepth Default `0`. Water level. Either a scalar, a matrix with the same dimensions as `heightmap`, or a spatial raster that can be projected/resampled to the heightmap grid. For spatial rasters, finite cells define the water footprint.
#'@param watercolor Default `lightblue`.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency.
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param water_render_method Default `"raster"`. Water meshing method. `"raster"` renders water at the supplied elevation and emits sidewalls down to the terrain wherever exposed water floats above the surface; `"polygon"` fits each spatial water component by matching flooded terrain-triangle area to raster footprint area, then clips the fixed-grid terrain triangles; `"legacy"` uses the previous box/grid renderer.
#'@param water_edge_extension Default `0.5`. For spatial `waterdepth` inputs, amount in grid cells to expand finite water cells at boundary edges, up to a maximum of half a cell.
#'@param water_edge_clamp Default `FALSE`. For spatial `waterdepth` inputs, if `TRUE`, resolves each connected water footprint to a single level, then lowers it by the largest finite exterior sidewall height after edge expansion. Heightmap-boundary and NA-slice edges are ignored when computing the lowering amount.
#'@param water_polygon_failure Default `"raster"`. Behavior for spatial polygon water components that cannot be fit to an admissible terrain-triangle flood. `"raster"` renders the failed component with the raster method; `"remove"` omits it.
#'@param clear_previous Default `TRUE`. If `TRUE`, removes the existing water
#'layer before drawing the new one. A clear-only call returns without rendering
#'a replacement.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'If `zscale` is omitted and `heightmap` is a spatial raster, rayshader uses the raster cell resolution.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param heightmap Default `NULL`. Height matrix or spatial raster for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'montereybay_spatial |>
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
#'  nrow = nrow(montereybay_spatial),
#'  ncol = ncol(montereybay_spatial)
#')
#'water_levels[col(water_levels) > ncol(water_levels) / 2 + 20 |
#' col(water_levels) < ncol(water_levels) / 2-20] = -8000
#'render_water(waterdepth = water_levels, watercolor = "dodgerblue4")
#'render_snapshot()
#'
#'#Use a matrix to vary the water level across the scene
#'water_ramp = matrix(
#'  seq(-1200, -300, length.out = length(montereybay_spatial)),
#'  nrow = nrow(montereybay_spatial),
#'  ncol = ncol(montereybay_spatial)
#')
#'render_water(waterdepth = water_ramp, watercolor = "dodgerblue3")
#'render_highquality()
#'
#'#Add waterlines
#'render_camera(theta=-45)
#'render_water(waterlinecolor="white", watercolor = "dodgerblue4")
#'render_snapshot()
render_water = function(
  waterdepth = 0,
  watercolor = "lightblue",
  wateralpha = 0.5,
  waterlinecolor = NULL,
  waterlinealpha = 1,
  linewidth = 2,
  water_render_method = c("raster", "polygon", "legacy"),
  water_edge_extension = 0.5,
  water_edge_clamp = FALSE,
  water_polygon_failure = c("raster", "remove"),
  clear_previous = TRUE,
  zscale = 1,
  vertical_exaggeration = 1,
  heightmap = NULL
) {
  if (
    is_render_clear_only_call(
      clear_previous,
      match.call(),
      function() rgl::pop3d(tag = c("waterlines", "water"))
    )
  ) {
    return(invisible(NULL))
  }
  water_render_method = match.arg(water_render_method)
  water_polygon_failure = match.arg(water_polygon_failure)
  heightmap = resolve_scene_render_heightmap(
    heightmap,
    heightmap_missing = missing(heightmap),
    caller = "render_water"
  )
  if (is.null(heightmap)) {
    stop(
      "No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
    )
  }
  zscale = resolve_scene_render_effective_zscale(
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
      water_polygon_failure = water_polygon_failure,
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
  resolve_scene_render_heightmap(
    heightmap = heightmap,
    heightmap_missing = heightmap_missing,
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
  resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = zscale_missing,
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = vertical_exaggeration_missing,
    heightmap = heightmap,
    caller = caller
  )
}
