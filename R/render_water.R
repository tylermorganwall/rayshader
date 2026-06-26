#'@title Render Water Layer
#'
#'@description Adds water layer to the scene, removing the previous water layer if desired.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param waterdepth Default `0`. Water level. Either a scalar, a matrix with the same dimensions as `heightmap`, or a spatial raster that can be projected/resampled to the heightmap grid.
#'@param watercolor Default `lightblue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency.
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param water_render_method Default `"contour"`. Water meshing method. `"contour"` clips the water mesh to the flooded region; `"legacy"` uses the previous box/grid renderer.
#'@param remove_water Default `TRUE`. If `TRUE`, will remove existing water layer and replace it with new layer.
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
  water_render_method = c("contour", "legacy"),
  remove_water = TRUE
) {
  water_render_method = match.arg(water_render_method)
  zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_water"
  )
  heightmap = resolve_scene_render_heightmap(
    heightmap,
    caller = "render_water"
  )
  if (is.null(heightmap)) {
    stop(
      "No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
    )
  }
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
  if (remove_water) {
    rgl::pop3d(tag = c("waterlines", "water"))
  }
  water_mesh = make_water(
    heightmap,
    waterheight = waterdepth,
    wateralpha = wateralpha,
    watercolor = watercolor,
    zscale = zscale,
    water_render_method = water_render_method,
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs
  )
  if (!is.null(waterlinecolor)) {
    if (identical(water_render_method, "contour")) {
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
