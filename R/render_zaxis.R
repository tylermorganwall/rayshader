#'@title Render Z-Axis
#'
#'@description Add a standalone z-axis to the active 3D scene.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param extent Default `NULL`. Either an object representing the spatial extent of the scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use cached extent metadata from [plot_gg()] or from [plot_3d()]
#'(either from an explicitly passed `extent` argument, or the built-in `montereybay_spatial` scene metadata).
#'@param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#'to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#'for non-ggplot scenes.
#'@param zscale Default `1`. The ratio between x/y spacing and z units.
#'If left at `1` with `zaxis_breaks = NULL` on non-ggplot terrain scenes, rayshader
#'will attempt to use the cached `plot_3d()` zscale to generate more meaningful defaults.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param zaxis_data Default `"auto"`. Data source used to generate z-axis
#'breaks and labels. Options are `"auto"`, `"topographic"`, `"polygon"`,
#'`"point"`, `"path"`, `"obj"`, `"raymesh"`, `"tree"`, `"label"`,
#'`"building"`, and `"cloud"`. Use `"polygon"` after [render_polygons()] or
#'[render_beveled_polygons()] to label breaks with cached polygon data values
#'(for example `data_column_top`) while placing ticks at the rendered scaled
#'heights. Use `"building"` after [render_buildings()] to refer to building
#'height data, or the matching render type after point/path/object/tree/label
#'and cloud calls to use those cached altitude values.
#'@param zaxis_location Default `"auto"`. Axis location. Options:
#'`"auto"`, `"panel"`, `"panelbottomleft"`, `"panelbottomright"`,
#'`"paneltopleft"`, `"paneltopright"`, `"bottomleft"`, `"bottomright"`,
#'`"topleft"`, `"topright"`.
#'@param zaxis_breaks Default `NULL`. Numeric breaks (in altitude units). If `NULL`,
#'breaks are generated from the full scene height range (minimum to maximum elevation).
#'@param zaxis_labels Default `NULL`. Labels for `zaxis_breaks`.
#'@param zaxis_title Default `"auto"`. Title for the z-axis. If `"auto"`, rayshader
#'uses the cached height aesthetic label from [plot_gg()] scenes when available.
#'Set to `NULL` to omit the title, or pass a character string to override it.
#'@param zaxis_title_location Default `"top"`. Title location. Options are
#'`"side"` and `"top"`. `"side"` places the title horizontally opposite the
#'tick-label side by default and centered on the axis.
#'@param zaxis_title_offset Default `1.25`. Title offset multiplier in tick
#'lengths. For
#'`zaxis_title_location = "side"`, this moves the title away from the axis on the
#'chosen title side, but never closer than one tick length beyond the tick label
#'offset.
#'For `"top"`, this moves the title above the top of the axis using
#'tick-length-scaled spacing.
#'@param zaxis_title_size Default `NULL`, which matches `zaxis_label_size`. Title text size passed to `rgl::texts3d()` as `cex`.
#'@param zaxis_color Default `"black"`. Axis/tick/label color.
#'@param zaxis_linewidth Default `2`. Axis line width.
#'@param zaxis_text_offset Default `0`. Label offset multiplier from the axis,
#'applied in the outward corner direction (diagonal for corner placements).
#'@param zaxis_label_size Default `0.8`. Tick label text size passed to `rgl::texts3d()` as `cex`.
#'@param zaxis_label_side Default `"auto"`. Side of the axis where tick labels
#'are placed from the current camera perspective. Options are `"auto"`,
#'`"left"`, and `"right"`. `"auto"` keeps the inferred outside-corner side.
#'@param zaxis_title_side Default `"auto"`. Side of the axis where the title is
#'placed from the current camera perspective. Options are `"auto"`, `"left"`,
#'and `"right"`. `"auto"` places side titles opposite the tick-label side and top
#'titles on the tick-label side.
#'@param zaxis_corner_offset Default `NULL`. Corner offset as a proportion of the
#'center-to-corner planar distance. If `NULL`, this defaults to `0` for ggplot scenes
#'and `0.08` for non-ggplot scenes. `0` places the axis exactly at the corner.
#'@param zaxis_tick_size Default `NULL`. Tick marker size. If `NULL`, auto-sized from line width.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'# Add a z-axis to a Monterey Bay terrain scene. The spatial extent and zscale
#'# are cached by sphere_shade()/plot_3d(), so render_zaxis() can infer them.
#'montereybay_spatial |>
#'  sphere_shade(texture = "imhof1", vertical_exaggeration = 20) |>
#'  plot_3d(
#'    vertical_exaggeration = 4,
#'    water = TRUE,
#'    theta = 245,
#'    phi = 20,
#'    zoom = 1
#'  )
#'render_zaxis(
#'  zaxis_location = "bottomleft",
#'  zaxis_color = "black",
#'  zaxis_corner_offset = 0.2
#')
#'render_snapshot()
#'
#' #Change the location
#'render_zaxis(
#'  zaxis_location = "topright",
#'  zaxis_title = "Elevation (m)",
#'  zaxis_title_location = "top",
#'  zaxis_color = "red",
#'  zaxis_corner_offset = 0.3,
#'  zaxis_tick_size = 3
#')
#'render_snapshot()
#'# Add a z-axis to a ggplot scene.
#'library(ggplot2)
#'mtplot = ggplot(mtcars) +
#'  geom_point(aes(x = mpg, y = disp, color = cyl)) +
#'  scale_color_continuous(limits = c(0, 8))
#'plot_gg(
#'  mtplot,
#'  width = 3.5,
#'  windowsize = c(1400, 866),
#'  sunangle = 225,
#'  zoom = 0.50,
#'  phi = 20,
#'  theta = 45
#')
#'render_zaxis(zaxis_location = "panel_bottomleft", zaxis_color = "red",
#'             zaxis_label_size = 1.5)
#'render_snapshot()
#'
#'# For faceted ggplot scenes, specify the panel whose corner should anchor the axis.
#'density_plot = ggplot(mtcars) +
#'  stat_density_2d(
#'    aes(x = mpg, y = disp, fill = after_stat(density)),
#'    geom = "raster",
#'    contour = FALSE
#'  ) +
#'  facet_wrap(~cyl) +
#'  scale_x_continuous(expand = c(0, 0)) +
#'  scale_y_continuous(expand = c(0, 0)) +
#'  scale_fill_gradient(low = "pink", high = "red")
#'plot_gg(
#'  density_plot,
#'  width = 6,
#'  windowsize = c(1400, 866),
#'  zoom = 0.55,
#'  theta = 45,
#'  phi = 25,
#'  vertical_exaggeration = 300
#')
#'render_zaxis(
#'  panel = 1,
#'  zaxis_location = "panel_bottomleft"
#')
#'render_snapshot()
#' #Move to the second panel, top left
#'render_camera(theta=-20, phi=20)
#'render_zaxis(
#'  panel = 2,
#'  zaxis_location = "panel_topleft"
#')
#'render_snapshot()
#' #Move to corner of the plot
#'render_camera(theta=-20, phi=20,zoom=0.6)
#'render_zaxis(
#'  zaxis_location = "topleft"
#')
#'render_snapshot()
render_zaxis = function(
  extent = NULL,
  panel = NULL,
  zscale = 1,
  vertical_exaggeration = 1,
  heightmap = NULL,
  zaxis_data = "auto",
  zaxis_location = "auto",
  zaxis_breaks = NULL,
  zaxis_labels = NULL,
  zaxis_title = "auto",
  zaxis_title_location = "side",
  zaxis_title_offset = 1.25,
  zaxis_title_size = NULL,
  zaxis_color = "black",
  zaxis_linewidth = 2,
  zaxis_text_offset = 0,
  zaxis_label_size = 0.8,
  zaxis_label_side = "auto",
  zaxis_title_side = "auto",
  zaxis_corner_offset = NULL,
  zaxis_tick_size = NULL
) {
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_zaxis"
  )
  extent_was_missing = missing(extent)
  if (!extent_was_missing && !is.null(extent)) {
    cache_scene_extent(
      extent,
      label = format_scene_cache_label(deparse(substitute(extent)))
    )
  }
  heightmap = resolve_scene_render_heightmap(
    heightmap,
    caller = "render_zaxis"
  )
  use_ggplot_panel_extent = zaxis_location_uses_panel_extent(zaxis_location)
  if (!use_ggplot_panel_extent && is.null(extent)) {
    extent = get_cached_plot_gg_scene_extent_for_zaxis(heightmap = heightmap)
  }
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "render_zaxis",
    panel = panel,
    allow_ggplot_extent = use_ggplot_panel_extent
  )
  render_zaxis_internal(
    zaxis = TRUE,
    extent = extent,
    zscale = zscale,
    heightmap = heightmap,
    zaxis_data = zaxis_data,
    zaxis_location = zaxis_location,
    zaxis_breaks = zaxis_breaks,
    zaxis_labels = zaxis_labels,
    zaxis_title = zaxis_title,
    zaxis_title_location = zaxis_title_location,
    zaxis_title_offset = zaxis_title_offset,
    zaxis_title_size = zaxis_title_size,
    zaxis_color = zaxis_color,
    zaxis_linewidth = zaxis_linewidth,
    zaxis_text_offset = zaxis_text_offset,
    zaxis_label_size = zaxis_label_size,
    zaxis_label_side = zaxis_label_side,
    zaxis_title_side = zaxis_title_side,
    zaxis_corner_offset = zaxis_corner_offset,
    zaxis_tick_size = zaxis_tick_size
  )
}

get_cached_plot_gg_scene_extent_for_zaxis = function(heightmap = NULL) {
  panel_info = get_cached_plot_gg_panel_info(
    heightmap = heightmap,
    default = NULL
  )
  if (
    is.null(panel_info) ||
      !is.data.frame(panel_info) ||
      !nrow(panel_info) ||
      !all(
        c(
          "extent_xmin",
          "extent_xmax",
          "extent_ymin",
          "extent_ymax"
        ) %in%
          names(panel_info)
      )
  ) {
    return(NULL)
  }
  extent_vals = c(
    xmin = min(panel_info$extent_xmin, na.rm = TRUE),
    xmax = max(panel_info$extent_xmax, na.rm = TRUE),
    ymin = min(panel_info$extent_ymin, na.rm = TRUE),
    ymax = max(panel_info$extent_ymax, na.rm = TRUE)
  )
  if (any(!is.finite(extent_vals))) {
    return(NULL)
  }
  attr(extent_vals, "panel_info") = panel_info
  extent_vals
}

zaxis_location_uses_panel_extent = function(zaxis_location = "auto") {
  if (is.null(zaxis_location) || !length(zaxis_location)) {
    return(TRUE)
  }
  location_key = tolower(as.character(zaxis_location)[1])
  location_key = gsub("[-_[:space:]]", "", location_key)
  location_key == "auto" ||
    location_key %in%
      c(
        "panel",
        "panelbottomleft",
        "panelbl",
        "panelbottomright",
        "panelbr",
        "paneltopleft",
        "paneltl",
        "paneltopright",
        "paneltr"
      )
}

zaxis_dot_names = function() {
  c(
    "zaxis",
    "zaxis_data",
    "zaxis_location",
    "zaxis_breaks",
    "zaxis_labels",
    "zaxis_title",
    "zaxis_title_location",
    "zaxis_title_offset",
    "zaxis_title_size",
    "zaxis_color",
    "zaxis_linewidth",
    "zaxis_text_offset",
    "zaxis_label_size",
    "zaxis_label_side",
    "zaxis_title_side",
    "zaxis_corner_offset",
    "zaxis_tick_size"
  )
}

split_zaxis_dots = function(dots) {
  if (is.null(dots)) {
    dots = list()
  }
  dot_names = names(dots)
  is_zaxis = rep(FALSE, length(dots))
  if (length(dots) > 0 && !is.null(dot_names)) {
    is_zaxis = nzchar(dot_names) & dot_names %in% zaxis_dot_names()
  }
  list(
    zaxis_args = dots[is_zaxis],
    other_args = dots[!is_zaxis]
  )
}

render_zaxis_from_dots = function(
  zaxis_args = list(),
  extent = NULL,
  panel = NULL,
  zscale = 1,
  heightmap = NULL,
  caller = NULL
) {
  if (length(zaxis_args) == 0) {
    return(invisible(NULL))
  }
  if (is.null(zscale)) {
    zscale = get_scene_effective_zscale(default = 1)
  } else {
    zscale = suppressWarnings(as.numeric(zscale)[1])
    if (!is.finite(zscale) || zscale <= 0) {
      zscale = get_scene_effective_zscale(default = 1)
    }
  }
  heightmap = resolve_scene_render_heightmap(heightmap)
  use_ggplot_panel_extent = zaxis_location_uses_panel_extent(
    zaxis_args$zaxis_location
  )
  if (!use_ggplot_panel_extent && is.null(extent)) {
    extent = get_cached_plot_gg_scene_extent_for_zaxis(heightmap = heightmap)
  }
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    caller = caller,
    allow_ggplot_extent = use_ggplot_panel_extent
  )
  if (is.null(zaxis_args$zaxis)) {
    zaxis_args$zaxis = TRUE
  }
  do.call(
    render_zaxis_internal,
    c(
      zaxis_args,
      list(
        extent = extent,
        zscale = zscale,
        heightmap = heightmap
      )
    )
  )
}
