#'@title Render Water Layer
#'
#'@description Adds water layer to the scene, removing the previous water layer if desired.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param water_input Default `0`. Water level or footprint. Either a scalar,
#'a matrix with the same dimensions as `heightmap`, a spatial raster that can
#'be projected/resampled to the heightmap grid, or an `sf`/`sfc` object containing
#'POLYGON or MULTIPOLYGON geometries. For spatial rasters, finite cells define
#'the water footprint. Polygon inputs require known source and heightmap CRSs
#'and a heightmap extent; they are transformed to the scene CRS and converted
#'to a spatial water raster on the resolved heightmap grid. Polygon holes are
#'retained at that grid resolution. Features smaller than a cell use touched
#'cells when they contain no cell centers. Polygon input requires `sf` and `terra`.
#'@param water_color Default `lightblue`.
#'@param water_alpha Default `0.5`. Water transparency.
#'@param water_line_color Default `NULL`. Color of the lines around the edges of the water layer.
#'@param water_line_alpha Default `1`. Water line tranparency.
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param water_render_method Default `"raster"`. Water meshing method. `"raster"` renders water at the supplied elevation and emits sidewalls down to the terrain wherever exposed water floats above the surface; `"polygon"` fits each spatial water component by matching flooded terrain-triangle area to raster footprint area, then clips the fixed-grid terrain triangles; `"legacy"` uses the previous box/grid renderer.
#'@param water_edge_extension Default `0.5`. For spatial `water_input` inputs, amount in grid cells to expand finite water cells at boundary edges, up to a maximum of half a cell.
#'@param water_edge_clamp Default `FALSE`. For spatial `water_input` inputs, if `TRUE`, resolves each connected water footprint to a single level, then lowers it by the largest finite exterior sidewall height after edge expansion. Heightmap-boundary and NA-slice edges are ignored when computing the lowering amount.
#'@param water_polygon_failure Default `"raster"`. Behavior for spatial polygon water components that cannot be fit to an admissible terrain-triangle flood. `"raster"` renders the failed component with the raster method; `"remove"` omits it.
#'@param clear_previous Default `TRUE`. If `TRUE`, removes the existing water
#'layer before drawing the new one. A clear-only call returns without rendering
#'a replacement.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'If `zscale` is omitted and `heightmap` is a spatial raster, rayshader uses the raster cell resolution.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param heightmap Default `NULL`. Height matrix or spatial raster for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@param water_elevation Default `NULL`. For polygon `water_input` only: a finite
#'numeric elevation, one elevation per original feature, or the name of a
#'numeric column in an `sf` object. Values use the heightmap's elevation units,
#'before `zscale` and `vertical_exaggeration`. If `NULL`, each polygon part uses
#'the median finite terrain elevation sampled along its exterior ring, falling
#'back to its interior cells when no finite shoreline samples are available.
#'Inferred levels receive a small positive lift, `max(1e-4, abs(level) * 1e-6)`,
#'to avoid coplanar water on flat terrain. Explicit levels are not lifted.
#'This default is a visualization estimate, not a measured water-surface level.
#'Overlapping raster cells use the highest component level. With method
#'`"polygon"`, the existing area-fit algorithm may replace these levels; they
#'are the supplied levels for raster rendering and polygon-to-raster fallback.
#'`water_edge_clamp = TRUE` may also lower the supplied levels. Use method
#'`"raster"` with clamping disabled to retain explicit elevations. With polygon
#'inputs, method `"legacy"` warns and uses `"raster"` instead.
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
#'render_water(water_input=-1000, water_color = "dodgerblue3")
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
#'render_water(water_input = water_levels, water_color = "dodgerblue4")
#'render_snapshot()
#'
#'#Use a matrix to vary the water level across the scene
#'water_ramp = matrix(
#'  seq(-1200, -300, length.out = length(montereybay_spatial)),
#'  nrow = nrow(montereybay_spatial),
#'  ncol = ncol(montereybay_spatial)
#')
#'render_water(water_input = water_ramp, water_color = "dodgerblue3")
#'render_highquality()
#'
#'#Add waterlines
#'render_camera(theta=-45)
#'render_water(water_line_color="white", water_color = "dodgerblue4")
#'render_snapshot()
render_water = function(
  water_input = 0,
  water_color = "lightblue",
  water_alpha = 0.5,
  water_line_color = NULL,
  water_line_alpha = 1,
  linewidth = 2,
  water_render_method = c("raster", "polygon", "legacy"),
  water_edge_extension = 0.5,
  water_edge_clamp = FALSE,
  water_polygon_failure = c("raster", "remove"),
  clear_previous = TRUE,
  zscale = 1,
  vertical_exaggeration = 1,
  heightmap = NULL,
  water_elevation = NULL
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
  water_input_is_sf = is_sf_water_input(water_input)
  if (!water_input_is_sf && !is.null(water_elevation)) {
    stop(
      "`water_elevation` is only used with polygon `water_input`.",
      call. = FALSE
    )
  }
  if (water_input_is_sf && identical(water_render_method, "legacy")) {
    warning(
      "`water_render_method = \"legacy\"` does not support polygon footprints; ",
      "using \"raster\".",
      call. = FALSE
    )
    water_render_method = "raster"
  }
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
  if (water_input_is_sf || is_spatial_heightmap_input(water_input)) {
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
  if (water_input_is_sf) {
    water_input = water_sf_to_raster(
      water_input = water_input,
      heightmap = heightmap,
      heightmap_extent = heightmap_extent,
      heightmap_crs = heightmap_crs,
      water_elevation = water_elevation
    )
    if (is.null(water_input)) {
      return(invisible(NULL))
    }
  }
  water_mesh = list(
    vertices = list(),
    lines = matrix(nrow = 0, ncol = 3)
  )
  if (!is.null(water_input)) {
    water_mesh = make_water(
      heightmap,
      waterheight = water_input,
      water_alpha = water_alpha,
      water_color = water_color,
      zscale = zscale,
      water_render_method = water_render_method_current,
      water_edge_extension = water_edge_extension,
      water_edge_clamp = water_edge_clamp,
      water_polygon_failure = water_polygon_failure,
      heightmap_extent = heightmap_extent,
      heightmap_crs = heightmap_crs
    )
  }
  if (!is.null(water_line_color)) {
    if (!identical(water_render_method_current, "legacy")) {
      make_waterlines_from_mesh(
        water_mesh,
        line_color = water_line_color,
        alpha = water_line_alpha,
        linewidth = linewidth
      )
    } else {
      if (all(!is.na(heightmap))) {
        make_lines(
          fliplr(heightmap),
          basedepth = water_input,
          line_color = water_line_color,
          zscale = zscale,
          linewidth = linewidth,
          alpha = water_line_alpha,
          solid = FALSE
        )
      }
      make_waterlines(
        heightmap,
        water_input = water_input,
        line_color = water_line_color,
        zscale = zscale,
        alpha = water_line_alpha,
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

# Internal sf -> spatial-raster adapter for render_water().
# Keep this predicate separate from is_spatial_heightmap_input(): sf polygons
# are footprints, not rasters, and must not enter raster-only code unchanged.
is_sf_water_input = function(x) {
  inherits(x, "sf") || inherits(x, "sfc")
}

# Extract polygon parts after st_make_valid(), without losing the association
# with the original feature. Collapsed line/point remnants are not water.
water_sf_polygon_parts = function(x) {
  if (inherits(x, "POLYGON")) {
    return(list(x))
  }
  if (inherits(x, "MULTIPOLYGON")) {
    return(lapply(x, sf::st_polygon))
  }
  if (inherits(x, "GEOMETRYCOLLECTION")) {
    result = list()
    for (part in x) {
      result = c(result, water_sf_polygon_parts(part))
    }
    return(result)
  }
  list()
}

water_sf_elevations = function(water_input, water_elevation, nfeatures) {
  if (is.null(water_elevation)) {
    return(NULL)
  }
  if (is.character(water_elevation)) {
    if (
      length(water_elevation) != 1L ||
        is.na(water_elevation) ||
        !inherits(water_input, "sf") ||
        !water_elevation %in% names(water_input)
    ) {
      stop(
        "`water_elevation` must name an existing numeric column in `water_input`.",
        call. = FALSE
      )
    }
    water_elevation = water_input[[water_elevation]]
  }
  if (
    !is.numeric(water_elevation) ||
      !is.null(dim(water_elevation)) ||
      !length(water_elevation) %in% c(1L, nfeatures) ||
      any(!is.finite(water_elevation))
  ) {
    stop(
      "`water_elevation` must be finite numeric elevations: one value or ",
      "one per original feature, in the heightmap's elevation units.",
      call. = FALSE
    )
  }
  rep_len(as.numeric(water_elevation), nfeatures)
}

# Return a SpatRaster on the *resolved scene grid*, or NULL for no drawable
# footprint. Elevations here are deliberately not divided by zscale: the
# existing water renderer applies the effective scene scale exactly once.
water_sf_to_raster = function(
  water_input,
  heightmap,
  heightmap_extent,
  heightmap_crs,
  water_elevation = NULL
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package `sf` is required for polygon water inputs.", call. = FALSE)
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package `terra` is required for polygon water inputs.", call. = FALSE)
  }
  if (!is_sf_water_input(water_input)) {
    stop("Polygon water must be an `sf` or `sfc` object.", call. = FALSE)
  }

  geometry = sf::st_geometry(water_input)
  feature_levels = water_sf_elevations(
    water_input,
    water_elevation,
    length(geometry)
  )
  nonempty = !sf::st_is_empty(geometry)
  if (!any(nonempty)) {
    return(NULL)
  }
  types = as.character(sf::st_geometry_type(geometry[nonempty]))
  if (any(!types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop(
      "`water_input` must contain only POLYGON or MULTIPOLYGON geometries.",
      call. = FALSE
    )
  }
  if (
    !is.matrix(heightmap) || !is.numeric(heightmap) || any(dim(heightmap) < 1L)
  ) {
    stop(
      "Polygon water requires a resolved numeric heightmap matrix.",
      call. = FALSE
    )
  }
  if (is.null(heightmap_extent)) {
    stop(
      "Polygon water requires the heightmap's spatial extent. ",
      "Plot a spatial heightmap first, or pass a georeferenced `heightmap`.",
      call. = FALSE
    )
  }
  target_crs = tryCatch(sf::st_crs(heightmap_crs), error = function(e) NA)
  if (is.null(heightmap_crs) || is.na(target_crs)) {
    stop(
      "Polygon water requires a known heightmap CRS. ",
      "Plot a spatial heightmap first, or pass a georeferenced `heightmap`.",
      call. = FALSE
    )
  }
  if (is.na(sf::st_crs(geometry))) {
    stop(
      "`water_input` has no CRS. Assign its actual source CRS before rendering.",
      call. = FALSE
    )
  }

  # get_extent() is rayshader's existing extent adapter. Preserve the meaning
  # of named vectors as well as sf bbox, raster Extent, and terra SpatExtent.
  extent_order = c("xmin", "xmax", "ymin", "ymax")
  if (is.numeric(heightmap_extent)) {
    if (length(heightmap_extent) != 4L) {
      stop("The heightmap extent must contain four bounds.", call. = FALSE)
    }
    if (all(extent_order %in% names(heightmap_extent))) {
      extent = as.numeric(heightmap_extent[extent_order])
    } else {
      extent = as.numeric(heightmap_extent)
    }
    names(extent) = extent_order
  } else {
    extent = get_extent(heightmap_extent)
    extent = extent[extent_order]
  }
  if (
    length(extent) != 4L ||
      any(!is.finite(extent)) ||
      extent["xmax"] <= extent["xmin"] ||
      extent["ymax"] <= extent["ymin"]
  ) {
    stop(
      "The heightmap must have a finite, nonzero spatial extent.",
      call. = FALSE
    )
  }

  # Inverse of raster_to_matrix(): rayshader's matrix row index is raster
  # column (west -> east), and its column index is raster row (north -> south).
  # This matters particularly for rectangular and resized scenes.
  terrain = terra::rast(
    nrows = ncol(heightmap),
    ncols = nrow(heightmap),
    xmin = unname(extent["xmin"]),
    xmax = unname(extent["xmax"]),
    ymin = unname(extent["ymin"]),
    ymax = unname(extent["ymax"]),
    crs = target_crs$wkt,
    vals = as.numeric(heightmap)
  )
  names(terrain) = "water_terrain"

  # Geometry is a footprint only; numeric data columns and Z/M coordinates
  # are never silently interpreted as water elevations.
  original_id = which(nonempty)
  geometry = sf::st_zm(geometry[nonempty], drop = TRUE, what = "ZM")
  geometry = sf::st_make_valid(geometry)
  geometry = sf::st_transform(geometry, target_crs)
  geometry = sf::st_make_valid(geometry)

  parts = list()
  source_id = integer()
  for (i in seq_along(geometry)) {
    feature_parts = water_sf_polygon_parts(geometry[[i]])
    if (length(feature_parts)) {
      parts = c(parts, feature_parts)
      source_id = c(source_id, rep.int(original_id[i], length(feature_parts)))
    }
  }
  if (!length(parts)) {
    warning(
      "No nonempty polygon water remains after geometry repair.",
      call. = FALSE
    )
    return(NULL)
  }
  parts = sf::st_sfc(parts, crs = target_crs)
  keep = !sf::st_is_empty(parts)
  parts = parts[keep]
  source_id = source_id[keep]
  if (!length(parts)) {
    return(NULL)
  }

  # Rasterize the footprint by cell membership. Unlike rasterizing a feature
  # ID once, extraction retains all memberships for overlapping polygons.
  # small=TRUE includes touched cells only when a part covers no cell centers;
  # this keeps very small lakes from silently disappearing after resizing.
  footprint = terra::extract(
    terrain,
    terra::vect(parts),
    cells = TRUE,
    touches = FALSE,
    small = TRUE
  )
  valid = is.finite(footprint$cell) & is.finite(footprint$water_terrain)
  footprint = footprint[valid, , drop = FALSE]
  if (!nrow(footprint)) {
    warning("No polygon water overlaps finite heightmap cells.", call. = FALSE)
    return(NULL)
  }
  covered_ids = unique(footprint$ID)
  missing_parts = length(parts) - length(covered_ids)
  if (missing_parts > 0L) {
    warning(
      "Omitted ",
      missing_parts,
      " polygon water component(s) without finite heightmap coverage.",
      call. = FALSE
    )
  }

  if (is.null(feature_levels)) {
    # Sample the ORIGINAL exterior rings, not the boundary of cropped
    # polygons: the edge of the scene is not necessarily a shoreline.
    # Exclude hole rings from the level estimate, but retain holes in the mask.
    outer_rings = lapply(parts, function(p) sf::st_linestring(p[[1L]]))
    shore = terra::extract(
      terrain,
      terra::vect(sf::st_sfc(outer_rings, crs = target_crs)),
      touches = TRUE
    )
    shore = shore[is.finite(shore$water_terrain), , drop = FALSE]
    shore_by_id = split(shore$water_terrain, shore$ID)
    interior_by_id = split(footprint$water_terrain, footprint$ID)
    component_levels = rep(NA_real_, length(parts))
    for (id in covered_ids) {
      values = shore_by_id[[as.character(id)]]
      if (!length(values)) {
        values = interior_by_id[[as.character(id)]]
      }
      level = stats::median(values)
      # Inferred levels receive a tiny positive lift to avoid an exactly
      # coplanar surface on flat DEM lakes. Explicit elevations are untouched.
      # This is a visualization heuristic, not a measured hydrological level.
      lift = max(1e-4, abs(level) * 1e-6)
      component_levels[id] = level + lift
    }
  } else {
    component_levels = feature_levels[source_id]
  }

  cells = as.integer(footprint$cell)
  levels = component_levels[footprint$ID]
  finite = is.finite(levels)
  cells = cells[finite]
  levels = levels[finite]
  if (!length(cells)) {
    return(NULL)
  }

  # Highest surface wins where different components share a raster cell;
  # deterministic under feature reordering. Never burn IDs or zero outside.
  ord = order(cells, -levels, method = "radix")
  ord = ord[!duplicated(cells[ord])]
  water_values = rep(NA_real_, length(heightmap))
  water_values[cells[ord]] = levels[ord]
  result = terra::setValues(terra::rast(terrain), water_values)
  names(result) = "water_elevation"
  result
}
