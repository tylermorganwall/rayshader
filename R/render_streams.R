#' Render Stream Paths
#'
#' @description Adds stream paths to the scene, removing the previous stream layer if desired.
#'
#' @param streams Spatial line data used to draw stream paths. Supports `sf`,
#' `sfc`, `sfg`, `SpatialLines`, and `SpatialLinesDataFrame` line inputs.
#' @param water_polygons Default `NULL`. Optional polygon data defining water
#' areas where streams should not be rendered. Supports `sf`, `sfc`, `sfg`,
#' `terra::SpatVector`, `SpatialPolygons`, and `SpatialPolygonsDataFrame` inputs.
#' Multiple polygons are combined before they are removed from the stream
#' linework. When both inputs have a CRS, the polygons are transformed to the
#' stream CRS before clipping. When only one input has a CRS, an error is
#' returned rather than assuming the coordinate systems match.
#' @param heightmap Default `NULL`. Height matrix or spatial raster for the current
#' scene. If omitted, this is taken from the cached scene set by [plot_3d()] or
#' [plot_gg()]. Pass explicitly to override the cached value.
#' @param watercolor Default `"lightblue"`. Stream color.
#' @param zscale Default `1`. The ratio between the x and y spacing and the z axis.
#' If omitted and `heightmap` is a spatial raster, rayshader uses the raster cell
#' resolution.
#' @param vertical_exaggeration Default `1`. Multiplier applied to the effective
#' visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()]
#' or [plot_gg()] when available; pass explicitly to override for this call.
#' @param width Default `1`. Stream width in scene grid-cell units for
#' [render_highquality()]. The rgl preview uses the same value as line width.
#' @param width_column Default `NULL`. Column name in an `sf` stream object used
#' to set per-feature stream widths. Values must be positive finite numbers and
#' use the same units as `width`. When supplied, stream merging is disabled to
#' preserve feature attributes.
#' @param densify Default `TRUE`. Whether to densify stream paths and resample
#' them along the terrain before [render_highquality()] meshing. Set to `FALSE`
#' to use the vertices returned by [render_path()] directly.
#' @param offset Default `NULL`. Vertical stream centerline offset in elevation
#' units. When `NULL`, the stream centerline is placed on the sampled surface so
#' the high-quality rectangular stream profile is rendered halfway through the
#' terrain.
#' @param merge Default `TRUE`. Whether to merge connected stream linework before
#' rendering. This reduces visible caps between adjacent line features in
#' [render_highquality()].
#' @param clear_previous Default `TRUE`. If `TRUE`, removes the existing stream
#' layer before drawing the new one.
#'
#' @return Invisibly returns the rendered stream coordinates.
#' @examplesIf all(vapply(c("sf", "terra", "dplyr", "tigris", "elevatr", "rayrender", "rayvertex", "skymodelr"), requireNamespace, logical(1), quietly = TRUE)) && (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(sf)
#' library(terra)
#' library(dplyr)
#' library(tigris)
#' library(elevatr)
#'
#' options(tigris_use_cache = TRUE)
#'
#' target_crs = "EPSG:32611"
#' dem_z = 12
#' min_water_area_m2 = 5000
#' min_stream_length_m = 1
#'
#' # Start with a longitude/latitude bounding box for the Mammoth Lakes,
#' # June Lake, and Crowley Lake region.
#' scene_ll = sf::st_as_sfc(
#'   sf::st_bbox(
#'     c(
#'       xmin = -119.12,
#'       ymin = 37.52,
#'       xmax = -118.68,
#'       ymax = 37.86
#'     ),
#'     crs = sf::st_crs(4326)
#'   )
#' ) |>
#'   sf::st_as_sf()
#'
#' # Transform to UTM zone 11N so areas, lengths, and stream widths are in
#' # projected meter-like units.
#' scene = sf::st_transform(scene_ll, target_crs)
#'
#' # TIGER/Line area water includes lakes and reservoirs. We crop to the scene,
#' # keep larger standing-water features, and later sample each water body's
#' # representative elevation from the DEM.
#' water_raw = tigris::area_water(
#'   state = "CA",
#'   county = "Mono",
#'   year = 2024,
#'   class = "sf"
#' )
#'
#' water = water_raw |>
#'   sf::st_make_valid() |>
#'   sf::st_transform(target_crs) |>
#'   sf::st_crop(scene) |>
#'   dplyr::filter(MTFCC %in% c("H2030", "H2040")) |>
#'   dplyr::mutate(area_m2 = as.numeric(sf::st_area(geometry))) |>
#'   dplyr::filter(area_m2 >= min_water_area_m2)
#'
#' water = water[!sf::st_is_empty(water), ]
#'
#' # Download a DEM from AWS via elevatr and aggregate it to keep the example
#' # light enough for interactive rendering.
#' dem_raw = elevatr::get_elev_raster(
#'   locations = scene,
#'   z = dem_z,
#'   src = "aws",
#'   clip = "bbox",
#'   tmp_dir = tempdir()
#' )
#'
#' dem = terra::rast(dem_raw) |>
#'   terra::aggregate(4)
#' names(dem) = "elevation_m"
#'
#' water = sf::st_transform(water, terra::crs(dem))
#'
#' water_level = terra::extract(
#'   dem,
#'   terra::vect(water),
#'   fun = median,
#'   na.rm = TRUE,
#'   touches = TRUE
#' )
#'
#' water$water_level_m = round(water_level$elevation_m, 1)
#' water = water[is.finite(water$water_level_m), ]
#'
#' # TIGER/Line linear water gives the stream and canal network. We crop,
#' # extract line strings, drop empty features, and remove tiny segments.
#' streams_raw = tigris::linear_water(
#'   state = "CA",
#'   county = "Mono",
#'   year = 2024,
#'   class = "sf"
#' )
#'
#' streams = streams_raw |>
#'   sf::st_make_valid() |>
#'   sf::st_transform(target_crs) |>
#'   sf::st_crop(scene) |>
#'   sf::st_collection_extract("LINESTRING", warn = FALSE)
#'
#' streams = streams[!sf::st_is_empty(streams), ]
#'
#' streams = streams |>
#'   dplyr::mutate(length_m = as.numeric(sf::st_length(geometry))) |>
#'   dplyr::filter(length_m >= min_stream_length_m)
#'
#' streams = sf::st_transform(streams, terra::crs(dem))
#'
#' # Rasterize per-waterbody elevations for render_water(), then lower the
#' # terrain under polygon water so the rendered water surface is visible.
#' water_level_rast = terra::rasterize(
#'   terra::vect(water),
#'   dem,
#'   field = "water_level_m",
#'   touches = TRUE
#' )
#'
#' new_dem = dem |>
#'   shift_terrain(water, amount = -10)
#'
#' new_dem |>
#'   height_shade() |>
#'   plot_3d(
#'     phi = 10,
#'     zoom = 0.15,
#'     fov = 120
#'   )
#'
#' render_water(
#'   waterdepth = water_level_rast,
#'   water_edge_extension = 0.25,
#'   watercolor = "dodgerblue"
#' )
#'
#' render_streams(
#'   streams = streams,
#'   water_polygons = water,
#'   watercolor = "dodgerblue",
#'   width = 0.35,
#'   clear_previous = TRUE
#' )
#'
#' render_highquality(
#'   joined_stream_mesh = TRUE,
#'   use_extruded_paths = TRUE,
#'   sky_sun_elevation = 30,
#'   width = 800,
#'   height = 800,
#'   sky_sun_azimuth = -113,
#'   sky_args = list(hosek = FALSE),
#'   camera_lookat = c(-183.38, -1.20, -232.91),
#'   camera_location = c(-279.37, 49.51, -177.42),
#'   water_ior = 1.2,
#'   water_material = "microfacet",
#'   iso = 100 / 2^5
#' )
#' @export
render_streams = function(
  streams,
  heightmap = NULL,
  watercolor = "lightblue",
  zscale = 1,
  vertical_exaggeration = 1,
  width = 1,
  width_column = NULL,
  densify = TRUE,
  offset = NULL,
  merge = TRUE,
  clear_previous = TRUE,
  water_polygons = NULL
) {
  # 1. Capture expressions needed to distinguish values from column references.
  heightmap_missing = missing(heightmap)
  zscale_missing = missing(zscale)
  vertical_exaggeration_missing = missing(vertical_exaggeration)
  width_missing = missing(width)
  width_expr = substitute(width)
  width_column_missing = missing(width_column)
  width_column_expr = substitute(width_column)

  # 2. Resolve the active scene and validate public scalar arguments once.
  if (!is_render_line_input(streams)) {
    stop(
      "`streams` must be an sf, sfc, sfg, SpatialLines, or SpatialLinesDataFrame line object.",
      call. = FALSE
    )
  }
  if (
    !is.null(water_polygons) &&
      !inherits(
        water_polygons,
        c(
          "sf",
          "sfc",
          "sfg",
          "SpatVector",
          "SpatialPolygons",
          "SpatialPolygonsDataFrame"
        )
      )
  ) {
    stop(
      paste0(
        "`water_polygons` must be an sf, sfc, sfg, SpatVector, ",
        "SpatialPolygons, or SpatialPolygonsDataFrame polygon object."
      ),
      call. = FALSE
    )
  }
  heightmap = resolve_scene_render_heightmap(
    heightmap = heightmap,
    heightmap_missing = heightmap_missing,
    caller = "render_streams"
  )
  if (is.null(heightmap)) {
    stop(
      "No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
    )
  }
  zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = zscale_missing,
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = vertical_exaggeration_missing,
    heightmap = heightmap,
    caller = "render_streams"
  )
  densify = resolve_render_scalar(
    densify,
    missing(densify),
    TRUE,
    "densify",
    type = "logical"
  )
  merge = resolve_render_scalar(
    merge,
    missing(merge),
    TRUE,
    "merge",
    type = "logical"
  )
  clear_previous = resolve_render_scalar(
    clear_previous,
    missing(clear_previous),
    TRUE,
    "clear_previous",
    type = "logical"
  )
  offset = if (is.null(offset)) {
    0
  } else {
    resolve_render_scalar(
      offset,
      FALSE,
      0,
      "offset",
      lower = 0
    )
  }
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }

  # 3. Resolve feature-aligned widths before geometry normalization.
  width_column_name = resolve_render_column_name(
    value = width_column,
    value_expr = width_column_expr,
    missing = width_column_missing,
    argument = "width_column"
  )
  width_column_supplied = !is.null(width_column_name)
  if (width_column_supplied) {
    if (inherits(streams, "SpatialLinesDataFrame")) {
      streams = sf::st_as_sf(streams)
    }
    if (!inherits(streams, "sf")) {
      stop(
        "`width_column` can only be used with `sf` or `SpatialLinesDataFrame` stream inputs.",
        call. = FALSE
      )
    }
    if (!(width_column_name %in% names(streams))) {
      stop(
        sprintf(
          "`width_column` must name a column in `streams`: %s",
          width_column_name
        ),
        call. = FALSE
      )
    }
    width_values = resolve_render_feature_values(
      data = streams,
      value = width_column_name,
      value_expr = as.name(width_column_name),
      missing = FALSE,
      default = 1,
      argument = "width_column",
      type = "double",
      lower = .Machine$double.xmin
    )
    streams$render_line_width = width_values
    width_column = "render_line_width"
    merge = FALSE
  } else {
    width_values = resolve_render_feature_values(
      data = streams,
      value = width,
      value_expr = width_expr,
      missing = width_missing,
      default = 1,
      argument = "width",
      type = "double",
      lower = .Machine$double.xmin
    )
    width_column = NULL
    if (length(unique(width_values)) > 1L) {
      if (inherits(streams, "SpatialLinesDataFrame")) {
        streams = sf::st_as_sf(streams)
      }
      if (!inherits(streams, "sf")) {
        stop(
          "Feature-varying `width` values require an `sf` stream input.",
          call. = FALSE
        )
      }
      streams$render_line_width = width_values
      width_column = "render_line_width"
      merge = FALSE
    }
  }
  width = if (is.null(width_column)) {
    if (length(width_values)) width_values[[1L]] else 1
  } else {
    NULL
  }
  if (isTRUE(clear_previous)) {
    rgl::pop3d(tag = "water_path")
  }

  # 4. Normalize, clip, and preserve source identities in the line geometry.
  extent = resolve_scene_render_extent(
    heightmap = heightmap,
    caller = "render_streams",
    error_if_missing = FALSE
  )
  streams = prepare_render_line_geometry(
    lines = streams,
    merge = merge,
    exclude_polygons = water_polygons,
    line_argument = "streams",
    polygon_argument = "water_polygons"
  )
  if (is_empty_scene_sf(streams)) {
    return(invisible(list()))
  }
  stream_width = if (is.null(width_column)) {
    width
  } else {
    as.numeric(streams[[width_column]])
  }

  # 5. Build terrain-sampled coordinates and their feature mapping.
  path_data = render_line_coords_by_width(
    lines = streams,
    heightmap = heightmap,
    extent = extent,
    zscale = zscale,
    color = watercolor,
    width = stream_width,
    force_by_feature = TRUE
  )
  coord_list = path_data$coords
  coord_width = path_data$width
  if (!length(coord_list)) {
    return(invisible(coord_list))
  }

  # 6. Densify against the terrain grid or apply the requested offset.
  if (isTRUE(densify)) {
    coord_list = densify_render_line_coords(
      coords = coord_list,
      heightmap = heightmap,
      zscale = zscale,
      offset = offset
    )
  } else if (!identical(offset, 0)) {
    coord_list = offset_render_line_coords(
      coords = coord_list,
      offset = offset / zscale
    )
  }

  # 7. Draw the preview paths and return the compatibility coordinate list.
  path_members = data.frame(
    water_path_id = seq_along(coord_list),
    render_line_feature_id = as.integer(path_data$feature_id),
    stringsAsFactors = FALSE
  )
  path_members$source_feature_id = I(path_data$source_feature_id)
  attr(coord_list, "path_members") = path_members
  for (coord_index in seq_along(coord_list)) {
    coord = coord_list[[coord_index]]
    if (is.matrix(coord) && nrow(coord) >= 2) {
      rgl::lines3d(
        coord,
        color = watercolor,
        tag = "water_path",
        lwd = coord_width[[coord_index]],
        line_antialias = FALSE
      )
    }
  }
  invisible(coord_list)
}

#' Collapse duplicated path vertices
#'
#' @param vertices Path vertex matrix.
#' @param minimum_step Default `sqrt(.Machine$double.eps)`. Minimum retained
#' distance between consecutive vertices.
#'
#' @return Path vertex matrix with consecutive duplicated vertices removed.
#' @keywords internal
collapse_render_highquality_path_vertices = function(
  vertices,
  minimum_step = sqrt(.Machine$double.eps)
) {
  vertices = as.matrix(vertices)
  if (nrow(vertices) < 2) {
    return(vertices)
  }
  finite_rows = stats::complete.cases(vertices)
  vertices = vertices[finite_rows, , drop = FALSE]
  if (nrow(vertices) < 2) {
    return(vertices)
  }
  minimum_step = suppressWarnings(as.numeric(minimum_step[[1L]]))
  if (!is.finite(minimum_step) || minimum_step < 0) {
    stop("`minimum_step` must be a non-negative finite number.", call. = FALSE)
  }
  keep = 1L
  if (nrow(vertices) > 2L) {
    for (vertex_index in seq.int(2L, nrow(vertices) - 1L)) {
      step_distance = sqrt(sum(
        (vertices[vertex_index, ] - vertices[utils::tail(keep, 1L), ])^2
      ))
      if (step_distance > minimum_step) {
        keep = c(keep, vertex_index)
      }
    }
  }
  final_index = nrow(vertices)
  while (
    length(keep) > 1L &&
      sqrt(sum(
        (vertices[final_index, ] - vertices[utils::tail(keep, 1L), ])^2
      )) <=
        minimum_step
  ) {
    keep = keep[-length(keep)]
  }
  vertices[c(keep, final_index), , drop = FALSE]
}

#' Make water path extrusion profile
#'
#' @return Two-column matrix defining a shallow rectangular extrusion profile.
#' @keywords internal
make_render_highquality_water_path_polygon = function() {
  height_ratio = 0.2
  matrix(
    c(
      -0.5,
      -height_ratio / 2,
      0.5,
      -height_ratio / 2,
      0.5,
      height_ratio / 2,
      -0.5,
      height_ratio / 2
    ),
    ncol = 2,
    byrow = TRUE
  )
}

#' Resolve render_highquality water path surface
#'
#' @return List containing the cached heightmap in scene units and effective zscale.
#' @keywords internal
resolve_render_highquality_water_path_surface = function() {
  heightmap = tryCatch(
    resolve_scene_render_heightmap(caller = "render_highquality"),
    error = function(e) NULL
  )
  if (!is.matrix(heightmap)) {
    heightmap = NULL
  }
  zscale = tryCatch(
    resolve_render_highquality_camera_zscale(),
    error = function(e) 1
  )
  scale_render_highquality_heightmap(
    heightmap = heightmap,
    zscale = zscale
  )
}

#' Make render_highquality water path meshes
#'
#' @param tasks Water path mesh task list.
#' @param verbose Default `FALSE`. Whether to display mesh-building progress.
#'
#' @return List of rayrender mesh objects.
#' @keywords internal
make_render_highquality_water_path_meshes = function(tasks, verbose = FALSE) {
  if (!length(tasks)) {
    return(list())
  }
  mesh_progress = new_render_highquality_progress_bar(
    verbose = verbose,
    label = "Converting stream lines to meshes",
    total = length(tasks)
  )
  meshes = vector("list", length(tasks))
  for (index in seq_along(tasks)) {
    meshes[[index]] = do.call(
      make_render_highquality_water_path_mesh,
      tasks[[index]]
    )
    if (!is.null(mesh_progress)) {
      mesh_progress$tick()
    }
  }
  Filter(Negate(is.null), meshes)
}

#' Make joined render_highquality water path meshes
#'
#' @param tasks Water path mesh task list.
#' @param verbose Default `FALSE`. Whether to display mesh-building progress.
#' @param ... Additional arguments passed to
#' [make_render_highquality_joined_water_path_mesh()].
#'
#' @return List of rayrender mesh objects.
#' @keywords internal
make_render_highquality_joined_water_path_meshes = function(
  tasks,
  verbose = FALSE,
  ...
) {
  if (!length(tasks)) {
    return(list())
  }
  groups = group_render_highquality_water_path_tasks(tasks)
  mesh_progress = new_render_highquality_progress_bar(
    verbose = verbose,
    label = "Converting stream lines to meshes",
    total = length(groups)
  )
  meshes = list()
  warned = FALSE
  warn_fallback = function(message) {
    if (!isTRUE(warned)) {
      warning(
        "Joined stream mesh generation failed; falling back to single-line stream meshes: ",
        message,
        call. = FALSE
      )
      warned <<- TRUE
    }
  }
  for (group in groups) {
    mesh = tryCatch(
      make_render_highquality_joined_water_path_mesh(group$tasks, ...),
      error = function(e) {
        warn_fallback(conditionMessage(e))
        NULL
      }
    )
    if (is.null(mesh)) {
      warn_fallback("empty joined mesh")
      meshes = c(
        meshes,
        make_render_highquality_water_path_meshes(
          group$tasks,
          verbose = FALSE
        )
      )
    } else {
      meshes[[length(meshes) + 1L]] = mesh
    }
    if (!is.null(mesh_progress)) {
      mesh_progress$tick()
    }
  }
  meshes
}

#' Group compatible render_highquality water path tasks
#'
#' @param tasks Water path mesh task list.
#'
#' @return List of grouped task lists.
#' @keywords internal
group_render_highquality_water_path_tasks = function(tasks) {
  same_number = function(x, y) {
    isTRUE(all.equal(
      suppressWarnings(as.numeric(x[1])),
      suppressWarnings(as.numeric(y[1])),
      tolerance = sqrt(.Machine$double.eps),
      check.attributes = FALSE
    ))
  }
  same_vector = function(x, y) {
    isTRUE(all.equal(
      suppressWarnings(as.numeric(x)),
      suppressWarnings(as.numeric(y)),
      tolerance = sqrt(.Machine$double.eps),
      check.attributes = FALSE
    ))
  }
  tasks_are_compatible = function(task, prototype) {
    same_number(task$width, prototype$width) &&
      same_number(task$zscale, prototype$zscale) &&
      same_vector(task$bbox_center, prototype$bbox_center) &&
      identical(task$heightmap, prototype$heightmap) &&
      identical(task$material, prototype$material)
  }

  groups = list()
  for (task in tasks) {
    matched_group = 0L
    for (group_index in seq_along(groups)) {
      if (
        tasks_are_compatible(
          task,
          groups[[group_index]]$prototype
        )
      ) {
        matched_group = group_index
        break
      }
    }
    if (matched_group == 0L) {
      groups[[length(groups) + 1L]] = list(
        prototype = task,
        tasks = list(task)
      )
    } else {
      groups[[matched_group]]$tasks[[
        length(groups[[matched_group]]$tasks) + 1L
      ]] =
        task
    }
  }
  groups
}

#' Make one joined render_highquality water path mesh
#'
#' @param tasks Compatible water path mesh tasks.
#' @param seal_epsilon Default `NULL`. Downward terrain sealing distance in scene
#' units. When `NULL`, uses a width-scaled epsilon.
#' @param bottom_cap Default `TRUE`. Whether to add a hidden bottom cap below the
#' terrain surface.
#' @param height Default `NULL`, which uses 20 percent of the path width.
#' Extrusion height in scene units.
#' @param extrusion_alignment Default `"above"`. Whether to place the extrusion
#' `"above"`, `"center"` it on, or place it `"below"` the path elevation.
#'
#' @return Rayrender mesh object.
#' @keywords internal
make_render_highquality_joined_water_path_mesh = function(
  tasks,
  seal_epsilon = NULL,
  bottom_cap = TRUE,
  height = NULL,
  extrusion_alignment = c("above", "center", "below")
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  orient_top_indices = function(vertices, indices) {
    if (!nrow(indices)) {
      return(indices)
    }
    oriented = indices
    keep = rep(TRUE, nrow(oriented))
    for (index in seq_len(nrow(oriented))) {
      triangle = vertices[oriented[index, ], , drop = FALSE]
      first_edge = triangle[2L, ] - triangle[1L, ]
      second_edge = triangle[3L, ] - triangle[1L, ]
      normal = c(
        first_edge[[2]] * second_edge[[3]] - first_edge[[3]] * second_edge[[2]],
        first_edge[[3]] * second_edge[[1]] - first_edge[[1]] * second_edge[[3]],
        first_edge[[1]] * second_edge[[2]] - first_edge[[2]] * second_edge[[1]]
      )
      if (
        !all(is.finite(normal)) ||
          sqrt(sum(normal^2)) <= .Machine$double.eps
      ) {
        keep[[index]] = FALSE
        next
      }
      if (normal[[2]] < 0) {
        oriented[index, c(2L, 3L)] = oriented[index, c(3L, 2L)]
      }
    }
    oriented[keep, , drop = FALSE]
  }

  group = prepare_render_highquality_water_path_line_group(tasks)
  if (is.null(group) || !length(group$lines)) {
    stop("No valid stream lines were available.")
  }
  lines = clamp_render_highquality_water_path_endpoints(
    group$lines,
    width = group$width
  )
  if (!length(lines)) {
    stop("Stream endpoint clamping removed all lines.")
  }
  footprint = make_render_highquality_water_path_buffer_footprint(
    lines = lines,
    width = group$width
  )
  if (is.null(footprint) || !length(footprint)) {
    stop("Stream buffering produced an empty footprint.")
  }
  terrain_triangles = make_render_highquality_water_path_valid_terrain_triangles(
    heightmap = group$heightmap,
    bbox = sf::st_bbox(footprint),
    margin = group$width
  )
  if (!nrow(terrain_triangles)) {
    stop("No valid terrain triangles intersect the stream footprint bounds.")
  }
  fragments = clip_render_highquality_water_path_footprint_to_terrain(
    footprint = footprint,
    terrain_triangles = terrain_triangles,
    area_epsilon = max(group$width, 1)^2 * 1e-12
  )
  if (!nrow(fragments)) {
    stop("The stream footprint did not intersect valid terrain.")
  }
  triangulated = triangulate_render_highquality_water_path_fragments(
    fragments = fragments,
    width = group$width
  )
  if (
    is.null(triangulated) ||
      !nrow(triangulated$vertices_xz) ||
      !nrow(triangulated$indices)
  ) {
    stop("The clipped stream footprint could not be triangulated.")
  }
  terrain_y = sample_render_highquality_water_path_surface(
    points_xz = triangulated$vertices_xz,
    heightmap = group$heightmap,
    terrain_triangles = terrain_triangles,
    tri_id = triangulated$vertex_tri_id
  )
  if (any(!is.finite(terrain_y))) {
    stop("Joined stream mesh height sampling produced non-finite values.")
  }
  if (is.null(height)) {
    height = group$width * 0.2
  } else {
    height = resolve_render_positive_number(height, "height")
  }
  extrusion_alignment = match.arg(extrusion_alignment)
  if (is.null(seal_epsilon)) {
    seal_epsilon = max(group$width, 1) * 1e-5
  } else {
    seal_epsilon = suppressWarnings(as.numeric(seal_epsilon[1]))
    if (!is.finite(seal_epsilon) || seal_epsilon < 0) {
      stop("`seal_epsilon` must be a single non-negative number.")
    }
  }
  surface_y = terrain_y + group$offset_scene
  top_y = switch(
    extrusion_alignment,
    above = surface_y + height,
    center = surface_y + height / 2,
    below = surface_y
  )
  bottom_y = switch(
    extrusion_alignment,
    above = terrain_y - seal_epsilon,
    center = surface_y - height / 2 - seal_epsilon,
    below = surface_y - height - seal_epsilon
  )
  top_vertices = cbind(
    triangulated$vertices_xz[, 1],
    top_y,
    triangulated$vertices_xz[, 2]
  )
  top_indices = orient_top_indices(
    vertices = top_vertices,
    indices = triangulated$indices
  )
  if (!nrow(top_indices)) {
    stop("Joined stream mesh triangulation produced no valid top faces.")
  }
  bottom_vertices = cbind(
    triangulated$vertices_xz[, 1],
    bottom_y,
    triangulated$vertices_xz[, 2]
  )
  vertex_count = nrow(top_vertices)
  bottom_indices = matrix(integer(0), ncol = 3)
  if (isTRUE(bottom_cap)) {
    bottom_indices = cbind(
      top_indices[, 3] + vertex_count,
      top_indices[, 2] + vertex_count,
      top_indices[, 1] + vertex_count
    )
  }

  # Top, bottom, and wall vertices are intentionally separate. A shared vertex
  # can carry only one shading normal, which would either facet the top surface
  # or smooth the hard stream edge into the walls.
  top_normals = interpolate_render_highquality_normals(
    points = top_vertices,
    heightmap = group$heightmap,
    zscale = 1
  )

  directed_edges = rbind(
    top_indices[, c(1L, 2L), drop = FALSE],
    top_indices[, c(2L, 3L), drop = FALSE],
    top_indices[, c(3L, 1L), drop = FALSE]
  )
  edge_keys = paste(
    pmin(directed_edges[, 1L], directed_edges[, 2L]),
    pmax(directed_edges[, 1L], directed_edges[, 2L]),
    sep = "_"
  )
  unique_edge_key = unique(edge_keys)
  edge_count = tabulate(match(edge_keys, unique_edge_key))
  boundary_edges = directed_edges[
    edge_count[match(edge_keys, unique_edge_key)] == 1L,
    ,
    drop = FALSE
  ]
  side_vertices = matrix(numeric(0), ncol = 3L)
  side_normals = matrix(numeric(0), ncol = 3L)
  side_indices = matrix(integer(0), ncol = 3L)
  if (nrow(boundary_edges)) {
    boundary_start = boundary_edges[, 1L]
    boundary_end = boundary_edges[, 2L]
    boundary_vertex = sort(unique(c(boundary_start, boundary_end)))
    boundary_vertex_count = length(boundary_vertex)
    boundary_start_index = match(boundary_start, boundary_vertex)
    boundary_end_index = match(boundary_end, boundary_vertex)
    side_vertices = rbind(
      top_vertices[boundary_vertex, , drop = FALSE],
      bottom_vertices[boundary_vertex, , drop = FALSE]
    )
    local_side_indices = rbind(
      cbind(
        boundary_start_index,
        boundary_end_index + boundary_vertex_count,
        boundary_end_index
      ),
      cbind(
        boundary_start_index,
        boundary_start_index + boundary_vertex_count,
        boundary_end_index + boundary_vertex_count
      )
    )
    side_first_edge =
      side_vertices[local_side_indices[, 2L], , drop = FALSE] -
      side_vertices[local_side_indices[, 1L], , drop = FALSE]
    side_second_edge =
      side_vertices[local_side_indices[, 3L], , drop = FALSE] -
      side_vertices[local_side_indices[, 1L], , drop = FALSE]
    side_face_cross = row_cross(side_first_edge, side_second_edge)
    side_normals = matrix(0, nrow = nrow(side_vertices), ncol = 3L)
    for (corner in seq_len(3L)) {
      corner_index = local_side_indices[, corner]
      corner_normal = rowsum(
        side_face_cross,
        group = corner_index,
        reorder = FALSE
      )
      normal_index = as.integer(rownames(corner_normal))
      side_normals[normal_index, ] =
        side_normals[normal_index, , drop = FALSE] + corner_normal
    }
    side_normals = normalize_render_highquality_rows(side_normals)
    side_normals = replace_invalid_render_highquality_vectors(
      side_normals,
      fallback = c(0, 0, 1)
    )
    side_indices = local_side_indices + 2L * vertex_count
  }

  indices = rbind(top_indices, bottom_indices, side_indices)
  if (!nrow(indices)) {
    stop("Joined stream mesh produced no faces.")
  }
  vertices = rbind(top_vertices, bottom_vertices, side_vertices)
  bottom_normals = matrix(
    c(0, -1, 0),
    nrow = vertex_count,
    ncol = 3L,
    byrow = TRUE
  )
  vertex_normals = rbind(top_normals, bottom_normals, side_normals)
  vertices = sweep(vertices, 2, group$bbox_center, FUN = "-")
  mesh = list(
    vb = t(cbind(vertices, 1)),
    it = t(indices),
    normals = t(vertex_normals)
  )
  class(mesh) = "mesh3d"
  rayrender::mesh3d_model(
    mesh,
    override_material = TRUE,
    material = group$material
  )
}

#' Prepare joined water path line group
#'
#' @param tasks Compatible water path mesh tasks.
#'
#' @return Prepared line group.
#' @keywords internal
prepare_render_highquality_water_path_line_group = function(tasks) {
  split_task_points = function(points) {
    points = as.matrix(points)
    if (!nrow(points) || ncol(points) < 3) {
      return(list())
    }
    separator = rowSums(!is.finite(points[, 1:3, drop = FALSE])) > 0
    groups = cumsum(separator)
    point_indices = split(seq_len(nrow(points)), groups)
    out = vector("list", length(point_indices))
    for (index in seq_along(point_indices)) {
      group_points = points[point_indices[[index]], , drop = FALSE]
      if (index > 1L && nrow(group_points) > 0) {
        group_points = group_points[-1L, , drop = FALSE]
      }
      group_points = group_points[
        stats::complete.cases(group_points[, c(1, 3), drop = FALSE]),
        ,
        drop = FALSE
      ]
      out[[index]] = group_points
    }
    out[vapply(out, nrow, integer(1)) > 0L]
  }

  if (!length(tasks)) {
    return(NULL)
  }
  prototype = tasks[[1]]
  width = suppressWarnings(as.numeric(prototype$width[1]))
  if (!is.finite(width) || width <= 0) {
    return(NULL)
  }
  heightmap_scene = scale_render_highquality_heightmap(
    heightmap = prototype$heightmap,
    zscale = prototype$zscale
  )$heightmap
  if (
    is.null(heightmap_scene) ||
      !is.matrix(heightmap_scene) ||
      nrow(heightmap_scene) < 2 ||
      ncol(heightmap_scene) < 2
  ) {
    return(NULL)
  }
  lines = list()
  offset_values = numeric(0)
  for (task in tasks) {
    point_groups = split_task_points(task$points)
    for (points in point_groups) {
      finite_xyz = stats::complete.cases(points[, 1:3, drop = FALSE])
      if (any(finite_xyz)) {
        terrain_y = interpolate_render_heightmap_height(
          heightmap_scene,
          points[finite_xyz, 1],
          points[finite_xyz, 3]
        )
        offset_values = c(
          offset_values,
          points[finite_xyz, 2] - terrain_y
        )
      }
      finite_xz = stats::complete.cases(points[, c(1, 3), drop = FALSE])
      line = points[finite_xz, c(1, 3), drop = FALSE]
      line = collapse_render_highquality_water_path_line(line)
      if (nrow(line) >= 2) {
        lines[[length(lines) + 1L]] = line
      }
    }
  }
  offset_values = offset_values[is.finite(offset_values)]
  offset_scene = if (length(offset_values)) {
    stats::median(offset_values, na.rm = TRUE)
  } else {
    0
  }
  if (!is.finite(offset_scene)) {
    offset_scene = 0
  }
  list(
    lines = lines,
    width = width,
    bbox_center = prototype$bbox_center,
    heightmap = heightmap_scene,
    zscale = 1,
    material = prototype$material,
    offset_scene = offset_scene,
    tasks = tasks
  )
}

#' Collapse duplicated water path line vertices
#'
#' @param line Two-column `x`/`z` line matrix.
#'
#' @return Line matrix with consecutive duplicate vertices removed.
#' @keywords internal
collapse_render_highquality_water_path_line = function(line) {
  line = as.matrix(line)
  if (!nrow(line) || ncol(line) < 2) {
    return(matrix(numeric(0), ncol = 2))
  }
  line = line[
    stats::complete.cases(line[, 1:2, drop = FALSE]),
    1:2,
    drop = FALSE
  ]
  if (nrow(line) < 2) {
    return(line)
  }
  distance = sqrt(rowSums(
    (line[-1, , drop = FALSE] - line[-nrow(line), , drop = FALSE])^2
  ))
  keep = c(TRUE, distance > sqrt(.Machine$double.eps))
  line[keep, , drop = FALSE]
}

#' Clamp nearby water path endpoints
#'
#' @param lines List of two-column `x`/`z` line matrices.
#' @param width Stream width in scene units.
#' @param snap_distance Default `width`. Maximum endpoint-to-line distance to
#' clamp.
#'
#' @return List of clamped line matrices.
#' @keywords internal
clamp_render_highquality_water_path_endpoints = function(
  lines,
  width,
  snap_distance = width
) {
  if (!length(lines)) {
    return(list())
  }
  width = suppressWarnings(as.numeric(width[1]))
  snap_distance = suppressWarnings(as.numeric(snap_distance[1]))
  if (!is.finite(width) || width <= 0 || !is.finite(snap_distance)) {
    return(lines)
  }
  source_lines = lines
  clamped_lines = lines
  for (line_index in seq_along(source_lines)) {
    line = source_lines[[line_index]]
    if (nrow(line) < 2) {
      next
    }
    endpoint_indices = unique(c(1L, nrow(line)))
    for (endpoint_index in endpoint_indices) {
      point = line[endpoint_index, ]
      nearest_distance = Inf
      nearest_point = NULL
      for (candidate_index in seq_along(source_lines)) {
        if (candidate_index == line_index) {
          next
        }
        candidate = source_lines[[candidate_index]]
        if (nrow(candidate) < 2) {
          next
        }
        segment_start = candidate[-nrow(candidate), , drop = FALSE]
        segment_end = candidate[-1L, , drop = FALSE]
        segment_delta = segment_end - segment_start
        segment_length2 = rowSums(segment_delta^2)
        valid_segment = is.finite(segment_length2) &
          segment_length2 > .Machine$double.eps
        if (!any(valid_segment)) {
          next
        }
        segment_start = segment_start[valid_segment, , drop = FALSE]
        segment_delta = segment_delta[valid_segment, , drop = FALSE]
        segment_length2 = segment_length2[valid_segment]
        point_delta = sweep(segment_start, 2, point, FUN = "-")
        t_value = -rowSums(point_delta * segment_delta) / segment_length2
        t_value = pmin(pmax(t_value, 0), 1)
        projected = segment_start + segment_delta * t_value
        distance = sqrt(rowSums(
          sweep(projected, 2, point, FUN = "-")^2
        ))
        best_index = which.min(distance)
        if (
          length(best_index) &&
            is.finite(distance[[best_index]]) &&
            distance[[best_index]] < nearest_distance
        ) {
          nearest_distance = distance[[best_index]]
          nearest_point = projected[best_index, ]
        }
      }
      if (
        !is.null(nearest_point) &&
          is.finite(nearest_distance) &&
          nearest_distance <= snap_distance
      ) {
        clamped_lines[[line_index]][endpoint_index, ] = nearest_point
      }
    }
  }
  precision = max(width, 1) * 1e-8
  out = list()
  for (line in clamped_lines) {
    line = round(line / precision) * precision
    line = collapse_render_highquality_water_path_line(line)
    if (nrow(line) >= 2) {
      out[[length(out) + 1L]] = line
    }
  }
  out
}

#' Make joined water path buffer footprint
#'
#' @param lines List of two-column `x`/`z` line matrices.
#' @param width Stream width in scene units.
#'
#' @return `sf` polygon geometry.
#' @keywords internal
make_render_highquality_water_path_buffer_footprint = function(lines, width) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  if (!length(lines)) {
    return(NULL)
  }
  line_geometries = lapply(lines, sf::st_linestring)
  line_sfc = do.call(sf::st_sfc, c(line_geometries, list(crs = sf::NA_crs_)))
  buffered = suppressWarnings(sf::st_buffer(
    line_sfc,
    dist = width / 2,
    nQuadSegs = 1,
    endCapStyle = "FLAT",
    joinStyle = "MITRE",
    mitreLimit = 1
  ))
  footprint = suppressWarnings(sf::st_union(buffered))
  footprint = assert_render_highquality_stream_footprint(footprint)
  if (is.null(footprint) || !length(footprint)) {
    return(NULL)
  }
  area = suppressWarnings(as.numeric(sf::st_area(footprint)))
  keep = is.finite(area) & area > max(width, 1)^2 * 1e-12
  footprint = footprint[keep]
  if (!length(footprint) || all(sf::st_is_empty(footprint))) {
    return(NULL)
  }
  footprint
}

#' Assert and repair stream footprint geometry
#'
#' @param footprint Footprint geometry.
#'
#' @return Valid polygon geometry or `NULL`.
#' @keywords internal
assert_render_highquality_stream_footprint = function(footprint) {
  if (is.null(footprint) || !length(footprint)) {
    return(NULL)
  }
  valid = tryCatch(
    suppressWarnings(sf::st_make_valid(footprint)),
    error = function(e) NULL
  )
  if (is.null(valid)) {
    valid = tryCatch(
      suppressWarnings(sf::st_buffer(footprint, 0)),
      error = function(e) NULL
    )
  }
  if (is.null(valid) || !length(valid)) {
    return(NULL)
  }
  valid = suppressWarnings(sf::st_collection_extract(valid, "POLYGON"))
  if (!length(valid) || all(sf::st_is_empty(valid))) {
    return(NULL)
  }
  valid[!sf::st_is_empty(valid)]
}

#' Make valid terrain triangle footprints for water paths
#'
#' @param heightmap Heightmap matrix in scene units.
#' @param bbox Default `NULL`. Optional stream footprint bounding box.
#' @param margin Default `0`. Extra scene-unit margin around `bbox`.
#'
#' @return `sf` object containing terrain triangle footprints.
#' @keywords internal
make_render_highquality_water_path_valid_terrain_triangles = function(
  heightmap,
  bbox = NULL,
  margin = 0
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  make_empty = function() {
    sf::st_sf(
      tri_id = integer(),
      row = integer(),
      col = integer(),
      triangle = character(),
      geometry = sf::st_sfc(crs = sf::NA_crs_)
    )
  }
  bbox_cell_range = function(min_value, max_value, center, upper) {
    start = max(1L, floor(min_value + center + 1) - 1L)
    end = min(upper, ceiling(max_value + center + 1))
    c(as.integer(start), as.integer(end))
  }

  empty = make_empty()
  if (
    is.null(heightmap) ||
      !is.matrix(heightmap) ||
      nrow(heightmap) < 2 ||
      ncol(heightmap) < 2
  ) {
    return(empty)
  }
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  row_range = c(1L, nr - 1L)
  col_range = c(1L, nc - 1L)
  if (!is.null(bbox)) {
    margin = suppressWarnings(as.numeric(margin[1]))
    if (!is.finite(margin) || margin < 0) {
      margin = 0
    }
    bbox = as.numeric(bbox)
    if (length(bbox) >= 4 && all(is.finite(bbox[1:4]))) {
      row_range = bbox_cell_range(
        min_value = bbox[[1]] - margin,
        max_value = bbox[[3]] + margin,
        center = (nr - 1) / 2,
        upper = nr - 1L
      )
      col_range = bbox_cell_range(
        min_value = bbox[[2]] - margin,
        max_value = bbox[[4]] + margin,
        center = (nc - 1) / 2,
        upper = nc - 1L
      )
    }
  }
  if (row_range[[1]] > row_range[[2]] || col_range[[1]] > col_range[[2]]) {
    return(empty)
  }
  row_center = (nr - 1) / 2
  col_center = (nc - 1) / 2
  geometries = list()
  tri_id = integer(0)
  rows = integer(0)
  cols = integer(0)
  triangle = character(0)
  next_id = 1L
  for (row in seq.int(row_range[[1]], row_range[[2]])) {
    for (col in seq.int(col_range[[1]], col_range[[2]])) {
      cell_heights = heightmap[cbind(
        c(row, row + 1L, row, row + 1L),
        c(col, col, col + 1L, col + 1L)
      )]
      if (!all(is.finite(cell_heights))) {
        next
      }
      x0 = row - 1 - row_center
      x1 = row - row_center
      z0 = col - 1 - col_center
      z1 = col - col_center
      top_ring = rbind(
        c(x0, z0),
        c(x1, z0),
        c(x0, z1),
        c(x0, z0)
      )
      bottom_ring = rbind(
        c(x1, z1),
        c(x0, z1),
        c(x1, z0),
        c(x1, z1)
      )
      geometries[[length(geometries) + 1L]] = sf::st_polygon(list(top_ring))
      tri_id = c(tri_id, next_id)
      rows = c(rows, row)
      cols = c(cols, col)
      triangle = c(triangle, "top")
      next_id = next_id + 1L
      geometries[[length(geometries) + 1L]] = sf::st_polygon(list(bottom_ring))
      tri_id = c(tri_id, next_id)
      rows = c(rows, row)
      cols = c(cols, col)
      triangle = c(triangle, "bottom")
      next_id = next_id + 1L
    }
  }
  if (!length(geometries)) {
    return(empty)
  }
  sf::st_sf(
    tri_id = tri_id,
    row = rows,
    col = cols,
    triangle = triangle,
    geometry = do.call(sf::st_sfc, c(geometries, list(crs = sf::NA_crs_)))
  )
}

#' Clip water path footprint to valid terrain
#'
#' @param footprint Stream footprint geometry.
#' @param terrain_triangles Terrain triangle `sf` object.
#' @param area_epsilon Minimum fragment area.
#'
#' @return `sf` polygon fragments.
#' @keywords internal
clip_render_highquality_water_path_footprint_to_terrain = function(
  footprint,
  terrain_triangles,
  area_epsilon = 1e-12
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  empty = terrain_triangles[0, , drop = FALSE]
  if (
    is.null(footprint) ||
      !length(footprint) ||
      !nrow(terrain_triangles)
  ) {
    return(empty)
  }
  footprint_sf = sf::st_sf(geometry = footprint)
  fragments = tryCatch(
    suppressWarnings(sf::st_intersection(terrain_triangles, footprint_sf)),
    error = function(e) NULL
  )
  if (is.null(fragments) || !nrow(fragments)) {
    return(empty)
  }
  fragments = suppressWarnings(sf::st_collection_extract(fragments, "POLYGON"))
  fragments = suppressWarnings(sf::st_cast(fragments, "POLYGON", warn = FALSE))
  if (!nrow(fragments)) {
    return(empty)
  }
  area = suppressWarnings(as.numeric(sf::st_area(fragments)))
  keep = is.finite(area) & area > area_epsilon & !sf::st_is_empty(fragments)
  fragments[keep, , drop = FALSE]
}

#' Triangulate water path footprint fragments
#'
#' @param fragments Clipped footprint fragments.
#' @param width Default `1`. Stream width in scene units.
#'
#' @return List containing `vertices_xz`, `indices`, and `vertex_tri_id`.
#' @keywords internal
triangulate_render_highquality_water_path_fragments = function(
  fragments,
  width = 1
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The `sf` package is required for joined stream meshes.")
  }
  signed_area2 = function(points) {
    (points[2L, 1] - points[1L, 1]) *
      (points[3L, 2] - points[1L, 2]) -
      (points[2L, 2] - points[1L, 2]) *
        (points[3L, 1] - points[1L, 1])
  }
  ring_area = function(points) {
    next_index = c(seq_len(nrow(points))[-1L], 1L)
    sum(
      points[, 1] * points[next_index, 2] - points[next_index, 1] * points[, 2]
    ) /
      2
  }
  edge_side = function(points, edge_start, edge_end) {
    (points[, 1] - edge_start[[1]]) *
      (edge_end[[2]] - edge_start[[2]]) -
      (points[, 2] - edge_start[[2]]) *
        (edge_end[[1]] - edge_start[[1]])
  }
  points_in_triangle = function(points, triangle) {
    area = signed_area2(triangle)
    if (!is.finite(area) || abs(area) <= .Machine$double.eps) {
      return(rep(FALSE, nrow(points)))
    }
    sign1 = edge_side(points, triangle[1L, ], triangle[2L, ])
    sign2 = edge_side(points, triangle[2L, ], triangle[3L, ])
    sign3 = edge_side(points, triangle[3L, ], triangle[1L, ])
    eps = sqrt(.Machine$double.eps)
    (sign1 >= -eps & sign2 >= -eps & sign3 >= -eps) |
      (sign1 <= eps & sign2 <= eps & sign3 <= eps)
  }
  extract_triangle_polygons = function(triangles, fragment) {
    if (is.null(triangles) || !length(triangles)) {
      return(NULL)
    }
    triangles = suppressWarnings(
      sf::st_collection_extract(triangles, "POLYGON")
    )
    triangles = suppressWarnings(
      sf::st_cast(triangles, "POLYGON", warn = FALSE)
    )
    if (!length(triangles)) {
      return(NULL)
    }
    triangles = triangles[!sf::st_is_empty(triangles)]
    if (!length(triangles)) {
      return(NULL)
    }
    representative_points = suppressWarnings(
      sf::st_point_on_surface(triangles)
    )
    inside = tryCatch(
      as.logical(sf::st_covered_by(
        representative_points,
        fragment,
        sparse = FALSE
      )[, 1]),
      error = function(e) rep(TRUE, length(triangles))
    )
    triangles[inside]
  }
  earclip_fragment = function(fragment) {
    geometry = fragment[[1]]
    if (!inherits(geometry, "XY") || !inherits(geometry, "POLYGON")) {
      return(NULL)
    }
    if (length(geometry) != 1L) {
      return(NULL)
    }
    ring = geometry[[1]]
    if (nrow(ring) > 1L && all(ring[1, ] == ring[nrow(ring), ])) {
      ring = ring[-nrow(ring), , drop = FALSE]
    }
    ring = collapse_render_highquality_water_path_line(ring)
    if (nrow(ring) < 3) {
      return(NULL)
    }
    if (ring_area(ring) < 0) {
      ring = ring[rev(seq_len(nrow(ring))), , drop = FALSE]
    }
    remaining = seq_len(nrow(ring))
    triangles = list()
    guard = 0L
    while (length(remaining) > 3L && guard < nrow(ring)^2) {
      guard = guard + 1L
      clipped = FALSE
      for (position in seq_along(remaining)) {
        previous = remaining[[ifelse(
          position == 1L,
          length(remaining),
          position - 1L
        )]]
        current = remaining[[position]]
        next_val = remaining[[ifelse(
          position == length(remaining),
          1L,
          position + 1L
        )]]
        ear = ring[c(previous, current, next_val), , drop = FALSE]
        if (signed_area2(ear) <= 0) {
          next
        }
        other = setdiff(remaining, c(previous, current, next_val))
        if (
          length(other) &&
            any(points_in_triangle(
              ring[other, , drop = FALSE],
              ear
            ))
        ) {
          next
        }
        triangles[[length(triangles) + 1L]] = ear
        remaining = remaining[-position]
        clipped = TRUE
        break
      }
      if (!clipped) {
        return(NULL)
      }
    }
    if (length(remaining) == 3L) {
      triangles[[length(triangles) + 1L]] =
        ring[remaining, , drop = FALSE]
    }
    if (!length(triangles)) {
      return(NULL)
    }
    triangle_geometries = lapply(triangles, function(triangle) {
      sf::st_polygon(list(rbind(triangle, triangle[1, ])))
    })
    do.call(sf::st_sfc, c(triangle_geometries, list(crs = sf::NA_crs_)))
  }
  triangulate_fragment = function(fragment) {
    triangles = NULL
    if (exists("st_triangulate_constrained", envir = asNamespace("sf"))) {
      triangles = tryCatch(
        suppressWarnings(sf::st_triangulate_constrained(fragment)),
        error = function(e) NULL
      )
      triangles = extract_triangle_polygons(triangles, fragment)
    }
    if (is.null(triangles) || !length(triangles)) {
      triangles = tryCatch(
        suppressWarnings(sf::st_triangulate(fragment)),
        error = function(e) NULL
      )
      triangles = extract_triangle_polygons(triangles, fragment)
    }
    if (is.null(triangles) || !length(triangles)) {
      triangles = earclip_fragment(fragment)
    }
    triangles
  }
  extract_triangle_vertices = function(geometry) {
    coords = sf::st_coordinates(geometry)
    if (!nrow(coords)) {
      return(list())
    }
    coords = coords[, 1:2, drop = FALSE]
    if (
      nrow(coords) > 1L &&
        all(coords[1, ] == coords[nrow(coords), ])
    ) {
      coords = coords[-nrow(coords), , drop = FALSE]
    }
    coords = collapse_render_highquality_water_path_line(coords)
    if (nrow(coords) < 3) {
      return(list())
    }
    if (nrow(coords) == 3L) {
      return(list(coords))
    }
    out = vector("list", nrow(coords) - 2L)
    for (index in seq_len(nrow(coords) - 2L)) {
      out[[index]] =
        coords[c(1L, index + 1L, index + 2L), , drop = FALSE]
    }
    out
  }

  if (is.null(fragments) || !nrow(fragments)) {
    return(NULL)
  }
  precision = max(width, 1) * 1e-8
  vertices = matrix(numeric(0), ncol = 2)
  indices = matrix(integer(0), ncol = 3)
  vertex_tri_id = integer(0)
  vertex_lookup = integer(0)
  for (fragment_index in seq_len(nrow(fragments))) {
    fragment = sf::st_geometry(fragments[fragment_index, , drop = FALSE])
    triangles = triangulate_fragment(fragment)
    if (is.null(triangles) || !length(triangles)) {
      next
    }
    for (triangle_index in seq_along(triangles)) {
      triangle_vertices = extract_triangle_vertices(triangles[[triangle_index]])
      for (triangle_vertex in triangle_vertices) {
        area2 = signed_area2(triangle_vertex)
        if (!is.finite(area2) || abs(area2) <= precision^2) {
          next
        }
        keys = paste(
          round(triangle_vertex[, 1] / precision),
          round(triangle_vertex[, 2] / precision),
          sep = "_"
        )
        triangle_indices = integer(3)
        for (vertex_index in seq_len(3L)) {
          key = keys[[vertex_index]]
          if (key %in% names(vertex_lookup)) {
            lookup_index = vertex_lookup[[key]]
          } else {
            vertices = rbind(vertices, triangle_vertex[vertex_index, ])
            vertex_tri_id = c(vertex_tri_id, fragments$tri_id[[fragment_index]])
            lookup_index = nrow(vertices)
            vertex_lookup[[key]] = lookup_index
          }
          triangle_indices[[vertex_index]] = lookup_index
        }
        if (length(unique(triangle_indices)) == 3L) {
          indices = rbind(indices, triangle_indices)
        }
      }
    }
  }
  list(
    vertices_xz = vertices,
    indices = indices,
    vertex_tri_id = vertex_tri_id
  )
}

#' Sample joined water path terrain surface
#'
#' @param points_xz Two-column `x`/`z` point matrix.
#' @param heightmap Heightmap matrix in scene units.
#' @param terrain_triangles Default `NULL`. Terrain triangle metadata.
#' @param tri_id Default `NULL`. Terrain triangle id for each point.
#'
#' @return Numeric terrain heights.
#' @keywords internal
sample_render_highquality_water_path_surface = function(
  points_xz,
  heightmap,
  terrain_triangles = NULL,
  tri_id = NULL
) {
  calculate_triangle_height = function(
    heightmap,
    points_xz,
    terrain_triangles
  ) {
    row_col = render_heightmap_row_col(
      heightmap,
      points_xz[, 1],
      points_xz[, 2],
      clamp = FALSE
    )
    row_weight = row_col$row - terrain_triangles$row
    col_weight = row_col$col - terrain_triangles$col
    height00 = heightmap[cbind(
      terrain_triangles$row,
      terrain_triangles$col
    )]
    height10 = heightmap[cbind(
      terrain_triangles$row + 1L,
      terrain_triangles$col
    )]
    height01 = heightmap[cbind(
      terrain_triangles$row,
      terrain_triangles$col + 1L
    )]
    height11 = heightmap[cbind(
      terrain_triangles$row + 1L,
      terrain_triangles$col + 1L
    )]
    heights = numeric(nrow(points_xz))
    top_triangle = terrain_triangles$triangle == "top"
    heights[top_triangle] = height00[top_triangle] +
      row_weight[top_triangle] *
        (height10[top_triangle] - height00[top_triangle]) +
      col_weight[top_triangle] *
        (height01[top_triangle] - height00[top_triangle])
    heights[!top_triangle] = height11[!top_triangle] +
      (1 - col_weight[!top_triangle]) *
        (height10[!top_triangle] - height11[!top_triangle]) +
      (1 - row_weight[!top_triangle]) *
        (height01[!top_triangle] - height11[!top_triangle])
    heights
  }

  points_xz = as.matrix(points_xz)
  heights = interpolate_render_heightmap_height(
    heightmap,
    points_xz[, 1],
    points_xz[, 2]
  )
  if (
    !is.null(terrain_triangles) &&
      !is.null(tri_id) &&
      length(tri_id) == nrow(points_xz)
  ) {
    triangle_index = match(tri_id, terrain_triangles$tri_id)
    valid = !is.na(triangle_index)
    exact_heights = rep(NA_real_, nrow(points_xz))
    if (any(valid)) {
      exact_heights[
        valid
      ] = calculate_triangle_height(
        heightmap = heightmap,
        points_xz = points_xz[valid, , drop = FALSE],
        terrain_triangles = terrain_triangles[
          triangle_index[valid],
          ,
          drop = FALSE
        ]
      )
      heights[is.finite(exact_heights)] = exact_heights[is.finite(
        exact_heights
      )]
    }
  }
  heights
}

#' Make render_highquality water path mesh
#'
#' @param points Path points in rgl scene coordinates.
#' @param bbox_center Scene bounding box center.
#' @param width Stream width.
#' @param heightmap Default `NULL`. Cached heightmap matrix.
#' @param zscale Effective zscale.
#' @param material Rayrender material.
#' @param segment_start Default `1`. First segment index to emit.
#' @param segment_end Default `NULL`. Last segment index to emit.
#' @param cap_start Default `TRUE`. Whether to cap the first emitted segment.
#' @param cap_end Default `TRUE`. Whether to cap the last emitted segment.
#'
#' @return Rayrender mesh object.
#' @keywords internal
make_render_highquality_water_path_mesh = function(
  points,
  bbox_center,
  width,
  heightmap = NULL,
  zscale = 1,
  material,
  segment_start = 1L,
  segment_end = NULL,
  cap_start = TRUE,
  cap_end = TRUE
) {
  points = as.matrix(points)
  if (is.null(segment_end)) {
    points = points[stats::complete.cases(points), , drop = FALSE]
  } else if (any(!stats::complete.cases(points))) {
    return(NULL)
  }
  if (nrow(points) < 2) {
    return(NULL)
  }
  if (is.null(segment_end)) {
    points = densify_render_highquality_path_points(
      points = points,
      width = width,
      heightmap = heightmap,
      zscale = zscale
    )
    if (nrow(points) < 2) {
      return(NULL)
    }
    points = collapse_render_highquality_path_vertices(
      points,
      minimum_step = max(
        abs(suppressWarnings(as.numeric(width[[1L]]))) * 0.01,
        sqrt(.Machine$double.eps)
      )
    )
    if (nrow(points) < 2) {
      return(NULL)
    }
  }
  if (is.null(segment_end)) {
    segment_end = nrow(points) - 1L
  }
  segment_start = suppressWarnings(as.integer(segment_start[1]))
  segment_end = suppressWarnings(as.integer(segment_end[1]))
  if (
    !is.finite(segment_start) ||
      !is.finite(segment_end) ||
      segment_start < 1L ||
      segment_end < segment_start
  ) {
    return(NULL)
  }
  segment_end = min(segment_end, nrow(points) - 1L)
  if (segment_end < segment_start) {
    return(NULL)
  }
  height_ratio = diff(range(make_render_highquality_water_path_polygon()[, 2]))
  half_width = width / 2
  half_thickness = width * height_ratio / 2
  normals = interpolate_render_highquality_normals(
    points = points,
    heightmap = heightmap,
    zscale = zscale
  )
  tangents = calculate_render_highquality_path_tangents(
    points = points,
    normals = normals
  )
  side_vectors = normalize_render_highquality_rows(row_cross(tangents, normals))
  side_vectors = replace_invalid_render_highquality_vectors(
    side_vectors,
    fallback = c(0, 0, 1)
  )

  edge_centers = make_render_highquality_path_edge_centers(
    points = points,
    side_vectors = side_vectors,
    half_width = half_width,
    heightmap = heightmap,
    zscale = zscale
  )
  left_center = edge_centers$left
  right_center = edge_centers$right
  left_normals = interpolate_render_highquality_normals(
    points = left_center,
    heightmap = heightmap,
    zscale = zscale
  )
  right_normals = interpolate_render_highquality_normals(
    points = right_center,
    heightmap = heightmap,
    zscale = zscale
  )

  left_top = left_center + left_normals * half_thickness
  right_top = right_center + right_normals * half_thickness
  left_bottom = left_center - left_normals * half_thickness
  right_bottom = right_center - right_normals * half_thickness

  segment_indices = seq.int(segment_start, segment_end)
  next_indices = segment_indices + 1L
  vertices = rbind(
    make_render_highquality_quad_rows(
      left_top[segment_indices, , drop = FALSE],
      left_top[next_indices, , drop = FALSE],
      right_top[next_indices, , drop = FALSE],
      right_top[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_quad_rows(
      left_bottom[segment_indices, , drop = FALSE],
      right_bottom[segment_indices, , drop = FALSE],
      right_bottom[next_indices, , drop = FALSE],
      left_bottom[next_indices, , drop = FALSE]
    ),
    make_render_highquality_quad_rows(
      left_bottom[segment_indices, , drop = FALSE],
      left_bottom[next_indices, , drop = FALSE],
      left_top[next_indices, , drop = FALSE],
      left_top[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_quad_rows(
      right_bottom[segment_indices, , drop = FALSE],
      right_top[segment_indices, , drop = FALSE],
      right_top[next_indices, , drop = FALSE],
      right_bottom[next_indices, , drop = FALSE]
    )
  )
  vertex_normals = rbind(
    make_render_highquality_quad_rows(
      left_normals[segment_indices, , drop = FALSE],
      left_normals[next_indices, , drop = FALSE],
      right_normals[next_indices, , drop = FALSE],
      right_normals[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_quad_rows(
      -left_normals[segment_indices, , drop = FALSE],
      -right_normals[segment_indices, , drop = FALSE],
      -right_normals[next_indices, , drop = FALSE],
      -left_normals[next_indices, , drop = FALSE]
    ),
    make_render_highquality_quad_rows(
      side_vectors[segment_indices, , drop = FALSE],
      side_vectors[next_indices, , drop = FALSE],
      side_vectors[next_indices, , drop = FALSE],
      side_vectors[segment_indices, , drop = FALSE]
    ),
    make_render_highquality_quad_rows(
      -side_vectors[segment_indices, , drop = FALSE],
      -side_vectors[segment_indices, , drop = FALSE],
      -side_vectors[next_indices, , drop = FALSE],
      -side_vectors[next_indices, , drop = FALSE]
    )
  )
  if (isTRUE(cap_start)) {
    vertices = rbind(
      vertices,
      make_render_highquality_quad_rows(
        matrix(left_bottom[segment_start, ], nrow = 1L),
        matrix(left_top[segment_start, ], nrow = 1L),
        matrix(right_top[segment_start, ], nrow = 1L),
        matrix(right_bottom[segment_start, ], nrow = 1L)
      )
    )
    vertex_normals = rbind(
      vertex_normals,
      make_render_highquality_quad_rows(
        matrix(-tangents[segment_start, ], nrow = 1L),
        matrix(-tangents[segment_start, ], nrow = 1L),
        matrix(-tangents[segment_start, ], nrow = 1L),
        matrix(-tangents[segment_start, ], nrow = 1L)
      )
    )
  }
  if (isTRUE(cap_end)) {
    end_index = segment_end + 1L
    vertices = rbind(
      vertices,
      make_render_highquality_quad_rows(
        matrix(left_bottom[end_index, ], nrow = 1L),
        matrix(right_bottom[end_index, ], nrow = 1L),
        matrix(right_top[end_index, ], nrow = 1L),
        matrix(left_top[end_index, ], nrow = 1L)
      )
    )
    vertex_normals = rbind(
      vertex_normals,
      make_render_highquality_quad_rows(
        matrix(tangents[end_index, ], nrow = 1L),
        matrix(tangents[end_index, ], nrow = 1L),
        matrix(tangents[end_index, ], nrow = 1L),
        matrix(tangents[end_index, ], nrow = 1L)
      )
    )
  }

  quad_starts = seq(1L, nrow(vertices), by = 4L)
  indices = rbind(
    cbind(quad_starts, quad_starts + 1L, quad_starts + 2L),
    cbind(quad_starts, quad_starts + 2L, quad_starts + 3L)
  )
  first_edge = vertices[indices[, 2L], , drop = FALSE] -
    vertices[indices[, 1L], , drop = FALSE]
  second_edge = vertices[indices[, 3L], , drop = FALSE] -
    vertices[indices[, 1L], , drop = FALSE]
  face_cross = row_cross(first_edge, second_edge)
  face_area = sqrt(rowSums(face_cross^2))
  average_normal = vertex_normals[indices[, 1L], , drop = FALSE] +
    vertex_normals[indices[, 2L], , drop = FALSE] +
    vertex_normals[indices[, 3L], , drop = FALSE]
  average_normal_length = sqrt(rowSums(average_normal^2))
  valid_face = stats::complete.cases(face_cross) &
    is.finite(face_area) &
    face_area > max(abs(width), 1)^2 * 1e-12 &
    stats::complete.cases(average_normal) &
    is.finite(average_normal_length) &
    average_normal_length > sqrt(.Machine$double.eps)
  if (!all(valid_face)) {
    indices = indices[valid_face, , drop = FALSE]
    face_cross = face_cross[valid_face, , drop = FALSE]
    face_area = face_area[valid_face]
    average_normal = average_normal[valid_face, , drop = FALSE]
    average_normal_length = average_normal_length[valid_face]
  }
  if (!nrow(indices)) {
    return(NULL)
  }
  face_normal = face_cross / face_area
  average_normal = average_normal / average_normal_length
  reverse_face = rowSums(face_normal * average_normal) < 0
  if (any(reverse_face)) {
    reverse_index = indices[reverse_face, 2L]
    indices[reverse_face, 2L] = indices[reverse_face, 3L]
    indices[reverse_face, 3L] = reverse_index
  }
  vertices = sweep(vertices, 2, bbox_center, FUN = "-")
  mesh = list(
    vb = t(cbind(vertices, 1)),
    it = t(indices),
    normals = t(vertex_normals)
  )
  class(mesh) = "mesh3d"
  rayrender::mesh3d_model(
    mesh,
    override_material = TRUE,
    material = material
  )
}

make_render_highquality_water_path_material = function(
  color,
  water_material,
  water_roughness,
  water_ior,
  water_attenuation,
  water_surface_color
) {
  if (is.null(color) || length(color) == 0) {
    color = "white"
  }
  surface_color = if (water_surface_color) {
    convert_color(color, as_hex = TRUE)
  } else {
    "white"
  }
  if (identical(water_material, "microfacet")) {
    return(rayrender::microfacet(
      color = surface_color,
      roughness = water_roughness,
      transmission = TRUE,
      eta = water_ior,
      kappa = water_attenuation
    ))
  }
  attenuation = (1 - convert_color(color)) * water_attenuation
  rayrender::dielectric(
    color = surface_color,
    refraction = water_ior,
    attenuation = attenuation
  )
}
