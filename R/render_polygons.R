#'@title Render Polygons
#'
#'@description Adds 3D polygons to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#' @param polygon `sf` object, "SpatialPolygon" `sp` object,  or xy coordinates
#' of polygon represented in a way that can be processed by `xy.coords()`.  If
#' xy-coordinate based polygons are open, they will be closed by adding an
#' edge from the last point to the first.
#' @param extent Either an object representing the spatial extent of the 3D scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax", "ymin", "ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use extent metadata cached by [plot_3d()] or [plot_gg()].
#' @param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#' to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#' for non-ggplot scenes.
#' @param crs Default `NULL`. CRS of the input numeric x/y coordinates, or CRS to assign to CRS-less spatial data before transforming it into the active scene CRS. If spatial data already carries a CRS, that CRS is used automatically.
#' @param color Default `black`. Color of the polygon. Use `"height"` to color polygons by the cached [plot_gg()] height aesthetic palette using `data_column_top`, `data_column_bottom`, or `top`.
#' @param top Default `1`. Extruded top distance. If this equals `bottom`, the polygon will not be
#' extruded and just the one side will be rendered.
#' @param bottom Default `0`. Extruded bottom distance. If this equals `top`, the polygon will not be
#' extruded and just the one side will be rendered.
#' @param data_column_top Default `NULL`. A string indicating the column in the `sf` object to use
#' to specify the top of the extruded polygon. Values are coerced to numeric, and rows with missing or non-finite values after coercion are omitted.
#' @param data_column_bottom Default `NULL`. A string indicating the column in the `sf` object to use
#' to specify the bottom of the extruded polygon. Values are coerced to numeric, and rows with missing or non-finite values after coercion are omitted.
#' @param scale_data Default `1`. If specifying `data_column_top` or `data_column_bottom`, how
#' much to scale that value when rendering. If used with `vertical_exaggeration`, both are applied.
#' @param parallel Default `FALSE`. If `TRUE`, polygons will be extruded in parallel, which
#' may be faster (depending on how many geometries are in `polygon`).
#' @param holes Default `0`. If passing in a polygon directly, this specifies which index represents
#' the holes in the polygon. See the `earcut` function in the `decido` package for more information.
#' @param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#' of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'  All points are assumed to be evenly spaced.
#' @param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#' @param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#' @param alpha Default `1`. Transparency of the polygons.
#' @param lit Default `TRUE`. Whether to light the polygons.
#' @param light_altitude Default `c(45, 60)`. Degree(s) from the horizon from which to light the polygons.
#' @param light_direction Default `c(45, 60)`. Degree(s) from north from which to light the polygons.
#' @param light_intensity Default `0.3`. Intensity of the specular highlight on the polygons.
#' @param light_relative Default `FALSE`. Whether the light direction should be taken relative to the camera,
#' or absolute.
#' @param clear_previous Default `FALSE`. If `TRUE`, clears all existing
#' polygons. A clear-only call returns without rendering a replacement.
#' @param filter_to_extent Default `TRUE`. If `TRUE`, polygon data outside the scene extent is omitted. Spatial polygon inputs are cropped to the extent. For scenes created with [plot_gg()], filtering uses the ggplot panel extent rather than the full rendered 3D ggplot extent.
#' @param ... Additional arguments passed to `rgl::triangles3d()`.
#' @export
#' @examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' #Render the county borders as polygons in Monterey Bay
#' montereybay_spatial |>
#'   sphere_shade(texture = "desert", vertical_exaggeration = 20) |>
#'   add_shadow(ray_shade(vertical_exaggeration = 4)) |>
#'   plot_3d(water = TRUE, vertical_exaggeration = 4, windowsize = 800, watercolor = "dodgerblue")
#' render_camera(theta = 140,  phi = 55, zoom = 0.85, fov = 30)
#'
#' #We will apply a negative buffer to create space between adjacent polygons. You may
#' #have to call `sf::sf_use_s2(FALSE)` before running this code to get it to run.
#' sf::sf_use_s2(FALSE)
#' mont_county_buff = sf::st_simplify(sf::st_buffer(monterey_counties_sf,-0.003), dTolerance=0.001)
#'
#' render_polygons(mont_county_buff, top = 2000,
#'                 parallel = FALSE)
#' render_snapshot()
#' #We can specify the bottom of the polygons as well. Here I float the polygons above the surface
#' #by specifying the bottom argument. We clear the previous polygons with `clear_previous = TRUE`.
#' render_camera(theta=-60,  phi=20, zoom = 0.85, fov=0)
#' render_polygons(mont_county_buff, bottom = 24000, top=25000,
#'                 parallel=FALSE,clear_previous=TRUE)
#' render_snapshot()
#' #We can set the height of the data to a column in the sf object: we'll use the land area.
#' #We'll have to scale this value because its max value is 2.6 billion:
#' render_camera(theta=-60,  phi=40, zoom = 0.85, fov=30)
#' render_polygons(mont_county_buff, data_column_top = "ALAND",
#'                 scale_data = 300/(5E7), color = "chartreuse4",
#'                 clear_previous = TRUE)
#' render_zaxis(zaxis_data = "polygon", zaxis_location = "topright",
#'              zaxis_title_location = "top")
#' render_snapshot()
#' #This function also works with `render_highquality()`
#' render_highquality(samples = 16, min_variance = 0)
render_polygons = function(
  polygon,
  extent = NULL,
  panel = NULL,
  color = "red",
  top = 1,
  bottom = NA,
  data_column_top = NULL,
  data_column_bottom = NULL,
  heightmap = NULL,
  zscale = 1,
  vertical_exaggeration = 1,
  scale_data = 1,
  parallel = FALSE,
  holes = 0,
  alpha = 1,
  lit = TRUE,
  light_altitude = c(45, 30),
  light_direction = c(315, 135),
  light_intensity = 0.3,
  light_relative = FALSE,
  clear_previous = FALSE,
  crs = NULL,
  filter_to_extent = TRUE,
  ...
) {
  if (
    is_render_clear_only_call(
      clear_previous,
      match.call(),
      function() rgl::pop3d(tag = "polygon3d")
    )
  ) {
    return(invisible(NULL))
  }
  validate_filter_to_extent(filter_to_extent, caller = "render_polygons")
  warn_scale_data_with_vertical_exaggeration(
    scale_data_missing = missing(scale_data),
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_polygons"
  )
  zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_polygons"
  )
  heightmap = resolve_scene_render_heightmap(
    heightmap,
    caller = "render_polygons"
  )
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if (!(length(find.package("rayrender", quiet = TRUE)) > 0)) {
    stop("rayrender required to use render_polygon()")
  }
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "render_polygons",
    panel = panel
  )
  if (
    inherits(polygon, "SpatialPolygonsDataFrame") ||
      inherits(polygon, "SpatialPolygons")
  ) {
    polygon = sf::st_as_sf(polygon)
  }
  if (inherits(polygon, "sfc")) {
    polygon = sf::st_sf(geometry = polygon)
  }
  if (inherits(polygon, "sfg")) {
    polygon = sf::st_sf(geometry = sf::st_sfc(polygon))
  }
  if (
    inherits(polygon, "sf") ||
      inherits(polygon, "sfc") ||
      inherits(polygon, "sfg")
  ) {
    n_polygon_before_filter = if (inherits(polygon, "sf")) {
      nrow(polygon)
    } else if (inherits(polygon, "sfc")) {
      length(polygon)
    } else {
      1
    }
    scene_polygon = auto_transform_scene_sf(
      sf_object = polygon,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      crs = crs,
      caller = "render_polygons"
    )
    polygon = scene_polygon$object
    if (!is.null(scene_polygon$extent)) {
      extent = scene_polygon$extent
    }
    filtered_polygon = filter_scene_sf_to_extent(
      sf_object = polygon,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      filter_to_extent = filter_to_extent,
      caller = "render_polygons"
    )
    polygon = filtered_polygon$object
    if (!is.null(filtered_polygon$source_index)) {
      top = subset_render_arg_by_index(
        top,
        filtered_polygon$source_index,
        n_polygon_before_filter
      )
      bottom = subset_render_arg_by_index(
        bottom,
        filtered_polygon$source_index,
        n_polygon_before_filter
      )
    }
    if (is_empty_scene_sf(polygon)) {
      return(invisible(NULL))
    }
  }
  if (!is.null(data_column_top) || !is.null(data_column_bottom)) {
    n_polygon_before_data_drop = nrow(polygon)
    coerced_polygon = coerce_polygon_data_columns(
      polygon = polygon,
      data_column_top = data_column_top,
      data_column_bottom = data_column_bottom,
      caller = "render_polygons"
    )
    polygon = coerced_polygon$polygon
    top = subset_render_arg(
      top,
      coerced_polygon$keep,
      n_polygon_before_data_drop
    )
    bottom = subset_render_arg(
      bottom,
      coerced_polygon$keep,
      n_polygon_before_data_drop
    )
    if (!nrow(polygon)) {
      return(invisible(NULL))
    }
  }
  if (is.na(bottom)) {
    vertex_info = get_ids_with_labels(typeval = c("base"))
    vertex_info2 = get_ids_with_labels(typeval = c("surface", "surface_tris"))

    bottom1 = min(
      rgl::rgl.attrib(vertex_info$id[1], "vertices")[, 2],
      na.rm = TRUE
    )
    bottom2 = min(
      rgl::rgl.attrib(vertex_info2$id[1], "vertices")[, 2],
      na.rm = TRUE
    )
    bottom = (bottom1 + bottom2) / 2
  }
  polygon_color_values = if (!is.null(data_column_top)) {
    polygon[[data_column_top]]
  } else if (!is.null(data_column_bottom)) {
    polygon[[data_column_bottom]]
  } else {
    top
  }
  color = resolve_ggplot_height_palette_color(
    color = color,
    values = polygon_color_values,
    heightmap = heightmap,
    caller = "render_polygons"
  )
  shape_to_vertex = function(poly_list) {
    matrix(poly_list[4:12], ncol = 3, nrow = 3, byrow = TRUE)
  }
  vertex_list = list()
  if (!parallel) {
    if (inherits(polygon, "data.frame")) {
      for (i in seq_len(nrow(polygon))) {
        if (
          inherits(polygon[i, ], "SpatialPolygonsDataFrame") ||
            inherits(polygon[i, ], "SpatialPolygons") ||
            inherits(polygon[i, ], "sf")
        ) {
          holes = NULL
        }
        mesh = rayrender::extruded_polygon(
          polygon[i, ],
          top = top,
          bottom = bottom,
          data_column_top = data_column_top,
          data_column_bottom = data_column_bottom,
          scale_data = scale_data,
          holes = holes
        )$shape_info[[1]]$mesh_info[[1]]
        mesh_obj = rgl::mesh3d(
          vertices = c(t(mesh$vertices)),
          triangles = c(t(mesh$indices)) + 1
        )
        vertex_list[[i]] = mesh_obj
      }
    }
  } else {
    if (is.null(options("cores")[[1]])) {
      numbercores = parallel::detectCores()
    } else {
      numbercores = options("cores")[[1]]
    }
    cl = parallel::makeCluster(numbercores)
    doParallel::registerDoParallel(cl, cores = numbercores)
    vertex_list = tryCatch(
      {
        foreach::foreach(
          i = seq_len(nrow(polygon)),
          .packages = c("rayrender", "sf")
        ) %dopar%
          {
            if (
              inherits(polygon[i, ], "SpatialPolygonsDataFrame") ||
                inherits(polygon[i, ], "SpatialPolygons") ||
                inherits(polygon[i, ], "sf")
            ) {
              holes = NULL
            }
            mesh = rayrender::extruded_polygon(
              polygon[i, ],
              top = top,
              bottom = bottom,
              data_column_top = data_column_top,
              data_column_bottom = data_column_bottom,
              scale_data = scale_data,
              holes = holes
            )$shape_info[[1]]$mesh_info[[1]]
            mesh_obj = rgl::mesh3d(
              vertices = c(t(mesh$vertices)),
              triangles = c(t(mesh$indices)) + 1
            )
            mesh_obj
          }
      },
      finally = {
        tryCatch(
          {
            parallel::stopCluster(cl)
          },
          error = function(e) {
            print(e)
          }
        )
      }
    )
  }
  if (is.null(heightmap)) {
    vertex_info = get_ids_with_labels(typeval = c("surface", "surface_tris"))
    nrow_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[, 1]) -
      min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[, 1])
    ncol_map = max(rgl::rgl.attrib(vertex_info$id[1], "vertices")[, 3]) -
      min(rgl::rgl.attrib(vertex_info$id[1], "vertices")[, 3])
  } else {
    ncol_map = ncol(heightmap)
    nrow_map = nrow(heightmap)
  }
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "render_polygons",
    panel = panel
  )
  e = get_extent(extent)
  for (group in seq_along(vertex_list)) {
    if (!is.null(vertex_list[[group]])) {
      single_poly = vertex_list[[group]]
      single_color = color[((group - 1) %% length(color)) + 1]
      single_poly$vb[1, ] = (-single_poly$vb[1, ] - e["xmin"]) /
        (e["xmax"] - e["xmin"]) *
        nrow_map -
        nrow_map / 2
      single_poly$vb[3, ] = ncol_map /
        2 -
        (single_poly$vb[3, ] - e["ymin"]) / (e["ymax"] - e["ymin"]) * ncol_map
      single_poly$vb[2, ] = single_poly$vb[2, ] / zscale

      rgl::shade3d(
        single_poly,
        color = single_color,
        tag = "polygon3d",
        lit = lit,
        alpha = alpha
      )
    }
  }
  if (lit) {
    existing_lights = rgl::ids3d(type = "lights")
    for (i in seq_len(nrow(existing_lights))) {
      rgl::pop3d(type = "lights")
    }
    if (length(light_altitude) < length(light_direction)) {
      stop("light_altitude and light_direction must be same length")
    }
    for (i in seq_len(length(light_direction))) {
      rgl::light3d(
        theta = -light_direction[i] + 180,
        phi = light_altitude[i],
        specular = convert_color(rep(light_intensity, 3), as_hex = TRUE),
        viewpoint.rel = light_relative
      )
    }
  }
  cache_polygon_zaxis_data(
    polygon = polygon,
    top = top,
    bottom = bottom,
    data_column_top = data_column_top,
    data_column_bottom = data_column_bottom,
    scale_data = scale_data
  )
  invisible(NULL)
}
