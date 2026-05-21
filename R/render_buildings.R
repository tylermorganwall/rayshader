#'@title Render Buildings
#'
#'@description Adds 3D polygons with roofs to the current scene,
#'using latitude/longitude or coordinates in the reference system defined by the extent object.
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
#' @param material Default `"grey80"`. If a color string, this will specify the color of the sides/base of the building
#' Alternatively (for more customization), this can be a r`ayvertex::material_list()` object to specify
#' the full color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param roof_material Default `NA`, defaults to the material specified in `material`. If a color string, this will specify the color of the roof of the building.
#' Alternatively (for more customization), this can be a `rayvertex::material_list()` object to specify
#' the full color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param roof_height Default `1`. Height from the base of the building to the start of the roof.
#' @param base_height Default `0`. Height of the base of the roof.
#' @param heights_relative_to_centroid Default `FALSE`. Whether the heights should be measured in absolute
#' terms, or relative to the centroid of the polygon.
#' @param data_column_top Default `NULL`. A string indicating the column in the `sf` object to use
#' to specify the top of the extruded polygon.
#' @param data_column_bottom Default `NULL`. A string indicating the column in the `sf` object to use
#' to specify the bottom of the extruded polygon.
#' @param scale_data Default `1`. How much to scale the `top`/`bottom` value when rendering. Use
#' `zscale` to adjust the data to account for `x`/`y` grid spacing, and this argument to scale the data
#' for visualization. If used with `vertical_exaggeration`, both are applied.
#' @param holes Default `0`. If passing in a polygon directly, this specifies which index represents
#' the holes in the polygon. See the `earcut` function in the `decido` package for more information.
#' @param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#' of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'  All points are assumed to be evenly spaced.
#' @param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#' @param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#' @param alpha Default `1`. Transparency of the polygons.
#' @param lit Default `TRUE`. Whether to light the polygons.
#' @param light_altitude Default `c(45, 30)`. Degree(s) from the horizon from which to light the polygons.
#' @param light_direction Default `c(315, 225)`. Degree(s) from north from which to light the polygons.
#' @param light_intensity Default `1`. Intensity of the specular highlight on the polygons.
#' @param light_relative Default `FALSE`. Whether the light direction should be taken relative to the camera,
#' or absolute.
#' @param angle Default `45`. Angle of the roof.
#' @param relative_heights Default `TRUE`. Whether the heights specified in `roof_height` and `base_height` should
#' be measured relative to the underlying heightmap.
#' @param flat_shading Default `FALSE`. Set to `TRUE` to have nicer shading on the 3D polygons. This comes
#' with the slight penalty of increasing the memory use of the scene due to vertex duplication. This
#' will not affect software or high quality renders.
#' @param filter_to_extent Default `TRUE`. If `TRUE`, building footprint data outside the scene extent is omitted. Spatial polygon inputs are cropped to the extent. For scenes created with [plot_gg()], filtering uses the ggplot panel extent rather than the full rendered 3D ggplot extent.
#' @param ... Additional arguments to pass to `rgl::triangles3d()`.
#' @param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing polygons.
#'
#' @export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Load and visualize building footprints from Overture Maps
#' library(DBI)
#' library(duckdb)
#' library(sf)
#' library(jsonlite)
#'
#' # Define the WGS84 bounding box for the scene in St. John's, Newfoundland.
#' overture_bbox = c(-52.718925, 47.552085, -52.666397, 47.586194)
#' scene_bbox = c(
#'   xmin = overture_bbox[1],
#'   ymin = overture_bbox[2],
#'   xmax = overture_bbox[3],
#'   ymax = overture_bbox[4]
#' )
#'
#' # Query a slightly padded area so roads and buildings that cross the scene
#' # boundary are included before rayshader crops them to the rendered extent.
#' query_padding = 0.001
#'
#' query_bbox = c(
#'   xmin = scene_bbox[["xmin"]] - query_padding,
#'   ymin = scene_bbox[["ymin"]] - query_padding,
#'   xmax = scene_bbox[["xmax"]] + query_padding,
#'   ymax = scene_bbox[["ymax"]] + query_padding
#' )
#'
#' # Keep an sf polygon for DEM retrieval and for documenting the exact render area.
#' scene_area = sf::st_sf(
#'   geometry = sf::st_as_sfc(sf::st_bbox(scene_bbox, crs = sf::st_crs(4326)))
#' )
#'
#' # Discover the current Overture release and build Parquet paths for the
#' # building and transportation layers.
#' overture_release = jsonlite::fromJSON(
#'   "https://stac.overturemaps.org/catalog.json"
#' )$latest
#'
#' building_uri = sprintf(
#'   "az://overturemapswestus2.blob.core.windows.net/release/%s/theme=buildings/type=building/*.parquet",
#'   overture_release
#' )
#'
#' road_uri = sprintf(
#'   "az://overturemapswestus2.blob.core.windows.net/release/%s/theme=transportation/type=segment/*.parquet",
#'   overture_release
#' )
#'
#' # Query the remote Parquet files in memory. The Overture geometry column is
#' # returned as WKB so sf can reconstruct the features locally.
#' con = DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
#'
#' # Select buildings whose feature bounding boxes start inside the padded
#' # query area.
#' building_query = sprintf(
#'   "
#' SELECT
#'   id,
#'   height,
#'   num_floors,
#'   ST_AsWKB(geometry)::BLOB AS geometry
#' FROM read_parquet('%s', filename = false, hive_partitioning = 1)
#' WHERE
#'   bbox.xmin BETWEEN %.10f AND %.10f
#'   AND bbox.ymin BETWEEN %.10f AND %.10f
#' ",
#'   building_uri,
#'   query_bbox[["xmin"]],
#'   query_bbox[["xmax"]],
#'   query_bbox[["ymin"]],
#'   query_bbox[["ymax"]]
#' )
#'
#' building_df = DBI::dbGetQuery(con, building_query)
#'
#' building_geom = sf::st_as_sfc(
#'   structure(building_df$geometry, class = "WKB"),
#'   crs = 4326
#' )
#'
#' building_df$geometry = NULL
#'
#' building_polys = sf::st_sf(
#'   building_df,
#'   geometry = building_geom,
#'   crs = 4326
#' )
#'
#' # Select road segments that overlap the padded query area.
#' road_query = sprintf(
#'   "
#' SELECT
#'   id,
#'   names.primary AS name,
#'   class AS road_class,
#'   subclass AS road_subclass,
#'   ST_AsWKB(geometry)::BLOB AS geometry
#' FROM read_parquet('%s', filename = false, hive_partitioning = 1)
#' WHERE
#'   subtype = 'road'
#'   AND bbox.xmin < %.10f
#'   AND bbox.xmax > %.10f
#'   AND bbox.ymin < %.10f
#'   AND bbox.ymax > %.10f
#' ",
#'   road_uri,
#'   query_bbox[["xmax"]],
#'   query_bbox[["xmin"]],
#'   query_bbox[["ymax"]],
#'   query_bbox[["ymin"]]
#' )
#'
#' road_df = DBI::dbGetQuery(con, road_query)
#'
#' road_geom = sf::st_as_sfc(
#'   structure(road_df$geometry, class = "WKB"),
#'   crs = 4326
#' )
#'
#' road_df$geometry = NULL
#'
#' road_lines = sf::st_sf(
#'   road_df,
#'   geometry = road_geom,
#'   crs = 4326
#' )
#'
#' # Close the in-memory DuckDB connection after all remote data is loaded.
#' DBI::dbDisconnect(con, shutdown = TRUE)
#'
#' # Fetch elevation for the render area. rayshader caches the spatial extent
#' # from this DEM object, so overlay and render calls can omit extent/heightmap.
#' scene_dem = elevatr::get_elev_raster(scene_area, z = 11, clip = "bbox")
#'
#' # Create a shaded terrain image, then draw building footprints and roads over
#' # it before opening the 3D scene.
#' scene_dem |>
#'   sphere_shade(texture = "imhof4", vertical_exaggeration = 20) |>
#'   add_overlay(
#'     generate_polygon_overlay(
#'       building_polys,
#'       linewidth = 6,
#'       resolution_multiply = 50
#'     ),
#'     rescale_original = TRUE
#'   ) |>
#'   add_overlay(
#'     generate_line_overlay(
#'       road_lines,
#'       linewidth = 6,
#'       resolution_multiply = 50
#'     ),
#'     rescale_original = TRUE
#'   ) |>
#'   plot_3d(
#'     water = TRUE,
#'     waterdepth = 0.5,
#'     windowsize = 800,
#'     watercolor = "dodgerblue",
#'     background = "pink"
#'   )
#'
#' # Overture building heights are sometimes missing. Fill those gaps before
#' # using the height column to drive roof elevation.
#' building_polys |>
#'   dplyr::mutate(height = dplyr::if_else(is.na(height), mean(height, na.rm = TRUE), height)) ->
#' building_poly_fixed
#'
#' # Render buildings
#' render_buildings(
#'   building_poly_fixed,
#'   flat_shading = TRUE,
#'   lit = FALSE,
#'   angle = 30,
#'   material = "white",
#'   roof_material = "white",
#'   roof_height = 3,
#'   base_height = 0,
#'   data_column_top = "height",
#'   relative_heights = TRUE
#' )
#'
#' render_camera(theta = 220, phi = 45, zoom = 0.55, fov = 0)
#' render_snapshot()
#'
#' # Zoom in to show roof details and render with render_highquality()
#' render_camera(fov = 120)
#'
#' # Generate an evening sky in the winter. The camera location and direction
#' # were determined using an interactive render_highquality() session by
#' # hitting the P key with the camera in the desired position.
#' render_highquality(
#'   camera_location = c(-9.49, 4.66, 11.85),
#'   camera_lookat = c(0.65, -1.85, 2.82),
#'   focal_distance = 15.056,
#'   samples = 100,
#'   iso = 100 / 16,
#'   datetime = as.POSIXct("2025-01-01 15:00:00", tz = "America/St_Johns"),
#'   sky_args = list(hosek = FALSE)
#' )
#'
render_buildings = function(
  polygon,
  extent = NULL,
  panel = NULL,
  material = "grey",
  roof_material = NA,
  angle = 45,
  zscale = 1,
  vertical_exaggeration = 1,
  scale_data = 1,
  relative_heights = TRUE,
  heights_relative_to_centroid = FALSE,
  roof_height = 1,
  base_height = 0,
  data_column_top = NULL,
  data_column_bottom = NULL,
  heightmap = NULL,
  holes = 0,
  alpha = 1,
  lit = TRUE,
  flat_shading = FALSE,
  light_altitude = c(45, 30),
  light_direction = c(315, 225),
  light_intensity = 1,
  light_relative = FALSE,
  clear_previous = FALSE,
  crs = NULL,
  filter_to_extent = TRUE,
  ...
) {
  validate_filter_to_extent(filter_to_extent, caller = "render_buildings")
  warn_scale_data_with_vertical_exaggeration(
    scale_data_missing = missing(scale_data),
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_buildings"
  )
  dot_split = split_zaxis_dots(list(...))
  zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_buildings"
  )
  heightmap = resolve_scene_render_heightmap(
    heightmap,
    caller = "render_buildings"
  )
  top = roof_height
  bottom = base_height
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if (!(length(find.package("raybevel", quiet = TRUE)) > 0)) {
    stop("raybevel required to use render_roofs()")
  }
  if (clear_previous) {
    rgl::pop3d(tag = "obj_raymesh_building")
    if (missing(polygon)) {
      render_zaxis_from_dots(
        zaxis_args = dot_split$zaxis_args,
        extent = extent,
        panel = panel,
        zscale = zscale,
        heightmap = heightmap,
        caller = "render_buildings"
      )
      return(invisible())
    }
  }
  if (is.character(material)) {
    material = rayvertex::material_list(diffuse = material)
  }
  if (is.character(roof_material)) {
    roof_material = rayvertex::material_list(diffuse = roof_material)
  }
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "render_buildings",
    panel = panel
  )
  if (inherits(polygon, "Spatial")) {
    polygon = sf::st_as_sf(polygon)
  }
  if (inherits(polygon, "sfc")) {
    polygon = sf::st_sf(geometry = polygon)
  }
  if (inherits(polygon, "sfg")) {
    polygon = sf::st_sf(geometry = sf::st_sfc(polygon))
  }
  polygon_scene_transformed = FALSE
  if (inherits(polygon, "sf")) {
    n_polygon_before_filter = nrow(polygon)
    scene_polygon = auto_transform_scene_sf(
      sf_object = polygon,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      crs = crs,
      caller = "render_buildings"
    )
    polygon = scene_polygon$object
    if (!is.null(scene_polygon$extent)) {
      extent = scene_polygon$extent
    }
    polygon_scene_transformed = TRUE
    filtered_polygon = filter_scene_sf_to_extent(
      sf_object = polygon,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      filter_to_extent = filter_to_extent,
      caller = "render_buildings"
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
      render_zaxis_from_dots(
        zaxis_args = dot_split$zaxis_args,
        extent = extent,
        panel = panel,
        zscale = zscale,
        heightmap = heightmap,
        caller = "render_buildings"
      )
      return(invisible(NULL))
    }
  }
  e = get_extent(extent)
  if (heights_relative_to_centroid) {
    if (is.null(heightmap)) {
      stop("Must pass in heightmap argument if using relative heights")
    }
    centroid_source_crs = NULL
    polygon_for_centroids = polygon
    centroid_transform_scene = !isTRUE(polygon_scene_transformed)
    if (!isTRUE(polygon_scene_transformed)) {
      scene_target_crs = get_scene_target_crs(
        extent = extent,
        heightmap = heightmap,
        panel = panel,
        caller = "render_buildings"
      )
      if (!is.null(scene_target_crs)) {
        resolved_polygon = resolve_scene_sf_source_crs(
          sf_data = polygon,
          crs = crs,
          target_crs = scene_target_crs,
          caller = "render_buildings"
        )
        polygon_for_centroids = resolved_polygon$sf_data
        centroid_source_crs = resolved_polygon$source_crs
      }
    }
    centroids = sf::st_coordinates(sf::st_centroid(polygon_for_centroids))
    xyz = transform_into_heightmap_coords(
      e,
      heightmap,
      centroids[, 2],
      centroids[, 1],
      altitude = NULL,
      offset = 0,
      zscale = 1,
      crs = centroid_source_crs,
      panel = panel,
      transform_scene = centroid_transform_scene,
      caller = "render_buildings"
    )
    bottom = xyz[, 2] + bottom
    bottom[is.na(bottom)] = min(xyz[, 2])
  }
  if (length(top) != 1) {
    stopifnot(length(top) == nrow(polygon))
  }
  if (!is.null(data_column_top)) {
    stopifnot(data_column_top %in% colnames(polygon))
  }
  if (length(bottom) != 1) {
    stopifnot(length(bottom) == nrow(polygon))
  }
  if (!is.null(data_column_bottom)) {
    stopifnot(data_column_bottom %in% colnames(polygon))
  }

  top_values = get_polygon_data_value(
    polygon,
    data_column_name = data_column_top,
    scale_data = scale_data,
    default_value = top
  )

  bottom_values = get_polygon_data_value(
    polygon,
    data_column_name = data_column_bottom,
    scale_data = scale_data,
    default_value = bottom
  )

  cache_polygon_like_zaxis_data(
    source = "building",
    polygon = polygon,
    top = top,
    bottom = bottom,
    data_column_top = data_column_top,
    data_column_bottom = data_column_bottom,
    scale_data = scale_data
  )

  polygon = transform_polygon_into_raycoords(
    polygon,
    heightmap = heightmap,
    e = e,
    top = top_values,
    bottom = bottom_values,
    panel = panel,
    crs = crs,
    caller = "render_buildings",
    transform_scene = !isTRUE(polygon_scene_transformed)
  )
  top = polygon$top / zscale
  bottom = polygon$bottom / zscale
  skeletons = raybevel::skeletonize(polygon)
  idx_sans_missing_geometry = get_skeleton_source_indices(skeletons)
  if (!length(idx_sans_missing_geometry)) {
    idx_sans_missing_geometry = seq_len(length(top))
  }
  top = top[idx_sans_missing_geometry]
  bottom = bottom[idx_sans_missing_geometry]

  if (!heights_relative_to_centroid) {
    roof_mesh = tryCatch(
      raybevel::generate_roof(
        skeletons,
        vertical_offset = top,
        base_height = 0,
        angle = angle,
        material = material,
        roof_material = roof_material,
        base = TRUE,
        sides = TRUE
      ),
      error = function(e) {
        stop(format_raybevel_error(e, "render_buildings"), call. = FALSE)
      }
    )
  } else {
    roof_mesh = tryCatch(
      raybevel::generate_roof(
        skeletons,
        vertical_offset = top,
        base_height = bottom,
        angle = angle,
        material = material,
        roof_material = roof_material,
        base = TRUE,
        sides = TRUE
      ),
      error = function(e) {
        stop(format_raybevel_error(e, "render_buildings"), call. = FALSE)
      }
    )
  }
  if (relative_heights && !heights_relative_to_centroid) {
    if (is.null(heightmap)) {
      stop("Must pass in heightmap argument if using relative heights")
    }
    offset_building_heightmap = function(verts, bottom_value) {
      tmpval = verts
      tmpval[, 1] = tmpval[, 1] + nrow(heightmap) / 2 + 0.5
      tmpval[, 3] = tmpval[, 3] + ncol(heightmap) / 2 + 0.5
      tmpval[tmpval[, 1] < 1, 1] = 1
      tmpval[tmpval[, 1] > nrow(heightmap), 1] = nrow(heightmap)
      tmpval[tmpval[, 3] < 1, 3] = 1
      tmpval[tmpval[, 3] > ncol(heightmap), 3] = ncol(heightmap)
      new_heights = rayimage::interpolate_array(
        t(heightmap),
        tmpval[, 1],
        tmpval[, 3]
      ) /
        zscale
      base_verts = tmpval[, 2] == 0
      verts[, 2] = verts[, 2] + new_heights
      verts[base_verts, 2] = (new_heights[base_verts] + bottom_value)
      return(verts)
    }
    for (i in seq_len(length(roof_mesh$vertices))) {
      roof_mesh$vertices[[i]] = offset_building_heightmap(
        roof_mesh$vertices[[i]],
        bottom[i]
      )
    }
  }

  render_raymesh(
    roof_mesh,
    extent = extent,
    panel = panel,
    xyz = matrix(c(0, 0, 0), ncol = 3),
    zscale = zscale,
    vertical_exaggeration = 1,
    heightmap = heightmap,
    flat_shading = flat_shading,
    change_material = FALSE,
    lit = lit,
    light_altitude = light_altitude,
    light_direction = light_direction,
    light_intensity = light_intensity,
    light_relative = light_relative,
    rgl_tag = "_building",
    crs = crs,
    ...
  )
}
