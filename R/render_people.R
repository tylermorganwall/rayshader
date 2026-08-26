#' @title Render People
#'
#' @description Adds one or more 3D people to an existing rayshader scene. The
#' bundled person models use meter-based coordinates and are scaled using the
#' scene's effective `zscale`, including `vertical_exaggeration`, so that their
#' native two-meter stature remains correctly sized relative to the terrain.
#' Choose from several bundled poses with `pose` and select the model variant
#' with `sex`. The bundled Wavefront OBJ models use `.txt` file extensions
#' for R package compatibility.
#'
#' When `line` is supplied, or `location` contains line geometry, people are
#' placed along each line component at regular intervals. Their local +Z axes
#' follow the direction of the line. Use `line_pattern` to repeat male and female
#' variants along the sampled positions.
#'
#' Cache fallback messages are disabled by default. Set
#' `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata
#' is reused.
#'
#' @param location Default `NULL`. Spatial input used to place people in the
#' scene. Accepts `sf`, `sfc`, `sfg`, or `sp` POINT, MULTIPOINT, LINESTRING, or
#' MULTILINESTRING geometries. Point inputs place one model at every point. Line
#' inputs generate placements according to `line_spacing`. If the input carries a
#' CRS, it is transformed automatically into the active scene CRS. If it has no
#' CRS, supply `crs`.
#' @param pose Default `"standing"`. Bundled pose to render. Supply one value
#' for every person or a single value to use for all people. Options are
#' `"clapping"`, `"ironman"`, `"slipping"`, `"stack"`, `"standing"`, `"stop"`,
#' `"stop_one_hand"`, `"stretch"`, `"walking"`, `"rocky"`, and `"yelling"`.
#' @param sex Default `"male"`. Person model variant. Options are `"male"`
#' and `"female"`.
#' @param line Default `NULL`. Spatial line input along which to place people.
#' Accepts `sf`, `sfc`, `sfg`, or `sp` LINESTRING or MULTILINESTRING geometry.
#' Line geometry may alternatively be supplied through `location`.
#' @param line_spacing Default `2`. Distance in meters between people placed along a
#' spatial line. For CRS-less lines, this is interpreted in the line's coordinate
#' units. Sampling starts at the beginning of every line component.
#' @param line_terrain_spacing Default `TRUE`. If `TRUE`, line spacing is measured
#' along the rendered terrain surface using the heightmap and effective `zscale`.
#' This keeps neighboring models together on slopes. If terrain metadata is not
#' available, spacing falls back to plan-view distance. Set to `FALSE` to always
#' use plan-view distance.
#' @param line_pattern Default `NULL`. Repeating male/female line_pattern used for line
#' placement. Supply a single string containing `M` and `F`, such as `"MF"` or
#' `"FMF"`. Overrides `sex` for line placements.
#' @param line_align_terrain Default `TRUE`. If `TRUE`, line placements are tilted
#' so each person's local +Y up vector matches the terrain normal. The local +Z
#' forward vector is lifted along the terrain without changing its plan-view
#' alignment with the line direction.
#' @param color Default `"white"`. Color of the person model. For line
#' placement, supply one color per generated person or one color per entry in
#' `line_pattern`; pattern colors are repeated with the pattern. Use `"height"` to
#' color placed models by the cached [plot_gg()] height aesthetic palette.
#' @param angle Default `c(0, 0, 0)`. Rotation around the x, y, and z axes. A
#' matrix or list can specify a separate rotation for each person. For line
#' placement, these rotations are added to the automatic line orientation.
#' @param lit Default `TRUE`. Whether to light the model polygons.
#' @param load_normals Default `TRUE`. Whether to load normals from the model.
#' @param clear_previous Default `FALSE`. If `TRUE`, remove previously rendered
#' people before rendering the new ones. A clear-only call returns without
#' rendering a replacement.
#' @param x Default `NULL`. Vector of x coordinates (or coordinates in the same
#' coordinate reference system as `extent`).
#' @param y Default `NULL`. Vector of y coordinates (or coordinates in the same
#' coordinate reference system as `extent`).
#' @param altitude Default `NULL`. Elevation of each person, in units of the
#' elevation matrix. If left `NULL`, each person is placed on the surface plus
#' `offset`. A single value places every person at that altitude. When one
#' horizontal location is supplied with multiple altitude values, that location
#' is repeated to place one person at each altitude.
#' @param xyz Default `NULL`. A three-column numeric matrix in which each row
#' specifies the raw rgl x/y/z coordinates of a person. Overrides coordinate
#' placement and `altitude`.
#' @param offset Default `0`. Offset from the surface when `altitude = NULL`.
#' @param lat Default `NULL`. Alias for `y` for geographic workflows.
#' @param long Default `NULL`. Alias for `x` for geographic workflows.
#' @param crs Default `NULL`. CRS of numeric x/y coordinates, or a CRS to assign
#' to CRS-less spatial data before transforming it into the active scene CRS.
#' @param filter_to_extent Default `TRUE`. If `TRUE`, placements outside the
#' scene extent are omitted.
#' @param extent Default `NULL`. Either an object representing the spatial
#' extent of the scene (from the `raster`, `terra`, `sf`, or `sp` packages), a
#' length-4 numeric vector specifying `c("xmin", "xmax", "ymin", "ymax")`, or a
#' spatial object that can be converted to an extent. If omitted, rayshader uses
#' extent metadata cached by [plot_3d()] or [plot_gg()].
#' @param panel Default `NULL`. Facet panel identifier for scenes created with
#' [plot_gg()]. Required to disambiguate faceted ggplot scenes when
#' panel-specific cached metadata is needed. Ignored for non-ggplot scenes.
#' @param zscale Default `1`. Ratio between horizontal spacing and the elevation
#' units in the original heightmap. If omitted, rayshader uses the cached scene
#' value when available.
#' @param vertical_exaggeration Default `1`. Multiplier applied to the effective
#' visual relief. If omitted, rayshader uses the cached scene value from
#' [plot_3d()] or [plot_gg()] when available; pass explicitly to override it for
#' this call.
#' @param heightmap Default `NULL`. Height matrix for the current scene. If
#' omitted, rayshader uses the cached scene heightmap from [plot_3d()] or
#' [plot_gg()]. Terrain elevations and existing `NA` cells are read directly
#' from this matrix.
#' @param ... Additional arguments passed to [render_obj()] and then to
#' `rgl::triangles3d()`.
#'
#' @export
#' @examples
#' # Add the OSM walking trails and place two-meter people along the summit
#' # circuit to show the scale of Maungawhau.
#' if (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")) {
#'   volcano_dem = volcano_spatial()
#'
#'   volcano_dem |>
#'     sphere_shade(texture = "desert") |>
#'     plot_3d(
#'       heightmap = volcano_dem,
#'       solid = FALSE,
#'       shadow = FALSE,
#'       windowsize = c(800, 600)
#'     )
#'
#'   render_trails(
#'     volcano_trails,
#'     color = "grey50",
#'     width = 2,
#'     width_units = "meters"
#'   )
#'
#'   summit_walk = volcano_trails[
#'     !is.na(volcano_trails$name) &
#'       volcano_trails$name == "Puhi Huia Road",
#'   ]
#'   render_people(
#'     line = summit_walk,
#'     pose = "walking",
#'     line_spacing = 30,
#'     line_pattern = "MF",
#'     color = c("#F6E8C3", "white"),
#'     clear_previous = TRUE
#'   )
#'   render_camera(theta = -40, phi = 35, zoom = 0.75)
#' }
#'
#' # Build a stepped pose gallery.
#' flat_heightmap = matrix(1:16 / 2, nrow = 4, byrow = TRUE) |>
#'   rayimage::render_reorient(flipy = TRUE, transpose = TRUE) |>
#'   rayimage::render_resized(mag = 50, method = "box")
#' scene_zscale = 1 / 25
#'
#' flat_heightmap |>
#'   constant_shade("#800") |>
#'   plot_3d(
#'     heightmap = flat_heightmap,
#'     zscale = scene_zscale,
#'     vertical_exaggeration = 1,
#'     shadowdepth = 0,
#'     soliddepth = 0,
#'     solidcolor = "#800",
#'     windowsize = c(800, 800)
#'   )
#' render_camera(
#'   theta = -28,
#'   phi = 16,
#'   zoom = 0.70,
#'   fov = 0,
#'   shift_vertical = -10
#' )
#'
#' # Place every pose on the podium. The ironman model is raised to
#' # clear its step, and the tenth person will support a second stack model.
#' # These x/y values use the raw matrix's cached 1-based extent.
#' gallery_positions = expand.grid(
#'   x = seq(25, 175, by = 50),
#'   y = seq(25, 125, by = 50)
#' )
#' gallery_poses = c(
#'   "standing",
#'   "walking",
#'   "stop",
#'   "stop_one_hand",
#'   "clapping",
#'   "yelling",
#'   "stretch",
#'   "slipping",
#'   "stack",
#'   "stack",
#'   "ironman",
#'   "rocky"
#' )
#' gallery_positions$offset = 0
#' gallery_positions$offset[gallery_poses == "ironman"] = 1
#' stack_index = 10L
#' stack_position = gallery_positions[stack_index, , drop = FALSE]
#' person_height = 2
#' label_clearance = 0.1
#'
#' # Render and label male pose gallery
#' render_people(
#'   pose = gallery_poses,
#'   sex = "male",
#'   clear_previous = TRUE,
#'   x = gallery_positions$x,
#'   y = gallery_positions$y,
#'   offset = gallery_positions$offset
#' )
#' render_people(
#'   pose = "stack",
#'   sex = "male",
#'   x = stack_position$x,
#'   y = stack_position$y,
#'   offset = stack_position$offset + person_height
#' )
#'
#' # Label the standalone models first, then label the completed tower above
#' # its upper person without relying on negative indexing.
#' regular_label_indices = setdiff(seq_along(gallery_poses), stack_index)
#' render_label(
#'   text = gallery_poses[regular_label_indices],
#'   font = "Helvetica",
#'   family = "serif",
#'   fonttype = "standard",
#'   textcolor = "white",
#'   line = FALSE,
#'   clear_previous = TRUE,
#'   x = gallery_positions$x[regular_label_indices],
#'   y = gallery_positions$y[regular_label_indices],
#'   altitude = gallery_positions$offset[regular_label_indices] +
#'     person_height + label_clearance,
#'   relativez = TRUE
#' )
#' render_label(
#'   text = "stack",
#'   font = "Helvetica",
#'   family = "serif",
#'   fonttype = "standard",
#'   textcolor = "white",
#'   line = FALSE,
#'   x = stack_position$x,
#'   y = stack_position$y,
#'   altitude = stack_position$offset + 2 * person_height + label_clearance,
#'   relativez = TRUE
#' )
#'
#' # Produce an 800-by-800 high-quality image of the male pose variants.
#' render_highquality(
#'   width = 800,
#'   height = 800,
#'   sky_sun_elevation = 10,
#'   sky_sun_azimuth = 0,
#'   rotate_env = 160,
#'   iso = 5
#' )
#'
#' # Female models
#' render_people(
#'   pose = gallery_poses,
#'   sex = "female",
#'   clear_previous = TRUE,
#'   x = gallery_positions$x,
#'   y = gallery_positions$y,
#'   offset = gallery_positions$offset
#' )
#' render_people(
#'   pose = "stack",
#'   sex = "female",
#'   x = stack_position$x,
#'   y = stack_position$y,
#'   offset = stack_position$offset + person_height
#' )
#' render_highquality(
#'   width = 800,
#'   height = 800,
#'   sky_sun_elevation = 10,
#'   sky_sun_azimuth = 0,
#'   rotate_env = 160,
#'   iso = 5
#' )
#'
#'@examplesIf length(find.package("sf", quiet = TRUE)) && length(find.package("elevatr", quiet = TRUE)) && length(find.package("raster", quiet = TRUE)) && (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#'library(sf)
#'#Set location of washington monument
#'washington_monument_location = st_point(c(-77.035249, 38.889462))
#'wm_point = washington_monument_location |>
#'  st_point() |>
#'  st_sfc(crs = 4326) |>
#'  st_transform(st_crs(washington_monument_multipolygonz))
#'
#'washington_monument_people = st_point(c(-77.035249, 38.889462)) |>
#'  st_sfc(crs = 4326)
#'elevation_data = elevatr::get_elev_raster(locations = wm_point, z = 14)
#'
#'scene_bbox = st_bbox(st_buffer(wm_point,300))
#'cropped_data = raster::crop(elevation_data, scene_bbox)
#'
#'#Plot a 3D map of the national mall
#'cropped_data |>
#'  height_shade() |>
#'  plot_3d(soliddepth=-10, windowsize = 800)
#'render_snapshot()
#'#Zoom in on the monument
#'render_camera(theta=150,  phi=35, zoom= 0.55, fov=70)
#'#Render the national monument
#'rgl::par3d(ignoreExtent = TRUE)
#'render_multipolygonz(
#'  washington_monument_multipolygonz,
#'  color = "grey80"
#')
#'render_camera(location = washington_monument_people)
#'render_snapshot()
#'#Create a meandering tourist queue that starts just beyond the camera-facing
#'#edge of the monument and extends toward the foreground.
#'monument_bbox = st_bbox(washington_monument_multipolygonz)
#'monument_center = c(
#'  mean(monument_bbox[c("xmin", "xmax")]),
#'  mean(monument_bbox[c("ymin", "ymax")])
#')
#'camera_direction = c(sinpi(150 / 180), -cospi(150 / 180))
#'cross_direction = c(-camera_direction[2], camera_direction[1])
#'monument_half_size = c(
#'  diff(monument_bbox[c("xmin", "xmax")]) / 2,
#'  diff(monument_bbox[c("ymin", "ymax")]) / 2
#')
#'queue_start_distance =
#'  min(monument_half_size / abs(camera_direction)) + 3
#'queue_progress = seq(0, 180, length.out = 120)
#'queue_meander =
#'  10 * sin(queue_progress / 18) + 3 * sin(queue_progress / 7)
#'tourist_queue_xy = cbind(
#'  x = monument_center[1] +
#'    (queue_start_distance + queue_progress) * camera_direction[1] +
#'    queue_meander * cross_direction[1],
#'  y = monument_center[2] +
#'    (queue_start_distance + queue_progress) * camera_direction[2] +
#'    queue_meander * cross_direction[2]
#')
#'tourist_queue = st_sfc(
#'  st_linestring(tourist_queue_xy),
#'  crs = st_crs(washington_monument_multipolygonz)
#')
#'
#'render_people(
#'  line = tourist_queue,
#'  pose = "stretch",
#'  line_spacing = 2,
#'  line_pattern = "MF",
#'  color = c("#355C7D", "#C06C84"),
#'  clear_previous = TRUE
#')
#'render_snapshot()
#'
#'#Build a human pyramid as tall as the Washington Monument. The stack model
#'#is about 0.75 meters wide, so adjacent people at each level just touch.
#'monument_elevation = range(
#'  st_coordinates(washington_monument_multipolygonz)[, "Z"]
#')
#'person_height = 2
#'horizontal_spacing = 1
#'level_count = ceiling(diff(monument_elevation) / person_height)
#'people_per_level = rev(seq_len(level_count))
#'pyramid_level = rep(
#'  seq_len(level_count) - 1,
#'  times = people_per_level
#')
#'pyramid_column = unlist(lapply(
#'  people_per_level,
#'  function(n) seq_len(n) - (n + 1) / 2
#'))
#'
#'#Run the rows north-south and keep the pyramid just west of the monument.
#'pyramid_center = washington_monument_people |>
#'  st_transform(st_crs(washington_monument_multipolygonz))
#'pyramid_center_xy = st_coordinates(pyramid_center)[1, 1:2]
#'pyramid_center_xy[1] =
#'  st_bbox(washington_monument_multipolygonz)[["xmin"]] +
#'  horizontal_spacing
#'
#'pyramid_people = st_as_sf(
#'  data.frame(
#'    x = rep(unname(pyramid_center_xy[1]), length(pyramid_level)),
#'    y = unname(pyramid_center_xy[2]) +
#'      pyramid_column * horizontal_spacing,
#'    altitude = monument_elevation[1] + pyramid_level * person_height
#'  ),
#'  coords = c("x", "y"),
#'  crs = st_crs(washington_monument_multipolygonz)
#')
#'
#'render_people(
#'  location = pyramid_people,
#'  pose = "stack",
#'  altitude = pyramid_people$altitude,
#'  angle = c(0, 90, 0),
#'  color = "#030",
#'  clear_previous = TRUE
#')
#'#This works with `render_highquality()`
#'render_highquality(
#'  min_variance = 0,
#'  samples = 16,
#'  defer = TRUE,
#'  datetime = as.POSIXct("2025-12-21 08:00:00", tz = "EST"),
#'  sky_args = list(hosek = FALSE, iso = 20)
#')
#' # Arrange alternating models around a closed line. The stretch pose extends
#' # along local Z, so automatic line orientation creates a hands-around-the-world
#' # effect.
#' flat_heightmap = rayimage::generate_2d_gaussian(dim = c(31,31)) * 1000
#' flat_extent = c(xmin = 0, xmax = 31, ymin = 0, ymax = 31)
#' flat_heightmap |>
#'   height_shade() |>
#'   plot_3d(
#'     flat_heightmap,
#'     zscale = 1,
#'     shadow = FALSE,
#'     extent = flat_extent
#'   )
#'
#' theta = seq(0, 2 * pi, length.out = 200)
#' people_line = sf::st_sfc(
#' sf::st_linestring(cbind(
#'   15.5 + 12.5 * cos(theta),
#'   15.5 + 12.5 * sin(theta)
#' )),
#' sf::st_linestring(cbind(
#'   15.5 + 10.5 * cos(theta),
#'   15.5 + 10.5 * sin(theta)
#' )),
#' sf::st_linestring(cbind(
#'   15.5 + 8.5 * cos(theta),
#'   15.5 + 8.5 * sin(theta)
#' )),
#' sf::st_linestring(cbind(
#'   15.5 + 6.5 * cos(theta),
#'   15.5 + 6.5 * sin(theta)
#' )),
#' sf::st_linestring(cbind(
#'   15.5 + 4.5 * cos(theta),
#'   15.5 + 4.5 * sin(theta)
#' )),
#' sf::st_linestring(cbind(
#'   15.5 + 2.5 * cos(theta),
#'   15.5 + 2.5 * sin(theta)
#' )))
#' render_people(
#'   line = people_line,
#'   pose = "stretch",
#'   line_pattern = "MF",
#'   line_spacing = 2,
#'   color = c("dodgerblue", "tomato"), clear_previous=T
#' )
#' render_people(
#'   x = c(13.5, 15.5, 17.5),
#'   y = rep(15.5, 3),
#'   pose = c("standing", "rocky", "walking"),
#'   color = "white"
#' )
render_people = function(
  location = NULL,
  pose = "standing",
  sex = "male",
  line = NULL,
  line_spacing = 2,
  line_terrain_spacing = TRUE,
  line_pattern = NULL,
  line_align_terrain = TRUE,
  color = "white",
  angle = c(0, 0, 0),
  lit = TRUE,
  load_normals = TRUE,
  clear_previous = FALSE,
  x = NULL,
  y = NULL,
  altitude = NULL,
  xyz = NULL,
  offset = 0,
  lat = NULL,
  long = NULL,
  crs = NULL,
  filter_to_extent = TRUE,
  extent = NULL,
  panel = NULL,
  zscale = 1,
  vertical_exaggeration = 1,
  heightmap = NULL,
  ...
) {
  if (
    is_render_clear_only_call(
      clear_previous,
      match.call(),
      function() rgl::pop3d(tag = "objperson")
    )
  ) {
    return(invisible(NULL))
  }
  if (
    !is.logical(line_align_terrain) ||
      length(line_align_terrain) != 1 ||
      is.na(line_align_terrain)
  ) {
    stop("`line_align_terrain` must be a single logical value.", call. = FALSE)
  }
  if (
    !is.logical(line_terrain_spacing) ||
      length(line_terrain_spacing) != 1 ||
      is.na(line_terrain_spacing)
  ) {
    stop(
      "`line_terrain_spacing` must be a single logical value.",
      call. = FALSE
    )
  }
  pose = resolve_person_pose(pose)
  sex = resolve_person_sex(sex)
  line_input = resolve_person_line_input(line = line, location = location)
  line_mode = !is.null(line_input)
  if (line_mode) {
    conflicting_inputs = names(Filter(
      Negate(is.null),
      list(x = x, y = y, lat = lat, long = long, xyz = xyz)
    ))
    if (length(conflicting_inputs)) {
      stop(
        paste0(
          "`line` cannot be combined with `",
          paste(conflicting_inputs, collapse = "`, `"),
          "`."
        ),
        call. = FALSE
      )
    }
    location = NULL
  } else if (!is.null(line_pattern)) {
    stop("`line_pattern` requires line geometry.", call. = FALSE)
  }
  effective_zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_people"
  )
  heightmap = resolve_scene_render_heightmap(
    heightmap,
    caller = "render_people"
  )
  person_scale = rep(1 / effective_zscale, 3)
  person_args = list(...)
  render_people_obj = function(...) {
    do.call(render_obj, c(list(...), person_args))
  }
  if (line_mode) {
    line_samples = sample_person_line(
      line = line_input,
      line_spacing = line_spacing,
      line_terrain_spacing = line_terrain_spacing,
      extent = extent,
      heightmap = heightmap,
      zscale = effective_zscale,
      panel = panel,
      crs = crs,
      caller = "render_people"
    )
    if (!is.null(line_samples$extent)) {
      extent = line_samples$extent
    }
    if (!is.null(line_samples$panel)) {
      panel = line_samples$panel
    }
    person_sexes = resolve_person_pattern(
      line_pattern = line_pattern,
      n = length(line_samples$x),
      sex = sex
    )
    person_poses = expand_person_poses(pose, length(person_sexes))
    person_colors = resolve_person_pattern_colors(
      color = color,
      line_pattern = line_pattern,
      n = length(person_sexes)
    )
    automatic_angles = if (isTRUE(line_align_terrain)) {
      person_terrain_line_angles(
        x = line_samples$x,
        y = line_samples$y,
        line_angle = line_samples$angle,
        extent = extent,
        heightmap = heightmap,
        zscale = effective_zscale,
        panel = panel
      )
    } else {
      NULL
    }
    person_angles = resolve_person_line_angles(
      angle = angle,
      line_angle = line_samples$angle,
      automatic_angle = automatic_angles
    )
    person_groups = paste(person_poses, person_sexes, sep = "\r")
    for (person_group in unique(person_groups)) {
      group_index = which(person_groups == person_group)
      render_people_obj(
        filename = person_obj(
          person_poses[group_index[1]],
          person_sexes[group_index[1]]
        ),
        extent = extent,
        panel = panel,
        x = line_samples$x[group_index],
        y = line_samples$y[group_index],
        altitude = subset_render_arg(
          altitude,
          seq_along(person_sexes) %in% group_index,
          length(person_sexes)
        ),
        zscale = effective_zscale,
        vertical_exaggeration = 1,
        heightmap = heightmap,
        load_material = FALSE,
        load_normals = load_normals,
        color = subset_render_color_arg(
          person_colors,
          seq_along(person_sexes) %in% group_index,
          length(person_sexes)
        ),
        offset = subset_render_arg(
          offset,
          seq_along(person_sexes) %in% group_index,
          length(person_sexes)
        ),
        obj_zscale = FALSE,
        swap_yz = FALSE,
        angle = person_angles[group_index, , drop = FALSE],
        scale = person_scale,
        clear_previous = FALSE,
        lit = lit,
        rgl_tag = "person",
        crs = NULL,
        filter_to_extent = filter_to_extent,
        transform_scene = FALSE
      )
    }
    return(invisible(NULL))
  }
  if (length(pose) > 1L) {
    point_input = resolve_render_location_input(
      location = location,
      x = x,
      y = y,
      long = long,
      lat = lat,
      missing_x = missing(x),
      missing_y = missing(y),
      missing_long = missing(long),
      missing_lat = missing(lat),
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      crs = crs,
      caller = "render_people"
    )
    x = point_input$x
    y = point_input$y
    if (!is.null(point_input$extent)) {
      extent = point_input$extent
    }
    if (!is.null(point_input$panel)) {
      panel = point_input$panel
    }
    location_supplied = isTRUE(point_input$location_supplied)
    input_crs = if (is.null(crs)) point_input$source_crs else crs
    if (location_supplied) {
      input_crs = NULL
    }
    if (
      is.null(xyz) &&
        !is.null(altitude) &&
        length(altitude) > 1L &&
        length(x) == 1L &&
        length(y) == 1L
    ) {
      x = rep(x, length(altitude))
      y = rep(y, length(altitude))
    }
    if (!is.null(xyz)) {
      if (!is.matrix(xyz) || !is.numeric(xyz) || ncol(xyz) != 3L) {
        stop(
          "`xyz` must be a numeric matrix with three columns.",
          call. = FALSE
        )
      }
      n_people = nrow(xyz)
    } else {
      if (is.null(x) || is.null(y)) {
        stop(
          "Vectorized `pose` requires point locations or `xyz` coordinates.",
          call. = FALSE
        )
      }
      if (length(x) != length(y)) {
        stop("`x` and `y` must have the same length.", call. = FALSE)
      }
      n_people = length(x)
    }
    person_poses = expand_person_poses(pose, n_people)
    person_groups = paste(person_poses, sex, sep = "\r")
    for (person_group in unique(person_groups)) {
      group_index = which(person_groups == person_group)
      render_people_obj(
        filename = person_obj(person_poses[group_index[1]], sex),
        extent = extent,
        panel = panel,
        x = subset_render_arg_by_index(x, group_index, n_people),
        y = subset_render_arg_by_index(y, group_index, n_people),
        altitude = subset_render_arg_by_index(
          altitude,
          group_index,
          n_people
        ),
        xyz = subset_render_arg_by_index(xyz, group_index, n_people),
        zscale = effective_zscale,
        vertical_exaggeration = 1,
        heightmap = heightmap,
        load_material = FALSE,
        load_normals = load_normals,
        color = subset_render_color_arg(
          color,
          seq_len(n_people) %in% group_index,
          n_people
        ),
        offset = subset_render_arg_by_index(offset, group_index, n_people),
        obj_zscale = FALSE,
        swap_yz = FALSE,
        angle = subset_render_row_arg(
          angle,
          seq_len(n_people) %in% group_index,
          n_people
        ),
        scale = person_scale,
        clear_previous = FALSE,
        lit = lit,
        rgl_tag = "person",
        crs = input_crs,
        filter_to_extent = filter_to_extent,
        transform_scene = !location_supplied
      )
    }
    return(invisible(NULL))
  }
  render_people_obj(
    filename = person_obj(pose, sex),
    extent = extent,
    panel = panel,
    x = x,
    y = y,
    altitude = altitude,
    xyz = xyz,
    zscale = effective_zscale,
    vertical_exaggeration = 1,
    heightmap = heightmap,
    load_material = FALSE,
    load_normals = load_normals,
    color = color,
    offset = offset,
    obj_zscale = FALSE,
    swap_yz = FALSE,
    angle = angle,
    scale = person_scale,
    clear_previous = FALSE,
    lit = lit,
    rgl_tag = "person",
    lat = lat,
    long = long,
    location = location,
    crs = crs,
    filter_to_extent = filter_to_extent
  )
}

#' Resolve a Bundled Person OBJ
#'
#' @param pose Default `"standing"`. Name of a bundled person pose.
#' @param sex Default `"male"`. Person model variant.
#'
#' @return Path to the bundled OBJ file, stored with a `.txt` extension for R
#' package compatibility.
#' @keywords internal
person_obj = function(pose = "standing", sex = "male") {
  pose = resolve_person_pose(pose)
  if (length(pose) != 1L) {
    stop("`pose` must be a single pose name.", call. = FALSE)
  }
  sex = resolve_person_sex(sex)
  file_sex = if (sex == "male") "man" else "woman"
  path = system.file(
    "extdata",
    "raypeople",
    paste0("person_", file_sex, "_", pose, ".txt"),
    package = "rayshader"
  )
  if (!nzchar(path)) {
    stop(
      sprintf(
        "The bundled %s OBJ for pose %s could not be found.",
        sex,
        shQuote(pose)
      ),
      call. = FALSE
    )
  }
  path
}

#' Resolve a Person Pose
#'
#' @param pose Names of bundled person poses.
#'
#' @return Canonical pose names.
#' @keywords internal
resolve_person_pose = function(pose) {
  poses = c(
    "clapping",
    "ironman",
    "slipping",
    "stack",
    "standing",
    "stop",
    "stop_one_hand",
    "stretch",
    "walking",
    "rocky",
    "yelling"
  )
  if (
    !is.character(pose) ||
      !length(pose) ||
      anyNA(pose) ||
      any(!nzchar(pose))
  ) {
    stop("`pose` must contain non-empty character values.", call. = FALSE)
  }
  if (any(!pose %in% poses)) {
    stop(
      paste0(
        "`pose` should be one of ",
        paste(shQuote(poses), collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  pose
}

#' Expand Person Poses
#'
#' @param pose Validated person pose names.
#' @param n Number of people being rendered.
#'
#' @return One pose name per person.
#' @keywords internal
expand_person_poses = function(pose, n) {
  if (length(pose) == 1L) {
    return(rep(pose, n))
  }
  if (length(pose) != n) {
    stop(
      "`pose` must contain one value or one value per person.",
      call. = FALSE
    )
  }
  pose
}

#' Resolve a Person Sex
#'
#' @param sex Default `"male"`. Person model variant.
#'
#' @return Canonical sex name.
#' @keywords internal
resolve_person_sex = function(sex = "male") {
  if (
    !is.character(sex) ||
      length(sex) != 1 ||
      is.na(sex) ||
      !nzchar(sex)
  ) {
    stop(
      "`sex` must be either \"male\" or \"female\".",
      call. = FALSE
    )
  }
  sex = tolower(sex)
  sexes = c("male", "female")
  if (!(sex %in% sexes)) {
    stop(
      paste0(
        "`sex` should be one of ",
        paste(shQuote(sexes), collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  sex
}

#' Detect Person Line Input
#'
#' @param location Spatial input supplied to `render_people()`.
#'
#' @return A single logical value.
#' @keywords internal
is_person_line_input = function(location) {
  if (is.null(location)) {
    return(FALSE)
  }
  if (
    inherits(
      location,
      c(
        "SpatialLines",
        "SpatialLinesDataFrame",
        "LINESTRING",
        "MULTILINESTRING"
      )
    )
  ) {
    return(TRUE)
  }
  if (!inherits(location, c("sf", "sfc"))) {
    return(FALSE)
  }
  geometry_types = tryCatch(
    as.character(sf::st_geometry_type(location, by_geometry = TRUE)),
    error = function(e) character()
  )
  any(
    geometry_types %in%
      c(
        "LINESTRING",
        "MULTILINESTRING",
        "GEOMETRYCOLLECTION"
      )
  )
}

#' Resolve Person Line Input
#'
#' @param line Default `NULL`. Explicit spatial line input.
#' @param location Default `NULL`. Spatial location input.
#'
#' @return Spatial line input or `NULL`.
#' @keywords internal
resolve_person_line_input = function(line = NULL, location = NULL) {
  if (!is.null(line) && !is.null(location)) {
    stop("Use only one of `line` or `location`.", call. = FALSE)
  }
  if (!is.null(line)) {
    return(line)
  }
  if (is_person_line_input(location)) {
    return(location)
  }
  NULL
}

#' Sample Person Placements Along Lines
#'
#' @param line Spatial line input.
#' @param line_spacing Default `2`. Distance between samples in meters.
#' @param line_terrain_spacing Default `TRUE`. Whether to measure line_spacing along the
#' rendered terrain surface.
#' @param extent Default `NULL`. Scene extent.
#' @param heightmap Default `NULL`. Scene heightmap.
#' @param zscale Default `1`. Effective scene zscale.
#' @param panel Default `NULL`. Facet panel identifier.
#' @param crs Default `NULL`. CRS assigned to CRS-less line input.
#' @param caller Default `NULL`. Calling function name.
#'
#' @return A list containing scene coordinates, line angles, and scene metadata.
#' @keywords internal
sample_person_line = function(
  line,
  line_spacing = 2,
  line_terrain_spacing = TRUE,
  extent = NULL,
  heightmap = NULL,
  zscale = 1,
  panel = NULL,
  crs = NULL,
  caller = NULL
) {
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop("`sf` is required for person line placement.", call. = FALSE)
  }
  if (
    !is.numeric(line_spacing) ||
      length(line_spacing) != 1 ||
      !is.finite(line_spacing) ||
      line_spacing <= 0
  ) {
    stop("`line_spacing` must be a single positive number.", call. = FALSE)
  }
  if (
    !is.logical(line_terrain_spacing) ||
      length(line_terrain_spacing) != 1 ||
      is.na(line_terrain_spacing)
  ) {
    stop(
      "`line_terrain_spacing` must be a single logical value.",
      call. = FALSE
    )
  }
  coerced_line = coerce_scene_sf_input(line)
  line_sf = coerced_line$sf_data
  target_crs = get_scene_target_crs(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    caller = caller
  )
  if (!is.null(target_crs)) {
    transformed_line = transform_scene_sf_to_target_crs(
      sf_object = line_sf,
      target_crs = target_crs,
      crs = crs,
      caller = caller
    )
    line_sf = transformed_line$object
  } else {
    line_sf = resolve_scene_sf_source_crs(
      sf_data = line_sf,
      crs = crs,
      caller = caller
    )$sf_data
  }
  line_sf = coerce_render_path_line_geometry(line_sf)
  if (is_empty_scene_sf(line_sf)) {
    stop(
      "`line` must contain at least one non-empty LINESTRING geometry.",
      call. = FALSE
    )
  }
  line_crs = suppressWarnings(sf::st_crs(line_sf))
  terrain_extent = NULL
  if (isTRUE(line_terrain_spacing) && is.matrix(heightmap)) {
    terrain_extent = resolve_scene_render_extent(
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      caller = caller,
      error_if_missing = FALSE
    )
  }
  use_line_terrain_spacing = !is.null(terrain_extent) &&
    nrow(heightmap) >= 2 &&
    ncol(heightmap) >= 2
  if (use_line_terrain_spacing) {
    sampling_line_sf = line_sf
    sampling_crs = line_crs
    raw_samples = sample_person_terrain_line_geometry(
      line_sf = sampling_line_sf,
      line_spacing = line_spacing,
      extent = terrain_extent,
      heightmap = heightmap,
      zscale = zscale
    )
  } else {
    sampling_line_sf = line_sf
    sampling_crs = line_crs
    if (!is.na(line_crs) && isTRUE(sf::st_is_longlat(line_crs))) {
      sampling_crs = person_line_local_metric_crs(line_sf)
      sampling_line_sf = sf::st_transform(line_sf, sampling_crs)
    }
    native_spacing = resolve_person_line_spacing(line_spacing, sampling_crs)
    raw_samples = sample_person_line_geometry(sampling_line_sf, native_spacing)
  }
  direction_xy = cbind(
    c(raw_samples$x, raw_samples$x + raw_samples$dx),
    c(raw_samples$y, raw_samples$y + raw_samples$dy)
  )
  if (
    !is.na(line_crs) &&
      !is.na(sampling_crs) &&
      !scene_crs_equal(sampling_crs, line_crs)
  ) {
    direction_points = sf::st_as_sf(
      data.frame(x = direction_xy[, 1], y = direction_xy[, 2]),
      coords = c("x", "y"),
      crs = sampling_crs
    )
    direction_points = sf::st_transform(direction_points, line_crs)
    direction_xy = sf::st_coordinates(direction_points)[, 1:2, drop = FALSE]
  }
  source_crs = if (is.na(line_crs)) NULL else line_crs
  n_samples = nrow(raw_samples)
  transformed_xy = auto_transform_scene_xy(
    x = direction_xy[, 1],
    y = direction_xy[, 2],
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    crs = source_crs,
    caller = caller
  )
  sample_x = transformed_xy$x[seq_len(n_samples)]
  sample_y = transformed_xy$y[seq_len(n_samples)]
  direction_x = transformed_xy$x[n_samples + seq_len(n_samples)] - sample_x
  direction_y = transformed_xy$y[n_samples + seq_len(n_samples)] - sample_y
  list(
    x = sample_x,
    y = sample_y,
    angle = person_line_yaw(direction_x, direction_y),
    extent = transformed_xy$extent,
    panel = transformed_xy$panel
  )
}

#' Create a Local Meter-Based CRS for a Person Line
#'
#' @param line_sf An `sf` object with longitude/latitude line geometry.
#'
#' @return An azimuthal equidistant coordinate reference system centered on the
#' line.
#' @keywords internal
person_line_local_metric_crs = function(line_sf) {
  line_wgs84 = sf::st_transform(line_sf, 4326)
  line_bbox = sf::st_bbox(line_wgs84)
  center_longitude = mean(c(line_bbox["xmin"], line_bbox["xmax"]))
  center_latitude = mean(c(line_bbox["ymin"], line_bbox["ymax"]))
  if (!is.finite(center_longitude) || !is.finite(center_latitude)) {
    stop("Could not determine a metric CRS for `line`.", call. = FALSE)
  }
  sf::st_crs(sprintf(
    paste0(
      "+proj=aeqd +lat_0=%.12f +lon_0=%.12f ",
      "+datum=WGS84 +units=m +no_defs"
    ),
    center_latitude,
    center_longitude
  ))
}

#' Resolve Person Line Spacing Units
#'
#' @param line_spacing Spacing in meters.
#' @param crs Line coordinate reference system.
#'
#' @return Spacing in the native line-coordinate units.
#' @keywords internal
resolve_person_line_spacing = function(line_spacing, crs) {
  if (is.null(crs) || is.na(crs)) {
    return(as.numeric(line_spacing))
  }
  crs_units = crs$units_gdal
  if (is.null(crs_units) || is.na(crs_units) || !nzchar(crs_units)) {
    return(as.numeric(line_spacing))
  }
  unit_name = gsub(" ", "_", crs_units, fixed = TRUE)
  converted_spacing = tryCatch(
    units::set_units(
      units::set_units(line_spacing, "m"),
      unit_name,
      mode = "standard"
    ),
    error = function(e) {
      stop(
        sprintf(
          "Could not convert `line_spacing` from meters to CRS units %s.",
          shQuote(crs_units)
        ),
        call. = FALSE
      )
    }
  )
  as.numeric(converted_spacing)
}

#' Sample Native Line Coordinates
#'
#' @param line_sf An `sf` object containing LINESTRING geometries.
#' @param line_spacing Positive spacing in native coordinate units.
#'
#' @return A data frame of sample coordinates and forward unit vectors.
#' @keywords internal
sample_person_line_geometry = function(line_sf, line_spacing) {
  sample_list = lapply(
    seq_along(sf::st_geometry(line_sf)),
    function(line_index) {
      coordinates = unclass(sf::st_geometry(line_sf)[[line_index]])
      coordinates = as.matrix(coordinates)[, 1:2, drop = FALSE]
      keep = c(
        TRUE,
        rowSums(diff(coordinates)^2) > .Machine$double.eps
      )
      coordinates = coordinates[keep, , drop = FALSE]
      if (nrow(coordinates) < 2) {
        return(NULL)
      }
      segment_delta = diff(coordinates)
      segment_length = sqrt(rowSums(segment_delta^2))
      total_length = sum(segment_length)
      if (!is.finite(total_length) || total_length <= 0) {
        return(NULL)
      }
      n_intervals = floor(
        (total_length + sqrt(.Machine$double.eps) * total_length) / line_spacing
      )
      sample_distance = seq.int(0, n_intervals) * line_spacing
      cumulative_length = c(0, cumsum(segment_length))
      segment_index = findInterval(
        sample_distance,
        cumulative_length,
        rightmost.closed = TRUE,
        all.inside = TRUE
      )
      segment_fraction = (sample_distance - cumulative_length[segment_index]) /
        segment_length[segment_index]
      sample_xy = coordinates[segment_index, , drop = FALSE] +
        segment_delta[segment_index, , drop = FALSE] * segment_fraction
      direction_xy = segment_delta[segment_index, , drop = FALSE] /
        segment_length[segment_index]
      data.frame(
        x = sample_xy[, 1],
        y = sample_xy[, 2],
        dx = direction_xy[, 1],
        dy = direction_xy[, 2],
        line = line_index,
        distance = sample_distance
      )
    }
  )
  sample_list = Filter(Negate(is.null), sample_list)
  if (!length(sample_list)) {
    stop("`line` must have positive length.", call. = FALSE)
  }
  do.call(rbind, sample_list)
}

#' Sample Native Line Coordinates Along Terrain
#'
#' @param line_sf An `sf` object containing LINESTRING geometries in the scene
#' coordinate reference system.
#' @param line_spacing Distance between samples in meters.
#' @param extent Scene extent.
#' @param heightmap Scene heightmap.
#' @param zscale Effective scene zscale.
#'
#' @return A data frame of sample coordinates and forward vectors.
#' @keywords internal
sample_person_terrain_line_geometry = function(
  line_sf,
  line_spacing,
  extent,
  heightmap,
  zscale
) {
  if (
    !is.numeric(zscale) ||
      length(zscale) != 1 ||
      !is.finite(zscale) ||
      zscale <= 0
  ) {
    stop("`zscale` must be a single positive number.", call. = FALSE)
  }
  terrain_extent = get_extent(extent)
  extent_width = terrain_extent["xmax"] - terrain_extent["xmin"]
  extent_height = terrain_extent["ymax"] - terrain_extent["ymin"]
  if (
    !is.finite(extent_width) ||
      extent_width <= 0 ||
      !is.finite(extent_height) ||
      extent_height <= 0
  ) {
    stop("`extent` must have positive width and height.", call. = FALSE)
  }

  map_width = nrow(heightmap) - 1
  map_height = ncol(heightmap) - 1
  spacing_scene = line_spacing / zscale
  heightmap_scene = heightmap / zscale
  tolerance = sqrt(.Machine$double.eps)

  crossing_parameters = function(start, end, lower, upper) {
    delta = end - start
    if (!is.finite(delta) || abs(delta) <= tolerance) {
      return(numeric())
    }
    first_boundary = max(lower, ceiling(min(start, end)))
    last_boundary = min(upper, floor(max(start, end)))
    if (first_boundary > last_boundary) {
      return(numeric())
    }
    parameters = (seq.int(first_boundary, last_boundary) - start) / delta
    parameters[
      parameters > tolerance & parameters < 1 - tolerance
    ]
  }

  sample_list = lapply(
    seq_along(sf::st_geometry(line_sf)),
    function(line_index) {
      coordinates = unclass(sf::st_geometry(line_sf)[[line_index]])
      coordinates = as.matrix(coordinates)[, 1:2, drop = FALSE]
      keep = c(
        TRUE,
        rowSums(diff(coordinates)^2) > .Machine$double.eps
      )
      coordinates = coordinates[keep, , drop = FALSE]
      if (nrow(coordinates) < 2) {
        return(NULL)
      }

      terrain_row = (coordinates[, 1] - terrain_extent["xmin"]) /
        extent_width *
        map_width +
        1
      terrain_col = 1 +
        map_height -
        (coordinates[, 2] - terrain_extent["ymin"]) /
          extent_height *
          map_height
      profile_parts = lapply(
        seq_len(nrow(coordinates) - 1),
        function(segment_index) {
          row_start = terrain_row[segment_index]
          row_end = terrain_row[segment_index + 1]
          col_start = terrain_col[segment_index]
          col_end = terrain_col[segment_index + 1]
          sum_start = row_start + col_start
          sum_end = row_end + col_end
          segment_breaks = c(
            0,
            crossing_parameters(row_start, row_end, 1, nrow(heightmap)),
            crossing_parameters(col_start, col_end, 1, ncol(heightmap)),
            crossing_parameters(
              sum_start,
              sum_end,
              2,
              nrow(heightmap) + ncol(heightmap)
            ),
            1
          )
          segment_breaks = sort(unique(segment_breaks))
          diagonal_break = !(terrain_row[segment_index] ==
            terrain_row[segment_index + 1] &&
            terrain_col[segment_index] == terrain_col[segment_index + 1])
          if (diagonal_break) {
            break_rows = row_start + (row_end - row_start) * segment_breaks
            break_cols = col_start + (col_end - col_start) * segment_breaks
            inside = break_rows >= 1 - tolerance &
              break_rows <= nrow(heightmap) + tolerance &
              break_cols >= 1 - tolerance &
              break_cols <= ncol(heightmap) + tolerance
            grid_break = abs(break_rows - round(break_rows)) <= tolerance |
              abs(break_cols - round(break_cols)) <= tolerance
            keep_break = inside |
              grid_break |
              segment_breaks == 0 |
              segment_breaks == 1
            segment_breaks = segment_breaks[keep_break]
          }
          delta = coordinates[segment_index + 1, ] -
            coordinates[segment_index, ]
          part = data.frame(
            x = coordinates[segment_index, 1] + delta[1] * segment_breaks,
            y = coordinates[segment_index, 2] + delta[2] * segment_breaks
          )
          if (segment_index > 1) {
            part = part[-1, , drop = FALSE]
          }
          part
        }
      )
      profile = do.call(rbind, profile_parts)
      profile$scene_x = (profile$x - terrain_extent["xmin"]) /
        extent_width *
        map_width -
        map_width / 2
      profile$scene_z = map_height /
        2 -
        (profile$y - terrain_extent["ymin"]) /
          extent_height *
          map_height
      terrain_y = interpolate_render_heightmap_height(
        heightmap_scene,
        profile$scene_x,
        profile$scene_z
      )
      if (any(!is.finite(terrain_y))) {
        fallback_height = suppressWarnings(min(heightmap_scene, na.rm = TRUE))
        if (!is.finite(fallback_height)) {
          fallback_height = 0
        }
        terrain_y[!is.finite(terrain_y)] = fallback_height
      }
      segment_length = sqrt(
        diff(profile$scene_x)^2 +
          diff(terrain_y)^2 +
          diff(profile$scene_z)^2
      )
      keep = c(TRUE, segment_length > .Machine$double.eps)
      profile = profile[keep, , drop = FALSE]
      terrain_y = terrain_y[keep]
      if (nrow(profile) < 2) {
        return(NULL)
      }
      segment_length = sqrt(
        diff(profile$scene_x)^2 +
          diff(terrain_y)^2 +
          diff(profile$scene_z)^2
      )
      total_length = sum(segment_length)
      if (!is.finite(total_length) || total_length <= 0) {
        return(NULL)
      }
      n_intervals = floor(
        (total_length + tolerance * total_length) / spacing_scene
      )
      sample_distance = seq.int(0, n_intervals) * spacing_scene
      cumulative_length = c(0, cumsum(segment_length))
      profile_index = findInterval(
        sample_distance,
        cumulative_length,
        rightmost.closed = TRUE,
        all.inside = TRUE
      )
      profile_fraction =
        (sample_distance - cumulative_length[profile_index]) /
        segment_length[profile_index]
      profile_delta_x = diff(profile$x)
      profile_delta_y = diff(profile$y)
      data.frame(
        x = profile$x[profile_index] +
          profile_delta_x[profile_index] * profile_fraction,
        y = profile$y[profile_index] +
          profile_delta_y[profile_index] * profile_fraction,
        dx = profile_delta_x[profile_index],
        dy = profile_delta_y[profile_index],
        line = line_index,
        distance = sample_distance * zscale
      )
    }
  )
  sample_list = Filter(Negate(is.null), sample_list)
  if (!length(sample_list)) {
    stop("`line` must have positive length.", call. = FALSE)
  }
  do.call(rbind, sample_list)
}

#' Convert Line Directions to Person Yaw
#'
#' @param dx Line direction in scene x coordinates.
#' @param dy Line direction in scene y coordinates.
#'
#' @return Rotation around the model's Y axis in degrees.
#' @keywords internal
person_line_yaw = function(dx, dy) {
  direction_length = sqrt(dx^2 + dy^2)
  if (any(!is.finite(direction_length)) || any(direction_length <= 0)) {
    stop("Could not determine person orientation along `line`.", call. = FALSE)
  }
  atan2(-dx, -dy) * 180 / pi
}

#' Align Person Orientations to Terrain
#'
#' @param x Line placement x coordinates in the scene extent.
#' @param y Line placement y coordinates in the scene extent.
#' @param line_angle Automatic line yaw angles in degrees.
#' @param extent Scene extent.
#' @param heightmap Scene heightmap.
#' @param zscale Effective scene zscale.
#' @param panel Default `NULL`. Facet panel identifier.
#'
#' @return A three-column matrix of Euler angles in degrees.
#' @keywords internal
person_terrain_line_angles = function(
  x,
  y,
  line_angle,
  extent,
  heightmap,
  zscale,
  panel = NULL
) {
  flat_angles = cbind(
    x = rep(0, length(line_angle)),
    y = line_angle,
    z = rep(0, length(line_angle))
  )
  if (is.null(heightmap) || !is.matrix(heightmap) || !length(line_angle)) {
    return(flat_angles)
  }
  terrain_extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    panel = panel,
    caller = "render_people",
    error_if_missing = FALSE
  )
  if (is.null(terrain_extent)) {
    return(flat_angles)
  }
  terrain_extent = get_extent(terrain_extent)
  extent_width = terrain_extent["xmax"] - terrain_extent["xmin"]
  extent_height = terrain_extent["ymax"] - terrain_extent["ymin"]
  if (
    !is.finite(extent_width) ||
      extent_width <= 0 ||
      !is.finite(extent_height) ||
      extent_height <= 0
  ) {
    return(flat_angles)
  }
  map_width = nrow(heightmap) - 1
  map_height = ncol(heightmap) - 1
  scene_x = (x - terrain_extent["xmin"]) /
    extent_width *
    map_width -
    map_width / 2
  scene_z = map_height /
    2 -
    (y - terrain_extent["ymin"]) / extent_height * map_height
  terrain_points = cbind(scene_x, 0, scene_z)
  terrain_normals = interpolate_render_highquality_normals(
    points = terrain_points,
    heightmap = heightmap,
    zscale = zscale
  )

  yaw_radians = line_angle * pi / 180
  horizontal_forward = cbind(
    -sin(yaw_radians),
    rep(0, length(yaw_radians)),
    cos(yaw_radians)
  )
  valid_up = abs(terrain_normals[, 2]) >= sqrt(.Machine$double.eps)
  terrain_forward = horizontal_forward
  terrain_forward[valid_up, 2] = -(horizontal_forward[valid_up, 1] *
    terrain_normals[valid_up, 1] +
    horizontal_forward[valid_up, 3] * terrain_normals[valid_up, 3]) /
    terrain_normals[valid_up, 2]
  if (any(!valid_up)) {
    terrain_forward[!valid_up, ] =
      horizontal_forward[!valid_up, , drop = FALSE] -
      terrain_normals[!valid_up, , drop = FALSE] *
        rowSums(
          horizontal_forward[!valid_up, , drop = FALSE] *
            terrain_normals[!valid_up, , drop = FALSE]
        )
  }
  terrain_forward = normalize_person_orientation_rows(
    terrain_forward,
    fallback = horizontal_forward
  )
  terrain_right = normalize_person_orientation_rows(
    row_cross(terrain_normals, terrain_forward),
    fallback = cbind(
      cos(yaw_radians),
      rep(0, length(yaw_radians)),
      sin(yaw_radians)
    )
  )
  terrain_forward = normalize_person_orientation_rows(
    row_cross(terrain_right, terrain_normals),
    fallback = horizontal_forward
  )

  angles = vapply(
    seq_along(line_angle),
    function(index) {
      person_rotation_matrix_to_euler(
        rotation = rbind(
          terrain_right[index, ],
          terrain_normals[index, ],
          terrain_forward[index, ]
        ),
        yaw_reference = line_angle[index]
      )
    },
    numeric(3)
  )
  t(angles)
}

#' Normalize Person Orientation Vectors
#'
#' @param values Matrix of row vectors.
#' @param fallback Matrix of fallback row vectors.
#'
#' @return A matrix of unit-length row vectors.
#' @keywords internal
normalize_person_orientation_rows = function(values, fallback) {
  values = as.matrix(values)
  fallback = as.matrix(fallback)
  if (nrow(fallback) == 1 && nrow(values) > 1) {
    fallback = fallback[rep(1, nrow(values)), , drop = FALSE]
  }
  lengths = sqrt(rowSums(values^2))
  invalid = !stats::complete.cases(values) |
    !is.finite(lengths) |
    lengths < sqrt(.Machine$double.eps)
  if (any(invalid)) {
    values[invalid, ] = fallback[invalid, , drop = FALSE]
    lengths[invalid] = sqrt(rowSums(values[invalid, , drop = FALSE]^2))
  }
  values / lengths
}

#' Convert a Person Rotation Matrix to Euler Angles
#'
#' @param rotation Three-by-three row-vector rotation matrix.
#' @param yaw_reference Default `NULL`. Preferred equivalent Y-axis angle in
#' degrees.
#'
#' @return Three Euler angles in degrees using the rotation order expected by
#' `rayvertex::rotate_mesh()`.
#' @keywords internal
person_rotation_matrix_to_euler = function(
  rotation,
  yaw_reference = NULL
) {
  rotation = as.matrix(rotation)
  if (
    !is.numeric(rotation) ||
      !identical(dim(rotation), c(3L, 3L)) ||
      any(!is.finite(rotation))
  ) {
    stop("`rotation` must be a finite 3 by 3 numeric matrix.", call. = FALSE)
  }
  sin_y = min(1, max(-1, rotation[1, 3]))
  angle_y = asin(sin_y)
  cos_y = cos(angle_y)
  if (abs(cos_y) > sqrt(.Machine$double.eps)) {
    angle_x = atan2(-rotation[2, 3], rotation[3, 3])
    angle_z = atan2(-rotation[1, 2], rotation[1, 1])
  } else {
    angle_x = atan2(
      sign(sin_y) * rotation[2, 1],
      rotation[2, 2]
    )
    angle_z = 0
  }
  primary = c(angle_x, angle_y, angle_z) * 180 / pi
  alternate_y = if (angle_y >= 0) pi - angle_y else -pi - angle_y
  alternate = c(angle_x + pi, alternate_y, angle_z + pi) * 180 / pi
  candidates = rbind(primary, alternate)
  candidates[, c(1, 3)] =
    (candidates[, c(1, 3), drop = FALSE] + 180) %% 360 - 180
  if (is.null(yaw_reference)) {
    return(unname(candidates[1, ]))
  }
  candidates[, 2] = candidates[, 2] +
    360 * round((yaw_reference - candidates[, 2]) / 360)
  selected = which.min(abs(candidates[, 2] - yaw_reference))
  unname(candidates[selected, ])
}

#' Resolve Person Line Angles
#'
#' @param angle User-supplied model rotations.
#' @param line_angle Automatic line yaw angles.
#' @param automatic_angle Default `NULL`. Complete automatic orientation angles.
#'
#' @return A three-column angle matrix.
#' @keywords internal
resolve_person_line_angles = function(
  angle,
  line_angle,
  automatic_angle = NULL
) {
  n = length(line_angle)
  if (is.list(angle) && !is.data.frame(angle)) {
    angle = do.call(rbind, angle)
  }
  if (is.matrix(angle) || is.data.frame(angle)) {
    angle = as.matrix(angle)
    if (!is.numeric(angle) || ncol(angle) != 3) {
      stop("`angle` must have exactly three numeric columns.", call. = FALSE)
    }
    if (nrow(angle) == 1 && n > 1) {
      angle = angle[rep(1, n), , drop = FALSE]
    }
    if (nrow(angle) != n) {
      stop(
        "For line placement, `angle` must have one row per person.",
        call. = FALSE
      )
    }
  } else {
    if (!is.numeric(angle) || length(angle) != 3 || anyNA(angle)) {
      stop("`angle` must contain exactly three numeric values.", call. = FALSE)
    }
    angle = matrix(angle, nrow = n, ncol = 3, byrow = TRUE)
  }
  if (is.null(automatic_angle)) {
    automatic_angle = cbind(
      x = rep(0, n),
      y = line_angle,
      z = rep(0, n)
    )
  } else {
    automatic_angle = as.matrix(automatic_angle)
    if (
      !is.numeric(automatic_angle) ||
        !identical(dim(automatic_angle), c(n, 3L)) ||
        any(!is.finite(automatic_angle))
    ) {
      stop(
        "`automatic_angle` must have one finite three-angle row per person.",
        call. = FALSE
      )
    }
  }
  angle + automatic_angle
}

#' Resolve a Repeating Person Pattern
#'
#' @param line_pattern Default `NULL`. String containing `M` and `F`.
#' @param n Number of people.
#' @param sex Default `"male"`. Fallback sex when `line_pattern` is `NULL`.
#'
#' @return Character vector of canonical sex names.
#' @keywords internal
resolve_person_pattern = function(line_pattern = NULL, n, sex = "male") {
  if (is.null(line_pattern)) {
    return(rep(resolve_person_sex(sex), n))
  }
  if (
    !is.character(line_pattern) ||
      length(line_pattern) != 1 ||
      is.na(line_pattern) ||
      !nzchar(line_pattern)
  ) {
    stop(
      "`line_pattern` must be a non-empty string containing M and F.",
      call. = FALSE
    )
  }
  pattern_values = strsplit(toupper(line_pattern), "", fixed = TRUE)[[1]]
  if (any(!pattern_values %in% c("M", "F"))) {
    stop("`line_pattern` may contain only M and F.", call. = FALSE)
  }
  pattern_sexes = ifelse(pattern_values == "M", "male", "female")
  rep(pattern_sexes, length.out = n)
}

#' Resolve Repeating Person Pattern Colors
#'
#' @param color Person model color specification.
#' @param line_pattern Default `NULL`. Repeating male/female pattern.
#' @param n Number of generated people.
#'
#' @return A color specification suitable for all line placements.
#' @keywords internal
resolve_person_pattern_colors = function(color, line_pattern = NULL, n) {
  if (is.numeric(color) && length(color) == 3) {
    return(color)
  }
  if (length(color) <= 1 || length(color) == n) {
    return(color)
  }
  if (!is.null(line_pattern)) {
    pattern_length = nchar(line_pattern, type = "chars")
    if (length(color) == pattern_length) {
      return(rep(color, length.out = n))
    }
  }
  stop(
    paste0(
      "For line placement, `color` must contain one value, one value per ",
      "generated person, or one value per entry in `line_pattern`."
    ),
    call. = FALSE
  )
}
