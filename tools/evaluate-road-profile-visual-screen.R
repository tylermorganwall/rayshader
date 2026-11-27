# Evaluate the completed road-profile visual sensitivity campaign.
#
# This script replays only road construction, not path tracing. It records
# physical road/terrain clearance and longitudinal-profile metrics for every
# factor cell, checkpointing each scene/cell so interrupted runs resume. Visual
# ratings are read from evaluation/visual-ratings.csv and joined to the numeric
# results. CSV files are always produced; an Excel workbook is also written when
# the writexl package is available.

repo_root = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run this script from the rayshader repository root.", call. = FALSE)
}

evaluation_input = function(name, default) {
  Sys.getenv(name, unset = as.character(default))
}

evaluation_flag = function(name, default = FALSE) {
  value = tolower(evaluation_input(name, if (default) "true" else "false"))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(sprintf("%s must be true or false.", name), call. = FALSE)
  }
  value %in% c("true", "1", "yes")
}

campaign_dir = normalizePath(
  evaluation_input(
    "RAYSHADER_SCREEN_OUTPUT_DIR",
    file.path(repo_root, "road-profile-visual-screen")
  ),
  winslash = "/",
  mustWork = TRUE
)
configuration_path = file.path(campaign_dir, "run-configuration.csv")
design_path = file.path(campaign_dir, "factor-design.csv")
manifest_path = file.path(campaign_dir, "render-manifest.csv")
required_campaign_files = c(configuration_path, design_path, manifest_path)
if (any(!file.exists(required_campaign_files))) {
  stop(
    "The campaign directory is missing its design or manifest.",
    call. = FALSE
  )
}

configuration = utils::read.csv(configuration_path, stringsAsFactors = FALSE)
configuration_value = function(name, default = NA_character_) {
  match = configuration$value[configuration$name == name]
  if (length(match)) match[[1L]] else default
}

input_dir = normalizePath(
  evaluation_input(
    "RAYSHADER_SCREEN_INPUT_DIR",
    configuration_value(
      "input_dir",
      file.path(repo_root, "road-profile-screen-inputs")
    )
  ),
  winslash = "/",
  mustWork = TRUE
)
evaluation_dir = evaluation_input(
  "RAYSHADER_SCREEN_EVALUATION_DIR",
  file.path(campaign_dir, "evaluation")
)
checkpoint_dir = file.path(evaluation_dir, "numeric-checkpoints")
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

selected_scenes = trimws(strsplit(
  evaluation_input(
    "RAYSHADER_SCREEN_EVALUATION_SCENES",
    "swiss_switchback,houston_interchange,houston_overpass"
  ),
  ",",
  fixed = TRUE
)[[1L]])
selected_cells_input = evaluation_input(
  "RAYSHADER_SCREEN_EVALUATION_CELLS",
  "all"
)
overwrite = evaluation_flag("RAYSHADER_SCREEN_EVALUATION_OVERWRITE")
parallel_mesh = evaluation_flag("RAYSHADER_SCREEN_EVALUATION_PARALLEL")
vertical_exaggeration = as.numeric(configuration_value(
  "vertical_exaggeration",
  "2"
))

required_packages = c("pkgload", "sf", "terra", "rgl")
missing_packages = required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]
if (length(missing_packages)) {
  stop(
    sprintf(
      "Install required package(s): %s.",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

old_null = Sys.getenv("RGL_USE_NULL", unset = NA_character_)
Sys.setenv(RGL_USE_NULL = "true")
on.exit(
  {
    options(rayshader.road_profile_screen_settings = NULL)
    try(rgl::close3d(), silent = TRUE)
    if (is.na(old_null)) {
      Sys.unsetenv("RGL_USE_NULL")
    } else {
      Sys.setenv(RGL_USE_NULL = old_null)
    }
  },
  add = TRUE
)
pkgload::load_all(repo_root, quiet = TRUE)

design = utils::read.csv(design_path, stringsAsFactors = FALSE)
manifest = utils::read.csv(manifest_path, stringsAsFactors = FALSE)
selected_cells = if (tolower(trimws(selected_cells_input)) == "all") {
  design$cell_id
} else {
  trimws(strsplit(selected_cells_input, ",", fixed = TRUE)[[1L]])
}
unknown_cells = setdiff(selected_cells, design$cell_id)
if (length(unknown_cells)) {
  stop(sprintf(
    "Unknown factor cell(s): %s.",
    paste(unknown_cells, collapse = ", ")
  ))
}
if (!"H00" %in% selected_cells) {
  selected_cells = c("H00", selected_cells)
}

scene_manifest = data.frame(
  scene_id = c("swiss_switchback", "houston_interchange", "houston_overpass"),
  case_id = c("swiss", "houston", "houston_overpass"),
  center_x = c(-450, 50, NA),
  center_z = c(-297, 110, NA),
  half_width_cells = c(120, 90, NA),
  half_height_cells = c(120, 80, NA),
  stringsAsFactors = FALSE
)
unknown_scenes = setdiff(selected_scenes, scene_manifest$scene_id)
if (length(unknown_scenes)) {
  stop(sprintf("Unknown scene(s): %s.", paste(unknown_scenes, collapse = ", ")))
}

scene_coordinate_to_spatial = function(dem, x, z) {
  extent = terra::ext(dem)
  x_cells = terra::ncol(dem) - 1
  z_cells = terra::nrow(dem) - 1
  c(
    x = terra::xmin(extent) +
      (x + x_cells / 2) / x_cells * (terra::xmax(extent) - terra::xmin(extent)),
    y = terra::ymin(extent) +
      (z_cells / 2 - z) / z_cells * (terra::ymax(extent) - terra::ymin(extent))
  )
}

load_scene = function(scene_id) {
  scene = scene_manifest[scene_manifest$scene_id == scene_id, , drop = FALSE]
  case_dir = file.path(input_dir, scene$case_id)
  case = list(
    case_id = scene$case_id,
    roads = readRDS(file.path(case_dir, "roads.rds")),
    dem = terra::rast(file.path(case_dir, "dem.tif"))
  )
  if (
    !all(is.finite(c(
      scene$center_x,
      scene$center_z,
      scene$half_width_cells,
      scene$half_height_cells
    )))
  ) {
    return(case)
  }
  center = scene_coordinate_to_spatial(case$dem, scene$center_x, scene$center_z)
  resolution = terra::res(case$dem)
  crop_extent = terra::ext(
    center[["x"]] - scene$half_width_cells * resolution[[1L]],
    center[["x"]] + scene$half_width_cells * resolution[[1L]],
    center[["y"]] - scene$half_height_cells * resolution[[2L]],
    center[["y"]] + scene$half_height_cells * resolution[[2L]]
  )
  case$dem = terra::crop(case$dem, crop_extent, snap = "near")
  crop_bbox = sf::st_bbox(
    c(
      xmin = terra::xmin(case$dem),
      ymin = terra::ymin(case$dem),
      xmax = terra::xmax(case$dem),
      ymax = terra::ymax(case$dem)
    ),
    crs = sf::st_crs(terra::crs(case$dem))
  )
  case$roads = suppressWarnings(sf::st_crop(case$roads, crop_bbox))
  case$roads = case$roads[!sf::st_is_empty(case$roads), ]
  case
}

parse_osm_integer = function(value) {
  suppressWarnings(as.integer(sub("[^0-9].*$", "", value)))
}

lane_count = function(roads) {
  lanes = parse_osm_integer(roads$lanes)
  forward = parse_osm_integer(roads[["lanes:forward"]])
  backward = parse_osm_integer(roads[["lanes:backward"]])
  directional = rowSums(cbind(forward, backward), na.rm = TRUE)
  directional[is.na(forward) & is.na(backward)] = NA_integer_
  lanes[is.na(lanes)] = directional[is.na(lanes)]
  link = grepl("_link$", roads$highway)
  oneway = !is.na(roads$oneway) & roads$oneway %in% c("yes", "1", "true")
  lanes[is.na(lanes) & (link | oneway)] = 1L
  lanes[is.na(lanes)] = 2L
  pmax(lanes, 1L)
}

finite_quantile = function(value, probability) {
  value = value[is.finite(value)]
  if (!length(value)) {
    return(NA_real_)
  }
  unname(stats::quantile(value, probability, names = FALSE, type = 8))
}

safe_extreme = function(value, fun) {
  value = value[is.finite(value)]
  if (!length(value)) {
    return(NA_real_)
  }
  fun(value)
}

path_measurements = function(coordinates, path_info, heightmap, zscale) {
  points = rayshader:::densify_render_highquality_path_points(
    points = coordinates,
    width = path_info$width,
    heightmap = if (isTRUE(path_info$terrain_following)) heightmap else NULL,
    zscale = zscale
  )
  frames = rayshader:::calculate_render_road_vertex_frames(points)
  sections = rayshader:::calculate_render_road_vertex_sections(
    points = points,
    left_distance = rep(path_info$width / 2, nrow(points)),
    right_distance = rep(path_info$width / 2, nrow(points)),
    heightmap = if (isTRUE(path_info$terrain_following)) heightmap else NULL,
    zscale = zscale,
    frames = frames
  )
  lateral_fraction = seq(-1, 1, length.out = 9L)
  surface = lapply(lateral_fraction, function(fraction) {
    (1 - (fraction + 1) / 2) *
      sections$left_top +
      ((fraction + 1) / 2) * sections$right_top
  })
  surface = do.call(rbind, surface)
  terrain = rayshader:::interpolate_render_heightmap_height(
    heightmap,
    surface[, 1L],
    surface[, 3L]
  ) /
    zscale
  clearance_m = (surface[, 2L] - terrain) * zscale
  station_m = rayshader:::calculate_road_path_cumulative_distance(
    points,
    texture_world_scale = path_info$texture_world_scale
  )
  elevation_m = points[, 2L] * zscale
  distance_m = diff(station_m)
  elevation_step_m = diff(elevation_m)
  valid_segment = is.finite(distance_m) &
    distance_m > 1e-8 &
    is.finite(elevation_step_m)
  grade = elevation_step_m[valid_segment] / distance_m[valid_segment]
  grade_distance_m = distance_m[valid_segment]
  grade_midpoint_m = (station_m[-length(station_m)] + station_m[-1L]) / 2
  grade_midpoint_m = grade_midpoint_m[valid_segment]
  if (length(grade) >= 2L) {
    grade_spacing_m = diff(grade_midpoint_m)
    valid_rate = is.finite(grade_spacing_m) & grade_spacing_m > 1e-8
    grade_rate = diff(grade)[valid_rate] / grade_spacing_m[valid_rate]
    grade_rate_distance_m = grade_spacing_m[valid_rate]
  } else {
    grade_rate = numeric()
    grade_rate_distance_m = numeric()
  }
  station_index = rep(seq_len(nrow(points)), times = length(lateral_fraction))
  low_side_gap_m = vapply(
    seq_len(nrow(points)),
    function(index) {
      max(clearance_m[station_index == index], na.rm = TRUE)
    },
    numeric(1)
  )
  segment_area_m2 = c(diff(station_m), 0) *
    path_info$width /
    length(lateral_fraction)
  sample_area_m2 = rep(segment_area_m2, times = length(lateral_fraction))
  list(
    clearance_m = clearance_m,
    sample_area_m2 = sample_area_m2,
    low_side_gap_m = low_side_gap_m,
    grade = grade,
    grade_distance_m = grade_distance_m,
    grade_rate = grade_rate,
    grade_rate_distance_m = grade_rate_distance_m,
    elevation_step_m = elevation_step_m[valid_segment],
    station_m = station_m,
    elevation_m = elevation_m
  )
}

profile_displacement = function(current, baseline, zscale) {
  path_count = min(length(current$coordinates), length(baseline$coordinates))
  displacement = vector("list", path_count)
  for (path_id in seq_len(path_count)) {
    current_coordinates = current$coordinates[[path_id]]
    baseline_coordinates = baseline$coordinates[[path_id]]
    current_scale = current$path_info[[path_id]]$texture_world_scale
    baseline_scale = baseline$path_info[[path_id]]$texture_world_scale
    current_station = rayshader:::calculate_road_path_cumulative_distance(
      current_coordinates,
      texture_world_scale = current_scale
    )
    baseline_station = rayshader:::calculate_road_path_cumulative_distance(
      baseline_coordinates,
      texture_world_scale = baseline_scale
    )
    if (!length(current_station) || !length(baseline_station)) {
      next
    }
    normalized_current = if (max(current_station) > 0) {
      current_station / max(current_station)
    } else {
      seq(0, 1, length.out = length(current_station))
    }
    normalized_baseline = if (max(baseline_station) > 0) {
      baseline_station / max(baseline_station)
    } else {
      seq(0, 1, length.out = length(baseline_station))
    }
    baseline_elevation = stats::approx(
      normalized_baseline,
      baseline_coordinates[, 2L] * zscale,
      xout = normalized_current,
      ties = "ordered",
      rule = 2
    )$y
    displacement[[path_id]] = current_coordinates[, 2L] *
      zscale -
      baseline_elevation
  }
  unlist(displacement, use.names = FALSE)
}

summarize_result = function(result, baseline, heightmap, zscale) {
  path_count = min(length(result$coordinates), length(result$path_info))
  measured = lapply(seq_len(path_count), function(path_id) {
    path_measurements(
      result$coordinates[[path_id]],
      result$path_info[[path_id]],
      heightmap,
      zscale
    )
  })
  clearance_m = unlist(lapply(measured, `[[`, "clearance_m"), use.names = FALSE)
  sample_area_m2 = unlist(
    lapply(measured, `[[`, "sample_area_m2"),
    use.names = FALSE
  )
  low_side_gap_m = unlist(
    lapply(measured, `[[`, "low_side_gap_m"),
    use.names = FALSE
  )
  grade = unlist(lapply(measured, `[[`, "grade"), use.names = FALSE)
  grade_distance_m = unlist(
    lapply(measured, `[[`, "grade_distance_m"),
    use.names = FALSE
  )
  grade_rate = unlist(lapply(measured, `[[`, "grade_rate"), use.names = FALSE)
  grade_rate_distance_m = unlist(
    lapply(measured, `[[`, "grade_rate_distance_m"),
    use.names = FALSE
  )
  elevation_step_m = unlist(
    lapply(measured, `[[`, "elevation_step_m"),
    use.names = FALSE
  )
  valid_clearance = is.finite(clearance_m)
  penetration_m = pmax(-clearance_m, 0)
  displacement_m = profile_displacement(result, baseline, zscale)
  absolute_grade = abs(grade)
  absolute_grade_rate = abs(grade_rate)
  diagnostics = result$diagnostics
  maximum_engineering_violation = diagnostics$maximum_engineering_violation
  if (is.null(maximum_engineering_violation)) {
    maximum_engineering_violation = NA_real_
  }
  total_grade_distance = sum(grade_distance_m, na.rm = TRUE)
  grade_threshold_tolerance = 1e-6
  data.frame(
    path_count = path_count,
    minimum_clearance_m = safe_extreme(clearance_m, min),
    p95_penetration_m = finite_quantile(penetration_m, 0.95),
    maximum_penetration_m = safe_extreme(penetration_m, max),
    approximate_penetration_area_m2 = sum(
      sample_area_m2[valid_clearance & clearance_m < 0],
      na.rm = TRUE
    ),
    p95_low_side_gap_m = finite_quantile(pmax(low_side_gap_m, 0), 0.95),
    maximum_low_side_gap_m = safe_extreme(pmax(low_side_gap_m, 0), max),
    p50_absolute_grade = finite_quantile(absolute_grade, 0.5),
    p95_absolute_grade = finite_quantile(absolute_grade, 0.95),
    maximum_absolute_grade = safe_extreme(absolute_grade, max),
    fraction_length_above_7_percent_grade = if (total_grade_distance > 0) {
      sum(
        grade_distance_m[
          absolute_grade > 0.07 + grade_threshold_tolerance
        ],
        na.rm = TRUE
      ) /
        total_grade_distance
    } else {
      NA_real_
    },
    fraction_length_above_15_percent_grade = if (total_grade_distance > 0) {
      sum(
        grade_distance_m[
          absolute_grade > 0.15 + grade_threshold_tolerance
        ],
        na.rm = TRUE
      ) /
        total_grade_distance
    } else {
      NA_real_
    },
    p95_absolute_grade_rate_per_m = finite_quantile(absolute_grade_rate, 0.95),
    maximum_absolute_grade_rate_per_m = safe_extreme(absolute_grade_rate, max),
    rms_grade_rate_per_m = if (sum(grade_rate_distance_m, na.rm = TRUE) > 0) {
      sqrt(stats::weighted.mean(
        grade_rate^2,
        grade_rate_distance_m,
        na.rm = TRUE
      ))
    } else {
      NA_real_
    },
    p95_absolute_elevation_step_m = finite_quantile(
      abs(elevation_step_m),
      0.95
    ),
    maximum_absolute_elevation_step_m = safe_extreme(
      abs(elevation_step_m),
      max
    ),
    mean_signed_displacement_from_H00_m = mean(displacement_m, na.rm = TRUE),
    p95_absolute_displacement_from_H00_m = finite_quantile(
      abs(displacement_m),
      0.95
    ),
    maximum_absolute_displacement_from_H00_m = safe_extreme(
      abs(displacement_m),
      max
    ),
    maximum_engineering_violation = maximum_engineering_violation[[1L]],
    stringsAsFactors = FALSE
  )
}

render_profile_result = function(case) {
  case$roads$evaluation_lanes = lane_count(case$roads)
  rayshader::plot_3d(
    rayshader::height_shade(case$dem),
    heightmap = case$dem,
    vertical_exaggeration = vertical_exaggeration,
    solid = FALSE,
    shadow = FALSE,
    window_size = c(400, 300)
  )
  on.exit(try(rgl::close3d(), silent = TRUE), add = TRUE)
  coordinates = rayshader::render_roads(
    case$roads,
    heightmap = case$dem,
    vertical_exaggeration = vertical_exaggeration,
    road_color = "#303236",
    lanes = evaluation_lanes,
    lane_width = 3.25,
    lane_texture = FALSE,
    layer = layer,
    layer_height = 5.5,
    merge = FALSE,
    clear_previous = TRUE,
    verbose = FALSE,
    parallel = parallel_mesh
  )
  road_ids = rayshader:::get_ids_with_labels(typeval = "road_path")$id
  list(
    coordinates = coordinates,
    path_info = lapply(road_ids, rayshader:::get_render_road_path_info),
    diagnostics = attr(coordinates, "profile_diagnostics")
  )
}

source_render_status = function(cell_id, scene_id) {
  role = if (cell_id == "H00") "baseline" else "candidate"
  rows = manifest[
    manifest$cell_id == cell_id &
      manifest$scene_id == scene_id &
      manifest$render_role == role,
    ,
    drop = FALSE
  ]
  if (!nrow(rows)) {
    return(list(succeeded = FALSE, error = "No source render rows."))
  }
  succeeded = all(rows$render_succeeded)
  error = unique(rows$error_message[!rows$render_succeeded])
  error = error[!is.na(error) & nzchar(error)]
  list(
    succeeded = succeeded,
    error = if (length(error)) paste(error, collapse = " | ") else NA_character_
  )
}

numeric_rows = list()
for (scene_id in selected_scenes) {
  message("Loading ", scene_id, "...")
  case = load_scene(scene_id)
  heightmap = rayshader::raster_to_matrix(case$dem)
  zscale = mean(terra::res(case$dem)) / vertical_exaggeration
  baseline_path = file.path(checkpoint_dir, paste0(scene_id, "-H00-result.rds"))
  baseline = if (file.exists(baseline_path) && !overwrite) {
    readRDS(baseline_path)
  } else {
    cell = design[design$cell_id == "H00", , drop = FALSE]
    options(
      rayshader.road_profile_screen_settings = list(
        maximum_grade_rate_relaxed = cell$maximum_grade_rate_relaxed,
        maximum_grade_relaxed = cell$maximum_grade_relaxed,
        continuation_grade_tolerance_relaxed = cell$continuation_grade_tolerance_relaxed,
        terrain_reference_weight_relaxed = cell$terrain_reference_weight_relaxed
      )
    )
    value = render_profile_result(case)
    saveRDS(value, baseline_path, compress = "xz")
    value
  }
  for (cell_id in selected_cells) {
    checkpoint_path = file.path(
      checkpoint_dir,
      paste0(scene_id, "-", cell_id, "-metrics.rds")
    )
    if (file.exists(checkpoint_path) && !overwrite) {
      numeric_rows[[paste(scene_id, cell_id)]] = readRDS(checkpoint_path)
      next
    }
    cell = design[design$cell_id == cell_id, , drop = FALSE]
    message("Auditing ", scene_id, " ", cell_id, "...")
    source_status = source_render_status(cell_id, scene_id)
    started = proc.time()[["elapsed"]]
    row = tryCatch(
      {
        options(
          rayshader.road_profile_screen_settings = list(
            maximum_grade_rate_relaxed = cell$maximum_grade_rate_relaxed,
            maximum_grade_relaxed = cell$maximum_grade_relaxed,
            continuation_grade_tolerance_relaxed = cell$continuation_grade_tolerance_relaxed,
            terrain_reference_weight_relaxed = cell$terrain_reference_weight_relaxed
          )
        )
        result = if (cell_id == "H00") baseline else render_profile_result(case)
        summary = summarize_result(result, baseline, heightmap, zscale)
        cbind(
          cell,
          scene_id = scene_id,
          numeric_succeeded = TRUE,
          numeric_error = NA_character_,
          source_render_succeeded = source_status$succeeded,
          source_render_error = source_status$error,
          numeric_runtime_seconds = proc.time()[["elapsed"]] - started,
          summary,
          stringsAsFactors = FALSE
        )
      },
      error = function(error) {
        cbind(
          cell,
          scene_id = scene_id,
          numeric_succeeded = FALSE,
          numeric_error = conditionMessage(error),
          source_render_succeeded = source_status$succeeded,
          source_render_error = source_status$error,
          numeric_runtime_seconds = proc.time()[["elapsed"]] - started,
          stringsAsFactors = FALSE
        )
      }
    )
    saveRDS(row, checkpoint_path)
    numeric_rows[[paste(scene_id, cell_id)]] = row
  }
}

all_columns = unique(unlist(lapply(numeric_rows, names), use.names = FALSE))
numeric_rows = lapply(numeric_rows, function(row) {
  missing = setdiff(all_columns, names(row))
  for (name in missing) {
    row[[name]] = NA
  }
  row[, all_columns, drop = FALSE]
})
numeric_metrics = do.call(rbind, numeric_rows)
rownames(numeric_metrics) = NULL
numeric_metrics$p50_absolute_grade_percent =
  100 * numeric_metrics$p50_absolute_grade
numeric_metrics$p95_absolute_grade_percent =
  100 * numeric_metrics$p95_absolute_grade
numeric_metrics$maximum_absolute_grade_percent =
  100 * numeric_metrics$maximum_absolute_grade
numeric_metrics$p95_absolute_grade_rate_percent_per_m =
  100 * numeric_metrics$p95_absolute_grade_rate_per_m
numeric_metrics$maximum_absolute_grade_rate_percent_per_m =
  100 * numeric_metrics$maximum_absolute_grade_rate_per_m
numeric_path = file.path(evaluation_dir, "numeric-profile-metrics.csv")
utils::write.csv(numeric_metrics, numeric_path, row.names = FALSE)

visual_ratings_path = file.path(evaluation_dir, "visual-ratings.csv")
if (file.exists(visual_ratings_path)) {
  visual_ratings = utils::read.csv(
    visual_ratings_path,
    stringsAsFactors = FALSE
  )
} else {
  visual_ratings = data.frame(
    cell_id = design$cell_id,
    swiss_render_status = NA_character_,
    ground_penetration_improvement = NA_integer_,
    floating_improvement = NA_integer_,
    terrain_hugging_improvement = NA_integer_,
    houston_regression = NA_integer_,
    visual_confidence = NA_character_,
    visual_notes = NA_character_,
    inspected_image_count = NA_integer_,
    stringsAsFactors = FALSE
  )
  utils::write.csv(visual_ratings, visual_ratings_path, row.names = FALSE)
}

wide_metrics = reshape(
  numeric_metrics,
  idvar = "cell_id",
  timevar = "scene_id",
  direction = "wide"
)
cell_evaluation = merge(design, visual_ratings, by = "cell_id", all.x = TRUE)
cell_evaluation = merge(
  cell_evaluation,
  wide_metrics,
  by = "cell_id",
  all.x = TRUE
)
cell_evaluation = cell_evaluation[
  match(design$cell_id, cell_evaluation$cell_id),
]
cell_evaluation_path = file.path(
  evaluation_dir,
  "road-profile-cell-evaluation.csv"
)
utils::write.csv(cell_evaluation, cell_evaluation_path, row.names = FALSE)

summary_metrics = c(
  "numeric_succeeded",
  "source_render_succeeded",
  "minimum_clearance_m",
  "maximum_penetration_m",
  "approximate_penetration_area_m2",
  "p95_low_side_gap_m",
  "maximum_low_side_gap_m",
  "p50_absolute_grade_percent",
  "p95_absolute_grade_percent",
  "maximum_absolute_grade_percent",
  "fraction_length_above_7_percent_grade",
  "fraction_length_above_15_percent_grade",
  "p95_absolute_grade_rate_percent_per_m",
  "maximum_absolute_grade_rate_percent_per_m",
  "p95_absolute_elevation_step_m",
  "maximum_absolute_elevation_step_m",
  "mean_signed_displacement_from_H00_m",
  "p95_absolute_displacement_from_H00_m",
  "maximum_absolute_displacement_from_H00_m"
)
evaluation_summary = merge(
  design,
  visual_ratings,
  by = "cell_id",
  all.x = TRUE,
  sort = FALSE
)
for (scene_id in selected_scenes) {
  scene_rows = numeric_metrics[
    numeric_metrics$scene_id == scene_id,
    c("cell_id", summary_metrics),
    drop = FALSE
  ]
  names(scene_rows)[-1L] = paste(scene_id, names(scene_rows)[-1L], sep = ".")
  evaluation_summary = merge(
    evaluation_summary,
    scene_rows,
    by = "cell_id",
    all.x = TRUE,
    sort = FALSE
  )
}
evaluation_summary = evaluation_summary[
  match(design$cell_id, evaluation_summary$cell_id),
]
utils::write.csv(
  evaluation_summary,
  file.path(evaluation_dir, "road-profile-evaluation-summary.csv"),
  row.names = FALSE
)

swiss_metrics = numeric_metrics[
  numeric_metrics$scene_id == "swiss_switchback",
  c(
    "cell_id",
    "numeric_succeeded",
    "maximum_penetration_m",
    "approximate_penetration_area_m2",
    "p95_low_side_gap_m",
    "maximum_low_side_gap_m",
    "p95_absolute_grade",
    "maximum_absolute_grade",
    "p95_absolute_grade_rate_per_m",
    "maximum_absolute_elevation_step_m"
  ),
  drop = FALSE
]
factor_data = merge(design, visual_ratings, by = "cell_id", all.x = TRUE)
factor_data = merge(factor_data, swiss_metrics, by = "cell_id", all.x = TRUE)
factor_columns = c(
  "maximum_grade_rate_relaxed",
  "maximum_grade_relaxed",
  "continuation_grade_tolerance_relaxed",
  "terrain_reference_weight_relaxed"
)
factor_effects = do.call(
  rbind,
  lapply(factor_columns, function(factor_name) {
    factor_value = factor_data[[factor_name]]
    level_keys = unique(as.character(factor_value))
    do.call(
      rbind,
      lapply(level_keys, function(level_key) {
        rows = factor_data[
          as.character(factor_value) == level_key,
          ,
          drop = FALSE
        ]
        solved = rows$numeric_succeeded %in% TRUE
        data.frame(
          factor = factor_name,
          level = level_key,
          attempted = nrow(rows),
          solved = sum(solved),
          solve_rate = mean(solved),
          mean_ground_penetration_improvement = mean(
            rows$ground_penetration_improvement,
            na.rm = TRUE
          ),
          mean_floating_improvement = mean(
            rows$floating_improvement,
            na.rm = TRUE
          ),
          mean_terrain_hugging_improvement = mean(
            rows$terrain_hugging_improvement,
            na.rm = TRUE
          ),
          median_maximum_penetration_m = stats::median(
            rows$maximum_penetration_m[solved],
            na.rm = TRUE
          ),
          median_penetration_area_m2 = stats::median(
            rows$approximate_penetration_area_m2[solved],
            na.rm = TRUE
          ),
          median_p95_low_side_gap_m = stats::median(
            rows$p95_low_side_gap_m[solved],
            na.rm = TRUE
          ),
          median_maximum_low_side_gap_m = stats::median(
            rows$maximum_low_side_gap_m[solved],
            na.rm = TRUE
          ),
          median_p95_absolute_grade_percent = 100 *
            stats::median(
              rows$p95_absolute_grade[solved],
              na.rm = TRUE
            ),
          median_maximum_absolute_grade_percent = 100 *
            stats::median(
              rows$maximum_absolute_grade[solved],
              na.rm = TRUE
            ),
          median_p95_grade_rate_percent_per_m = 100 *
            stats::median(
              rows$p95_absolute_grade_rate_per_m[solved],
              na.rm = TRUE
            ),
          median_maximum_elevation_step_m = stats::median(
            rows$maximum_absolute_elevation_step_m[solved],
            na.rm = TRUE
          ),
          stringsAsFactors = FALSE
        )
      })
    )
  })
)
utils::write.csv(
  factor_effects,
  file.path(evaluation_dir, "road-profile-factor-effects.csv"),
  row.names = FALSE
)

rating_guide = data.frame(
  field = c(
    "ground_penetration_improvement",
    "floating_improvement",
    "terrain_hugging_improvement",
    "houston_regression"
  ),
  minus_two = c(
    "Materially more terrain piercing",
    "Materially more unsupported floating",
    "Materially less terrain conforming",
    "Material Houston regression"
  ),
  minus_one = c(
    "Slightly more terrain piercing",
    "Slightly more unsupported floating",
    "Slightly less terrain conforming",
    "Slight Houston regression"
  ),
  zero = c(
    "No visible change",
    "No visible change",
    "No visible change",
    "No visible Houston regression"
  ),
  plus_one = c(
    "Slightly less terrain piercing",
    "Slightly less unsupported floating",
    "Slightly better terrain conforming",
    "Slight Houston improvement"
  ),
  plus_two = c(
    "Materially less terrain piercing",
    "Materially less unsupported floating",
    "Materially better terrain conforming",
    "Material Houston improvement"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  rating_guide,
  file.path(evaluation_dir, "visual-rating-guide.csv"),
  row.names = FALSE
)

metric_guide = data.frame(
  field = c(
    "minimum_clearance_m",
    "maximum_penetration_m",
    "approximate_penetration_area_m2",
    "p95_low_side_gap_m",
    "maximum_low_side_gap_m",
    "p50_absolute_grade",
    "p95_absolute_grade",
    "maximum_absolute_grade",
    "fraction_length_above_7_percent_grade",
    "fraction_length_above_15_percent_grade",
    "p95_absolute_grade_rate_per_m",
    "maximum_absolute_grade_rate_per_m",
    "p95_absolute_elevation_step_m",
    "maximum_absolute_elevation_step_m",
    "mean_signed_displacement_from_H00_m",
    "p95_absolute_displacement_from_H00_m",
    "maximum_absolute_displacement_from_H00_m"
  ),
  meaning = c(
    "Most negative road-surface clearance; negative values penetrate terrain.",
    "Deepest sampled terrain penetration across the road width.",
    "Approximate plan area of road samples lying below terrain.",
    "95th percentile of the highest cross-road clearance at each station.",
    "Largest highest-edge clearance at any road station.",
    "Median absolute longitudinal grade.",
    "95th percentile absolute longitudinal grade.",
    "Largest absolute longitudinal grade.",
    "Distance-weighted share of road exceeding 7 percent grade by more than a 1e-6 numerical tolerance.",
    "Distance-weighted share of road exceeding 15 percent grade by more than a 1e-6 numerical tolerance.",
    "95th percentile absolute change in grade per metre.",
    "Largest absolute change in grade per metre.",
    "95th percentile elevation change between consecutive mesh stations.",
    "Largest elevation change between consecutive mesh stations.",
    "Mean signed elevation shift relative to H00; positive means higher.",
    "95th percentile absolute elevation shift relative to H00.",
    "Largest absolute elevation shift relative to H00."
  ),
  preferred_direction = c(
    "toward zero from below",
    "lower",
    "lower",
    "lower outside intentional structures",
    "lower outside intentional structures",
    "context dependent",
    "lower",
    "lower",
    "lower",
    "lower",
    "lower",
    "lower",
    "lower",
    "lower",
    "context dependent",
    "lower",
    "lower"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metric_guide,
  file.path(evaluation_dir, "numeric-metric-guide.csv"),
  row.names = FALSE
)

methodology = data.frame(
  section = c(
    "visual baseline",
    "visual evidence",
    "visual score meaning",
    "mixed terrain-hugging result",
    "numeric terrain clearance",
    "numeric vertical-profile damage",
    "failed solves"
  ),
  details = c(
    "All ordinal visual scores compare the candidate with H00; zero means no visible improvement, not that H00 is acceptable.",
    "Ratings use the fixed-camera path-traced baseline, candidate, amplified-difference, and overlay evidence. Numerical metrics supplement rather than replace scene inspection.",
    "Scores range from -2 (material regression) through 0 (no material change) to +2 (material improvement).",
    "Terrain-hugging remains zero when a candidate visibly lowers unsupported spans but simultaneously drives other road sections through terrain.",
    "Clearance is sampled at nine positions across every road station. Negative clearance is penetration; positive low-side clearance exposes unsupported road edges.",
    "Longitudinal damage is reported with grade percentiles/maxima, length above grade thresholds, grade-rate percentiles/maxima, elevation steps, and displacement from H00.",
    "A failed solve receives no Swiss visual score and remains explicitly marked blocked_before_render."
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  methodology,
  file.path(evaluation_dir, "evaluation-methodology.csv"),
  row.names = FALSE
)

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    list(
      methodology = methodology,
      summary = evaluation_summary,
      factor_effects = factor_effects,
      cell_evaluation = cell_evaluation,
      visual_ratings = visual_ratings,
      numeric_by_scene = numeric_metrics,
      factor_design = design,
      rating_guide = rating_guide,
      metric_guide = metric_guide
    ),
    file.path(evaluation_dir, "road-profile-evaluation.xlsx")
  )
} else {
  message("Install `writexl` to also produce road-profile-evaluation.xlsx.")
}

message("Evaluation written to ", normalizePath(evaluation_dir, winslash = "/"))
