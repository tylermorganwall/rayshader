# Full visual sensitivity screen for terrain-adaptive road-profile settings.
#
# Run this script from the rayshader repository root. It loads the working
# package once with pkgload::load_all(), then changes the four experimental
# profile settings at runtime. No package installation is performed between
# factorial cells.
#
# The default campaign renders all 81 cells with fixed Swiss, Houston, Houston
# overpass, and synthetic cameras. It is resumable: successful renders are
# skipped unless RAYSHADER_SCREEN_OVERWRITE=true.

repo_root = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run this script from the rayshader repository root.", call. = FALSE)
}

screen_input = function(name, default) {
  Sys.getenv(name, unset = as.character(default))
}

screen_flag = function(name, default = FALSE) {
  value = tolower(screen_input(name, if (default) "true" else "false"))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(sprintf("%s must be true or false.", name), call. = FALSE)
  }
  value %in% c("true", "1", "yes")
}

screen_integer = function(name, default, minimum = 1L) {
  value = suppressWarnings(as.integer(screen_input(name, default)))
  if (is.na(value) || value < minimum) {
    stop(sprintf("%s must be an integer >= %d.", name, minimum), call. = FALSE)
  }
  value
}

screen_number = function(name, default, minimum = -Inf) {
  value = suppressWarnings(as.numeric(screen_input(name, default)))
  if (!is.finite(value) || value < minimum) {
    stop(sprintf("%s must be a finite number >= %s.", name, minimum))
  }
  value
}

screen_levels = function(
  name,
  default,
  allow_zero = FALSE,
  allow_infinite = FALSE
) {
  values = suppressWarnings(as.numeric(strsplit(
    screen_input(name, paste(default, collapse = ",")),
    ",",
    fixed = TRUE
  )[[1L]]))
  invalid = is.na(values) |
    (!allow_infinite & !is.finite(values)) |
    if (allow_zero) values < 0 else values <= 0
  if (length(values) != 3L || any(invalid)) {
    qualifier = if (allow_zero) "nonnegative" else "positive"
    finite = if (allow_infinite) "" else " finite"
    stop(sprintf(
      "%s must contain three%s %s comma-separated numbers.",
      name,
      finite,
      qualifier
    ))
  }
  values
}

default_input_candidates = c(
  file.path(
    "/private/tmp",
    "road-profile-evidence",
    "campaign-20260815",
    "inputs"
  ),
  file.path(repo_root, "road-profile-screen-inputs")
)
existing_input = default_input_candidates[dir.exists(default_input_candidates)]
default_input_dir = if (length(existing_input)) {
  existing_input[[1L]]
} else {
  default_input_candidates[[2L]]
}

config = list(
  input_dir = screen_input("RAYSHADER_SCREEN_INPUT_DIR", default_input_dir),
  output_dir = screen_input(
    "RAYSHADER_SCREEN_OUTPUT_DIR",
    file.path(repo_root, "road-profile-visual-screen")
  ),
  cells = screen_input("RAYSHADER_SCREEN_CELLS", "all"),
  scenes = screen_input(
    "RAYSHADER_SCREEN_SCENES",
    paste(
      c(
        "swiss_switchback",
        "houston_interchange",
        "houston_overpass",
        "synthetic_cross_slope"
      ),
      collapse = ","
    )
  ),
  relaxation_mode = tolower(screen_input(
    "RAYSHADER_SCREEN_RELAXATION_MODE",
    "smooth"
  )),
  samples = screen_integer("RAYSHADER_SCREEN_SAMPLES", 8L),
  width = screen_integer("RAYSHADER_SCREEN_WIDTH", 1200L, minimum = 100L),
  height = screen_integer("RAYSHADER_SCREEN_HEIGHT", 800L, minimum = 100L),
  vertical_exaggeration = screen_number(
    "RAYSHADER_SCREEN_VERTICAL_EXAGGERATION",
    2,
    minimum = 0
  ),
  seed = screen_integer("RAYSHADER_SCREEN_SEED", 20260816L, minimum = 0L),
  overwrite = screen_flag("RAYSHADER_SCREEN_OVERWRITE"),
  verbose = screen_flag("RAYSHADER_SCREEN_VERBOSE", TRUE),
  parallel_mesh = screen_flag("RAYSHADER_SCREEN_PARALLEL_MESH"),
  parallel_render = screen_flag("RAYSHADER_SCREEN_PARALLEL_RENDER", TRUE),
  dry_run = screen_flag("RAYSHADER_SCREEN_DRY_RUN")
)
if (config$vertical_exaggeration == 0) {
  stop(
    "RAYSHADER_SCREEN_VERTICAL_EXAGGERATION must be greater than zero.",
    call. = FALSE
  )
}
if (!config$relaxation_mode %in% c("smooth", "global")) {
  stop(
    "RAYSHADER_SCREEN_RELAXATION_MODE must be `smooth` or `global`.",
    call. = FALSE
  )
}

factor_levels = list(
  grade_rate = screen_levels(
    "RAYSHADER_SCREEN_GRADE_RATE_LEVELS",
    c(0.001, 0.005, 0.02)
  ),
  maximum_grade = screen_levels(
    "RAYSHADER_SCREEN_MAXIMUM_GRADE_LEVELS",
    c(0.07, 0.15, Inf),
    allow_infinite = TRUE
  ),
  continuation = screen_levels(
    "RAYSHADER_SCREEN_CONTINUATION_LEVELS",
    c(0, 0.03, 0.14),
    allow_zero = TRUE
  ),
  terrain_reference = screen_levels(
    "RAYSHADER_SCREEN_TERRAIN_REFERENCE_LEVELS",
    c(0.001, 0.03, 1),
    allow_zero = TRUE
  )
)

required_packages = c("pkgload", "sf", "terra", "magick", "rgl")
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
    options(rayshader.road_profile_screen_relaxation = NULL)
    try(rgl::close3d(), silent = TRUE)
    if (is.na(old_null)) {
      Sys.unsetenv("RGL_USE_NULL")
    } else {
      Sys.setenv(RGL_USE_NULL = old_null)
    }
  },
  add = TRUE
)

# This compiles and loads the working checkout once. Factor changes below are
# ordinary runtime option changes and never reinstall the package.
pkgload::load_all(repo_root, quiet = TRUE)
options(
  rayshader.road_profile_screen_relaxation = if (
    identical(config$relaxation_mode, "global")
  ) {
    1
  } else {
    NULL
  }
)
if (
  !exists(
    "resolve_render_road_profile_screen_settings",
    envir = asNamespace("rayshader"),
    inherits = FALSE
  )
) {
  stop(
    "The working checkout does not contain the road-profile screen hooks.",
    call. = FALSE
  )
}

file_hash = function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  unname(tools::md5sum(path)[[1L]])
}

implementation_files = file.path(
  repo_root,
  c(
    "R/render_roads.R",
    "src/road_profile.cpp",
    "tools/run-road-profile-visual-screen.R"
  )
)
implementation_fingerprint = paste(
  basename(implementation_files),
  vapply(implementation_files, file_hash, character(1)),
  sep = "=",
  collapse = ";"
)
git_head = tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(error) NA_character_
)

design = expand.grid(
  grade_rate_level = 0:2,
  maximum_grade_level = 0:2,
  continuation_level = 0:2,
  terrain_level = 0:2,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
design = design[
  order(
    rowSums(design),
    design$grade_rate_level,
    design$maximum_grade_level,
    design$continuation_level,
    design$terrain_level
  ),
  ,
  drop = FALSE
]
design$cell_id = sprintf("H%02d", seq_len(nrow(design)) - 1L)
design$maximum_grade_rate_relaxed = factor_levels$grade_rate[
  design$grade_rate_level + 1L
]
design$maximum_grade_relaxed = factor_levels$maximum_grade[
  design$maximum_grade_level + 1L
]
design$continuation_grade_tolerance_relaxed = factor_levels$continuation[
  design$continuation_level + 1L
]
design$terrain_reference_weight_relaxed = factor_levels$terrain_reference[
  design$terrain_level + 1L
]
design = design[, c(
  "cell_id",
  "grade_rate_level",
  "maximum_grade_rate_relaxed",
  "maximum_grade_level",
  "maximum_grade_relaxed",
  "continuation_level",
  "continuation_grade_tolerance_relaxed",
  "terrain_level",
  "terrain_reference_weight_relaxed"
)]

parse_selection = function(value, allowed, label) {
  if (identical(tolower(trimws(value)), "all")) {
    return(allowed)
  }
  selected = unique(trimws(strsplit(value, ",", fixed = TRUE)[[1L]]))
  unknown = setdiff(selected, allowed)
  if (length(unknown)) {
    stop(sprintf("Unknown %s: %s.", label, paste(unknown, collapse = ", ")))
  }
  selected
}

selected_cells = parse_selection(config$cells, design$cell_id, "cell(s)")
if (!"H00" %in% selected_cells) {
  selected_cells = c("H00", selected_cells)
}

scene_manifest = data.frame(
  scene_id = c(
    "swiss_switchback",
    "houston_interchange",
    "houston_overpass",
    "synthetic_cross_slope"
  ),
  case_id = c("swiss", "houston", "houston_overpass", "synthetic"),
  center_x = c(-450, 50, NA, NA),
  center_z = c(-297, 110, NA, NA),
  half_width_cells = c(120, 90, NA, NA),
  half_height_cells = c(120, 80, NA, NA),
  stringsAsFactors = FALSE
)
selected_scenes = parse_selection(
  config$scenes,
  scene_manifest$scene_id,
  "scene(s)"
)

camera_manifest = rbind(
  data.frame(
    scene_id = rep(c("swiss_switchback", "houston_interchange"), each = 6L),
    camera_id = rep(
      c(
        "oblique_overview",
        "low_side",
        "high_side",
        "longitudinal",
        "reverse_longitudinal",
        "worst_station"
      ),
      times = 2L
    ),
    theta = rep(c(225, 270, 90, 180, 0, 225), times = 2L),
    phi = rep(c(35, 8, 18, 10, 10, 22), times = 2L),
    zoom = 0.5 * rep(c(0.62, 0.4, 0.4, 0.4, 0.4, 0.3), times = 2L),
    fov = rep(c(45, 24, 24, 24, 24, 20), times = 2L),
    stringsAsFactors = FALSE
  ),
  data.frame(
    scene_id = "houston_overpass",
    camera_id = c("oblique_overview", "low_side"),
    theta = c(225, 270),
    phi = c(35, 10),
    zoom = 0.5 * c(0.62, 0.42),
    fov = c(45, 24),
    stringsAsFactors = FALSE
  ),
  data.frame(
    scene_id = "synthetic_cross_slope",
    camera_id = c("oblique_overview", "defect_close"),
    theta = c(225, 250),
    phi = c(35, 16),
    zoom = 0.5 * c(0.62, 0.34),
    fov = c(45, 24),
    stringsAsFactors = FALSE
  )
)
camera_manifest$width = config$width
camera_manifest$height = config$height

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
design_path = file.path(config$output_dir, "factor-design.csv")
if (file.exists(design_path)) {
  previous_design = utils::read.csv(design_path, stringsAsFactors = FALSE)
  if (!isTRUE(all.equal(previous_design, design, check.attributes = FALSE))) {
    stop(
      paste(
        "The output directory contains a different factorial design.",
        "Use a new directory or set RAYSHADER_SCREEN_OVERWRITE=true."
      ),
      call. = FALSE
    )
  }
}
utils::write.csv(design, design_path, row.names = FALSE)
utils::write.csv(
  scene_manifest,
  file.path(config$output_dir, "scene-manifest.csv"),
  row.names = FALSE
)
utils::write.csv(
  camera_manifest,
  file.path(config$output_dir, "camera-manifest.csv"),
  row.names = FALSE
)

stable_config = config[setdiff(
  names(config),
  c(
    "cells",
    "scenes",
    "overwrite",
    "verbose",
    "parallel_mesh",
    "parallel_render",
    "dry_run"
  )
)]
stable_config$git_head = git_head
stable_config$implementation_fingerprint = implementation_fingerprint
configuration = data.frame(
  name = names(stable_config),
  value = vapply(stable_config, paste, character(1), collapse = ","),
  stringsAsFactors = FALSE
)
configuration_path = file.path(config$output_dir, "run-configuration.csv")
if (file.exists(configuration_path)) {
  previous_configuration = utils::read.csv(
    configuration_path,
    stringsAsFactors = FALSE
  )
  if (
    !isTRUE(all.equal(
      previous_configuration,
      configuration,
      check.attributes = FALSE
    ))
  ) {
    stop(
      paste(
        "The output directory was created with different render or implementation settings.",
        "Use a new RAYSHADER_SCREEN_OUTPUT_DIR so evidence from different runs is not mixed."
      ),
      call. = FALSE
    )
  }
}
utils::write.csv(
  configuration,
  configuration_path,
  row.names = FALSE
)
utils::write.csv(
  data.frame(
    selected_cells = config$cells,
    selected_scenes = config$scenes,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ),
  file.path(config$output_dir, "last-run-selection.csv"),
  row.names = FALSE
)

if (config$dry_run) {
  message("Dry run complete. Factor design: ", normalizePath(design_path))
  quit(save = "no", status = 0)
}

ffmpeg = Sys.which("ffmpeg")
if (!nzchar(ffmpeg)) {
  stop("Install `ffmpeg` to generate the required flip MP4s.", call. = FALSE)
}

real_cases = unique(scene_manifest$case_id[
  scene_manifest$scene_id %in%
    selected_scenes &
    scene_manifest$case_id != "synthetic"
])
missing_inputs = character()
for (case_id in real_cases) {
  case_dir = file.path(config$input_dir, case_id)
  required = file.path(case_dir, c("roads.rds", "dem.tif"))
  missing_inputs = c(missing_inputs, required[!file.exists(required)])
}
if (length(missing_inputs)) {
  stop(
    paste(
      "Missing frozen visual-screen input(s):",
      paste(missing_inputs, collapse = ", "),
      "Set RAYSHADER_SCREEN_INPUT_DIR to the campaign input directory."
    ),
    call. = FALSE
  )
}

manifest_path = file.path(config$output_dir, "render-manifest.csv")
empty_manifest = data.frame(
  cell_id = character(),
  scene_id = character(),
  camera_id = character(),
  render_role = character(),
  maximum_grade_rate_relaxed = numeric(),
  maximum_grade_relaxed = numeric(),
  continuation_grade_tolerance_relaxed = numeric(),
  terrain_reference_weight_relaxed = numeric(),
  renderer_function = character(),
  renderer_quality = character(),
  width = integer(),
  height = integer(),
  vertical_exaggeration = numeric(),
  renderer_seed = integer(),
  implementation_fingerprint = character(),
  image_path = character(),
  image_hash = character(),
  road_mesh_fingerprint = character(),
  road_mesh_count = integer(),
  road_mesh_vertex_count = integer(),
  road_mesh_triangle_count = integer(),
  baseline_mesh_fingerprint = character(),
  mesh_changed_from_baseline = logical(),
  render_succeeded = logical(),
  status = character(),
  error_message = character(),
  completed_at = character(),
  stringsAsFactors = FALSE
)
manifest = if (file.exists(manifest_path)) {
  utils::read.csv(manifest_path, stringsAsFactors = FALSE)
} else {
  empty_manifest
}

write_manifest = function() {
  utils::write.csv(manifest, manifest_path, row.names = FALSE)
}

record_manifest = function(row) {
  key_match = manifest$cell_id == row$cell_id &
    manifest$scene_id == row$scene_id &
    manifest$camera_id == row$camera_id &
    manifest$render_role == row$render_role
  manifest <<- rbind(manifest[!key_match, , drop = FALSE], row)
  write_manifest()
}

manifest_row = function(
  cell,
  scene_id,
  camera_id,
  role,
  path = NA_character_,
  mesh = NULL,
  baseline_mesh_fingerprint = NA_character_,
  succeeded = TRUE,
  status = "rendered",
  error_message = NA_character_,
  renderer_function = "render_highquality"
) {
  if (is.null(mesh)) {
    mesh = list(
      fingerprint = NA_character_,
      count = NA_integer_,
      vertex_count = NA_integer_,
      triangle_count = NA_integer_
    )
  }
  data.frame(
    cell_id = cell$cell_id,
    scene_id = scene_id,
    camera_id = camera_id,
    render_role = role,
    maximum_grade_rate_relaxed = cell$maximum_grade_rate_relaxed,
    maximum_grade_relaxed = cell$maximum_grade_relaxed,
    continuation_grade_tolerance_relaxed = cell$continuation_grade_tolerance_relaxed,
    terrain_reference_weight_relaxed = cell$terrain_reference_weight_relaxed,
    renderer_function = renderer_function,
    renderer_quality = sprintf("pathtraced_samples_%d", config$samples),
    width = config$width,
    height = config$height,
    vertical_exaggeration = config$vertical_exaggeration,
    renderer_seed = config$seed,
    implementation_fingerprint = implementation_fingerprint,
    image_path = if (is.na(path)) {
      NA_character_
    } else {
      normalizePath(
        path,
        winslash = "/",
        mustWork = FALSE
      )
    },
    image_hash = if (is.na(path)) NA_character_ else file_hash(path),
    road_mesh_fingerprint = mesh$fingerprint,
    road_mesh_count = mesh$count,
    road_mesh_vertex_count = mesh$vertex_count,
    road_mesh_triangle_count = mesh$triangle_count,
    baseline_mesh_fingerprint = baseline_mesh_fingerprint,
    mesh_changed_from_baseline = if (
      is.na(baseline_mesh_fingerprint) || is.na(mesh$fingerprint)
    ) {
      NA
    } else {
      !identical(mesh$fingerprint, baseline_mesh_fingerprint)
    },
    render_succeeded = succeeded,
    status = status,
    error_message = error_message,
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
}

completed_render = function(cell_id, scene_id, camera_id, role) {
  row = manifest[
    manifest$cell_id == cell_id &
      manifest$scene_id == scene_id &
      manifest$camera_id == camera_id &
      manifest$render_role == role,
    ,
    drop = FALSE
  ]
  nrow(row) == 1L &&
    isTRUE(row$render_succeeded) &&
    !is.na(row$image_path) &&
    file.exists(row$image_path) &&
    identical(file_hash(row$image_path), row$image_hash)
}

load_case = function(case_id) {
  list(
    case_id = case_id,
    roads = readRDS(file.path(config$input_dir, case_id, "roads.rds")),
    dem = terra::rast(file.path(config$input_dir, case_id, "dem.tif"))
  )
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

crop_case = function(case, scene) {
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

make_synthetic_case = function() {
  dem = terra::rast(
    nrows = 161,
    ncols = 161,
    xmin = 0,
    xmax = 1600,
    ymin = 0,
    ymax = 1600,
    crs = "EPSG:32615"
  )
  xy = terra::xyFromCell(dem, seq_len(terra::ncell(dem)))
  elevation = 120 + 0.12 * xy[, 2L] + 8 * sin(xy[, 1L] / 150)
  terra::values(dem) = elevation
  names(dem) = "elevation_m"
  line_xy = cbind(
    seq(150, 1450, length.out = 17L),
    800 + 230 * sin(seq(-pi, pi, length.out = 17L))
  )
  roads = sf::st_sf(
    highway = "primary",
    lanes = "2",
    `lanes:forward` = NA_character_,
    `lanes:backward` = NA_character_,
    oneway = "no",
    bridge = "no",
    tunnel = "no",
    layer = "0",
    geometry = sf::st_sfc(
      sf::st_linestring(line_xy),
      crs = sf::st_crs(32615)
    )
  )
  list(case_id = "synthetic", roads = roads, dem = dem)
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

load_scene = function(scene_id) {
  scene = scene_manifest[scene_manifest$scene_id == scene_id, , drop = FALSE]
  if (identical(scene$case_id, "synthetic")) {
    return(make_synthetic_case())
  }
  crop_case(load_case(scene$case_id), scene)
}

mesh_fingerprint = function() {
  meshes = rayshader:::get_scene_road_meshes(default = list())
  temporary = tempfile(fileext = ".rds")
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(meshes, temporary, version = 3)
  list(
    fingerprint = file_hash(temporary),
    count = length(meshes),
    vertex_count = sum(vapply(
      meshes,
      function(mesh) if (is.null(mesh$vb)) 0L else ncol(mesh$vb),
      integer(1)
    )),
    triangle_count = sum(vapply(
      meshes,
      function(mesh) if (is.null(mesh$it)) 0L else ncol(mesh$it),
      integer(1)
    ))
  )
}

build_scene = function(case) {
  case$roads$screen_lanes = lane_count(case$roads)
  texture = rayshader::sphere_shade(
    case$dem,
    texture = "imhof2",
    vertical_exaggeration = config$vertical_exaggeration
  )
  rayshader::plot_3d(
    texture,
    heightmap = case$dem,
    vertical_exaggeration = config$vertical_exaggeration,
    solid = FALSE,
    shadow = TRUE,
    background = "white",
    window_size = c(config$width, config$height)
  )
  rayshader::render_roads(
    case$roads,
    heightmap = case$dem,
    vertical_exaggeration = config$vertical_exaggeration,
    road_color = "#303236",
    lanes = screen_lanes,
    lane_width = 3.25,
    lane_texture = !grepl("^houston", case$case_id),
    layer = layer,
    layer_height = 5.5,
    merge = FALSE,
    clear_previous = TRUE,
    verbose = config$verbose,
    parallel = config$parallel_mesh
  )
  mesh_fingerprint()
}

render_camera = function(camera, filename, reset_scene_cache) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  rayshader::render_camera(
    theta = camera$theta,
    phi = camera$phi,
    zoom = camera$zoom,
    fov = camera$fov
  )
  set.seed(config$seed)
  rayshader::render_highquality(
    filename = filename,
    samples = config$samples,
    sample_method = "sobol_blue",
    min_variance = 0,
    light = TRUE,
    light_direction = 315,
    light_altitude = 45,
    light_intensity = 800,
    light_color = "white",
    cache_scene = TRUE,
    reset_scene_cache = reset_scene_cache,
    width = config$width,
    height = config$height,
    clear = FALSE,
    clamp_value = 1000,
    plot = FALSE,
    verbose = config$verbose,
    parallel = config$parallel_render
  )
  filename
}

create_differences = function(baseline_path, candidate_path, output_prefix) {
  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
  baseline = magick::image_read(baseline_path)
  candidate = magick::image_read(candidate_path)
  difference = magick::image_composite(
    baseline,
    candidate,
    operator = "difference"
  )
  absolute_path = paste0(output_prefix, "-absolute.png")
  amplified_path = paste0(output_prefix, "-amplified.png")
  overlay_path = paste0(output_prefix, "-overlay.png")
  magick::image_write(difference, absolute_path)
  amplified = magick::image_fx(difference, expression = "min(1,u*32)")
  gray = magick::image_convert(amplified, colorspace = "gray")
  info = magick::image_info(gray)
  black = magick::image_blank(info$width, info$height, color = "black")
  magenta = magick::image_combine(c(gray, black, gray), colorspace = "sRGB")
  magick::image_write(magenta, amplified_path)
  overlay = magick::image_composite(candidate, magenta, operator = "screen")
  magick::image_write(overlay, overlay_path)
  c(
    difference = absolute_path,
    amplified_difference = amplified_path,
    diagnostic_overlay = overlay_path
  )
}

label_frame = function(path, label, color, output_path) {
  image = magick::image_read(path)
  image = magick::image_annotate(
    image,
    label,
    gravity = "northwest",
    location = "+20+20",
    size = 34,
    color = color,
    boxcolor = "#000000CC",
    weight = 700
  )
  magick::image_write(image, output_path, format = "png")
}

quote_concat_path = function(path) {
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  paste0("'", gsub("'", "'\\''", path, fixed = TRUE), "'")
}

create_flip_video = function(
  baseline_path,
  candidate_path,
  difference_path,
  output_path,
  label
) {
  temporary = tempfile("road-profile-visual-flip-")
  dir.create(temporary, recursive = TRUE)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  frames = file.path(
    temporary,
    c("baseline.png", "candidate.png", "difference.png")
  )
  label_frame(
    baseline_path,
    paste("BASELINE H00", label),
    "#00E5FF",
    frames[[1L]]
  )
  label_frame(
    candidate_path,
    paste("CANDIDATE", label),
    "#FF00FF",
    frames[[2L]]
  )
  label_frame(
    difference_path,
    paste("32X MAGENTA DIFFERENCE", label),
    "#FF00FF",
    frames[[3L]]
  )
  concat_path = file.path(temporary, "frames.txt")
  writeLines(
    c(
      paste("file", quote_concat_path(frames[[1L]])),
      "duration 0.5",
      paste("file", quote_concat_path(frames[[2L]])),
      "duration 0.5",
      paste("file", quote_concat_path(frames[[1L]])),
      "duration 0.5",
      paste("file", quote_concat_path(frames[[2L]])),
      "duration 0.5",
      paste("file", quote_concat_path(frames[[3L]])),
      "duration 1",
      paste("file", quote_concat_path(frames[[3L]]))
    ),
    concat_path
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  output = system2(
    ffmpeg,
    c(
      "-hide_banner",
      "-loglevel",
      "error",
      "-y",
      "-f",
      "concat",
      "-safe",
      "0",
      "-i",
      shQuote(concat_path),
      "-vf",
      "fps=6,format=yuv420p",
      "-an",
      "-c:v",
      "libx264",
      "-preset",
      "veryfast",
      "-crf",
      "20",
      "-threads",
      "1",
      "-map_metadata",
      "-1",
      "-movflags",
      "+faststart",
      shQuote(output_path)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status = attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(paste(output, collapse = "\n"), call. = FALSE)
  }
  output_path
}

create_contact_sheet = function(
  baseline_path,
  candidate_path,
  amplified_path,
  overlay_path,
  output_path,
  label
) {
  paths = c(baseline_path, candidate_path, amplified_path, overlay_path)
  labels = c("BASELINE H00", "CANDIDATE", "32X MAGENTA DIFF", "OVERLAY")
  panels = Map(
    function(path, panel_label) {
      magick::image_annotate(
        magick::image_read(path),
        paste(panel_label, label),
        gravity = "northwest",
        location = "+20+20",
        size = 30,
        color = "white",
        boxcolor = "#000000AA"
      )
    },
    paths,
    labels
  )
  top = magick::image_append(do.call(c, panels[1:2]), stack = FALSE)
  bottom = magick::image_append(do.call(c, panels[3:4]), stack = FALSE)
  sheet = magick::image_append(c(top, bottom), stack = TRUE)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  magick::image_write(sheet, output_path)
  output_path
}

relative_path = function(path) {
  normalized_root = paste0(
    normalizePath(config$output_dir, winslash = "/"),
    "/"
  )
  sub(normalized_root, "", normalizePath(path, winslash = "/"), fixed = TRUE)
}

html_escape = function(value) {
  value = gsub("&", "&amp;", value, fixed = TRUE)
  value = gsub("<", "&lt;", value, fixed = TRUE)
  value = gsub(">", "&gt;", value, fixed = TRUE)
  value = gsub('"', "&quot;", value, fixed = TRUE)
  value
}

write_cell_gallery = function(cell) {
  role = if (identical(cell$cell_id, "H00")) "baseline" else "candidate"
  rows = manifest[
    manifest$cell_id == cell$cell_id & manifest$render_role == role,
    ,
    drop = FALSE
  ]
  cards = vapply(
    seq_len(nrow(rows)),
    function(index) {
      row = rows[index, , drop = FALSE]
      camera = row$camera_id
      scene = row$scene_id
      contact = file.path(
        config$output_dir,
        "contact-sheets",
        cell$cell_id,
        scene,
        paste0(camera, ".png")
      )
      video = file.path(
        config$output_dir,
        "videos",
        cell$cell_id,
        scene,
        paste0(camera, ".mp4")
      )
      if (!isTRUE(row$render_succeeded)) {
        return(sprintf(
          '<section class="failed"><h2>%s — %s</h2><p>%s</p></section>',
          html_escape(scene),
          html_escape(camera),
          html_escape(row$error_message)
        ))
      }
      if (identical(role, "baseline")) {
        source = relative_path(row$image_path)
        return(sprintf(
          paste0(
            '<section><h2>%s — %s</h2>',
            '<p>Road mesh fingerprint: %s</p>',
            '<a href="../%s"><img src="../%s" loading="lazy"></a></section>'
          ),
          html_escape(scene),
          html_escape(camera),
          html_escape(row$road_mesh_fingerprint),
          source,
          source
        ))
      }
      if (!file.exists(contact) || !file.exists(video)) {
        source = relative_path(row$image_path)
        return(sprintf(
          paste0(
            '<section class="failed"><h2>%s — %s</h2>',
            '<p>The candidate rendered, but its baseline comparison is unavailable.</p>',
            '<a href="../%s"><img src="../%s" loading="lazy"></a></section>'
          ),
          html_escape(scene),
          html_escape(camera),
          source,
          source
        ))
      }
      sprintf(
        paste0(
          '<section><h2>%s — %s</h2>',
          '<p>Road mesh changed from H00: %s; fingerprint: %s</p>',
          '<a href="../%s"><img src="../%s" loading="lazy"></a>',
          '<video controls preload="none" src="../%s"></video></section>'
        ),
        html_escape(scene),
        html_escape(camera),
        if (isTRUE(row$mesh_changed_from_baseline)) "yes" else "no",
        html_escape(row$road_mesh_fingerprint),
        relative_path(contact),
        relative_path(contact),
        relative_path(video)
      )
    },
    character(1)
  )
  page = c(
    "<!doctype html><html><head><meta charset='utf-8'>",
    sprintf("<title>%s visual sensitivity</title>", cell$cell_id),
    paste0(
      "<style>body{font-family:system-ui;margin:24px;background:#171717;color:#eee}",
      "a{color:#6cf}img,video{width:min(100%,1200px);display:block;margin:8px 0 24px}",
      "section{border-top:1px solid #555;padding-top:12px}.failed{color:#ff9a9a}</style>"
    ),
    "</head><body>",
    sprintf(
      "<p><a href='../index.html'>← campaign index</a></p><h1>%s</h1>",
      cell$cell_id
    ),
    sprintf(
      paste0(
        "<p>grade rate=%g; maximum grade=%g; continuation tolerance=%g; ",
        "terrain reference=%g</p>"
      ),
      cell$maximum_grade_rate_relaxed,
      cell$maximum_grade_relaxed,
      cell$continuation_grade_tolerance_relaxed,
      cell$terrain_reference_weight_relaxed
    ),
    cards,
    "</body></html>"
  )
  gallery_dir = file.path(config$output_dir, "gallery")
  dir.create(gallery_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(page, file.path(gallery_dir, paste0(cell$cell_id, ".html")))
}

write_sensitivity_galleries = function() {
  for (scene_id in scene_manifest$scene_id) {
    cameras = camera_manifest[
      camera_manifest$scene_id == scene_id,
      ,
      drop = FALSE
    ]
    scene_dir = file.path(config$output_dir, "sensitivity", scene_id)
    dir.create(scene_dir, recursive = TRUE, showWarnings = FALSE)
    for (camera_id in cameras$camera_id) {
      cards = vapply(
        seq_len(nrow(design)),
        function(index) {
          cell = design[index, , drop = FALSE]
          role = if (identical(cell$cell_id, "H00")) "baseline" else "candidate"
          row = manifest[
            manifest$cell_id == cell$cell_id &
              manifest$scene_id == scene_id &
              manifest$camera_id == camera_id &
              manifest$render_role == role,
            ,
            drop = FALSE
          ]
          heading = sprintf(
            paste0(
              "%s — rate %g; maximum grade %g; continuation %g; terrain %g"
            ),
            cell$cell_id,
            cell$maximum_grade_rate_relaxed,
            cell$maximum_grade_relaxed,
            cell$continuation_grade_tolerance_relaxed,
            cell$terrain_reference_weight_relaxed
          )
          if (!nrow(row)) {
            return(sprintf(
              '<section class="pending"><h2>%s</h2><p>Pending.</p></section>',
              html_escape(heading)
            ))
          }
          if (!isTRUE(row$render_succeeded)) {
            return(sprintf(
              '<section class="failed"><h2>%s</h2><p>%s</p></section>',
              html_escape(heading),
              html_escape(row$error_message)
            ))
          }
          source = paste0("../../", relative_path(row$image_path))
          if (identical(cell$cell_id, "H00")) {
            return(sprintf(
              paste0(
                '<section><h2>%s</h2><p>Road mesh fingerprint: %s</p><a href="%s">',
                '<img src="%s" loading="lazy"></a></section>'
              ),
              html_escape(heading),
              html_escape(row$road_mesh_fingerprint),
              source,
              source
            ))
          }
          overlay = file.path(
            config$output_dir,
            "renders",
            "differences",
            cell$cell_id,
            scene_id,
            paste0(camera_id, "-overlay.png")
          )
          video = file.path(
            config$output_dir,
            "videos",
            cell$cell_id,
            scene_id,
            paste0(camera_id, ".mp4")
          )
          comparison = if (file.exists(overlay) && file.exists(video)) {
            overlay_href = paste0("../../", relative_path(overlay))
            video_href = paste0("../../", relative_path(video))
            sprintf(
              paste0(
                '<a href="%s"><img src="%s" loading="lazy"></a>',
                '<p><a href="%s">Open baseline/candidate/difference flip MP4</a></p>'
              ),
              overlay_href,
              overlay_href,
              video_href
            )
          } else {
            "<p>Baseline comparison pending.</p>"
          }
          sprintf(
            paste0(
              '<section><h2>%s</h2>',
              '<p>Road mesh changed from H00: %s; fingerprint: %s</p>',
              '<div class="pair">',
              '<a href="%s"><img src="%s" loading="lazy"></a>%s</div></section>'
            ),
            html_escape(heading),
            if (isTRUE(row$mesh_changed_from_baseline)) "yes" else "no",
            html_escape(row$road_mesh_fingerprint),
            source,
            source,
            comparison
          )
        },
        character(1)
      )
      page = c(
        "<!doctype html><html><head><meta charset='utf-8'>",
        sprintf("<title>%s — %s sensitivity</title>", scene_id, camera_id),
        paste0(
          "<style>body{font-family:system-ui;margin:24px;background:#171717;color:#eee}",
          "a{color:#6cf}.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(520px,1fr));gap:18px}",
          "section{border:1px solid #555;padding:12px}.pair{display:grid;grid-template-columns:1fr 1fr;gap:8px}",
          "img{width:100%;height:auto}.failed{color:#ff9a9a}.pending{color:#aaa}</style>"
        ),
        "</head><body>",
        "<p><a href='../../index.html'>← campaign index</a></p>",
        sprintf(
          "<h1>%s — %s</h1>",
          html_escape(scene_id),
          html_escape(camera_id)
        ),
        paste(
          "<p>Each card shows the full candidate render and, when available,",
          "the candidate with the 32× magenta difference overlaid. Images link",
          "to native resolution; MP4s hard-flip against H00.</p>"
        ),
        "<div class='grid'>",
        cards,
        "</div></body></html>"
      )
      writeLines(page, file.path(scene_dir, paste0(camera_id, ".html")))
    }
  }
}

write_campaign_index = function() {
  rows = vapply(
    seq_len(nrow(design)),
    function(index) {
      cell = design[index, , drop = FALSE]
      cell_manifest = manifest[
        manifest$cell_id == cell$cell_id &
          manifest$render_role %in% c("baseline", "candidate"),
        ,
        drop = FALSE
      ]
      succeeded = sum(cell_manifest$render_succeeded, na.rm = TRUE)
      failed = sum(!cell_manifest$render_succeeded, na.rm = TRUE)
      sprintf(
        paste0(
          "<tr><td><a href='gallery/%s.html'>%s</a></td>",
          "<td>%g</td><td>%g</td><td>%g</td><td>%g</td>",
          "<td>%d</td><td>%d</td></tr>"
        ),
        cell$cell_id,
        cell$cell_id,
        cell$maximum_grade_rate_relaxed,
        cell$maximum_grade_relaxed,
        cell$continuation_grade_tolerance_relaxed,
        cell$terrain_reference_weight_relaxed,
        succeeded,
        failed
      )
    },
    character(1)
  )
  sensitivity_links = unlist(lapply(selected_scenes, function(scene_id) {
    camera_ids = camera_manifest$camera_id[camera_manifest$scene_id == scene_id]
    c(
      sprintf("<h3>%s</h3><ul>", html_escape(scene_id)),
      vapply(
        camera_ids,
        function(camera_id) {
          sprintf(
            "<li><a href='sensitivity/%s/%s.html'>%s</a></li>",
            scene_id,
            paste0(camera_id),
            html_escape(camera_id)
          )
        },
        character(1)
      ),
      "</ul>"
    )
  }))
  page = c(
    "<!doctype html><html><head><meta charset='utf-8'><title>Road-profile visual screen</title>",
    paste0(
      "<style>body{font-family:system-ui;margin:24px}table{border-collapse:collapse}",
      "th,td{border:1px solid #aaa;padding:7px 10px;text-align:right}",
      "th:first-child,td:first-child{text-align:left}</style></head><body>"
    ),
    "<h1>Road-profile visual sensitivity screen</h1>",
    paste(
      "<p>Every cell page contains native-resolution baseline/candidate/",
      "magenta-difference sheets and hard-flip MP4s. No visual verdict is",
      "assigned by this script.</p>"
    ),
    "<h2>Compare every factor cell at one fixed camera</h2>",
    sensitivity_links,
    "<h2>Per-cell evidence</h2>",
    paste0(
      "<table><thead><tr><th>Cell</th><th>Grade rate</th><th>Maximum grade</th>",
      "<th>Continuation</th><th>Terrain reference</th><th>Rendered</th>",
      "<th>Blocked/failed</th></tr></thead><tbody>"
    ),
    rows,
    "</tbody></table></body></html>"
  )
  writeLines(page, file.path(config$output_dir, "index.html"))
}

baseline_mesh_from_manifest = function(scene_id) {
  row = manifest[
    manifest$cell_id == "H00" &
      manifest$scene_id == scene_id &
      manifest$render_role == "baseline" &
      manifest$render_succeeded,
    ,
    drop = FALSE
  ]
  if (!nrow(row)) {
    return(NA_character_)
  }
  row$road_mesh_fingerprint[[1L]]
}

postprocess_error_path = file.path(config$output_dir, "postprocess-errors.csv")
record_postprocess_error = function(
  cell_id,
  scene_id,
  camera_id,
  error_message
) {
  row = data.frame(
    cell_id = cell_id,
    scene_id = scene_id,
    camera_id = camera_id,
    error_message = error_message,
    failed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
  existing = if (file.exists(postprocess_error_path)) {
    utils::read.csv(postprocess_error_path, stringsAsFactors = FALSE)
  } else {
    row[0, , drop = FALSE]
  }
  key_match = existing$cell_id == cell_id &
    existing$scene_id == scene_id &
    existing$camera_id == camera_id
  utils::write.csv(
    rbind(existing[!key_match, , drop = FALSE], row),
    postprocess_error_path,
    row.names = FALSE
  )
}

finalize_comparison = function(
  cell,
  scene_id,
  camera_id,
  candidate_path,
  mesh
) {
  baseline_path = file.path(
    config$output_dir,
    "renders",
    "baseline",
    "H00",
    scene_id,
    paste0(camera_id, ".png")
  )
  if (!file.exists(baseline_path)) {
    return(invisible(NULL))
  }
  prefix = file.path(
    config$output_dir,
    "renders",
    "differences",
    cell$cell_id,
    scene_id,
    camera_id
  )
  differences = create_differences(baseline_path, candidate_path, prefix)
  baseline_fingerprint = baseline_mesh_from_manifest(scene_id)
  for (role in names(differences)) {
    record_manifest(manifest_row(
      cell,
      scene_id,
      camera_id,
      role,
      differences[[role]],
      mesh,
      baseline_fingerprint,
      renderer_function = "magick post-processing"
    ))
  }
  label = paste(cell$cell_id, scene_id, camera_id, sep = " | ")
  video_path = file.path(
    config$output_dir,
    "videos",
    cell$cell_id,
    scene_id,
    paste0(camera_id, ".mp4")
  )
  create_flip_video(
    baseline_path,
    candidate_path,
    differences[["amplified_difference"]],
    video_path,
    label
  )
  contact_path = file.path(
    config$output_dir,
    "contact-sheets",
    cell$cell_id,
    scene_id,
    paste0(camera_id, ".png")
  )
  create_contact_sheet(
    baseline_path,
    candidate_path,
    differences[["amplified_difference"]],
    differences[["diagnostic_overlay"]],
    contact_path,
    label
  )
  invisible(NULL)
}

finalize_comparison_safely = function(
  cell,
  scene_id,
  camera_id,
  candidate_path,
  mesh
) {
  error_message = tryCatch(
    {
      finalize_comparison(cell, scene_id, camera_id, candidate_path, mesh)
      NULL
    },
    error = function(error) conditionMessage(error)
  )
  if (!is.null(error_message)) {
    record_postprocess_error(
      cell$cell_id,
      scene_id,
      camera_id,
      error_message
    )
    warning(
      sprintf(
        "Post-processing failed for %s / %s / %s: %s",
        cell$cell_id,
        scene_id,
        camera_id,
        error_message
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

selected_design = design[match(selected_cells, design$cell_id), , drop = FALSE]
inspection_path = file.path(config$output_dir, "visual-inspection.csv")
if (!file.exists(inspection_path)) {
  inspection_rows = merge(
    design[design$cell_id != "H00", , drop = FALSE],
    camera_manifest[, c("scene_id", "camera_id"), drop = FALSE],
    by = NULL
  )
  inspection_rows$opened_and_inspected = FALSE
  inspection_rows$ordinary_roads_float = NA_character_
  inspection_rows$terrain_pierces_road = NA_character_
  inspection_rows$unsupported_low_edge = NA_character_
  inspection_rows$walls_slabs_or_wedges = NA_character_
  inspection_rows$continuation_gaps = NA_character_
  inspection_rows$abrupt_grade_change = NA_character_
  inspection_rows$bridge_approach_plausible = NA_character_
  inspection_rows$bridge_clearance_maintained = NA_character_
  inspection_rows$unrelated_roads_changed = NA_character_
  inspection_rows$visual_verdict = NA_character_
  inspection_rows$notes = NA_character_
  utils::write.csv(inspection_rows, inspection_path, row.names = FALSE)
}
total_jobs = sum(vapply(
  selected_scenes,
  function(scene_id) {
    sum(camera_manifest$scene_id == scene_id)
  },
  integer(1)
)) *
  nrow(selected_design)
completed_jobs = 0L

for (cell_index in seq_len(nrow(selected_design))) {
  cell = selected_design[cell_index, , drop = FALSE]
  cell_settings = list(
    maximum_grade_relaxed = cell$maximum_grade_relaxed,
    maximum_grade_rate_relaxed = cell$maximum_grade_rate_relaxed,
    continuation_grade_tolerance_relaxed = cell$continuation_grade_tolerance_relaxed,
    terrain_reference_weight_relaxed = cell$terrain_reference_weight_relaxed
  )
  options(rayshader.road_profile_screen_settings = cell_settings)
  resolved_settings = rayshader:::resolve_render_road_profile_screen_settings()
  if (
    !isTRUE(all.equal(
      resolved_settings,
      cell_settings,
      check.attributes = FALSE
    ))
  ) {
    stop(
      sprintf(
        "Runtime package settings did not resolve for cell %s.",
        cell$cell_id
      ),
      call. = FALSE
    )
  }
  message(sprintf(
    paste0(
      "Cell %s (%d/%d): grade-rate=%g, maximum-grade=%g, ",
      "continuation=%g, terrain=%g"
    ),
    cell$cell_id,
    cell_index,
    nrow(selected_design),
    cell$maximum_grade_rate_relaxed,
    cell$maximum_grade_relaxed,
    cell$continuation_grade_tolerance_relaxed,
    cell$terrain_reference_weight_relaxed
  ))

  for (scene_id in selected_scenes) {
    cameras = camera_manifest[
      camera_manifest$scene_id == scene_id,
      ,
      drop = FALSE
    ]
    role = if (identical(cell$cell_id, "H00")) "baseline" else "candidate"
    camera_complete = vapply(
      cameras$camera_id,
      function(camera_id) {
        completed_render(cell$cell_id, scene_id, camera_id, role)
      },
      logical(1)
    )
    if (all(camera_complete) && !config$overwrite) {
      completed_jobs = completed_jobs + nrow(cameras)
      if (!identical(cell$cell_id, "H00")) {
        existing_mesh = manifest[
          manifest$cell_id == cell$cell_id &
            manifest$scene_id == scene_id &
            manifest$render_role == "candidate",
          ,
          drop = FALSE
        ][1L, ]
        mesh = list(
          fingerprint = existing_mesh$road_mesh_fingerprint,
          count = existing_mesh$road_mesh_count,
          vertex_count = existing_mesh$road_mesh_vertex_count,
          triangle_count = existing_mesh$road_mesh_triangle_count
        )
        for (camera_id in cameras$camera_id) {
          candidate_path = manifest$image_path[
            manifest$cell_id == cell$cell_id &
              manifest$scene_id == scene_id &
              manifest$camera_id == camera_id &
              manifest$render_role == "candidate"
          ][[1L]]
          finalize_comparison_safely(
            cell,
            scene_id,
            camera_id,
            candidate_path,
            mesh
          )
        }
      }
      next
    }

    scene_result = tryCatch(
      {
        case = load_scene(scene_id)
        mesh = build_scene(case)
        list(mesh = mesh, error = NULL)
      },
      error = function(error) {
        list(mesh = NULL, error = conditionMessage(error))
      }
    )
    if (!is.null(scene_result$error)) {
      for (camera_id in cameras$camera_id) {
        completed_jobs = completed_jobs + 1L
        record_manifest(manifest_row(
          cell,
          scene_id,
          camera_id,
          role,
          succeeded = FALSE,
          status = "blocked_before_render",
          error_message = scene_result$error
        ))
      }
      try(rgl::close3d(), silent = TRUE)
      next
    }

    mesh = scene_result$mesh
    baseline_fingerprint = if (identical(cell$cell_id, "H00")) {
      mesh$fingerprint
    } else {
      baseline_mesh_from_manifest(scene_id)
    }
    reset_scene_cache = TRUE
    for (camera_index in seq_len(nrow(cameras))) {
      camera = cameras[camera_index, , drop = FALSE]
      camera_id = camera$camera_id
      completed_jobs = completed_jobs + 1L
      if (
        !config$overwrite &&
          completed_render(cell$cell_id, scene_id, camera_id, role)
      ) {
        next
      }
      output_path = file.path(
        config$output_dir,
        "renders",
        role,
        cell$cell_id,
        scene_id,
        paste0(camera_id, ".png")
      )
      message(sprintf(
        "  [%d/%d] %s / %s",
        completed_jobs,
        total_jobs,
        scene_id,
        camera_id
      ))
      render_error = tryCatch(
        {
          render_camera(camera, output_path, reset_scene_cache)
          NULL
        },
        error = function(error) conditionMessage(error)
      )
      if (!is.null(render_error)) {
        record_manifest(manifest_row(
          cell,
          scene_id,
          camera_id,
          role,
          output_path,
          mesh,
          baseline_fingerprint,
          succeeded = FALSE,
          status = "render_failed",
          error_message = render_error
        ))
        next
      }
      reset_scene_cache = FALSE
      record_manifest(manifest_row(
        cell,
        scene_id,
        camera_id,
        role,
        output_path,
        mesh,
        baseline_fingerprint
      ))
      if (!identical(cell$cell_id, "H00")) {
        finalize_comparison_safely(
          cell,
          scene_id,
          camera_id,
          output_path,
          mesh
        )
      }
    }
    try(rgl::close3d(), silent = TRUE)
    gc()
  }
  write_cell_gallery(cell)
  write_sensitivity_galleries()
  write_campaign_index()
}

write_sensitivity_galleries()
write_campaign_index()
message(
  "Visual screen complete or checkpointed at: ",
  normalizePath(config$output_dir)
)
message(
  "Open the visual index: ",
  normalizePath(file.path(config$output_dir, "index.html"))
)
