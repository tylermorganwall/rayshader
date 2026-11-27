road_fuzz_env_integer = function(name, default, minimum = 0L) {
  value = suppressWarnings(as.integer(Sys.getenv(
    name,
    unset = as.character(default)
  )))
  if (length(value) != 1L || is.na(value) || value < minimum) {
    stop(
      sprintf(
        "%s must be an integer greater than or equal to %i.",
        name,
        minimum
      ),
      call. = FALSE
    )
  }
  value
}

road_fuzz_env_double = function(name, default, minimum = 0) {
  value = suppressWarnings(as.numeric(Sys.getenv(
    name,
    unset = as.character(default)
  )))
  if (length(value) != 1L || !is.finite(value) || value < minimum) {
    stop(
      sprintf(
        "%s must be a finite number greater than or equal to %s.",
        name,
        minimum
      ),
      call. = FALSE
    )
  }
  value
}

road_fuzz_env_flag = function(name, default = FALSE) {
  raw_value = tolower(Sys.getenv(
    name,
    unset = if (isTRUE(default)) "true" else "false"
  ))
  if (!raw_value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(sprintf("%s must be true or false.", name), call. = FALSE)
  }
  raw_value %in% c("true", "1", "yes")
}

road_fuzz_config = function(repo_root = getwd()) {
  random_cases = road_fuzz_env_integer(
    "RAYSHADER_ROAD_FUZZ_CASES",
    5L,
    minimum = 0L
  )
  min_bbox_m = road_fuzz_env_double(
    "RAYSHADER_ROAD_FUZZ_MIN_BBOX_M",
    1500,
    minimum = 100
  )
  max_bbox_m = road_fuzz_env_double(
    "RAYSHADER_ROAD_FUZZ_MAX_BBOX_M",
    5000,
    minimum = min_bbox_m
  )
  if (max_bbox_m < min_bbox_m) {
    stop(
      "RAYSHADER_ROAD_FUZZ_MAX_BBOX_M must not be smaller than the minimum.",
      call. = FALSE
    )
  }
  seed_default = as.integer(format(Sys.Date(), "%Y%m%d"))
  seed = road_fuzz_env_integer(
    "RAYSHADER_ROAD_FUZZ_SEED",
    seed_default,
    minimum = 1L
  )
  highway_default = paste(
    c(
      "motorway",
      "motorway_link",
      "trunk",
      "trunk_link",
      "primary",
      "primary_link",
      "secondary",
      "secondary_link",
      "tertiary",
      "tertiary_link"
    ),
    collapse = ","
  )
  highway_types = trimws(strsplit(
    Sys.getenv("RAYSHADER_ROAD_FUZZ_HIGHWAYS", unset = highway_default),
    ",",
    fixed = TRUE
  )[[1L]])
  highway_types = unique(highway_types[nzchar(highway_types)])
  if (!length(highway_types)) {
    stop("RAYSHADER_ROAD_FUZZ_HIGHWAYS must contain at least one value.")
  }
  output_dir = Sys.getenv(
    "RAYSHADER_ROAD_FUZZ_OUTPUT_DIR",
    unset = file.path(repo_root, "road-mesh-fuzz-artifacts")
  )
  dem_cache_dir = Sys.getenv(
    "RAYSHADER_ROAD_FUZZ_DEM_CACHE_DIR",
    unset = file.path(output_dir, "dem-cache")
  )
  elevation_source = Sys.getenv(
    "RAYSHADER_ROAD_FUZZ_ELEVATION_SOURCE",
    unset = "aws"
  )
  if (!nzchar(elevation_source)) {
    stop(
      "RAYSHADER_ROAD_FUZZ_ELEVATION_SOURCE must not be empty.",
      call. = FALSE
    )
  }
  list(
    repo_root = normalizePath(repo_root, winslash = "/", mustWork = TRUE),
    gol_path = Sys.getenv(
      "GEODESKR_GOL_PATH",
      unset = "/Volumes/geodesk-nfs/osm/planet-latest.osm.gol"
    ),
    gol_threads = road_fuzz_env_integer("GEODESKR_THREADS", 1L, 1L),
    random_cases = random_cases,
    seed = seed,
    min_bbox_m = min_bbox_m,
    max_bbox_m = max_bbox_m,
    min_roads = road_fuzz_env_integer(
      "RAYSHADER_ROAD_FUZZ_MIN_ROADS",
      5L,
      1L
    ),
    max_roads = road_fuzz_env_integer(
      "RAYSHADER_ROAD_FUZZ_MAX_ROADS",
      3000L,
      1L
    ),
    max_random_attempts = road_fuzz_env_integer(
      "RAYSHADER_ROAD_FUZZ_MAX_ATTEMPTS",
      max(20L, random_cases * 20L),
      1L
    ),
    elevation_zoom = road_fuzz_env_integer(
      "RAYSHADER_ROAD_FUZZ_ELEVATION_ZOOM",
      12L,
      1L
    ),
    elevation_source = elevation_source,
    highway_types = highway_types,
    lane_texture = road_fuzz_env_flag(
      "RAYSHADER_ROAD_FUZZ_LANE_TEXTURE",
      TRUE
    ),
    layered = road_fuzz_env_flag("RAYSHADER_ROAD_FUZZ_LAYERED", TRUE),
    merge = road_fuzz_env_flag("RAYSHADER_ROAD_FUZZ_MERGE", TRUE),
    parallel = road_fuzz_env_flag("RAYSHADER_ROAD_FUZZ_PARALLEL", TRUE),
    verbose = road_fuzz_env_flag("RAYSHADER_ROAD_FUZZ_VERBOSE", TRUE),
    include_curated = road_fuzz_env_flag(
      "RAYSHADER_ROAD_FUZZ_INCLUDE_CURATED",
      TRUE
    ),
    fail_fast = road_fuzz_env_flag("RAYSHADER_ROAD_FUZZ_FAIL_FAST", FALSE),
    output_dir = normalizePath(
      output_dir,
      winslash = "/",
      mustWork = FALSE
    ),
    dem_cache_dir = normalizePath(
      dem_cache_dir,
      winslash = "/",
      mustWork = FALSE
    ),
    curated_file = file.path(
      repo_root,
      "tests",
      "testthat",
      "fixtures",
      "geodeskr-road-mesh-cases.csv"
    )
  )
}

road_fuzz_require_packages = function(layered = TRUE) {
  packages = c(
    "geodeskr",
    "elevatr",
    "sf",
    "spData",
    "terra",
    "rgl",
    "rayrender",
    "rayvertex"
  )
  if (isTRUE(layered)) {
    packages = c(packages, "igraph", "Matrix", "osqp")
  }
  missing_packages = packages[
    !vapply(
      packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
  if (length(missing_packages)) {
    stop(
      sprintf(
        "The GeoDesk road fuzz fixture requires: %s.",
        paste(missing_packages, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

road_fuzz_load_us_regions = function() {
  data_environment = new.env(parent = emptyenv())
  for (dataset in c("us_states", "alaska", "hawaii")) {
    utils::data(
      list = dataset,
      package = "spData",
      envir = data_environment
    )
  }
  states = data_environment$us_states
  states = sf::st_sf(
    state = states$NAME,
    geometry = sf::st_transform(sf::st_geometry(states), 4326)
  )
  alaska = sf::st_sf(
    state = "Alaska",
    geometry = sf::st_transform(
      sf::st_geometry(data_environment$alaska),
      4326
    )
  )
  hawaii = sf::st_sf(
    state = "Hawaii",
    geometry = sf::st_transform(
      sf::st_geometry(data_environment$hawaii),
      4326
    )
  )
  regions = rbind(states, alaska, hawaii)
  sf::st_make_valid(regions)
}

road_fuzz_utm_crs = function(longitude, latitude) {
  zone = min(60L, max(1L, floor((longitude + 180) / 6) + 1L))
  epsg = if (latitude >= 0) 32600L + zone else 32700L + zone
  sprintf("EPSG:%i", epsg)
}

road_fuzz_case_from_bbox = function(
  case_id,
  state,
  bbox,
  source,
  note = "",
  case_seed = NA_integer_
) {
  bbox = suppressWarnings(as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")]))
  if (length(bbox) != 4L || any(!is.finite(bbox))) {
    stop("Road fuzz bounding boxes must contain four finite coordinates.")
  }
  names(bbox) = c("xmin", "ymin", "xmax", "ymax")
  if (bbox[["xmin"]] >= bbox[["xmax"]] || bbox[["ymin"]] >= bbox[["ymax"]]) {
    stop("Road fuzz bounding boxes must have positive width and height.")
  }
  center = c(
    longitude = mean(bbox[c("xmin", "xmax")]),
    latitude = mean(bbox[c("ymin", "ymax")])
  )
  target_crs = road_fuzz_utm_crs(center[[1L]], center[[2L]])
  scene_ll = sf::st_as_sfc(sf::st_bbox(bbox, crs = sf::st_crs(4326)))
  scene_metric = sf::st_transform(scene_ll, target_crs)
  metric_bbox = sf::st_bbox(scene_metric)
  data.frame(
    case_id = as.character(case_id),
    state = as.character(state),
    source = as.character(source),
    note = as.character(note),
    case_seed = as.integer(case_seed),
    xmin = bbox[["xmin"]],
    ymin = bbox[["ymin"]],
    xmax = bbox[["xmax"]],
    ymax = bbox[["ymax"]],
    width_m = unname(metric_bbox[["xmax"]] - metric_bbox[["xmin"]]),
    height_m = unname(metric_bbox[["ymax"]] - metric_bbox[["ymin"]]),
    target_crs = target_crs,
    stringsAsFactors = FALSE
  )
}

road_fuzz_read_curated_cases = function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  cases = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required_columns = c(
    "case_id",
    "state",
    "xmin",
    "ymin",
    "xmax",
    "ymax",
    "note"
  )
  missing_columns = setdiff(required_columns, names(cases))
  if (length(missing_columns)) {
    stop(
      sprintf(
        "Curated road cases are missing columns: %s.",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!nrow(cases)) {
    return(data.frame())
  }
  output = lapply(seq_len(nrow(cases)), function(index) {
    road_fuzz_case_from_bbox(
      case_id = cases$case_id[[index]],
      state = cases$state[[index]],
      bbox = c(
        xmin = cases$xmin[[index]],
        ymin = cases$ymin[[index]],
        xmax = cases$xmax[[index]],
        ymax = cases$ymax[[index]]
      ),
      source = "curated",
      note = cases$note[[index]],
      case_seed = NA_integer_
    )
  })
  do.call(rbind, output)
}

road_fuzz_sample_bbox = function(
  regions,
  case_index,
  seed,
  min_bbox_m,
  max_bbox_m,
  max_attempts = 100L
) {
  regions_equal_area = sf::st_transform(regions, 9311)
  for (attempt in seq_len(max_attempts)) {
    state_index = sample.int(nrow(regions), 1L)
    center_equal_area = suppressWarnings(sf::st_sample(
      regions_equal_area[state_index, ],
      size = 1L,
      type = "random",
      exact = TRUE
    ))
    if (!length(center_equal_area)) {
      next
    }
    center_ll = sf::st_transform(center_equal_area, 4326)
    center_coordinates = sf::st_coordinates(center_ll)[1L, 1:2]
    target_crs = road_fuzz_utm_crs(
      center_coordinates[[1L]],
      center_coordinates[[2L]]
    )
    center_metric = sf::st_transform(center_ll, target_crs)
    center_xy = sf::st_coordinates(center_metric)[1L, 1:2]
    width_m = exp(stats::runif(
      1L,
      log(min_bbox_m),
      log(max_bbox_m)
    ))
    aspect = exp(stats::runif(1L, log(0.75), log(4 / 3)))
    height_m = width_m / aspect
    if (height_m < min_bbox_m || height_m > max_bbox_m) {
      next
    }
    scene_metric = sf::st_as_sfc(sf::st_bbox(
      c(
        xmin = center_xy[[1L]] - width_m / 2,
        ymin = center_xy[[2L]] - height_m / 2,
        xmax = center_xy[[1L]] + width_m / 2,
        ymax = center_xy[[2L]] + height_m / 2
      ),
      crs = sf::st_crs(target_crs)
    ))
    state_metric = sf::st_transform(regions[state_index, ], target_crs)
    inside_state = sf::st_covered_by(
      scene_metric,
      state_metric,
      sparse = FALSE
    )[[1L]]
    if (!isTRUE(inside_state)) {
      next
    }
    scene_ll = sf::st_transform(scene_metric, 4326)
    bbox_ll = sf::st_bbox(scene_ll)
    case_seed = sample.int(.Machine$integer.max, 1L)
    return(road_fuzz_case_from_bbox(
      case_id = sprintf("random-%i-%03i", seed, case_index),
      state = regions$state[[state_index]],
      bbox = bbox_ll,
      source = "random",
      note = sprintf("sampling attempt %i", attempt),
      case_seed = case_seed
    ))
  }
  stop(
    sprintf(
      "Could not sample a contained U.S. bounding box after %i attempts.",
      max_attempts
    ),
    call. = FALSE
  )
}

road_fuzz_query_roads = function(store, case, config) {
  query_bbox = c(
    west = case$xmin[[1L]],
    south = case$ymin[[1L]],
    east = case$xmax[[1L]],
    north = case$ymax[[1L]]
  )
  road_tags = c(
    "name",
    "ref",
    "highway",
    "lanes",
    "lanes:forward",
    "lanes:backward",
    "oneway",
    "bridge",
    "tunnel",
    "location",
    "layer"
  )
  roads = store |>
    geodeskr::gol_bbox(query_bbox) |>
    geodeskr::gol_add_osm_feature(
      key = "highway",
      value = config$highway_types,
      types = "way"
    ) |>
    geodeskr::gol_collect_sf(
      tags = road_tags,
      geometry_errors = "empty",
      limit = config$max_roads + 1L
    )
  for (column in setdiff(road_tags, names(roads))) {
    roads[[column]] = NA_character_
  }
  roads = roads[!sf::st_is_empty(roads), , drop = FALSE]
  if (nrow(roads)) {
    roads = sf::st_make_valid(roads)
    geometry_type = as.character(sf::st_geometry_type(
      roads,
      by_geometry = TRUE
    ))
    roads = roads[
      geometry_type %in% c("LINESTRING", "MULTILINESTRING"),
      ,
      drop = FALSE
    ]
  }
  roads
}

road_fuzz_scene = function(case) {
  scene_ll = sf::st_as_sfc(sf::st_bbox(
    c(
      xmin = case$xmin[[1L]],
      ymin = case$ymin[[1L]],
      xmax = case$xmax[[1L]],
      ymax = case$ymax[[1L]]
    ),
    crs = sf::st_crs(4326)
  ))
  scene = sf::st_transform(scene_ll, case$target_crs[[1L]])
  list(scene_ll = scene_ll, scene = scene)
}

road_fuzz_fill_dem_gaps = function(dem) {
  missing_cells = sum(!is.finite(terra::values(dem, mat = FALSE)))
  while (missing_cells > 0L) {
    filled_dem = terra::focal(
      dem,
      w = 3,
      fun = "mean",
      na.policy = "only",
      na.rm = TRUE
    )
    next_missing_cells = sum(
      !is.finite(terra::values(filled_dem, mat = FALSE))
    )
    if (next_missing_cells >= missing_cells) {
      stop(
        sprintf(
          "Could not fill %i missing cells in the downloaded DEM.",
          missing_cells
        ),
        call. = FALSE
      )
    }
    dem = filled_dem
    missing_cells = next_missing_cells
  }
  dem
}

road_fuzz_fetch_dem = function(case, config) {
  scene = road_fuzz_scene(case)
  dir.create(config$dem_cache_dir, recursive = TRUE, showWarnings = FALSE)
  downloaded_dem = elevatr::get_elev_raster(
    locations = sf::st_as_sf(scene$scene),
    z = config$elevation_zoom,
    src = config$elevation_source,
    clip = "bbox",
    tmp_dir = config$dem_cache_dir
  )
  if (is.null(downloaded_dem)) {
    stop(
      sprintf(
        "No elevation raster was returned by the %s source.",
        config$elevation_source
      ),
      call. = FALSE
    )
  }
  dem = terra::rast(downloaded_dem)
  dem = terra::project(
    dem,
    case$target_crs[[1L]],
    method = "bilinear"
  )
  dem = terra::crop(
    dem,
    terra::vect(scene$scene),
    snap = "out"
  )
  if (!terra::ncell(dem)) {
    stop("The downloaded DEM does not overlap the sampled scene.")
  }
  dem = road_fuzz_fill_dem_gaps(dem)
  names(dem) = "elevation_m"
  list(dem = dem, scene = scene$scene)
}

road_fuzz_pack_dem = function(dem) {
  list(
    nrows = terra::nrow(dem),
    ncols = terra::ncol(dem),
    extent = as.vector(terra::ext(dem)),
    crs = terra::crs(dem),
    values = terra::values(dem, mat = FALSE)
  )
}

road_fuzz_unpack_dem = function(packed_dem) {
  dem = terra::rast(
    nrows = packed_dem$nrows,
    ncols = packed_dem$ncols,
    xmin = packed_dem$extent[[1L]],
    xmax = packed_dem$extent[[2L]],
    ymin = packed_dem$extent[[3L]],
    ymax = packed_dem$extent[[4L]],
    crs = packed_dem$crs
  )
  terra::values(dem) = packed_dem$values
  names(dem) = "elevation_m"
  dem
}

road_fuzz_validate_meshes = function(meshes) {
  if (!length(meshes)) {
    stop("Road meshing completed without producing any cached meshes.")
  }
  vertex_count = 0L
  face_count = 0L
  for (mesh_index in seq_along(meshes)) {
    mesh = meshes[[mesh_index]]
    if (!inherits(mesh, "mesh3d")) {
      stop(sprintf("Cached road mesh %i is not a mesh3d object.", mesh_index))
    }
    vertices = mesh$vb[1:3, , drop = FALSE]
    indices = mesh$it
    normals = mesh$normals
    texcoords = mesh$texcoords
    if (!ncol(vertices) || any(!is.finite(vertices))) {
      stop(sprintf("Cached road mesh %i has invalid vertices.", mesh_index))
    }
    if (is.null(indices) || !ncol(indices) || any(!is.finite(indices))) {
      stop(sprintf(
        "Cached road mesh %i has invalid triangle indices.",
        mesh_index
      ))
    }
    if (any(indices < 1L) || any(indices > ncol(vertices))) {
      stop(sprintf("Cached road mesh %i has out-of-range indices.", mesh_index))
    }
    if (is.null(normals) || any(!is.finite(normals))) {
      stop(sprintf("Cached road mesh %i has invalid normals.", mesh_index))
    }
    if (!is.null(texcoords) && any(!is.finite(texcoords))) {
      stop(sprintf(
        "Cached road mesh %i has invalid texture coordinates.",
        mesh_index
      ))
    }
    vertex_count = vertex_count + ncol(vertices)
    face_count = face_count + ncol(indices)
  }
  list(
    mesh_count = length(meshes),
    vertex_count = vertex_count,
    face_count = face_count
  )
}

road_fuzz_run_case = function(case, roads_ll, config, surface = NULL) {
  if (is.null(surface)) {
    surface = road_fuzz_fetch_dem(case, config)
  }
  roads = suppressWarnings(sf::st_transform(
    roads_ll,
    case$target_crs[[1L]]
  ))
  roads = suppressWarnings(sf::st_crop(
    roads,
    sf::st_bbox(surface$scene)
  ))
  roads = roads[!sf::st_is_empty(roads), , drop = FALSE]
  if (!nrow(roads)) {
    stop("No road geometry remained after projection and scene cropping.")
  }

  previous_null_setting = Sys.getenv("RGL_USE_NULL", unset = NA_character_)
  Sys.setenv(RGL_USE_NULL = "true")
  on.exit(
    {
      if (is.na(previous_null_setting)) {
        Sys.unsetenv("RGL_USE_NULL")
      } else {
        Sys.setenv(RGL_USE_NULL = previous_null_setting)
      }
    },
    add = TRUE
  )
  rgl::open3d(useNULL = TRUE)
  on.exit(rgl::close3d(), add = TRUE)
  rayshader::height_shade(surface$dem) |>
    rayshader::plot_3d(
      heightmap = surface$dem,
      solid = FALSE,
      shadow = FALSE,
      water = FALSE,
      window_size = c(400, 400)
    )

  start_time = proc.time()[["elapsed"]]
  if (isTRUE(config$layered)) {
    rayshader::render_roads(
      roads,
      heightmap = surface$dem,
      lanes = lanes,
      layer = layer,
      lane_width = 3.25,
      lane_texture = config$lane_texture,
      merge = config$merge,
      verbose = config$verbose,
      parallel = config$parallel
    )
  } else {
    rayshader::render_roads(
      roads,
      heightmap = surface$dem,
      lanes = lanes,
      lane_width = 3.25,
      lane_texture = config$lane_texture,
      merge = config$merge,
      verbose = config$verbose,
      parallel = config$parallel
    )
  }
  elapsed = proc.time()[["elapsed"]] - start_time
  meshes = rayshader:::get_scene_road_meshes(default = NULL)
  validation = road_fuzz_validate_meshes(meshes)
  c(
    validation,
    list(
      elapsed_seconds = unname(elapsed),
      projected_road_count = nrow(roads)
    )
  )
}

road_fuzz_empty_manifest_row = function(case, road_count = NA_integer_) {
  data.frame(
    case_id = case$case_id[[1L]],
    state = case$state[[1L]],
    source = case$source[[1L]],
    seed = case$case_seed[[1L]],
    xmin = case$xmin[[1L]],
    ymin = case$ymin[[1L]],
    xmax = case$xmax[[1L]],
    ymax = case$ymax[[1L]],
    width_m = case$width_m[[1L]],
    height_m = case$height_m[[1L]],
    road_count = as.integer(road_count),
    projected_road_count = NA_integer_,
    mesh_count = NA_integer_,
    vertex_count = NA_integer_,
    face_count = NA_integer_,
    elapsed_seconds = NA_real_,
    status = "running",
    error = "",
    artifact = "",
    stringsAsFactors = FALSE
  )
}

road_fuzz_write_manifest = function(rows, path) {
  manifest = if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame()
  }
  utils::write.csv(manifest, path, row.names = FALSE, na = "")
  invisible(manifest)
}

road_fuzz_write_replay_input = function(
  path,
  case,
  roads,
  config,
  surface = NULL,
  error = NULL
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(
      case = case,
      roads = roads,
      config = config[
        setdiff(
          names(config),
          c("repo_root", "output_dir", "dem_cache_dir")
        )
      ],
      dem = if (is.null(surface)) NULL else road_fuzz_pack_dem(surface$dem),
      error = if (is.null(error)) NULL else conditionMessage(error),
      recorded_at = Sys.time()
    ),
    path
  )
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

road_fuzz_save_failure = function(
  case,
  roads,
  config,
  error,
  surface = NULL
) {
  safe_case_id = gsub(
    "[^A-Za-z0-9._-]+",
    "-",
    case$case_id[[1L]]
  )
  if (!nzchar(safe_case_id)) {
    safe_case_id = "unnamed-case"
  }
  failure_dir = file.path(
    config$output_dir,
    "failures",
    safe_case_id
  )
  dir.create(failure_dir, recursive = TRUE, showWarnings = FALSE)
  artifact = file.path(failure_dir, "input.rds")
  artifact = road_fuzz_write_replay_input(
    path = artifact,
    case = case,
    roads = roads,
    config = config,
    surface = surface,
    error = error
  )
  utils::write.csv(
    case[c("case_id", "state", "xmin", "ymin", "xmax", "ymax", "note")],
    file.path(failure_dir, "curated-case.csv"),
    row.names = FALSE
  )
  normalizePath(artifact, winslash = "/", mustWork = TRUE)
}

road_fuzz_run = function(config = road_fuzz_config()) {
  road_fuzz_require_packages(config$layered)
  if (!file.exists(config$gol_path)) {
    stop(
      sprintf("GeoDesk GOL file does not exist: %s", config$gol_path),
      call. = FALSE
    )
  }
  if (config$max_roads < config$min_roads) {
    stop("The maximum road count must not be smaller than the minimum.")
  }
  dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
  manifest_path = file.path(config$output_dir, "manifest.csv")
  message(sprintf(
    "GeoDesk road mesh fuzz seed %i; %i random cases; artifacts: %s",
    config$seed,
    config$random_cases,
    config$output_dir
  ))
  store = geodeskr::gol_open(
    config$gol_path,
    threads = config$gol_threads
  )
  regions = road_fuzz_load_us_regions()
  curated_cases = if (isTRUE(config$include_curated)) {
    road_fuzz_read_curated_cases(config$curated_file)
  } else {
    data.frame()
  }
  set.seed(config$seed)
  rows = list()
  failure_count = 0L
  completed_random_cases = 0L
  random_attempt = 0L

  run_candidate = function(case, is_random) {
    roads_result = tryCatch(
      list(
        roads = road_fuzz_query_roads(store, case, config),
        error = NULL
      ),
      error = function(error) list(roads = NULL, error = error)
    )
    if (!is.null(roads_result$error)) {
      row = road_fuzz_empty_manifest_row(case)
      row$status = "query_failure"
      row$error = conditionMessage(roads_result$error)
      rows[[length(rows) + 1L]] <<- row
      road_fuzz_write_manifest(rows, manifest_path)
      failure_count <<- failure_count + 1L
      return(FALSE)
    }
    roads = roads_result$roads
    road_count = nrow(roads)
    minimum_road_count = if (isTRUE(is_random)) config$min_roads else 1L
    if (road_count < minimum_road_count || road_count > config$max_roads) {
      row = road_fuzz_empty_manifest_row(case, road_count)
      row$status = if (road_count < minimum_road_count) {
        "rejected_too_few_roads"
      } else {
        "rejected_too_many_roads"
      }
      rows[[length(rows) + 1L]] <<- row
      road_fuzz_write_manifest(rows, manifest_path)
      return(FALSE)
    }

    message(sprintf(
      "[%s] %s: %i roads; bbox %.0f x %.0f m",
      case$source[[1L]],
      case$case_id[[1L]],
      road_count,
      case$width_m[[1L]],
      case$height_m[[1L]]
    ))
    row_index = length(rows) + 1L
    rows[[row_index]] <<- road_fuzz_empty_manifest_row(case, road_count)
    road_fuzz_write_manifest(rows, manifest_path)
    terrain_result = tryCatch(
      list(surface = road_fuzz_fetch_dem(case, config), error = NULL),
      error = function(error) list(surface = NULL, error = error)
    )
    if (!is.null(terrain_result$error)) {
      artifact = road_fuzz_save_failure(
        case,
        roads,
        config,
        terrain_result$error
      )
      rows[[row_index]]$status <<- "terrain_failure"
      rows[[row_index]]$error <<- conditionMessage(terrain_result$error)
      rows[[row_index]]$artifact <<- artifact
      failure_count <<- failure_count + 1L
      road_fuzz_write_manifest(rows, manifest_path)
      message(sprintf(
        "FAILED %s while downloading terrain: %s",
        case$case_id[[1L]],
        rows[[row_index]]$error
      ))
      if (isTRUE(config$fail_fast)) {
        stop(rows[[row_index]]$error, call. = FALSE)
      }
      return(TRUE)
    }
    surface = terrain_result$surface
    checkpoint = road_fuzz_write_replay_input(
      path = file.path(config$output_dir, "current-input.rds"),
      case = case,
      roads = roads,
      config = config,
      surface = surface
    )
    rows[[row_index]]$artifact <<- checkpoint
    road_fuzz_write_manifest(rows, manifest_path)
    mesh_result = tryCatch(
      list(
        result = road_fuzz_run_case(case, roads, config, surface),
        error = NULL
      ),
      error = function(error) list(result = NULL, error = error)
    )
    if (!is.null(mesh_result$error)) {
      artifact = road_fuzz_save_failure(
        case,
        roads,
        config,
        mesh_result$error,
        surface
      )
      rows[[row_index]]$status <<- "mesh_failure"
      rows[[row_index]]$error <<- conditionMessage(mesh_result$error)
      rows[[row_index]]$artifact <<- artifact
      failure_count <<- failure_count + 1L
      road_fuzz_write_manifest(rows, manifest_path)
      message(sprintf(
        "FAILED %s: %s",
        case$case_id[[1L]],
        rows[[row_index]]$error
      ))
      if (isTRUE(config$fail_fast)) {
        stop(rows[[row_index]]$error, call. = FALSE)
      }
      return(TRUE)
    }
    result = mesh_result$result
    rows[[row_index]]$status <<- "passed"
    rows[[row_index]]$projected_road_count <<- result$projected_road_count
    rows[[row_index]]$mesh_count <<- result$mesh_count
    rows[[row_index]]$vertex_count <<- result$vertex_count
    rows[[row_index]]$face_count <<- result$face_count
    rows[[row_index]]$elapsed_seconds <<- result$elapsed_seconds
    rows[[row_index]]$artifact <<- ""
    road_fuzz_write_manifest(rows, manifest_path)
    message(sprintf(
      "PASSED %s: %i meshes, %i vertices, %.2f s",
      case$case_id[[1L]],
      result$mesh_count,
      result$vertex_count,
      result$elapsed_seconds
    ))
    TRUE
  }

  if (nrow(curated_cases)) {
    for (case_index in seq_len(nrow(curated_cases))) {
      run_candidate(curated_cases[case_index, , drop = FALSE], FALSE)
    }
  }
  while (
    completed_random_cases < config$random_cases &&
      random_attempt < config$max_random_attempts
  ) {
    random_attempt = random_attempt + 1L
    case = road_fuzz_sample_bbox(
      regions = regions,
      case_index = random_attempt,
      seed = config$seed,
      min_bbox_m = config$min_bbox_m,
      max_bbox_m = config$max_bbox_m
    )
    accepted = run_candidate(case, TRUE)
    if (isTRUE(accepted)) {
      completed_random_cases = completed_random_cases + 1L
    }
  }
  manifest = road_fuzz_write_manifest(rows, manifest_path)
  if (completed_random_cases < config$random_cases) {
    stop(
      sprintf(
        "Only %i of %i requested random cases met the road-count constraints after %i attempts. See %s.",
        completed_random_cases,
        config$random_cases,
        random_attempt,
        manifest_path
      ),
      call. = FALSE
    )
  }
  if (failure_count) {
    stop(
      sprintf(
        "%i GeoDesk road mesh fuzz case(s) failed. See %s.",
        failure_count,
        manifest_path
      ),
      call. = FALSE
    )
  }
  message(sprintf(
    "All %i GeoDesk road mesh fuzz cases passed.",
    sum(manifest$status == "passed")
  ))
  invisible(manifest)
}

road_fuzz_replay = function(path, config = road_fuzz_config()) {
  input = readRDS(path)
  if (is.null(input$case) || is.null(input$roads)) {
    stop("Replay input must contain `case` and `roads`.", call. = FALSE)
  }
  if (!is.null(input$config)) {
    config = utils::modifyList(config, input$config)
  }
  road_fuzz_require_packages(config$layered)
  surface = if (is.null(input$dem)) {
    NULL
  } else {
    list(
      dem = road_fuzz_unpack_dem(input$dem),
      scene = road_fuzz_scene(input$case)$scene
    )
  }
  result = road_fuzz_run_case(input$case, input$roads, config, surface)
  message(sprintf(
    "Replay passed: %i meshes, %i vertices, %.2f s",
    result$mesh_count,
    result$vertex_count,
    result$elapsed_seconds
  ))
  invisible(result)
}
