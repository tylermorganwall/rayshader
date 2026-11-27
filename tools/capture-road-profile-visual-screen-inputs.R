# Regenerate the frozen inputs used by run-road-profile-visual-screen.R.
#
# Run from the rayshader repository root. The output layout is exactly the one
# consumed by the visual sensitivity driver:
#
#   <input-dir>/<case-id>/roads.rds
#   <input-dir>/<case-id>/dem.tif

repo_root = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run this script from the rayshader repository root.", call. = FALSE)
}

fixture_input = function(name, default) {
  Sys.getenv(name, unset = as.character(default))
}

fixture_flag = function(name, default = FALSE) {
  value = tolower(fixture_input(name, if (default) "true" else "false"))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(sprintf("%s must be true or false.", name), call. = FALSE)
  }
  value %in% c("true", "1", "yes")
}

fixture_integer = function(name, default, minimum = 1L) {
  value = suppressWarnings(as.integer(fixture_input(name, default)))
  if (is.na(value) || value < minimum) {
    stop(sprintf("%s must be an integer >= %d.", name, minimum), call. = FALSE)
  }
  value
}

fixture_number = function(name, default, minimum = 0) {
  value = suppressWarnings(as.numeric(fixture_input(name, default)))
  if (!is.finite(value) || value < minimum) {
    stop(
      sprintf("%s must be a finite number >= %g.", name, minimum),
      call. = FALSE
    )
  }
  value
}

case_definitions = data.frame(
  case_id = c("houston", "houston_overpass", "swiss"),
  xmin = c(-95.381, -95.3775264478, 7.76),
  ymin = c(29.740, 29.7584334130, 46.52),
  xmax = c(-95.347, -95.3671915281, 8.02),
  ymax = c(29.765, 29.7674505836, 46.80),
  target_crs = c("EPSG:32615", "EPSG:32615", "EPSG:32632"),
  description = c(
    "Houston layered interchange and dense-city regression scene",
    paste(
      "Houston problem overpass centered at 29.762942, -95.372359",
      "with an approximately 500 m buffer"
    ),
    "Swiss steep-terrain and switchback scene"
  ),
  stringsAsFactors = FALSE
)

parse_selection = function(value, allowed) {
  if (identical(tolower(trimws(value)), "all")) {
    return(allowed)
  }
  selected = unique(trimws(strsplit(value, ",", fixed = TRUE)[[1L]]))
  unknown = setdiff(selected, allowed)
  if (length(unknown)) {
    stop(
      sprintf("Unknown fixture case(s): %s.", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }
  selected
}

fixture_output_dir = fixture_input(
  "RAYSHADER_SCREEN_INPUT_DIR",
  file.path(repo_root, "road-profile-screen-inputs")
)
config = list(
  output_dir = fixture_output_dir,
  cases = fixture_input("RAYSHADER_SCREEN_FIXTURE_CASES", "all"),
  overwrite = fixture_flag("RAYSHADER_SCREEN_FIXTURE_OVERWRITE"),
  gol_path = fixture_input(
    "GEODESKR_GOL_PATH",
    "/Volumes/geodesk-nfs/osm/planet-latest.osm.gol"
  ),
  gol_threads = fixture_integer("GEODESKR_THREADS", 1L),
  elevation_zoom = fixture_integer("RAYSHADER_SCREEN_ELEVATION_ZOOM", 13L),
  elevation_source = fixture_input(
    "RAYSHADER_SCREEN_ELEVATION_SOURCE",
    "aws"
  ),
  download_cache_dir = fixture_input(
    "RAYSHADER_SCREEN_DEM_CACHE_DIR",
    file.path(fixture_output_dir, "download-cache")
  ),
  houston_dem_buffer_m = fixture_number(
    "RAYSHADER_SCREEN_HOUSTON_DEM_BUFFER_M",
    250
  ),
  swiss_dem_scale = fixture_number(
    "RAYSHADER_SCREEN_SWISS_DEM_SCALE",
    0.5
  )
)
if (config$swiss_dem_scale <= 0 || config$swiss_dem_scale > 1) {
  stop("RAYSHADER_SCREEN_SWISS_DEM_SCALE must be in (0, 1].", call. = FALSE)
}

selected_cases = parse_selection(config$cases, case_definitions$case_id)

required_packages = c("pkgload", "sf", "terra", "elevatr", "geodeskr")
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

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(config$download_cache_dir, recursive = TRUE, showWarnings = FALSE)

# resize_spatial() is the only package function needed during capture. Loading
# the working checkout keeps the Swiss DEM preparation identical to the visual
# campaign without installing rayshader.
pkgload::load_all(repo_root, quiet = TRUE)

highway_types = c(
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

file_hash = function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  unname(tools::md5sum(path)[[1L]])
}

scene_polygon = function(case) {
  bbox = sf::st_bbox(
    c(
      xmin = case$xmin,
      ymin = case$ymin,
      xmax = case$xmax,
      ymax = case$ymax
    ),
    crs = sf::st_crs(4326)
  )
  sf::st_as_sf(sf::st_as_sfc(bbox))
}

fill_dem_edges = function(dem) {
  missing = sum(is.na(terra::values(dem, mat = FALSE)))
  while (missing > 0L) {
    message(sprintf("    Filling %d empty DEM edge cells", missing))
    filled = terra::focal(
      dem,
      w = 3,
      fun = "mean",
      na.policy = "only",
      na.rm = TRUE
    )
    next_missing = sum(is.na(terra::values(filled, mat = FALSE)))
    if (next_missing >= missing) {
      stop("Could not fill all empty DEM cells.", call. = FALSE)
    }
    dem = filled
    missing = next_missing
  }
  dem
}

query_roads = function(store, case) {
  bbox = c(
    west = case$xmin,
    south = case$ymin,
    east = case$xmax,
    north = case$ymax
  )
  roads = store |>
    geodeskr::gol_bbox(bbox) |>
    geodeskr::gol_query("w[highway]") |>
    geodeskr::gol_collect_sf(tags = road_tags)
  if (!"highway" %in% names(roads)) {
    roads$highway = NA_character_
  }
  roads = roads[roads$highway %in% highway_types, ]
  geometry_type = as.character(sf::st_geometry_type(roads, by_geometry = TRUE))
  roads[geometry_type %in% c("LINESTRING", "MULTILINESTRING"), ]
}

capture_dem = function(case, scene_ll, scene) {
  case_cache = file.path(config$download_cache_dir, case$case_id)
  dir.create(case_cache, recursive = TRUE, showWarnings = FALSE)
  houston = grepl("^houston", case$case_id)
  download_area = if (houston) {
    sf::st_buffer(scene, dist = config$houston_dem_buffer_m)
  } else {
    scene_ll
  }
  dem = elevatr::get_elev_raster(
    locations = download_area,
    z = config$elevation_zoom,
    src = config$elevation_source,
    clip = "bbox",
    tmp_dir = case_cache
  ) |>
    terra::rast()
  if (!houston && config$swiss_dem_scale < 1) {
    dem = rayshader::resize_spatial(
      dem,
      scale = config$swiss_dem_scale,
      method_down = "mean"
    )
  }
  dem = terra::project(dem, case$target_crs, method = "bilinear") |>
    terra::crop(terra::vect(scene), snap = "out")
  dem = fill_dem_edges(dem)
  names(dem) = "elevation_m"
  dem
}

needs_roads = vapply(
  selected_cases,
  function(case_id) {
    path = file.path(config$output_dir, case_id, "roads.rds")
    config$overwrite || !file.exists(path)
  },
  logical(1)
)

store = NULL
if (any(needs_roads)) {
  if (!file.exists(config$gol_path)) {
    stop(
      sprintf("GeoDesk planet file is unavailable at `%s`.", config$gol_path),
      call. = FALSE
    )
  }
  message("Opening GeoDesk planet file: ", config$gol_path)
  store = geodeskr::gol_open(config$gol_path, threads = config$gol_threads)
}

case_records = vector("list", length(selected_cases))
for (case_index in seq_along(selected_cases)) {
  case_id = selected_cases[[case_index]]
  case = case_definitions[case_definitions$case_id == case_id, , drop = FALSE]
  case_dir = file.path(config$output_dir, case_id)
  roads_path = file.path(case_dir, "roads.rds")
  dem_path = file.path(case_dir, "dem.tif")
  dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)
  message(sprintf(
    "[%d/%d] Capturing %s",
    case_index,
    length(selected_cases),
    case_id
  ))

  scene_ll = scene_polygon(case)
  scene = sf::st_transform(scene_ll, case$target_crs)

  if (config$overwrite || !file.exists(roads_path)) {
    message("  Querying GeoDesk roads")
    roads = query_roads(store, case)
  } else {
    message("  Reusing roads.rds")
    roads = readRDS(roads_path)
  }

  if (config$overwrite || !file.exists(dem_path)) {
    message("  Downloading and preparing terrain")
    dem = capture_dem(case, scene_ll, scene)
    terra::writeRaster(
      dem,
      dem_path,
      overwrite = TRUE,
      gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3")
    )
  } else {
    message("  Reusing dem.tif")
    dem = terra::rast(dem_path)
  }

  roads = sf::st_transform(roads, terra::crs(dem))
  roads = suppressWarnings(sf::st_crop(roads, sf::st_bbox(scene)))
  roads = roads[!sf::st_is_empty(roads), ]
  saveRDS(roads, roads_path, compress = "xz")

  case_records[[case_index]] = data.frame(
    case_id = case_id,
    description = case$description,
    xmin = case$xmin,
    ymin = case$ymin,
    xmax = case$xmax,
    ymax = case$ymax,
    target_crs = terra::crs(dem, proj = TRUE),
    road_count = nrow(roads),
    dem_rows = terra::nrow(dem),
    dem_columns = terra::ncol(dem),
    dem_resolution_x = terra::res(dem)[[1L]],
    dem_resolution_y = terra::res(dem)[[2L]],
    elevation_zoom = config$elevation_zoom,
    elevation_source = config$elevation_source,
    roads_path = normalizePath(roads_path, winslash = "/"),
    roads_hash = file_hash(roads_path),
    dem_path = normalizePath(dem_path, winslash = "/"),
    dem_hash = file_hash(dem_path),
    captured_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
  message(sprintf(
    "  Saved %d roads and a %d x %d DEM",
    nrow(roads),
    terra::ncol(dem),
    terra::nrow(dem)
  ))
}

new_records = do.call(rbind, case_records)
registry_csv = file.path(config$output_dir, "input-registry.csv")
if (file.exists(registry_csv)) {
  existing_records = utils::read.csv(registry_csv, stringsAsFactors = FALSE)
  existing_records = existing_records[
    !existing_records$case_id %in% new_records$case_id,
    ,
    drop = FALSE
  ]
  new_records = rbind(existing_records, new_records)
}
new_records = new_records[
  match(case_definitions$case_id, new_records$case_id, nomatch = 0L),
]
utils::write.csv(new_records, registry_csv, row.names = FALSE)

gol_info = if (file.exists(config$gol_path)) {
  file.info(config$gol_path)
} else {
  NULL
}
registry = list(
  fixture_id = "road-profile-visual-screen",
  captured_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  geodesk = list(
    path = if (file.exists(config$gol_path)) {
      normalizePath(config$gol_path, winslash = "/")
    } else {
      config$gol_path
    },
    size = if (is.null(gol_info)) NA_real_ else unname(gol_info$size),
    modified = if (is.null(gol_info)) {
      NA_character_
    } else {
      format(gol_info$mtime, "%Y-%m-%dT%H:%M:%S%z")
    },
    threads = config$gol_threads
  ),
  elevation = list(
    zoom = config$elevation_zoom,
    source = config$elevation_source,
    houston_buffer_m = config$houston_dem_buffer_m,
    swiss_scale = config$swiss_dem_scale
  ),
  cases = lapply(seq_len(nrow(new_records)), function(index) {
    as.list(new_records[index, , drop = FALSE])
  })
)
if (requireNamespace("yaml", quietly = TRUE)) {
  yaml::write_yaml(registry, file.path(config$output_dir, "input-registry.yml"))
} else {
  message("Install `yaml` to also write input-registry.yml; CSV was written.")
}

message("Fixture capture complete: ", normalizePath(config$output_dir))
message("Run the visual screen with:")
message(
  "  RAYSHADER_SCREEN_INPUT_DIR=",
  normalizePath(config$output_dir),
  " Rscript tools/run-road-profile-visual-screen.R"
)
