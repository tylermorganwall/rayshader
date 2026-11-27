repo_root = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run this script from the rayshader repository root.", call. = FALSE)
}

# Edit the values in this block for an interactive run. They are authoritative
# by default, including when this script is sourced repeatedly in one R session.
# Set this to TRUE only when values supplied by the launching shell should win.
use_shell_environment = FALSE
random_cases = 100L
maximum_attempts = max(20L, random_cases * 20L)
highway_types = paste(
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
output_dir = file.path(repo_root, "road-mesh-fuzz-artifacts")
road_fuzz_environment = c(
  GEODESKR_GOL_PATH = "/Volumes/geodesk-nfs/osm/planet-latest.osm.gol",
  GEODESKR_THREADS = 1L,
  RAYSHADER_ROAD_FUZZ_CASES = random_cases,
  RAYSHADER_ROAD_FUZZ_SEED = format(Sys.Date(), "%Y%m%d"),
  RAYSHADER_ROAD_FUZZ_MIN_BBOX_M = 20000,
  RAYSHADER_ROAD_FUZZ_MAX_BBOX_M = 30000,
  RAYSHADER_ROAD_FUZZ_MIN_ROADS = 5L,
  RAYSHADER_ROAD_FUZZ_MAX_ROADS = 10000L,
  RAYSHADER_ROAD_FUZZ_MAX_ATTEMPTS = maximum_attempts,
  RAYSHADER_ROAD_FUZZ_ELEVATION_ZOOM = 13L,
  RAYSHADER_ROAD_FUZZ_ELEVATION_SOURCE = "aws",
  RAYSHADER_ROAD_FUZZ_DEM_CACHE_DIR = file.path(output_dir, "dem-cache"),
  RAYSHADER_ROAD_FUZZ_HIGHWAYS = highway_types,
  RAYSHADER_ROAD_FUZZ_LANE_TEXTURE = "true",
  RAYSHADER_ROAD_FUZZ_LAYERED = "true",
  RAYSHADER_ROAD_FUZZ_MERGE = "true",
  RAYSHADER_ROAD_FUZZ_PARALLEL = "true",
  RAYSHADER_ROAD_FUZZ_VERBOSE = "true",
  RAYSHADER_ROAD_FUZZ_INCLUDE_CURATED = "true",
  RAYSHADER_ROAD_FUZZ_FAIL_FAST = "false",
  RAYSHADER_ROAD_FUZZ_OUTPUT_DIR = output_dir,
  RAYSHADER_ROAD_FUZZ_REPLAY_RDS = "",
  RGL_USE_NULL = "true"
)
if (isTRUE(use_shell_environment)) {
  road_fuzz_environment = vapply(
    names(road_fuzz_environment),
    function(name) {
      Sys.getenv(name, unset = road_fuzz_environment[[name]])
    },
    character(1)
  )
}
do.call(Sys.setenv, as.list(road_fuzz_environment))

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install `pkgload` to run the road mesh fuzz fixture.", call. = FALSE)
}
pkgload::load_all(repo_root, quiet = TRUE)
fixture_environment = new.env(parent = globalenv())
sys.source(
  file.path(
    repo_root,
    "tests",
    "testthat",
    "fixtures",
    "geodeskr-road-mesh-fuzz.R"
  ),
  envir = fixture_environment
)
config = fixture_environment$road_fuzz_config(repo_root)
replay_path = road_fuzz_environment[["RAYSHADER_ROAD_FUZZ_REPLAY_RDS"]]
if (nzchar(replay_path)) {
  fixture_environment$road_fuzz_replay(replay_path, config)
} else {
  fixture_environment$road_fuzz_run(config)
}
