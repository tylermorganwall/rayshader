# GeoDesk road mesh fuzz fixture

This opt-in integration fixture samples projected bounding boxes wholly inside
U.S. state polygons, queries current OpenStreetMap roads from a local GeoDesk
GOL, downloads real elevation data through `elevatr`, builds the road meshes
through `render_roads()`, and validates every cached mesh. It also runs the
checked-in cases in
`tests/testthat/fixtures/geodeskr-road-mesh-cases.csv`.

It requires `geodeskr`, `elevatr`, `sf`, `spData`, `terra`, `rgl`, `rayrender`,
and `rayvertex`. Layered mode also requires `igraph`, `Matrix`, and `osqp`.

Run it from the repository root:

```sh
RAYSHADER_ROAD_FUZZ_CASES=10 \
RAYSHADER_ROAD_FUZZ_SEED=20260809 \
Rscript tools/run-geodeskr-road-mesh-fuzz.R
```

The runner contains a complete `road_fuzz_environment` input block listing
every supported environment setting. Values edited in the script are
authoritative by default, including when it is sourced repeatedly in one R
session. Set `use_shell_environment = TRUE` when values supplied by the
launching shell should take precedence.

The seed defaults to the current date, so repeated runs on one day are
reproducible while occasional runs naturally explore new locations. The default
GOL path is `/Volumes/geodesk-nfs/osm/planet-latest.osm.gol`; override it with
`GEODESKR_GOL_PATH`.

Important controls:

- `RAYSHADER_ROAD_FUZZ_CASES`: number of accepted random cases; default `5`.
- `RAYSHADER_ROAD_FUZZ_MIN_BBOX_M` and `RAYSHADER_ROAD_FUZZ_MAX_BBOX_M`:
  projected side-length bounds; defaults `1500` and `5000` metres.
- `RAYSHADER_ROAD_FUZZ_MIN_ROADS` and `RAYSHADER_ROAD_FUZZ_MAX_ROADS`:
  accepted road-feature range; defaults `5` and `3000`.
- `RAYSHADER_ROAD_FUZZ_HIGHWAYS`: comma-separated OSM highway classes.
- `RAYSHADER_ROAD_FUZZ_ELEVATION_ZOOM`: `elevatr` zoom level; default `12`.
- `RAYSHADER_ROAD_FUZZ_ELEVATION_SOURCE`: `elevatr` source; default `"aws"`.
- `RAYSHADER_ROAD_FUZZ_DEM_CACHE_DIR`: elevation download cache; defaults to
  `dem-cache` inside the artifact directory.
- `RAYSHADER_ROAD_FUZZ_LAYERED`, `RAYSHADER_ROAD_FUZZ_LANE_TEXTURE`,
  `RAYSHADER_ROAD_FUZZ_MERGE`, and `RAYSHADER_ROAD_FUZZ_PARALLEL`: default
  `true`.
- `RAYSHADER_ROAD_FUZZ_INCLUDE_CURATED`: run checked-in replay boxes; default
  `true`.
- `RAYSHADER_ROAD_FUZZ_OUTPUT_DIR`: artifact directory; default
  `road-mesh-fuzz-artifacts`.

The manifest is rewritten after every stage, and `current-input.rds` is written
before native meshing, so the last running case, its exact OSM features, and its
downloaded terrain remain available after a native crash. Caught failures are
copied to `failures/CASE/input.rds` with a one-row `curated-case.csv`. Copy useful
failure rows into the checked-in case CSV after fixing the regression to grow
permanent coverage.

Replay an exact captured input with:

```sh
RAYSHADER_ROAD_FUZZ_REPLAY_RDS=road-mesh-fuzz-artifacts/failures/CASE/input.rds \
Rscript tools/run-geodeskr-road-mesh-fuzz.R
```

Replay restores the captured meshing options as well as the exact road
features and bounding box.

The same harness can be invoked through testthat by setting
`RAYSHADER_RUN_GEODESKR_ROAD_FUZZ=true` and running the
`test-geodeskr-road-mesh-fuzz.R` test file.
