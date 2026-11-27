# Road-profile visual sensitivity screen

> Historical campaign harness (2026-08-16). The four relaxed-setting hooks and
> terrain-gradient smoothstep were removed after the global 2x2 visual campaign
> accepted fixed `maximum_grade = 0.15` and
> `continuation_grade_tolerance = 0.14`. This document describes how the
> preserved 81-cell evidence was produced; it is not the current production
> interface. New comparisons should pass the two supported arguments directly
> to `render_roads()`.

`run-road-profile-visual-screen.R` renders the complete 3^4 road-profile
factorial through the working checkout's production `render_roads()` and
`render_highquality()` paths. It is a visual experiment driver, not a numerical
acceptance gate. The script assigns no success or failure verdict to a rendered
candidate.

The package is loaded and compiled once with `pkgload::load_all()`. Each cell's
four solver inputs are then changed with the internal
`rayshader.road_profile_screen_settings` runtime option. The package is not
reinstalled between cells.

## Default evidence coverage

The 81 cells combine three levels of each factor:

| Factor | Baseline | Middle | Maximum relaxation |
|---|---:|---:|---:|
| maximum grade rate | 0.001/m | 0.005/m | 0.020/m |
| maximum absolute grade | 7% | 15% | unbounded |
| continuation-grade tolerance | 0 | 0.03 | 0.14 |
| terrain-reference weight | 0.001 | 0.03 | 1 |

The ordinary-terrain maximum grade remains 7%. From a 7% terrain gradient to
a 30% terrain gradient, the active limit blends smoothly toward the selected
steep-terrain maximum. The unbounded level relaxes reciprocal constraint
strength: it starts at 7%, increases smoothly, and removes the hard grade bound
at full relaxation. Curvature weight remains fixed at its production value of
100 in every cell.

Every cell uses these 16 fixed cameras:

- six Swiss switchback views: overview, low side, high side, both longitudinal
  directions, and worst station;
- six Houston interchange views with the same camera classes;
- two views of the problem Houston overpass centered at 29.762942,
  -95.372359 with a 500 m buffer;
- two synthetic cross-slope views.

The default run therefore creates 1,296 source path-traced images: 16 shared
H00 baseline renders and 16 independently built candidate renders for each of
the other 80 cells. Every candidate render also gets an absolute difference, a
32x magenta difference, a magenta overlay, a native-resolution 2x2 contact
sheet, and an MP4 that hard-flips between baseline, candidate, and difference.
Houston lane textures are disabled so painted lines do not dominate the visual
difference.

The camera zoom values are the accepted 2x-closer framing (`zoom / 2`). The
script never sets `preview = "mesh"`.

## Frozen inputs

By default, the script first looks in:

```text
/private/tmp/road-profile-evidence/campaign-20260815/inputs
```

It then falls back to `road-profile-screen-inputs` in the repository. A custom
input directory must have this layout:

```text
<input-dir>/
  houston/
    dem.tif
    roads.rds
  houston_overpass/
    dem.tif
    roads.rds
  swiss/
    dem.tif
    roads.rds
```

Set `RAYSHADER_SCREEN_INPUT_DIR` when those fixtures live elsewhere.

### Regenerate the frozen inputs

With the GeoDesk planet store mounted, regenerate all three cases from the
repository root with:

```sh
RAYSHADER_SCREEN_INPUT_DIR="$PWD/road-profile-screen-inputs" \
Rscript tools/capture-road-profile-visual-screen-inputs.R
```

The capture utility queries the fixed Houston, Houston-overpass, and Swiss
bounding boxes from GeoDesk and downloads their real AWS zoom-13 terrain. It
uses the original campaign projections and DEM preparation, writes compressed
`dem.tif` and `roads.rds` files, and records file hashes and dimensions in
`input-registry.csv`. Complete existing cases are reused so an interrupted
capture can be resumed. Set `RAYSHADER_SCREEN_FIXTURE_OVERWRITE=true` to
replace them.

The relevant capture settings are:

| Environment variable | Default | Purpose |
|---|---|---|
| `GEODESKR_GOL_PATH` | `/Volumes/geodesk-nfs/osm/planet-latest.osm.gol` | mounted GeoDesk store |
| `GEODESKR_THREADS` | `1` | GeoDesk query threads |
| `RAYSHADER_SCREEN_FIXTURE_CASES` | `all` | comma-separated cases to capture |
| `RAYSHADER_SCREEN_FIXTURE_OVERWRITE` | `false` | replace complete case files |
| `RAYSHADER_SCREEN_ELEVATION_ZOOM` | `13` | AWS terrain zoom |
| `RAYSHADER_SCREEN_DEM_CACHE_DIR` | `<input-dir>/download-cache` | elevation download cache |

Fixture capture requires `geodeskr`, `elevatr`, `sf`, and `terra`, plus network
access for terrain tiles. The completed fixtures are offline inputs to the
visual screen; running the screen does not query GeoDesk or download terrain.

## Validate without rendering

Run this from the repository root. It compiles the current checkout and writes
the factor, scene, camera, and configuration manifests, but creates no render:

```sh
RAYSHADER_SCREEN_DRY_RUN=true \
RAYSHADER_SCREEN_OUTPUT_DIR=/private/tmp/road-profile-visual-screen-dry-run \
Rscript tools/run-road-profile-visual-screen.R
```

## Run the complete campaign

```sh
RAYSHADER_SCREEN_OUTPUT_DIR=/path/to/road-profile-visual-screen \
Rscript tools/run-road-profile-visual-screen.R
```

The script checkpoints after every image. Repeating the same command verifies
the stored image hashes and skips completed renders. A scene that cannot solve
or construct records `blocked_before_render` and the error message, then the
campaign continues. Use a new output directory whenever the implementation,
input fixtures, image dimensions, seed, sample count, or vertical exaggeration
changes; the driver rejects mixed evidence.

To split an overnight campaign into batches, select cells without reinstalling
the package:

```sh
RAYSHADER_SCREEN_OUTPUT_DIR=/path/to/road-profile-visual-screen \
RAYSHADER_SCREEN_CELLS=H01,H02,H03,H04,H05 \
Rscript tools/run-road-profile-visual-screen.R
```

H00 is added automatically whenever it is not listed, ensuring that every
batch has the independently generated shared baseline. Later batches can use
the same output directory because cell selection is not part of the immutable
render configuration.

For a short harness check, limit both cells and scenes:

```sh
RAYSHADER_SCREEN_OUTPUT_DIR=/private/tmp/road-profile-visual-screen-pilot \
RAYSHADER_SCREEN_CELLS=H04 \
RAYSHADER_SCREEN_SCENES=synthetic_cross_slope \
RAYSHADER_SCREEN_SAMPLES=1 \
RAYSHADER_SCREEN_WIDTH=400 \
RAYSHADER_SCREEN_HEIGHT=300 \
Rscript tools/run-road-profile-visual-screen.R
```

## Runtime inputs

| Environment variable | Default | Purpose |
|---|---|---|
| `RAYSHADER_SCREEN_INPUT_DIR` | frozen campaign input directory | DEM and road fixtures |
| `RAYSHADER_SCREEN_OUTPUT_DIR` | `road-profile-visual-screen` | all evidence and checkpoints |
| `RAYSHADER_SCREEN_CELLS` | `all` | comma-separated H00-H80 cells |
| `RAYSHADER_SCREEN_SCENES` | all four scenes | comma-separated scene IDs |
| `RAYSHADER_SCREEN_SAMPLES` | `8` | `render_highquality()` samples |
| `RAYSHADER_SCREEN_WIDTH` | `1200` | image width |
| `RAYSHADER_SCREEN_HEIGHT` | `800` | image height |
| `RAYSHADER_SCREEN_VERTICAL_EXAGGERATION` | `2` | spatial-scene vertical exaggeration |
| `RAYSHADER_SCREEN_SEED` | `20260816` | fixed renderer seed |
| `RAYSHADER_SCREEN_PARALLEL_RENDER` | `true` | multithread the path tracer |
| `RAYSHADER_SCREEN_PARALLEL_MESH` | `false` | parallel road mesh construction |
| `RAYSHADER_SCREEN_VERBOSE` | `true` | package progress output |
| `RAYSHADER_SCREEN_OVERWRITE` | `false` | rerender selected completed images |
| `RAYSHADER_SCREEN_DRY_RUN` | `false` | validate and write manifests only |
| `RAYSHADER_SCREEN_GRADE_RATE_LEVELS` | `0.001,0.005,0.02` | three grade-rate levels |
| `RAYSHADER_SCREEN_MAXIMUM_GRADE_LEVELS` | `0.07,0.15,Inf` | three steep-terrain absolute-grade levels |
| `RAYSHADER_SCREEN_CONTINUATION_LEVELS` | `0,0.03,0.14` | three continuation levels |
| `RAYSHADER_SCREEN_TERRAIN_REFERENCE_LEVELS` | `0.001,0.03,1` | three terrain-weight levels |

Changing factor levels changes `factor-design.csv`; use a new output directory
for that design.

## Reviewing the visual sensitivity analysis

Open `<output-dir>/index.html`. It provides two complementary views:

- per-cell pages with native-size contact sheets and flip MP4s; and
- per-camera sensitivity pages that hold framing constant while showing all 81
  factor cells, their magenta overlays, and links to their flip videos.

The driver also creates `visual-inspection.csv` with one row for every
candidate/camera combination. Its visual fields are intentionally blank and
`opened_and_inspected` is initially `FALSE`; they are for the reviewer to fill
after opening the full-resolution source, overlay, and video. A black
difference is not treated as a conclusion: the manifest records the runtime
factor values, implementation fingerprint, and road-mesh fingerprint so an
unchanged result can be distinguished from stale or shared geometry.
