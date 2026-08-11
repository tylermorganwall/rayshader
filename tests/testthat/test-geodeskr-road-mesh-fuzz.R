test_that("GeoDesk road fuzz bounding boxes are reproducible and bounded", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spData")
  fixture_environment = new.env(parent = globalenv())
  sys.source(
    test_path("fixtures", "geodeskr-road-mesh-fuzz.R"),
    envir = fixture_environment
  )
  regions = fixture_environment$road_fuzz_load_us_regions()
  sample_cases = function() {
    set.seed(8675309)
    lapply(seq_len(3L), function(case_index) {
      fixture_environment$road_fuzz_sample_bbox(
        regions = regions,
        case_index = case_index,
        seed = 8675309,
        min_bbox_m = 1500,
        max_bbox_m = 5000
      )
    })
  }
  first_sample = sample_cases()
  second_sample = sample_cases()
  expect_identical(first_sample, second_sample)
  sampled_cases = do.call(rbind, first_sample)
  expect_true(all(sampled_cases$width_m >= 1500))
  expect_true(all(sampled_cases$width_m <= 5000 * 1.01))
  expect_true(all(sampled_cases$height_m >= 1500))
  expect_true(all(sampled_cases$height_m <= 5000 * 1.01))
  expect_true(all(nzchar(sampled_cases$state)))
  expect_true(all(grepl("^EPSG:326", sampled_cases$target_crs)))
})

test_that("GeoDesk road fuzz replay preserves downloaded terrain", {
  skip_if_not_installed("terra")
  fixture_environment = new.env(parent = globalenv())
  sys.source(
    test_path("fixtures", "geodeskr-road-mesh-fuzz.R"),
    envir = fixture_environment
  )
  dem = terra::rast(
    nrows = 3,
    ncols = 4,
    xmin = 100,
    xmax = 140,
    ymin = 200,
    ymax = 230,
    crs = "EPSG:32619"
  )
  terra::values(dem) = seq_len(terra::ncell(dem)) + 0.25
  packed = fixture_environment$road_fuzz_pack_dem(dem)
  restored = fixture_environment$road_fuzz_unpack_dem(packed)

  expect_true(terra::compareGeom(dem, restored, stopOnError = FALSE))
  expect_identical(
    terra::values(restored, mat = FALSE),
    terra::values(dem, mat = FALSE)
  )
})

test_that("GeoDesk road meshing fuzz fixture succeeds", {
  skip_if_not(
    tolower(Sys.getenv("RAYSHADER_RUN_GEODESKR_ROAD_FUZZ")) %in%
      c("true", "1", "yes"),
    "Set RAYSHADER_RUN_GEODESKR_ROAD_FUZZ=true to run this integration fixture."
  )
  fixture_environment = new.env(parent = globalenv())
  sys.source(
    test_path("fixtures", "geodeskr-road-mesh-fuzz.R"),
    envir = fixture_environment
  )
  config = fixture_environment$road_fuzz_config(
    repo_root = normalizePath(test_path("..", ".."), mustWork = TRUE)
  )
  manifest = fixture_environment$road_fuzz_run(config)
  expect_true(nrow(manifest) >= config$random_cases)
  expect_false(any(manifest$status == "mesh_failure"))
  expect_true(all(
    manifest$mesh_count[manifest$status == "passed"] > 0L
  ))
})
