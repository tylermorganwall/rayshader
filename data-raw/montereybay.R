if (!requireNamespace("terra", quietly = TRUE)) {
  stop(
    "The `terra` package is required to build the Monterey Bay package data."
  )
}

# Original source generation:
# montbay = terra::rast("monterey_13_navd88_2012.nc")
# bottom_left = c(y = -122.366765, x = 36.179392)
# top_right = c(y = -121.366765, x = 37.179392)
# extent_latlong = sp::SpatialPoints(
#   rbind(bottom_left, top_right),
#   proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# )
# monterey_cropped = terra::crop(montbay, extent_latlong)
# montereybay_spatraster = terra::aggregate(monterey_cropped, 20)

read_montereybay_source = function() {
  source_file = Sys.getenv("RAYSHADER_MONTEREYBAY_SOURCE", unset = "")
  if (nzchar(source_file)) {
    if (!file.exists(source_file)) {
      stop("`RAYSHADER_MONTEREYBAY_SOURCE` does not point to an existing file.")
    }
    return(terra::rast(source_file))
  }

  if (file.exists("montereybay2.rds")) {
    return(readRDS("montereybay2.rds"))
  }

  if (file.exists("R/sysdata.rda")) {
    sysdata_env = new.env(parent = emptyenv())
    load("R/sysdata.rda", envir = sysdata_env)
    if (exists(".montereybay_packed", envir = sysdata_env, inherits = FALSE)) {
      return(terra::unwrap(get(
        ".montereybay_packed",
        envir = sysdata_env,
        inherits = FALSE
      )))
    }
  }

  if (file.exists("data/montereybay.rda")) {
    source_env = new.env(parent = emptyenv())
    load("data/montereybay.rda", envir = source_env)
    if (exists("montereybay", envir = source_env, inherits = FALSE)) {
      return(get("montereybay", envir = source_env, inherits = FALSE))
    }
  }

  stop(
    "Could not find Monterey Bay source data. Set `RAYSHADER_MONTEREYBAY_SOURCE` ",
    "to the original NOAA raster path, or provide `montereybay2.rds`."
  )
}

montereybay_source = read_montereybay_source()
if (inherits(montereybay_source, "PackedSpatRaster")) {
  montereybay_source = terra::unwrap(montereybay_source)
}

if (inherits(montereybay_source, "SpatRaster")) {
  montereybay = matrix(
    terra::values(montereybay_source, mat = FALSE),
    nrow = terra::ncol(montereybay_source),
    ncol = terra::nrow(montereybay_source)
  )
  attr(montereybay, "extent") = c(
    xmin = terra::xmin(montereybay_source),
    xmax = terra::xmax(montereybay_source),
    ymin = terra::ymin(montereybay_source),
    ymax = terra::ymax(montereybay_source)
  )
  attr(montereybay, "crs") = terra::crs(montereybay_source)
} else if (is.matrix(montereybay_source)) {
  montereybay = montereybay_source
} else {
  stop("Monterey Bay source data must be a matrix or terra spatial raster.")
}
attr(montereybay, "rayshader_data") = TRUE

save(
  montereybay,
  file = "data/montereybay.rda",
  compress = "xz"
)

sysdata_env = new.env(parent = emptyenv())
if (file.exists("R/sysdata.rda")) {
  load("R/sysdata.rda", envir = sysdata_env)
}
if (exists(".montereybay_packed", envir = sysdata_env, inherits = FALSE)) {
  rm(list = ".montereybay_packed", envir = sysdata_env)
}
save(
  list = ls(sysdata_env, all.names = TRUE),
  file = "R/sysdata.rda",
  envir = sysdata_env,
  compress = "xz"
)
