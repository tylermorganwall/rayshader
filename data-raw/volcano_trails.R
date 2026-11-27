if (!requireNamespace("sf", quietly = TRUE)) {
  stop("The `sf` package is required to build the volcano trail data.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The `jsonlite` package is required to build the volcano trail data.")
}

walking_highways = c(
  "bridleway",
  "footway",
  "path",
  "pedestrian",
  "steps",
  "track"
)
# Pad the retrieval area so ways crossing the DEM edge are included before the
# exact projected crop below.
volcano_bbox_wgs84 = c(
  xmin = 174.760,
  ymin = -36.881,
  xmax = 174.770,
  ymax = -36.871
)
overpass_query = paste0(
  "[out:json][timeout:90];",
  "way[\"highway\"](",
  paste(volcano_bbox_wgs84[c("ymin", "xmin", "ymax", "xmax")], collapse = ","),
  ");out geom;"
)

source_file = Sys.getenv("RAYSHADER_VOLCANO_OSM_SOURCE", unset = "")
if (!nzchar(source_file)) {
  source_file = tempfile(fileext = ".json")
  overpass_url = paste0(
    "https://overpass-api.de/api/interpreter?data=",
    utils::URLencode(overpass_query, reserved = TRUE)
  )
  utils::download.file(overpass_url, source_file, mode = "wb")
}
if (!file.exists(source_file)) {
  stop("Could not find the volcano OpenStreetMap source data.")
}

osm = jsonlite::fromJSON(source_file, simplifyVector = FALSE)
ways = Filter(
  function(element) {
    identical(element$type, "way") && length(element$geometry) >= 2L
  },
  osm$elements
)
read_tag = function(element, tag) {
  value = element$tags[[tag]]
  if (is.null(value)) NA_character_ else as.character(value)
}
geometry = lapply(ways, function(element) {
  coordinates = do.call(
    rbind,
    lapply(element$geometry, function(point) {
      c(point$lon, point$lat)
    })
  )
  sf::st_linestring(coordinates)
})
volcano_trails = sf::st_sf(
  osm_id = vapply(
    ways,
    function(element) {
      as.character(element$id)
    },
    character(1)
  ),
  highway = vapply(ways, read_tag, character(1), "highway"),
  name = vapply(ways, read_tag, character(1), "name"),
  foot = vapply(ways, read_tag, character(1), "foot"),
  surface = vapply(ways, read_tag, character(1), "surface"),
  access = vapply(ways, read_tag, character(1), "access"),
  sac_scale = vapply(ways, read_tag, character(1), "sac_scale"),
  geometry = sf::st_sfc(geometry, crs = 4326)
)

volcano_trails = volcano_trails[
  volcano_trails$highway %in%
    walking_highways &
    (is.na(volcano_trails$access) |
      volcano_trails$access %in% c("yes", "permissive", "designated")) &
    (is.na(volcano_trails$foot) |
      !volcano_trails$foot %in% c("no", "private")),
]

sf::sf_proj_network(TRUE)
volcano_trails = sf::st_transform(volcano_trails, 27200)
volcano_extent = sf::st_bbox(
  c(
    xmin = 2667400,
    ymin = 6478700,
    xmax = 2668010,
    ymax = 6479570
  ),
  crs = sf::st_crs(27200)
)
volcano_trails = suppressWarnings(
  sf::st_crop(volcano_trails, volcano_extent)
)
volcano_trails = volcano_trails[!sf::st_is_empty(volcano_trails), ]
volcano_trails = suppressWarnings(
  sf::st_cast(volcano_trails, "MULTILINESTRING")
)
volcano_trails = suppressWarnings(
  sf::st_cast(volcano_trails, "LINESTRING")
)
volcano_trails = volcano_trails[
  order(as.numeric(volcano_trails$osm_id)),
]
row.names(volcano_trails) = NULL

save(
  volcano_trails,
  file = "data/volcano_trails.rda",
  compress = "xz"
)
