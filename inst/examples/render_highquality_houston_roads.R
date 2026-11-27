library(rayshader)
library(terra)
library(sf)
library(elevatr)
library(osmdata)

output_dir = Sys.getenv(
  "RAYSHADER_ROAD_EXAMPLE_DIR",
  unset = file.path(getwd(), "houston-road-renders")
)
cache_dir = Sys.getenv(
  "RAYSHADER_ROAD_CACHE_DIR",
  unset = file.path(tempdir(), "rayshader-houston-roads")
)
samples = as.integer(Sys.getenv("RAYSHADER_ROAD_SAMPLES", unset = "64"))
image_width = as.integer(Sys.getenv("RAYSHADER_ROAD_WIDTH", unset = "1200"))
image_height = as.integer(Sys.getenv("RAYSHADER_ROAD_HEIGHT", unset = "675"))
max_segments_per_lane = as.numeric(
  Sys.getenv("RAYSHADER_ROAD_SEGMENTS_PER_LANE", unset = "Inf")
)
if (
  is.na(max_segments_per_lane) ||
    max_segments_per_lane <= 0 ||
    (is.finite(max_segments_per_lane) &&
      max_segments_per_lane != floor(max_segments_per_lane))
) {
  stop("RAYSHADER_ROAD_SEGMENTS_PER_LANE must be a positive integer or Inf")
}
max_buildings = as.integer(
  Sys.getenv("RAYSHADER_ROAD_BUILDINGS", unset = "10000")
)
max_trees = as.integer(Sys.getenv("RAYSHADER_ROAD_TREES", unset = "500"))
road_layer_height = as.numeric(
  Sys.getenv("RAYSHADER_ROAD_LAYER_HEIGHT", unset = "5.5")
)
if (!is.finite(road_layer_height) || road_layer_height <= 0) {
  stop("RAYSHADER_ROAD_LAYER_HEIGHT must be a positive number")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

bbox_ll = c(
  xmin = -95.381,
  ymin = 29.740,
  xmax = -95.347,
  ymax = 29.765
)
target_crs = "EPSG:32615"
scene_ll = st_as_sfc(st_bbox(bbox_ll, crs = st_crs(4326))) |>
  st_as_sf()
scene = st_transform(scene_ll, target_crs)

# roads_file = file.path(cache_dir, "houston_osm_roads.rds")
# if (file.exists(roads_file)) {
#   osm_roads = readRDS(roads_file)
# } else {
#   highway_types = c(
#     "motorway",
#     "motorway_link",
#     "trunk",
#     "trunk_link",
#     "primary",
#     "primary_link",
#     "secondary",
#     "tertiary"
#   )
#   osm_roads = opq(bbox = bbox_ll, timeout = 90) |>
#     add_osm_feature(key = "highway", value = highway_types) |>
#     osmdata_sf(quiet = TRUE) |>
#     getElement("osm_lines")
#   saveRDS(osm_roads, roads_file)
# }

osm_roads = readRDS("road-profile-screen-inputs/houston/roads.rds")

road_columns = c(
  "osm_id",
  "name",
  "highway",
  "lanes",
  "lanes:forward",
  "lanes:backward",
  "oneway",
  "bridge",
  "tunnel",
  "layer"
)
for (column in setdiff(road_columns, names(osm_roads))) {
  osm_roads[[column]] = NA_character_
}
roads = st_sf(
  st_drop_geometry(osm_roads)[, road_columns, drop = FALSE],
  geometry = st_geometry(osm_roads),
  crs = st_crs(osm_roads)
)

parse_osm_lane_count = function(value) {
  suppressWarnings(as.integer(sub("[^0-9].*$", "", value)))
}

lane_count = parse_osm_lane_count(roads$lanes)
lanes_forward = parse_osm_lane_count(roads[["lanes:forward"]])
lanes_backward = parse_osm_lane_count(roads[["lanes:backward"]])
directional_lane_count = rowSums(
  cbind(lanes_forward, lanes_backward),
  na.rm = TRUE
)
directional_lane_count[
  is.na(lanes_forward) & is.na(lanes_backward)
] = NA_integer_
roads$lane_count = ifelse(
  is.na(lane_count),
  directional_lane_count,
  lane_count
)
# roads$render_layer = suppressWarnings(as.integer(roads$layer))
# is_bridge = !is.na(roads$bridge) &
#   nzchar(roads$bridge) &
#   roads$bridge != "no"
# is_tunnel = !is.na(roads$tunnel) &
#   nzchar(roads$tunnel) &
#   roads$tunnel != "no"
# roads$render_layer[is.na(roads$render_layer) & is_bridge] = 1L
# roads$render_layer[is.na(roads$render_layer) & is_tunnel] = -1L
# roads = roads[
#   is.finite(roads$lane_count) &
#     roads$lane_count >= 1 &
#     roads$lane_count <= 6,
# ]
# roads = st_transform(roads, target_crs)
# roads = suppressWarnings(st_crop(
#   roads,
#   st_bbox(st_buffer(scene, dist = -50))
# ))
# roads$length_m = as.numeric(st_length(roads))
# road_indices = split(seq_len(nrow(roads)), roads$lane_count)
# road_indices = unlist(lapply(road_indices, function(index) {
#   index = index[order(roads$length_m[index], decreasing = TRUE)]
#   if (is.finite(max_segments_per_lane)) {
#     index = head(index, max_segments_per_lane)
#   }
#   index
# }))
# roads = roads[road_indices, ]

message("Selected OSM roads by explicit lane count:")
print(table(roads$lane_count))

# buildings_file = file.path(cache_dir, "houston_osm_buildings.rds")
# if (file.exists(buildings_file)) {
#   osm_buildings = readRDS(buildings_file)
# } else {
#   osm_buildings = opq(bbox = bbox_ll, timeout = 90) |>
#     add_osm_feature(key = "building") |>
#     osmdata_sf(quiet = TRUE) |>
#     getElement("osm_polygons")
#   saveRDS(osm_buildings, buildings_file)
# }

# Overture supplies additional heights, including USGS lidar estimates.
# Release and per-building source details are preserved in the cached data.
osm_buildings = readRDS(
  "road-profile-screen-inputs/houston/buildings_overture.rds"
)

building_columns = c(
  "osm_id",
  "name",
  "building",
  "height",
  "building:height",
  "building:levels"
)
for (column in setdiff(building_columns, names(osm_buildings))) {
  osm_buildings[[column]] = NA_character_
}
buildings = st_sf(
  st_drop_geometry(osm_buildings)[, building_columns, drop = FALSE],
  geometry = st_geometry(osm_buildings),
  crs = st_crs(osm_buildings)
)

parse_osm_length = function(value) {
  height = suppressWarnings(as.numeric(sub(
    "^\\s*([0-9.]+).*$",
    "\\1",
    value
  )))
  height[grepl("ft|feet|'", value, ignore.case = TRUE)] =
    height[grepl("ft|feet|'", value, ignore.case = TRUE)] * 0.3048
  height
}

building_height = parse_osm_length(buildings$height)
building_height_fallback = parse_osm_length(buildings[["building:height"]])
building_height[is.na(building_height)] = building_height_fallback[
  is.na(building_height)
]
building_levels = parse_osm_lane_count(buildings[["building:levels"]])
building_height[is.na(building_height)] =
  building_levels[is.na(building_height)] * 3.2
buildings$height_m = ifelse(
  is.finite(building_height) & building_height > 0,
  building_height,
  8
)
buildings = st_transform(buildings, target_crs)
buildings = suppressWarnings(st_crop(
  buildings,
  st_bbox(st_buffer(scene, dist = -50))
))
buildings$area_m2 = as.numeric(st_area(buildings))
buildings = buildings[
  head(order(buildings$area_m2, decreasing = TRUE), max_buildings),
]

# trees_file = file.path(cache_dir, "houston_osm_trees.rds")
# if (file.exists(trees_file)) {
#   osm_trees = readRDS(trees_file)
# } else {
#   osm_trees = opq(bbox = bbox_ll, timeout = 90) |>
#     add_osm_feature(key = "natural", value = "tree") |>
#     osmdata_sf(quiet = TRUE) |>
#     getElement("osm_points")
#   saveRDS(osm_trees, trees_file)
# }

osm_trees = readRDS(
  "road-profile-screen-inputs/houston/trees.rds"
)
trees = st_sf(
  osm_id = osm_trees$osm_id,
  geometry = st_geometry(osm_trees),
  crs = st_crs(osm_trees)
) |>
  st_transform(target_crs)
trees = suppressWarnings(st_crop(
  trees,
  st_bbox(st_buffer(scene, dist = -50))
))
trees = head(trees, max_trees)
trees$height_m = 16 + (seq_len(nrow(trees)) - 1) %% 10
trees$crown_width_ratio = 0.42 +
  0.04 * ((seq_len(nrow(trees)) - 1) %% 4)
trees$angle_z = ((seq_len(nrow(trees)) - 1) * 137.5) %% 360

water = readRDS("road-profile-screen-inputs/houston/water.rds") |>
  st_transform(target_crs)
water = suppressWarnings(st_crop(
  water,
  st_bbox(st_buffer(scene, dist = -50))
))
# Join adjoining river sections before sampling a level for each water body.
water = st_sf(geometry = st_cast(st_union(water), "POLYGON"))
water = water[!st_is_empty(water), ]

streams = readRDS("road-profile-screen-inputs/houston/streams.rds") |>
  st_transform(target_crs)
streams = suppressWarnings(st_crop(
  streams,
  st_bbox(st_buffer(scene, dist = -50))
))
streams = st_collection_extract(streams, "LINESTRING", warn = FALSE)
streams = streams[!st_is_empty(streams) & as.numeric(st_length(streams)) > 1, ]
streams$width_m = parse_osm_length(streams$width)
missing_stream_width = !is.finite(streams$width_m) | streams$width_m <= 0
# Use mapped widths when available, otherwise illustrative widths in meters.
streams$width_m[missing_stream_width] = ifelse(
  streams$waterway[missing_stream_width] == "river",
  20,
  3
)

trails = readRDS("road-profile-screen-inputs/houston/trails.rds") |>
  st_transform(target_crs)
trails = suppressWarnings(st_crop(
  trails,
  st_bbox(st_buffer(scene, dist = -50))
))
trails = st_collection_extract(trails, "LINESTRING", warn = FALSE)
trails = trails[!st_is_empty(trails) & as.numeric(st_length(trails)) > 1, ]
trails$width_m = parse_osm_length(trails$width)
missing_trail_width = !is.finite(trails$width_m) | trails$width_m <= 0
trail_default_width = ifelse(trails$highway == "cycleway", 3, 2)
trail_default_width[trails$highway == "pedestrian"] = 4
trails$width_m[missing_trail_width] = trail_default_width[missing_trail_width]

dem_file = file.path(cache_dir, "houston_dem.tif")
update_dem_cache = FALSE
if (file.exists(dem_file)) {
  dem = rast(dem_file)
} else {
  dem_download_area = st_buffer(scene, dist = 250)
  dem = get_elev_raster(
    locations = dem_download_area,
    z = 13,
    src = "aws",
    clip = "bbox",
    tmp_dir = cache_dir
  ) |>
    rast() |>
    project(target_crs, method = "bilinear") |>
    crop(vect(scene), snap = "out")
  update_dem_cache = TRUE
}

missing_dem_cells = sum(is.na(values(dem, mat = FALSE)))
if (missing_dem_cells > 0) {
  message(sprintf(
    "Filling %i empty DEM edge cells created during reprojection",
    missing_dem_cells
  ))
  while (missing_dem_cells > 0) {
    filled_dem = focal(
      dem,
      w = 3,
      fun = "mean",
      na.policy = "only",
      na.rm = TRUE
    )
    next_missing_dem_cells = sum(is.na(values(filled_dem, mat = FALSE)))
    if (next_missing_dem_cells >= missing_dem_cells) {
      stop("Could not fill all empty DEM cells")
    }
    dem = filled_dem
    missing_dem_cells = next_missing_dem_cells
  }
  update_dem_cache = TRUE
}
names(dem) = "elevation_m"
if (update_dem_cache) {
  writeRaster(dem, dem_file, overwrite = TRUE)
}

# render_water() takes an elevation raster whose finite cells define water.
# Omit polygons smaller than one DEM cell rather than enlarging tiny fountains.
water = water[as.numeric(st_area(water)) >= prod(res(dem)), ]
water_level_rast = NULL
if (nrow(water)) {
  water$water_level_m = extract(
    dem,
    vect(water),
    fun = median,
    na.rm = TRUE,
    touches = TRUE
  )$elevation_m
  water = water[is.finite(water$water_level_m), ]
}
if (nrow(water)) {
  water_level_rast = rasterize(
    vect(water),
    dem,
    field = "water_level_m",
    fun = "min",
    touches = TRUE
  )
  # Lower each water body's terrain enough to leave at least 0.5 m of water.
  # These display offsets preserve the terrain shape beneath each polygon.
  water_terrain_max = extract(
    dem,
    vect(water),
    fun = max,
    na.rm = TRUE,
    touches = TRUE
  )$elevation_m
  dem = shift_terrain(
    dem,
    water,
    amount = water$water_level_m - water_terrain_max - 0.5,
    fun = "min"
  )
}

building_centroids = suppressWarnings(st_centroid(buildings))
building_elevation = extract(dem, vect(building_centroids))$elevation_m
building_elevation[!is.finite(building_elevation)] = minmax(dem)[1, 1]
buildings$base_m = building_elevation
buildings$top_m = buildings$base_m + buildings$height_m

vertical_exaggeration = 1
dem |>
  height_shade(
    texture = colorRampPalette(c("#798d62", "#d7d9bb"))(256)
  ) |>
  plot_3d(
    heightmap = dem,
    vertical_exaggeration = vertical_exaggeration,
    solid = FALSE,
    shadow = FALSE,
    background = "#8e9b8e",
    theta = -35,
    phi = 45,
    zoom = 0.65,
    fov = 45,
    window_size = c(image_width, image_height)
  )

if (!is.null(water_level_rast)) {
  message(sprintf("Adding %i water polygons", nrow(water)))
  render_water(
    water_input = water_level_rast,
    water_color = "#497c83",
    water_alpha = 0.8,
    water_render_method = "polygon",
    water_edge_extension = 0.5,
    clear_previous = TRUE
  )
}

if (nrow(streams)) {
  message(sprintf("Adding %i stream and bayou lines", nrow(streams)))
  render_streams(
    streams,
    water_polygons = water,
    water_color = "#497c83",
    width_column = "width_m",
    width_units = "meters",
    clear_previous = TRUE
  )
}

if (nrow(trails)) {
  message(sprintf("Adding %i walking and cycling paths", nrow(trails)))
  render_trails(
    trails,
    color = "#777",
    width_column = "width_m",
    width_units = "meters",
    clear_previous = TRUE
  )
}

message(sprintf(
  "Adding %i roads with local OSM layer ordering",
  nrow(roads)
))
render_roads(
  roads,
  road_color = "#303236",
  lanes = "lane_count",
  lane_texture = TRUE,
  layer = "layer",
  layer_height = road_layer_height
)

message(sprintf("Adding %i OSM buildings", nrow(buildings)))
raybevel_roofs_available = requireNamespace("raybevel", quietly = TRUE) &&
  tryCatch(
    {
      backend_test_polygon = st_sf(
        geometry = st_sfc(st_polygon(list(rbind(
          c(0, 0),
          c(10, 0),
          c(10, 10),
          c(0, 10),
          c(0, 0)
        ))))
      )
      backend_test_skeleton = raybevel::skeletonize(
        backend_test_polygon,
        progress = FALSE
      )
      raybevel::generate_roof(
        backend_test_skeleton,
        progress = FALSE
      )
      TRUE
    },
    error = function(error) FALSE
  )
if (raybevel_roofs_available) {
  render_buildings(
    buildings,
    data_column_top = "height_m",
    material = "#e5e5e5",
    angle = 10,
    roof_material = "#e5e5e5",
    clear_previous = TRUE
  )
} else {
  message("raybevel roof backend unavailable; using flat-roof buildings")
  render_polygons(
    buildings,
    bottom = 0,
    data_column_top = "top_m",
    data_column_bottom = "base_m",
    color = "#dedccf",
    lit = TRUE
  )
}

# Place the R logo on the Hilton roof, at half the hotel's height.
hilton = buildings[buildings$name %in% "Hilton Americas-Houston", ]
r_logo_y = range(rayvertex::read_obj(rayrender::r_obj())$vertices[[1]][, 2])
r_logo_scale_m = hilton$height_m / (2 * diff(r_logo_y))
render_obj(
  rayrender::r_obj(),
  location = st_centroid(st_geometry(hilton)),
  altitude = hilton$top_m - r_logo_y[1] * r_logo_scale_m,
  scale = rep(r_logo_scale_m / get_scene_metadata()$effective_zscale, 3),
  load_material = TRUE,
  rgl_tag = "_hilton_r",
  clear_previous = TRUE
)

message(sprintf("Adding %i mapped OSM trees", nrow(trees)))
render_tree(
  location = trees,
  type = "cone",
  tree_height_column = "height_m",
  crown_width_ratio = trees$crown_width_ratio,
  trunk_height_ratio = 0.14,
  crown_color = "#185d3d",
  trunk_color = "#33261b",
  # angle = cbind(0, 0, trees$angle_z),
  lit = TRUE,
  clear_previous = TRUE
)

# Scattered fair-weather cumulus: dimensions and bases above ground in meters.
# Spread clouds over downtown and beyond the map edges, leaving clear sky gaps.
cloud_layout = data.frame(
  x_m = c(-1900, -350, 1300, -1450, 550, 2100, -3200),
  z_m = c(-1500, -1800, -1100, 450, 250, 1350, 900),
  base_m = c(1200, 1250, 1150, 1300, 1200, 1350, 1250) / 2,
  width_m = c(1400, 1050, 1700, 1250, 1500, 1100, 1800),
  height_m = c(450, 350, 550, 400, 500, 350, 600),
  depth_m = c(950, 850, 1200, 1000, 1150, 800, 1300),
  seed = c(42, 17, 81, 123, 209, 314, 503)
)
scene_metadata = get_scene_metadata()
meters_per_unit = scene_metadata$meters_per_scene_unit
# render_highquality() centers this unpanned scene on the rgl bounding box.
scene_y_offset = mean(rgl::par3d("bbox")[3:4])
scene_ground_y = median(terra::values(dem), na.rm = TRUE) /
  scene_metadata$effective_zscale -
  scene_y_offset
houston_clouds = NULL
for (i in seq_len(nrow(cloud_layout))) {
  houston_clouds = rayrender::add_object(
    houston_clouds,
    rayrender::cloud(
      x = cloud_layout$x_m[i] / meters_per_unit,
      y = scene_ground_y +
        (cloud_layout$base_m[i] + cloud_layout$height_m[i] / 2) /
          meters_per_unit,
      z = cloud_layout$z_m[i] / meters_per_unit,
      width = cloud_layout$width_m[i] / meters_per_unit,
      height = cloud_layout$height_m[i] / meters_per_unit,
      depth = cloud_layout$depth_m[i] / meters_per_unit,
      style = "cumulus",
      seed = cloud_layout$seed[i],
      resolution = 128,
      coverage = 0.6,
      detail = 0.35,
      optical_depth = 10
    )
  )
}
houston_scene = render_highquality(
  light = FALSE,
  scene_elements = houston_clouds,
  return_scene = TRUE
)
library(rayrender)
houston_scene |>
  add_infinite_light(sky_light(
    long = -95.381,
    lat = 29.740,
    datetime = as.POSIXct("2026-09-13 19:30:00", tz = "America/Chicago"),
    planets = TRUE,
    moon = TRUE,
    haze = TRUE,
    meters_per_unit = meters_per_unit,
    atmosphere_origin = c(0, -scene_y_offset, 0),
    query_altitude = TRUE,
  )) |>
  render_scene(
    lookfrom = c(-40.06, -7.44, -20.89),
    lookat = c(76.89, -7.77, -30.48),
    fov = 72.5,
    integrator_type = "nee"
  )

houston_scene |>
  add_infinite_light(sky_light(
    long = -95.381,
    lat = 29.740,
    datetime = as.POSIXct("2026-09-13 18:00:00", tz = "America/Chicago"),
    planets = TRUE,
    haze = TRUE,
    meters_per_unit = meters_per_unit,
    atmosphere_origin = c(0, -scene_y_offset, 0),
    moon = TRUE
  )) |>
  render_scene(
    lookfrom = c(-40.06, -7.44, -20.89),
    lookat = c(76.89, -7.77, -30.48),
    fov = 72.5,
    integrator_type = "nee",
    width = 1600,
    height = 800
  )
views = list(
  overview = list(theta = -35, phi = 45, zoom = 0.5, fov = 45),
  closeup = list(theta = -20, phi = 32, zoom = 0.38, fov = 45),
  lane_detail = list(theta = -10, phi = 70, zoom = 0.18, fov = 35)
)
requested_views = strsplit(
  Sys.getenv(
    "RAYSHADER_ROAD_VIEWS",
    unset = paste(names(views), collapse = ",")
  ),
  ",",
  fixed = TRUE
)[[1]] |>
  trimws()
views = views[intersect(requested_views, names(views))]
if (!length(views)) {
  stop("RAYSHADER_ROAD_VIEWS did not contain a recognized view name")
}
for (view_name in names(views)) {
  message(sprintf("Rendering %s view", view_name))
  do.call(render_camera, views[[view_name]])
  render_highquality(
    filename = file.path(
      output_dir,
      sprintf("houston_roads_%s.png", view_name)
    ),
    samples = samples,
    width = image_width,
    heigPOSIXct("2026-09-13 18:00:00", tz = "America/Chicago"),
    ht = image_height,
    scene_elements = houston_clouds,
    integrator_type = "nee",
    light = TRUE,
    light_direction = 315,
    light_altitude = 35,
    light_size = 5,
    light_intensity = 450,
    clamp_value = 10,
    joined_stream_mesh = TRUE,
    use_extruded_paths = TRUE,
    water_material = "microfacet",
    cache_scene = TRUE,
    backgroundhigh = "#8e9b8e",
    backgroundlow = "#8e9b8e"
  )
}

rgl::close3d()


render_movie_hq(
  keyframes = kf,
  camera_motion_blur = TRUE,
  render_highquality_args = list(
    long = -95.381,
    lat = 29.740,
    camera_motion_blur = TRUE,
    scene_elements = houston_clouds,
    datetime = as.POSIXct("2026-09-13 18:00:00", tz = "America/Chicago")
  ),
  width = 800,
  height = 400,
  integrator_type = "nee",
  iso = 4,
  samples = 32,
  filename = "houstcloud"
)

render_highquality(
  long = -95.381,
  lat = 29.740,
  scene_elements = houston_clouds,
  datetime = as.POSIXct("2026-09-13 18:00:00", tz = "America/Chicago"),
  width = 800,
  height = 400,
  sky_haze = TRUE,
  sky_query_altitude = TRUE,
  iso = 4,
  samples = 32,
  defer = TRUE
)

render_highquality(
  long = -95.381,
  lat = 29.740,
  # scene_elements = houston_clouds,
  datetime = as.POSIXct("2026-09-13 18:00:00", tz = "America/Chicago"),
  width = 800,
  height = 400,
  sky_haze = TRUE,
  sky_query_altitude = TRUE,
  iso = 4,
  samples = 32,
  defer = TRUE
)
