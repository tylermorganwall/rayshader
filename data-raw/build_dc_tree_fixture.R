required_packages = c("sf", "terra", "elevatr")
missing_packages = required_packages[
	!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
	stop(
		sprintf(
			"Missing required package(s): %s",
			paste(missing_packages, collapse = ", ")
		),
		call. = FALSE
	)
}

script_path = local({
	file_arg = "--file="
	matched = grep(file_arg, commandArgs(trailingOnly = FALSE), value = TRUE)
	if (!length(matched)) {
		return(normalizePath("data-raw/build_dc_tree_fixture.R", mustWork = FALSE))
	}
	normalizePath(sub(file_arg, "", matched[[1]]), winslash = "/", mustWork = TRUE)
})
repo_root = normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

source_path = file.path(
	repo_root,
	"tests", "testthat", "fixtures", "spatial", "DC_Trees.geojson"
)
points_output_path = file.path(
	repo_root,
	"tests", "testthat", "fixtures", "spatial", "dc_tree_points_4326.gpkg"
)
dem_output_path = file.path(
	repo_root,
	"tests", "testthat", "fixtures", "spatial", "dc_trees_dem_32618.tif"
)

target_crs = sf::st_crs(32618)
washington_monument_location_4326 = sf::st_sfc(
	sf::st_point(c(-77.035249, 38.889462)),
	crs = sf::st_crs(4326)
)
scene_radius_m = 2000
tree_radius_m = 500
scene_aoi_utm = sf::st_as_sf(sf::st_buffer(
	sf::st_transform(washington_monument_location_4326, target_crs),
	dist = scene_radius_m
))
subset_bbox_4326 = sf::st_bbox(sf::st_transform(scene_aoi_utm, 4326))
dem_zoom = 14L

read_dc_tree_subset = function(path, bbox_4326) {
	bbox_sfc = sf::st_as_sfc(bbox_4326)
	tree_points = suppressWarnings(sf::st_read(
		path,
		wkt_filter = sf::st_as_text(bbox_sfc),
		quiet = TRUE
	))
	if (!nrow(tree_points)) {
		stop("No DC tree points found inside the fixed monument-centered scene bbox.", call. = FALSE)
	}
	geometry_types = as.character(sf::st_geometry_type(tree_points, by_geometry = TRUE))
	if (any(geometry_types != "POINT")) {
		stop("DC tree source must contain only POINT geometries.", call. = FALSE)
	}
	if (is.na(sf::st_crs(tree_points))) {
		stop("DC tree source is missing a CRS.", call. = FALSE)
	}
	if (is.na(sf::st_crs(tree_points)$epsg) || sf::st_crs(tree_points)$epsg != 4326) {
		tree_points = sf::st_transform(tree_points, 4326)
	}
	tree_points = sf::st_filter(tree_points, bbox_sfc, .predicate = sf::st_within)
	if (!nrow(tree_points)) {
		stop("No DC tree points remained after clipping to the fixed bbox.", call. = FALSE)
	}
	tree_points
}

retain_dc_tree_points = function(tree_points, scene_center, target_crs) {
	tree_points = tree_points[is.finite(tree_points$HEIGHT) & tree_points$HEIGHT > 0, ]
	if (!nrow(tree_points)) {
		stop("No DC tree points with positive heights remained after filtering.", call. = FALSE)
	}
	tree_points_utm = sf::st_transform(tree_points, target_crs)
	scene_center_utm = sf::st_transform(scene_center, target_crs)
	tree_points_utm = tree_points_utm[
		as.numeric(sf::st_distance(tree_points_utm, scene_center_utm)) <= tree_radius_m,
	]
	if (!nrow(tree_points_utm)) {
		stop("No DC tree points remained inside the fixed 500 m monument-centered radius.", call. = FALSE)
	}
	point_coords = sf::st_coordinates(tree_points_utm)
	sort_columns = intersect(c("TREE_ID", "OBJECTID"), names(tree_points))
	order_index = do.call(order, c(
		list(point_coords[, 1], point_coords[, 2]),
		lapply(sort_columns, function(column_name) tree_points_utm[[column_name]]),
		list(na.last = TRUE)
	))
	sf::st_transform(tree_points_utm[order_index, ], 4326)
}

tree_points = read_dc_tree_subset(source_path, subset_bbox_4326)
retained_trees = retain_dc_tree_points(
	tree_points,
	scene_center = washington_monument_location_4326,
	target_crs = target_crs
)

message(sprintf("Retained %s DC tree points.", nrow(retained_trees)))
message(sprintf(
	"Retained bbox (EPSG:4326): xmin=%0.6f xmax=%0.6f ymin=%0.6f ymax=%0.6f",
	sf::st_bbox(retained_trees)[["xmin"]],
	sf::st_bbox(retained_trees)[["xmax"]],
	sf::st_bbox(retained_trees)[["ymin"]],
	sf::st_bbox(retained_trees)[["ymax"]]
))

if (file.exists(points_output_path)) {
	invisible(file.remove(points_output_path))
}
sf::st_write(retained_trees, points_output_path, quiet = TRUE)

aoi_4326 = sf::st_transform(scene_aoi_utm, 4326)

dem_raw = suppressWarnings(elevatr::get_elev_raster(
	locations = aoi_4326,
	z = dem_zoom,
	src = "aws",
	clip = "bbox",
	override_size_check = TRUE
))
dem_raster = if (inherits(dem_raw, "SpatRaster")) {
	dem_raw
} else {
	terra::rast(dem_raw)
}
source_crs_wkt = sf::st_crs(4326)$wkt
target_crs_wkt = target_crs$wkt
if (!nzchar(terra::crs(dem_raster, proj = TRUE))) {
	terra::crs(dem_raster) = source_crs_wkt
}
dem_utm = terra::project(dem_raster, target_crs_wkt, method = "bilinear")
aoi_extent_utm = sf::st_bbox(scene_aoi_utm)
dem_utm = terra::crop(
	dem_utm,
	terra::ext(
		aoi_extent_utm[["xmin"]],
		aoi_extent_utm[["xmax"]],
		aoi_extent_utm[["ymin"]],
		aoi_extent_utm[["ymax"]]
	)
)

terra::writeRaster(
	dem_utm,
	dem_output_path,
	overwrite = TRUE,
	gdal = c("COMPRESS=DEFLATE")
)

message(sprintf(
	"Wrote fixtures:\n- %s\n- %s",
	points_output_path,
	dem_output_path
))
