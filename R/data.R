#' Monterey Bay combined topographic and bathymetric elevation matrix
#'
#' This dataset is a downsampled matrix version of a combined topographic and
#' bathymetric elevation raster representing the Monterey Bay, CA region.
#' Original data are from the NOAA National Centers for Environmental
#' Information.
#'
#' @format A matrix with 540 rows and 540 columns. Elevation values are in
#' NAVD88 meters. Spatial extent and CRS metadata are stored in the `"extent"`
#' and `"crs"` attributes, and the `"rayshader_data"` attribute is `TRUE`.
#' @source https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:3544/html
#' @name montereybay
#' @docType data
#' @keywords datasets
#' @usage montereybay
#' @export
#' @examples
#' # This is the full code (commented out) used to generate this dataset from
#' # the original NOAA data:
#' #montbay = terra::rast("~/Downloads/monterey_13_navd88_2012.nc")
#' #bottom_left = c(y = -122.366765, x = 36.179392)
#' #top_right   = c(y = -121.366765, x = 37.179392)
#' #extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right),
#' #                 proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#' #monterey_cropped = terra::crop(montbay, extent_latlong)
#' #montereybay_raster = terra::aggregate(monterey_cropped, 20)
#' #montereybay = raster_to_matrix(montereybay_raster)
"montereybay"

#' Monterey Bay spatial elevation raster
#'
#' A spatial DEM constructed from [`montereybay`] when rayshader is loaded.
#' It contains the same elevation values as the matrix and supplies its extent
#' and CRS metadata directly to spatial operations.
#'
#' @format A one-layer `terra::SpatRaster` with 540 rows and 540 columns.
#' Elevation values are in NAVD88 meters. The raster uses a WGS84 longitude and
#' latitude CRS (EPSG:4326), has approximately 0.001851852 degree cell spacing,
#' and covers approximately -122.366806 to -121.366806 longitude and 36.179398
#' to 37.179398 latitude.
#' @source https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:3544/html
#' @name montereybay_spatial
#' @docType data
#' @keywords datasets
#' @usage montereybay_spatial
#' @export
montereybay_spatial = NULL

#' California County Data Around Monterey Bay
#'
#' This dataset is an `sf` object containing polygon data from the U.S. Department of Commerce
#' with selected geographic and cartographic information from the U.S. Census Bureau's Master
#' Address File / Topologically Integrated Geographic Encoding and Referencing (MAF/TIGER)
#' Database (MTDB). This data has been trimmed to only include 26 features in the extent of
#' the `montereybay` dataset.
#'
#' @format An `sf` object with MULTIPOLYGON geometry.
#' @source https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-california-current-county-subdivision-state-based
#' @examples
#' # This is the full code (commented out) used to generate this dataset from the original data:
#' #counties = sf::st_read("tl_2016_06_cousub.shp")
#' #monterey_bbox = sf::st_bbox(c(
#' #  xmin = terra::xmin(montereybay_spatial),
#' #  ymin = terra::ymin(montereybay_spatial),
#' #  xmax = terra::xmax(montereybay_spatial),
#' #  ymax = terra::ymax(montereybay_spatial)
#' #), crs = sf::st_crs(terra::crs(montereybay_spatial)))
#' #monterey_counties_sf = sf::st_crop(counties, monterey_bbox)
"monterey_counties_sf"


#' Road Data Around Monterey Bay
#'
#' This dataset is an `sf` object containing line data from the U.S. Department of Commerce
#' with selected roads, TIGER/Line Shapefile, 2015,  state, California, Primary and Secondary
#' Roads State-based Shapefile. This data has been trimmed to only include 330 features in the extent of
#' the `montereybay` dataset.
#'
#' @format An `sf` object with LINESTRING geometry.
#' @source https://www2.census.gov/geo/tiger/TIGER2015/PRISECROADS/tl_2015_06_prisecroads.zip
#' @examples
#' # This is the full code (commented out) used to generate this dataset from the original data:
#' #counties = sf::st_read("tl_2015_06_prisecroads.shp")
#' #monterey_bbox = sf::st_bbox(c(
#' #  xmin = terra::xmin(montereybay_spatial),
#' #  ymin = terra::ymin(montereybay_spatial),
#' #  xmax = terra::xmax(montereybay_spatial),
#' #  ymax = terra::ymax(montereybay_spatial)
#' #), crs = sf::st_crs(terra::crs(montereybay_spatial)))
#' #monterey_roads_sf = sf::st_crop(counties, monterey_bbox)
"monterey_roads_sf"

#' Walking Trails Around Maungawhau
#'
#' Public walking paths from OpenStreetMap clipped to the extent of
#' [volcano_spatial()]. Restricted and customer-only paths are omitted.
#'
#' @format An `sf` object with 160 LINESTRING features in EPSG:27200,
#' NZGD49 / New Zealand Map Grid. It contains `osm_id`, `highway`, `name`,
#' `foot`, `surface`, `access`, and `sac_scale` attributes.
#' @source © OpenStreetMap contributors, retrieved 2026-08-25 from
#' [OpenStreetMap](https://www.openstreetmap.org/) under the
#' [Open Data Commons Open Database License](https://opendatacommons.org/licenses/odbl/).
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   plot(sf::st_geometry(volcano_trails))
#' }
"volcano_trails"

#' Washington Monument 3D Model as Multipolygon Z Data
#'
#' This dataset is an `sf` object containing MULTIPOLYGON Z 3D data of the Washington Monument in Washington, DC.
#'
#' @format An `sf` object with MULTIPOLYGONZ geometry.
#' @source https://opendata.dc.gov/documents/DCGIS::buildings-in-3d/
#' @examples
#' # See the `render_multipolygonz()` documentation for examples of using this data.
"washington_monument_multipolygonz"
