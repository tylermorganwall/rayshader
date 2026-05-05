#' Monterey Bay combined topographic and bathymetric elevation raster
#'
#' This dataset is a downsampled spatial version of a combined topographic and
#' bathymetric elevation raster representing the Monterey Bay, CA region. Original
#' data from the NOAA National Centers for Environmental Information.
#'
#' @format A one-layer `terra::SpatRaster` with 540 rows and 540 columns.
#' Elevation values are in NAVD88 meters. The raster uses a WGS84 longitude/latitude
#' CRS (EPSG:4326), has 0.001851852 degree cell spacing, and covers approximately
#' -122.366806 to -121.366806 longitude and 36.179398 to 37.179398 latitude.
#' @source https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:3544/html
#' @name montereybay
#' @docType data
#' @keywords datasets
#' @usage montereybay
#' @export
#' @examples 
#' # This is the full code (commented out) used to generate this dataset from the original NOAA data:
#' #montbay = terra::rast("~/Downloads/monterey_13_navd88_2012.nc")
#' #bottom_left = c(y = -122.366765, x = 36.179392)
#' #top_right   = c(y = -121.366765, x = 37.179392)
#' #extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), 
#' #                 proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#' #monterey_cropped = terra::crop(montbay, extent_latlong)
#' #montereybay = terra::aggregate(monterey_cropped, 20)
NULL

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
#' #  xmin = terra::xmin(montereybay), ymin = terra::ymin(montereybay),
#' #  xmax = terra::xmax(montereybay), ymax = terra::ymax(montereybay)
#' #), crs = sf::st_crs(terra::crs(montereybay)))
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
#' #  xmin = terra::xmin(montereybay), ymin = terra::ymin(montereybay),
#' #  xmax = terra::xmax(montereybay), ymax = terra::ymax(montereybay)
#' #), crs = sf::st_crs(terra::crs(montereybay)))
#' #monterey_roads_sf = sf::st_crop(counties, monterey_bbox)
"monterey_roads_sf"

#' Washington Monument 3D Model as Multipolygon Z Data 
#'
#' This dataset is an `sf` object containing MULTIPOLYGON Z 3D data of the Washington Monument in Washington, DC.
#'
#' @format An `sf` object with MULTIPOLYGONZ geometry.
#' @source https://opendata.dc.gov/documents/DCGIS::buildings-in-3d/
#' @examples 
#' # See the `render_multipolygonz()` documentation for examples of using this data.
"washington_monument_multipolygonz"
