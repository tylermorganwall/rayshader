#' Monterey Bay combined topographic and bathymetric elevation matrix.
#'
#' This dataset is a downsampled version of a combined topographic and bathymetric 
#' elevation matrix representing the Monterey Bay, CA region. Original data from 
#' from the NOAA National Map website. 
#'
#' @format A matrix with 540 rows and 540 columns. Elevation is in meters, and the spacing between each
#' coordinate is 200 meters (zscale = 200). Water level is 0. Raster extent located in "extent" attribute. CRS located in "CRS" attribute.
#' @source https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:3544/html
#' @examples 
#' # This is the full code (commented out) used to generate this dataset from the original NOAA data:
#' #raster::raster("monterey_13_navd88_2012.nc")
#' #bottom_left = c(y=-122.366765, x=36.179392)
#' #top_right   = c(y=-121.366765, x=37.179392) 
#' #extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), 
#' #                 proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#' #monterey_cropped = raster::crop(montbay,extent_latlong)
#' #montbay_mat = raster_to_matrix(montbay_cropped)
#' #montereybay = resize_matrix(montbay_mat,0.05)
#' #attr(montereybay, "extent") = extent_latlong
#' #attr(montereybay, "crs") = crs(monterey_cropped)
#' #attr(montereybay, "crs") = crs(monterey_cropped)
#' #attr(montereybay, "rayshader_data") = TRUE
"montereybay"

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
#' #monterey_counties_sf = sf::st_crop(counties, attr(montereybay,"extent"))
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
#' #monterey_roads_sf = sf::st_crop(counties, attr(montereybay,"extent"))
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
