#'@title Generate Altitude Overlay
#'
#'@description Using a hillshade and the height map, generates a semi-transparent hillshade to
#'layer onto an existing map.
#'
#'@param hillshade  The hillshade to transition into.
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All grid points are assumed to be evenly spaced.
#'@param start_transition Elevation above which `hillshade` is completely transparent. 
#'@param end_transition Default `NULL`. Elevation below which `hillshade` is completely opaque. By default, this is equal to `start_transition`.
#'@param lower Default `TRUE`. This makes `hillshade` completely opaque below `start_transition`. If
#'`FALSE`, the direction will be reversed.
#'@return 4-layer RGB array representing the semi-transparent hillshade.
#'@export
#'@examples
#'#Create a bathymetric hillshade
#'#Only run these examples if the `magick` package is installed.
#'if ("magick" %in% rownames(utils::installed.packages())) {
#'\donttest{
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'plot_map(bathy_hs)
#'
#'#Set everything below 0m to water palette
#'montereybay %>%
#'  sphere_shade(zscale=10) %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'
#'#Add snow peaks by setting `lower = FALSE`  
#'snow_palette = "white"
#'snow_hs = height_shade(montereybay, texture = snow_palette)
#'
#'#Set the snow transition region from 500m to 1200m
#'montereybay %>%
#'  sphere_shade(zscale=10, texture = "desert") %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_overlay(generate_altitude_overlay(snow_hs, montereybay, 500, 1200, lower=FALSE))  %>%
#'  add_shadow(ambient_shade(montereybay,zscale=50,maxsearch=100),0) %>%
#'  plot_map()
#'}
#'}
generate_altitude_overlay = function(hillshade, heightmap, 
                                       start_transition, end_transition=NULL, lower = TRUE) {
  if(is.null(end_transition)) {
    end_transition = start_transition
  }
  if(length(dim(hillshade)) == 2) {
    hillarray = array(0,dim=c(nrow(hillshade),ncol(hillshade),4))
    hillarray[,,1] = hillshade
    hillarray[,,2] = hillshade
    hillarray[,,3] = hillshade
    hillshade = hillarray
  }
  if(dim(hillshade)[3] == 3) {
    temp_hillshade = array(1, dim = c(dim(hillshade)[1:2],4))
    temp_hillshade[,,4] = 1
    temp_hillshade[,,1:3] = hillshade
    hillshade = temp_hillshade
  }
  heightmap = t(heightmap)
  if(any(dim(heightmap) != dim(hillshade)[1:2])) {
    heightmap = rayimage::render_resized(heightmap, dims =  dim(hillshade)[1:2])
  }
  trans_map = heightmap
  if(start_transition == end_transition) {
    if(!lower) {
      trans_map_temp = (trans_map - start_transition)
      trans_map[trans_map_temp < 0] = 0
      trans_map[trans_map_temp >= 0] = 1
    } else {
      trans_map_temp = (trans_map - start_transition)
      trans_map[trans_map_temp < 0] = 1
      trans_map[trans_map_temp >= 0] = 0
    }
    hillshade[,,4] = trans_map
    return(hillshade)
  } 
  if(lower) {
    transition_region = end_transition - start_transition
    trans_map_temp = (trans_map - start_transition) / transition_region
    trans_map = 1 - trans_map_temp
    trans_map[trans_map_temp < 0] = 1
    trans_map[trans_map_temp >= 1] = 0
  } else {
    transition_region = end_transition - start_transition
    trans_map_temp = (trans_map - start_transition) / transition_region
    trans_map = trans_map_temp
    trans_map[trans_map_temp < 0] = 0
    trans_map[trans_map_temp >= 1] = 1
  } 
  hillshade[,,4] = trans_map
  return(hillshade)
}
