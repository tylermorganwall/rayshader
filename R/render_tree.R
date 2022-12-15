#'@title Render Tree
#'
#'@description Adds 3D tree to the current scene, using latitude/longitude or coordinates in the reference
#'system defined by the extent object. 
#'
#'@param long Vector of longitudes (or other coordinate in the same coordinate reference system as extent).
#'@param lat Vector of latitudes (or other coordinate in the same coordinate reference system as extent).
#'@param type Default `"basic"`. Type of tree. Other built-in option: `"cone"`.
#'@param canopy_color Default `"darkgreen"`. Color(s) of the canopy.
#'@param trunk_color Default `"#964B00"` (brown). Color(s) of the trunk,
#'@param absolute_height Default `FALSE`. Default is specifying the tree height directly, relative to the 
#'underlying height map. If `TRUE`, `canopy_height` will specified by the actual altitude of the top of the tree.  
#'Total tree height will be `canopy_height + trunk_height`.
#'@param canopy_height Default `9`. Height of the canopy, in units of height map. 
#'Total tree height will be `canopy_height + trunk_height`.
#'@param canopy_width_ratio Default `1`. Ratio of the canopy width to the canopy height. `1` is spherical.
#'@param trunk_height Default `NULL`, automatically computed. Height of the trunk, from the ground.  
#'Default is 1/3rd the canopy height if `type = "basic"`, and 1/6th the canopy height if `type = "cone"`.
#'Total tree height will be `canopy_height + trunk_height`.
#'@param trunk_radius Default `NULL`, automatically computed.
#'Default is 1/5rd the trunk height if `type = "basic"`, and 1/10th the trunk height if `type = "cone"`.
#'@param tree_zscale Default `TRUE`. Whether to scale the size of the tree by zscale to have it match
#'the size of the map. If zscale is very big, this will make the trees very small.
#'@param min_height Default `NULL`. Minimum height of a tree. Set to a positive number to filter out trees 
#'below that height.
#'@param max_height Default `NA`. Maximum height of a tree. Set to a positive number to filter out trees 
#'above that height.
#'@param angle Default `c(0,0,0)`. Angle of rotation around the x, y, and z axes. If this is a matrix or list,
#'each row (or list entry) specifies the rotation of the nth tree specified (number of rows/length of list must
#'equal the length of `lat`/`long`).
#'@param extent Either an object representing the spatial extent of the 3D scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax", "ymin", "ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#'@param baseshape Default `rectangle`. Shape of the base. Options are `c("rectangle","circle","hex")`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param heightmap Default `NULL`. Automatically extracted from the rgl window--only use if auto-extraction
#'of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing trees.
#'@param ... Additional arguments to pass to `rgl::rgl.triangles()`.
#'@export
#'@examples
#'if(rayshader:::run_documentation()) {
#'#Let's first start by drawing some trees in a circle around Monterey Bay
#'#We won't scale these to a realistic size (yet)
#'moss_landing_coord = c(36.806807, -121.793332)
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,water=TRUE,
#'          shadowcolor="#40310a", background = "tan",
#'          theta=210,  phi=22, zoom=0.20, fov=55)
#'
#'t = seq(0,2*pi,length.out=20)
#'circle_coords_lat = moss_landing_coord[1] + 0.3 * sin(t)
#'circle_coords_long = moss_landing_coord[2] + 0.3 * cos(t)
#'
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, canopy_height = 30, canopy_width_ratio = 1,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the canopy width ratio (compared to the height)
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, canopy_height = 30, canopy_width_ratio = 0.5,
#'            clear_previous = TRUE, 
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the trunk height and width
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, canopy_height = 10, canopy_width_ratio = 2,
#'            clear_previous = TRUE, trunk_height=15, trunk_radius = 1.5,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the tree type
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, canopy_height = 30,
#'            clear_previous = TRUE, type = "cone",
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the canopy color:
#'render_camera(theta = 150,  phi = 38, zoom = 0.4, fov = 55)
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, canopy_height = 30, canopy_width_ratio = 1,
#'            canopy_color = rainbow(20),  trunk_height=20, 
#'            clear_previous = TRUE, 
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'rgl::rgl.close()
#'}
#'
#'#We will use the lidR package to generate a DEM and detect the canopy tops of trees, and
#'#then use rayshader to render 3D tree models scaled to those heights on the map.
#'run_example = length(find.package("lidR", quiet = TRUE)) > 0 && 
#'              length(find.package("sf", quiet = TRUE)) > 0 && 
#'              length(find.package("terra", quiet = TRUE)) > 0 &&
#'              rayshader:::run_documentation()
#'if (run_example) {
#'#Load the example data from the lidR package
#'LASfile = system.file("extdata", "Topography.laz", package="lidR")
#'las = lidR::readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#'
#'#Convert the lidar point data to a DEM and detect the location of trees from the same data
#'dem = lidR::rasterize_terrain(las, algorithm = lidR::tin())
#'tree_top_data = lidR::locate_trees(las, lidR::lmf(ws = 5))
#'tree_locations = sf::st_coordinates(tree_top_data)
#'
#'#Convert DEM to a matrix and extract the extent of the scene
#'dem_matrix = raster_to_matrix(dem)
#'dem_extent = terra::ext(dem)
#'extent_values = dem_extent@ptr$vector
#'
#'#Plot the ground
#'dem_matrix |>
#'  height_shade() |>
#'  add_shadow(texture_shade(dem_matrix),0.2) |> 
#'  add_shadow(lamb_shade(dem_matrix),0) |> 
#'  plot_3d(dem_matrix)
#'render_snapshot()
#'}
#'if (run_example) {
#'#The tree locations are given as an absolute height (as opposed to relative to the surface)
#'#so we set `absolute_height = TRUE`.
#'render_tree(lat = tree_locations[,2], long = tree_locations[,1],
#'            canopy_width_ratio = 0.5, clear_previous = T,
#'            absolute_height = TRUE, canopy_height = tree_locations[,3],
#'            canopy_color = "#00aa00",
#'            extent = raster::extent(extent_values), heightmap = dem_matrix)
#'            
#'#Remove existing lights and add our own with rgl
#'rgl::rgl.pop("lights")
#'rgl::light3d(phi=35,theta=90, viewpoint.rel=F, diffuse="#ffffff", specular="#000000")
#'rgl::light3d(phi=-45,theta=-40, viewpoint.rel=F, diffuse="#aaaaaa", specular="#000000")
#'render_snapshot() 
#'}
#'if (run_example) {
#'#Render tree also works with `render_highquality()`
#'render_highquality(lightdirection=c(90,45),lightaltitude=c(90,45), 
#'                   lightcolor=c("dodgerblue","orange"), 
#'                   min_variance = 0, sample_method="sobol_blue", clamp_value=10)
#'rgl::rgl.close()
#'}
render_tree = function(lat = NULL, long = NULL, extent = NULL,  
                       type = "basic", canopy_color = "#22aa22", trunk_color = "#964B00",
                       absolute_height = FALSE, canopy_height = 9, canopy_width_ratio = NULL, 
                       trunk_height = NULL, trunk_radius = NULL, 
                       tree_zscale = TRUE, min_height = 0, max_height = Inf,
                       zscale=1, heightmap = NULL, baseshape = "rectangle",
                       angle=c(0,0,0), clear_previous = FALSE,
                       ...) {
  if(clear_previous) {
    rgl::pop3d(tag = "objtree")
    if(missing(lat) || missing(long)) {
      return(invisible())
    }
  }
  if(!is.null(min_height) && is.numeric(min_height) && min_height > 0) {
    filter_heights = TRUE
  } else {
    filter_heights = FALSE
  }
  if(absolute_height && length(canopy_height) == length(lat)) {
    xyz_tree = transform_into_heightmap_coords(extent = extent, heightmap = heightmap, lat = lat, long = long, 
                                               altitude = NULL, offset = 0, zscale = 1)
    z_tree = xyz_tree[,2]
    filter_nan = is.na(z_tree)
    if(all(filter_nan)) {
      return(invisible())
    }
    z_tree = z_tree[!filter_nan]
    lat = lat[!filter_nan]
    long = long[!filter_nan]
    if(length(trunk_color) == nrow(xyz_tree)) {
      trunk_color = trunk_color[!filter_nan]
    }
    if(length(trunk_height) == nrow(xyz_tree)) {
      trunk_height = trunk_height[!filter_nan]
    }
    if(length(trunk_radius) == nrow(xyz_tree)) {
      trunk_radius = trunk_radius[!filter_nan]
    }
    if(length(canopy_width_ratio) == nrow(xyz_tree)) {
      canopy_width_ratio = canopy_width_ratio[!filter_nan]
    }
    if(length(canopy_height) == nrow(xyz_tree)) {
      canopy_height = canopy_height[!filter_nan]
    }
    canopy_height = canopy_height - z_tree
    if(!is.infinite(max_height) || min_height > 0) {
      filter_height = canopy_height >= max_height | canopy_height <= min_height 
      long = long[!filter_height]
      lat = lat[!filter_height]
      trunk_color = trunk_color[!filter_height]
      trunk_height = trunk_height[!filter_height]
      trunk_radius = trunk_radius[!filter_height]
      canopy_width_ratio = canopy_width_ratio[!filter_height]
      canopy_height = canopy_height[!filter_height]
    }
    if(length(long) == 0) {
      return(invisible())
    }
  }
  if(is.null(trunk_height)) {
    if(type == "cone") {
      trunk_height = canopy_height/6
    } else {
      trunk_height = canopy_height/3
    }
  }
  if(is.null(trunk_radius)) {
    if(type == "cone") {
      trunk_radius = trunk_height/5
    } else {
      trunk_radius = trunk_height/10
    }
  }
  canopy_radius = canopy_height * canopy_width_ratio / 2
  if(is.null(canopy_radius)) {
    canopy_radius = canopy_height/4
  }
  if(tree_zscale) {
    canopy_radius = canopy_radius/zscale
    trunk_radius = trunk_radius/zscale
  }
  if(type == "cone") {
    canopy_radius = canopy_radius*2
  }
  stopifnot(length(lat) == length(long))
  canopy_width = canopy_radius
  height_zscale = 1
  if(length(canopy_height) == 1) {
    canopy_height = rep(canopy_height,length(lat))
  }
  if(length(canopy_width) == 1) {
    canopy_width = rep(canopy_width,length(lat))
  }
  if(length(trunk_radius) == 1) {
    trunk_radius = rep(trunk_radius,length(lat))
  }
  if(length(trunk_height) == 1) {
    trunk_height = rep(trunk_height,length(lat))
  }
  if(tree_zscale) {
    tree_scale = matrix(c(canopy_width/5,canopy_height/10/zscale,canopy_width/5),
                        ncol=3,nrow=length(lat))
    trunk_scale = matrix(c(trunk_radius/0.3,(trunk_height + canopy_height/3)/zscale,trunk_radius/0.3),
                         ncol=3,nrow=length(lat))
  } else {
    tree_scale = matrix(c(canopy_width/5,canopy_height/10,canopy_width/5),
                        ncol=3,nrow=length(lat))
    trunk_scale = matrix(c(trunk_radius/0.3,(trunk_height + canopy_height/3),trunk_radius/0.3),
                         ncol=3,nrow=length(lat))
    height_zscale = zscale
  }

  if(type == "basic") {
    render_obj(tree_basic_center_obj(), color = canopy_color,
               lat = lat, long = long, extent = extent, zscale = zscale,
               offset = (trunk_height + canopy_height/3)*height_zscale,
               heightmap = heightmap, angle = angle, scale = tree_scale, 
               baseshape = baseshape,
               clear_previous = FALSE, rgl_tag = "tree",
               ...)
    render_obj(tree_trunk_obj(), color = trunk_color,
               lat = lat, long = long, extent = extent, zscale = zscale, offset = 0,
               baseshape = baseshape,
               heightmap = heightmap, angle = angle, scale = trunk_scale, rgl_tag = "tree", 
               ...)
  } else if (type == "cone") {
    render_obj(tree_cone_center_obj(), color = canopy_color,
              lat = lat, long = long, extent = extent, zscale = zscale, 
              offset = trunk_height*height_zscale,
              baseshape = baseshape,
              heightmap = heightmap, angle = angle, scale = tree_scale, clear_previous = FALSE,
              rgl_tag = "tree",
              ...)
    render_obj(tree_trunk_obj(), color = trunk_color,
               lat = lat, long = long, extent = extent, zscale = zscale, offset = 0,
               baseshape = baseshape,
               heightmap = heightmap, angle = angle, scale = trunk_scale, rgl_tag = "tree",
               ...)
  } else {
    stop(sprintf("%s not recognized as built-in type of tree", type))
  }
}
