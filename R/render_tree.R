#'@title Render Tree
#'
#'@description Adds a 3D representation of trees to an existing 3D scene generated with rayshader.
#'Users can specify the trees' geographical positions using latitude and longitude or the same coordinate reference system as `extent`.
#'Different types of tree models can be used, including a basic and a cone-shaped tree. Users can also use their own custom tree model in
#'OBJ format. The function allows customization of various aspects of the tree, including the color of the crown and the trunk,
#'the size of the crown (the leafy part of the tree) and the trunk, the overall scale of the tree, and the rotation angle around the x, y, and z axes.
#'Users can also specify the minimum and maximum height of the trees to be rendered.
#'
#'@param long Vector of longitudes (or other coordinate in the same coordinate reference system as extent).
#'@param lat Vector of latitudes (or other coordinate in the same coordinate reference system as extent).
#'@param type Default `"basic"`. Type of tree. Other built-in option: `"cone"`.
#'@param custom_obj_tree Default `NULL`. Instead of using the built-in types, users can also load a custom tree 
#'model in OBJ format. This function loads and manipulates the model, assuming the tree model's trunk begins 
#'at the origin. Color and specific trunk/crown proportions will be fixed to the model specified, although the overall 
#'scale can be changed per-tree via `crown_height`.
#'@param custom_obj_crown Default `NULL`. Instead of using the built-in types, users can also load a custom crown 
#'model in OBJ format. This function loads a crown model and allows you to control the crown and trunk proportions separately.
#'@param custom_obj_trunk Default `NULL`.  Instead of using the built-in types, users can also load a custom trunk 
#'model in OBJ format. This function loads a trunk model and allows you to control the crown and trunk proportions separately.
#'@param crown_color Default `"darkgreen"`. Color(s) of the crown.
#'@param trunk_color Default `"#964B00"` (brown). Color(s) of the trunk,
#'@param absolute_height Default `FALSE`. Default is specifying the tree height directly, relative to the 
#'underlying height map. If `TRUE`, `crown_height` will specified by the actual altitude of the top of the tree.  
#'Total tree height will be `crown_height + trunk_height`.
#'@param tree_height Default `NULL`. Height of the tree, automatically set to `10` if not specified. If `absolute_height = TRUE`, then this is interpreted as 
#'the altitude of the top of the tree in the coordinate reference system used. If `absolute_height = FALSE`, then 
#'this is interpreted as the height of the tree relative to the underlying heightmap. 
#'@param trunk_height_ratio Default `NULL`. The ratio of the height of the trunk to the total height of the tree.
#'Default is 1/3rd the crown height if `type = "basic"`, and 1/6th the crown height if `type = "cone"`.
#'@param crown_width_ratio Default `NULL`. Ratio of the crown width to the crown height. A value of `1` is spherical.
#'@param crown_width Default `NULL`. As an alternative to specifying the ratio, you can use this argument to 
#'specify the crown width directly. 
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
#'@param ... Additional arguments to pass to `rgl::triangles3d()`.
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
#'            tree_zscale = FALSE, tree_height = 30, 
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the crown width ratio (compared to the height)
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, tree_height = 60, crown_width_ratio = 0.5,
#'            clear_previous = TRUE, 
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the trunk height and width
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, tree_height = 40, crown_width_ratio = 2,
#'            clear_previous = TRUE, trunk_height_ratio=1/2, trunk_radius = 1.5,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the tree type
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, tree_height = 30, 
#'            clear_previous = TRUE, type = "cone",trunk_height_ratio = 1/6,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Change the crown color:
#'render_camera(theta = 150,  phi = 38, zoom = 0.4, fov = 55)
#'render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
#'            tree_zscale = FALSE, tree_height = 30, crown_width_ratio = 0.5 + runif(20),
#'            crown_color = rainbow(20),  clear_previous = TRUE, 
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50) 
#'render_snapshot()
#'}
#'
#'#We will use the lidR package to generate a DEM and detect the crown tops of trees, and
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
#'render_tree(lat = tree_locations[,2], 
#'            long = tree_locations[,1],
#'            crown_width_ratio = 0.5, 
#'            absolute_height = TRUE, 
#'            tree_height = tree_locations[,3],
#'            trunk_height_ratio = 0.2 + 0.1*runif(nrow(tree_locations)),
#'            crown_color = "#00aa00",
#'            extent = raster::extent(extent_values), 
#'            heightmap = dem_matrix,
#'            clear_previous = TRUE)
#'            
#'#Remove existing lights and add our own with rgl
#'rgl::pop3d("lights")
#'rgl::light3d(phi=35,theta=90, viewpoint.rel=F, diffuse="#ffffff", specular="#000000")
#'rgl::light3d(phi=-45,theta=-40, viewpoint.rel=F, diffuse="#aaaaaa", specular="#000000")
#'render_snapshot() 
#'}
#'if (run_example) {
#'#Render tree also works with `render_highquality()`
#'render_highquality(lightdirection=c(90,45),lightaltitude=c(90,45), 
#'                   lightcolor=c("dodgerblue","orange"), 
#'                   min_variance = 0, sample_method="sobol_blue", clamp_value=10)
#'}
render_tree = function(lat = NULL, 
                       long = NULL, 
                       extent = NULL,  
                       type = "basic", 
                       custom_obj_tree = NULL,
                       custom_obj_crown = NULL,
                       custom_obj_trunk = NULL,
                       crown_color = "#22aa22", 
                       trunk_color = "#964B00",
                       absolute_height = FALSE, 
                       tree_height =  NULL, 
                       trunk_height_ratio = NULL, 
                       crown_width_ratio = NULL,
                       crown_width = NULL,
                       trunk_radius = NULL,
                       tree_zscale = TRUE, 
                       min_height = 0, 
                       max_height = Inf,
                       zscale=1, 
                       heightmap = NULL, 
                       baseshape = "rectangle",
                       angle=c(0,0,0), 
                       clear_previous = FALSE,
                       ...) {
  # If clear_previous is TRUE, remove previous tree object
  if(clear_previous) {
    rgl::pop3d(tag = "objtree")
    if(missing(lat) || missing(long)) {
      return(invisible())
    }
  }
  # Check if custom tree models exist
  has_custom_tree = !is.null(custom_obj_tree) && file.exists(custom_obj_tree)
  has_custom_crown = !is.null(custom_obj_crown) && file.exists(custom_obj_crown)
  has_custom_trunk = !is.null(custom_obj_trunk) && file.exists(custom_obj_trunk)
  
  if((has_custom_trunk && !has_custom_crown) || (!has_custom_trunk && has_custom_crown)) {
    stop("If specifying either one of `custom_obj_crown` or `custom_obj_trunk`, both must be specified.")
  }
  
  # Check if the tree is fully custom or partially custom
  fully_custom_tree = has_custom_trunk && has_custom_crown
  custom_tree = any(c(has_custom_tree, has_custom_crown, has_custom_trunk))
  
  # Handling case where both full tree and parts are specified
  if(custom_tree && has_custom_tree && (has_custom_crown || has_custom_trunk)) {
    warning("Using `custom_obj_tree` over models specied in `custom_obj_crown` and `custom_obj_trunk`")
    has_custom_crown = FALSE
    has_custom_trunk = FALSE
  }
  
  use_default_crown_height = FALSE
  use_default_trunk_height = FALSE
  use_default_trunk_radius = FALSE
  use_absolute_widths = FALSE
  
  if(is.null(tree_height)) {
    tree_height = 10
  }
  if(is.null(trunk_height_ratio)) {
    use_default_crown_height = TRUE
    use_default_trunk_height = TRUE
    if(!custom_tree) {
      if(type == "cone") {
        trunk_height_ratio = 1/6
      }
      if(type == "basic") {
        trunk_height_ratio = 1/3
      }
    } else {
      trunk_height_ratio = 1/3
    }
  } else {
    stopifnot(all(trunk_height_ratio < 1 & trunk_height_ratio >= 0))
  }
  if(!absolute_height) {
    crown_height = (1 - trunk_height_ratio) * tree_height
    trunk_height = (trunk_height_ratio) * tree_height
  }
  if(is.null(crown_width_ratio)) {
    if(type == "cone") {
      crown_width_ratio = 1/2
    }
    if(type == "basic") {
      crown_width_ratio = 1
    }
  }
  
  
  if(is.null(trunk_radius)) {
    use_default_trunk_radius = TRUE
  }
  if(!is.null(crown_width)) {
    use_absolute_widths = TRUE
  }

  # If absolute height is specified, calculate offset in heightmap coordinates
  if(absolute_height && length(tree_height) == length(lat)) {
    xyz_tree = transform_into_heightmap_coords(extent = extent, 
                                               heightmap = heightmap, 
                                               lat = lat, 
                                               long = long, 
                                               altitude = NULL, 
                                               offset = 0, 
                                               zscale = 1)
    z_tree = xyz_tree[,2]
    filter_nan = is.na(z_tree)
    if(all(filter_nan)) {
      return(invisible())
    }
    z_tree = z_tree[!filter_nan]
    lat = lat[!filter_nan]
    long = long[!filter_nan]
    if(length(tree_height) == nrow(xyz_tree)) {
      tree_height = tree_height[!filter_nan]
    }
    if(length(trunk_height_ratio) == nrow(xyz_tree)) {
      trunk_height_ratio = trunk_height_ratio[!filter_nan]
    }
    if(length(trunk_color) == nrow(xyz_tree)) {
      trunk_color = trunk_color[!filter_nan]
    }
    if(length(trunk_radius) == nrow(xyz_tree)) {
      trunk_radius = trunk_radius[!filter_nan]
    }
    if(length(crown_width_ratio) == nrow(xyz_tree)) {
      crown_width_ratio = crown_width_ratio[!filter_nan]
    }
    if(use_absolute_widths && length(crown_width) == nrow(xyz_tree)) {
      crown_width = crown_width[!filter_nan]
    }
    tree_height = tree_height - z_tree
    if(!is.infinite(max_height) || min_height > 0) {
      filter_height = tree_height >= max_height | tree_height <= min_height 
      
      if(length(long) > 1) {
        long = long[!filter_height]
      }
      
      if(length(lat) > 1) {
        lat = lat[!filter_height]
      }
      
      if(length(trunk_color) > 1) {
        trunk_color = trunk_color[!filter_height]
      }
      
      if(length(trunk_height_ratio) > 1) {
        trunk_height_ratio = trunk_height_ratio[!filter_height]
      }
      
      if(length(trunk_radius) > 1) {
        trunk_radius = trunk_radius[!filter_height]
      }
      
      if(length(crown_width_ratio) > 1) {
        crown_width_ratio = crown_width_ratio[!filter_height]
      }
      
      if(use_absolute_widths) {
        if(length(crown_width) > 1) {
          crown_width = crown_width[!filter_height]
        }
      } 
      
      if(length(tree_height) > 1) {
        tree_height = tree_height[!filter_height]
      }
    }
    crown_height = (1 - trunk_height_ratio) * tree_height
    trunk_height = trunk_height_ratio * tree_height
    if(length(long) == 0) {
      return(invisible())
    }
  }
  # Determine default trunk height and radius based on tree type
  # The basic trunk included has a radius of 0.6 units and a height of 1.0 units, 
  # with the bottom of the trunk located at 0.0 (midpoint at 0.5).
  # The basic spherical crown is centered at zero with a radius of 5.0 units
  # The basic conical crown has a radius of 5.0 units and a height of 10 units, with the base at 0.0
  if(!custom_tree) {
    # Calculate crown radius
    crown_radius = crown_height * crown_width_ratio / 2
    if(is.null(crown_radius)) {
      crown_radius = crown_height/4
    }
    # Scaling tree dimensions if tree_zscale is TRUE
    if(tree_zscale) {
      crown_radius = crown_radius/zscale
      trunk_radius = trunk_radius/zscale
    }
    #This just ensures the aspect ratio is correct
    if(type == "cone") {
      crown_radius = crown_radius*2
    }
    crown_width = crown_radius
  } else {
    if(!fully_custom_tree) {
      if(is.null(crown_height)) {
        crown_height = 1
      }
      if(!is.null(trunk_height) || !is.null(trunk_radius)) {
        warning("When specifying single `crown_obj_tree` file (instead of separate ",
                "crown and trunk OBJs), `crown_height` controls the overall scale of ",
                "the tree and trunk settings cannot be changed.")
        trunk_height = 1
        trunk_radius = 1
      } 
      if(!is.null(crown_width_ratio)) {
        crown_radius = crown_height * crown_width_ratio / 2
      } else {
        crown_radius = 1
      }
    } else {
      if(is.null(trunk_height)) {
        trunk_height = 1
      }
      if(!use_absolute_widths) {
        if(!is.null(crown_width_ratio)) {
          crown_width = crown_height * crown_width_ratio
        } else {
          crown_width = crown_height
        }
      } 
      # Scaling tree dimensions if tree_zscale is TRUE
      if(tree_zscale) {
        crown_width = crown_width/zscale
        trunk_radius = trunk_radius/zscale
      }
    }
  }
  if(use_default_trunk_radius) {
    trunk_radius = crown_width / 6
    if(type == "cone") {
      trunk_radius = trunk_radius / 2
    }
  }
  stopifnot(length(lat) == length(long))
  height_zscale = 1
  
  # Expand scalar dimensions to vectors if needed
  if(length(crown_height) == 1) {
    crown_height = rep(crown_height,length(lat))
  }
  if(length(crown_width) == 1) {
    crown_width = rep(crown_width,length(lat))
  }
  if(length(trunk_radius) == 1) {
    trunk_radius = rep(trunk_radius,length(lat))
  }
  if(length(trunk_height) == 1) {
    trunk_height = rep(trunk_height,length(lat))
  }
  if(!custom_tree) {
    if(tree_zscale) {
      tree_scale = matrix(c(crown_width/5,crown_height/10/zscale,crown_width/5),
                          ncol=3,nrow=length(lat))
      trunk_scale = matrix(c(trunk_radius/0.3,(trunk_height + crown_height/3)/zscale,trunk_radius/0.3),
                           ncol=3,nrow=length(lat))
    } else {
      tree_scale = matrix(c(crown_width/5,crown_height/10,crown_width/5),
                          ncol=3,nrow=length(lat))
      trunk_scale = matrix(c(trunk_radius/0.3,(trunk_height + crown_height/3),trunk_radius/0.3),
                           ncol=3,nrow=length(lat))
      height_zscale = zscale
    }
  } else {
    #Scale the custom trees
    if(fully_custom_tree) {
      # For this version, we can control all proportions. 
      # This assumes the tree trunk/crown has a radius of 1 and a height of 1.
      if(tree_zscale) {
        tree_scale = matrix(c(crown_width,crown_height/zscale,crown_width),
                            ncol=3,nrow=length(lat))
        trunk_scale = matrix(c(trunk_radius,trunk_height/zscale,trunk_radius),
                             ncol=3,nrow=length(lat))
      } else {
        tree_scale = matrix(c(crown_width,crown_height,crown_width),
                            ncol=3,nrow=length(lat))
        trunk_scale = matrix(c(trunk_radius,trunk_height,trunk_radius),
                             ncol=3,nrow=length(lat))
        height_zscale = zscale
      }
    } else {
      if(tree_zscale) {
        tree_scale = matrix(c(crown_width,crown_height/zscale,crown_width),
                            ncol=3,nrow=length(lat))
        trunk_scale = matrix(c(trunk_radius,trunk_height/zscale,trunk_radius),
                             ncol=3,nrow=length(lat))
      } else {
        tree_scale = matrix(c(crown_width,crown_height,crown_width),
                            ncol=3,nrow=length(lat))
        height_zscale = zscale
      }
    }
  }
  if(fully_custom_tree) {
    # If a fully custom tree is specified, render the custom crown and trunk
    render_obj(custom_obj_crown, color = crown_color,
               lat = lat, long = long, extent = extent, zscale = zscale,
               offset = trunk_height*height_zscale,
               heightmap = heightmap, angle = angle, scale = tree_scale, 
               baseshape = baseshape,
               clear_previous = FALSE, rgl_tag = "tree",
               ...)
    render_obj(custom_obj_trunk, color = trunk_color,
               lat = lat, long = long, extent = extent, zscale = zscale, offset = 0,
               baseshape = baseshape,
               heightmap = heightmap, angle = angle, scale = trunk_scale, rgl_tag = "tree", 
               ...)
  } else if(custom_tree) {
    # If a custom tree is specified (but not fully custom), render the custom tree
    render_obj(custom_obj_tree, 
               load_material = TRUE,
               lat = lat, 
               long = long, 
               extent = extent, 
               zscale = zscale,
               offset = 0,
               heightmap = heightmap, 
               angle = angle, 
               scale = tree_scale, 
               baseshape = baseshape,
               clear_previous = FALSE, 
               rgl_tag = "tree",
               ...)
  } else if(type == "basic") {
    # If a basic type is specified, render the basic tree's crown and trunk
    render_obj(tree_basic_center_obj(), color = crown_color,
               lat = lat, long = long, extent = extent, zscale = zscale,
               offset = (trunk_height + crown_height/3)*height_zscale,
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
    # If a cone type is specified, render the cone tree's crown and trunk
    render_obj(tree_cone_center_obj(), color = crown_color,
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
