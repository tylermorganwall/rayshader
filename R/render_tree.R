#'@title Render Tree
#'
#'@description Adds a 3D representation of trees to an existing 3D scene generated with rayshader.
#'Users can specify the trees' positions using x/y coordinates or the same coordinate reference system as `extent`.
#'Different types of tree models can be used, including a basic and a cone-shaped tree. Users can also use their own custom tree model in
#'OBJ format. The function allows customization of various aspects of the tree, including the color of the crown and the trunk,
#'the size of the crown (the leafy part of the tree) and the trunk, the overall scale of the tree, and the rotation angle around the x, y, and z axes.
#'Users can also specify the minimum and maximum height of the trees to be rendered.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param location Default `NULL`. Spatial point input used to place the rendered tree(s) in the scene. Accepts `sf`, `sfc`, `sfg`, or `sp` POINT or MULTIPOINT geometries. MULTIPOINT inputs are flattened to point placements internally, and vectorized arguments such as `tree_height`, `angle`, `crown_color`, and `trunk_color` are applied against that flattened point count. If the input carries a CRS, it will be transformed automatically into the active scene CRS. If it has no CRS, supply `crs`.
#'@param type Default `"basic"`. Type of tree. Other built-in option: `"cone"`.
#'@param custom_obj_tree Default `NULL`. Instead of using the built-in types, users can also load a custom tree
#'model in OBJ format. This function loads and manipulates the model, assuming the tree model's trunk begins
#'at the origin. Color and specific trunk/crown proportions will be fixed to the model specified, although the overall
#'scale can be changed per-tree via `crown_height`.
#'@param custom_obj_crown Default `NULL`. Instead of using the built-in types, users can also load a custom crown
#'model in OBJ format. This function loads a crown model and allows you to control the crown and trunk proportions separately.
#'@param custom_obj_trunk Default `NULL`.  Instead of using the built-in types, users can also load a custom trunk
#'model in OBJ format. This function loads a trunk model and allows you to control the crown and trunk proportions separately.
#'@param crown_color Default `"darkgreen"`. Color(s) of the crown. Use `"height"` to color crowns by the cached [plot_gg()] height aesthetic palette using `tree_height`.
#'@param trunk_color Default `"#964B00"` (brown). Color(s) of the trunk. Use `"height"` to color trunks by the cached [plot_gg()] height aesthetic palette using `tree_height`.
#'@param absolute_height Default `FALSE`. Default is specifying the tree height directly, relative to the
#'underlying height map. If `TRUE`, `crown_height` will specified by the actual altitude of the top of the tree.
#'Total tree height will be `crown_height + trunk_height`.
#'@param tree_height Default `NULL`. Height of the tree, automatically set to `10` if not specified. If `absolute_height = TRUE`, then this is interpreted as
#'the altitude of the top of the tree in the coordinate reference system used. If `absolute_height = FALSE`, then
#'this is interpreted as the height of the tree relative to the underlying heightmap.
#'@param tree_height_column Default `NULL`. Column name in `location` to use for
#'`tree_height`. Requires `location` to be an `sf`/spatial point object with
#'attribute data. Cannot be combined with an explicit `tree_height`.
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
#'@param lit Default `TRUE`. Whether to apply lighting to the tree.
#'@param baseshape Default `rectangle`. Shape of the base. Options are `c("rectangle","circle","hex")`.
#'@param angle Default `c(0,0,0)`. Angle of rotation around the x, y, and z axes. If this is a matrix or list,
#'each row (or list entry) specifies the rotation of the nth tree specified (number of rows/length of list must
#'equal the length of `x`/`y`).
#'@param clear_previous Default `FALSE`. If `TRUE`, clears all existing trees.
#'A clear-only call returns without rendering a replacement.
#'@param x Vector of x coordinates (or other coordinate in the same coordinate reference system as extent).
#'@param y Vector of y coordinates (or other coordinate in the same coordinate reference system as extent).
#'@param lat Default `NULL`. Alias for `y` for geographic workflows.
#'@param long Default `NULL`. Alias for `x` for geographic workflows.
#'@param crs Default `NULL`. CRS of the input numeric x/y coordinates, or CRS to assign to CRS-less spatial data before transforming it into the active scene CRS. If spatial data already carries a CRS, that CRS is used automatically.
#'@param filter_to_extent Default `TRUE`. If `TRUE`, tree placements outside the scene extent are omitted. For scenes created with [plot_gg()], filtering uses the ggplot panel extent rather than the full rendered 3D ggplot extent.
#'@param extent Either an object representing the spatial extent of the 3D scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax", "ymin", "ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use extent metadata cached by [plot_3d()] or [plot_gg()].
#'@param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#'to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#'for non-ggplot scenes.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis in the original heightmap.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'of matrix extent isn't working. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' All points are assumed to be evenly spaced.
#'@param ... Additional arguments to pass to `rgl::triangles3d()`.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Let's first start by drawing some trees in a circle around Monterey Bay
#'#We won't scale these to a realistic size (yet)
#'moss_landing_coord = c(36.806807, -121.793332)
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration = 20) |>
#'  plot_3d(vertical_exaggeration = 4,water=TRUE,
#'          shadowcolor="#40310a", background = "tan",
#'          theta=210,  phi=22, zoom=0.20, fov=55)
#'
#'t = seq(0,2*pi,length.out=20)
#'circle_coords_lat = moss_landing_coord[1] + 0.3 * sin(t)
#'circle_coords_long = moss_landing_coord[2] + 0.3 * cos(t)
#'
#'render_tree(tree_zscale = FALSE, tree_height = 30,  lit = TRUE,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long))
#'render_snapshot()
#'#Change the crown width ratio (compared to the height)
#'render_tree(tree_zscale = FALSE, tree_height = 60, crown_width_ratio = 0.5,
#'            clear_previous = TRUE,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long))
#'render_snapshot()
#'#Change the trunk height and width
#'render_tree(tree_zscale = FALSE, tree_height = 40, crown_width_ratio = 2,
#'            clear_previous = TRUE, trunk_height_ratio=1/2, trunk_radius = 1.5,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long))
#'render_snapshot()
#'#Change the tree type
#'render_tree(tree_zscale = FALSE, tree_height = 30,
#'            clear_previous = TRUE, type = "cone",trunk_height_ratio = 1/6,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long))
#'render_snapshot()
#'#Change the crown color:
#'render_camera(theta = 150,  phi = 38, zoom = 0.4, fov = 55)
#'render_tree(tree_zscale = FALSE, tree_height = 30, crown_width_ratio = 0.5 + runif(20),
#'            crown_color = rainbow(20),  clear_previous = TRUE,
#'            lat = unlist(circle_coords_lat), long = unlist(circle_coords_long))
#'render_snapshot()
#'
#'#We will use the lidR package to generate a DEM and detect the crown tops of trees, and
#'#then use rayshader to render 3D tree models scaled to those heights on the map.
#'@examplesIf length(find.package("lidR", quiet = TRUE)) > 0 && length(find.package("sf", quiet = TRUE)) > 0 && length(find.package("terra", quiet = TRUE)) > 0 && (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#'#Load the example data from the lidR package
#'LASfile = system.file("extdata", "Topography.laz", package="lidR")
#'las = lidR::readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#'
#'#Convert the lidar point data to a DEM and detect the location of trees from the same data
#'dem = lidR::rasterize_terrain(las, algorithm = lidR::tin())
#'tree_top_data = lidR::locate_trees(las, lidR::lmf(ws = 5))
#'tree_locations = sf::st_coordinates(tree_top_data)
#'
#'#Plot the ground
#'dem |>
#'  height_shade() |>
#'  add_shadow(texture_shade(),0) |>
#'  add_shadow(lamb_shade(),0) |>
#'  plot_3d(windowsize = 800, shadowdepth=min(raster_to_matrix(dem),na.rm=TRUE))
#'render_snapshot()
#'#The tree locations are given as an absolute height (as opposed to relative to the surface)
#'#so we set `absolute_height = TRUE`.
#'render_tree(y = tree_locations[,2],
#'            x = tree_locations[,1],
#'            crown_width_ratio = 0.5,
#'            absolute_height = TRUE,
#'            tree_height = tree_locations[,3],
#'            trunk_height_ratio = 0.2 + 0.1*runif(nrow(tree_locations)),
#'            crown_color = "#007700",
#'            clear_previous = TRUE)
#'render_camera(zoom=0.85)
#'#Remove existing lights and add our own with rgl
#'invisible(rgl::pop3d("lights"))
#'invisible(rgl::light3d(phi=35,theta=90, viewpoint.rel=F, diffuse="#ffffff", specular="#000000"))
#'invisible(rgl::light3d(phi=-45,theta=-40, viewpoint.rel=F, diffuse="#aaaaaa", specular="#000000"))
#'render_snapshot()
#'#Render tree also works with `render_highquality()`
#'render_highquality(sky_sun_elevation = 30, sky_sun_azimuth=225, iso=3)
render_tree = function(
  location = NULL,
  type = "basic",
  custom_obj_tree = NULL,
  custom_obj_crown = NULL,
  custom_obj_trunk = NULL,
  crown_color = "#22aa22",
  trunk_color = "#964B00",
  absolute_height = FALSE,
  tree_height = NULL,
  tree_height_column = NULL,
  trunk_height_ratio = NULL,
  crown_width_ratio = NULL,
  crown_width = NULL,
  trunk_radius = NULL,
  tree_zscale = TRUE,
  min_height = 0,
  max_height = Inf,
  lit = TRUE,
  baseshape = "rectangle",
  angle = c(0, 0, 0),
  clear_previous = FALSE,
  x = NULL,
  y = NULL,
  lat = NULL,
  long = NULL,
  crs = NULL,
  filter_to_extent = TRUE,
  extent = NULL,
  panel = NULL,
  zscale = 1,
  vertical_exaggeration = 1,
  heightmap = NULL,
  ...
) {
  if (
    is_render_clear_only_call(
      clear_previous,
      match.call(),
      function() rgl::pop3d(tag = "objtree")
    )
  ) {
    return(invisible(NULL))
  }
  validate_filter_to_extent(filter_to_extent, caller = "render_tree")
  tree_height_supplied = !missing(tree_height) && !is.null(tree_height)
  tree_args = list(...)
  zscale = resolve_scene_render_effective_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    vertical_exaggeration = vertical_exaggeration,
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_tree"
  )
  heightmap = resolve_scene_render_heightmap(
    heightmap,
    caller = "render_tree"
  )
  scene_extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "render_tree",
    panel = panel,
    error_if_missing = FALSE
  )
  point_input = resolve_render_location_input(
    location = location,
    x = x,
    y = y,
    long = long,
    lat = lat,
    missing_x = missing(x),
    missing_y = missing(y),
    missing_long = missing(long),
    missing_lat = missing(lat),
    extent = if (!is.null(scene_extent)) scene_extent else extent,
    heightmap = heightmap,
    panel = panel,
    crs = crs,
    caller = "render_tree"
  )
  x = point_input$x
  y = point_input$y
  lat = y
  long = x
  input_crs = if (is.null(crs)) point_input$source_crs else crs
  if (!is.null(point_input$extent)) {
    extent = point_input$extent
  } else if (is.null(extent) && !is.null(scene_extent)) {
    extent = scene_extent
  }
  location_supplied = isTRUE(point_input$location_supplied)
  render_obj_crs = if (location_supplied) NULL else input_crs
  tree_coords_transformed = FALSE
  if (!is.null(tree_height_column)) {
    tree_height = resolve_render_tree_height_column(
      location = location,
      tree_height_column = tree_height_column,
      tree_height_supplied = tree_height_supplied,
      crs = crs,
      caller = "render_tree"
    )
  }
  render_obj_tree = function(...) {
    do.call(render_obj, c(list(..., vertical_exaggeration = 1), tree_args))
  }
  if (!is.null(lat) && !is.null(long)) {
    if (!location_supplied) {
      scene_xy = auto_transform_scene_xy(
        x = long,
        y = lat,
        extent = extent,
        heightmap = heightmap,
        panel = panel,
        crs = input_crs,
        caller = "render_tree"
      )
      long = scene_xy$x
      lat = scene_xy$y
      if (!is.null(scene_xy$extent)) {
        extent = scene_xy$extent
      }
      render_obj_crs = NULL
      tree_coords_transformed = TRUE
    }
    n_tree_before_filter = length(lat)
    filtered_tree_xy = filter_scene_xy_to_extent(
      x = long,
      y = lat,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      filter_to_extent = filter_to_extent,
      caller = "render_tree"
    )
    long = filtered_tree_xy$x
    lat = filtered_tree_xy$y
    if (length(filtered_tree_xy$keep) == n_tree_before_filter) {
      tree_height = subset_render_arg(
        tree_height,
        filtered_tree_xy$keep,
        n_tree_before_filter
      )
      trunk_height_ratio = subset_render_arg(
        trunk_height_ratio,
        filtered_tree_xy$keep,
        n_tree_before_filter
      )
      crown_width_ratio = subset_render_arg(
        crown_width_ratio,
        filtered_tree_xy$keep,
        n_tree_before_filter
      )
      crown_width = subset_render_arg(
        crown_width,
        filtered_tree_xy$keep,
        n_tree_before_filter
      )
      trunk_radius = subset_render_arg(
        trunk_radius,
        filtered_tree_xy$keep,
        n_tree_before_filter
      )
      crown_color = subset_render_color_arg(
        crown_color,
        filtered_tree_xy$keep,
        n_tree_before_filter
      )
      trunk_color = subset_render_color_arg(
        trunk_color,
        filtered_tree_xy$keep,
        n_tree_before_filter
      )
      angle = subset_render_row_arg(
        angle,
        filtered_tree_xy$keep,
        n_tree_before_filter
      )
    }
    if (!length(lat) || !length(long)) {
      return(invisible())
    }
  }
  # Check if custom tree models exist
  has_custom_tree = !is.null(custom_obj_tree) && file.exists(custom_obj_tree)
  has_custom_crown = !is.null(custom_obj_crown) && file.exists(custom_obj_crown)
  has_custom_trunk = !is.null(custom_obj_trunk) && file.exists(custom_obj_trunk)

  if (
    (has_custom_trunk && !has_custom_crown) ||
      (!has_custom_trunk && has_custom_crown)
  ) {
    stop(
      "If specifying either one of `custom_obj_crown` or `custom_obj_trunk`, both must be specified."
    )
  }

  # Check if the tree is fully custom or partially custom
  fully_custom_tree = has_custom_trunk && has_custom_crown
  custom_tree = any(c(has_custom_tree, has_custom_crown, has_custom_trunk))

  # Handling case where both full tree and parts are specified
  if (
    custom_tree && has_custom_tree && (has_custom_crown || has_custom_trunk)
  ) {
    warning(
      "Using `custom_obj_tree` over models specied in `custom_obj_crown` and `custom_obj_trunk`"
    )
    has_custom_crown = FALSE
    has_custom_trunk = FALSE
  }

  use_default_crown_height = FALSE
  use_default_trunk_height = FALSE
  use_default_trunk_radius = FALSE
  use_absolute_widths = FALSE

  if (is.null(tree_height)) {
    tree_height = 10
  }
  tree_zaxis_raw = tree_height
  tree_zaxis_scene = tree_height
  tree_zaxis_label = if (!is.null(tree_height_column)) {
    tree_height_column
  } else {
    "tree"
  }
  if (is.null(trunk_height_ratio)) {
    use_default_crown_height = TRUE
    use_default_trunk_height = TRUE
    if (!custom_tree) {
      if (type == "cone") {
        trunk_height_ratio = 1 / 6
      }
      if (type == "basic") {
        trunk_height_ratio = 1 / 3
      }
    } else {
      trunk_height_ratio = 1 / 3
    }
  } else {
    if (
      !is.numeric(trunk_height_ratio) ||
        anyNA(trunk_height_ratio) ||
        any(trunk_height_ratio < 0 | trunk_height_ratio >= 1)
    ) {
      stop(
        "`trunk_height_ratio` must contain numeric values greater than or equal to 0 and less than 1.",
        call. = FALSE
      )
    }
  }
  if (!absolute_height) {
    crown_height = (1 - trunk_height_ratio) * tree_height
    trunk_height = (trunk_height_ratio) * tree_height
  }
  if (is.null(crown_width_ratio)) {
    if (type == "cone") {
      crown_width_ratio = 1 / 2
    }
    if (type == "basic") {
      crown_width_ratio = 1
    }
  }

  if (is.null(trunk_radius)) {
    use_default_trunk_radius = TRUE
  }
  if (!is.null(crown_width)) {
    use_absolute_widths = TRUE
  }

  # If absolute height is specified, calculate offset in heightmap coordinates
  if (absolute_height && length(tree_height) == length(lat)) {
    xyz_tree = transform_into_heightmap_coords(
      extent = extent,
      heightmap = heightmap,
      lat = lat,
      long = long,
      altitude = NULL,
      offset = 0,
      zscale = 1,
      crs = render_obj_crs,
      panel = panel,
      transform_scene = !location_supplied && !tree_coords_transformed,
      caller = "render_tree"
    )
    z_tree = xyz_tree[, 2]
    filter_nan = is.na(z_tree)
    if (all(filter_nan)) {
      return(invisible())
    }
    z_tree = z_tree[!filter_nan]
    lat = lat[!filter_nan]
    long = long[!filter_nan]
    if (length(tree_height) == nrow(xyz_tree)) {
      tree_height = tree_height[!filter_nan]
    }
    if (length(tree_zaxis_raw) == nrow(xyz_tree)) {
      tree_zaxis_raw = tree_zaxis_raw[!filter_nan]
      tree_zaxis_scene = tree_zaxis_scene[!filter_nan]
    }
    if (length(trunk_height_ratio) == nrow(xyz_tree)) {
      trunk_height_ratio = trunk_height_ratio[!filter_nan]
    }
    if (length(trunk_color) == nrow(xyz_tree)) {
      trunk_color = trunk_color[!filter_nan]
    }
    if (length(trunk_radius) == nrow(xyz_tree)) {
      trunk_radius = trunk_radius[!filter_nan]
    }
    if (length(crown_width_ratio) == nrow(xyz_tree)) {
      crown_width_ratio = crown_width_ratio[!filter_nan]
    }
    if (use_absolute_widths && length(crown_width) == nrow(xyz_tree)) {
      crown_width = crown_width[!filter_nan]
    }
    tree_height = tree_height - z_tree
    if (!is.infinite(max_height) || min_height > 0) {
      filter_height = tree_height >= max_height | tree_height <= min_height

      if (length(long) > 1) {
        long = long[!filter_height]
      }

      if (length(lat) > 1) {
        lat = lat[!filter_height]
      }

      if (length(trunk_color) > 1) {
        trunk_color = trunk_color[!filter_height]
      }

      if (length(trunk_height_ratio) > 1) {
        trunk_height_ratio = trunk_height_ratio[!filter_height]
      }

      if (length(trunk_radius) > 1) {
        trunk_radius = trunk_radius[!filter_height]
      }

      if (length(crown_width_ratio) > 1) {
        crown_width_ratio = crown_width_ratio[!filter_height]
      }

      if (use_absolute_widths) {
        if (length(crown_width) > 1) {
          crown_width = crown_width[!filter_height]
        }
      }

      if (length(tree_height) > 1) {
        tree_height = tree_height[!filter_height]
      }
      if (length(tree_zaxis_raw) > 1) {
        tree_zaxis_raw = tree_zaxis_raw[!filter_height]
        tree_zaxis_scene = tree_zaxis_scene[!filter_height]
      }
    }
    crown_height = (1 - trunk_height_ratio) * tree_height
    trunk_height = trunk_height_ratio * tree_height
    if (length(long) == 0) {
      return(invisible())
    }
  }
  # Determine default trunk height and radius based on tree type
  # The basic trunk included has a radius of 0.6 units and a height of 1.0 units,
  # with the bottom of the trunk located at 0.0 (midpoint at 0.5).
  # The basic spherical crown is centered at zero with a radius of 5.0 units
  # The basic conical crown has a radius of 5.0 units and a height of 10 units, with the base at 0.0
  if (!custom_tree) {
    # Calculate crown radius
    crown_radius = crown_height * crown_width_ratio / 2
    if (is.null(crown_radius)) {
      crown_radius = crown_height / 4
    }
    # Scaling tree dimensions if tree_zscale is TRUE
    if (tree_zscale) {
      crown_radius = crown_radius / zscale
      trunk_radius = trunk_radius / zscale
    }
    #This just ensures the aspect ratio is correct
    if (type == "cone") {
      crown_radius = crown_radius * 2
    }
    crown_width = crown_radius
  } else {
    if (!fully_custom_tree) {
      if (is.null(crown_height)) {
        crown_height = 1
      }
      if (!is.null(trunk_height) || !is.null(trunk_radius)) {
        warning(
          "When specifying single `crown_obj_tree` file (instead of separate ",
          "crown and trunk OBJs), `crown_height` controls the overall scale of ",
          "the tree and trunk settings cannot be changed."
        )
        trunk_height = 1
        trunk_radius = 1
      }
      if (!is.null(crown_width_ratio)) {
        crown_radius = crown_height * crown_width_ratio / 2
      } else {
        crown_radius = 1
      }
    } else {
      if (is.null(trunk_height)) {
        trunk_height = 1
      }
      if (!use_absolute_widths) {
        if (!is.null(crown_width_ratio)) {
          crown_width = crown_height * crown_width_ratio
        } else {
          crown_width = crown_height
        }
      }
      # Scaling tree dimensions if tree_zscale is TRUE
      if (tree_zscale) {
        crown_width = crown_width / zscale
        trunk_radius = trunk_radius / zscale
      }
    }
  }
  if (use_default_trunk_radius) {
    trunk_radius = crown_width / 6
    if (type == "cone") {
      trunk_radius = trunk_radius / 2
    }
  }
  if (length(lat) != length(long)) {
    stop("`lat` and `long` must have the same length.", call. = FALSE)
  }
  height_zscale = 1

  # Expand scalar dimensions to vectors if needed
  if (length(crown_height) == 1) {
    crown_height = rep(crown_height, length(lat))
  }
  if (length(crown_width) == 1) {
    crown_width = rep(crown_width, length(lat))
  }
  if (length(trunk_radius) == 1) {
    trunk_radius = rep(trunk_radius, length(lat))
  }
  if (length(trunk_height) == 1) {
    trunk_height = rep(trunk_height, length(lat))
  }
  crown_color = resolve_ggplot_height_palette_color(
    color = crown_color,
    values = tree_zaxis_raw,
    heightmap = heightmap,
    caller = "render_tree",
    arg_name = "crown_color"
  )
  trunk_color = resolve_ggplot_height_palette_color(
    color = trunk_color,
    values = tree_zaxis_raw,
    heightmap = heightmap,
    caller = "render_tree",
    arg_name = "trunk_color"
  )
  if (!custom_tree) {
    if (tree_zscale) {
      tree_scale = matrix(
        c(crown_width / 5, crown_height / 10 / zscale, crown_width / 5),
        ncol = 3,
        nrow = length(lat)
      )
      trunk_scale = matrix(
        c(
          trunk_radius / 0.3,
          (trunk_height + crown_height / 3) / zscale,
          trunk_radius / 0.3
        ),
        ncol = 3,
        nrow = length(lat)
      )
    } else {
      tree_scale = matrix(
        c(crown_width / 5, crown_height / 10, crown_width / 5),
        ncol = 3,
        nrow = length(lat)
      )
      trunk_scale = matrix(
        c(
          trunk_radius / 0.3,
          (trunk_height + crown_height / 3),
          trunk_radius / 0.3
        ),
        ncol = 3,
        nrow = length(lat)
      )
      height_zscale = zscale
    }
  } else {
    #Scale the custom trees
    if (fully_custom_tree) {
      # For this version, we can control all proportions.
      # This assumes the tree trunk/crown has a radius of 1 and a height of 1.
      if (tree_zscale) {
        tree_scale = matrix(
          c(crown_width, crown_height / zscale, crown_width),
          ncol = 3,
          nrow = length(lat)
        )
        trunk_scale = matrix(
          c(trunk_radius, trunk_height / zscale, trunk_radius),
          ncol = 3,
          nrow = length(lat)
        )
      } else {
        tree_scale = matrix(
          c(crown_width, crown_height, crown_width),
          ncol = 3,
          nrow = length(lat)
        )
        trunk_scale = matrix(
          c(trunk_radius, trunk_height, trunk_radius),
          ncol = 3,
          nrow = length(lat)
        )
        height_zscale = zscale
      }
    } else {
      if (tree_zscale) {
        tree_scale = matrix(
          c(crown_width, crown_height / zscale, crown_width),
          ncol = 3,
          nrow = length(lat)
        )
        trunk_scale = matrix(
          c(trunk_radius, trunk_height / zscale, trunk_radius),
          ncol = 3,
          nrow = length(lat)
        )
      } else {
        tree_scale = matrix(
          c(crown_width, crown_height, crown_width),
          ncol = 3,
          nrow = length(lat)
        )
        height_zscale = zscale
      }
    }
  }
  if (fully_custom_tree) {
    # If a fully custom tree is specified, render the custom crown and trunk
    render_obj_tree(
      custom_obj_crown,
      color = crown_color,
      lat = lat,
      long = long,
      extent = extent,
      panel = panel,
      zscale = zscale,
      crs = render_obj_crs,
      offset = trunk_height * height_zscale,
      heightmap = heightmap,
      angle = angle,
      scale = tree_scale,
      baseshape = baseshape,
      lit = lit,
      transform_scene = !location_supplied && !tree_coords_transformed,
      filter_to_extent = filter_to_extent,
      clear_previous = FALSE,
      rgl_tag = "tree"
    )
    render_obj_tree(
      custom_obj_trunk,
      color = trunk_color,
      lat = lat,
      long = long,
      extent = extent,
      panel = panel,
      zscale = zscale,
      crs = render_obj_crs,
      offset = 0,
      baseshape = baseshape,
      lit = lit,
      heightmap = heightmap,
      angle = angle,
      scale = trunk_scale,
      transform_scene = !location_supplied && !tree_coords_transformed,
      filter_to_extent = filter_to_extent,
      rgl_tag = "tree"
    )
  } else if (custom_tree) {
    # If a custom tree is specified (but not fully custom), render the custom tree
    render_obj_tree(
      custom_obj_tree,
      load_material = TRUE,
      lat = lat,
      long = long,
      extent = extent,
      panel = panel,
      zscale = zscale,
      crs = render_obj_crs,
      offset = 0,
      heightmap = heightmap,
      angle = angle,
      scale = tree_scale,
      baseshape = baseshape,
      transform_scene = !location_supplied && !tree_coords_transformed,
      filter_to_extent = filter_to_extent,
      clear_previous = FALSE,
      rgl_tag = "tree",
      lit = lit
    )
  } else if (type == "basic") {
    # If a basic type is specified, render the basic tree's crown and trunk
    render_obj_tree(
      tree_basic_center_obj(),
      color = crown_color,
      lat = lat,
      long = long,
      extent = extent,
      panel = panel,
      zscale = zscale,
      crs = render_obj_crs,
      offset = (trunk_height + crown_height / 3) * height_zscale,
      heightmap = heightmap,
      angle = angle,
      scale = tree_scale,
      baseshape = baseshape,
      lit = lit,
      transform_scene = !location_supplied && !tree_coords_transformed,
      filter_to_extent = filter_to_extent,
      clear_previous = FALSE,
      rgl_tag = "tree"
    )
    render_obj_tree(
      tree_trunk_obj(),
      color = trunk_color,
      lat = lat,
      long = long,
      extent = extent,
      panel = panel,
      zscale = zscale,
      crs = render_obj_crs,
      offset = 0,
      baseshape = baseshape,
      lit = lit,
      heightmap = heightmap,
      angle = angle,
      scale = trunk_scale,
      transform_scene = !location_supplied && !tree_coords_transformed,
      filter_to_extent = filter_to_extent,
      rgl_tag = "tree"
    )
  } else if (type == "cone") {
    # If a cone type is specified, render the cone tree's crown and trunk
    render_obj_tree(
      tree_cone_center_obj(),
      color = crown_color,
      lat = lat,
      long = long,
      extent = extent,
      panel = panel,
      zscale = zscale,
      crs = render_obj_crs,
      offset = trunk_height * height_zscale,
      baseshape = baseshape,
      lit = lit,
      heightmap = heightmap,
      angle = angle,
      scale = tree_scale,
      transform_scene = !location_supplied && !tree_coords_transformed,
      filter_to_extent = filter_to_extent,
      clear_previous = FALSE,
      rgl_tag = "tree"
    )
    render_obj_tree(
      tree_trunk_obj(),
      color = trunk_color,
      lat = lat,
      long = long,
      extent = extent,
      panel = panel,
      zscale = zscale,
      crs = render_obj_crs,
      offset = 0,
      baseshape = baseshape,
      lit = lit,
      heightmap = heightmap,
      angle = angle,
      scale = trunk_scale,
      transform_scene = !location_supplied && !tree_coords_transformed,
      filter_to_extent = filter_to_extent,
      rgl_tag = "tree"
    )
  } else {
    stop(sprintf("%s not recognized as built-in type of tree", type))
  }
  if (!isTRUE(absolute_height)) {
    tree_zaxis_scene = tree_height
  }
  cache_altitude_zaxis_data(
    source = "tree",
    altitude = tree_zaxis_raw,
    scene_altitude = tree_zaxis_scene,
    label = tree_zaxis_label
  )
  invisible(NULL)
}

resolve_render_tree_height_column = function(
  location,
  tree_height_column = NULL,
  tree_height_supplied = FALSE,
  crs = NULL,
  caller = NULL
) {
  if (is.null(location)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`tree_height_column` requires `location`."
      ),
      call. = FALSE
    )
  }
  if (isTRUE(tree_height_supplied)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`tree_height_column` cannot be combined with `tree_height`."
      ),
      call. = FALSE
    )
  }
  if (
    !is.character(tree_height_column) ||
      length(tree_height_column) != 1 ||
      !nzchar(trimws(tree_height_column))
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`tree_height_column` must be a single non-empty column name."
      ),
      call. = FALSE
    )
  }
  point_input = coerce_scene_point_input(
    location = location,
    crs = crs,
    caller = caller
  )
  if (!tree_height_column %in% names(point_input$point_sf_data)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`tree_height_column` was not found in `location`: ",
        tree_height_column
      ),
      call. = FALSE
    )
  }
  tree_height = point_input$point_sf_data[[tree_height_column]]
  if (inherits(tree_height, "units")) {
    tree_height = units::drop_units(tree_height)
  }
  if (!is.numeric(tree_height)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`tree_height_column` must refer to a numeric column."
      ),
      call. = FALSE
    )
  }
  if (any(!is.finite(tree_height))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`tree_height_column` cannot contain NA, NaN, or infinite values."
      ),
      call. = FALSE
    )
  }
  as.numeric(tree_height)
}
