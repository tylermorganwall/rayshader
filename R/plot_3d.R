#'@title Plot 3D 
#'
#'@description Displays the shaded map in 3D with the `rgl` package. 
#'
#'Note: Calling `plot_3d()` resets the scene cache for the `render_snapshot()`, `render_depth()`, and `render_highquality()`
#'
#'@param hillshade Hillshade/image to be added to 3D surface map.
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
#'@param baseshape Default `rectangle`. Shape of the base. Options are `c("rectangle","circle","hex")`.
#'@param solid Default `TRUE`. If `FALSE`, just the surface is rendered.
#'@param soliddepth Default `auto`, which sets it to the lowest elevation in the matrix minus one unit (scaled by zscale). Depth of the solid base. If heightmap is uniform and set on `auto`, this is automatically set to a slightly lower level than the uniform elevation.
#'@param solidcolor Default `grey20`. Base color.
#'@param solidlinecolor Default `grey30`. Base edge line color.
#'@param shadow Default `TRUE`. If `FALSE`, no shadow is rendered.
#'@param shadowdepth Default `auto`, which sets it to `soliddepth - soliddepth/10`. Depth of the shadow layer.
#'@param shadowcolor Default `auto`. Color of the shadow, automatically computed as `shadow_darkness`
#'the luminance of the `background` color in the CIELuv colorspace if not specified.
#'@param shadow_darkness Default `0.5`. Darkness of the shadow, if `shadowcolor = "auto"`.
#'@param shadowwidth Default `auto`, which sizes it to 1/10th the smallest dimension of `heightmap`. Width of the shadow in units of the matrix. 
#'@param water Default `FALSE`. If `TRUE`, a water layer is rendered.
#'@param waterdepth Default `0`. Water level.
#'@param watercolor Default `lightblue`. Color of the water.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency. 
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param lineantialias Default `FALSE`. Whether to anti-alias the lines in the scene.
#'@param soil Default `FALSE`. Whether to draw the solid base with a textured soil layer.
#'@param soil_freq Default `0.1`. Frequency of soil clumps. Higher frequency values give smaller soil clumps.
#'@param soil_levels Default `16`. Fractal level of the soil.
#'@param soil_color_light Default `"#b39474"`. Light tint of soil.
#'@param soil_color_dark Default `"#8a623b"`. Dark tint of soil.
#'@param soil_gradient Default `2`. Sharpness of the soil darkening gradient. `0` turns off the gradient entirely.
#'@param soil_gradient_darken Default `4`. Amount to darken the `soil_color_dark` value for the deepest soil layers. Higher
#'numbers increase the darkening effect.
#'@param theta Default `45`. Rotation around z-axis.
#'@param phi Default `45`. Azimuth angle.
#'@param fov Default `0`--isometric. Field-of-view angle.
#'@param zoom Default `1`. Zoom factor.
#'@param background Default `grey10`. Color of the background.
#'@param windowsize Default `600`. Position, width, and height of the `rgl` device displaying the plot. 
#'If a single number, viewport will be a square and located in upper left corner. 
#'If two numbers, (e.g. `c(600,800)`), user will specify width and height separately. 
#'If four numbers (e.g. `c(200,0,600,800)`), the first two coordinates 
#'specify the location of the x-y coordinates of the bottom-left corner of the viewport on the screen,
#'and the next two (or one, if square) specify the window size. NOTE: The absolute positioning of the
#'window does not currently work on macOS (tested on Mojave), but the size can still be specified.
#'@param precomputed_normals Default `NULL`. Takes the output of `calculate_normals()` to save
#' computing normals internally.
#'@param asp Default `1`. Aspect ratio of the resulting plot. Use `asp = 1/cospi(mean_latitude/180)` to rescale
#'lat/long at higher latitudes to the correct the aspect ratio.
#'@param triangulate Default `FALSE`. Reduce the size of the 3D model by triangulating the height map.
#'Set this to `TRUE` if generating the model is slow, or moving it is choppy. Will also reduce the size
#'of 3D models saved to disk.
#'@param max_error Default `0.001`. Maximum allowable error when triangulating the height map,
#'when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with `render_highquality()`, or need 
#'to save a smaller 3D OBJ file to disk with `save_obj()`,
#'@param max_tri Default `0`, which turns this setting off and uses `max_error`. 
#'Maximum number of triangles allowed with triangulating the
#'height map, when `triangulate = TRUE`. Increase this if you encounter problems with 3D performance, want
#'to decrease render time with `render_highquality()`, or need 
#'to save a smaller 3D OBJ file to disk with `save_obj()`,
#'@param verbose Default `TRUE`, if `interactive()`. Prints information about the mesh triangulation
#'if `triangulate = TRUE`.
#'@param plot_new Default `TRUE`, opens new window with each `plot_3d()` call. If `FALSE`, 
#'the data will be plotted in the same window.
#'@param close_previous Default `TRUE`. Closes any previously open `rgl` window. If `FALSE`, 
#'old windows will be kept open.
#'@param clear_previous Default `TRUE`. Clears the previously open `rgl` window if `plot_new = FALSE`.
#'
#'@import rgl
#'@export
#'@examples
#'#Plotting a spherical texture map of the built-in `montereybay` dataset.
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade(texture="desert") %>%
#'  plot_3d(montereybay,zscale=50)
#'render_snapshot()
#'}
#'
#'#With a water layer  
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade(texture="imhof2") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof2", 
#'          waterlinecolor="white", waterlinealpha=0.5)
#'render_snapshot()
#'}
#'
#'#With a soil texture to the base  
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade(texture="imhof3") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE,  watercolor="imhof4", 
#'          waterlinecolor="white", waterlinealpha=0.5, soil=TRUE)
#'render_camera(theta=225, phi=7, zoom=0.5, fov=67)
#'render_snapshot()
#'}
#'
#'#We can also change the base by setting "baseshape" to "hex" or "circle"
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", theta=-45, zoom=0.7,
#'          waterlinecolor="white", waterlinealpha=0.5,baseshape="circle")
#'render_snapshot()
#'}
#'
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", theta=-45, zoom=0.7,
#'          waterlinecolor="white", waterlinealpha=0.5,baseshape="hex")
#'render_snapshot()
#'}
#'
#'
#'
#'#Or we can carve out the region of interest ourselves, by setting those entries to NA
#'#to the elevation map passed into `plot_3d`
#'
#'#Here, we only include the deep bathymetry data by setting all points greater than -10
#'#in the copied elevation matrix to NA.
#'
#'mb_water = montereybay
#'mb_water[mb_water > -10] = NA
#'
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(mb_water, zscale=50, water = TRUE, watercolor="imhof1", theta=-45,
#'          waterlinecolor="white", waterlinealpha=0.5)
#'render_snapshot()
#'}
plot_3d = function(hillshade, heightmap, zscale=1, baseshape="rectangle",
                   solid = TRUE, soliddepth="auto", solidcolor="grey20",solidlinecolor="grey30",
                   shadow = TRUE, shadowdepth = "auto", shadowcolor = "auto", shadow_darkness = 0.5,
                   shadowwidth = "auto", 
                   water = FALSE, waterdepth = 0, watercolor="dodgerblue", wateralpha = 0.5, 
                   waterlinecolor=NULL, waterlinealpha = 1, 
                   linewidth = 2, lineantialias = FALSE, 
                   soil = FALSE, soil_freq = 0.1, soil_levels = 16, 
                   soil_color_light = "#b39474", soil_color_dark = "#8a623b", 
                   soil_gradient = 2, soil_gradient_darken = 4,
                   theta=45, phi = 45, fov=0, zoom = 1, background="white", windowsize = 600,
                   precomputed_normals = NULL, asp = 1,
                   triangulate = FALSE, max_error = 0, max_tri = 0, verbose = FALSE,
                   plot_new = TRUE, close_previous = TRUE, clear_previous = TRUE) {
  if(!plot_new && clear_previous) {
    rgl::clear3d()
  }
  if(!is.null(get("scene_cache", envir = ray_cache_scene_envir))) {
    assign("scene_cache", NULL, envir = ray_cache_scene_envir)
  }
  #setting default zscale if montereybay is used and tell user about zscale
  argnames = as.list(sys.call())
  if(!is.null(attr(heightmap,"rayshader_data"))) {
    if (!("zscale" %in% as.character(names(argnames)))) {
      if(length(argnames) <= 3) {
        zscale = 50
        message("`montereybay` dataset used with no zscale--setting `zscale=50`. For a realistic depiction, raise `zscale` to 200.")
      } else {
        if (!is.numeric(argnames[[4]]) || !is.null(names(argnames))) {
          if(names(argnames)[4] != "")  {
            zscale = 50
            message("`montereybay` dataset used with no zscale--setting `zscale=50`.  For a realistic depiction, raise `zscale` to 200.")
          }
        }
      }
    }
  }
  if(shadowcolor == "auto") {
    shadowcolor = convert_color(darken_color(background, darken=shadow_darkness), as_hex = TRUE)
  }
  #Set window size and position
  if(length(windowsize) == 1) {
    windowsize = c(0,0,windowsize,windowsize)
  } else if (length(windowsize) == 2) {
    windowsize = c(0,0,windowsize)
  } else if (length(windowsize) == 3) {
    windowsize = c(windowsize[1],windowsize[2],windowsize[1]+windowsize[3],windowsize[2]+windowsize[3])
  } else if (length(windowsize) == 4) {
    windowsize = c(windowsize[1],windowsize[2],windowsize[1]+windowsize[3],windowsize[2]+windowsize[4])
  } else {
    stop(paste0("Don't know what to do with `windowsize` argument of length ",length(windowsize)))
  }
  heightmap = generate_base_shape(heightmap, baseshape)
  
  if(any(hillshade > 1 | hillshade < 0, na.rm = TRUE)) {
    stop("Argument `hillshade` must not contain any entries less than 0 or more than 1")
  }
  flipud = function(x) {
    x[nrow(x):1,]
  }
  if(is.null(heightmap)) {
    stop("heightmap argument missing--need to input both hillshade and original elevation matrix")
  }
  min_height = min(heightmap,na.rm=TRUE)
  max_height = max(heightmap,na.rm=TRUE)
  if(soliddepth == "auto") {
    if(min_height != max_height) {
      soliddepth = min_height/zscale - (max_height/zscale-min_height/zscale)/5
    } else {
      max_dim = max(dim(heightmap))
      soliddepth = min_height/zscale - max_dim/25
    }
  } else {
    if(soliddepth > min_height) {
      message(sprintf("`soliddepth` (set to %f) must be less than or equal to heightmap minimum value (%f). Setting to min(heightmap)",
                      soliddepth, min_height))
      soliddepth = min_height/zscale
    } else {
      soliddepth = soliddepth/zscale
    }
  }
  if(solid) {
    min_height_shadow = min(c(min_height, soliddepth*zscale))
  } else {
    min_height_shadow = min_height
  }
  if(shadowdepth == "auto") {
    if(min_height_shadow != max_height) {
      if(solid) {
        shadowdepth = soliddepth - (max_height/zscale-min_height_shadow/zscale)/5
      } else {
        shadowdepth = min_height_shadow/zscale - (max_height/zscale-min_height_shadow/zscale)/5
      }
    } else {
      if(solid) {
        max_dim = max(dim(heightmap))
        shadowdepth = soliddepth - max_dim/25
      } else {
        max_dim = max(dim(heightmap))
        shadowdepth = min_height - max_dim/25
      }
    }
  } else {
    if(shadowdepth > min_height) {
      message(sprintf("`shadowdepth` (set to %f) is greater to heightmap minimum value (%f). Shadow will appear to be intersecting 3D model.",
                      shadowdepth, min_height))
    } else {
      shadowdepth = shadowdepth/zscale
    }
  }
  if(shadowwidth == "auto") {
    shadowwidth = max(floor(min(dim(heightmap))/10),5)
  }
  if(water) {
    if (watercolor == "imhof1") {
      watercolor = "#defcf5"
    } else if (watercolor == "imhof2") {
      watercolor = "#337c73"
    } else if (watercolor == "imhof3") {
      watercolor = "#4e7982"
    } else if (watercolor == "imhof4") {
      watercolor = "#638d99"
    } else if (watercolor == "desert") {
      watercolor = "#caf0f7"
    } else if (watercolor == "bw") {
      watercolor = "#dddddd"
    } else if (watercolor == "unicorn") {
      watercolor = "#ff00ff"
    }
    if (is.null(waterlinecolor)) {
      
    } else if (waterlinecolor == "imhof1") {
      waterlinecolor = "#f9fffb"
    } else if (waterlinecolor == "imhof2") {
      waterlinecolor = "#8accc4"
    } else if (waterlinecolor == "imhof3") {
      waterlinecolor = "#8cd4e2"
    } else if (waterlinecolor == "imhof4") {
      waterlinecolor = "#c7dfe5"
    } else if (waterlinecolor == "desert") {
      waterlinecolor = "#cde3f2"
    } else if (waterlinecolor == "bw") {
      waterlinecolor = "#ffffff"
    } else if (waterlinecolor == "unicorn") {
      waterlinecolor = "#ffd1fb"
    }
  }
  tempmap = tempfile(fileext=".png")
  save_png(hillshade,tempmap)
  precomputed = FALSE
  if(is.list(precomputed_normals)) {
    normals = precomputed_normals
    precomputed = TRUE
  }
  if(triangulate && any(is.na(heightmap))) {
    if(interactive()) {
      message("`triangulate = TRUE` cannot be currently set if any NA values present--settings `triangulate = FALSE`")
    }
    triangulate = FALSE
  }
  
  if(close_previous && rgl::cur3d() != 0) {
    rgl::close3d()
  }
  if(plot_new || rgl::cur3d() == 0) {
    rgl::open3d(windowRect = windowsize, 
                mouseMode = c("none", "polar", "fov", "zoom", "pull"))
  }
  rgl::view3d(zoom = zoom, phi = phi, theta = theta, fov = fov)
  attributes(heightmap) = attributes(heightmap)["dim"]
  if(!triangulate) {
    if(!precomputed) {
      normals = calculate_normal(heightmap, zscale = zscale)
    }
    dim(heightmap) = unname(dim(heightmap))
    normalsx = (t(normals$x[c(-1, -nrow(normals$x)), c(-1,-ncol(normals$x))]))
    normalsy = (t(normals$z[c(-1, -nrow(normals$z)), c(-1,-ncol(normals$z))]))
    normalsz = (t(normals$y[c(-1, -nrow(normals$y)), c(-1,-ncol(normals$y))]))
    replace_na_vals = is.na(normalsx) | is.na(normalsy) | is.na(normalsz)
    normalsx[replace_na_vals] = 0
    normalsy[replace_na_vals] = 1
    normalsz[replace_na_vals] = 0
    
    ray_surface = generate_surface(heightmap, zscale = zscale)
    rgl::triangles3d(x = ray_surface$verts, 
                     indices = ray_surface$inds, 
                     texcoords = ray_surface$texcoords, 
                     normals = matrix(c(normalsz,normalsy,-normalsx), ncol = 3L),
                     texture = tempmap, color="white", lit = FALSE, tag = "surface_tris",
                     back = "culled")
  } else {
    tris = terrainmeshr::triangulate_matrix(heightmap, maxError = max_error, 
                                            maxTriangles = max_tri, start_index = 0L, 
                                            verbose = verbose)
    index_vals = seq_len(nrow(tris))
    # if(!precomputed) {
    #   normals = calculate_normal(heightmap,zscale=zscale)
    # }
    # normalsx = as.vector(t(flipud(normals$x[c(-1,-nrow(normals$x)),c(-1,-ncol(normals$x))])))
    # normalsy = as.vector(t(flipud(normals$z[c(-1,-nrow(normals$z)),c(-1,-ncol(normals$z))])))
    # normalsz = as.vector(t(flipud(normals$y[c(-1,-nrow(normals$y)),c(-1,-ncol(normals$y))])))
    tris[,2] =  tris[,2]/zscale
    nr = nrow(heightmap)
    nc = ncol(heightmap)
    # rn = tris[,1]+1
    # cn = tris[,3]+1
    
    # normal_comp = matrix(c(normalsz[rn + nr*(cn-1)],normalsy[rn + nr*(cn-1)],-normalsx[rn + nr*(cn-1)]),ncol=3)
    texcoords = tris[,c(1,3)]
    texcoords[,1] = texcoords[,1]/(nr-1)
    texcoords[,2] = texcoords[,2]/(nc-1)
    tris[,1] = tris[,1] - (nr-1)/2# +1
    tris[,3] = tris[,3] - (nc-1)/2
    tris[,3] = -tris[,3]
    rgl::triangles3d(tris, texcoords = texcoords, 
                     indices = index_vals, back = "cull",
                     #normals = normal_comp,
                     texture=tempmap,lit=FALSE,color="white",tag = "surface_tris")
  }
  rgl::bg3d(color = background,texture=NULL)
  if(solid && !triangulate) {
    make_base(heightmap,basedepth=soliddepth,basecolor=solidcolor,zscale=zscale, 
              soil = soil, soil_freq = soil_freq, soil_levels = soil_levels, soil_color1=soil_color_light,
              soil_color2=soil_color_dark, soil_gradient = soil_gradient, gradient_darken = soil_gradient_darken)
  } else if(solid && triangulate) {
    make_base_triangulated(tris,basedepth=soliddepth,basecolor=solidcolor)
  }
  if(!is.null(solidlinecolor) && solid) {
    make_lines(heightmap,basedepth=soliddepth,linecolor=solidlinecolor,zscale=zscale,linewidth = linewidth)
  }
  if(shadow) {
    make_shadow(heightmap, shadowdepth, shadowwidth, background, shadowcolor)
  }
  if(water) {
    make_water(heightmap,waterheight=waterdepth,wateralpha=wateralpha,watercolor=watercolor,zscale=zscale)
  }
  if(!is.null(waterlinecolor) && water) {
    if(all(!is.na(heightmap))) {
      make_lines(fliplr(heightmap),basedepth=waterdepth,linecolor=waterlinecolor,
                 zscale=zscale,linewidth = linewidth,alpha=waterlinealpha,solid=FALSE)
    }
    make_waterlines(heightmap,waterdepth=waterdepth,linecolor=waterlinecolor,
                    zscale=zscale,alpha=waterlinealpha,linewidth=linewidth,antialias=lineantialias)
  }
  if(asp != 1) {
    height_range = range(heightmap,na.rm=TRUE)/zscale
    height_scale = height_range[2]-height_range[1]
    rgl::aspect3d(x = dim(heightmap)[1]/height_scale, y = 1, z = dim(heightmap)[2]*asp/height_scale)
  }
}
