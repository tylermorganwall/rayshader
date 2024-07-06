#'@title Render Depth of Field
#'
#'@description Adds depth of field to the current RGL scene by simulating a synthetic aperture. 
#'
#'The size of the circle of confusion is determined by the following formula (z_depth is from the image's depth map).
#'
#'\code{abs(z_depth-focus)*focal_length^2/(f_stop*z_depth*(focus - focal_length))}
#'
#'@param focus Focal point. Defaults to the center of the bounding box. Depth in which to blur, in distance to the camera plane.
#'@param focallength Default `1`. Focal length of the virtual camera.
#'@param fstop Default `1`. F-stop of the virtual camera.
#'@param filename The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param preview_focus Default `FALSE`. If `TRUE`, a red line will be drawn across the image
#'showing where the camera will be focused.
#'@param bokehshape Default `circle`. Also built-in: `hex`. The shape of the bokeh. 
#'@param bokehintensity Default `3`. Intensity of the bokeh when the pixel intensity is greater than `bokehlimit`.
#'@param bokehlimit Default `0.8`. Limit after which the bokeh intensity is increased by `bokehintensity`. 
#'@param rotation Default `0`. Number of degrees to rotate the hexagon bokeh shape.
#'@param aberration Default `0`. Adds chromatic aberration to the image. Maximum of `1`. 
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@param transparent_water Default `FALSE`. If `TRUE`, depth is determined without water layer. User will have to re-render the water
#'layer with `render_water()` if they want to recreate the water layer.
#'@param heightmap Default `NULL`. The height matrix for the scene. Passing this will allow `render_depth()` 
#'to automatically redraw the water layer if `transparent_water = TRUE`.
#'@param zscale Default `NULL`. The zscale value for the heightmap. Passing this will allow `render_depth()` 
#'to automatically redraw the water layer if `transparent_water = TRUE`.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using magick::image_annotate. 
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in 
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica", 
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_bar_color Default `NULL`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param title_position Default `northwest`. Position of the title.
#'@param image_overlay Default `NULL`. Either a string indicating the location of a png image to overlay
#'over the image (transparency included), or a 4-layer RGBA array. This image will be resized to the 
#'dimension of the image if it does not match exactly.
#'@param vignette Default `FALSE`. If `TRUE` or numeric, a camera vignetting effect will be added to the image.
#'`1` is the darkest vignetting, while `0` is no vignetting. If vignette is a length-2 vector, the second entry will
#'control the blurriness of the vignette effect.
#'@param vignette_color Default `"black"`. Color of the vignette.
#'@param vignette_radius Default `1.3`. Radius of the vignette, as a porportion of the image dimensions.
#'@param width Default `NULL`. Optional argument to pass to `rgl::snapshot3d()` to specify the
#'width when `software_render = TRUE`..
#'@param height Default `NULL`. Optional argument to pass to `rgl::snapshot3d()` to specify the
#'height when `software_render = TRUE`.
#'@param progbar Default `TRUE` if in an interactive session. Displays a progress bar. 
#'@param instant_capture Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, a slight delay is added 
#'before taking the snapshot. This can help stop prevent rendering issues when running scripts.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param bring_to_front Default `FALSE`. Whether to bring the window to the front when rendering the snapshot.
#'@param software_render Default `FALSE`. If `TRUE`, rayshader will use the rayvertex package to render the snapshot, which
#'is not constrained by the screen size or requires OpenGL. 
#'@param cache_scene Default `FALSE`. Whether to cache the current scene to memory so it does not have to be converted to a `raymesh` object 
#'each time `render_snapshot()` is called. If `TRUE` and a scene has been cached, it will be used when rendering.
#'@param reset_scene_cache Default `FALSE`. Resets the scene cache before rendering.
#'@param background Default `"white"`. Background color when `software_render = TRUE`.
#'@param text_angle Default `NULL`, which forces the text always to face the camera. If a single angle (degrees),
#'will specify the absolute angle all the labels are facing. If three angles, this will specify all three orientations
#'(relative to the x,y, and z axes) of the text labels.
#'@param text_size Default `10`. Height of the text.
#'@param point_radius Default `0.5`. Radius of 3D points (rendered with `render_points()`.
#'@param line_offset Default `1e-7`. Small number indicating the offset in the scene to apply to lines if using software rendering. Increase this if your lines 
#'aren't showing up, or decrease it if lines are appearing through solid objects.
#'@param camera_location Default `NULL`. Custom position of the camera. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window.
#'@param camera_lookat Default `NULL`. Custom point at which the camera is directed. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window.
#'@param text_offset Default `c(0,0,0)`. Offset to be applied to all text labels.
#'@param print_scene_info Default `FALSE`. If `TRUE`, it will print the position and lookat point of the camera.
#'@param ... Additional parameters to pass to `rayvertex::rasterize_scene()`. 
#'@return 4-layer RGBA array.
#'@export
#'@examples
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50, water=TRUE, waterlinecolor="white",
#'          zoom=0.3,theta=-135,fov=70, phi=20) 
#'  
#'#Preview where the focal plane lies
#'render_depth(preview_focus=TRUE)
#'}
#'if(run_documentation()) {
#'#Render the depth of field effect
#'render_depth(focallength = 300)
#'}
#'if(run_documentation()) {
#'#Add a chromatic aberration effect
#'render_depth(focallength = 300, aberration = 0.3)
#'}
#'if(run_documentation()) {
#'#Render the depth of field effect, ignoring water and re-drawing the waterlayer
#'render_depth(preview_focus=TRUE, 
#'             heightmap = montereybay, zscale=50, focallength=300, transparent_water=TRUE)
#'render_depth(heightmap = montereybay, zscale=50, focallength=300, transparent_water=TRUE)
#'render_camera(theta=45,zoom=0.15,phi=20)
#'}
#'
#'if(run_documentation()) {
#'#Change the bokeh shape and intensity
#'render_depth(focus=900, bokehshape = "circle",focallength=500,bokehintensity=30,
#'             title_text = "Circular Bokeh", title_size = 30, title_color = "white", 
#'             title_bar_color = "black")
#'render_depth(focus=900, bokehshape = "hex",focallength=500,bokehintensity=30,
#'             title_text = "Hexagonal Bokeh", title_size = 30, title_color = "white", 
#'             title_bar_color = "black")
#'}
#'
#'if(run_documentation()) {
#'#Add a title and vignette effect.
#'render_camera(theta=0,zoom=0.7,phi=30)
#'render_depth(focallength = 250, title_text = "Monterey Bay, CA", 
#'             title_size = 20, title_color = "white", title_bar_color = "black", vignette = TRUE)
#'}
render_depth = function(focus = NULL, focallength = 100, fstop = 4, filename=NULL,
                     preview_focus = FALSE, bokehshape = "circle", bokehintensity = 1, bokehlimit=0.8, 
                     rotation = 0, gamma_correction = TRUE, aberration = 0,
                     transparent_water = FALSE, heightmap = NULL, zscale = NULL, 
                     title_text = NULL, title_offset = c(20,20),
                     title_color = "black", title_size = 30, title_font = "sans",
                     title_bar_color = NULL, title_bar_alpha = 0.5, title_position = "northwest",
                     image_overlay = NULL, 
                     vignette = FALSE, vignette_color = "black", vignette_radius = 1.3,
                     progbar = interactive(), software_render = FALSE, 
                     width = NULL, height = NULL, 
                     camera_location = NULL, camera_lookat = c(0,0,0),
                     background = "white",
                     text_angle = NULL, text_size = 10, text_offset = c(0,0,0),
                     point_radius = 0.5, line_offset = 1e-7,
                     cache_scene  = FALSE,  reset_scene_cache = FALSE,
                     print_scene_info = FALSE,
                     instant_capture = interactive(), clear = FALSE, bring_to_front = FALSE, ...) {
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if(!instant_capture) {
    Sys.sleep(0.5)
  }
  if(focallength < 1) {
    stop("focal length must be greater than 1")
  }
  if(reset_scene_cache) {
    assign("scene_cache", NULL, envir = ray_cache_scene_envir)
  }
  if(is.null(focus)) {
    fov = rgl::par3d()$FOV
    rotmat = rot_to_euler(rgl::par3d()$userMatrix)
    projmat = rgl::par3d()$projMatrix
    zoom = rgl::par3d()$zoom
    scalevals = rgl::par3d("scale")
    
    phi = rotmat[1]
    if(0.001 > abs(abs(rotmat[3]) - 180)) {
      theta = -rotmat[2] + 180
      movevec = rgl::rotationMatrix(-rotmat[2]*pi/180, 0, 1, 0) %*%
        rgl::rotationMatrix(-rotmat[1]*pi/180, 1, 0, 0) %*% 
        rgl::par3d()$userMatrix[,4]
    } else {
      theta = rotmat[2]
      movevec = rgl::rotationMatrix(rotmat[3]*pi/180, 0, 0, 1) %*%
        rgl::rotationMatrix(-rotmat[2]*pi/180, 0, 1, 0) %*%
        rgl::rotationMatrix(-rotmat[1]*pi/180, 1, 0, 0) %*% 
        rgl::par3d()$userMatrix[,4]
    }
    observer_radius = rgl::par3d()$observer[3]
    observery = sinpi(phi/180) * observer_radius
    observerx = cospi(phi/180) * sinpi(theta/180) * observer_radius
    observerz = cospi(phi/180) * cospi(theta/180) * observer_radius
    
    focus = sqrt(observerx^2 + observery^2 + observerz^2)
    message(sprintf("Focus distance: %g", focus))
  }
  if(rgl::rgl.useNULL()) {
    software_render = TRUE
  }
  temp = paste0(tempfile(),".png")
  if(!software_render) {
    render_snapshot(filename=temp, software_render = software_render)
  } else {
    all_image = render_snapshot_software(filename = temp, cache_scene = cache_scene,
                             camera_location = camera_location, camera_lookat = camera_lookat,
                             background = background, debug="all",
                             width = width, height = height, light_direction = c(0,1,0), fake_shadow = TRUE, 
                             text_angle = text_angle, text_size = text_size, text_offset = text_offset,
                             print_scene_info = print_scene_info, point_radius = point_radius, ...)
  }
  
  if(transparent_water) {
    idlist = get_ids_with_labels(typeval = c("water","waterlines"))
    waterid = idlist$id[idlist$tag == "water"][1]
    waterdepthval = max(rgl::rgl.attrib(waterid, "vertices")[1:3,2],na.rm=TRUE)
    has_waterlines = FALSE
    water_color = idlist$water_color[idlist$tag == "water"][1]
    water_alpha = idlist$water_alpha[idlist$tag == "water"][1]
    if("waterlines" %in% idlist$tag) {
      has_waterlines = TRUE
      water_line_color = idlist$waterline_color[idlist$tag == "waterlines"][1]
      water_line_alpha = idlist$waterline_alpha[idlist$tag == "waterlines"][1]
    }
    rgl::pop3d(id=idlist$id)
    if(software_render) {
      new_depth = render_snapshot_software(filename = temp, cache_scene = cache_scene,
                                           camera_location = camera_location, camera_lookat = camera_lookat,
                                           background = background, debug="linear_depth",
                                           width = width, height = height, light_direction = c(0,1,0), fake_shadow = TRUE, 
                                           text_angle = text_angle, text_size = text_size, text_offset = text_offset,
                                           print_scene_info = print_scene_info, point_radius = point_radius, 
                                           line_offset = -line_offset, ...)
      all_image$linear_depth = new_depth
    }
  }
  if(!software_render) {
    image_to_convolve = png::readPNG(temp)
    depthmap = (rgl::rgl.pixels(component = "depth"))
    depthmap = 2*depthmap-1
    projmat = rgl::par3d()$projMatrix
    A = projmat[3,3]
    B = projmat[3,4]
    near_clip = B/(A-1)
    far_clip = B/(A+1)
    depthmap = 2*near_clip*far_clip/(far_clip + near_clip - depthmap * (far_clip-near_clip));
  } else {
    image_to_convolve = array(0,dim=c(dim(all_image$r)[2:1],3))
    image_to_convolve[,,1] = flipud(t(all_image$r))
    image_to_convolve[,,2] = flipud(t(all_image$g))
    image_to_convolve[,,3] = flipud(t(all_image$b))
    depthmap = (all_image$linear_depth)
  }
  depthmap = rayimage::render_reorient(depthmap, flipx = TRUE, transpose = TRUE)
  if(preview_focus) {
    depthmap = (depthmap)
    temp_depth = paste0(tempfile(),".png")
    if(nrow(depthmap) < 1) {
      message("Can't fetch depth component, stopping")
      return(NULL)
    }
    maxval = max(depthmap[depthmap != 1])
    depthmap[depthmap == 1] = maxval
    rayimage::render_bokeh(image_to_convolve, depthmap, focus = focus, preview_focus = TRUE, preview = TRUE)
    if(transparent_water && !is.null(heightmap)) {
      if(is.null(zscale)) {
        zscale = 1
      }
      if(has_waterlines) {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     waterlinealpha = water_line_alpha, waterlinecolor = water_line_color,
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      } else {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      }
    }
  } else {
    temp_depth = paste0(tempfile(),".png")
    if(nrow(depthmap) < 1) {
      message("Can't fetch depth component, stopping")
      return(NULL)
    }
    if(transparent_water) {
      rgl::pop3d(tag="waterlines")
    }
    tempmap = png::readPNG(temp)
    if(transparent_water && !is.null(heightmap)) {
      if(is.null(zscale)) {
        zscale = 1
      }
      if(has_waterlines) {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     waterlinealpha = water_line_alpha, waterlinecolor = water_line_color,
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      } else {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      }
    }
    if(all(dim(tempmap)[1:2] != dim(depthmap)[1:2])) {
      depthmap = rayimage::render_resized(depthmap, dims = dim(tempmap)[1:2])
    }
    tempmap = rayimage::render_bokeh(tempmap, depthmap = depthmap, focus = focus, focallength = focallength,
                           fstop = fstop, bokehshape = bokehshape, bokehintensity = bokehintensity,
                           bokehlimit = bokehlimit, rotation = rotation, aberration = aberration,
                           gamma_correction = gamma_correction, progress = progbar, preview = FALSE)
    if(!is.null(title_text)) {
      has_title = TRUE
    } else {
      has_title = FALSE
    }
    if(length(title_offset) != 2) {
      stop("`title_offset` needs to be length-2 vector")
    }
    if(!is.null(image_overlay)) {
      if(inherits(image_overlay,"character")) {
        image_overlay_file = image_overlay
        has_overlay = TRUE
      } else if(inherits(image_overlay,"array")) {
        image_overlay_file = tempfile()
        png::writePNG(image_overlay, image_overlay_file)
        has_overlay = TRUE
      }
    } else {
      has_overlay = FALSE
    }
    dimensions = dim(tempmap)
    if(has_overlay) {
      tempmap = rayimage::add_image_overlay(tempmap, image_overlay = image_overlay_file)
    }
    if(vignette || is.numeric(vignette)) {
      tempmap = rayimage::add_vignette(tempmap, vignette = vignette, color = vignette_color,
                                       radius = vignette_radius)
    }
    if(has_title) {
      tempmap = rayimage::add_title(tempmap, title_text = title_text, 
                          title_bar_color = title_bar_color,title_bar_alpha = title_bar_alpha,
                          title_offset = title_offset, title_color = title_color,
                          title_position = title_position,
                          title_size = title_size, title_font = title_font)
    }
    if(is.null(filename)) {
      plot_map(tempmap)
    } else {
      save_png(tempmap,filename)
    }
    if(clear) {
      rgl::clear3d()
    }
  }
}
