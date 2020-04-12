#'@title Render High Quality
#'
#'@description Renders a raytraced version of the displayed rgl scene, using the `rayrender` package. 
#'User can specify the light direction, intensity, and color, as well as specify the material of the 
#'ground and add additional scene elements.
#'
#'@param filename Filename of saved image. If missing, will display to current device.
#'@param light Default `TRUE`. Whether there should be a light in the scene. If not, the scene will be lit with a bluish sky.
#'@param lightdirection Default `315`. Position of the light angle around the scene. 
#'If this is a vector longer than one, multiple lights will be generated (using values from 
#'`lightaltitude`, `lightintensity`, and `lightcolor`)
#'@param lightaltitude Default `45`. Angle above the horizon that the light is located. 
#'If this is a vector longer than one, multiple lights will be generated (using values from 
#'`lightdirection`, `lightintensity`, and `lightcolor`)
#'@param lightsize Default `NULL`. Radius of the light(s). Automatically chosen, but can be set here by the user.
#'@param lightintensity Default `500`. Intensity of the light.
#'@param lightcolor Default `white`. The color of the light.
#'@param obj_material Default `rayrender::diffuse()`. The material properties of the object file. 
#'@param cache_filename Name of temporary filename to store OBJ file, if the user does not want to rewrite the file each time.
#'@param width Defaults to the width of the rgl window. Width of the rendering. 
#'@param height Defaults to the height of the rgl window. Height of the rendering. 
#'@param text_angle Default `NULL`, which forces the text always to face the camera. If a single angle (degrees),
#'will specify the absolute angle all the labels are facing. If three angles, this will specify all three orientations
#'(relative to the x,y, and z axes) of the text labels.
#'@param text_size Default `6`. Height of the text.
#'@param text_offset Default `c(0,0,0)`. Offset to be applied to all text labels.
#'@param line_radius Default `0.5`. Radius of the label line segments.
#'@param scale_text_angle Default `NULL`. Same as `text_angle`, but for the scale bar.
#'@param scale_text_size Default `6`. Height of the scale bar text.
#'@param scale_text_offset Default `c(0,0,0)`. Offset to be applied to all scale bar text labels.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using magick::image_annotate. 
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in 
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica", 
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_bar_color Default `NULL`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param ground_material Default `diffuse()`. Material defined by the rayrender material functions.
#'@param ground_size Default `100000`. The width of the plane representing the ground.
#'@param camera_location Default `NULL`. Custom position of the camera. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window.
#'@param camera_lookat Default `NULL`. Custom point at which the camera is directed. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window.
#'@param camera_interpolate Default `c(0,0)`. Maximum `1`, minimum `0`. Sets the camera at a point between the `rgl` view and the `camera_location` 
#'and `camera_lookat` vectors.
#'@param scene_elements Default `NULL`. Extra scene elements to add to the scene, created with rayrender.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param print_scene_info Default `FALSE`. If `TRUE`, it will print the position and lookat point of the camera.
#'@param ... Additional parameters to pass to rayrender::render_scene()
#'@export
#'@examples
#'#Render the volcano dataset using pathtracing
#'\donttest{
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale = 2)
#'render_highquality()
#'}
#'
#'#Change position of light
#'\donttest{
#'render_highquality(lightdirection = 45)
#'}
#'
#'#Change vertical position of light
#'\donttest{
#'render_highquality(lightdirection = 45, lightaltitude=10)
#'}
#'
#'#Change the ground material
#'\donttest{
#'render_highquality(lightdirection = 45, lightaltitude=60, 
#'                   ground_material = rayrender::diffuse(checkerperiod = 30, checkercolor="grey50"))
#'}
#'
#'#Add three different color lights and a title
#'\donttest{
#'render_highquality(lightdirection = c(0,120,240), lightaltitude=45, 
#'                   lightcolor=c("red","green","blue"), title_text = "Red, Green, Blue",
#'                   title_bar_color="white", title_bar_alpha=0.8)
#'}
#'
#'#Change the camera:
#'\donttest{
#'render_camera(theta=-45,phi=60,fov=60,zoom=0.8)
#'render_highquality(lightdirection = c(0), 
#'                   title_bar_color="white", title_bar_alpha=0.8)
#'}
#'#Add a shiny metal sphere
#'\donttest{
#'render_camera(theta=-45,phi=60,fov=60,zoom=0.8)
#'render_highquality(lightdirection = c(0,120,240), lightaltitude=45, 
#'                   lightcolor=c("red","green","blue"),
#'                   scene_elements = rayrender::sphere(z=-60,y=0,
#'                                                      radius=20,material=rayrender::metal()))
#'}
#'
#'#Add a red light to the volcano and change the ambient light to dusk
#'\donttest{
#'render_camera(theta=45,phi=45)
#'render_highquality(lightdirection = c(240), lightaltitude=30, 
#'                   lightcolor=c("#5555ff"),
#'                   scene_elements = rayrender::sphere(z=0,y=15, x=-18, radius=5,
#'                                    material=rayrender::light(color="red",intensity=10)))
#'}
#'#Manually change the camera location and direction
#'\donttest{
#'render_camera(theta=45,phi=45,fov=90)
#'render_highquality(lightdirection = c(240), lightaltitude=30, lightcolor=c("#5555ff"), 
#'                   camera_location = c(50,10,10), camera_lookat = c(0,15,0), 
#'                   scene_elements = rayrender::sphere(z=0,y=15, x=-18, radius=5,
#'                                    material=rayrender::light(color="red",intensity=10)))
#'rgl::rgl.close()
#'}
render_highquality = function(filename = NULL, light = TRUE, lightdirection = 315, lightaltitude = 45, lightsize=NULL,
                              lightintensity = 500, lightcolor = "white", obj_material = rayrender::diffuse(),
                              cache_filename=NULL, width = NULL, height = NULL, 
                              text_angle = NULL, text_size = 6, text_offset = c(0,0,0), line_radius=0.5,
                              scale_text_angle = NULL, scale_text_size = 6, scale_text_offset = c(0,0,0), 
                              title_text = NULL, title_offset = c(20,20), 
                              title_color = "black", title_size = 30, title_font = "sans",
                              title_bar_color = NULL, title_bar_alpha = 0.5,
                              ground_material = rayrender::diffuse(), ground_size=100000,scene_elements=NULL, 
                              camera_location = NULL, camera_lookat = c(0,0,0), 
                              camera_interpolate=1, clear  = FALSE, 
                              print_scene_info = FALSE, ...) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(!("rayrender" %in% rownames(utils::installed.packages()))) {
    stop("`rayrender` package required for render_highquality()")
  }
  windowrect = rgl::par3d()$windowRect
  if(!is.null(title_text)) {
    has_title = TRUE
  } else {
    has_title = FALSE
  }
  if(is.null(width)) {
    width = windowrect[3]-windowrect[1]
  }
  if(is.null(height)) {
    height = windowrect[4]-windowrect[2]
  }
  no_cache = FALSE
  if(is.null(cache_filename)) {
    no_cache = TRUE
    if(.Platform$OS.type == "windows") {
      sepval = "\\"
    } else {
      sepval = "/"
    }
    cache_filename = paste0(tempdir(), sepval, "temprayfile.obj")
  }
  shadowid = get_ids_with_labels(typeval = "shadow")
  if(nrow(shadowid) > 0) {
    shadowvertices = rgl.attrib(shadowid$id[1], "vertices")
    shadowdepth = shadowvertices[1,2]
    has_shadow = TRUE
  } else {
    has_shadow = FALSE
  }
  camera_interpolate[camera_interpolate > 1] = 1
  camera_interpolate[camera_interpolate < 0] = 0
  fov = rgl::par3d()$FOV
  rotmat = rot_to_euler(rgl::par3d()$userMatrix)
  projmat = rgl::par3d()$projMatrix
  zoom = rgl::par3d()$zoom
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
  movevec = movevec[1:3]
  observer_radius = rgl::par3d()$observer[3]
  lookvals = rgl::par3d()$bbox
  if(fov == 0) {
    ortho_dimensions = c(2/projmat[1,1],2/projmat[2,2])
  } else {
    fov = 2 * atan(1/projmat[2,2]) * 180/pi
    ortho_dimensions = c(1,1)
  }
  bbox_center = c(mean(lookvals[1:2]),mean(lookvals[3:4]),mean(lookvals[5:6])) - movevec
  observery = sinpi(phi/180) * observer_radius
  observerx = cospi(phi/180) * sinpi(theta/180) * observer_radius 
  observerz = cospi(phi/180) * cospi(theta/180) * observer_radius 
  if(is.null(camera_location)) {
    lookfrom = c(observerx, observery, observerz) 
  } else {
    lookfrom = camera_location
  }
  if(length(camera_interpolate) == 1) {
    camera_interpolate = c(camera_interpolate,camera_interpolate)
  }
  if(all(camera_interpolate != 1)) {
    if(!is.null(camera_location)) {
      lookfrom = (1-camera_interpolate[1]) * c(observerx, observery, observerz) + 
        camera_interpolate[1] * camera_location
    }
    if(!is.null(camera_lookat)) {
      camera_lookat = camera_interpolate[2] * camera_lookat
    }
  }
  if(tools::file_ext(cache_filename) != "obj") {
    cache_filename = paste0(cache_filename, ".obj")
  }
  if(no_cache || !file.exists(cache_filename)) {
    if(obj_material$type %in% c("diffuse","oren-nayar")) {
      save_obj(cache_filename)
    } else {
      save_obj(cache_filename, save_texture = FALSE)
    }
  }
  if(obj_material$type %in% c("diffuse","oren-nayar")) {
    scene = rayrender::obj_model(cache_filename, x = -bbox_center[1], y = -bbox_center[2],
                      z = -bbox_center[3], texture = TRUE,
                      material = obj_material)
  } else {
    scene = rayrender::obj_model(cache_filename, x = -bbox_center[1], y = -bbox_center[2], 
                      z = -bbox_center[3],
                      material = obj_material)
  }
  has_rayimage = TRUE
  if(!("rayimage" %in% rownames(utils::installed.packages()))) {
    warning("`rayimage` package required for labels")
    has_rayimage = FALSE
  }
  labelids = get_ids_with_labels(typeval = "raytext")$id
  labels = list()
  counter = 1
  for(i in seq_len(length(labelids))) {
    if(!has_rayimage) {
      break
    }
    temp_label = rgl.attrib(labelids[i], "texts")
    temp_center = rgl.attrib(labelids[i], "centers")
    temp_color = rgl.attrib(labelids[i], "colors")
    for(j in seq_len(nrow(temp_label))) {
      labelfile = ""
      if(!no_cache) {
        labelfile = paste0(temp_label[j,1],".png")
      } else {
        labelfile = tempfile(fileext = ".png")
      }
      rayimage::add_title(matrix(0,ncol = nchar(temp_label[j,1])*60, nrow=60), 
                          title_size  = 60,
                          title_offset = c(0,0),title_text = temp_label, title_color = "white",
                          title_position = "center", filename = labelfile)
      if(is.null(text_angle)) {
        anglevec = c(rotmat[1],theta,0)
      } else {
        if(length(text_angle) == 1) {
          anglevec = c(0,text_angle,0)
        } else {
          anglevec = text_angle
        }
      }
      labels[[counter]] = rayrender::xy_rect(x=temp_center[j,1] - bbox_center[1] + text_offset[1], 
                                  y=temp_center[j,2] - bbox_center[2] + text_offset[2], 
                                  z=temp_center[j,3] - bbox_center[3] + text_offset[3],
                                  angle = anglevec,
                                  xwidth = nchar(temp_label[j,1])*text_size, ywidth = text_size,
                                  material = rayrender::diffuse(color = temp_color[j,1:3], alpha_texture = labelfile))
      counter = counter + 1
    }
  }
  if(length(labels) > 0) {
    all_labels = do.call(rbind, labels)
    scene = rayrender::add_object(scene, all_labels)
  }
  labellineids = get_ids_with_labels(typeval = "textline")$id
  labelline = list()
  counter = 1
  for(i in seq_len(length(labellineids))) {
    if(!has_rayimage) {
      break
    }
    temp_verts = rgl.attrib(labellineids[i], "vertices")
    temp_color = rgl.attrib(labellineids[i], "colors")
    for(j in seq_len(nrow(temp_verts)/2)) {
      labelline[[counter]] = rayrender::segment(start = temp_verts[2*j-1,] - bbox_center, 
                                     end   = temp_verts[2*j,] - bbox_center,
                                     radius = line_radius,
                                     material = rayrender::diffuse(color = temp_color[j,1:3]))
      counter = counter + 1
    }
  }
  scalelabelids = get_ids_with_labels(typeval = "text_scalebar")$id
  scalelabels = list()
  counter = 1
  for(i in seq_len(length(scalelabelids))) {
    if(!has_rayimage) {
      break
    }
    temp_label = rgl.attrib(scalelabelids[i], "texts")
    temp_center = rgl.attrib(scalelabelids[i], "centers")
    temp_color = rgl.attrib(scalelabelids[i], "colors")
    for(j in seq_len(nrow(temp_label))) {
      scalelabelfile = ""
      if(!no_cache) {
        scalelabelfile = paste0(temp_label[j,1],".png")
      } else {
        scalelabelfile = tempfile(fileext = ".png")
      }
      rayimage::add_title(matrix(0,ncol = nchar(temp_label[j,1])*60, nrow=60), 
                          title_size  = 60,
                          title_offset = c(0,0),title_text = temp_label, title_color = "white",
                          title_position = "center", filename = scalelabelfile)
      if(is.null(text_angle)) {
        anglevec = c(rotmat[1],theta,0)
      } else {
        if(length(text_angle) == 1) {
          anglevec = c(0,text_angle,0)
        } else {
          anglevec = text_angle
        }
      }
      scalelabels[[counter]] = rayrender::xy_rect(x=temp_center[j,1] - bbox_center[1] + scale_text_offset[1], 
                                  y=temp_center[j,2] - bbox_center[2] + scale_text_offset[2], 
                                  z=temp_center[j,3] - bbox_center[3] + scale_text_offset[3],
                                  angle = anglevec,
                                  xwidth = nchar(temp_label[j,1])*scale_text_size, ywidth = scale_text_size,
                                  material = rayrender::diffuse(color = temp_color[j,1:3], alpha_texture = scalelabelfile))
      counter = counter + 1
    }
  }
  if(length(labels) > 0) {
    all_labels = do.call(rbind, labels)
    scene = rayrender::add_object(scene, all_labels)
  }
  if(length(labelline) > 0) {
    all_labellines = do.call(rbind, labelline)
    scene = rayrender::add_object(scene, all_labellines)
  }
  if(length(scalelabels) > 0) {
    all_scalelabels = do.call(rbind, scalelabels)
    scene = rayrender::add_object(scene, all_scalelabels)
  }
  if(has_shadow) {
    scene = rayrender::add_object(scene, rayrender::xz_rect(zwidth=ground_size,xwidth=ground_size,
                                                            y=shadowdepth-bbox_center[2], material = ground_material))
  }
  if(light) {
    if(is.null(lightsize)) {
      lightsize = observer_radius/5
    }
    if(length(lightaltitude) >= 1 || length(lightdirection) >= 1) {
      if(length(lightaltitude) > 1 && length(lightdirection) > 1 && length(lightdirection) != length(lightaltitude)) {
        stop("lightaltitude vector ", lightaltitude, " and lightdirection vector ", lightdirection, "both greater than length 1 but not equal length" )
      }
      numberlights = ifelse(length(lightaltitude) > length(lightdirection), length(lightaltitude),length(lightdirection))
      lightaltitudetemp = lightaltitude[1]
      lightdirectiontemp = lightdirection[1]
      lightintensitytemp = lightintensity[1]
      lightcolortemp = lightcolor[1]
      lightsizetemp = lightsize[1]
      for(i in seq_len(numberlights)) {
        if(!is.na(lightaltitude[i])) {
          lightaltitudetemp = lightaltitude[i]
        }
        if(!is.na(lightdirection[i])) {
          lightdirectiontemp = lightdirection[i]
        }
        if(!is.na(lightintensity[i])) {
          lightintensitytemp = lightintensity[i]
        }
        if(!is.na(lightcolor[i])) {
          lightcolortemp = lightcolor[i]
        }
        if(!is.na(lightsize[i])) {
          lightsizetemp = lightsize[i]
        }
        scene = rayrender::add_object(scene, rayrender::sphere(x=observer_radius*5 * cospi(lightaltitudetemp/180) * sinpi(lightdirectiontemp/180),
                                         y=observer_radius*5 * sinpi(lightaltitudetemp/180),
                                         z=-observer_radius*5 * cospi(lightaltitudetemp/180) * cospi(lightdirectiontemp/180), radius=lightsizetemp,
                                         material = rayrender::light(color = lightcolortemp, intensity = lightintensitytemp)))
      }
    }
  }
  if(print_scene_info) {
    print(paste0(c("Camera position: c(", paste0(lookfrom,collapse=","), 
                   "), Camera lookat: c(", paste0(camera_lookat,collapse=","), ")"),collapse=""))
  }
  if(!is.null(scene_elements)) {
    scene = rayrender::add_object(scene,scene_elements)
  }
  if(has_title) {
    temp = tempfile(fileext = ".png")
    rayrender::render_scene(scene, lookfrom = lookfrom, lookat = camera_lookat, fov = fov, filename=temp,
                 ortho_dimensions = ortho_dimensions, width = width, height = height, ...)
    if(has_title) {
      if(!("magick" %in% rownames(utils::installed.packages()))) {
        stop("`magick` package required for adding title")
      }
      if(!is.null(title_bar_color)) {
        title_bar_color = col2rgb(title_bar_color)/255
        title_bar = array(0,c(width,height,4))
        title_bar_width = 2 * title_offset[1] + title_size
        title_bar[1:title_bar_width,,1] = title_bar_color[1]
        title_bar[1:title_bar_width,,2] = title_bar_color[2]
        title_bar[1:title_bar_width,,3] = title_bar_color[3]
        title_bar[1:title_bar_width,,4] = title_bar_alpha
        title_bar_temp = paste0(tempfile(),".png")
        png::writePNG(title_bar,title_bar_temp)
        magick::image_read(temp) %>%
          magick::image_composite(magick::image_read(title_bar_temp),
          ) %>%
          magick::image_write(path = temp, format = "png")
      }
      magick::image_read(temp) %>%
        magick::image_annotate(title_text, 
                               location = paste0("+", title_offset[1],"+",title_offset[2]),
                               size = title_size, color = title_color, 
                               font = title_font) %>%
        magick::image_write(path = temp, format = "png")
      tempfileload = png::readPNG(temp)
      if(is.null(filename)) {
        plot_map(tempfileload)
      } else {
        save_png(tempfileload,filename)
      }
    }
  } else {
    rayrender::render_scene(scene, lookfrom = lookfrom, lookat = camera_lookat, fov = fov, filename=filename,
                 ortho_dimensions = ortho_dimensions, width = width, height = height, ...)
  }
  if(clear) {
    rgl::rgl.clear()
  }
}
