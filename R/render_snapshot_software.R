#'@title Render High Quality
#'
#'@param cache_filename Name of temporary filename to store OBJ file, if the user does not want to rewrite the file each time.
#'@param ground_size Default `10000`. The width of the plane representing the ground.
#'@param camera_location Default `NULL`. Custom position of the camera. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window.
#'@param ... Additional parameters to pass to `rayvertex::rasterize_scene()`
#'@keywords internal
render_snapshot_software = function(filename, cache_filename = NULL, camera_location = NULL, 
                                    camera_lookat = c(0,0,0),background=NULL, return_all = FALSE,
                                    width = NULL, height = NULL, light_direction = NULL, fake_shadow = TRUE, 
                                    text_angle = NULL, text_size = 1, text_offset = c(0,0,0), fov=NULL, 
                                    print_scene_info = FALSE, point_radius = 1, line_offset=-1e-7, 
                                    fsaa = 1, thick_lines = FALSE, line_radius = 0.5, 
                                    rayvertex_lighting = FALSE,
                                    rayvertex_lights = NULL, rayvertex_shadow_map = FALSE, ...) {
  if(run_documentation()) {
    fsaa = 2
  }
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  text_offset = text_offset + c(0,text_size,0)
  has_rayimage = TRUE
  if(!(length(find.package("rayimage", quiet = TRUE)) > 0)) {
    warning("`rayimage` package required for labels")
    has_rayimage = FALSE
  }
  windowrect = rgl::par3d()$windowRect
  
  if(is.null(width)) {
    width = windowrect[3]-windowrect[1]
  }
  if(is.null(height)) {
    height = windowrect[4]-windowrect[2]
  }
  if(.Platform$OS.type == "windows") {
    sepval = "\\"
  } else {
    sepval = "/"
  }
  no_cache = FALSE
  if(is.null(cache_filename)) {
    no_cache = TRUE
    cache_filename = paste0(tempdir(), sepval, "temprayfile.obj")
  } 
  surfaceid = get_ids_with_labels(typeval = c("surface", "surface_tris"))
  surfacevertices = rgl.attrib(surfaceid$id[1], "vertices")
  polygonid = get_ids_with_labels(typeval = c("polygon3d"))
  baseid = get_ids_with_labels(typeval = c("base"))
  if(nrow(polygonid) > 0) {
    polyrange = c()
    polyrange_x = c()
    polyrange_z = c()
    for(i in seq_len(nrow(polygonid))) {
      tempverts = range(rgl.attrib(polygonid$id[i], "vertices")[,2])
      tempverts_x = range(rgl.attrib(polygonid$id[i], "vertices")[,1])
      tempverts_z = range(rgl.attrib(polygonid$id[i], "vertices")[,3])
      
      if(all(!is.na(tempverts))) {
        polyrange = range(c(tempverts,polyrange))
      }
      if(all(!is.na(tempverts_x))) {
        polyrange_x = range(c(tempverts_x,polyrange_x))
      }
      if(all(!is.na(tempverts_z))) {
        polyrange_z = range(c(tempverts_z,polyrange_z))
      }
    }
  }
  if(nrow(baseid) > 0) {
    baserange = c()
    for(i in seq_len(nrow(baseid))) {
      tempverts = range(rgl.attrib(baseid$id[i], "vertices")[,2])
      if(all(!is.na(tempverts))) {
        baserange = range(c(tempverts,baserange))
      }
    }
  }
  surfacerange = range(surfacevertices[,2],na.rm=TRUE)
  if(nrow(polygonid) > 0) {
    surfacerange[2] = range(c(surfacerange,polyrange))[2]
  }
  if(nrow(baseid) > 0) {
    surfacerange[2] = range(c(surfacerange,baserange))[2]
  }
  shadowid = get_ids_with_labels(typeval = "shadow")
  if(nrow(shadowid) > 0) {
    shadowvertices = rgl.attrib(shadowid$id[1], "vertices")
    shadowdepth = shadowvertices[1,2]
    has_shadow = TRUE
  } else {
    has_shadow = FALSE
  }
  
  fov = rgl::par3d()$FOV
  rotmat = rot_to_euler(rgl::par3d()$userMatrix)
  if(abs(rotmat[1]) == 90) {
    camera_up = rgl::rotationMatrix(rotmat[2]*pi/180, 0, 1, 0) %*% 
      matrix(c(1,0,0,1),nrow=4,ncol=1)
    camera_up = camera_up[1:3]
  } else {
    camera_up = c(0,1,0)
  }
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
      rgl::rotationMatrix(rotmat[2]*pi/180, 0, 1, 0) %*%
      rgl::rotationMatrix(-rotmat[1]*pi/180, 1, 0, 0) %*% 
      rgl::par3d()$userMatrix[,4]
  }
  movevec = movevec[1:3]
  observer_radius = rgl::par3d()$observer[3]
  lookvals = rgl::par3d()$bbox
  # lookvals[4] = surfacerange[2]
  # lookvals = rgl::par3d()$bbox
  
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
  
  if(tools::file_ext(cache_filename) != "obj") {
    cache_filename = paste0(cache_filename, ".obj")
  }
  if(no_cache || !file.exists(cache_filename)) {
    save_obj(cache_filename, save_shadow = TRUE)
  }
  scene = rayvertex::obj_mesh(cache_filename, 
                              position = c(-bbox_center[1],-bbox_center[2],-bbox_center[3]))
  
  ##########
  labelids = get_ids_with_labels(typeval = "raytext")$id
  labels = list()
  for(i in seq_len(length(labelids))) {
    if(!has_rayimage) {
      break
    }
    temp_label = rgl.attrib(labelids[i], "texts")
    temp_center = rgl.attrib(labelids[i], "centers")
    temp_color = rgl.attrib(labelids[i], "colors")
    for(j in seq_len(nrow(temp_label))) {
      if(is.null(text_angle)) {
        anglevec = c(rotmat[1],-theta,0)
      } else {
        if(length(text_angle) == 1) {
          anglevec = c(0,text_angle,0)
        } else {
          anglevec = text_angle
        }
      }
      labels = rayvertex::add_shape(labels, rayvertex::text3d_mesh(label=temp_label[j,1],
                                            position=c(temp_center[j,1] - bbox_center[1] + text_offset[1], 
                                            temp_center[j,2] - bbox_center[2] + text_offset[2], 
                                            temp_center[j,3] - bbox_center[3] + text_offset[3]),
                                            angle = anglevec,
                                            text_height = text_size,
                                            color = temp_color[j,1:3]))
    }
  }
  labellineids = get_ids_with_labels(typeval = c("textline","lines","waterlines"))$id
  labelline = matrix(nrow=0,ncol=9)
  line_scene = list()
  line_counter = 1
  for(i in seq_len(length(labellineids))) {
    temp_verts = rgl.attrib(labellineids[i], "vertices")
    temp_color = rgl.attrib(labellineids[i], "colors")
    if(nrow(temp_color) == 1) {
      temp_color = matrix(temp_color[1:3], byrow = TRUE, ncol = 3, nrow = nrow(temp_verts))
    }
    if(thick_lines) {
      for(j in seq_len(nrow(temp_verts)/2)) {
        line_mat = rayvertex::material_list(diffuse = temp_color[j,1:3], type = "color")
        line_scene[[line_counter]] = rayvertex::segment_mesh(start = temp_verts[2*j-1,] - bbox_center, 
                                                             end   = temp_verts[2*j,] - bbox_center,
                                                             radius = line_radius,
                                                             material = line_mat)
        line_counter = line_counter + 1
      }
    } else {
      for(j in seq_len(nrow(temp_verts)/2)) {
        labelline = rayvertex::add_lines(labelline, rayvertex::generate_line(start = temp_verts[2*j-1,] - bbox_center, 
                                                  end   = temp_verts[2*j,] - bbox_center,
                                                  color = temp_color[j,1:3]))
      }
    }
  }
  
  pathids = get_ids_with_labels(typeval = c("path3d","contour3d"))$id
  pathline = matrix(nrow=0,ncol=9)
  
  for(i in seq_len(length(pathids))) {
    temp_verts = rgl.attrib(pathids[i], "vertices")
    temp_color = rgl.attrib(pathids[i], "colors")
    if(nrow(temp_color) == 1) {
      temp_color = matrix(temp_color[1:3], byrow = TRUE, ncol = 3, nrow = nrow(temp_verts))
    }

    
    if(thick_lines) {
      for(j in seq_len(nrow(temp_verts)-1)) {
        line_mat = rayvertex::material_list(diffuse = temp_color[j,1:3], type = "color")
        line_scene[[line_counter]] = rayvertex::segment_mesh(start = temp_verts[j,] - bbox_center, 
                                                             end   = temp_verts[j+1,] - bbox_center,
                                                             radius = line_radius,
                                                             material = line_mat)
        line_counter = line_counter + 1
      }
    } else {
      for(j in seq_len(nrow(temp_verts)-1)) {
        pathline = rayvertex::add_lines(pathline, 
                                        rayvertex::generate_line(start = temp_verts[j,] - bbox_center, 
                                                 end   = temp_verts[j+1,] - bbox_center,
                                                 color = temp_color[j,1:3]))
      }
    }
  }
  pointids = get_ids_with_labels(typeval = "points3d")$id
  pointlist = list()
  for(i in seq_len(length(pointids))) {
    temp_verts = rgl.attrib(pointids[i], "vertices")
    temp_color = rgl.attrib(pointids[i], "colors")
    if(nrow(temp_color) == 1) {
      temp_color = matrix(temp_color[1:3], byrow = TRUE, ncol = 3, nrow = nrow(temp_verts))
    }
    for(j in seq_len(nrow(temp_verts))) {
      pointlist = rayvertex::add_shape(pointlist, rayvertex::sphere_mesh(position = c(temp_verts[j,1] - bbox_center[1],
                                               temp_verts[j,2] - bbox_center[2],
                                               temp_verts[j,3] - bbox_center[3]),
                                               radius = point_radius,
                                               material = rayvertex::material_list(diffuse = temp_color[j,1:3])))
    }
  }
  scalelabelids = get_ids_with_labels(typeval = "text_scalebar")$id
  scalelabels = list()
  if(is.null(text_angle)) {
    anglevec = c(rotmat[1],-theta,0)
  } else {
    if(length(text_angle) == 1) {
      anglevec = c(0,text_angle,0)
    } else {
      anglevec = text_angle
    }
  }
  for(i in seq_len(length(scalelabelids))) {
    if(!has_rayimage) {
      break
    }
    temp_label = rgl.attrib(scalelabelids[i], "texts")
    temp_center = rgl.attrib(scalelabelids[i], "centers")
    temp_color = rgl.attrib(scalelabelids[i], "colors")
    for(j in seq_len(nrow(temp_label))) {
      scalelabels = rayvertex::add_shape(scalelabels, rayvertex::text3d_mesh(as.character(temp_label),
                                          position=c(temp_center[j,1] - bbox_center[1] + text_offset[1],
                                          temp_center[j,2] - bbox_center[2] + text_offset[2],
                                          temp_center[j,3] - bbox_center[3] + text_offset[3]),
                                          angle = anglevec, 
                                          text_height = text_size,
                                          color = as.vector(temp_color[j,1:3])))
    }
  }
  if(length(labels) > 0) {
    scene = rayvertex::add_shape(scene, labels)
  }
  if(length(scalelabels) > 0) {
    scene = rayvertex::add_shape(scene, scalelabels)
  }
  if(length(pointlist) > 0) {
    scene = rayvertex::add_shape(scene, pointlist)
  }
  ranges = apply(do.call(rbind,scene$vertices),2,range)
  if(has_shadow && !fake_shadow) {
    scene = rayvertex::add_shape(scene, rayvertex::xz_rect_mesh(position=c(0,shadowdepth-bbox_center[2],0),
                                                                scale=c(ranges[2,1]-ranges[1,1],1,ranges[2,3]-ranges[1,3])*2,
                                 material = rayvertex::material_list(diffuse="white")))
  }
  if(print_scene_info) {
    print(sprintf("Camera position: c(%0.2f, %0.2f, %0.2f), Camera Lookat: c(%0.2f, %0.2f, %0.2f)",
                  lookfrom[1],lookfrom[2],lookfrom[3], camera_lookat[1], camera_lookat[2], camera_lookat[3]))
  }
  if(is.null(background)) {
    if(!is.null(scene$materials$ray_shadow)) {
      if(file.exists(scene$materials$ray_shadow$diffuse_texname)) {
        shdw = png::readPNG(scene$materials$ray_shadow$diffuse_texname)
        background = c(shdw[1,1,1],shdw[1,1,2],shdw[1,1,3])
      } else if(file.exists(sprintf("%s%s%s",tempdir(), sepval, scene$materials$ray_shadow$diffuse_texname))) {
        shdw = png::readPNG(sprintf("%s%s%s",tempdir(), sepval, scene$materials$ray_shadow$diffuse_texname))
        background = c(shdw[1,1,1],shdw[1,1,2],shdw[1,1,3])
      } else {
        background = "white"
      }
    } else {
      background = "white"
    }
  } 
  if(!is.null(scene$materials$ray_polygon3d)) {
    scene$materials$ray_polygon3d$type = "diffuse"
    if(any(scene$materials$ray_polygon3d$ambient != 0)) {
      scene$materials$ray_polygon3d$diffuse_intensity = 0
    }
  }
  if(!rayvertex_lighting) {
    #Get light info
    lightinfo = rgl::ids3d(type = "lights")
    light_list = list()
    for(i in seq_len(nrow(lightinfo))) {
      idval = lightinfo$id[i]
      direction = rgl::rgl.attrib(idval, "vertices")
      flags = rgl::rgl.attrib(idval, "flags")
      is_viewpoint = flags[1]
      if(is_viewpoint) {
        direction = local_to_world(direction,build_from_w(lookfrom))
      }
      is_finite = flags[2]
      if(!is_finite) {
        light_list[[i]] = rayvertex::directional_light(direction = direction)
      } else {
        light_list[[i]] = rayvertex::point_light(position = direction)
      }
    }
    lights = do.call(rbind,light_list)
  } else {
    scene = rayvertex::change_material(scene, type = "diffuse")
    lights = rayvertex_lights
  }
  if(thick_lines) {
    line_scene_processed = rayvertex::scene_from_list(line_scene)
    scene = rayvertex::add_shape(scene, line_scene_processed)
  }
  for(i in seq_len(length(scene$materials[[1]]))) {
    if(scene$materials[[1]][[i]]$illum == 5) {
      scene$materials[[1]][[i]]$illum = 0
      scene$materials[[1]][[i]]$type = "color"
    } else {
      scene$materials[[1]][[i]]$two_sided = TRUE
    }
  }
  debug = rayvertex::rasterize_scene(scene, lookat = camera_lookat, camera_up = camera_up,
                                     filename = filename, fsaa = fsaa,
                                     lookfrom = lookfrom, width = width, height = height, 
                                     ortho_dimensions = ortho_dimensions,
                                     fov=fov, background = background, light_info = lights,
                                     line_info = rayvertex::add_lines(labelline, pathline), 
                                     line_offset=line_offset, shadow_map = rayvertex_shadow_map,
                                     ...)
  return(invisible(debug))
}
