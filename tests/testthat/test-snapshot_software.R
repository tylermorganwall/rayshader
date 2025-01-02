save_3d_sw_test_png = function(code, path) {
  code
  path
}

compare_image = function(path1, path2) {
  image1 = png::readPNG(path1)
  image2 = png::readPNG(path2)
  return(identical(image1, image2))
}

run_tests_sw = function(func, argument_grid, plot_prefix="", ...) {
  stopifnot(inherits(argument_grid,"data.frame"))
  for(i in seq_len(nrow(argument_grid))){
    args = unlist(argument_grid[i,], recursive = FALSE)
    # test_filename = paste0(sprintf("%s-%s", names(args), args),collapse="_")
    test_filename = sprintf("%s_test%i.png",
                            plot_prefix , i)
    path = tempfile(fileext = ".png")
    
    args = append(args, ...)
    args = append(args, list(filename = path))
    
    save_3d_sw_test_png(do.call(func, args = args), path) |> 
      suppressMessages() |> 
      suppressWarnings() |> 
      expect_snapshot_file(name = test_filename, compare = compare_image)
  }
}

test_that("Checking render_snapshot(software_render = TRUE) features", {
  testthat::skip_on_cran()
  #Render the 3D map
  montereybay %>%
    sphere_shade() %>%
    plot_3d(montereybay,zscale=50,water=TRUE,
            shadowcolor="#40310a", watercolor="#233aa1", background = "tan",
            waterlinecolor = "white",
            theta=210,  phi=22, zoom=0.20, fov=55, windowsize = 800)
  set.seed(1)
  moss_landing_coord = c(36.806807, -121.793332)
  x_vel_out = -0.001 + rnorm(1000)[1:300]/1000
  y_vel_out = rnorm(1000)[1:300]/200
  z_out = c(seq(0,2000,length.out = 180), seq(2000,0,length.out=10),
            seq(0,2000,length.out = 100), seq(2000,0,length.out=10))
  bird_track_lat = list()
  bird_track_long = list()
  bird_track_lat[[1]] = moss_landing_coord[1]
  bird_track_long[[1]] = moss_landing_coord[2]
  for(i in 2:300) {
    bird_track_lat[[i]] = bird_track_lat[[i-1]] + y_vel_out[i]
    bird_track_long[[i]] = bird_track_long[[i-1]] + x_vel_out[i]
  }
  
  
  t = seq(0,2*pi,length.out=20)
  circle_coords_lat = moss_landing_coord[1] + 0.3 * sin(t)
  circle_coords_long = moss_landing_coord[2] + 0.3 * cos(t)
  
  t2 = seq(0,2*pi,length.out=40)
  circle_coords_lat2 = moss_landing_coord[1] + 0.1 * sin(t2)
  circle_coords_long2 = moss_landing_coord[2] + 0.1 * cos(t2)
  
  render_path(extent = attr(montereybay,"extent"),
              lat = unlist(bird_track_lat), long = unlist(bird_track_long),
              altitude = z_out, zscale=50,color="orange", antialias=TRUE)
  render_points(extent = attr(montereybay,"extent"),
                lat = unlist(bird_track_lat)-0.1, long = unlist(bird_track_long),
                altitude = z_out, zscale=50,color="purple", clear_previous = T)
  render_tree(extent = attr(montereybay,"extent"), heightmap = montereybay,
              tree_zscale = FALSE, tree_height = 30, crown_width_ratio = 1,
              lat = unlist(circle_coords_lat), long = unlist(circle_coords_long), zscale=50)
  render_obj(flag_full_obj(), extent = attr(montereybay,"extent"), heightmap = montereybay,
             lat = unlist(circle_coords_lat2), long = unlist(circle_coords_long2),
             scale=c(2,2,2), angle=c(0,45,0),
             zscale=50, color=rainbow(40), smooth = FALSE, clear_previous = TRUE)
  santa_cruz = c(36.962957, -122.021033) 
  
  #Can't do text: fonts different on other systems
  # render_label(montereybay,lat = santa_cruz[1]+0.1, long = santa_cruz[2],
  #              extent = attr(montereybay, "extent"), 
  #              altitude=2000, zscale=50, text = "Santa Cruz 2", clear_previous = T)
  # render_label(montereybay,lat = santa_cruz[1], long = santa_cruz[2],
  #              extent = attr(montereybay, "extent"),
  #              altitude=2000, zscale=50, text = "Santa Cruz")
  #                
  # render_scalebar(limits=c(0, 80), label_unit = "", position="S")

  # render_polygons(monterey_counties_sf[7,], 
  #                 extent = attr(montereybay, "extent"), data_column_top = "ALAND",
  #                 scale_data = 100/(2.6E9), color = "chartreuse4",
  #                 parallel = TRUE, clear_previous = TRUE)
  road_overlay = generate_line_overlay(monterey_roads_sf, attr(montereybay,"extent"), 
                                       heightmap = montereybay)
  render_camera(zoom=0.35,phi=34,theta=220,fov=85)
  

  software_render_args = expand.grid(fsaa   = list(1,2),
                                     rayvertex_lighting = list(TRUE, FALSE),
                                     rayvertex_lights = list(rayvertex::directional_light(c(1,1,1))),
                                     rayvertex_shadow_map = list(TRUE, FALSE),
                                     camera_location = list(NULL, c(374.72, 393.21, -446.58)),
                                     camera_lookat = list(c(0,0,0), c(100, 100, 200)))
  
  run_tests_sw("render_snapshot", software_render_args, plot_prefix = "sw_render",
               list(cache_scene = TRUE, software_render = TRUE))
  
  software_render_args_dims = expand.grid(software_render = list(TRUE),
                                          width = list(NULL, 600),
                                          height = list(NULL, 600))
  run_tests_sw("render_snapshot", software_render_args_dims, plot_prefix = "sw_render_dims",
               list(cache_scene = TRUE))
  
  software_render_args_lines = expand.grid(software_render = list(TRUE),
                                     thick_lines = list(TRUE, FALSE),
                                     line_offset = list(1e-07,1e-06),
                                     line_radius = list(0.5,10))
  
  run_tests_sw("render_snapshot", software_render_args_lines, plot_prefix = "sw_render_lines",
               list(cache_scene = TRUE))
  
  # software_render_text_args = expand.grid(software_render = list(TRUE),
  #                                    text_size = list(10,30),
  #                                    text_angle = list(NULL,180),
  #                                    text_offset = list(c(1,0,0),c(0,1,0),c(0,0,1)))
  # 
  # run_tests_sw("render_snapshot", software_render_text_args, plot_prefix = "sw_render_text",
  #              list(cache_scene = temp_cache))
  
  software_render_points_args = expand.grid(software_render = list(TRUE),
                                            point_radius = list(2,10))
  
  run_tests_sw("render_snapshot", software_render_points_args, plot_prefix = "sw_render_points",
               list(cache_scene = TRUE))
  rgl::close3d()
})
