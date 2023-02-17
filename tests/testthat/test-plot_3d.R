save_3d_test_png = function(code) {
  path = tempfile(fileext = ".png")
  code
  render_snapshot(path)
  path
}

compare_image = function(path1, path2) {
  image1 = png::readPNG(path1)
  image2 = png::readPNG(path2)
  return(identical(image1, image2))
}

run_tests = function(func, argument_grid, plot_prefix="", ...) {
  stopifnot(inherits(argument_grid,"data.frame"))

  for(i in seq_len(nrow(argument_grid))){
    args = unlist(argument_grid[i,], recursive = FALSE)
    # test_filename = paste0(sprintf("%s-%s", substr(names(args),start_name,start_name + name_len), args),collapse="_")
    test_filename = sprintf("%s_test%i.png",
                            plot_prefix , i)
    args = append(args, ...)
    save_3d_test_png(do.call(func, args = args)) |> 
      suppressMessages() |> 
      suppressWarnings() |> 
    expect_snapshot_file(name = test_filename, compare = compare_image)
  }
  rgl::close3d()
}

test_that("plot_3d plots basic options", {
  skip_if(rgl::rgl.useNULL(), message = "rgl.useNULL is TRUE--not testing raw rgl snapshots")
  
  hillshade = sphere_shade(volcano)
  volcano_na = volcano
  volcano_na[60:80, 30:40] = NA
                             
  plot_3d_args_meshing = expand.grid(zscale     = list(1,3),
                                     baseshape  = list("rectangle", "circle", "hex"),
                                     solid      = list(TRUE, FALSE),
                                     soliddepth = list("auto", -100, 200),
                                     shadow      = list(TRUE, FALSE))
  
  plot_3d_args_meshing_subset = expand.grid(water      = list(TRUE, FALSE),
                                     waterdepth  = list(150,300),
                                     solid       = list(TRUE, FALSE),
                                     shadow      = list(TRUE, FALSE),
                                     shadowdepth = list(-100, 0, 200),
                                     soliddepth  = list("auto", -100, 200))
  

  run_tests("plot_3d", plot_3d_args_meshing, plot_prefix = "basic", 
            list(hillshade=hillshade, 
                 heightmap = volcano, 
                 windowsize=200,
                 close_previous = FALSE, 
                 clear_previous = TRUE, 
                 plot_new = FALSE))
  run_tests("plot_3d", plot_3d_args_meshing_subset, plot_prefix = "na", 
            list(hillshade=hillshade, 
                 heightmap = volcano_na, 
                 windowsize=200,
                 close_previous = FALSE, 
                 clear_previous = TRUE, 
                 plot_new = FALSE))
})

test_that("plot_3d plots color options", {
  skip_if(rgl::rgl.useNULL(), message = "rgl.useNULL is TRUE--not testing raw rgl snapshots")
  
  hillshade = sphere_shade(volcano)
  plot_3d_args_colors = expand.grid(
    solidlinecolor = list(NULL,"#d8b8c8"),
    solidcolor = list("grey20", "#28b8d8"),
    shadowcolor = list("auto", "red"),
    shadow_darkness = list(0.5,0.2),
    background = list("white","purple"))
  
  plot_3d_args_water_colors = expand.grid(
    water = list(TRUE),
    waterdepth = list(150),
    watercolor = list("lightblue","green"),
    wateralpha = list(0.5,1))

  run_tests("plot_3d", plot_3d_args_colors, plot_prefix = "color",
            list(hillshade=hillshade, 
                 heightmap = volcano,
                 windowsize=200,
                 close_previous = FALSE, 
                 clear_previous = TRUE, 
                 plot_new = FALSE))
  run_tests("plot_3d", plot_3d_args_water_colors, plot_prefix = "wcolor",
            list(hillshade=hillshade, 
                 heightmap = volcano,
                 windowsize=200,
                 close_previous = FALSE, 
                 clear_previous = TRUE, 
                 plot_new = FALSE))
})

test_that("plot_3d triangulation", {
  skip_if(rgl::rgl.useNULL(), message = "rgl.useNULL is TRUE--not testing raw rgl snapshots")
  
  hillshade = sphere_shade(volcano)
  plot_3d_args_triangulation = expand.grid(triangulate = list(TRUE),
                                           max_error = list(0,0.001,1,10),
                                           max_tri = list(0,100,1000),
                                           asp  = list(1,2,0.5))
  run_tests("plot_3d", plot_3d_args_triangulation, plot_prefix = "triangl",
            list(hillshade=hillshade, 
                 heightmap = volcano, 
                 windowsize=200,
                 close_previous = FALSE, 
                 clear_previous = TRUE, 
                 plot_new = FALSE))
})

test_that("plot_3d plots line options", {
  skip_if(rgl::rgl.useNULL(), message = "rgl.useNULL is TRUE--not testing raw rgl snapshots")
  
  hillshade = sphere_shade(volcano)
  plot_3d_args_lines = expand.grid(water = list(TRUE, FALSE),
                                   waterdepth = list(0,150),
                                   waterlinecolor = list(NULL,"red"),
                                   waterlinealpha = list(0.5,1),
                                   linewidth = list(2,5),
                                   lineantialias = list(FALSE,TRUE))

  run_tests("plot_3d", plot_3d_args_lines, plot_prefix = "line",
            list(hillshade=hillshade, 
                 heightmap = volcano, 
                 windowsize=200,
                 close_previous = FALSE, 
                 clear_previous = TRUE, 
                 plot_new = FALSE))

})

test_that("plot_3d plots soil options", {
  skip_if(rgl::rgl.useNULL(), message = "rgl.useNULL is TRUE--not testing raw rgl snapshots")
  
  hillshade = sphere_shade(volcano)
  plot_3d_args_soil = expand.grid(soil = list(TRUE),
                                  soil_freq = list(0.1,0.5),
                                  soil_levels = list(1,32),
                                  soil_color_light = list("#b39474","#ffdfd4"),
                                  soil_color_dark = list("#8a623b","grey20"),
                                  soil_gradient = list(0,2,8),
                                  soil_gradient_darken = list(1,4,8))


  run_tests("plot_3d", plot_3d_args_soil, plot_prefix = "soil",
            list(hillshade=hillshade, 
                 heightmap = volcano, 
                 windowsize=200,
                 close_previous = FALSE, 
                 clear_previous = TRUE, 
                 plot_new = FALSE))
})
