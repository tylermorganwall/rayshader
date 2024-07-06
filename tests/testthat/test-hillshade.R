run_tests_success = function(func, argument_grid, ...) {
  stopifnot(inherits(argument_grid,"data.frame"))
  for(i in seq_len(nrow(argument_grid))){
    args = unlist(argument_grid[i,], recursive = FALSE)
    args = append(args, ...)
    expect_no_condition(do.call(func, args = args))
  }
}

test_that("sphere_shade", {
  sphere_args_palette = expand.grid(texture = list("imhof1", "imhof2", "imhof3", "imhof4",
                                    "desert", "bw", "unicorn"),
                                    sunangle = list(315))
  run_tests_success("sphere_shade", sphere_args_palette, list(heightmap = volcano))
  
  custom_tex = create_texture("red","green","blue","yellow","purple")
  expect_no_condition(sphere_shade(heightmap = volcano, texture = custom_tex))
  
  
  sphere_args_palette_sunangle = expand.grid(texture = list("imhof1"),
                                    sunangle = list(315, -315,0, 720,-800),
                                    zscale = list(1,10))
  run_tests_success("sphere_shade", sphere_args_palette_sunangle, list(heightmap = volcano))
  
  normal_vecs = calculate_normal(volcano)
  
  sphere_args_normals = expand.grid(texture = list("imhof1"),
                                             normalvectors = list(normal_vecs))
  run_tests_success("sphere_shade", sphere_args_normals, list(heightmap = volcano))
})

test_that("height_shade", {
  hs_args = expand.grid(texture = list(grDevices::colorRampPalette(c("#6AA85B", "#D9CC9A", "#FFFFFF"))(256),
                                       heat.colors(256),
                                       terrain.colors(256)),
                        range = list(NULL,range(montereybay),c(0,max(montereybay))))
  run_tests_success("height_shade", hs_args, list(heightmap = montereybay))
})

test_that("lamb_shade", {
  ls_args = expand.grid(sunangle = list(-45,780),
                        sunaltitude = list(0,45,90),
                        zscale = list(1,10),
                        zero_negative = list(TRUE, FALSE))
  run_tests_success("lamb_shade", ls_args, list(heightmap = volcano))
})

test_that("texture_shade", {
  ts_args = expand.grid(detail = list(0,1),
                        contrast = list(0.5,10),
                        brightness = list(-10,10),
                        transform = list(TRUE, FALSE),
                        dx = list(1,10),
                        dy = list(1,10),
                        pad = list(50,200))
  run_tests_success("texture_shade", ts_args, list(heightmap = volcano))
})

test_that("ray_shade", {
  rs_args = expand.grid(sunangle = list(-45,90),
                        sunaltitude = list(10,90),
                        zscale = list(1,3),
                        maxsearch = list(NULL, 10),
                        anglebreaks = list(NULL, seq(10,20,by=1), seq(10,50,by=5)))
  run_tests_success("ray_shade", rs_args, list(heightmap = volcano))
  
  #Test with shadow cache
  shadow_cache = ray_shade(volcano)
  cache_mask = volcano > 150

  run_tests_success("ray_shade", rs_args, list(heightmap = volcano,
                                               cache_mask = cache_mask,
                                               shadow_cache = shadow_cache))
  
})

test_that("ambient_shade", {
  as_args = expand.grid(sunbreaks=list(3,12,24),
                        zscale = list(1,3),
                        maxsearch = list(10, 30),
                        anglebreaks = list(NULL, seq(10,20,by=1)))
  run_tests_success("ambient_shade", as_args, list(heightmap = volcano))
  
  #Test with shadow cache
  shadow_cache = ray_shade(volcano)
  cache_mask = volcano > 150
  
  run_tests_success("ambient_shade", as_args, list(heightmap = volcano,
                                               cache_mask = cache_mask,
                                               shadow_cache = shadow_cache))
  
})

test_that("constant_shade", {
  cs_args = expand.grid(color=list("white","red","black"),
                        alpha = list(0,0.5,1))
  run_tests_success("constant_shade", cs_args, list(heightmap = volcano))
})

test_that("create_texture", {
  expect_no_condition(create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9"))
  expect_no_condition(create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9",
                                              cornercolors = c("red","blue","pink","orange")))
})
