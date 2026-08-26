test_that("render_label() groups aesthetics before placement and scene context", {
  expect_identical(
    names(formals(render_label)),
    c(
      "location",
      "text",
      "data_column_text",
      "font",
      "family",
      "fonttype",
      "textsize",
      "textcolor",
      "textalpha",
      "freetype",
      "adjustvec",
      "line",
      "linecolor",
      "linewidth",
      "alpha",
      "dashed",
      "dashlength",
      "antialias",
      "clear_previous",
      "x",
      "y",
      "z",
      "altitude",
      "data_column_z",
      "scale_data",
      "relativez",
      "offset",
      "lat",
      "long",
      "crs",
      "filter_to_extent",
      "extent",
      "panel",
      "zscale",
      "vertical_exaggeration",
      "heightmap"
    )
  )
})

test_that("render_points() uses cached scene heightmap and zscale", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))
  expect_equal(get_scene_vertical_exaggeration(), 1)

  expect_no_condition(render_points(
    y = 10,
    x = 10,
    extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
    offset = 10
  ))

  ids = get_ids_with_labels()
  point_id = ids$id[ids$tag == "points3d"][1]
  point_verts = rgl::rgl.attrib(point_id, "vertices")
  expect_equal(unname(point_verts[1, 2]), 1, tolerance = 1e-6)
})

test_that("render_points() combines cached zscale and vertical_exaggeration", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    vertical_exaggeration = 2,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))
  expect_equal(get_scene_zscale(), 10)
  expect_equal(get_scene_vertical_exaggeration(), 2)
  expect_equal(get_scene_effective_zscale(), 5)

  expect_no_condition(render_points(
    y = 10,
    x = 10,
    extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
    offset = 10
  ))
  ids = get_ids_with_labels()
  point_id = ids$id[ids$tag == "points3d"][1]
  point_verts = rgl::rgl.attrib(point_id, "vertices")
  expect_equal(unname(point_verts[1, 2]), 2, tolerance = 1e-6)

  expect_no_condition(render_points(
    y = 10,
    x = 10,
    extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
    offset = 10,
    vertical_exaggeration = 1,
    clear_previous = TRUE
  ))
  ids = get_ids_with_labels()
  point_id = ids$id[ids$tag == "points3d"][1]
  point_verts = rgl::rgl.attrib(point_id, "vertices")
  expect_equal(unname(point_verts[1, 2]), 1, tolerance = 1e-6)
})

test_that("scene cache is rejected after switching to a different open scene", {
  local_rgl_use_null()
  withr::defer({
    while (rgl::cur3d() != 0) {
      rgl::close3d()
    }
  })

  heightmap1 = matrix(0, nrow = 20, ncol = 20)
  expect_no_condition(plot_3d_test(
    sphere_shade(heightmap1),
    heightmap1,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250),
    close_previous = TRUE
  ))
  scene1 = rgl::cur3d()

  heightmap2 = matrix(1, nrow = 12, ncol = 12)
  expect_no_condition(plot_3d_test(
    sphere_shade(heightmap2),
    heightmap2,
    zscale = 5,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250),
    close_previous = FALSE
  ))
  scene2 = rgl::cur3d()

  expect_false(identical(scene1, scene2))
  expect_equal(get_scene_context_token(default = NULL), unname(scene2))

  rgl::set3d(scene1)
  expect_null(get_scene_heightmap(default = NULL))
  expect_null(get_scene_zscale(default = NULL))
  expect_null(get_scene_vertical_exaggeration(default = NULL))
  expect_error(
    render_water(waterdepth = 1),
    "No heightmap found"
  )

  rgl::set3d(scene2)
  expect_equal(get_scene_heightmap(default = NULL), heightmap2)
  expect_equal(get_scene_zscale(default = NULL), 5)
  expect_equal(get_scene_vertical_exaggeration(default = NULL), 1)
  expect_no_condition(render_water(waterdepth = 2, watercolor = "lightblue"))
})

test_that("render_water() uses cached scene heightmap and zscale", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_water(
    waterdepth = 100,
    watercolor = "lightblue"
  ))

  ids = get_ids_with_labels()
  water_id = ids$id[ids$tag == "water"][1]
  water_verts = rgl::rgl.attrib(water_id, "vertices")
  expect_equal(max(water_verts[, 2], na.rm = TRUE), 10, tolerance = 1e-6)
})

test_that("render_contours() uses cached scene heightmap", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  skip_if_not_installed("sf")
  skip_if_not_installed("isoband")

  heightmap = outer(1:30, 1:30, `+`)
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_contours(nlevels = 5))
  ids = get_ids_with_labels(typeval = "contour3d")
  expect_gt(nrow(ids), 0)
})

test_that("render_label() uses cached scene heightmap and zscale", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_label(
    text = "A",
    x = 10,
    y = 10,
    z = 10
  ))

  ids = get_ids_with_labels(typeval = c("raytext", "textline"))
  expect_gt(nrow(ids), 0)
})

test_that("render_label() can render text without label lines", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_label(
    text = "A",
    x = 10,
    y = 10,
    z = 10,
    line = FALSE,
    freetype = FALSE,
    clear_previous = TRUE
  ))

  ids = get_ids_with_labels(typeval = c("raytext", "textline"))
  expect_true(any(ids$tag == "raytext"))
  expect_false(any(ids$tag == "textline"))
})

test_that("render_label() accepts x/y names and lat/long aliases", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_label(
    text = "A",
    y = 10,
    x = 10,
    z = 10,
    extent = extent,
    clear_previous = TRUE
  ))
  expect_no_condition(render_label(
    text = "A",
    lat = 10,
    long = 10,
    z = 10,
    extent = extent,
    clear_previous = TRUE
  ))
  expect_error(
    render_label(
      text = "A",
      y = 10,
      x = 10,
      lat = 10,
      z = 10,
      extent = extent
    ),
    "Use only one of `y` or `lat`"
  )
  expect_error(
    render_label(
      text = "A",
      y = 10,
      x = 10,
      long = 10,
      z = 10,
      extent = extent
    ),
    "Use only one of `x` or `long`"
  )
})

test_that("render_label() reads label z values from an sf point column", {
  skip_if_not_installed("sf")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  labels_sf = sf::st_as_sf(
    data.frame(
      text = c("A", "B"),
      x = c(5, 15),
      y = c(5, 15),
      height_m = c("200", "400")
    ),
    coords = c("x", "y"),
    crs = NA,
    remove = FALSE
  )
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250),
    extent = extent
  ))

  expect_no_condition(render_label(
    location = labels_sf,
    text = labels_sf$text,
    data_column_z = "height_m",
    scale_data = 0.5,
    relativez = FALSE,
    offset = 0,
    freetype = FALSE,
    clear_previous = TRUE
  ))

  ids = get_ids_with_labels(typeval = "textline")
  line_ids = ids$id[ids$tag == "textline"]
  line_tops = vapply(
    line_ids,
    function(id) {
      max(rgl::rgl.attrib(id, "vertices")[, 2])
    },
    numeric(1)
  )
  expect_equal(sort(unname(line_tops)), c(10, 20), tolerance = 1e-6)
  expect_error(
    render_label(
      location = labels_sf,
      text = labels_sf$text,
      data_column_z = "height_m",
      z = labels_sf$height_m
    ),
    "cannot be combined"
  )
  labels_sf$height_m[2] = "invalid"
  expect_no_condition(render_label(
    location = labels_sf,
    text = labels_sf$text,
    data_column_z = "height_m",
    scale_data = 0.5,
    relativez = FALSE,
    offset = 0,
    freetype = FALSE,
    clear_previous = TRUE
  ))
  ids = get_ids_with_labels(typeval = "textline")
  line_ids = ids$id[ids$tag == "textline"]
  line_tops = vapply(
    line_ids,
    function(id) {
      max(rgl::rgl.attrib(id, "vertices")[, 2])
    },
    numeric(1)
  )
  expect_equal(sort(unname(line_tops)), 10, tolerance = 1e-6)
})

test_that("render_label() reads text from sf columns and labels polygon centroids", {
  skip_if_not_installed("sf")
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  labels_sf = sf::st_sf(
    label = c("A", "B"),
    height_m = c(200, 400),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(4, 4, 6, 4, 6, 6, 4, 6, 4, 4),
        ncol = 2,
        byrow = TRUE
      ))),
      sf::st_polygon(list(matrix(
        c(14, 14, 16, 14, 16, 16, 14, 16, 14, 14),
        ncol = 2,
        byrow = TRUE
      )))
    )
  )
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250),
    extent = extent
  ))

  expect_no_condition(render_label(
    labels_sf,
    data_column_text = "label",
    data_column_z = "height_m",
    scale_data = 0.5,
    relativez = FALSE,
    line = FALSE,
    freetype = FALSE,
    clear_previous = TRUE
  ))

  ids = get_ids_with_labels(typeval = c("raytext", "textline"))
  expect_equal(sum(ids$tag == "raytext"), 2)
  expect_false(any(ids$tag == "textline"))
  expect_error(
    render_label(
      labels_sf,
      text = labels_sf$label,
      data_column_text = "label",
      data_column_z = "height_m"
    ),
    "cannot be combined"
  )
})

test_that("plot_3d() caches a raw matrix's 1-based extent", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 30)
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_equal(
    get_scene_extent(),
    c(
      xmin = 1,
      xmax = nrow(heightmap),
      ymin = 1,
      ymax = ncol(heightmap)
    )
  )

  expect_no_condition(render_label(
    text = "A",
    x = 1,
    y = 1,
    z = 10,
    clear_previous = TRUE
  ))

  ids = get_ids_with_labels(typeval = "textline")
  line_id = ids$id[ids$tag == "textline"][1]
  line_verts = rgl::rgl.attrib(line_id, "vertices")
  expect_equal(
    unname(line_verts[1, 1]),
    -(nrow(heightmap) - 1) / 2,
    tolerance = 1e-6
  )
  expect_equal(
    unname(line_verts[1, 3]),
    (ncol(heightmap) - 1) / 2,
    tolerance = 1e-6
  )

  expect_no_condition(render_people(
    x = 10,
    y = 15,
    altitude = 0,
    clear_previous = TRUE,
    lit = FALSE
  ))
  expect_equal(sum(get_ids_with_labels()$tag == "objperson"), 1)
})

test_that("render_points() accepts x/y names and lat/long aliases", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_points(
    y = 10,
    x = 10,
    extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
    offset = 10,
    clear_previous = TRUE
  ))
  expect_no_condition(render_points(
    lat = 10,
    long = 10,
    extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
    offset = 10,
    clear_previous = TRUE
  ))
  expect_error(
    render_points(
      y = 10,
      x = 10,
      lat = 10,
      extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
    ),
    "Use only one of `y` or `lat`"
  )
  expect_error(
    render_points(
      y = 10,
      x = 10,
      long = 10,
      extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
    ),
    "Use only one of `x` or `long`"
  )
})

test_that("render_obj() and render_tree() accept x/y coordinates", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_obj(
    flag_pole_obj(),
    y = 10,
    x = 10,
    extent = extent,
    heightmap = heightmap,
    scale = c(1, 1, 1),
    clear_previous = TRUE
  ))
  expect_true(any(get_ids_with_labels()$tag == "obj"))

  expect_no_condition(render_tree(
    y = 12,
    x = 12,
    extent = extent,
    heightmap = heightmap,
    tree_height = 5,
    clear_previous = TRUE
  ))
  expect_true(any(get_ids_with_labels()$tag == "objtree"))
})

test_that("render_path() and render_raymesh() accept x/y coordinates", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  skip_if_not_installed("rayvertex")

  heightmap = matrix(0, nrow = 20, ncol = 20)
  texture = sphere_shade(heightmap)
  extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20)
  expect_no_condition(plot_3d_test(
    texture,
    heightmap,
    zscale = 10,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_no_condition(render_path(
    y = c(5, 15),
    x = c(5, 15),
    extent = extent,
    heightmap = heightmap,
    clear_previous = TRUE
  ))
  expect_true(any(get_ids_with_labels()$tag == "path3d"))

  expect_no_condition(render_path(
    lat = c(6, 16),
    long = c(6, 16),
    extent = extent,
    heightmap = heightmap,
    clear_previous = TRUE
  ))
  expect_error(
    render_path(
      y = c(5, 15),
      x = c(5, 15),
      lat = c(5, 15),
      extent = extent,
      heightmap = heightmap
    ),
    "Use only one of `y` or `lat`"
  )

  mesh = rayvertex::sphere_mesh(radius = 1)
  expect_no_condition(render_raymesh(
    mesh,
    y = 10,
    x = 10,
    extent = extent,
    heightmap = heightmap,
    clear_previous = TRUE
  ))
  expect_true(any(get_ids_with_labels()$tag == "obj_raymesh"))

  expect_no_condition(render_raymesh(
    mesh,
    lat = 12,
    long = 12,
    extent = extent,
    heightmap = heightmap,
    clear_previous = TRUE
  ))
  expect_error(
    render_raymesh(
      mesh,
      y = 10,
      x = 10,
      long = 10,
      extent = extent,
      heightmap = heightmap
    ),
    "Use only one of `x` or `long`"
  )
})

test_that("cached scene messages include cached symbol labels", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  withr::local_options(list(rayshader.verbose_scene_cache = TRUE))

  elmat = matrix(0, nrow = 20, ncol = 20)
  zs = 10
  texture = sphere_shade(elmat)
  expect_no_condition(plot_3d_test(
    texture,
    elmat,
    zscale = zs,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  out = character()
  expect_no_error(withCallingHandlers(
    render_points(
      y = 10,
      x = 10,
      extent = c(xmin = 0, xmax = 20, ymin = 0, ymax = 20),
      offset = 10
    ),
    message = function(cnd) {
      out <<- c(out, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))
  expect_true(any(grepl("scene_heightmap", out, fixed = TRUE)))
  expect_true(any(grepl("elmat", out, fixed = TRUE)))
  expect_true(any(grepl("scene_zscale", out, fixed = TRUE)))
  expect_true(any(grepl("zs", out, fixed = TRUE)))
})

test_that("cached scene messages stay off when the option is unset", {
  withr::local_options(list(rayshader.verbose_scene_cache = NULL))

  out = character()
  expect_no_error(withCallingHandlers(
    emit_scene_cache_message(
      caller = "render_points",
      argument_name = "heightmap",
      cache_name = "scene_heightmap",
      cache_label = "elmat"
    ),
    message = function(cnd) {
      out <<- c(out, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))
  expect_length(out, 0)
})

test_that("plot_3d() accepts raster input and caches spatial metadata", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  skip_if_not_installed("raster")

  elev_raster = raster::raster(
    nrows = 20,
    ncols = 20,
    xmn = 100,
    xmx = 500,
    ymn = 1000,
    ymx = 1800,
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  )
  raster::values(elev_raster) = seq_len(raster::ncell(elev_raster))
  texture = sphere_shade(raster_to_matrix(elev_raster))

  expect_no_condition(plot_3d_test(
    texture,
    elev_raster,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  expect_equal(
    get_scene_zscale(),
    mean(raster::res(elev_raster)),
    tolerance = 1e-8
  )
  expect_equal(
    get_extent(get_scene_extent()),
    c(xmin = 100, xmax = 500, ymin = 1000, ymax = 1800)
  )
  expect_false(is.null(get_scene_crs(default = NULL)))
  expect_false(is.na(get_scene_crs()))

  expect_no_condition(render_points(
    y = 1400,
    x = 200,
    offset = 30,
    size = 1
  ))
})

test_that("cached scene extent can resolve the scene center in latlong", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  skip_if_not_installed("raster")
  skip_if_not_installed("sf")

  scene_center_long = -77.035249
  scene_center_lat = 38.889462
  scene_center_proj = transform_xy_between_crs(
    x_vals = scene_center_long,
    y_vals = scene_center_lat,
    source_crs = 4326,
    target_crs = 3857
  )
  elev_raster = raster::raster(
    nrows = 20,
    ncols = 20,
    xmn = scene_center_proj$x - 200,
    xmx = scene_center_proj$x + 200,
    ymn = scene_center_proj$y - 200,
    ymx = scene_center_proj$y + 200,
    crs = sf::st_crs(3857)$wkt
  )
  raster::values(elev_raster) = seq_len(raster::ncell(elev_raster))
  texture = sphere_shade(raster_to_matrix(elev_raster))

  expect_no_condition(plot_3d_test(
    texture,
    elev_raster,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  scene_center_latlong = resolve_cached_extent_center_latlong(
    caller = "render_highquality"
  )
  expect_equal(scene_center_latlong$source, "scene")
  expect_equal(scene_center_latlong$long, scene_center_long, tolerance = 1e-6)
  expect_equal(scene_center_latlong$lat, scene_center_lat, tolerance = 1e-6)
})

test_that("transform_into_heightmap_coords() can use cached scene extent", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()
  skip_if_not_installed("raster")

  elev_raster = raster::raster(
    nrows = 10,
    ncols = 10,
    xmn = 0,
    xmx = 100,
    ymn = 0,
    ymx = 200
  )
  raster::values(elev_raster) = seq_len(raster::ncell(elev_raster))
  texture = sphere_shade(raster_to_matrix(elev_raster))

  expect_no_condition(plot_3d_test(
    texture,
    elev_raster,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(250, 250)
  ))

  coords = transform_into_heightmap_coords(
    extent = NULL,
    heightmap = NULL,
    lat = 100,
    long = 50,
    altitude = 50,
    zscale = get_scene_zscale()
  )
  expect_equal(dim(coords), c(1, 3))
  expect_true(all(is.finite(coords)))
})
