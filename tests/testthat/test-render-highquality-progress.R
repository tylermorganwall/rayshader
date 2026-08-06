new_test_progress_recorder = function() {
  recorder = new.env(parent = emptyenv())
  recorder$calls = list()
  recorder$ticks = integer()
  recorder$factory = function(verbose, label, total) {
    call_index = length(recorder$calls) + 1L
    recorder$calls[[call_index]] = list(
      verbose = verbose,
      label = label,
      total = total
    )
    recorder$ticks[[call_index]] = 0L
    progress_bar = new.env(parent = emptyenv())
    progress_bar$tick = local({
      index = call_index
      function(len = 1L) {
        recorder$ticks[[index]] = recorder$ticks[[index]] + len
        invisible(NULL)
      }
    })
    progress_bar
  }
  recorder
}

test_that("render_highquality progress bars honor verbose", {
  expect_null(new_render_highquality_progress_bar(
    verbose = FALSE,
    label = "Ignored",
    total = 1L
  ))
  expect_null(new_render_highquality_progress_bar(
    verbose = TRUE,
    label = "Ignored",
    total = 0L
  ))

  progress_bar = new_render_highquality_progress_bar(
    verbose = TRUE,
    label = "Test scene building",
    total = 1L
  )
  expect_s3_class(progress_bar, "R6")
  suppressMessages(progress_bar$tick())
  expect_true(progress_bar$finished)
})

test_that("stream mesh progress tracks every task", {
  recorder = new_test_progress_recorder()
  testthat::local_mocked_bindings(
    new_render_highquality_progress_bar = recorder$factory,
    make_render_highquality_water_path_mesh = function(value) value,
    .package = "rayshader"
  )

  meshes = make_render_highquality_water_path_meshes(
    list(list(value = 1L), list(value = 2L), list(value = 3L)),
    verbose = TRUE
  )

  expect_equal(unlist(meshes), seq_len(3L))
  expect_length(recorder$calls, 1L)
  expect_true(recorder$calls[[1L]]$verbose)
  expect_equal(
    recorder$calls[[1L]]$label,
    "Converting stream lines to meshes"
  )
  expect_equal(recorder$calls[[1L]]$total, 3L)
  expect_equal(recorder$ticks[[1L]], 3L)
})

test_that("road mesh progress tracks every assembled chain", {
  recorder = new_test_progress_recorder()
  chain_tasks = list(list(value = 1L), list(value = 2L))
  attr(chain_tasks, "mesh_chain_members") = data.frame(
    mesh_chain_id = seq_len(2L),
    render_road_fragment_id = 11:12
  )
  attr(chain_tasks, "envelope_sections") = NULL
  attr(chain_tasks, "mesh_chain_diagnostics") = list()
  testthat::local_mocked_bindings(
    new_render_highquality_progress_bar = recorder$factory,
    attach_render_road_mesh_task_metadata = function(tasks) tasks,
    assemble_render_road_mesh_chain_tasks = function(tasks) chain_tasks,
    make_render_highquality_road_chain_mesh = function(value) {
      structure(list(value = value), class = "test_road_mesh")
    },
    .package = "rayshader"
  )

  meshes = make_render_highquality_road_path_meshes(
    list(list(value = 1L)),
    verbose = TRUE
  )

  expect_length(meshes, 2L)
  expect_length(recorder$calls, 1L)
  expect_true(recorder$calls[[1L]]$verbose)
  expect_equal(recorder$calls[[1L]]$label, "Converting roads to meshes")
  expect_equal(recorder$calls[[1L]]$total, 2L)
  expect_equal(recorder$ticks[[1L]], 2L)
})

test_that("render_highquality propagates verbose through scene building", {
  on.exit(rgl::close3d(), add = TRUE)
  local_rgl_use_null()

  heightmap = matrix(0, nrow = 5, ncol = 5)
  expect_no_condition(plot_3d_test(
    constant_shade(heightmap),
    heightmap,
    solid = FALSE,
    shadow = FALSE,
    water = FALSE,
    windowsize = c(200, 200)
  ))
  rgl::lines3d(
    x = c(-1, 1),
    y = c(0, 0),
    z = c(-1, 1),
    color = "black",
    tag = "path3d"
  )

  recorder = new_test_progress_recorder()
  propagated = new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    new_render_highquality_progress_bar = recorder$factory,
    make_render_highquality_water_path_meshes = function(
      tasks,
      verbose = FALSE
    ) {
      propagated$stream = verbose
      list()
    },
    make_render_highquality_road_path_meshes = function(
      tasks,
      verbose = FALSE
    ) {
      propagated$road = verbose
      list()
    },
    .package = "rayshader"
  )

  expect_no_condition(render_highquality(
    return_scene = TRUE,
    light = FALSE,
    line_render = "world",
    verbose = TRUE,
    parallel = TRUE
  ))

  expect_true(propagated$stream)
  expect_true(propagated$road)
  expect_length(recorder$calls, 1L)
  expect_equal(recorder$calls[[1L]]$label, "Preparing line paths")
  expect_equal(recorder$calls[[1L]]$total, 1L)
  expect_equal(recorder$ticks[[1L]], 1L)
  expect_error(
    render_highquality(return_scene = TRUE, verbose = NA),
    "`verbose` must be TRUE or FALSE",
    fixed = TRUE
  )
})
