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
  propagated = new.env(parent = emptyenv())
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
    prepare_render_highquality_road_chain_meshes = function(
      tasks,
      verbose,
      parallel
    ) {
      progress = recorder$factory(
        verbose,
        "Preparing road mesh jobs",
        length(tasks)
      )
      lapply(tasks, function(task) {
        progress$tick()
        list(
          prepared = list(
            job = list(value = task$value),
            specifications = list(list()),
            value = task$value
          ),
          error = NULL
        )
      })
    },
    build_render_highquality_road_mesh_batch_cpp = function(
      input_jobs,
      parallel,
      verbose
    ) {
      propagated$jobs = input_jobs
      propagated$parallel = parallel
      propagated$verbose = verbose
      lapply(input_jobs, function(job) {
        list(success = TRUE, error = NULL, meshes = list(job))
      })
    },
    finalize_render_highquality_road_chain_mesh = function(
      prepared,
      native_result
    ) {
      structure(list(value = prepared$value), class = "test_road_mesh")
    },
    .package = "rayshader"
  )

  meshes = make_render_highquality_road_path_meshes(
    list(list(value = 1L)),
    verbose = TRUE,
    parallel = TRUE
  )

  expect_length(meshes, 2L)
  expect_length(recorder$calls, 1L)
  expect_true(recorder$calls[[1L]]$verbose)
  expect_equal(recorder$calls[[1L]]$label, "Preparing road mesh jobs")
  expect_equal(recorder$calls[[1L]]$total, 2L)
  expect_equal(recorder$ticks[[1L]], 2L)
  expect_length(propagated$jobs, 2L)
  expect_equal(vapply(propagated$jobs, `[[`, integer(1), "value"), 1:2)
  expect_true(propagated$parallel)
  expect_true(propagated$verbose)
})

test_that("road preparation batches each shared terrain once", {
  recorder = new_test_progress_recorder()
  calls = new.env(parent = emptyenv())
  calls$densify = list()
  calls$sections = list()
  original_densify = densify_render_road_paths_batch_cpp
  original_sections = sample_render_road_sections_batch_cpp
  heightmap = outer(seq_len(16L), seq_len(14L), `+`)
  tasks = lapply(seq_len(3L), function(index) {
    list(
      points = matrix(
        c(-4, index / 10, -3, 0, index / 10, 0, 4, index / 10, 3),
        ncol = 3L,
        byrow = TRUE
      ),
      bbox_center = c(0, 0, 0),
      width = 0.5,
      heightmap = heightmap,
      zscale = 2,
      material = list(name = index),
      terrain_following = TRUE,
      return_mesh = TRUE
    )
  })
  testthat::local_mocked_bindings(
    new_render_highquality_progress_bar = recorder$factory,
    densify_render_road_paths_batch_cpp = function(
      input_jobs,
      heightmap,
      zscale,
      parallel,
      verbose
    ) {
      calls$densify[[length(calls$densify) + 1L]] = list(
        total = length(input_jobs),
        parallel = parallel,
        verbose = verbose
      )
      original_densify(
        input_jobs,
        heightmap,
        zscale,
        parallel,
        FALSE
      )
    },
    sample_render_road_sections_batch_cpp = function(
      input_jobs,
      heightmap,
      zscale,
      parallel,
      verbose
    ) {
      calls$sections[[length(calls$sections) + 1L]] = list(
        total = length(input_jobs),
        parallel = parallel,
        verbose = verbose
      )
      original_sections(
        input_jobs,
        heightmap,
        zscale,
        parallel,
        FALSE
      )
    },
    .package = "rayshader"
  )

  prepared = prepare_render_highquality_road_chain_meshes(
    tasks,
    verbose = TRUE,
    parallel = TRUE
  )

  expect_true(all(vapply(
    prepared,
    function(result) {
      is.null(result$error) && !is.null(result$prepared)
    },
    logical(1)
  )))
  expect_length(calls$densify, 1L)
  expect_length(calls$sections, 1L)
  expect_equal(calls$densify[[1L]]$total, 3L)
  expect_equal(calls$sections[[1L]]$total, 3L)
  expect_true(calls$densify[[1L]]$parallel)
  expect_true(calls$sections[[1L]]$parallel)
  expect_true(calls$densify[[1L]]$verbose)
  expect_true(calls$sections[[1L]]$verbose)
  expect_length(recorder$calls, 1L)
  expect_equal(recorder$calls[[1L]]$label, "Preparing road mesh jobs")
  expect_equal(recorder$calls[[1L]]$total, 3L)
  expect_equal(recorder$ticks[[1L]], 3L)
})

test_that("road batch failures fall back in source order", {
  fallback_order = integer()
  chain_tasks = lapply(seq_len(3L), function(value) {
    task = list(value = value)
    attr(task, "mesh_topology") = list(mesh_chain_id = value)
    task
  })
  attr(chain_tasks, "mesh_chain_members") = data.frame(
    mesh_chain_id = seq_len(3L),
    render_road_fragment_id = 11:13
  )
  attr(chain_tasks, "envelope_sections") = NULL
  attr(chain_tasks, "mesh_chain_diagnostics") = list()
  testthat::local_mocked_bindings(
    attach_render_road_mesh_task_metadata = function(tasks) tasks,
    assemble_render_road_mesh_chain_tasks = function(tasks) chain_tasks,
    prepare_render_highquality_road_chain_meshes = function(
      tasks,
      verbose,
      parallel
    ) {
      lapply(tasks, function(task) {
        if (task$value == 1L) {
          return(list(prepared = NULL, error = "preparation failed"))
        }
        list(
          prepared = list(
            job = list(value = task$value),
            specifications = list(list())
          ),
          error = NULL
        )
      })
    },
    build_render_highquality_road_mesh_batch_cpp = function(
      input_jobs,
      parallel,
      verbose
    ) {
      expect_equal(vapply(input_jobs, `[[`, integer(1), "value"), 2:3)
      list(
        list(success = FALSE, error = "native failed", meshes = list()),
        list(success = TRUE, error = NULL, meshes = list(list(value = 3L)))
      )
    },
    finalize_render_highquality_road_chain_mesh = function(
      prepared,
      native_result
    ) {
      structure(list(value = 3L), class = "test_road_mesh")
    },
    make_render_highquality_buffered_road_chain_mesh = function(
      task,
      sweep_error
    ) {
      fallback_order <<- c(fallback_order, task$value)
      expect_equal(
        conditionMessage(sweep_error),
        if (task$value == 1L) "preparation failed" else "native failed"
      )
      structure(
        list(value = task$value),
        class = "test_road_mesh",
        render_road_buffered_fallback = list(used = TRUE)
      )
    },
    .package = "rayshader"
  )

  expect_warning(
    meshes <- make_render_highquality_road_path_meshes(
      list(list(value = 1L)),
      verbose = FALSE,
      parallel = TRUE
    ),
    "fallback for 2 chains"
  )

  expect_equal(fallback_order, 1:2)
  expect_equal(vapply(meshes, `[[`, integer(1), "value"), 1:3)
  expect_equal(
    attr(meshes, "mesh_chain_diagnostics")$buffered_fallback_chain_id,
    1:2
  )
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
      verbose = FALSE,
      parallel = FALSE
    ) {
      propagated$road = verbose
      propagated$road_parallel = parallel
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
  expect_true(propagated$road_parallel)
  expect_length(recorder$calls, 1L)
  expect_equal(recorder$calls[[1L]]$label, "Preparing line paths")
  expect_equal(recorder$calls[[1L]]$total, 1L)
  expect_equal(recorder$ticks[[1L]], 1L)
  expect_error(
    render_highquality(return_scene = TRUE, verbose = NA),
    "`verbose` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    render_highquality(return_scene = TRUE, parallel = NA),
    "`parallel` must be TRUE or FALSE",
    fixed = TRUE
  )
})
