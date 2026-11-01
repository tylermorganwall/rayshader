test_that('native terrain snapshots preserve sections and densification', {
  heightmap = outer(seq_len(15), seq_len(13), function(x, y) sin(x / 4) + y / 3)
  heightmap[4:5, 7:8] = NA_real_
  scale = c(0.7, 1.3)
  points = cbind(c(-2, 0, 2), c(1, 1, 1), c(-3, 0, 3))
  frames = calculate_render_road_vertex_frames(points)
  jobs = rep(
    list(list(
      points = points,
      left_distance = rep(.4, 3),
      right_distance = rep(.4, 3),
      side = frames$side,
      miter_scale = frames$miter_scale,
      terrain_following = TRUE
    )),
    3
  )
  context = create_render_road_terrain_context_cpp(heightmap, 7, scale)
  for (parallel in c(FALSE, TRUE)) {
    args = list(
      input_jobs = jobs,
      heightmap = heightmap,
      zscale = 7,
      parallel = parallel,
      verbose = FALSE,
      terrain_scale = scale
    )
    reference = do.call(sample_render_road_sections_batch_cpp, args)
    actual = do.call(
      sample_render_road_sections_batch_cpp,
      c(args, list(terrain_context = context))
    )
    expect_identical(actual, reference)
    args$input_jobs = rep(
      list(list(points = points, width = .8, terrain_following = TRUE)),
      3
    )
    reference = do.call(densify_render_road_paths_batch_cpp, args)
    actual = do.call(
      densify_render_road_paths_batch_cpp,
      c(args, list(terrain_context = context))
    )
    expect_identical(actual, reference)
  }
})

test_that('terrain snapshots own their data and reject invalid handles', {
  h = matrix(0, 5, 5)
  points = cbind(c(-1, 1), 0, 0)
  frames = calculate_render_road_vertex_frames(points)
  job = list(
    points = points,
    left_distance = c(.3, .3),
    right_distance = c(.3, .3),
    side = frames$side,
    miter_scale = frames$miter_scale,
    terrain_following = TRUE
  )
  context = create_render_road_terrain_context_cpp(h, 10)
  sample = function(context) {
    sample_render_road_sections_batch_cpp(
      list(job),
      h,
      10,
      FALSE,
      FALSE,
      terrain_context = context
    )
  }
  original = sample(context)
  h[,] = rep(seq_len(5) * 100, each = 5)
  gc()
  expect_identical(sample(context), original)
  fresh = create_render_road_terrain_context_cpp(h, 10)
  expect_identical(sample(fresh), sample(NULL))
  expect_false(identical(sample(fresh), original))
  expect_error(sample(list()), 'context is invalid')
  # Serialized pointers have no live native allocation in the restored value.
  expired = unserialize(serialize(context, NULL))
  expect_error(sample(expired), 'context is invalid')
})

test_that('snapshot-free and empty-terrain helpers remain supported', {
  h = matrix(numeric(), 0, 0)
  context = create_render_road_terrain_context_cpp(h, 1)
  expect_identical(
    densify_render_road_paths_batch_cpp(list(), h, 1, FALSE, FALSE),
    list()
  )
  expect_identical(
    sample_render_road_sections_batch_cpp(
      list(),
      h,
      1,
      FALSE,
      FALSE,
      terrain_context = context
    ),
    list()
  )
})

test_that('preparation keeps distinct terrain groups and failures separate', {
  a = matrix(0, 9, 9)
  b = outer(seq_len(9), seq_len(9), function(x, y) x * y)
  task = function(h, z) {
    list(
      points = cbind(c(-2, 0, 2), c(0, .3, 0), c(-1, 0, 1)),
      bbox_center = c(0, 0, 0),
      width = .5,
      material = list(),
      heightmap = h,
      zscale = z,
      return_mesh = TRUE
    )
  }
  tasks = list(task(a, 1), task(b, 5), list(width = 1), task(a, 1), task(b, 10))
  batch = prepare_render_highquality_road_chain_meshes(tasks, parallel = TRUE)
  expect_match(batch[[3]]$error, 'inputs do not match')
  for (i in c(1, 2, 4, 5)) {
    single = prepare_render_highquality_road_chain_meshes(
      list(tasks[[i]]),
      parallel = FALSE
    )
    expect_null(batch[[i]]$error)
    expect_identical(batch[[i]]$prepared$job, single[[1]]$prepared$job)
  }
})
