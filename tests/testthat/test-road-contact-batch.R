test_that("native contact batches preserve order and serial results", {
  heightmap = outer(seq_len(31), seq_len(25), function(x, y) sin(x / 3) + y)
  paths = list(
    rbind(c(-8, 3, 0), c(0, 2, 1), c(8, 4, 3)),
    rbind(c(0, 0, 0), c(5, 0, 0), c(0, 0, 0)),
    rbind(c(40, 0, 40), c(42, 1, 41))
  )
  jobs = lapply(
    paths,
    prepare_render_road_terrain_contact_job,
    width = 2,
    texture_world_scale = c(1, 1)
  )
  expected = lapply(jobs, function(job) {
    sample_render_road_terrain_contacts_cpp(
      job$vertices,
      job$indices,
      job$station,
      heightmap,
      2,
      c(1, 1)
    )
  })
  context = create_render_road_terrain_context_cpp(heightmap, 2, c(1, 1))
  for (parallel in c(FALSE, TRUE)) {
    for (snapshot in list(NULL, context)) {
      actual = sample_render_road_terrain_contacts_batch_cpp(
        jobs,
        heightmap,
        2,
        c(1, 1),
        parallel,
        snapshot
      )
      expect_identical(actual, expected)
    }
  }
  expect_identical(
    sample_render_road_terrain_contacts_batch_cpp(
      list(),
      heightmap,
      2,
      c(1, 1),
      TRUE
    ),
    list()
  )
})

test_that("contact snapshots own terrain and expire safely", {
  h = matrix(7, 21, 21)
  job = prepare_render_road_terrain_contact_job(
    rbind(c(-3, 1, 0), c(3, 1, 0)),
    2,
    c(1, 1)
  )
  context = create_render_road_terrain_context_cpp(h, 1, c(1, 1))
  h[] = 12
  gc()
  frozen = sample_render_road_terrain_contacts_batch_cpp(
    list(job),
    h,
    1,
    c(1, 1),
    TRUE,
    context
  )
  current = sample_render_road_terrain_contacts_batch_cpp(
    list(job),
    h,
    1,
    c(1, 1),
    TRUE
  )
  expect_identical(unique(frozen[[1]]$floor), 7)
  expect_identical(unique(current[[1]]$floor), 12)
  expired = unserialize(serialize(context, NULL))
  expect_error(
    sample_render_road_terrain_contacts_batch_cpp(
      list(job),
      h,
      1,
      c(1, 1),
      TRUE,
      expired
    ),
    "invalid or has expired"
  )
})

test_that("contact batches validate inputs and recover from worker errors", {
  h = matrix(7, 21, 21)
  job = prepare_render_road_terrain_contact_job(
    rbind(c(-3, 1, 0), c(3, 1, 0)),
    2,
    c(1, 1)
  )
  bad = job
  bad$station[1] = NA_real_
  expect_error(
    sample_render_road_terrain_contacts_batch_cpp(
      list(job, bad),
      h,
      1,
      c(1, 1),
      TRUE
    ),
    "finite coordinates and stations"
  )
  bad = job
  bad$indices[1, 1] = NA_integer_
  expect_error(
    sample_render_road_terrain_contacts_batch_cpp(
      list(bad),
      h,
      1,
      c(1, 1),
      TRUE
    ),
    "Invalid road triangle index"
  )
  expect_error(
    sample_render_road_terrain_contacts_batch_cpp(
      list(job),
      h,
      0,
      c(1, 1),
      TRUE
    ),
    "positive scales"
  )
  for (parallel in c(FALSE, TRUE)) {
    expect_error(
      sample_render_road_terrain_contacts_batch_cpp(
        list(job, job),
        h * NA_real_,
        1,
        c(1, 1),
        parallel
      ),
      "job 1: Road footprint contains nonfinite terrain"
    )
  }
  expect_no_error(sample_render_road_terrain_contacts_batch_cpp(
    list(job, job),
    h,
    1,
    c(1, 1),
    TRUE
  ))
})

test_that("envelope batching preserves results across chunk boundaries", {
  h = matrix(7, 21, 21)
  points = rbind(c(-3, 1, 0), c(3, 1, 0))
  expected = sample_render_road_terrain_envelope(points, 2, h, 1, c(1, 1))
  for (parallel in c(FALSE, TRUE)) {
    result = sample_render_road_terrain_envelopes(
      rep(list(points), 257),
      rep(2, 257),
      h,
      1,
      c(1, 1),
      parallel
    )
    expect_identical(result, rep(list(expected), 257))
  }
  expect_identical(
    sample_render_road_terrain_envelopes(
      list(),
      numeric(),
      h,
      1,
      c(1, 1)
    ),
    list()
  )
  expect_error(
    sample_render_road_terrain_envelopes(
      list(points),
      numeric(),
      h,
      1,
      c(1, 1)
    ),
    "widths must match"
  )
})
