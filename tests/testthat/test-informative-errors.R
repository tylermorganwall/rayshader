test_that("package functions do not use stopifnot for validation", {
  namespace = asNamespace("rayshader")
  object_names = ls(namespace, all.names = TRUE)
  function_names = object_names[vapply(
    object_names,
    function(name) is.function(get(name, envir = namespace)),
    logical(1)
  )]
  offenders = function_names[vapply(
    function_names,
    function(name) {
      function_text = paste(
        deparse(body(get(name, envir = namespace))),
        collapse = "\n"
      )
      grepl("stopifnot[[:space:]]*\\(", function_text)
    },
    logical(1)
  )]

  expect_length(offenders, 0)
})

test_that("extent validation reports the required ordering", {
  expect_error(
    get_extent(c(1, 0, 0, 1)),
    "each maximum greater than its minimum",
    fixed = TRUE
  )
  expect_error(
    get_extent(c(0, 1, 0)),
    "must contain exactly four values",
    fixed = TRUE
  )
})

test_that("cloud parameter validation identifies the invalid argument", {
  expect_error(
    validate_cloud_parameters(100, 100, 1, 45),
    "`start_altitude` and `end_altitude` must be different.",
    fixed = TRUE
  )
  expect_error(
    validate_cloud_parameters(100, 200, 0, 45),
    "`layers` must begin with a finite number greater than 0.",
    fixed = TRUE
  )
  expect_error(
    validate_cloud_parameters(100, 200, 1, NA_real_),
    "`sun_altitude` must be a single finite number",
    fixed = TRUE
  )
})

test_that("internal polygon value validation names invalid inputs", {
  polygon = data.frame(value = 1)

  expect_error(
    get_polygon_data_value(polygon, default_value = "invalid"),
    "`default_value` must be numeric",
    fixed = TRUE
  )
  expect_error(
    get_polygon_data_value(polygon, scale_data = "invalid"),
    "`scale_data` must be numeric.",
    fixed = TRUE
  )
})

test_that("exported entry points use argument-specific errors", {
  expect_error(
    render_snapshot(fsaa = 0),
    "`fsaa` must be a single finite number greater than or equal to 1.",
    fixed = TRUE
  )
  expect_error(
    plot_gg(ggobj = 1),
    "`ggobj` must be a ggplot object or a length-2 list of ggplot objects.",
    fixed = TRUE
  )
})
