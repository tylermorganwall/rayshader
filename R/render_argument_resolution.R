#' Validate filter-to-extent input
#'
#' @param filter_to_extent Logical value.
#' @param caller Default `NULL`. Calling function name used in errors.
#'
#' @return Validated logical value.
#' @keywords internal
validate_filter_to_extent = function(filter_to_extent = TRUE, caller = NULL) {
  if (
    !is.logical(filter_to_extent) ||
      length(filter_to_extent) != 1L ||
      is.na(filter_to_extent)
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`filter_to_extent` must be a single TRUE/FALSE value."
      ),
      call. = FALSE
    )
  }
  invisible(filter_to_extent)
}

#' Clear a render layer and detect clear-only calls
#'
#' Calls `clear_previous_handler` whenever `clear_previous` is `TRUE`, then
#' reports whether the call contains no arguments that request new geometry.
#'
#' @param clear_previous Clear-previous argument value.
#' @param call Matched render call.
#' @param clear_previous_handler Function that removes the existing render
#' layer.
#' @param routing_arguments Default `character()`. Arguments that select the
#' render layer without supplying new geometry.
#'
#' @return Whether rendering should return after clearing the existing layer.
#' @keywords internal
is_render_clear_only_call = function(
  clear_previous,
  call,
  clear_previous_handler,
  routing_arguments = character()
) {
  if (!isTRUE(clear_previous)) {
    return(FALSE)
  }
  clear_previous_handler()
  supplied_arguments = names(as.list(call)[-1L])
  length(supplied_arguments) > 0L &&
    "clear_previous" %in% supplied_arguments &&
    all(
      supplied_arguments %in%
        c("clear_previous", routing_arguments)
    )
}

#' Resolve a positive render number
#'
#' @param value Numeric-like value.
#' @param name Argument name.
#' @param allow_zero Default `FALSE`. Whether zero is accepted.
#'
#' @return Numeric scalar.
#' @keywords internal
resolve_render_positive_number = function(
  value,
  name,
  allow_zero = FALSE
) {
  value = suppressWarnings(as.numeric(value))
  valid = length(value) == 1L &&
    is.finite(value) &&
    if (allow_zero) value >= 0 else value > 0
  if (!valid) {
    stop(
      sprintf(
        "`%s` must be a single %s number.",
        name,
        if (allow_zero) "non-negative" else "positive"
      ),
      call. = FALSE
    )
  }
  value
}

#' Resolve a render logical
#'
#' @param value Logical-like value.
#' @param name Argument name.
#'
#' @return Logical scalar.
#' @keywords internal
resolve_render_logical = function(value, name) {
  value = suppressWarnings(as.logical(value))
  if (!length(value) || is.na(value[[1L]])) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  value[[1L]]
}

#' Validate a render column name
#'
#' @param value Column name.
#' @param argument Argument name used in errors.
#'
#' @return Validated column name.
#' @keywords internal
validate_render_column_name = function(value, argument) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop(
      sprintf("`%s` must be a single column name.", argument),
      call. = FALSE
    )
  }
  value
}

#' Resolve a render column reference
#'
#' @param value Column argument value.
#' @param value_expr Captured column argument expression.
#' @param missing Whether the argument was omitted.
#' @param argument Argument name used in errors.
#' @param allow_null Default `TRUE`. Whether omission or explicit `NULL` returns
#' `NULL`.
#'
#' @return Column name or `NULL`.
#' @keywords internal
resolve_render_column_name = function(
  value = NULL,
  value_expr = NULL,
  missing = FALSE,
  argument,
  allow_null = TRUE
) {
  null_value = isTRUE(missing) || identical(value_expr, quote(NULL))
  if (null_value) {
    if (isTRUE(allow_null)) {
      return(NULL)
    }
    stop(sprintf("`%s` cannot be NULL.", argument), call. = FALSE)
  }
  if (is.character(value_expr)) {
    return(validate_render_column_name(value_expr, argument))
  }
  if (is.name(value_expr)) {
    evaluated = tryCatch(value, error = function(error) NULL)
    if (is.character(evaluated) && length(evaluated) == 1L) {
      return(validate_render_column_name(evaluated, argument))
    }
    return(validate_render_column_name(
      as.character(value_expr),
      argument
    ))
  }
  validate_render_column_name(value, argument)
}

#' Resolve a scalar-or-column render argument
#'
#' @param value Argument value.
#' @param value_expr Captured argument expression.
#' @param missing Whether the argument was omitted.
#' @param default Default scalar used for an omitted argument.
#' @param argument Argument name used in errors.
#' @param type Default `c("double", "integer", "logical")`. Scalar type.
#' @param lower Default `-Inf`. Lower accepted scalar bound.
#' @param upper Default `Inf`. Upper accepted scalar bound.
#' @param lower_inclusive Default `TRUE`. Whether `lower` is inclusive.
#' @param upper_inclusive Default `TRUE`. Whether `upper` is inclusive.
#'
#' @return A list containing exactly one resolved scalar `value` or `column`.
#' @keywords internal
resolve_render_scalar_or_column = function(
  value,
  value_expr,
  missing,
  default,
  argument,
  type = c("double", "integer", "logical"),
  lower = -Inf,
  upper = Inf,
  lower_inclusive = TRUE,
  upper_inclusive = TRUE
) {
  type = match.arg(type)
  if (isTRUE(missing)) {
    return(list(
      value = resolve_render_scalar(
        value = default,
        missing = FALSE,
        default = default,
        argument = argument,
        type = type,
        lower = lower,
        upper = upper,
        lower_inclusive = lower_inclusive,
        upper_inclusive = upper_inclusive
      ),
      column = NULL
    ))
  }
  if (is.character(value_expr)) {
    return(list(
      value = NULL,
      column = validate_render_column_name(value_expr, argument)
    ))
  }
  if (is.name(value_expr)) {
    evaluated = tryCatch(value, error = function(error) NULL)
    if (
      is.numeric(evaluated) ||
        (type == "logical" && is.logical(evaluated))
    ) {
      return(list(
        value = resolve_render_scalar(
          value = evaluated,
          missing = FALSE,
          default = default,
          argument = argument,
          type = type,
          lower = lower,
          upper = upper,
          lower_inclusive = lower_inclusive,
          upper_inclusive = upper_inclusive
        ),
        column = NULL
      ))
    }
    if (is.character(evaluated) && length(evaluated) == 1L) {
      return(list(
        value = NULL,
        column = validate_render_column_name(evaluated, argument)
      ))
    }
    return(list(
      value = NULL,
      column = validate_render_column_name(
        as.character(value_expr),
        argument
      )
    ))
  }
  list(
    value = resolve_render_scalar(
      value = value,
      missing = FALSE,
      default = default,
      argument = argument,
      type = type,
      lower = lower,
      upper = upper,
      lower_inclusive = lower_inclusive,
      upper_inclusive = upper_inclusive
    ),
    column = NULL
  )
}

#' Resolve a scalar render argument
#'
#' @param value Argument value.
#' @param missing Whether the argument was omitted.
#' @param default Default value used for an omitted argument.
#' @param argument Argument name used in errors.
#' @param type Default `c("double", "integer", "logical")`. Scalar type.
#' @param lower Default `-Inf`. Lower accepted bound.
#' @param upper Default `Inf`. Upper accepted bound.
#' @param lower_inclusive Default `TRUE`. Whether `lower` is inclusive.
#' @param upper_inclusive Default `TRUE`. Whether `upper` is inclusive.
#' @param allow_null Default `FALSE`. Whether an explicit `NULL` is accepted.
#'
#' @return A normalized scalar or `NULL`.
#' @keywords internal
resolve_render_scalar = function(
  value,
  missing,
  default,
  argument,
  type = c("double", "integer", "logical"),
  lower = -Inf,
  upper = Inf,
  lower_inclusive = TRUE,
  upper_inclusive = TRUE,
  allow_null = FALSE
) {
  type = match.arg(type)
  if (isTRUE(missing)) {
    value = default
  }
  if (is.null(value)) {
    if (isTRUE(allow_null)) {
      return(NULL)
    }
    stop(sprintf("`%s` cannot be NULL.", argument), call. = FALSE)
  }
  if (length(value) != 1L) {
    stop(sprintf("`%s` must be a single value.", argument), call. = FALSE)
  }
  resolved = switch(
    type,
    double = suppressWarnings(as.numeric(value)),
    integer = suppressWarnings(as.numeric(value)),
    logical = suppressWarnings(as.logical(value))
  )
  if (
    length(resolved) != 1L ||
      is.na(resolved) ||
      (type != "logical" && !is.finite(resolved))
  ) {
    stop(
      sprintf("`%s` must be a single %s value.", argument, type),
      call. = FALSE
    )
  }
  if (type == "integer") {
    if (resolved != floor(resolved)) {
      stop(sprintf("`%s` must be a whole number.", argument), call. = FALSE)
    }
    resolved = as.integer(resolved)
  }
  if (type == "logical") {
    return(resolved)
  }
  below = if (isTRUE(lower_inclusive)) {
    resolved < lower
  } else {
    resolved <= lower
  }
  above = if (isTRUE(upper_inclusive)) {
    resolved > upper
  } else {
    resolved >= upper
  }
  if (below || above) {
    lower_relation = if (isTRUE(lower_inclusive)) ">=" else ">"
    upper_relation = if (isTRUE(upper_inclusive)) "<=" else "<"
    stop(
      sprintf(
        "`%s` must satisfy %s %s and %s %s.",
        argument,
        lower_relation,
        format(lower),
        upper_relation,
        format(upper)
      ),
      call. = FALSE
    )
  }
  resolved
}

#' Resolve feature-aligned render argument values
#'
#' @param data Spatial or tabular feature data.
#' @param value Argument value.
#' @param value_expr Captured argument expression.
#' @param missing Whether the argument was omitted.
#' @param default Default scalar used for an omitted argument.
#' @param argument Argument name used in errors.
#' @param type Default `c("double", "integer", "character")`. Value type.
#' @param lower Default `-Inf`. Lower accepted numeric bound.
#' @param upper Default `Inf`. Upper accepted numeric bound.
#' @param allow_na Default `FALSE`. Whether missing values are accepted.
#'
#' @return A vector aligned with the rows or features in `data`.
#' @keywords internal
resolve_render_feature_values = function(
  data,
  value,
  value_expr,
  missing,
  default,
  argument,
  type = c("double", "integer", "character"),
  lower = -Inf,
  upper = Inf,
  allow_na = FALSE
) {
  type = match.arg(type)
  feature_data = if (inherits(data, "SpatialLinesDataFrame")) {
    data@data
  } else if (inherits(data, "sf") || is.data.frame(data)) {
    data
  } else {
    NULL
  }
  feature_count = if (!is.null(feature_data)) {
    nrow(feature_data)
  } else if (inherits(data, "sfc")) {
    length(data)
  } else if (inherits(data, "sfg")) {
    1L
  } else if (inherits(data, "SpatialLines")) {
    length(data@lines)
  } else {
    0L
  }
  column_names = if (is.null(feature_data)) {
    character(0)
  } else {
    names(feature_data)
  }
  if (isTRUE(missing)) {
    resolved = default
  } else if (
    is.name(value_expr) &&
      as.character(value_expr) %in% column_names
  ) {
    resolved = feature_data[[as.character(value_expr)]]
  } else {
    evaluated = tryCatch(
      value,
      error = function(error) {
        stop(
          sprintf(
            "Could not resolve `%s`: %s",
            argument,
            conditionMessage(error)
          ),
          call. = FALSE
        )
      }
    )
    if (
      is.character(evaluated) &&
        length(evaluated) == 1L &&
        !is.na(evaluated) &&
        evaluated %in% column_names
    ) {
      resolved = feature_data[[evaluated]]
    } else {
      resolved = evaluated
    }
  }
  if (is.factor(resolved)) {
    resolved = as.character(resolved)
  }
  raw_missing = is.na(resolved)
  if (is.character(resolved)) {
    raw_missing = raw_missing | !nzchar(trimws(resolved))
  }
  resolved = switch(
    type,
    double = suppressWarnings(as.numeric(resolved)),
    integer = suppressWarnings(as.numeric(resolved)),
    character = as.character(resolved)
  )
  if (length(resolved) == 1L && feature_count != 1L) {
    resolved = rep(resolved, feature_count)
  }
  if (length(resolved) != feature_count) {
    stop(
      sprintf(
        "`%s` must be scalar or contain one value per feature.",
        argument
      ),
      call. = FALSE
    )
  }
  missing_value = is.na(resolved)
  if (type != "character") {
    missing_value = missing_value | !is.finite(resolved)
    invalid_coercion = !raw_missing & missing_value
    if (any(invalid_coercion)) {
      stop(
        sprintf("`%s` contains values that cannot be converted.", argument),
        call. = FALSE
      )
    }
  }
  if (!isTRUE(allow_na) && any(missing_value)) {
    stop(
      sprintf("`%s` cannot contain missing values.", argument),
      call. = FALSE
    )
  }
  present = !missing_value
  if (type != "character") {
    if (any(resolved[present] < lower | resolved[present] > upper)) {
      stop(
        sprintf(
          "`%s` values must be between %s and %s.",
          argument,
          format(lower),
          format(upper)
        ),
        call. = FALSE
      )
    }
    if (
      type == "integer" &&
        any(resolved[present] != floor(resolved[present]))
    ) {
      stop(sprintf("`%s` must contain whole numbers.", argument), call. = FALSE)
    }
  }
  if (type == "integer") {
    resolved = as.integer(resolved)
  }
  resolved
}
