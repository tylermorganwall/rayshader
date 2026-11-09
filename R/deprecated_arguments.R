#' @param call Matched call with unexpanded `...`.
#' @param aliases Named character vector mapping old arguments to current names.
#' @param function_name Name used in deprecation messages.
#' @param allow_dots Default `TRUE`. Whether other dots arguments are supported.
#' @return A call using current names, or `NULL` when no aliases were supplied.
#' @keywords internal
#' @noRd
deprecated_argument_call = function(
  call,
  aliases,
  function_name,
  allow_dots = TRUE
) {
  dots = as.list(call$...)
  renamed = which(names(dots) %in% names(aliases))
  if (!allow_dots && length(dots) > length(renamed)) {
    unknown = setdiff(seq_along(dots), renamed)
    labels = vapply(
      unknown,
      function(i) {
        label = names(dots)[i]
        if (is.null(label) || is.na(label) || !nzchar(label)) {
          deparse1(dots[[i]])
        } else {
          label
        }
      },
      character(1)
    )
    stop("unused argument(s): ", paste(labels, collapse = ", "), call. = FALSE)
  }
  if (!length(renamed)) {
    return(NULL)
  }
  old_names = names(dots)[renamed]
  new_names = unname(aliases[old_names])
  supplied = c(setdiff(names(call), "..."), new_names)
  duplicated_names = unique(supplied[duplicated(supplied)])
  if (length(duplicated_names)) {
    stop(
      "Arguments supplied more than once through current or deprecated names: ",
      paste(duplicated_names, collapse = ", "),
      call. = FALSE
    )
  }
  warning(
    paste(
      sprintf(
        "`%s` is deprecated in `%s()`; use `%s` instead.",
        old_names,
        function_name,
        new_names
      ),
      collapse = "\n"
    ),
    call. = FALSE
  )
  names(dots)[renamed] = new_names
  call$... = NULL
  # Re-enter before forcing promises, so missing() checks and explicit NULLs
  # behave exactly as they do for current names. Unrelated dots retain order.
  as.call(c(as.list(call), dots))
}
