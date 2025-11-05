#' Defunct functions in rayshader
#'
#' The following functions have been removed and are now defunct. Calling them
#' results in an error. Use the indicated replacements instead.
#'
#' \describe{
#'   \item{`reduce_matrix_size()`}{Use [resize_matrix()].}
#' }
#'
#' @name rayshader-defunct
#' @title Defunct functions in rayshader
#' @keywords internal
NULL

#' Reduce Matrix Size (defunct)
#'
#' @param ... Arguments to pass to [resize_matrix()] function.
#'
#' @return Reduced matrix.
#' @export
#' @examples
#'if(run_documentation()) {
#'montbaysmall = resize_matrix(montereybay, scale=0.5)
#'montbaysmall |>
#'  sphere_shade() |>
#'  plot_map()
#'}
reduce_matrix_size = function(...) {
	.Defunct("resize_matrix()", package = "rayshader")
}
