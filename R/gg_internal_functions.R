#'@title translate_shape_string
#'
#'@description Required internal ggplot object for 3D ggplots
#'
#'@param shape_string A shape string.
#'@keywords internal
translate_shape_string = function (shape_string) {
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }
  pch_table = c(`square open` = 0, `circle open` = 1, `triangle open` = 2, 
                plus = 3, cross = 4, `diamond open` = 5, `triangle down open` = 6, 
                `square cross` = 7, asterisk = 8, `diamond plus` = 9, 
                `circle plus` = 10, star = 11, `square plus` = 12, `circle cross` = 13, 
                `square triangle` = 14, `triangle square` = 14, square = 15, 
                `circle small` = 16, triangle = 17, diamond = 18, circle = 19, 
                bullet = 20, `circle filled` = 21, `square filled` = 22, 
                `diamond filled` = 23, `triangle filled` = 24, `triangle down filled` = 25)
  shape_match = charmatch(shape_string, names(pch_table))
  invalid_strings = is.na(shape_match)
  nonunique_strings = shape_match == 0
  if (any(invalid_strings)) {
    bad_string = unique(shape_string[invalid_strings])
    n_bad = length(bad_string)
    collapsed_names = sprintf("\n* '%s'", bad_string[1:min(5, 
                                                           n_bad)])
    more_problems = if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 
                5, ifelse(n_bad > 6, "s", ""))
    }
    stop("Can't find shape name:", collapsed_names, more_problems, 
         call. = FALSE)
  }
  if (any(nonunique_strings)) {
    bad_string = unique(shape_string[nonunique_strings])
    n_bad = length(bad_string)
    n_matches = vapply(bad_string[1:min(5, n_bad)], function(shape_string) sum(grepl(paste0("^", 
                                                                                            shape_string), names(pch_table))), integer(1))
    collapsed_names = sprintf("\n* '%s' partially matches %d shape names", 
                              bad_string[1:min(5, n_bad)], n_matches)
    more_problems = if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 
                5, ifelse(n_bad > 6, "s", ""))
    }
    stop("Shape names must be unambiguous:", collapsed_names, 
         more_problems, call. = FALSE)
  }
  unname(pch_table[shape_match])
}

#'@title drawkeyfunction points
#'
#'@description Required internal ggplot object for 3D ggplots.
#'
#'@param data Data.
#'@param params Params.
#'@param size Sizes.
#'@keywords internal
drawkeyfunction_points = function(data,params,size) {
  if (is.character(data$shape)) {
    data$shape = translate_shape_string(data$shape)
  }
  grid::pointsGrob(0.5, 0.5, pch = data$shape, gp = grid::gpar(col = scales::alpha("white",0), 
                                                               fill = scales::alpha("white", 0),
                                                               fontsize = data$size  * .pt + data$stroke * .stroke/2, 
                                                               lwd = 0))
}

#'@title drawkeyfunction lines
#'
#'@description Required internal ggplot object for 3D ggplots.
#'
#'@param data Data.
#'@param params Params.
#'@param size Sizes.
#'@keywords internal
drawkeyfunction_lines = function(data, params, size) {
  if (is.null(data$linetype)) {
    data$linetype = 0
  } else {
    data$linetype[is.na(data$linetype)] = 0
  }
  `%||%` = function(a, b) {
    if (!is.null(a)) a else b
  }
  grid::segmentsGrob(0.1, 0.5, 0.9, 0.5, 
               gp = grid::gpar(col = scales::alpha(data$colour %||% data$fill %||% "black", 0), 
                         lwd = (data$size %||% 0.5) * .pt, lty = data$linetype %||% 1, 
                         lineend = "butt"), 
               arrow = params$arrow)
}

globalVariables(".pt")
globalVariables(".stroke")
