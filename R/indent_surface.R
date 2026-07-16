#' Shift a Heightmap Surface
#'
#' @description
#' Shifts a heightmap under spatial geometries by a constant or per-feature
#' amount. This is useful when terrain needs to be lowered below water or raised
#' beneath structures.
#'
#' Positive `amount` values raise the surface and negative values lower it.
#'
#' @param heightmap Height matrix or spatial raster DEM. Matrix inputs require
#' `extent` or cached or attribute extent metadata. Spatial raster inputs return
#' a spatial raster; matrix inputs return a matrix.
#' @param geometry Spatial geometry used to select the affected cells. Supports
#' `sf`, `sfc`, `sfg`, `terra::SpatVector`, and `sp` vector objects.
#' @param amount Default `1`. Constant amount to apply, numeric vector with one
#' value per feature, or a single character string naming a numeric column in
#' `geometry`. Positive values raise terrain, negative values lower terrain,
#' and `NA` values skip their features.
#' @param transition Default `0`. Distance over which `amount` transitions from
#' the polygon edge to the full shift. `0` applies the full amount abruptly to
#' all selected cells.
#' @param transition_units Default `c("map", "cells")`. Units for `transition`.
#' `"map"` uses meters for longitude and latitude rasters, the CRS linear units
#' for projected rasters, and coordinate units for CRS-less rasters. `"cells"`
#' measures distance in unit-width and unit-height raster cells, independent of
#' CRS and spatial resolution.
#' @param extent Default `NULL`. Spatial extent for matrix `heightmap` inputs.
#' Ignored for spatial raster inputs.
#' @param crs Default `NULL`. CRS for matrix `heightmap` inputs or CRS-less
#' spatial raster inputs. If a spatial raster already has a CRS, explicit `crs`
#' must match it.
#' @param touches Default `TRUE`. Passed to `terra::rasterize()`. If `TRUE`,
#' cells touched by polygons or lines are modified.
#' @param fun Default `"max"`. Rasterization reducer passed to
#' `terra::rasterize()` for cells intersected by multiple features.
#'
#' @details
#' In map mode, transition distance is measured by `terra::distance()`. The
#' center-to-boundary inset is one-half of the smaller raster resolution. In
#' cell mode, distance is calculated on a temporary unit-resolution raster and
#' uses a half-cell inset of `0.5`. A polygon that fills the raster has no
#' internal polygon edge, so the raster boundary does not reduce its shift.
#'
#' @return A shifted height matrix or `terra::SpatRaster`. Legacy `Raster*`
#' inputs are returned as `terra::SpatRaster` objects.
#' @export
#'
#' @examplesIf length(find.package("sf", quiet = TRUE)) > 0
#' water_poly = sf::st_sfc(
#'   sf::st_polygon(list(rbind(
#'     c(0.2, 0.2), c(0.8, 0.2), c(0.8, 0.8), c(0.2, 0.8), c(0.2, 0.2)
#'   ))),
#'   crs = 4326
#' )
#' lowered = shift_terrain(
#'   matrix(10, 10, 10),
#'   water_poly,
#'   amount = -2,
#'   extent = c(0, 1, 0, 1)
#' )
shift_terrain = function(
  heightmap,
  geometry,
  amount = 1,
  transition = 0,
  transition_units = c("map", "cells"),
  extent = NULL,
  crs = NULL,
  touches = TRUE,
  fun = "max"
) {
  if (!(length(find.package("terra", quiet = TRUE)) > 0)) {
    stop("`terra` package required for shift_terrain().", call. = FALSE)
  }
  if (missing(heightmap)) {
    stop("`heightmap` must be supplied.", call. = FALSE)
  }
  if (missing(geometry)) {
    stop("`geometry` must be supplied.", call. = FALSE)
  }

  if (
    !is.numeric(transition) ||
      length(transition) != 1 ||
      is.na(transition) ||
      !is.finite(transition) ||
      transition < 0
  ) {
    stop(
      "`transition` must be a single non-negative finite number.",
      call. = FALSE
    )
  }
  transition_units = match.arg(transition_units)
  if (!is.logical(touches) || length(touches) != 1 || is.na(touches)) {
    stop("`touches` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (
    !is.function(fun) &&
      (!is.character(fun) || length(fun) != 1 || !nzchar(trimws(fun)))
  ) {
    stop(
      "`fun` must be a function or a single non-empty character string.",
      call. = FALSE
    )
  }
  surface = prepare_indent_surface_heightmap(
    heightmap = heightmap,
    extent = extent,
    crs = crs,
    caller = "shift_terrain"
  )
  geometry = coerce_indent_surface_geometry(
    geometry = geometry,
    caller = "shift_terrain"
  )
  if (nrow(geometry) == 0) {
    return(finalize_indent_surface(surface))
  }
  geometry = assign_indent_surface_amount(
    geometry = geometry,
    amount = amount,
    caller = "shift_terrain"
  )
  geometry = align_indent_surface_geometry(
    geometry = geometry,
    template = surface$template,
    caller = "shift_terrain"
  )
  amount_raster = rasterize_indent_surface_amount(
    geometry = geometry,
    template = surface$template,
    touches = touches,
    fun = fun,
    transition = transition,
    transition_units = transition_units,
    caller = "shift_terrain"
  )
  apply_indent_surface_amount(
    surface = surface,
    amount_raster = amount_raster
  )
}

#' Prepare heightmap for surface indentation
#'
#' @param heightmap Height matrix or spatial raster DEM.
#' @param extent Default `NULL`. Matrix heightmap extent.
#' @param crs Default `NULL`. Matrix heightmap CRS.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Surface metadata list.
#' @keywords internal
prepare_indent_surface_heightmap = function(
  heightmap,
  extent = NULL,
  crs = NULL,
  caller = NULL
) {
  if (is.matrix(heightmap)) {
    return(prepare_indent_surface_matrix(
      heightmap = heightmap,
      extent = extent,
      crs = crs,
      caller = caller
    ))
  }
  if (is_spatial_heightmap_input(heightmap)) {
    return(prepare_indent_surface_spatial(
      heightmap = heightmap,
      crs = crs,
      caller = caller
    ))
  }
  stop(
    paste0(
      format_render_caller_prefix(caller),
      "`heightmap` must be a numeric matrix or supported spatial raster."
    ),
    call. = FALSE
  )
}

#' Prepare matrix heightmap for surface indentation
#'
#' @param heightmap Height matrix.
#' @param extent Default `NULL`. Matrix heightmap extent.
#' @param crs Default `NULL`. Matrix heightmap CRS.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Surface metadata list.
#' @keywords internal
prepare_indent_surface_matrix = function(
  heightmap,
  extent = NULL,
  crs = NULL,
  caller = NULL
) {
  if (!is.numeric(heightmap)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`heightmap` must be numeric."
      ),
      call. = FALSE
    )
  }
  if (nrow(heightmap) < 1 || ncol(heightmap) < 1) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`heightmap` must have at least one row and one column."
      ),
      call. = FALSE
    )
  }
  if (is.null(extent)) {
    extent = attr(heightmap, "extent", exact = TRUE)
  }
  resolved_extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = caller
  )
  extent_values = tryCatch(
    get_extent(resolved_extent),
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not interpret `extent`: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  target_crs = resolve_indent_surface_matrix_crs(
    crs = crs,
    extent = extent,
    resolved_extent = resolved_extent,
    heightmap = heightmap
  )
  template = terra::rast(
    nrows = ncol(heightmap),
    ncols = nrow(heightmap),
    xmin = extent_values["xmin"],
    xmax = extent_values["xmax"],
    ymin = extent_values["ymin"],
    ymax = extent_values["ymax"]
  )
  if (is.null(target_crs)) {
    terra::crs(template) = ""
  } else {
    terra::crs(template) = target_crs
  }
  list(
    type = "matrix",
    heightmap = heightmap,
    template = template
  )
}

#' Prepare spatial heightmap for surface indentation
#'
#' @param heightmap Spatial raster DEM.
#' @param crs Default `NULL`. CRS for CRS-less spatial rasters.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Surface metadata list.
#' @keywords internal
prepare_indent_surface_spatial = function(
  heightmap,
  crs = NULL,
  caller = NULL
) {
  input_type = "spatraster"
  if (is.character(heightmap)) {
    heightmap = terra::rast(heightmap)
    input_type = "spatraster"
  } else if (inherits(heightmap, "SpatRaster")) {
    input_type = "spatraster"
  } else if (
    inherits(heightmap, c("RasterLayer", "RasterBrick", "RasterStack"))
  ) {
    warn_raster_support_deprecated()
    heightmap = terra::rast(heightmap)
    input_type = "spatraster"
  }
  if (!inherits(heightmap, "SpatRaster")) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`heightmap` must resolve to a spatial raster."
      ),
      call. = FALSE
    )
  }
  if (terra::nlyr(heightmap) > 1) {
    warning("`heightmap` has multiple layers; using the first layer.")
    heightmap = heightmap[[1]]
  }
  heightmap = heightmap[[1]]
  heightmap = resolve_indent_surface_spatial_crs(
    heightmap = heightmap,
    crs = crs,
    caller = caller
  )
  template = terra::rast(heightmap)
  list(
    type = input_type,
    heightmap = heightmap,
    template = template
  )
}

#' Resolve matrix heightmap CRS for surface indentation
#'
#' @param crs Default `NULL`. Explicit CRS.
#' @param extent Default `NULL`. Explicit extent.
#' @param resolved_extent Resolved extent.
#' @param heightmap Height matrix.
#'
#' @return CRS string or `NULL`.
#' @keywords internal
resolve_indent_surface_matrix_crs = function(
  crs = NULL,
  extent = NULL,
  resolved_extent = NULL,
  heightmap = NULL
) {
  target_crs = indent_surface_terra_crs(crs)
  if (!is.null(target_crs)) {
    return(target_crs)
  }
  target_crs = indent_surface_terra_crs(attr(heightmap, "crs", exact = TRUE))
  if (!is.null(target_crs)) {
    return(target_crs)
  }
  target_crs = indent_surface_terra_crs(infer_spatialize_image_extent_crs(
    extent
  ))
  if (!is.null(target_crs)) {
    return(target_crs)
  }
  target_crs = indent_surface_terra_crs(get_scene_target_crs(
    extent = resolved_extent,
    heightmap = heightmap,
    caller = "shift_terrain"
  ))
  if (!is.null(target_crs)) {
    return(target_crs)
  }
  NULL
}

#' Resolve spatial heightmap CRS for surface indentation
#'
#' @param heightmap Spatial raster DEM.
#' @param crs Default `NULL`. Explicit CRS.
#' @param caller Default `NULL`. Calling function.
#'
#' @return Spatial raster DEM.
#' @keywords internal
resolve_indent_surface_spatial_crs = function(
  heightmap,
  crs = NULL,
  caller = NULL
) {
  explicit_crs = indent_surface_terra_crs(crs)
  existing_crs = tryCatch(terra::crs(heightmap), error = function(e) "")
  existing_has_crs = indent_surface_has_crs(existing_crs)
  if (is.null(explicit_crs)) {
    return(heightmap)
  }
  if (!existing_has_crs) {
    terra::crs(heightmap) = explicit_crs
    return(heightmap)
  }
  if (
    !isTRUE(tryCatch(
      terra::same.crs(existing_crs, explicit_crs),
      error = function(e) FALSE
    ))
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Explicit `crs` does not match the CRS on spatial `heightmap`."
      ),
      call. = FALSE
    )
  }
  heightmap
}

#' Coerce surface indentation geometry
#'
#' @param geometry Spatial geometry.
#' @param caller Default `NULL`. Calling function.
#'
#' @return A `terra::SpatVector`.
#' @keywords internal
coerce_indent_surface_geometry = function(geometry, caller = NULL) {
  if (inherits(geometry, "SpatVector")) {
    return(geometry)
  }
  if (inherits(geometry, c("sf", "sfc", "sfg"))) {
    if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`sf` package required for sf geometry inputs."
        ),
        call. = FALSE
      )
    }
    if (inherits(geometry, "sfg")) {
      geometry = sf::st_sfc(geometry)
    }
    if (inherits(geometry, "sfc")) {
      geometry = sf::st_sf(geometry = geometry)
    }
  }
  geometry = tryCatch(
    terra::vect(geometry),
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`geometry` must be a supported spatial vector object: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  if (!inherits(geometry, "SpatVector")) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`geometry` must resolve to a spatial vector object."
      ),
      call. = FALSE
    )
  }
  geometry
}

#' Assign indentation amount to geometry
#'
#' @param geometry A `terra::SpatVector`.
#' @param amount Amount input.
#' @param caller Default `NULL`. Calling function.
#'
#' @return A `terra::SpatVector` with internal amount field.
#' @keywords internal
assign_indent_surface_amount = function(
  geometry,
  amount = 1,
  caller = NULL
) {
  n_features = nrow(geometry)
  if (is.character(amount) && length(amount) == 1) {
    geometry_data = as.data.frame(geometry)
    if (!amount %in% names(geometry_data)) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`amount` column `",
          amount,
          "` was not found in `geometry`."
        ),
        call. = FALSE
      )
    }
    amount_values = geometry_data[[amount]]
  } else if (is.numeric(amount) && length(amount) == 1) {
    amount_values = rep(amount, n_features)
  } else if (is.numeric(amount) && length(amount) == n_features) {
    amount_values = amount
  } else {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`amount` must be a numeric scalar, a numeric vector matching the ",
        "number of features in `geometry`, or a single column name."
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(amount_values)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`amount` values must be numeric."
      ),
      call. = FALSE
    )
  }
  amount_values = as.numeric(amount_values)
  if (any(!is.na(amount_values) & !is.finite(amount_values))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`amount` values must be finite or `NA`."
      ),
      call. = FALSE
    )
  }
  amount_field = "rayshader_surface_amount"
  geometry[[amount_field]] = amount_values
  geometry
}

#' Align geometry to surface indentation template
#'
#' @param geometry A `terra::SpatVector`.
#' @param template A `terra::SpatRaster`.
#' @param caller Default `NULL`. Calling function.
#'
#' @return A `terra::SpatVector`.
#' @keywords internal
align_indent_surface_geometry = function(
  geometry,
  template,
  caller = NULL
) {
  target_crs = tryCatch(terra::crs(template), error = function(e) "")
  source_crs = tryCatch(terra::crs(geometry), error = function(e) "")
  target_has_crs = indent_surface_has_crs(target_crs)
  source_has_crs = indent_surface_has_crs(source_crs)

  if (target_has_crs && !source_has_crs) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`geometry` must have a CRS when `heightmap` has a CRS."
      ),
      call. = FALSE
    )
  }
  if (target_has_crs && source_has_crs) {
    same_crs = isTRUE(tryCatch(
      terra::same.crs(source_crs, target_crs),
      error = function(e) FALSE
    ))
    if (!same_crs) {
      geometry = tryCatch(
        terra::project(geometry, target_crs),
        error = function(e) {
          stop(
            paste0(
              format_render_caller_prefix(caller),
              "Could not project `geometry` to the `heightmap` CRS: ",
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    }
  }
  geometry
}

#' Rasterize surface indentation amount
#'
#' @param geometry A `terra::SpatVector`.
#' @param template A `terra::SpatRaster`.
#' @param touches Default `TRUE`. Whether touched cells should be included.
#' @param fun Default `"max"`. Rasterization reducer.
#' @param transition Default `0`. Transition distance.
#' @param transition_units Default `"map"`. Transition distance units.
#' @param caller Default `NULL`. Calling function.
#'
#' @return A `terra::SpatRaster`.
#' @keywords internal
rasterize_indent_surface_amount = function(
  geometry,
  template,
  touches = TRUE,
  fun = "max",
  transition = 0,
  transition_units = "map",
  caller = NULL
) {
  if (transition > 0) {
    return(rasterize_indent_surface_transition_amount(
      geometry = geometry,
      template = template,
      touches = touches,
      fun = fun,
      transition = transition,
      transition_units = transition_units,
      caller = caller
    ))
  }
  tryCatch(
    terra::rasterize(
      geometry,
      template,
      field = "rayshader_surface_amount",
      fun = fun,
      background = NA_real_,
      touches = touches
    ),
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not rasterize `geometry`: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

#' Rasterize transitioned surface indentation amount
#'
#' @param geometry A `terra::SpatVector`.
#' @param template A `terra::SpatRaster`.
#' @param touches Default `TRUE`. Whether touched cells should be included.
#' @param fun Default `"max"`. Rasterization reducer.
#' @param transition Transition distance.
#' @param transition_units Default `"map"`. Transition distance units.
#' @param caller Default `NULL`. Calling function.
#'
#' @return A `terra::SpatRaster`.
#' @keywords internal
rasterize_indent_surface_transition_amount = function(
  geometry,
  template,
  touches = TRUE,
  fun = "max",
  transition,
  transition_units = "map",
  caller = NULL
) {
  amount_values = as.data.frame(geometry)[["rayshader_surface_amount"]]
  finite_features = which(is.finite(amount_values))
  empty_raster = terra::rast(template)
  terra::values(empty_raster) = NA_real_
  if (!length(finite_features)) {
    return(empty_raster)
  }

  finite_amounts = amount_values[finite_features]
  if (length(unique(finite_amounts)) == 1) {
    geometry_union = tryCatch(
      terra::aggregate(geometry[finite_features, ]),
      error = function(e) {
        stop(
          paste0(
            format_render_caller_prefix(caller),
            "Could not combine `geometry`: ",
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
    feature_mask = rasterize_indent_surface_feature_mask(
      geometry = geometry_union,
      template = template,
      touches = touches,
      caller = caller
    )
    transition_factor = indent_surface_transition_factor(
      feature_mask = feature_mask,
      transition = transition,
      transition_units = transition_units
    )
    transition_values = finite_amounts[1] * transition_factor
    result = terra::rast(template)
    terra::values(result) = transition_values
    return(result)
  }

  use_fast_max = identical(fun, "max") || identical(fun, base::max)
  result_values = rep(NA_real_, terra::ncell(template))
  cell_indices = vector("list", length(finite_features))
  cell_amounts = vector("list", length(finite_features))

  for (finite_index in seq_along(finite_features)) {
    feature_index = finite_features[finite_index]
    feature_mask = rasterize_indent_surface_feature_mask(
      geometry = geometry[feature_index, ],
      template = template,
      touches = touches,
      caller = caller
    )
    transition_factor = indent_surface_transition_factor(
      feature_mask = feature_mask,
      transition = transition,
      transition_units = transition_units
    )
    covered_cells = which(is.finite(transition_factor))
    if (!length(covered_cells)) {
      next
    }
    transitioned_amount = amount_values[feature_index] *
      transition_factor[covered_cells]

    if (use_fast_max) {
      existing_values = result_values[covered_cells]
      empty_cells = is.na(existing_values)
      existing_values[empty_cells] = transitioned_amount[empty_cells]
      existing_values[!empty_cells] = pmax(
        existing_values[!empty_cells],
        transitioned_amount[!empty_cells]
      )
      result_values[covered_cells] = existing_values
    } else {
      cell_indices[[finite_index]] = covered_cells
      cell_amounts[[finite_index]] = transitioned_amount
    }
  }

  if (!use_fast_max) {
    populated = lengths(cell_indices) > 0
    if (!any(populated)) {
      return(empty_raster)
    }
    all_cells = unlist(cell_indices[populated], use.names = FALSE)
    all_amounts = unlist(cell_amounts[populated], use.names = FALSE)
    grouped_amounts = split(all_amounts, all_cells)
    reducer = if (is.function(fun)) {
      fun
    } else if (identical(fun, "modal") || identical(fun, "mode")) {
      function(values) {
        unique_values = unique(values)
        unique_values[which.max(tabulate(match(values, unique_values)))]
      }
    } else {
      tryCatch(
        match.fun(fun),
        error = function(e) {
          stop(
            paste0(
              format_render_caller_prefix(caller),
              "Could not resolve `fun`: ",
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    }
    reduced_amounts = vapply(
      grouped_amounts,
      function(values) {
        reduced = tryCatch(
          reducer(values),
          error = function(e) {
            stop(
              paste0(
                format_render_caller_prefix(caller),
                "`fun` failed while combining transitioned amounts: ",
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
        if (!is.numeric(reduced) || length(reduced) != 1) {
          stop(
            paste0(
              format_render_caller_prefix(caller),
              "`fun` must return one numeric value per raster cell."
            ),
            call. = FALSE
          )
        }
        as.numeric(reduced)
      },
      numeric(1)
    )
    result_values[as.integer(names(reduced_amounts))] = reduced_amounts
  }

  result = terra::rast(template)
  terra::values(result) = result_values
  result
}

#' Rasterize one indentation feature mask
#'
#' @param geometry A single-feature `terra::SpatVector`.
#' @param template A `terra::SpatRaster`.
#' @param touches Default `TRUE`. Whether touched cells should be included.
#' @param caller Default `NULL`. Calling function.
#'
#' @return A `terra::SpatRaster`.
#' @keywords internal
rasterize_indent_surface_feature_mask = function(
  geometry,
  template,
  touches = TRUE,
  caller = NULL
) {
  tryCatch(
    terra::rasterize(
      geometry,
      template,
      field = 1,
      background = NA_real_,
      touches = touches
    ),
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not rasterize `geometry`: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

#' Calculate indentation transition factor
#'
#' @param feature_mask Rasterized single-feature mask.
#' @param transition Transition distance.
#' @param transition_units Default `"map"`. Transition distance units.
#'
#' @return Numeric vector of scale factors matching raster cell order.
#' @keywords internal
indent_surface_transition_factor = function(
  feature_mask,
  transition,
  transition_units = "map"
) {
  inside = is.finite(terra::values(feature_mask))
  transition_factor = rep(NA_real_, length(inside))
  if (!any(inside)) {
    return(transition_factor)
  }
  inside_matrix = matrix(
    inside,
    nrow = terra::nrow(feature_mask),
    ncol = terra::ncol(feature_mask),
    byrow = TRUE
  )
  edge_matrix = indent_surface_edge_cells(inside_matrix)
  if (!any(edge_matrix)) {
    transition_factor[inside] = 1
    return(transition_factor)
  }
  if (identical(transition_units, "cells")) {
    distance_template = terra::rast(
      nrows = terra::nrow(feature_mask),
      ncols = terra::ncol(feature_mask),
      xmin = 1000,
      xmax = 1000 + terra::ncol(feature_mask),
      ymin = 1000,
      ymax = 1000 + terra::nrow(feature_mask)
    )
    terra::crs(distance_template) = ""
    edge_cell_inset = 0.5
  } else {
    distance_template = terra::rast(feature_mask)
    distance_has_crs = indent_surface_has_crs(terra::crs(feature_mask))
    if (distance_has_crs && isTRUE(terra::is.lonlat(feature_mask))) {
      center = matrix(
        c(
          mean(c(terra::xmin(feature_mask), terra::xmax(feature_mask))),
          mean(c(terra::ymin(feature_mask), terra::ymax(feature_mask)))
        ),
        nrow = 1
      )
      raster_resolution = terra::res(feature_mask)
      x_neighbor = center + matrix(c(raster_resolution[1], 0), nrow = 1)
      y_neighbor = center + matrix(c(0, raster_resolution[2]), nrow = 1)
      center_distances = c(
        terra::distance(center, x_neighbor, lonlat = TRUE),
        terra::distance(center, y_neighbor, lonlat = TRUE)
      )
      edge_cell_inset = min(center_distances) / 2
    } else {
      edge_cell_inset = min(terra::res(feature_mask)) / 2
    }
  }
  edge_values = rep(NA_real_, length(inside))
  edge_values[as.vector(t(edge_matrix))] = 1
  terra::values(distance_template) = edge_values
  distance_raster = if (indent_surface_has_crs(terra::crs(distance_template))) {
    terra::distance(distance_template)
  } else {
    suppressWarnings(terra::distance(distance_template))
  }
  cell_distance = terra::values(distance_raster)
  transition_factor[inside] = pmin(
    (cell_distance[inside] + edge_cell_inset) / transition,
    1
  )
  transition_factor
}

#' Locate rasterized indentation edge cells
#'
#' @param inside_matrix Logical matrix indicating feature-covered cells.
#'
#' @return Logical matrix indicating edge cells.
#' @keywords internal
indent_surface_edge_cells = function(inside_matrix) {
  nr = nrow(inside_matrix)
  nc = ncol(inside_matrix)
  padded = matrix(TRUE, nrow = nr + 2L, ncol = nc + 2L)
  padded[seq_len(nr) + 1L, seq_len(nc) + 1L] = inside_matrix
  interior = inside_matrix
  for (row_offset in -1:1) {
    for (col_offset in -1:1) {
      if (row_offset == 0 && col_offset == 0) {
        next
      }
      neighbor = padded[
        seq_len(nr) + 1L + row_offset,
        seq_len(nc) + 1L + col_offset,
        drop = FALSE
      ]
      interior = interior & neighbor
    }
  }
  inside_matrix & !interior
}

#' Apply indentation amount to surface
#'
#' @param surface Surface metadata list.
#' @param amount_raster Rasterized amount.
#' @return Modified heightmap.
#' @keywords internal
apply_indent_surface_amount = function(
  surface,
  amount_raster
) {
  if (identical(surface$type, "matrix")) {
    amount_matrix = raster_to_matrix(amount_raster, verbose = FALSE)
    modified_heightmap = surface$heightmap
    valid_cells = is.finite(modified_heightmap) & is.finite(amount_matrix)
    modified_heightmap[valid_cells] =
      modified_heightmap[valid_cells] +
      amount_matrix[valid_cells]
    return(modified_heightmap)
  }

  modified_raster = terra::rast(surface$template)
  height_values = as.numeric(terra::values(surface$heightmap))
  amount_values = as.numeric(terra::values(amount_raster))
  valid_cells = is.finite(height_values) & is.finite(amount_values)
  height_values[valid_cells] =
    height_values[valid_cells] + amount_values[valid_cells]
  terra::values(modified_raster) = height_values
  names(modified_raster) = names(surface$heightmap)
  finalize_indent_surface(list(
    type = surface$type,
    heightmap = modified_raster,
    template = surface$template
  ))
}

#' Finalize surface indentation output
#'
#' @param surface Surface metadata list.
#'
#' @return Heightmap output.
#' @keywords internal
finalize_indent_surface = function(surface) {
  if (identical(surface$type, "matrix")) {
    return(surface$heightmap)
  }
  surface$heightmap
}

#' Convert CRS input for surface indentation
#'
#' @param crs Default `NULL`. CRS input.
#'
#' @return CRS string or `NULL`.
#' @keywords internal
indent_surface_terra_crs = function(crs = NULL) {
  parsed_crs = tryCatch(try_parse_scene_crs(crs), error = function(e) NULL)
  if (!is.null(parsed_crs) && !is.na(parsed_crs)) {
    return(parsed_crs$wkt)
  }
  if (is.character(crs) && length(crs) && nzchar(trimws(crs[1]))) {
    return(crs[1])
  }
  NULL
}

#' Check CRS value for surface indentation
#'
#' @param crs Default `NULL`. CRS value.
#'
#' @return `TRUE` if the CRS is non-empty.
#' @keywords internal
indent_surface_has_crs = function(crs = NULL) {
  is.character(crs) && length(crs) && nzchar(trimws(crs[1]))
}
