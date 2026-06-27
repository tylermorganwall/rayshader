#' Indent or Raise a Heightmap Surface
#'
#' @description
#' Modifies a heightmap under spatial geometries by a constant or per-feature
#' amount. This is useful when a terrain DEM lacks matching bathymetry and a
#' water surface would otherwise be coincident with the terrain surface.
#'
#' Positive `amount` values lower the surface with `direction = "down"` and
#' raise it with `direction = "up"`. Negative values are allowed and reverse the
#' effect.
#'
#' @param heightmap Height matrix or spatial raster DEM. Matrix inputs require
#' `extent` or cached/attribute extent metadata. Spatial raster inputs return a
#' spatial raster; matrix inputs return a matrix.
#' @param geometry Spatial geometry used to select the affected cells. Supports
#' `sf`, `sfc`, `sfg`, `terra::SpatVector`, and `sp` vector objects.
#' @param amount Default `1`. Constant amount to apply, numeric vector with one
#' value per feature, or a single character string naming a numeric column in
#' `geometry`.
#' @param direction Default `"down"`. Direction to apply `amount`. `"down"`
#' subtracts from `heightmap`; `"up"` adds to `heightmap`.
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
#' @return A modified height matrix or spatial raster, matching `heightmap`.
#' @export
#'
#' @examplesIf length(find.package("sf", quiet = TRUE)) > 0
#' water_poly = sf::st_sfc(
#'   sf::st_polygon(list(rbind(
#'     c(0.2, 0.2), c(0.8, 0.2), c(0.8, 0.8), c(0.2, 0.8), c(0.2, 0.2)
#'   )))
#' )
#' lowered = indent_surface(
#'   matrix(10, 10, 10),
#'   water_poly,
#'   amount = 2,
#'   extent = c(0, 1, 0, 1)
#' )
indent_surface = function(
  heightmap,
  geometry,
  amount = 1,
  direction = c("down", "up"),
  extent = NULL,
  crs = NULL,
  touches = TRUE,
  fun = "max"
) {
  if (!(length(find.package("terra", quiet = TRUE)) > 0)) {
    stop("`terra` package required for indent_surface().", call. = FALSE)
  }
  stopifnot(!missing(heightmap))
  stopifnot(!missing(geometry))

  direction = match.arg(direction)
  touches = validate_indent_surface_touches(touches)
  fun = validate_indent_surface_fun(fun)
  surface = prepare_indent_surface_heightmap(
    heightmap = heightmap,
    extent = extent,
    crs = crs,
    caller = "indent_surface"
  )
  geometry = coerce_indent_surface_geometry(
    geometry = geometry,
    caller = "indent_surface"
  )
  if (nrow(geometry) == 0) {
    return(finalize_indent_surface(surface))
  }
  geometry = assign_indent_surface_amount(
    geometry = geometry,
    amount = amount,
    caller = "indent_surface"
  )
  geometry = align_indent_surface_geometry(
    geometry = geometry,
    template = surface$template,
    caller = "indent_surface"
  )
  amount_raster = rasterize_indent_surface_amount(
    geometry = geometry,
    template = surface$template,
    touches = touches,
    fun = fun,
    caller = "indent_surface"
  )
  apply_indent_surface_amount(
    surface = surface,
    amount_raster = amount_raster,
    direction = direction
  )
}

#' Validate indent surface touches
#'
#' @param touches Default `TRUE`. Whether touched cells should be included.
#'
#' @return A single logical.
#' @keywords internal
validate_indent_surface_touches = function(touches = TRUE) {
  if (!is.logical(touches) || length(touches) != 1 || is.na(touches)) {
    stop("`touches` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  touches
}

#' Validate indent surface reducer
#'
#' @param fun Default `"max"`. Rasterization reducer.
#'
#' @return A reducer accepted by `terra::rasterize()`.
#' @keywords internal
validate_indent_surface_fun = function(fun = "max") {
  if (is.function(fun)) {
    return(fun)
  }
  if (!is.character(fun) || length(fun) != 1 || !nzchar(trimws(fun))) {
    stop(
      "`fun` must be a function or a single non-empty character string.",
      call. = FALSE
    )
  }
  fun
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
    heightmap = terra::rast(heightmap)
    input_type = "raster"
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
    caller = "indent_surface"
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
#' @param caller Default `NULL`. Calling function.
#'
#' @return A `terra::SpatRaster`.
#' @keywords internal
rasterize_indent_surface_amount = function(
  geometry,
  template,
  touches = TRUE,
  fun = "max",
  caller = NULL
) {
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

#' Apply indentation amount to surface
#'
#' @param surface Surface metadata list.
#' @param amount_raster Rasterized amount.
#' @param direction Default `"down"`. Direction to apply.
#'
#' @return Modified heightmap.
#' @keywords internal
apply_indent_surface_amount = function(
  surface,
  amount_raster,
  direction = "down"
) {
  direction_sign = if (identical(direction, "down")) -1 else 1
  if (identical(surface$type, "matrix")) {
    amount_matrix = raster_to_matrix(amount_raster, verbose = FALSE)
    modified_heightmap = surface$heightmap
    valid_cells = is.finite(modified_heightmap) & is.finite(amount_matrix)
    modified_heightmap[valid_cells] =
      modified_heightmap[valid_cells] +
      direction_sign * amount_matrix[valid_cells]
    return(modified_heightmap)
  }

  modified_raster = terra::rast(surface$template)
  height_values = as.numeric(terra::values(surface$heightmap))
  amount_values = as.numeric(terra::values(amount_raster))
  valid_cells = is.finite(height_values) & is.finite(amount_values)
  height_values[valid_cells] =
    height_values[valid_cells] + direction_sign * amount_values[valid_cells]
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
  if (identical(surface$type, "raster")) {
    return(raster::raster(surface$heightmap))
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
