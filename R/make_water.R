#'@title make_water
#'
#'@description Makes the water in the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param waterheight Default `0`. Water level. Either a scalar, a matrix with the same dimensions as `heightmap`, or a spatial raster that can be projected/resampled to the heightmap grid.
#'@param watercolor Default `blue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param water_render_method Default `"contour"`. Water meshing method. `"contour"` clips the water mesh to the flooded region; `"legacy"` uses the previous box/grid renderer.
#'@param heightmap_extent Default `NULL`. Active scene extent for spatial `waterheight` inputs.
#'@param heightmap_crs Default `NULL`. Active scene CRS for spatial `waterheight` inputs.
#'@keywords internal
make_water = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5,
  water_render_method = c("contour", "legacy"),
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  water_render_method = match.arg(water_render_method)
  if (identical(water_render_method, "legacy")) {
    return(make_water_legacy(
      heightmap = heightmap,
      waterheight = waterheight,
      watercolor = watercolor,
      zscale = zscale,
      wateralpha = wateralpha
    ))
  }
  make_water_contour(
    heightmap = heightmap,
    waterheight = waterheight,
    watercolor = watercolor,
    zscale = zscale,
    wateralpha = wateralpha,
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs
  )
}

#'@keywords internal
make_water_contour = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5,
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  heightmap = heightmap / zscale
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  waterheight = normalize_waterheight_matrix(
    waterheight,
    nr = nr,
    nc = nc,
    zscale = zscale,
    caller = "make_water",
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs
  )
  valid_water = is.finite(heightmap) & is.finite(waterheight)
  if (!any(valid_water)) {
    warning(
      "No water rendered--no finite heightmap and water level values overlap."
    )
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }
  flooded = valid_water & heightmap < waterheight
  if (!any(flooded)) {
    warning(format_no_water_warning(heightmap, waterheight, zscale))
    return(invisible(list(
      vertices = list(),
      lines = matrix(nrow = 0, ncol = 3)
    )))
  }

  water_mesh = make_water_mesh_cpp(heightmap, waterheight)
  vertices = water_mesh$vertices
  if (length(vertices) > 0) {
    for (component in vertices) {
      if (is.matrix(component) && nrow(component) > 0) {
        rgl::triangles3d(
          x = component,
          indices = seq_len(nrow(component)),
          color = watercolor,
          alpha = wateralpha,
          lit = FALSE,
          front = "filled",
          back = "filled",
          texture = NULL,
          tag = "water"
        )
      }
    }
  }
  invisible(water_mesh)
}

#'@keywords internal
make_water_legacy = function(
  heightmap,
  waterheight = mean(heightmap),
  watercolor = "lightblue",
  zscale = 1,
  wateralpha = 0.5
) {
  if (
    is.matrix(waterheight) ||
      is_spatial_heightmap_input(waterheight) ||
      length(waterheight) != 1
  ) {
    stop(
      "`water_render_method = \"legacy\"` only supports a scalar `waterdepth`.",
      call. = FALSE
    )
  }
  heightmap = heightmap / zscale
  na_matrix = is.na(heightmap)
  nr = nrow(heightmap)
  nc = ncol(heightmap)

  waterheight = waterheight / zscale
  if (all(heightmap >= waterheight, na.rm = TRUE)) {
    warning(
      "No water rendered--all elevations above or equal to water level. Range of heights: ",
      min(heightmap, na.rm = TRUE) * zscale,
      "-",
      max(heightmap, na.rm = TRUE) * zscale,
      ". Depth specified: ",
      waterheight * zscale
    )
  } else {
    heightlist = make_water_cpp(heightmap, na_matrix, waterheight)
    if (length(heightlist) > 0) {
      fullsides = do.call(rbind, heightlist)
      fullsides[, 3] = -fullsides[, 3]
      fullsides[, 1] = fullsides[, 1] - 1
      fullsides[, 3] = fullsides[, 3]

      fullsides[, 1] = fullsides[, 1] - (nr - 1) / 2
      fullsides[, 3] = fullsides[, 3] - (nc - 1) / 2
    }
    nr1 = nr - 1
    nc1 = nc - 1

    if (all(!na_matrix)) {
      vertices = rbind(
        matrix(
          c(
            -nr1 / 2,
            nr1 / 2,
            -nr1 / 2,
            waterheight,
            waterheight,
            waterheight,
            nc1 / 2,
            -nc1 / 2,
            -nc1 / 2
          ),
          nrow = 3L,
          ncol = 3L
        ),
        matrix(
          c(
            -nr1 / 2,
            nr1 / 2,
            nr1 / 2,
            waterheight,
            waterheight,
            waterheight,
            nc1 / 2,
            nc1 / 2,
            -nc1 / 2
          ),
          nrow = 3L,
          ncol = 3L
        )
      )
      indices = seq_len(6L)
      rgl::triangles3d(
        x = vertices,
        indices = indices,
        color = watercolor,
        alpha = wateralpha,
        lit = FALSE,
        front = "filled",
        back = "cull",
        texture = NULL,
        tag = "water"
      )
      if (length(heightlist) > 0) {
        indices = rev(seq_len(nrow(fullsides)))
        rgl::triangles3d(
          fullsides,
          indices = indices,
          lit = FALSE,
          color = watercolor,
          alpha = wateralpha,
          front = "filled",
          back = "cull",
          depth_test = "less",
          texture = NULL,
          tag = "water"
        )
      }
    } else {
      if (length(heightlist) > 0) {
        indices = rev(seq_len(nrow(fullsides)))
        rgl::triangles3d(
          fullsides,
          indices = indices,
          lit = FALSE,
          color = watercolor,
          alpha = wateralpha,
          front = "fill",
          back = "culled",
          texture = NULL,
          tag = "water"
        )
      }

      basemat = matrix(waterheight, nr, nc)
      basemat[is.na(heightmap)] = NA
      ray_surface = generate_surface(basemat, zscale = 1)

      rgl::triangles3d(
        x = ray_surface$verts,
        indices = ray_surface$inds,
        texcoords = ray_surface$texcoords,
        color = watercolor,
        alpha = wateralpha,
        back = "culled",
        front = "fill",
        lit = FALSE,
        texture = NULL,
        tag = "water"
      )
    }
  }
  invisible(NULL)
}

#'@keywords internal
normalize_waterheight_matrix = function(
  waterheight,
  nr,
  nc,
  zscale,
  caller,
  heightmap_extent = NULL,
  heightmap_crs = NULL
) {
  if (is.matrix(waterheight)) {
    if (!is.numeric(waterheight)) {
      stop("`waterdepth` must be numeric.", call. = FALSE)
    }
    if (!all(dim(waterheight) == c(nr, nc))) {
      stop(
        sprintf(
          "`waterdepth` matrix must have dimensions %i x %i to match `heightmap`.",
          nr,
          nc
        ),
        call. = FALSE
      )
    }
    return(waterheight / zscale)
  }
  if (is_spatial_heightmap_input(waterheight)) {
    waterheight = resolve_spatial_waterheight_matrix(
      waterheight = waterheight,
      nr = nr,
      nc = nc,
      heightmap_extent = heightmap_extent,
      heightmap_crs = heightmap_crs,
      caller = caller
    )
    return(waterheight / zscale)
  }
  if (
    !is.numeric(waterheight) || length(waterheight) != 1 || is.na(waterheight)
  ) {
    stop(
      sprintf(
        "`waterdepth` must be a scalar, a matrix, or a spatial raster for %s().",
        caller
      ),
      call. = FALSE
    )
  }
  matrix(waterheight / zscale, nrow = nr, ncol = nc)
}

#'@keywords internal
resolve_spatial_waterheight_matrix = function(
  waterheight,
  nr,
  nc,
  heightmap_extent = NULL,
  heightmap_crs = NULL,
  caller = NULL
) {
  water_raster = coerce_spatial_waterheight_raster(waterheight)
  target_template = build_waterheight_template(
    nr = nr,
    nc = nc,
    heightmap_extent = heightmap_extent,
    heightmap_crs = heightmap_crs,
    caller = caller
  )
  source_crs = tryCatch(terra::crs(water_raster), error = function(e) "")
  target_crs = tryCatch(terra::crs(target_template), error = function(e) "")
  source_has_crs = is.character(source_crs) &&
    length(source_crs) &&
    nzchar(source_crs[1])
  target_has_crs = is.character(target_crs) &&
    length(target_crs) &&
    nzchar(target_crs[1])

  if (target_has_crs && !source_has_crs) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` inputs must have a CRS when the active heightmap has a CRS."
      ),
      call. = FALSE
    )
  }

  aligned_raster = tryCatch(
    {
      if (target_has_crs) {
        same_crs = isTRUE(tryCatch(
          scene_crs_equal(source_crs, target_crs),
          error = function(e) FALSE
        ))
        if (same_crs) {
          terra::resample(water_raster, target_template, method = "bilinear")
        } else {
          terra::project(water_raster, target_template, method = "bilinear")
        }
      } else {
        terra::resample(water_raster, target_template, method = "bilinear")
      }
    },
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not project/resample spatial `waterdepth` to the active heightmap grid: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  waterheight_matrix = raster_to_matrix(aligned_raster, verbose = FALSE)
  if (!all(dim(waterheight_matrix) == c(nr, nc))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` could not be aligned to the active heightmap grid."
      ),
      call. = FALSE
    )
  }
  waterheight_matrix
}

#'@keywords internal
coerce_spatial_waterheight_raster = function(waterheight) {
  if (is.character(waterheight)) {
    waterheight = terra::rast(waterheight)
  } else if (
    inherits(waterheight, c("RasterLayer", "RasterBrick", "RasterStack"))
  ) {
    waterheight = terra::rast(waterheight)
  }
  if (!inherits(waterheight, "SpatRaster")) {
    stop("`waterdepth` must resolve to a spatial raster.", call. = FALSE)
  }
  if (terra::nlyr(waterheight) > 1) {
    warning("`waterdepth` has multiple layers; using the first layer.")
    waterheight = waterheight[[1]]
  }
  waterheight
}

#'@keywords internal
build_waterheight_template = function(
  nr,
  nc,
  heightmap_extent = NULL,
  heightmap_crs = NULL,
  caller = NULL
) {
  if (is.null(heightmap_extent)) {
    heightmap_extent = get_scene_extent(default = NULL)
  }
  if (is.null(heightmap_extent)) {
    heightmap_extent = get_hillshade_extent(default = NULL)
  }
  if (is.null(heightmap_extent)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Spatial `waterdepth` inputs require an active heightmap extent."
      ),
      call. = FALSE
    )
  }
  heightmap_extent = tryCatch(
    get_extent(heightmap_extent),
    error = function(e) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "Could not interpret the active heightmap extent for spatial `waterdepth`: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  target_template = terra::rast(
    nrows = nc,
    ncols = nr,
    xmin = heightmap_extent["xmin"],
    xmax = heightmap_extent["xmax"],
    ymin = heightmap_extent["ymin"],
    ymax = heightmap_extent["ymax"]
  )
  if (is.null(heightmap_crs)) {
    heightmap_crs = get_scene_target_crs(
      extent = heightmap_extent,
      caller = caller
    )
  }
  heightmap_crs = waterheight_terra_crs(heightmap_crs)
  if (!is.null(heightmap_crs)) {
    terra::crs(target_template) = heightmap_crs
  }
  target_template
}

#'@keywords internal
waterheight_terra_crs = function(crs) {
  parsed_crs = try_parse_scene_crs(crs)
  if (!is.null(parsed_crs) && !is.na(parsed_crs)) {
    return(parsed_crs$wkt)
  }
  if (is.character(crs) && length(crs) && nzchar(trimws(crs[1]))) {
    return(crs[1])
  }
  NULL
}

#'@keywords internal
format_no_water_warning = function(heightmap, waterheight, zscale) {
  height_range = range(heightmap, na.rm = TRUE) * zscale
  water_range = range(waterheight, na.rm = TRUE) * zscale
  if (diff(water_range) == 0) {
    return(paste0(
      "No water rendered--all elevations above or equal to water level. Range of heights: ",
      height_range[1],
      "-",
      height_range[2],
      ". Depth specified: ",
      water_range[1]
    ))
  }
  paste0(
    "No water rendered--all elevations above or equal to water levels. Range of heights: ",
    height_range[1],
    "-",
    height_range[2],
    ". Water level range specified: ",
    water_range[1],
    "-",
    water_range[2]
  )
}

#'@keywords internal
make_waterlines_from_mesh = function(
  water_mesh,
  linecolor = "grey40",
  alpha = 1,
  linewidth = 2,
  antialias = FALSE
) {
  if (is.null(water_mesh) || is.null(water_mesh$lines)) {
    return(invisible(NULL))
  }
  segmentlist = water_mesh$lines
  if (!is.matrix(segmentlist) || nrow(segmentlist) == 0) {
    return(invisible(NULL))
  }
  rgl::segments3d(
    segmentlist,
    color = linecolor,
    lwd = linewidth,
    alpha = alpha,
    depth_mask = TRUE,
    line_antialias = antialias,
    depth_test = "lequal",
    tag = "waterlines",
    lit = FALSE
  )
  invisible(NULL)
}
