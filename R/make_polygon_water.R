#' @param heightmap Terrain height matrix in elevation units.
#' @param water_input Water level, matrix, spatial raster, or polygon footprints.
#' @param zscale Elevation units per scene unit.
#' @param heightmap_extent Scene extent, or `NULL` for nonspatial inputs.
#' @param heightmap_crs Scene CRS, or `NULL` for nonspatial inputs.
#' @param water_elevation Default `NULL`. Supplied polygon elevations.
#' @param water_surface_tolerance Default `NULL`. Coincidence tolerance in elevation units; `NULL` uses scene precision.
#' @param water_color Default `"lightblue"`. Water color.
#' @param water_alpha Default `0.5`. Water opacity.
#' @return Water triangle components and top boundary segments, invisibly.
#' @keywords internal
#' @noRd
make_polygon_water = function(
  heightmap,
  water_input,
  zscale,
  heightmap_extent,
  heightmap_crs,
  water_elevation = NULL,
  water_surface_tolerance = NULL,
  water_color = "lightblue",
  water_alpha = 0.5
) {
  if (
    !requireNamespace("sf", quietly = TRUE) ||
      !requireNamespace("terra", quietly = TRUE)
  ) {
    stop('Polygon water requires the sf and terra packages.', call. = FALSE)
  }
  if (
    !is.null(water_surface_tolerance) &&
      (!is.numeric(water_surface_tolerance) ||
        length(water_surface_tolerance) != 1L ||
        !is.finite(water_surface_tolerance) ||
        water_surface_tolerance < 0)
  ) {
    stop(
      '`water_surface_tolerance` must be NULL or one nonnegative finite number.',
      call. = FALSE
    )
  }
  footprints = polygon_water_footprints(
    water_input,
    heightmap,
    heightmap_extent,
    heightmap_crs,
    water_elevation
  )
  empty = list(vertices = list(), lines = matrix(numeric(), ncol = 3L))
  if (!length(footprints$geometry)) {
    return(invisible(empty))
  }
  terrain = index_polygon_water_terrain(heightmap, zscale)
  if (!nrow(terrain$a)) {
    return(invisible(empty))
  }
  # Single-precision rgl coordinates require clearance proportional to scene
  # size. Keep the sensitivity distinct from the minimum safe rendered gap.
  clearance = max(1e-4 / zscale, 16 * 2^-23 * terrain$magnitude)
  tolerance = if (is.null(water_surface_tolerance)) {
    clearance
  } else {
    water_surface_tolerance / zscale
  }
  vertices = lines = list()
  for (i in seq_along(footprints$geometry)) {
    result = tryCatch(
      {
        support = polygon_water_terrain_support(footprints$geometry[i], terrain)
        if (is.null(support)) {
          next
        }
        level = footprints$levels[i] / zscale
        if (!is.finite(level)) {
          # Prefer the original exterior shoreline. Intersecting it with terrain
          # triangles samples the actual rendered surface even for tiny pools.
          level = if (length(support$shore)) {
            stats::median(support$shore)
          } else {
            stats::median(support$heights)
          }
        }
        # Only nearly horizontal terrain patches coincident with the top trigger
        # a lift. A shoreline crossing on a slope must not raise an entire lake.
        near = abs(support$min_height - level) <= tolerance &
          abs(support$max_height - level) <= tolerance
        if (any(near)) {
          level = max(level, max(support$max_height[near]) + clearance)
        }
        bottom = min(support$heights)
        if (level <= bottom) {
          next
        }
        # Union the clipped triangles to retain the input outline and holes, but
        # omit portions outside the scene or over missing terrain. No cell-sized
        # footprint is introduced, and no area fitting changes the water level.
        geometry = sf::st_union(support$geometry)
        # Triangle intersections split straight shoreline edges into nearly
        # collinear fragments. Remove only double-roundoff-scale deviations,
        # consistently from caps and walls, before the extrusion triangulator
        # checks for collapsed faces. This is far below rgl vertex precision.
        geometry = sf::st_simplify(
          geometry,
          preserveTopology = TRUE,
          dTolerance = 8 * .Machine$double.eps * terrain$magnitude
        )
        mesh = rayvertex::extruded_polygon_mesh(
          sf::st_sf(geometry = geometry),
          top = level,
          bottom = bottom,
          flip_horizontal = TRUE
        )
        triangles = mesh$vertices[[1]][
          as.vector(t(mesh$shapes[[1]]$indices)) + 1L,
          ,
          drop = FALSE
        ]
        boundary = list()
        for (part in water_sf_polygon_parts(geometry[[1]])) {
          for (ring in part) {
            p = cbind(ring[, 1], level, ring[, 2])
            boundary[[length(boundary) + 1L]] = p[
              as.vector(rbind(seq_len(nrow(p) - 1L), 2:nrow(p))),
              ,
              drop = FALSE
            ]
          }
        }
        list(vertices = triangles, lines = do.call(rbind, boundary))
      },
      error = function(e) {
        warning(
          sprintf('Omitted water polygon %s: %s', i, conditionMessage(e)),
          call. = FALSE
        )
        NULL
      }
    )
    if (!is.null(result)) {
      vertices[[length(vertices) + 1L]] = result$vertices
      lines[[length(lines) + 1L]] = result$lines
    }
  }
  if (!length(vertices)) {
    return(invisible(empty))
  }
  for (v in vertices) {
    rgl::triangles3d(
      v,
      color = water_color,
      alpha = water_alpha,
      lit = FALSE,
      front = 'filled',
      back = 'filled',
      texture = NULL,
      tag = 'water'
    )
  }
  invisible(list(vertices = vertices, lines = do.call(rbind, lines)))
}

#' @param water_input Water levels or footprints.
#' @param heightmap Terrain height matrix.
#' @param extent Scene extent.
#' @param crs Scene CRS.
#' @param water_elevation Explicit polygon elevations, or `NULL` to infer them.
#' @return Polygon parts in scene XZ coordinates and their elevation-unit levels.
#' @keywords internal
#' @noRd
polygon_water_footprints = function(
  water_input,
  heightmap,
  extent,
  crs,
  water_elevation
) {
  spatial = is_sf_water_input(water_input) ||
    is_spatial_heightmap_input(water_input)
  if (is_sf_water_input(water_input)) {
    geometry = sf::st_geometry(water_input)
    levels = water_sf_elevations(water_input, water_elevation, length(geometry))
    if (is.null(levels)) levels = rep(NA_real_, length(geometry))
  } else {
    if (spatial) {
      raster = coerce_spatial_waterheight_raster(water_input)
    } else {
      values = normalize_waterheight_matrix(
        water_input,
        nrow(heightmap),
        ncol(heightmap),
        zscale = 1
      )
      raster = terra::rast(
        nrows = ncol(heightmap),
        ncols = nrow(heightmap),
        xmin = -nrow(heightmap) / 2,
        xmax = nrow(heightmap) / 2,
        ymin = -ncol(heightmap) / 2,
        ymax = ncol(heightmap) / 2,
        crs = ''
      )
      terra::values(raster) = as.numeric(values)
    }
    # Polygonize equal-elevation cells in their native grid. In particular,
    # fine input raster footprints need not be enlarged to coarse DEM cells.
    polygons = sf::st_as_sf(terra::as.polygons(
      raster,
      aggregate = TRUE,
      round = FALSE,
      na.rm = TRUE
    ))
    levels = sf::st_drop_geometry(polygons)[[1L]]
    geometry = sf::st_geometry(polygons)
  }
  keep = !sf::st_is_empty(geometry) &
    vapply(
      geometry,
      function(p) length(water_sf_polygon_parts(p)) > 0L,
      logical(1)
    )
  geometry = geometry[keep]
  levels = levels[keep]
  if (!length(geometry)) {
    return(list(geometry = sf::st_sfc(), levels = numeric()))
  }
  if (spatial) {
    if (
      is.null(extent) ||
        is.null(crs) ||
        is.na(sf::st_crs(crs)) ||
        is.na(sf::st_crs(geometry))
    ) {
      stop(
        'Polygon water requires a scene extent and known source and scene CRSs.',
        call. = FALSE
      )
    }
    geometry = sf::st_transform(sf::st_make_valid(sf::st_zm(geometry)), crs)
  }
  geometry = sf::st_make_valid(geometry)
  parts = list()
  part_levels = numeric()
  for (i in seq_along(geometry)) {
    for (part in water_sf_polygon_parts(geometry[[i]])) {
      rings = lapply(part, function(ring) {
        if (spatial) {
          # Scene extents span the first and last terrain vertices, matching
          # transform_into_heightmap_coords(), including spatial ggplot panels.
          bounds = get_extent(extent)
          x = (ring[, 1] - bounds['xmin']) /
            (bounds['xmax'] - bounds['xmin']) *
            (nrow(heightmap) - 1) -
            (nrow(heightmap) - 1) / 2
          z = (bounds['ymax'] - ring[, 2]) /
            (bounds['ymax'] - bounds['ymin']) *
            (ncol(heightmap) - 1) -
            (ncol(heightmap) - 1) / 2
          xyz = apply_geographic_aspect_to_vertices(cbind(x, 0, z))
        } else {
          xyz = apply_geographic_aspect_to_vertices(cbind(
            ring[, 1],
            0,
            -ring[, 2]
          ))
        }
        xyz[, c(1, 3), drop = FALSE]
      })
      parts[[length(parts) + 1L]] = sf::st_polygon(rings)
      part_levels = c(part_levels, levels[i])
    }
  }
  list(geometry = sf::st_sfc(parts), levels = part_levels)
}

#' @param heightmap Terrain height matrix, used if no rendered surface exists.
#' @param zscale Elevation units per scene unit.
#' @return Triangle vertices and a coarse spatial bin index in scene coordinates.
#' @keywords internal
#' @noRd
index_polygon_water_terrain = function(heightmap, zscale) {
  objects = rgl::ids3d(tags = TRUE)
  ids = objects$id[grepl('^surface(_tris)?-', objects$tag)]
  if (length(ids)) {
    # Read the actual indexed surface, including adaptive triangulation and
    # road grading. Never infer its diagonal or elevations from a second mesh.
    chunks = lapply(ids, function(id) {
      v = rgl::rgl.attrib(id, 'vertices')
      indices = as.integer(rgl::rgl.attrib(id, 'indices'))
      if (length(indices)) {
        v = v[indices, , drop = FALSE]
      }
      v
    })
    v = do.call(rbind, chunks)
  } else {
    surface = generate_surface(heightmap, zscale)
    v = surface$verts[as.integer(surface$inds), , drop = FALSE]
  }
  if (!nrow(v)) {
    return(list(a = matrix(numeric(), ncol = 3)))
  }
  a = v[seq(1L, nrow(v), 3L), , drop = FALSE]
  b = v[seq(2L, nrow(v), 3L), , drop = FALSE]
  c = v[seq(3L, nrow(v), 3L), , drop = FALSE]
  determinant = (b[, 1] - a[, 1]) *
    (c[, 3] - a[, 3]) -
    (b[, 3] - a[, 3]) * (c[, 1] - a[, 1])
  keep = is.finite(rowSums(a + b + c)) &
    is.finite(determinant) &
    determinant != 0
  a = a[keep, , drop = FALSE]
  b = b[keep, , drop = FALSE]
  c = c[keep, , drop = FALSE]
  if (!nrow(a)) {
    return(list(a = a))
  }
  xmin = pmin(a[, 1], b[, 1], c[, 1])
  xmax = pmax(a[, 1], b[, 1], c[, 1])
  zmin = pmin(a[, 3], b[, 3], c[, 3])
  zmax = pmax(a[, 3], b[, 3], c[, 3])
  origin = c(min(xmin), min(zmin))
  bins = max(1L, min(64L, ceiling(sqrt(nrow(a) / 128))))
  step = pmax(c(max(xmax), max(zmax)) - origin, .Machine$double.eps) / bins
  x0 = pmin(bins - 1L, floor((xmin - origin[1]) / step[1]))
  x1 = pmin(bins - 1L, floor((xmax - origin[1]) / step[1]))
  z0 = pmin(bins - 1L, floor((zmin - origin[2]) / step[2]))
  z1 = pmin(bins - 1L, floor((zmax - origin[2]) / step[2]))
  nx = x1 - x0 + 1L
  counts = nx * (z1 - z0 + 1L)
  face = rep(seq_len(nrow(a)), counts)
  offset = sequence(counts) - 1L
  bin = x0[face] + offset %% nx[face] + bins * (z0[face] + offset %/% nx[face])
  list(
    a = a,
    b = b,
    c = c,
    xmin = xmin,
    xmax = xmax,
    zmin = zmin,
    zmax = zmax,
    origin = origin,
    step = step,
    bins = bins,
    faces = split(face, bin),
    magnitude = max(1, abs(v), na.rm = TRUE)
  )
}

#' @param footprint One polygon in scene XZ coordinates.
#' @param terrain Indexed rendered terrain triangles.
#' @return Clipped terrain footprints, heights, and exterior shoreline samples; `NULL` without coverage.
#' @keywords internal
#' @noRd
polygon_water_terrain_support = function(footprint, terrain) {
  bb = sf::st_bbox(footprint)
  lo = pmax(0, floor((bb[c('xmin', 'ymin')] - terrain$origin) / terrain$step))
  hi = pmin(
    terrain$bins - 1L,
    floor((bb[c('xmax', 'ymax')] - terrain$origin) / terrain$step)
  )
  if (any(lo > hi)) {
    return(NULL)
  }
  bins = as.vector(outer(
    seq.int(lo[1], hi[1]),
    terrain$bins * seq.int(lo[2], hi[2]),
    '+'
  ))
  ids = unique(unlist(terrain$faces[as.character(bins)], use.names = FALSE))
  ids = ids[
    terrain$xmax[ids] >= bb['xmin'] &
      terrain$xmin[ids] <= bb['xmax'] &
      terrain$zmax[ids] >= bb['ymin'] &
      terrain$zmin[ids] <= bb['ymax']
  ]
  if (!length(ids)) {
    return(NULL)
  }
  geometry = sf::st_sfc(lapply(ids, function(i) {
    sf::st_polygon(list(rbind(
      terrain$a[i, c(1, 3)],
      terrain$b[i, c(1, 3)],
      terrain$c[i, c(1, 3)],
      terrain$a[i, c(1, 3)]
    )))
  }))
  clipped = suppressWarnings(sf::st_intersection(
    sf::st_sf(face = ids, geometry = geometry),
    footprint
  ))
  clipped = suppressWarnings(sf::st_collection_extract(clipped, 'POLYGON'))
  clipped = clipped[as.numeric(sf::st_area(clipped)) > 0, ]
  if (!nrow(clipped)) {
    return(NULL)
  }
  xyz = sf::st_coordinates(clipped)
  # POLYGON/MULTIPOLYGON coordinates carry the feature index in the last column.
  feature = xyz[, ncol(xyz)]
  id = clipped$face[feature]
  a = terrain$a[id, , drop = FALSE]
  b = terrain$b[id, , drop = FALSE]
  c = terrain$c[id, , drop = FALSE]
  det = (b[, 1] - a[, 1]) *
    (c[, 3] - a[, 3]) -
    (b[, 3] - a[, 3]) * (c[, 1] - a[, 1])
  u = ((xyz[, 1] - a[, 1]) *
    (c[, 3] - a[, 3]) -
    (xyz[, 2] - a[, 3]) * (c[, 1] - a[, 1])) /
    det
  v = ((b[, 1] - a[, 1]) *
    (xyz[, 2] - a[, 3]) -
    (b[, 3] - a[, 3]) * (xyz[, 1] - a[, 1])) /
    det
  heights = a[, 2] + u * (b[, 2] - a[, 2]) + v * (c[, 2] - a[, 2])
  by_face = split(heights, feature)
  outer = sf::st_sfc(sf::st_linestring(footprint[[1]][[1]]))
  # Coordinate-to-boundary distance avoids treating a scene/NA cut as shore.
  on_shore = lengths(sf::st_is_within_distance(
    sf::st_as_sf(data.frame(x = xyz[, 1], z = xyz[, 2]), coords = c('x', 'z')),
    outer,
    dist = 1e-8 * terrain$magnitude
  )) >
    0
  list(
    geometry = sf::st_geometry(clipped),
    heights = heights,
    shore = heights[on_shore],
    min_height = vapply(by_face, min, numeric(1)),
    max_height = vapply(by_face, max, numeric(1))
  )
}
