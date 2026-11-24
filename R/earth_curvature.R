#' @return Whether the active scene uses a curved elevation datum.
#' @keywords internal
#' @noRd
get_scene_earth_curvature = function() {
  isTRUE(get_scene_context_value("scene_earth_curvature", default = FALSE))
}

#' @param distance_squared Squared ground distance from the map center in meters squared.
#' @return Drop below the center tangent plane in meters.
#' @keywords internal
#' @noRd
earth_curvature_drop_meters = function(distance_squared) {
  radius = 6371008.8
  # The half-angle form of R * (1 - cos(distance / R)) retains small drops.
  2 * radius * sin(sqrt(distance_squared) / (2 * radius))^2
}

#' @param x Horizontal east-west coordinate in centered rgl scene units.
#' @param z Horizontal north-south coordinate in centered rgl scene units.
#' @return Downward displacement in scene units, independent of elevation exaggeration.
#' @keywords internal
#' @noRd
scene_curvature_drop = function(x, z) {
  if (!get_scene_earth_curvature()) {
    return(0)
  }
  aspect = get_scene_geographic_aspect()
  meters = aspect$cell_meters / aspect$scale
  earth_curvature_drop_meters((x * meters[["x"]])^2 + (z * meters[["z"]])^2) /
    aspect$mean_cell_meters
}

#' @param vertices Matrix of centered scene x, vertical y, and z coordinates.
#' @return Vertices measured relative to the scene's tangent plane.
#' @keywords internal
#' @noRd
curve_scene_vertices = function(vertices) {
  if (get_scene_earth_curvature() && nrow(vertices)) {
    vertices[, 2] = vertices[, 2] -
      scene_curvature_drop(vertices[, 1], vertices[, 3])
  }
  vertices
}

#' @param heightmap Matrix of source elevations.
#' @param zscale Effective elevation units per scene unit for this layer.
#' @return Terrain in the rendered datum for operations that sample the displayed mesh.
#' @keywords internal
#' @noRd
scene_draping_heightmap = function(heightmap, zscale) {
  if (!is.matrix(heightmap) || !get_scene_earth_curvature()) {
    return(heightmap)
  }
  if (isTRUE(attr(heightmap, "rayshader_curved_terrain"))) {
    return(heightmap)
  }
  if (
    identical(zscale, get_scene_effective_zscale()) &&
      identical(heightmap, get_scene_context_value("scene_source_heightmap"))
  ) {
    heightmap = get_scene_heightmap()
  } else {
    heightmap = apply_earth_curvature(
      heightmap,
      get_scene_geographic_aspect(),
      zscale
    )
  }
  attr(heightmap, "rayshader_curved_terrain") = TRUE
  heightmap
}

#' @param vertices Consecutive planar triangles or independent line segments in scene coordinates.
#' @param size Default `3L`. Number of vertices per primitive, either two or three.
#' @return Vertices following the spherical surface, including intermediate samples on long edges.
#' @keywords internal
#' @noRd
curve_scene_primitives = function(vertices, size = 3L) {
  if (!get_scene_earth_curvature() || !nrow(vertices)) {
    return(vertices)
  }
  aspect = get_scene_geographic_aspect()
  meters = aspect$cell_meters / aspect$scale
  # Limit chord sag to about 1/100 of one scene unit. Split along the longest
  # horizontal edge so adjacent faces bisect their shared edges consistently.
  limit_squared = curvature_chord_limit(aspect)^2
  bend = function(v) {
    index = matrix(seq_len(nrow(v)), ncol = size, byrow = TRUE)
    next_col = c(seq.int(2L, size), 1L)
    dx = matrix(v[index, 1], ncol = size)
    dz = matrix(v[index, 3], ncol = size)
    lengths_squared = ((dx - dx[, next_col]) * meters[["x"]])^2 +
      ((dz - dz[, next_col]) * meters[["z"]])^2
    longest = max.col(lengths_squared, ties.method = "first")
    split = lengths_squared[cbind(seq_len(nrow(index)), longest)] >
      limit_squared
    if (!any(split)) {
      return(curve_scene_vertices(v))
    }
    finished = curve_scene_vertices(v[
      as.vector(t(index[!split, , drop = FALSE])),
      ,
      drop = FALSE
    ])
    rows = which(split)
    a = v[index[cbind(rows, longest[rows])], , drop = FALSE]
    b = v[index[cbind(rows, next_col[longest[rows]])], , drop = FALSE]
    mid = (a + b) / 2
    if (size == 2L) {
      children = rbind(a, mid, mid, b)
    } else {
      c_vertex = v[
        index[cbind(rows, next_col[next_col[longest[rows]]])],
        ,
        drop = FALSE
      ]
      children = rbind(a, mid, c_vertex, mid, b, c_vertex)
    }
    ordered = as.vector(t(matrix(seq_len(nrow(children)), nrow = length(rows))))
    rbind(finished, bend(children[ordered, , drop = FALSE]))
  }
  bend(vertices)
}

#' @param aspect Scene geographic aspect with metric cell spacing.
#' @return Maximum ground length in meters for about 0.01 scene units of chord sag.
#' @keywords internal
#' @noRd
curvature_chord_limit = function(aspect) {
  sqrt(8 * 6371008.8 * aspect$mean_cell_meters / 100)
}
