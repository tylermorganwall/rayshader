#'@title Save MULTIPOLYGON Z sf data to OBJ file
#'
#'@description Converts MULTIPOLYGON Z features into a triangulated 3D OBJ model,
#'preserving concave boundaries and holes in each face.
#'
#'@param sf_obj sf object with MULTIPOLYGON Z geometry,
#'@param filename Filename of the OBJ to save the 3D model to.
#'@param swap_yz Default `FALSE`. Whether to swap the Y and Z axes. (Y axis is vertical in
#'rayshader coordinates, but data is often provided with Z being vertical).
#'@export
#'@details Faces with more than three vertices or holes require the `decido`
#'package. Triangulation uses each face's dominant plane and preserves its
#'original winding. OBJ coordinates are written to four decimal places.
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Convert the built-in Washington Monument MULTIPOLYGON Z data to an OBJ file
#'obj_temp = tempfile(fileext=".obj")
#'save_multipolygonz_to_obj(washington_monument_multipolygonz, obj_temp, swap_yz=TRUE)
#'#Render with rgl
#'rgl::open3d()
#'render_obj(filename=obj_temp, xyz=matrix(c(0,0,0),ncol=3), color="red")
#'render_camera(theta=30,phi=40)
save_multipolygonz_to_obj = function(sf_obj, filename, swap_yz = FALSE) {
  faces = multipolygonz_mesh_faces(sf_obj, swap_yz = swap_yz)
  con = file(filename, "w")
  on.exit(close(con))
  total_verts = 0L
  for (face in faces) {
    vertices = face$vertices
    triangles = face$indices + total_verts
    writeLines(
      c(
        sprintf(
          "v %.4f %.4f %.4f",
          vertices[, 1],
          vertices[, 2],
          vertices[, 3]
        ),
        sprintf("f %d %d %d", triangles[, 1], triangles[, 2], triangles[, 3])
      ),
      con
    )
    total_verts = total_verts + nrow(vertices)
  }
}

multipolygonz_to_raymesh = function(sf_obj, swap_yz = FALSE) {
  faces = multipolygonz_mesh_faces(sf_obj, swap_yz = swap_yz)
  sizes = vapply(faces, function(face) nrow(face$vertices), 0L)
  offsets = c(0L, head(cumsum(sizes), -1L))
  rayvertex::construct_mesh(
    vertices = do.call(rbind, lapply(faces, `[[`, "vertices")),
    indices = do.call(
      rbind,
      lapply(seq_along(faces), function(i) {
        faces[[i]]$indices - 1L + offsets[i]
      })
    )
  )
}

#' @param sf_obj Spatial object containing polygon faces with XYZ coordinates.
#' @param swap_yz Default `FALSE`. Swap Y and Z and reverse triangle winding.
#' @return List of face vertices and one-based triangle indices.
#' @keywords internal
#' @noRd
multipolygonz_mesh_faces = function(sf_obj, swap_yz = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("`sf` is required to convert MULTIPOLYGON Z geometry.", call. = FALSE)
  }
  coords = sf::st_coordinates(sf::st_geometry(sf_obj))
  if (!all(c("X", "Y", "Z", "L1", "L2") %in% colnames(coords))) {
    stop("sf_obj must contain MULTIPOLYGON Z geometry.", call. = FALSE)
  }
  if (!nrow(coords)) {
    stop("sf_obj contains no nonempty polygon faces.", call. = FALSE)
  }
  if (any(!is.finite(coords[, c("X", "Y", "Z")]))) {
    stop("Polygon face coordinates must be finite.", call. = FALSE)
  }
  # L3 identifies features and L2 identifies faces; L1 must remain separate
  # because it identifies the exterior ring and each interior hole.
  group_cols = intersect(c("L3", "L2"), colnames(coords))
  group_id = do.call(
    paste,
    c(as.data.frame(coords[, group_cols, drop = FALSE]), sep = "\r")
  )
  face_rows = split(
    seq_len(nrow(coords)),
    factor(group_id, levels = unique(group_id))
  )
  lapply(seq_along(face_rows), function(i) {
    rows = face_rows[[i]]
    ring_id = coords[rows, "L1"]
    rings = split(rows, factor(ring_id, levels = unique(ring_id)))
    rings = lapply(rings, function(rows) {
      vertices = coords[rows, c("X", "Y", "Z"), drop = FALSE]
      # Dropping the first closing duplicate retains the historical vertex
      # ordering for simple faces. Remove adjacent duplicates within each ring.
      if (
        nrow(vertices) > 1L && all(vertices[1, ] == vertices[nrow(vertices), ])
      ) {
        vertices = vertices[-1, , drop = FALSE]
      }
      if (nrow(vertices) > 1L) {
        keep = c(
          TRUE,
          rowSums(
            vertices[-1, , drop = FALSE] !=
              vertices[-nrow(vertices), , drop = FALSE]
          ) >
            0
        )
        vertices = vertices[keep, , drop = FALSE]
      }
      vertices
    })
    vertices = do.call(rbind, rings)
    indices = tryCatch(
      multipolygonz_triangulate_face(rings),
      error = function(e) {
        stop(
          sprintf(
            "Could not triangulate polygon face %d: %s",
            i,
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
    if (swap_yz) {
      vertices = vertices[, c(1, 3, 2), drop = FALSE]
      indices = indices[, 3:1, drop = FALSE]
    }
    list(vertices = vertices, indices = indices)
  })
}

#' @param rings List of open XYZ rings, exterior first and holes afterward.
#' @return One-based triangle indices into the concatenated rings.
#' @keywords internal
#' @noRd
multipolygonz_triangulate_face = function(rings) {
  sizes = vapply(rings, nrow, 0L)
  if (any(sizes < 3L)) {
    stop("Each ring needs at least three distinct vertices.", call. = FALSE)
  }
  vertices = do.call(rbind, rings)
  # Work near the origin at unit scale to avoid cancellation with large
  # projected coordinates. Original XYZ vertices are never rounded or moved.
  local = sweep(vertices, 2, vertices[1, ])
  span = max(abs(local))
  if (span == 0) {
    stop("The face has zero area.", call. = FALSE)
  }
  local = local / span
  outer = local[seq_len(sizes[1]), , drop = FALSE]
  next_vertex = outer[c(seq_len(nrow(outer))[-1], 1L), , drop = FALSE]
  normal = colSums(cbind(
    outer[, 2] * next_vertex[, 3] - outer[, 3] * next_vertex[, 2],
    outer[, 3] * next_vertex[, 1] - outer[, 1] * next_vertex[, 3],
    outer[, 1] * next_vertex[, 2] - outer[, 2] * next_vertex[, 1]
  ))
  # Newell's normal selects the least-degenerate projection for horizontal,
  # vertical, and sloping faces. A fixed XY projection would collapse walls.
  if (max(abs(normal)) <= 64 * .Machine$double.eps) {
    stop("The face has zero area or an invalid exterior ring.", call. = FALSE)
  }
  if (length(sizes) == 1L && sizes[1] == 3L) {
    return(matrix(1:3, nrow = 1))
  }
  if (!requireNamespace("decido", quietly = TRUE)) {
    stop(
      "Install `decido` to triangulate polygon faces with more than three vertices or holes.",
      call. = FALSE
    )
  }
  xy = local[, setdiff(1:3, which.max(abs(normal))), drop = FALSE]
  starts = c(0L, head(cumsum(sizes), -1L))
  triangles = matrix(
    decido::earcut(
      xy,
      holes = if (length(starts) > 1L) starts[-1L] + 1L else 0L
    ),
    ncol = 3L,
    byrow = TRUE
  )
  if (!nrow(triangles)) {
    stop("Triangulation produced no triangles.", call. = FALSE)
  }
  a = xy[triangles[, 2], , drop = FALSE] - xy[triangles[, 1], , drop = FALSE]
  b = xy[triangles[, 3], , drop = FALSE] - xy[triangles[, 1], , drop = FALSE]
  triangle_area = a[, 1] * b[, 2] - a[, 2] * b[, 1]
  ring_area = vapply(
    seq_along(sizes),
    function(i) {
      ring = xy[starts[i] + seq_len(sizes[i]), , drop = FALSE]
      next_ring = ring[c(seq_len(nrow(ring))[-1], 1L), , drop = FALSE]
      sum(ring[, 1] * next_ring[, 2] - ring[, 2] * next_ring[, 1])
    },
    0
  )
  expected_area = abs(ring_area[1]) - sum(abs(ring_area[-1]))
  # Reject incomplete triangulation rather than silently filling only part of
  # a face. Areas are twice the projected area in normalized coordinates.
  tolerance = 1e-8 * abs(ring_area[1])
  if (
    expected_area <= 0 ||
      abs(sum(abs(triangle_area)) - expected_area) > tolerance
  ) {
    stop(
      "Triangulation does not cover the exterior minus its holes.",
      call. = FALSE
    )
  }
  triangles = triangles[triangle_area != 0, , drop = FALSE]
  triangle_area = triangle_area[triangle_area != 0]
  # Earcut chooses its own winding. Restore the original exterior orientation.
  reverse = sign(triangle_area) != sign(ring_area[1])
  triangles[reverse, ] = triangles[reverse, c(1, 3, 2), drop = FALSE]
  triangles
}
