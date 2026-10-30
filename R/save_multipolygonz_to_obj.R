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
#'original winding. Crossed or overlapping rings are repaired in the face plane;
#'new intersection vertices are interpolated along the original 3D edges.
#'By default, `options(rayshader.multipolygonz_repair = TRUE)` also repairs
#'ambiguous projected intersections: existing XYZ vertices are retained and new
#'vertices use the nearest projected source edge (the first edge breaks ties).
#'This depth choice is a heuristic for nonplanar faces. Set
#'`options(rayshader.multipolygonz_repair = FALSE)` to drop these ambiguous faces
#'instead. Unambiguous planar repairs are performed in either mode.
#'Zero-area faces and faces that still cannot be triangulated are skipped with a
#'summary warning in either mode. If no faces remain, an empty OBJ is written.
#'Touching-hole cases may require an sf/GEOS version supporting constrained
#'triangulation. OBJ coordinates are written to four decimal places.
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
  if (!length(faces)) {
    return(NULL)
  }
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
  if (!nrow(coords)) {
    warning("sf_obj contains no nonempty polygon faces.", call. = FALSE)
    return(list())
  }
  if (!all(c("X", "Y", "Z", "L1", "L2") %in% colnames(coords))) {
    stop("sf_obj must contain MULTIPOLYGON Z geometry.", call. = FALSE)
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
  failures = character(length(face_rows))
  faces = lapply(seq_along(face_rows), function(i) {
    tryCatch(
      {
        rows = face_rows[[i]]
        if (any(!is.finite(coords[rows, c("X", "Y", "Z")]))) {
          stop("Polygon face coordinates must be finite.", call. = FALSE)
        }
        ring_id = coords[rows, "L1"]
        rings = split(rows, factor(ring_id, levels = unique(ring_id)))
        rings = lapply(rings, function(rows) {
          vertices = coords[rows, c("X", "Y", "Z"), drop = FALSE]
          # Dropping the first closing duplicate retains the historical vertex
          # ordering for simple faces. Remove adjacent duplicates within each ring.
          if (
            nrow(vertices) > 1L &&
              all(vertices[1, ] == vertices[nrow(vertices), ])
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
        triangulated = multipolygonz_triangulate_face(rings)
        vertices = triangulated$vertices
        indices = triangulated$indices
        if (swap_yz) {
          vertices = vertices[, c(1, 3, 2), drop = FALSE]
          indices = indices[, 3:1, drop = FALSE]
        }
        if (!nrow(indices)) {
          failures[i] <<- "zero-area face"
          return(NULL)
        }
        list(vertices = vertices, indices = indices)
      },
      error = function(e) {
        failures[i] <<- conditionMessage(e)
        NULL
      }
    )
  })
  skipped = which(nzchar(failures))
  if (length(skipped)) {
    # One bounded summary keeps large building datasets from flooding the console.
    examples = head(skipped, 5L)
    warning(
      sprintf(
        "Skipped %d polygon face(s). %s%s",
        length(skipped),
        paste(
          sprintf("Face %d: %s", examples, failures[examples]),
          collapse = "; "
        ),
        if (length(skipped) > length(examples)) {
          "; additional failures omitted."
        } else {
          ""
        }
      ),
      call. = FALSE
    )
  }
  Filter(Negate(is.null), faces)
}

#' @param rings List of open XYZ rings, exterior first and holes afterward.
#' @return List of XYZ vertices and one-based triangle indices.
#' @keywords internal
#' @noRd
multipolygonz_triangulate_face = function(rings) {
  sizes = vapply(rings, nrow, 0L)
  vertices = do.call(rbind, rings)
  empty_face = list(vertices = vertices, indices = matrix(integer(), ncol = 3L))
  if (sizes[1] < 3L) {
    return(empty_face)
  }
  if (any(sizes[-1] < 3L)) {
    stop(
      "An interior ring needs at least three distinct vertices.",
      call. = FALSE
    )
  }
  # Work near the origin at unit scale to avoid cancellation with large
  # projected coordinates. Original XYZ vertices are never rounded or moved.
  local = sweep(vertices, 2, vertices[1, ])
  span = max(abs(local))
  if (span == 0) {
    return(empty_face)
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
    # A symmetric bow-tie can cancel its signed area despite spanning a plane.
    # Recover a projection from the best-fit plane before repairing its rings.
    fit = svd(outer)
    if (length(fit$d) < 2L || fit$d[2] <= 64 * .Machine$double.eps) {
      return(empty_face)
    }
    normal = fit$v[, 3]
  }
  if (length(sizes) == 1L && sizes[1] == 3L) {
    return(list(vertices = vertices, indices = matrix(1:3, nrow = 1)))
  }
  if (!requireNamespace("decido", quietly = TRUE)) {
    stop(
      "Install `decido` to triangulate polygon faces with more than three vertices or holes.",
      call. = FALSE
    )
  }
  xy = local[, setdiff(1:3, which.max(abs(normal))), drop = FALSE]
  starts = c(0L, head(cumsum(sizes), -1L))
  projected_rings = lapply(seq_along(sizes), function(i) {
    rows = starts[i] + seq_len(sizes[i])
    xy[c(rows, rows[1]), , drop = FALSE]
  })
  # Invalid rings can make earcut stall, so repair crossings and overlapping
  # edges before triangulation instead of relying only on an area check.
  projected_face = sf::st_sfc(sf::st_polygon(projected_rings))
  if (!isTRUE(sf::st_is_valid(projected_face))) {
    return(multipolygonz_repair_face(vertices, xy, sizes, normal))
  }
  triangles = matrix(
    decido::earcut(
      xy,
      holes = if (length(starts) > 1L) starts[-1L] + 1L else 0L
    ),
    ncol = 3L,
    byrow = TRUE
  )

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
    return(multipolygonz_repair_face(vertices, xy, sizes, normal))
  }
  triangles = triangles[triangle_area != 0, , drop = FALSE]
  triangle_area = triangle_area[triangle_area != 0]
  # Earcut chooses its own winding. Restore the original exterior orientation.
  reverse = sign(triangle_area) != sign(ring_area[1])
  triangles[reverse, ] = triangles[reverse, c(1, 3, 2), drop = FALSE]
  list(vertices = vertices, indices = triangles)
}


#' @param vertices Original XYZ vertices, with rings concatenated.
#' @param xy Normalized dominant-plane coordinates of the vertices.
#' @param sizes Number of vertices in each ring, exterior first.
#' @param normal Face normal used to select the projection and winding.
#' @return List of XYZ vertices and one-based triangle indices for repaired faces.
#' @keywords internal
#' @noRd
multipolygonz_repair_face = function(vertices, xy, sizes, normal) {
  repair_ambiguous = isTRUE(getOption("rayshader.multipolygonz_repair", TRUE))
  starts = c(0L, head(cumsum(sizes), -1L))
  rings = lapply(seq_along(sizes), function(i) {
    rows = starts[i] + seq_len(sizes[i])
    xy[c(rows, rows[1]), , drop = FALSE]
  })
  # Repair only in the face's 2D plane. Repairing the original sf object in XY
  # would collapse vertical walls and discard or misassign their elevations.
  repaired = sf::st_make_valid(
    sf::st_sfc(sf::st_polygon(rings)),
    geos_method = "valid_linework"
  )
  if (inherits(repaired, "sfc_GEOMETRYCOLLECTION")) {
    repaired = suppressWarnings(sf::st_collection_extract(repaired, "POLYGON"))
  }
  repaired = repaired[
    !is.na(sf::st_dimension(repaired)) & sf::st_dimension(repaired) == 2L
  ]
  if (!length(repaired)) {
    return(list(vertices = vertices, indices = matrix(integer(), ncol = 3L)))
  }
  polygons = suppressWarnings(sf::st_cast(repaired, "POLYGON"))
  polygons = polygons[!sf::st_is_empty(polygons)]
  if (!length(polygons)) {
    stop("The repaired face has no polygon area.", call. = FALSE)
  }

  end_rows = unlist(
    lapply(seq_along(sizes), function(i) {
      starts[i] + c(seq_len(sizes[i])[-1L], 1L)
    }),
    use.names = FALSE
  )
  edges = xy[end_rows, , drop = FALSE] - xy
  edge_length2 = rowSums(edges * edges)
  result_vertices = list()
  result_indices = list()
  offset = 0L
  drop_axis = which.max(abs(normal))
  orientation = sign(normal[drop_axis]) * c(1, -1, 1)[drop_axis]
  for (polygon in polygons) {
    expected = as.numeric(sf::st_area(sf::st_sfc(polygon)))
    # GEOS can create roundoff-sized slivers where edges nearly coincide.
    # Coordinates are normalized to unit span; discard sub-precision area.
    if (expected <= 64 * .Machine$double.eps) {
      next
    }
    repaired_rings = lapply(polygon, function(ring) {
      ring[-nrow(ring), , drop = FALSE]
    })
    repaired_xy = do.call(rbind, repaired_rings)
    repaired_sizes = vapply(repaired_rings, nrow, 0L)
    hole_starts = head(cumsum(repaired_sizes), -1L) + 1L
    triangles = matrix(
      decido::earcut(
        repaired_xy,
        holes = if (length(hole_starts)) hole_starts else 0L
      ),
      ncol = 3L,
      byrow = TRUE
    )
    if (!nrow(triangles)) {
      stop("The repaired face could not be triangulated.", call. = FALSE)
    }
    a = repaired_xy[triangles[, 2], , drop = FALSE] -
      repaired_xy[triangles[, 1], , drop = FALSE]
    b = repaired_xy[triangles[, 3], , drop = FALSE] -
      repaired_xy[triangles[, 1], , drop = FALSE]
    areas = a[, 1] * b[, 2] - a[, 2] * b[, 1]
    if (
      abs(sum(abs(areas)) / 2 - expected) > 1e-8 * expected &&
        "st_triangulate_constrained" %in% getNamespaceExports("sf")
    ) {
      # Earcut can bridge incorrectly when a hole touches its outer ring.
      # GEOS constrained triangulation retains that shared boundary vertex.
      constrained = sf::st_triangulate_constrained(sf::st_sfc(polygon))
      pieces = suppressWarnings(sf::st_collection_extract(
        constrained,
        "POLYGON"
      ))
      pieces = suppressWarnings(sf::st_cast(pieces, "POLYGON"))
      repaired_xy = do.call(
        rbind,
        lapply(pieces, function(piece) piece[[1]][1:3, , drop = FALSE])
      )
      triangles = matrix(seq_len(nrow(repaired_xy)), ncol = 3L, byrow = TRUE)
      a = repaired_xy[triangles[, 2], , drop = FALSE] -
        repaired_xy[triangles[, 1], , drop = FALSE]
      b = repaired_xy[triangles[, 3], , drop = FALSE] -
        repaired_xy[triangles[, 1], , drop = FALSE]
      areas = a[, 1] * b[, 2] - a[, 2] * b[, 1]
    }
    if (abs(sum(abs(areas)) / 2 - expected) > 1e-8 * expected) {
      stop(
        "Triangulation does not cover the repaired face; update sf/GEOS for constrained triangulation support.",
        call. = FALSE
      )
    }
    triangles = triangles[areas != 0, , drop = FALSE]
    areas = areas[areas != 0]
    reverse = sign(areas) != orientation
    triangles[reverse, ] = triangles[reverse, c(1, 3, 2), drop = FALSE]

    # GEOS adds intersection vertices. Keep existing XYZ values exactly and
    # lift new points along the nearest projected edge (first edge breaks ties).
    # Repair mode permits this heuristic depth choice for warped faces. Drop
    # mode instead requires all incident edges to agree on the 3D position.
    lifted = t(vapply(
      seq_len(nrow(repaired_xy)),
      function(i) {
        point = repaired_xy[i, ]
        distance2 = rowSums(sweep(xy, 2, point)^2)
        original = which.min(distance2)
        if (
          repair_ambiguous &&
            distance2[original] <= (64 * .Machine$double.eps)^2
        ) {
          return(vertices[original, ])
        }
        delta = sweep(xy, 2, point, function(a, b) b - a)
        fraction = rowSums(delta * edges) / edge_length2
        fraction[!is.finite(fraction)] = 0
        fraction = pmax(0, pmin(1, fraction))
        closest = xy + edges * fraction
        edge_distance2 = rowSums(sweep(closest, 2, point)^2)
        edge = which.min(edge_distance2)
        if (edge_distance2[edge] > 1e-16) {
          stop(
            "A repaired vertex does not lie on an original face edge.",
            call. = FALSE
          )
        }
        if (!repair_ambiguous) {
          # Match edges within 1e-8 of normalized face span, then require their
          # interpolated XYZ positions to agree to the same relative tolerance.
          incident = which(edge_distance2 <= 1e-16)
          candidates = vertices[incident, , drop = FALSE] +
            fraction[incident] *
              (vertices[end_rows[incident], , drop = FALSE] -
                vertices[incident, , drop = FALSE])
          span = max(abs(sweep(vertices, 2, vertices[1, ])))
          if (max(abs(sweep(candidates, 2, candidates[1, ]))) > 1e-8 * span) {
            stop(
              "Projected edges intersect at different 3D depths.",
              call. = FALSE
            )
          }
        }
        if (distance2[original] <= (64 * .Machine$double.eps)^2) {
          return(vertices[original, ])
        }
        vertices[edge, ] +
          fraction[edge] * (vertices[end_rows[edge], ] - vertices[edge, ])
      },
      numeric(3)
    ))
    result_vertices[[length(result_vertices) + 1L]] = lifted
    result_indices[[length(result_indices) + 1L]] = triangles + offset
    offset = offset + nrow(lifted)
  }
  if (!length(result_vertices)) {
    return(list(vertices = vertices, indices = matrix(integer(), ncol = 3L)))
  }
  list(
    vertices = do.call(rbind, result_vertices),
    indices = do.call(rbind, result_indices)
  )
}
