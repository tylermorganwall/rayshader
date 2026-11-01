#include <Rcpp.h>
#include <cmath>
#include <limits>

using namespace Rcpp;

// Keep the R path's separate multiply/add stages. Small rounding differences
// can affect height interpolation or classification at the shoreline tolerance.
#ifdef __clang__
#pragma clang fp contract(off)
#endif

// The terrain index already removed nonfinite and zero-XZ-area triangles.
// Return sfg objects; st_sfc remains responsible for CRS/bbox metadata.
// [[Rcpp::export]]
List water_triangle_rings_cpp(NumericMatrix a, NumericMatrix b, NumericMatrix c,
                             IntegerVector ids) {
  // Each row identifies one terrain face, with its three XYZ vertices stored
  // in corresponding rows of a, b and c. Validate before following face IDs.
  if (a.ncol() != 3 || b.ncol() != 3 || c.ncol() != 3 ||
      a.nrow() != b.nrow() || a.nrow() != c.nrow()) {
    stop("Invalid terrain matrices.");
  }

  List result(ids.size());
  CharacterVector polygon_class = CharacterVector::create("XY", "POLYGON", "sfg");

  for (R_xlen_t j = 0; j < ids.size(); ++j) {
    if ((j & 4095) == 0) {
      checkUserInterrupt();
    }

    if (ids[j] < 1 || ids[j] > a.nrow()) {
      stop("Invalid terrain face index.");
    }

    const int i = ids[j] - 1;

    // Project the terrain face onto scene XZ, represented as planar XY to sf.
    // Preserve vertex order and repeat the first vertex to close the ring.
    NumericMatrix ring(4, 2);

    for (int axis = 0; axis < 2; ++axis) {
      const int col = axis * 2;
      ring(0, axis) = a(i, col);
      ring(1, axis) = b(i, col);
      ring(2, axis) = c(i, col);
      ring(3, axis) = a(i, col);
    }

    // Wrap one exterior ring in the sfg POLYGON layout. These triangles were
    // validated when the terrain index was built, so no ring repair is needed.
    List polygon = List::create(ring);
    polygon.attr("class") = polygon_class;
    result[j] = polygon;
  }

  return result;
}


// Heights are evaluated on the same GEOS-clipped coordinates as the R path.
// A point is shore only if within tolerance of the original EXTERIOR ring;
// scene/NA cut edges and hole boundaries are deliberately not shore samples.
// [[Rcpp::export]]
List water_support_samples_cpp(NumericMatrix coordinates, IntegerVector face,
                               NumericMatrix a, NumericMatrix b, NumericMatrix c,
                               NumericMatrix exterior, double tolerance) {
  // The last coordinate column contains sf's one-based feature index. The face
  // vector maps each clipped feature to its original terrain triangle. Exterior
  // ring coordinates and the distance tolerance use the same scene XZ units.
  if (coordinates.ncol() < 3 || a.ncol() != 3 || b.ncol() != 3 || c.ncol() != 3 ||
      a.nrow() != b.nrow() || a.nrow() != c.nrow() || exterior.ncol() != 2 ||
      exterior.nrow() < 2 || !R_finite(tolerance) || tolerance < 0) {
    stop("Invalid water support inputs.");
  }

  const int count = coordinates.nrow(),
            groups = face.size();
  NumericVector heights(count),
                minimum(groups, R_PosInf),
                maximum(groups, R_NegInf);
  std::vector<double> shore;

  for (int row = 0; row < count; ++row) {
    if ((row & 4095) == 0) {
      checkUserInterrupt();
    }

    // Resolve the source face without changing GEOS coordinate order. Repeated
    // ring endpoints remain in the samples, matching the R path's medians.
    const double feature = coordinates(row, coordinates.ncol() - 1);

    if (!R_finite(feature) || feature < 1 || feature > groups ||
        feature != std::floor(feature)) {
      stop("Invalid water feature index.");
    }

    const int group = static_cast<int>(feature) - 1;

    if (face[group] < 1 || face[group] > a.nrow()) {
      stop("Invalid water terrain face index.");
    }

    const int id = face[group] - 1;
    const double x = coordinates(row, 0),
                 z = coordinates(row, 1);

    // Solve p = a + u(b-a) + v(c-a) in the horizontal XZ plane, then use the
    // same barycentric weights to interpolate Y (height). The terrain index
    // excludes zero-area projected faces, so the determinant is nonzero.
    // Keep the arithmetic order unchanged from the R implementation.
    const double det =
      (b(id, 0) - a(id, 0)) * (c(id, 2) - a(id, 2)) -
      (b(id, 2) - a(id, 2)) * (c(id, 0) - a(id, 0));

    const double u =
      ((x - a(id, 0)) * (c(id, 2) - a(id, 2)) -
       (z - a(id, 2)) * (c(id, 0) - a(id, 0))) / det;

    const double v =
      ((b(id, 0) - a(id, 0)) * (z - a(id, 2)) -
       (b(id, 2) - a(id, 2)) * (x - a(id, 0))) / det;

    const double height =
      a(id, 1) + u * (b(id, 1) - a(id, 1)) + v * (c(id, 1) - a(id, 1));

    // Retain every sample plus per-feature extrema. The caller uses these to
    // choose a bottom elevation and detect nearly coincident horizontal water.
    heights[row] = height;
    minimum[group] = std::min(minimum[group], height);
    maximum[group] = std::max(maximum[group], height);

    // Test only the original exterior shoreline, not boundaries introduced by
    // clipping against the scene or missing terrain. Stop after the first close
    // segment so a vertex near multiple edges contributes its height only once.
    for (int edge = 1; edge < exterior.nrow(); ++edge) {
      const double ax = exterior(edge - 1, 0),
                   az = exterior(edge - 1, 1);
      const double bx = exterior(edge, 0),
                   bz = exterior(edge, 1);
      const double dx = bx - ax,
                   dz = bz - az,
                   length2 = dx * dx + dz * dz;
      double distance;

      if (length2 == 0) {
        // Repeated ring coordinates form a point, not a usable segment.
        distance = std::sqrt((x - ax) * (x - ax) + (z - az) * (z - az));
      } else {
        // Projection is the fraction along the segment. Outside [0, 1], the
        // closest point is an endpoint; inside, use perpendicular distance.
        const double projection = ((x - ax) * dx + (z - az) * dz) / length2;

        if (projection <= 0) {
          distance = std::sqrt((x - ax) * (x - ax) + (z - az) * (z - az));
        } else if (projection >= 1) {
          distance = std::sqrt((x - bx) * (x - bx) + (z - bz) * (z - bz));
        } else {
          // Match GEOS Distance::pointToSegment: the signed cross-product
          // ratio times segment length gives the perpendicular distance.
          const double s = ((az - z) * dx - (ax - x) * dz) / length2;
          distance = std::abs(s) * std::sqrt(length2);
        }
      }

      if (distance <= tolerance) {
        shore.push_back(height);
        break;
      }
    }
  }

  // Preserve the one-based feature names produced by R's split()/vapply()
  // summaries, as well as the original order of full and shoreline samples.
  CharacterVector names(groups);

  for (int i = 0; i < groups; ++i) {
    names[i] = std::to_string(i + 1);
  }

  minimum.attr("names") = names;
  maximum.attr("names") = names;

  return List::create(
    _["heights"] = heights,
    _["shore"] = wrap(shore),
    _["min_height"] = minimum,
    _["max_height"] = maximum
  );
}
