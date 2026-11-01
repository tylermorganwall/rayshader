#include <Rcpp.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <earcut.h>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace scene_geos {
#include <libgeos.c>
#include <libgeos.h>
} // namespace scene_geos

// Resolve registered callables once at package load. GEOS objects stay within
// this provider and the context owning each native batch.
// [[Rcpp::export(rng = false)]]
void initialize_scene_geos_cpp() { scene_geos::libgeos_init_api(); }

#ifdef __clang__
#pragma clang fp contract(off)
#endif

namespace scene_geometry {
using namespace scene_geos;
using XY = std::array<double, 2>;
using XYZ = std::array<double, 3>;
using Ring = std::vector<XY>;

struct Context {
  GEOSContextHandle_t handle = GEOS_init_r();
  std::string error;

  Context() {
    if (!handle)
      throw std::runtime_error("Could not initialize scene GEOS context.");
    GEOSContext_setErrorMessageHandler_r(
        handle, [](const char *message, void *data) { static_cast<Context *>(data)->error = message; }, this);
    GEOSContext_setNoticeMessageHandler_r(handle, [](const char *, void *) {}, nullptr);
  }
  ~Context() { GEOS_finish_r(handle); }
  Context(const Context &) = delete;
  Context &operator=(const Context &) = delete;
};

struct Deleter {
  Context *context;
  void operator()(GEOSGeometry *geometry) const {
    if (geometry)
      GEOSGeom_destroy_r(context->handle, geometry);
  }
};
using Geometry = std::unique_ptr<GEOSGeometry, Deleter>;

Geometry own(Context &context, GEOSGeometry *geometry) {
  if (!geometry)
    throw std::runtime_error(context.error.empty() ? "GEOS operation failed." : context.error);
  return Geometry(geometry, Deleter{&context});
}

Geometry make_line(Context &context, const Ring &points, bool closed = false) {
  auto sequence = GEOSCoordSeq_create_r(context.handle, points.size() + (closed ? 1 : 0), 2);
  if (!sequence)
    throw std::runtime_error("GEOS coordinate allocation failed.");
  for (size_t i = 0; i < points.size() + (closed ? 1 : 0); ++i) {
    const auto &point = points[i % points.size()];
    GEOSCoordSeq_setX_r(context.handle, sequence, i, point[0]);
    GEOSCoordSeq_setY_r(context.handle, sequence, i, point[1]);
  }
  // Constructors consume the coordinate sequence, including failure paths.
  return own(context, closed ? GEOSGeom_createLinearRing_r(context.handle, sequence)
                             : GEOSGeom_createLineString_r(context.handle, sequence));
}

// Share one GEOS context across the batch instead of allocating one per item.
// Keep interruption checks outside the geometry callbacks; result assembly
// happens after the batch, without per-item R closures or spatial wrappers.
template <class Work> void run_batch(size_t count, Work work) {
  if (!count)
    return;
  Context context;
  for (size_t i = 0; i < count; ++i) {
    if (i % 32 == 0)
      Rcpp::checkUserInterrupt();
    context.error.clear();
    work(context, i);
  }
}

struct Face {
  std::vector<XYZ> vertices;
  std::vector<size_t> sizes;
  std::vector<std::array<int, 3>> triangles;
  bool fallback = false;
};

// Fast path mirrors the R triangulator's unit-scale dominant-plane projection.
// Invalid rings, ambiguous normals, and coverage failures return to the existing
// R repair routine; the fast path must not introduce a second repair policy.
void triangulate_face(Context &context, Face &face) {
  if (face.sizes.empty() || face.sizes[0] < 3) {
    face.fallback = true;
    return;
  }
  for (auto size : face.sizes)
    if (size < 3) {
      face.fallback = true;
      return;
    }
  auto local = face.vertices;
  double span = 0;
  for (auto &point : local) {
    for (size_t j = 0; j < 3; ++j) {
      point[j] -= face.vertices[0][j];
      span = std::max(span, std::abs(point[j]));
    }
  }
  if (!(span > 0)) {
    face.fallback = true;
    return;
  }
  for (auto &point : local)
    for (auto &value : point)
      value /= span;
  // Long-double accumulation matches R's colSums behavior near cancellation.
  std::array<long double, 3> normal{0, 0, 0};
  for (size_t i = 0; i < face.sizes[0]; ++i) {
    const auto &a = local[i], &b = local[(i + 1) % face.sizes[0]];
    normal[0] += a[1] * b[2] - a[2] * b[1];
    normal[1] += a[2] * b[0] - a[0] * b[2];
    normal[2] += a[0] * b[1] - a[1] * b[0];
  }
  size_t drop = 0;
  for (size_t i = 1; i < 3; ++i)
    if (std::abs(normal[i]) > std::abs(normal[drop]))
      drop = i;
  if (std::abs(normal[drop]) <= 64 * std::numeric_limits<double>::epsilon()) {
    face.fallback = true;
    return; // R's SVD recovery handles these faces.
  }
  if (face.sizes.size() == 1 && face.sizes[0] == 3) {
    face.triangles.push_back({1, 2, 3});
    return;
  }
  std::vector<Ring> rings;
  Ring xy;
  std::vector<double> area;
  size_t offset = 0;
  for (auto size : face.sizes) {
    Ring ring;
    for (size_t i = 0; i < size; ++i) {
      XY point;
      size_t k = 0;
      for (size_t j = 0; j < 3; ++j)
        if (j != drop)
          point[k++] = local[offset + i][j];
      ring.push_back(point);
      xy.push_back(point);
    }
    long double signed_area = 0;
    for (size_t i = 0; i < size; ++i) {
      const auto &a = ring[i], &b = ring[(i + 1) % size];
      signed_area += a[0] * b[1] - a[1] * b[0];
    }
    area.push_back(static_cast<double>(signed_area));
    rings.push_back(std::move(ring));
    offset += size;
  }

  auto shell = make_line(context, rings[0], true);
  std::vector<Geometry> holes;
  std::vector<GEOSGeometry *> hole_pointers;
  for (size_t i = 1; i < rings.size(); ++i) {
    holes.push_back(make_line(context, rings[i], true));
    hole_pointers.push_back(holes.back().get());
  }
  auto polygon = own(
      context, GEOSGeom_createPolygon_r(context.handle, shell.release(), hole_pointers.data(), hole_pointers.size()));
  for (auto &hole : holes)
    hole.release();
  if (GEOSisValid_r(context.handle, polygon.get()) != 1) {
    face.fallback = true;
    return;
  }

  auto indices = mapbox::earcut<uint32_t>(rings);
  long double covered = 0;
  for (size_t i = 0; i < indices.size(); i += 3) {
    auto a = xy[indices[i]], b = xy[indices[i + 1]], c = xy[indices[i + 2]];
    double cross = (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0]);
    covered += std::abs(cross);
    if (cross == 0)
      continue;
    std::array<int, 3> triangle{int(indices[i] + 1), int(indices[i + 1] + 1), int(indices[i + 2] + 1)};
    if ((cross > 0) != (area[0] > 0))
      std::swap(triangle[1], triangle[2]);
    face.triangles.push_back(triangle);
  }
  long double expected = std::abs(area[0]);
  for (size_t i = 1; i < area.size(); ++i)
    expected -= std::abs(area[i]);
  if (expected <= 0 || std::abs(covered - expected) > 1e-8 * std::abs(area[0]) || face.triangles.empty())
    face.fallback = true;
}

struct Projection {
  double distance = 0;
  bool start = false, end = false, valid = false;
};

Projection project(const Ring &line, const XY &point, double tolerance) {
  Projection result;
  double best = std::numeric_limits<double>::infinity();
  long double station = 0;
  for (size_t i = 1; i < line.size(); ++i) {
    const auto &a = line[i - 1], &b = line[i];
    double dx = b[0] - a[0], dy = b[1] - a[1];
    double length2 = dx * dx + dy * dy, length = std::sqrt(length2);
    if (length2 > 0 && std::isfinite(length2)) {
      double fraction = std::max(0.0, std::min(1.0, ((point[0] - a[0]) * dx + (point[1] - a[1]) * dy) / length2));
      double ex = a[0] + fraction * dx - point[0], ey = a[1] + fraction * dy - point[1];
      double separation = ex * ex + ey * ey;
      if (separation < best) {
        best = separation;
        result.distance = static_cast<double>(station) + fraction * length;
        result.valid = true;
      }
    }
    station += length;
  }
  tolerance = std::max(tolerance, static_cast<double>(station) * 1e-10);
  result.start = result.distance <= tolerance;
  result.end = result.distance >= static_cast<double>(station) - tolerance;
  return result;
}

struct PointContact {
  XY point;
  Projection a, b;
};
struct RoadResult {
  std::vector<PointContact> points;
  std::vector<Ring> lines;
  std::string error;
};

void read_contacts(Context &context, const GEOSGeometry *geometry, const Ring &a, const Ring &b, double tolerance,
                   RoadResult &result) {
  if (GEOSisEmpty_r(context.handle, geometry))
    return;
  int type = GEOSGeomTypeId_r(context.handle, geometry);
  if (type == GEOS_POINT || type == GEOS_LINESTRING) {
    auto sequence = GEOSGeom_getCoordSeq_r(context.handle, geometry);
    unsigned count = 0;
    if (!sequence || !GEOSCoordSeq_getSize_r(context.handle, sequence, &count))
      throw std::runtime_error("Could not read road intersection coordinates.");
    Ring coordinates(count);
    for (unsigned i = 0; i < count; ++i) {
      GEOSCoordSeq_getX_r(context.handle, sequence, i, &coordinates[i][0]);
      GEOSCoordSeq_getY_r(context.handle, sequence, i, &coordinates[i][1]);
    }
    if (type == GEOS_LINESTRING)
      result.lines.push_back(std::move(coordinates));
    else if (count) {
      auto pa = project(a, coordinates[0], tolerance), pb = project(b, coordinates[0], tolerance);
      if (pa.valid && pb.valid)
        result.points.push_back({coordinates[0], pa, pb});
    }
  } else {
    int count = GEOSGetNumGeometries_r(context.handle, geometry);
    for (int i = 0; i < count; ++i)
      read_contacts(context, GEOSGetGeometryN_r(context.handle, geometry, i), a, b, tolerance, result);
  }
}
} // namespace scene_geometry

// [[Rcpp::export(rng = false)]]
Rcpp::List triangulate_polygon_faces_cpp(Rcpp::NumericMatrix coordinates, Rcpp::IntegerVector face_columns,
                                         int ring_column) {
  using namespace scene_geometry;
  if (coordinates.ncol() < 3 || ring_column < 1 || ring_column > coordinates.ncol() || face_columns.size() == 0)
    Rcpp::stop("Invalid polygon coordinate columns.");
  for (int column : face_columns)
    if (column < 1 || column > coordinates.ncol())
      Rcpp::stop("Invalid polygon face column.");
  std::vector<Face> faces;
  std::vector<std::array<int, 2>> spans;
  // sf's coordinate table orders feature, face, then ring contiguously. Read
  // those runs directly rather than allocating strings/factors and one R row
  // vector per face. Only exceptional faces need row ranges for the R repair.
  int row = 0;
  while (row < coordinates.nrow()) {
    if (row % 1024 == 0)
      Rcpp::checkUserInterrupt();
    int begin = row, end = row + 1;
    while (end < coordinates.nrow()) {
      bool same = true;
      for (int column : face_columns)
        if (coordinates(begin, column - 1) != coordinates(end, column - 1))
          same = false;
      if (!same)
        break;
      ++end;
    }
    Face face;
    while (row < end) {
      int ring_end = row + 1;
      while (ring_end < end && coordinates(row, ring_column - 1) == coordinates(ring_end, ring_column - 1))
        ++ring_end;
      std::vector<XYZ> ring;
      for (int i = row; i < ring_end; ++i) {
        XYZ point{coordinates(i, 0), coordinates(i, 1), coordinates(i, 2)};
        for (double value : point)
          if (!std::isfinite(value))
            face.fallback = true;
        ring.push_back(point);
      }
      size_t first = ring.size() > 1 && ring.front() == ring.back() ? 1 : 0;
      size_t count = 0;
      for (size_t i = first; i < ring.size(); ++i) {
        if (!count || face.vertices.back() != ring[i]) {
          face.vertices.push_back(ring[i]);
          ++count;
        }
      }
      face.sizes.push_back(count);
      row = ring_end;
    }
    faces.push_back(std::move(face));
    spans.push_back({begin + 1, end});
  }
  run_batch(faces.size(), [&](Context &context, size_t i) {
    if (faces[i].fallback)
      return;
    try {
      triangulate_face(context, faces[i]);
    } catch (const std::exception &) {
      faces[i].fallback = true;
    }
  });
  Rcpp::List output(faces.size());
  std::vector<int> fallback_ids, fallback_begin, fallback_end;
  for (size_t i = 0; i < faces.size(); ++i) {
    const auto &face = faces[i];
    if (face.fallback) {
      fallback_ids.push_back(i + 1);
      fallback_begin.push_back(spans[i][0]);
      fallback_end.push_back(spans[i][1]);
      continue;
    }
    Rcpp::NumericMatrix vertices(face.vertices.size(), 3);
    Rcpp::IntegerMatrix indices(face.triangles.size(), 3);
    for (size_t j = 0; j < face.vertices.size(); ++j)
      for (size_t k = 0; k < 3; ++k)
        vertices(j, k) = face.vertices[j][k];
    for (size_t j = 0; j < face.triangles.size(); ++j)
      for (size_t k = 0; k < 3; ++k)
        indices(j, k) = face.triangles[j][k];
    Rcpp::colnames(vertices) = Rcpp::CharacterVector::create("X", "Y", "Z");
    output[i] = Rcpp::List::create(Rcpp::_["vertices"] = vertices, Rcpp::_["indices"] = indices);
  }
  output.attr("fallback") = Rcpp::List::create(Rcpp::_["id"] = fallback_ids, Rcpp::_["begin"] = fallback_begin,
                                               Rcpp::_["end"] = fallback_end);
  return output;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List intersect_road_pairs_cpp(Rcpp::List geometry, Rcpp::IntegerMatrix pairs, double tolerance) {
  using namespace scene_geometry;
  if (pairs.ncol() != 2 || !std::isfinite(tolerance) || tolerance < 0)
    Rcpp::stop("Invalid road intersection inputs.");
  std::vector<Ring> lines(geometry.size());
  for (int i = 0; i < geometry.size(); ++i) {
    Rcpp::NumericMatrix coordinates = geometry[i];
    if (coordinates.ncol() < 2 || coordinates.nrow() < 2)
      Rcpp::stop("Road fragments need two XY coordinates.");
    for (int j = 0; j < coordinates.nrow(); ++j) {
      XY point{coordinates(j, 0), coordinates(j, 1)};
      if (!std::isfinite(point[0]) || !std::isfinite(point[1]))
        Rcpp::stop("Road coordinates must be finite.");
      lines[i].push_back(point);
    }
  }
  std::vector<std::array<int, 2>> jobs(pairs.nrow());
  for (int i = 0; i < pairs.nrow(); ++i)
    for (int j = 0; j < 2; ++j) {
      int index = pairs(i, j) - 1;
      if (index < 0 || index >= geometry.size())
        Rcpp::stop("Invalid road fragment index.");
      jobs[i][j] = index;
    }
  std::vector<RoadResult> results(jobs.size());
  run_batch(jobs.size(), [&](Context &context, size_t i) {
    try {
      const auto &a = lines[jobs[i][0]], &b = lines[jobs[i][1]];
      auto ga = make_line(context, a), gb = make_line(context, b);
      auto intersection = own(context, GEOSIntersection_r(context.handle, ga.get(), gb.get()));
      read_contacts(context, intersection.get(), a, b, tolerance, results[i]);
    } catch (const std::exception &error) {
      results[i] = RoadResult();
      results[i].error = error.what();
    }
  });
  size_t count = 0, overlaps = 0;
  for (const auto &result : results) {
    count += result.points.size();
    overlaps += result.lines.size();
  }
  // Columns are pair ID, XY, station A/B, start/end A, start/end B. Keep source
  // pair and GEOS component order so existing event IDs remain stable.
  Rcpp::NumericMatrix points(count, 9);
  Rcpp::IntegerVector overlap_pairs(overlaps);
  Rcpp::List overlap_lines(overlaps);
  Rcpp::CharacterVector errors(results.size());
  size_t row = 0, overlap = 0;
  for (size_t i = 0; i < results.size(); ++i) {
    errors[i] = results[i].error;
    for (const auto &point : results[i].points) {
      points(row, 0) = i + 1;
      points(row, 1) = point.point[0];
      points(row, 2) = point.point[1];
      points(row, 3) = point.a.distance;
      points(row, 4) = point.b.distance;
      points(row, 5) = point.a.start;
      points(row, 6) = point.a.end;
      points(row, 7) = point.b.start;
      points(row, 8) = point.b.end;
      ++row;
    }
    for (const auto &line : results[i].lines) {
      Rcpp::NumericMatrix xy(line.size(), 2);
      for (size_t j = 0; j < line.size(); ++j)
        for (size_t k = 0; k < 2; ++k)
          xy(j, k) = line[j][k];
      overlap_pairs[overlap] = i + 1;
      overlap_lines[overlap] = xy;
      ++overlap;
    }
  }
  return Rcpp::List::create(Rcpp::_["points"] = points, Rcpp::_["overlap_pairs"] = overlap_pairs,
                            Rcpp::_["overlap_lines"] = overlap_lines, Rcpp::_["errors"] = errors);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List score_road_continuations_cpp(Rcpp::NumericMatrix xy_direction, Rcpp::IntegerMatrix metadata,
                                        Rcpp::IntegerVector feature, Rcpp::IntegerVector lanes,
                                        Rcpp::IntegerMatrix pairs, double endpoint_tolerance,
                                        double minimum_direction_score) {
  using namespace scene_geometry;
  const int n = xy_direction.nrow(), count = pairs.nrow();
  if (xy_direction.ncol() != 4 || metadata.nrow() != n || metadata.ncol() != 4 || feature.size() != n ||
      lanes.size() != n || pairs.ncol() != 2)
    Rcpp::stop("Invalid road continuation columns.");
  struct Endpoint {
    XY xy, direction;
    std::array<int, 4> metadata;
    int feature, lanes;
  };
  struct Score {
    double gap = 0, direction = 0;
    std::array<bool, 4> same{false, false, false, false};
    bool exact = false, lane_evidence = false, same_lanes = false, eligible = false;
    int lane_a = 0, lane_b = 0, lane_difference = 0, evidence = 0;
    std::string tier, subclass, metadata_match, reason;
  };
  std::vector<Endpoint> endpoints(n);
  for (int i = 0; i < n; ++i) {
    endpoints[i].xy = {xy_direction(i, 0), xy_direction(i, 1)};
    endpoints[i].direction = {xy_direction(i, 2), xy_direction(i, 3)};
    for (int j = 0; j < 4; ++j)
      endpoints[i].metadata[j] = metadata(i, j);
    endpoints[i].feature = feature[i];
    endpoints[i].lanes = lanes[i];
  }
  std::vector<std::array<int, 2>> jobs(count);
  for (int i = 0; i < count; ++i)
    for (int j = 0; j < 2; ++j) {
      jobs[i][j] = pairs(i, j) - 1;
      if (jobs[i][j] < 0 || jobs[i][j] >= n)
        Rcpp::stop("Invalid continuation endpoint index.");
    }
  std::vector<Score> scores(count);
  run_batch(count, [&](Context &, size_t i) {
    const auto &a = endpoints[jobs[i][0]], &b = endpoints[jobs[i][1]];
    auto &s = scores[i];
    double dx = b.xy[0] - a.xy[0], dy = b.xy[1] - a.xy[1];
    s.gap = std::sqrt(dx * dx + dy * dy);
    s.exact = s.gap <= endpoint_tolerance;
    // Preserve the scalar policy: both inward tangents must agree with the gap
    // and with each other; exact shared endpoints have no gap-direction test.
    double through = -(a.direction[0] * b.direction[0] + a.direction[1] * b.direction[1]);
    double alignment_a = 1, alignment_b = 1;
    if (!s.exact) {
      double gx = dx / s.gap, gy = dy / s.gap;
      alignment_a = -(gx * a.direction[0] + gy * a.direction[1]);
      alignment_b = gx * b.direction[0] + gy * b.direction[1];
    }
    s.direction = std::min(through, std::min(alignment_a, alignment_b));
    if (!std::isfinite(through) || !std::isfinite(alignment_a) || !std::isfinite(alignment_b))
      s.direction = std::numeric_limits<double>::quiet_NaN();
    for (size_t j = 0; j < 4; ++j)
      s.same[j] = a.metadata[j] != NA_INTEGER && b.metadata[j] != NA_INTEGER && a.metadata[j] == b.metadata[j];
    s.same[0] = s.same[0] || a.feature == b.feature;
    s.lane_a = a.lanes;
    s.lane_b = b.lanes;
    s.lane_evidence = a.lanes != NA_INTEGER && b.lanes != NA_INTEGER;
    s.same_lanes = s.lane_evidence && a.lanes == b.lanes;
    s.lane_difference = s.lane_evidence ? std::abs(a.lanes - b.lanes) : NA_INTEGER;
    const char *labels[] = {"way", "ref", "name", "highway"};
    for (size_t j = 0; j < 4; ++j)
      if (s.same[j]) {
        if (!s.metadata_match.empty())
          s.metadata_match += "+";
        s.metadata_match += labels[j];
      }
    if (s.metadata_match.empty())
      s.metadata_match = "direction";
    int rank = s.same[0] ? 5 : s.same[1] ? 4 : s.same[2] ? 3 : s.same[3] ? 2 : 1;
    const char *tiers[] = {"direction", "same_highway", "same_name", "same_ref", "same_way"};
    s.evidence = (s.exact ? 10 : 0) + rank;
    s.tier = std::string(s.exact ? "exact_" : "gap_") + tiers[rank - 1];
    s.subclass = s.tier + (s.same_lanes ? "_same_lanes" : s.lane_evidence ? "_different_lanes" : "_unknown_lanes");
    s.eligible = std::isfinite(s.direction) && s.direction >= minimum_direction_score;
    s.reason = !s.eligible ? "direction_mismatch"
               : s.exact   ? "exact_shared_endpoint"
               : s.same[0] ? "same_way"
               : s.same[1] ? "matching_ref"
               : s.same[2] ? "matching_name"
               : s.same[3] ? "compatible_highway"
                           : "unique_direction";
  });
  Rcpp::NumericVector gap(count), direction(count), selection(count);
  Rcpp::LogicalVector same_way(count), same_ref(count), same_name(count), same_highway(count), lane_evidence(count),
      same_lanes(count), exact(count), eligible(count);
  Rcpp::IntegerVector lane_a(count), lane_b(count), difference(count), continuity(count), evidence(count);
  Rcpp::CharacterVector tier(count), subclass(count), matches(count), reason(count), status(count);
  for (int i = 0; i < count; ++i) {
    const auto &s = scores[i];
    gap[i] = s.gap;
    direction[i] = s.direction;
    same_way[i] = s.same[0];
    same_ref[i] = s.same[1];
    same_name[i] = s.same[2];
    same_highway[i] = s.same[3];
    lane_a[i] = s.lane_a;
    lane_b[i] = s.lane_b;
    difference[i] = s.lane_difference;
    lane_evidence[i] = s.lane_evidence;
    same_lanes[i] = s.same_lanes;
    continuity[i] = s.same_lanes;
    exact[i] = s.exact;
    evidence[i] = s.evidence;
    tier[i] = s.tier;
    subclass[i] = s.subclass;
    matches[i] = s.metadata_match;
    reason[i] = s.reason;
    eligible[i] = s.eligible;
    status[i] = s.eligible ? "candidate" : "rejected";
    selection[i] = s.evidence * 1000 + (s.same_lanes ? 100 : 0) + s.direction;
  }
  return Rcpp::List::create(
      Rcpp::_["endpoint_distance"] = gap, Rcpp::_["direction_score"] = direction, Rcpp::_["same_way"] = same_way,
      Rcpp::_["same_ref"] = same_ref, Rcpp::_["same_name"] = same_name, Rcpp::_["same_highway"] = same_highway,
      Rcpp::_["lane_count_a"] = lane_a, Rcpp::_["lane_count_b"] = lane_b, Rcpp::_["lane_evidence"] = lane_evidence,
      Rcpp::_["same_lanes"] = same_lanes, Rcpp::_["lane_difference"] = difference,
      Rcpp::_["lane_continuity_rank"] = continuity, Rcpp::_["exact_endpoint"] = exact,
      Rcpp::_["evidence_rank"] = evidence, Rcpp::_["evidence_tier"] = tier, Rcpp::_["evidence_subclass"] = subclass,
      Rcpp::_["metadata_match"] = matches, Rcpp::_["selection_score"] = selection, Rcpp::_["selection_reason"] = reason,
      Rcpp::_["eligible"] = eligible, Rcpp::_["status"] = status);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List select_road_candidates_cpp(Rcpp::IntegerMatrix endpoint_rows, Rcpp::LogicalVector eligible,
                                      Rcpp::IntegerVector evidence, Rcpp::IntegerVector lanes,
                                      Rcpp::NumericVector direction, int endpoint_count, double margin) {
  const int count = endpoint_rows.nrow();
  if (endpoint_count < 0 || endpoint_rows.ncol() != 2 || eligible.size() != count || evidence.size() != count ||
      lanes.size() != count || direction.size() != count)
    Rcpp::stop("Invalid road candidate columns.");
  // Index eligible incident candidates once. The previous R loop rescanned all
  // candidates for every endpoint, making a sparse network unnecessarily quadratic.
  std::vector<std::vector<int>> incident(endpoint_count);
  std::vector<int> eligible_rows;
  for (int i = 0; i < count; ++i) {
    for (int j = 0; j < 2; ++j)
      if (endpoint_rows(i, j) < 1 || endpoint_rows(i, j) > endpoint_count)
        Rcpp::stop("Invalid candidate endpoint row.");
    if (eligible[i] != TRUE)
      continue;
    eligible_rows.push_back(i + 1);
    incident[endpoint_rows(i, 0) - 1].push_back(i);
    if (endpoint_rows(i, 0) != endpoint_rows(i, 1))
      incident[endpoint_rows(i, 1) - 1].push_back(i);
  }
  Rcpp::IntegerVector best(endpoint_count);
  Rcpp::LogicalVector ambiguous_endpoint(endpoint_count), ambiguous_candidate(count);
  for (int endpoint = 0; endpoint < endpoint_count; ++endpoint) {
    if (endpoint % 1024 == 0)
      Rcpp::checkUserInterrupt();
    const auto &rows = incident[endpoint];
    if (rows.empty())
      continue;
    int highest_evidence = std::numeric_limits<int>::min(), highest_lanes = -1;
    double best_direction = -std::numeric_limits<double>::infinity();
    for (int row : rows)
      highest_evidence = std::max(highest_evidence, evidence[row]);
    for (int row : rows)
      if (evidence[row] == highest_evidence)
        highest_lanes = std::max(highest_lanes, lanes[row]);
    for (int row : rows)
      if (evidence[row] == highest_evidence && lanes[row] == highest_lanes)
        best_direction = std::max(best_direction, direction[row]);
    std::vector<int> competing;
    for (int row : rows)
      if (evidence[row] == highest_evidence && lanes[row] == highest_lanes && direction[row] >= best_direction - margin)
        competing.push_back(row);
    if (competing.size() == 1)
      best[endpoint] = competing[0] + 1;
    else {
      ambiguous_endpoint[endpoint] = true;
      for (int row : competing)
        ambiguous_candidate[row] = true;
    }
  }
  return Rcpp::List::create(Rcpp::_["eligible_rows"] = eligible_rows, Rcpp::_["best_candidate"] = best,
                            Rcpp::_["ambiguous_endpoint"] = ambiguous_endpoint,
                            Rcpp::_["ambiguous_candidate"] = ambiguous_candidate);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List summarize_road_events_cpp(Rcpp::IntegerVector membership, Rcpp::NumericMatrix xy,
                                     Rcpp::IntegerMatrix fragments, Rcpp::NumericMatrix stations,
                                     Rcpp::NumericVector layers) {
  const int count = membership.size();
  if (xy.nrow() != count || xy.ncol() != 2 || fragments.nrow() != count || fragments.ncol() != 2 ||
      stations.nrow() != count || stations.ncol() != 2)
    Rcpp::stop("Invalid road event columns.");
  int group_count = 0;
  for (int value : membership) {
    if (value < 1 || value > count)
      Rcpp::stop("Invalid event membership.");
    group_count = std::max(group_count, value);
  }
  std::vector<std::vector<int>> groups(group_count);
  for (int i = 0; i < count; ++i) {
    groups[membership[i] - 1].push_back(i);
    for (int j = 0; j < 2; ++j)
      if (fragments(i, j) < 1 || fragments(i, j) > layers.size())
        Rcpp::stop("Invalid event fragment row.");
  }
  Rcpp::NumericMatrix events(group_count, 6);
  std::vector<std::array<double, 3>> participants;
  // Reuse dense scratch indexing across groups; only entries touched by the
  // current group are initialized. Participant order is all A then all B, as
  // unique(c(fragment_a, fragment_b)) in the original implementation.
  std::vector<int> seen(layers.size(), -1), slots(layers.size());
  for (int g = 0; g < group_count; ++g) {
    if (g % 1024 == 0)
      Rcpp::checkUserInterrupt();
    const auto &rows = groups[g];
    if (rows.empty())
      Rcpp::stop("Road event membership must be contiguous.");
    std::vector<int> members, observations;
    std::vector<long double> sums;
    long double x = 0, y = 0;
    for (int row : rows) {
      x += xy(row, 0);
      y += xy(row, 1);
    }
    double mean_x = static_cast<double>(x / rows.size()), mean_y = static_cast<double>(y / rows.size());
    double spread2 = 0;
    for (size_t i = 0; i < rows.size(); ++i)
      for (size_t j = i + 1; j < rows.size(); ++j) {
        double dx = xy(rows[i], 0) - xy(rows[j], 0), dy = xy(rows[i], 1) - xy(rows[j], 1);
        spread2 = std::max(spread2, dx * dx + dy * dy);
      }
    for (int side = 0; side < 2; ++side)
      for (int row : rows) {
        int member = fragments(row, side) - 1;
        if (seen[member] != g) {
          seen[member] = g;
          slots[member] = members.size();
          members.push_back(member);
          sums.push_back(0);
          observations.push_back(0);
        }
        int slot = slots[member];
        sums[slot] += stations(row, side);
        ++observations[slot];
      }
    double min_layer = std::numeric_limits<double>::infinity(), max_layer = -min_layer;
    for (size_t i = 0; i < members.size(); ++i) {
      min_layer = std::min(min_layer, layers[members[i]]);
      max_layer = std::max(max_layer, layers[members[i]]);
      participants.push_back({double(g + 1), double(members[i] + 1), double(sums[i] / observations[i])});
    }
    events(g, 0) = mean_x;
    events(g, 1) = mean_y;
    events(g, 2) = members.size();
    events(g, 3) = min_layer;
    events(g, 4) = max_layer;
    events(g, 5) = std::sqrt(spread2);
  }
  Rcpp::NumericMatrix participant_matrix(participants.size(), 3);
  for (size_t i = 0; i < participants.size(); ++i)
    for (int j = 0; j < 3; ++j)
      participant_matrix(i, j) = participants[i][j];
  return Rcpp::List::create(Rcpp::_["events"] = events, Rcpp::_["participants"] = participant_matrix);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List road_point_neighbors_cpp(Rcpp::NumericMatrix xy, double tolerance) {
  using namespace scene_geometry;
  if (xy.ncol() != 2 || !std::isfinite(tolerance) || tolerance < 0)
    Rcpp::stop("Invalid road neighbor coordinates or tolerance.");
  const size_t count = xy.nrow();
  Context context;
  std::vector<Geometry> points;
  std::vector<size_t> indices(count);
  // Build/query one native spatial index instead of allocating an R polygon
  // and sf wrappers for every point's tolerance-expanded search envelope.
  auto destroy_tree = [&](GEOSSTRtree *tree) { GEOSSTRtree_destroy_r(context.handle, tree); };
  std::unique_ptr<GEOSSTRtree, decltype(destroy_tree)> tree(GEOSSTRtree_create_r(context.handle, 10), destroy_tree);
  if (!tree)
    Rcpp::stop("Could not allocate road neighbor index.");
  for (size_t i = 0; i < count; ++i) {
    if (i % 1024 == 0)
      Rcpp::checkUserInterrupt();
    if (!std::isfinite(xy(i, 0)) || !std::isfinite(xy(i, 1)))
      Rcpp::stop("Road neighbor coordinates must be finite.");
    indices[i] = i;
    points.push_back(own(context, GEOSGeom_createPointFromXY_r(context.handle, xy(i, 0), xy(i, 1))));
    GEOSSTRtree_insert_r(context.handle, tree.get(), points.back().get(), &indices[i]);
  }
  Rcpp::List result(count);
  for (size_t i = 0; i < count; ++i) {
    if (i % 1024 == 0)
      Rcpp::checkUserInterrupt();
    double x = xy(i, 0), y = xy(i, 1);
    double padding = 8 * std::numeric_limits<double>::epsilon() * std::max({1.0, std::abs(x), std::abs(y), tolerance});
    double radius = tolerance + padding;
    auto box = own(context, GEOSGeom_createRectangle_r(context.handle, x - radius, y - radius, x + radius, y + radius));
    std::vector<size_t> candidates;
    GEOSSTRtree_query_r(
        context.handle, tree.get(), box.get(),
        [](void *item, void *data) {
          static_cast<std::vector<size_t> *>(data)->push_back(*static_cast<size_t *>(item));
        },
        &candidates);
    std::vector<int> neighbors;
    for (size_t j : candidates) {
      double dx = xy(j, 0) - x, dy = xy(j, 1) - y;
      if (std::sqrt(dx * dx + dy * dy) <= tolerance)
        neighbors.push_back(j + 1);
    }
    std::sort(neighbors.begin(), neighbors.end());
    result[i] = Rcpp::wrap(neighbors);
  }
  return result;
}
