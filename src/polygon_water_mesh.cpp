#include <Rcpp.h>
#include <RcppThread.h>
#include <array>
#include <atomic>
#include <cmath>
#include <cstdint>
#include <earcut.h>
#include <limits>
#include <map>
#include <memory>
#include <numeric>
#include <stdexcept>
#include <string>
#include <vector>

// Resolve libgeos's registered C callables once on R's main thread at load.
// Keep the function-pointer definitions local to this namespace: other packages
// may use a different GEOS provider in the same process. Never exchange GEOS
// objects across those providers; this file creates all geometry from coordinates.
namespace rayshader_geos {
#include <libgeos.c>
#include <libgeos.h>
} // namespace rayshader_geos

// [[Rcpp::export(rng = false)]]
void initialize_water_geos_cpp() {
  rayshader_geos::libgeos_init_api();
}

#ifdef __clang__
#pragma clang fp contract(off)
#endif

namespace water_threads {

using namespace rayshader_geos;

using Point = std::array<double, 2>;
using XYZ = std::array<double, 3>;
using Ring = std::vector<Point>;
using Rings = std::vector<Ring>;

// Each thread owns its GEOS context and every GEOS geometry it constructs.
// The handler stores errors in native memory; it never invokes R's handlers.
struct Context {
  GEOSContextHandle_t handle;
  std::string error;

  Context() : handle(GEOS_init_r()) {
    if (!handle)
      throw std::runtime_error("Could not initialize GEOS context.");
    GEOSContext_setErrorMessageHandler_r(handle, receive_error, this);
    GEOSContext_setNoticeMessageHandler_r(handle, ignore_notice, nullptr);
  }

  ~Context() {
    GEOS_finish_r(handle);
  }
  Context(const Context &) = delete;
  Context &operator=(const Context &) = delete;

  static void receive_error(const char *message, void *data) {
    static_cast<Context *>(data)->error = message;
  }
  static void ignore_notice(const char *, void *) {}

  void require(bool ok, const char *operation) const {
    if (!ok)
      throw std::runtime_error(error.empty() ? operation : error);
  }
};

struct GeometryDeleter {
  Context *context;
  void operator()(GEOSGeometry *geometry) const {
    if (geometry)
      GEOSGeom_destroy_r(context->handle, geometry);
  }
};
using Geometry = std::unique_ptr<GEOSGeometry, GeometryDeleter>;

Geometry own(Context &context, GEOSGeometry *geometry) {
  context.require(geometry != nullptr, "GEOS geometry construction failed.");
  return Geometry(geometry, GeometryDeleter{&context});
}

Geometry make_ring(Context &context, const Ring &points) {
  auto seq = GEOSCoordSeq_create_r(context.handle, points.size(), 2);
  context.require(seq != nullptr, "GEOS coordinate allocation failed.");
  for (size_t i = 0; i < points.size(); ++i) {
    GEOSCoordSeq_setX_r(context.handle, seq, i, points[i][0]);
    GEOSCoordSeq_setY_r(context.handle, seq, i, points[i][1]);
  }
  // GEOS assumes ownership of the coordinate sequence.
  return own(context, GEOSGeom_createLinearRing_r(context.handle, seq));
}

Geometry make_polygon(Context &context, const Rings &rings) {
  if (rings.empty())
    throw std::runtime_error("Empty polygons cannot be extruded.");
  auto shell = make_ring(context, rings.front());
  std::vector<Geometry> holes;
  std::vector<GEOSGeometry *> pointers;
  for (size_t i = 1; i < rings.size(); ++i) {
    holes.push_back(make_ring(context, rings[i]));
    pointers.push_back(holes.back().get());
  }
  auto polygon =
      GEOSGeom_createPolygon_r(context.handle, shell.get(), pointers.data(), pointers.size());
  context.require(polygon != nullptr, "GEOS polygon construction failed.");
  shell.release();
  for (auto &hole : holes)
    hole.release();
  return own(context, polygon);
}

Ring read_ring(Context &context, const GEOSGeometry *ring) {
  auto seq = GEOSGeom_getCoordSeq_r(context.handle, ring);
  unsigned count = 0;
  context.require(seq && GEOSCoordSeq_getSize_r(context.handle, seq, &count),
                  "GEOS ring coordinates unavailable.");
  Ring points(count);
  for (unsigned i = 0; i < count; ++i) {
    GEOSCoordSeq_getX_r(context.handle, seq, i, &points[i][0]);
    GEOSCoordSeq_getY_r(context.handle, seq, i, &points[i][1]);
  }
  return points;
}

Rings read_polygon(Context &context, const GEOSGeometry *polygon) {
  Rings rings;
  rings.push_back(read_ring(context, GEOSGetExteriorRing_r(context.handle, polygon)));
  int holes = GEOSGetNumInteriorRings_r(context.handle, polygon);
  for (int i = 0; i < holes; ++i) {
    rings.push_back(read_ring(context, GEOSGetInteriorRingN_r(context.handle, polygon, i)));
  }
  return rings;
}

// Intersections may contain line/point contacts or polygon collections. Only
// positive-area polygon components support water, matching sf extraction.
template <class Visitor>
void visit_polygons(Context &context, const GEOSGeometry *geometry, Visitor &visit) {
  int type = GEOSGeomTypeId_r(context.handle, geometry);
  if (type == GEOS_POLYGON) {
    double area = 0;
    context.require(GEOSArea_r(context.handle, geometry, &area), "GEOS area failed.");
    if (area > 0)
      visit(geometry);
  } else if (type == GEOS_MULTIPOLYGON || type == GEOS_GEOMETRYCOLLECTION) {
    int count = GEOSGetNumGeometries_r(context.handle, geometry);
    for (int i = 0; i < count; ++i) {
      visit_polygons(context, GEOSGetGeometryN_r(context.handle, geometry, i), visit);
    }
  }
}

struct Triangle {
  XYZ a, b, c;
  double xmin, xmax, zmin, zmax;
};
struct Terrain {
  std::vector<Triangle> triangles;
  std::vector<std::vector<int>> faces;
  Point origin, step;
  int bins;
  double magnitude;
  std::vector<Rings> coverage;
  std::string coverage_error;
  size_t boundary_edges = 0;
  size_t coverage_faces = 0;
};
struct Footprint {
  Rings rings;
  double level;
  double xmin, xmax, zmin, zmax;
};
struct Result {
  std::vector<XYZ> vertices, lines;
  std::string error;
  size_t boundary_faces = 0, interior_faces = 0;
};

// Native per-thread scratch marks avoid allocating a hash table or clearing a
// triangle-sized vector for each polygon. A polygon's input index is its stamp.
std::vector<int> candidate_faces(const Footprint &polygon, const Terrain &terrain,
                                 std::vector<size_t> &seen, size_t stamp) {
  int x0 = std::max(
      0, static_cast<int>(std::floor((polygon.xmin - terrain.origin[0]) / terrain.step[0])));
  int z0 = std::max(
      0, static_cast<int>(std::floor((polygon.zmin - terrain.origin[1]) / terrain.step[1])));
  int x1 =
      std::min(terrain.bins - 1,
               static_cast<int>(std::floor((polygon.xmax - terrain.origin[0]) / terrain.step[0])));
  int z1 =
      std::min(terrain.bins - 1,
               static_cast<int>(std::floor((polygon.zmax - terrain.origin[1]) / terrain.step[1])));
  std::vector<int> ids;
  for (int z = z0; z <= z1; ++z) {
    for (int x = x0; x <= x1; ++x) {
      for (int id : terrain.faces[x + terrain.bins * z]) {
        if (seen[id] == stamp)
          continue;
        seen[id] = stamp;
        const auto &t = terrain.triangles[id];
        if (t.xmax >= polygon.xmin && t.xmin <= polygon.xmax && t.zmax >= polygon.zmin &&
            t.zmin <= polygon.zmax)
          ids.push_back(id);
      }
    }
  }
  return ids;
}

double sample_height(const Triangle &t, const Point &p) {
  double det = (t.b[0] - t.a[0]) * (t.c[2] - t.a[2]) - (t.b[2] - t.a[2]) * (t.c[0] - t.a[0]);
  double u = ((p[0] - t.a[0]) * (t.c[2] - t.a[2]) - (p[1] - t.a[2]) * (t.c[0] - t.a[0])) / det;
  double v = ((t.b[0] - t.a[0]) * (p[1] - t.a[2]) - (t.b[2] - t.a[2]) * (p[0] - t.a[0])) / det;
  return t.a[1] + u * (t.b[1] - t.a[1]) + v * (t.c[1] - t.a[1]);
}

bool on_shore(const Point &p, const Ring &exterior, double tolerance) {
  for (size_t i = 1; i < exterior.size(); ++i) {
    const auto &a = exterior[i - 1];
    const auto &b = exterior[i];
    double dx = b[0] - a[0], dz = b[1] - a[1];
    double length2 = dx * dx + dz * dz;
    double distance;
    double fraction = length2 == 0 ? 0 : ((p[0] - a[0]) * dx + (p[1] - a[1]) * dz) / length2;
    if (fraction <= 0) {
      distance = std::sqrt((p[0] - a[0]) * (p[0] - a[0]) + (p[1] - a[1]) * (p[1] - a[1]));
    } else if (fraction >= 1) {
      distance = std::sqrt((p[0] - b[0]) * (p[0] - b[0]) + (p[1] - b[1]) * (p[1] - b[1]));
    } else {
      double s = ((a[1] - p[1]) * dx - (a[0] - p[0]) * dz) / length2;
      distance = std::abs(s) * std::sqrt(length2);
    }
    if (distance <= tolerance)
      return true;
  }
  return false;
}

double median(std::vector<double> &values) {
  size_t middle = values.size() / 2;
  std::nth_element(values.begin(), values.begin() + middle, values.end());
  double result = values[middle];
  if (values.size() % 2 == 0) {
    result = (*std::max_element(values.begin(), values.begin() + middle) + result) / 2;
  }
  return result;
}

// Extrusion follows rayvertex's ring cleanup, edge validation, winding and cap
// boundary checks. Those checks are retained rather than relying on earcut to
// repair invalid input or accepting a cap with unmatched wall vertices.
// Ring coordinates are translated/scaled before predicates to limit cancellation.
Ring normalized_coordinates(const Ring &points) {
  Ring local(points.size());
  double size = 0;
  for (size_t i = 0; i < points.size(); ++i) {
    local[i] = {points[i][0] - points[0][0], points[i][1] - points[0][1]};
    size = std::max({size, std::abs(local[i][0]), std::abs(local[i][1])});
  }
  if (!(size > 0) || !std::isfinite(size))
    throw std::runtime_error("Polygon coordinate extent must be finite and nonzero.");
  for (auto &p : local) {
    p[0] /= size;
    p[1] /= size;
  }
  return local;
}

double signed_area(const Ring &points) {
  auto q = normalized_coordinates(points);
  long double sum = 0;
  for (size_t i = 0; i < q.size(); ++i) {
    auto &next = q[(i + 1) % q.size()];
    sum += q[i][0] * next[1] - q[i][1] * next[0];
  }
  return static_cast<double>(sum / 2);
}

Ring clean_ring(const Ring &input) {
  Ring p;
  for (auto q : input) {
    if (!std::isfinite(q[0]) || !std::isfinite(q[1]))
      throw std::runtime_error(
          "Each polygon ring must contain at least three finite x/y vertices.");
    if (p.empty() || q != p.back())
      p.push_back(q);
  }
  if (p.size() > 1 && p.front() == p.back())
    p.pop_back();
  if (p.size() < 3)
    throw std::runtime_error("Each polygon ring needs at least three distinct vertices.");
  for (;;) {
    auto q = normalized_coordinates(p);
    std::vector<bool> remove(p.size(), false);
    size_t count = 0;
    for (size_t i = 0; i < p.size(); ++i) {
      auto prev = q[(i + p.size() - 1) % p.size()];
      auto next = q[(i + 1) % p.size()];
      double ax = q[i][0] - prev[0], ay = q[i][1] - prev[1];
      double bx = next[0] - q[i][0], by = next[1] - q[i][1];
      bool straight = ax * by == ay * bx;
      double dot = ax * bx + ay * by;
      if (straight && dot < 0)
        throw std::runtime_error("Polygon rings must not self-intersect or retrace an edge.");
      if (straight && dot > 0) {
        remove[i] = true;
        ++count;
      }
    }
    if (!count || p.size() - count < 3)
      break;
    Ring retained;
    for (size_t i = 0; i < p.size(); ++i)
      if (!remove[i])
        retained.push_back(p[i]);
    p.swap(retained);
  }
  if (signed_area(p) == 0)
    throw std::runtime_error("Polygon rings must have nonzero area and must not self-intersect.");
  return p;
}

bool contains(const Ring &ring, const Point &point) {
  int crossings = 0;
  for (size_t i = 0; i < ring.size(); ++i) {
    const auto &a = ring[i];
    const auto &b = ring[(i + 1) % ring.size()];
    double ay = a[1] - point[1], by = b[1] - point[1];
    if ((ay > 0) == (by > 0))
      continue;
    double ax = a[0] - point[0], bx = b[0] - point[0];
    if (ax + (bx - ax) * (-ay / (by - ay)) > 0)
      ++crossings;
  }
  return crossings % 2 == 1;
}

int orientation(const Point &a, const Point &b, const Point &c) {
  double left = (b[0] - a[0]) * (c[1] - a[1]);
  double right = (b[1] - a[1]) * (c[0] - a[0]);
  double cross = left - right;
  if (std::abs(cross) <=
      8 * std::numeric_limits<double>::epsilon() * (std::abs(left) + std::abs(right)))
    return 0;
  return cross > 0 ? 1 : -1;
}

void validate_component(Rings &rings) {
  Ring points;
  std::vector<size_t> next;
  for (auto &ring : rings) {
    ring = clean_ring(ring);
    size_t start = points.size();
    for (size_t i = 0; i < ring.size(); ++i)
      next.push_back(start + (i + 1) % ring.size());
    points.insert(points.end(), ring.begin(), ring.end());
  }
  // Sweep overlapping edge bounds; adjacent edges share their endpoint by design.
  auto p = normalized_coordinates(points);
  std::vector<size_t> order(p.size()), active;
  std::iota(order.begin(), order.end(), 0);
  auto xmin = [&](size_t i) { return std::min(p[i][0], p[next[i]][0]); };
  auto xmax = [&](size_t i) { return std::max(p[i][0], p[next[i]][0]); };
  std::stable_sort(order.begin(), order.end(),
                   [&](size_t a, size_t b) { return xmin(a) < xmin(b); });
  for (size_t i : order) {
    active.erase(
        std::remove_if(active.begin(), active.end(), [&](size_t j) { return xmax(j) < xmin(i); }),
        active.end());
    for (size_t j : active) {
      if (next[j] == i || next[i] == j)
        continue;
      if (std::min(p[j][1], p[next[j]][1]) > std::max(p[i][1], p[next[i]][1]) ||
          std::max(p[j][1], p[next[j]][1]) < std::min(p[i][1], p[next[i]][1]))
        continue;
      if (orientation(p[j], p[next[j]], p[i]) * orientation(p[j], p[next[j]], p[next[i]]) <= 0 &&
          orientation(p[i], p[next[i]], p[j]) * orientation(p[i], p[next[i]], p[next[j]]) <= 0) {
        throw std::runtime_error(
            "Polygon rings must not self-intersect, cross, or touch other rings.");
      }
    }
    active.push_back(i);
  }
  for (size_t i = 1; i < rings.size(); ++i) {
    if (!contains(rings[0], rings[i][0]))
      throw std::runtime_error("Every hole must lie strictly inside its outer polygon.");
    for (size_t j = 1; j < i; ++j) {
      if (contains(rings[j], rings[i][0]) || contains(rings[i], rings[j][0]))
        throw std::runtime_error("Polygon holes must not overlap or contain other holes.");
    }
  }
  for (size_t i = 0; i < rings.size(); ++i) {
    if ((signed_area(rings[i]) > 0) != (i == 0))
      std::reverse(rings[i].begin() + 1, rings[i].end());
  }
}

void extrude(Rings rings, double top, double bottom, Result &output) {
  validate_component(rings);
  Ring points;
  std::vector<uint32_t> next;
  for (auto &ring : rings) {
    size_t start = points.size();
    for (size_t i = 0; i < ring.size(); ++i)
      next.push_back(start + (i + 1) % ring.size());
    points.insert(points.end(), ring.begin(), ring.end());
  }
  auto local = normalized_coordinates(points);
  Rings local_rings;
  size_t offset = 0;
  for (const auto &ring : rings) {
    local_rings.emplace_back(local.begin() + offset, local.begin() + offset + ring.size());
    offset += ring.size();
  }
  // Earcut uses the cleaned, normalized rings. Validate every cap edge against
  // the same rings used for walls so a rejected cap cannot leave an open solid.
  auto caps = mapbox::earcut<uint32_t>(local_rings);
  if (caps.empty())
    throw std::runtime_error("Polygon could not be triangulated.");
  std::map<std::pair<uint32_t, uint32_t>, int> edges;
  auto key = [](uint32_t a, uint32_t b) { return std::make_pair(std::min(a, b), std::max(a, b)); };
  for (size_t i = 0; i < caps.size(); i += 3) {
    auto a = local[caps[i]], b = local[caps[i + 1]], c = local[caps[i + 2]];
    double area = (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0]);
    if (area == 0)
      throw std::runtime_error("Polygon triangulation produced a degenerate cap.");
    if (area < 0)
      std::swap(caps[i + 1], caps[i + 2]);
    for (int k = 0; k < 3; ++k)
      ++edges[key(caps[i + k], caps[i + (k + 1) % 3])];
  }
  for (size_t i = 0; i < next.size(); ++i) {
    auto found = edges.find(key(i, next[i]));
    if (found == edges.end() || found->second != 1)
      throw std::runtime_error("Polygon triangulation did not preserve a closed cap boundary.");
    edges.erase(found);
  }
  for (const auto &edge : edges)
    if (edge.second != 2)
      throw std::runtime_error("Polygon triangulation did not preserve a closed cap boundary.");
  auto vertex = [&](size_t i, double y) { return XYZ{points[i][0], y, points[i][1]}; };
  for (size_t i = 0; i < caps.size(); i += 3) {
    for (int k : {0, 1, 2})
      output.vertices.push_back(vertex(caps[i + k], bottom));
  }
  for (size_t i = 0; i < caps.size(); i += 3) {
    for (int k : {0, 2, 1})
      output.vertices.push_back(vertex(caps[i + k], top));
  }
  for (size_t i = 0; i < next.size(); ++i) {
    output.vertices.push_back(vertex(i, bottom));
    output.vertices.push_back(vertex(i, top));
    output.vertices.push_back(vertex(next[i], top));
  }
  for (size_t i = 0; i < next.size(); ++i) {
    output.vertices.push_back(vertex(i, bottom));
    output.vertices.push_back(vertex(next[i], top));
    output.vertices.push_back(vertex(next[i], bottom));
  }
}

// A GEOS union may return components that touch at a point. Extruding those
// components creates a shared vertical edge, even though each individual solid
// is closed. Check the assembled solid as well as each cap before accepting it.
// Use exact coordinates: snapping here could conceal a real gap or collapse.
void validate_closed_mesh(const Result &mesh) {
  std::map<XYZ, size_t> vertex_ids;
  struct EdgeUse {
    int count = 0;
    int direction = 0;
  };
  std::map<std::pair<size_t, size_t>, EdgeUse> edges;
  for (size_t start = 0; start < mesh.vertices.size(); start += 3) {
    std::array<size_t, 3> ids;
    for (size_t k = 0; k < 3; ++k) {
      const auto &point = mesh.vertices[start + k];
      if (!std::isfinite(point[0]) || !std::isfinite(point[1]) || !std::isfinite(point[2]))
        throw std::runtime_error("Water extrusion contains nonfinite coordinates.");
      ids[k] = vertex_ids.emplace(point, vertex_ids.size()).first->second;
    }
    const auto &a = mesh.vertices[start];
    const auto &b = mesh.vertices[start + 1];
    const auto &c = mesh.vertices[start + 2];
    XYZ u, v;
    for (size_t k = 0; k < 3; ++k) {
      u[k] = b[k] - a[k];
      v[k] = c[k] - a[k];
    }
    if (u[1] * v[2] == u[2] * v[1] && u[2] * v[0] == u[0] * v[2] && u[0] * v[1] == u[1] * v[0])
      throw std::runtime_error("Water extrusion contains a degenerate triangle.");
    for (size_t k = 0; k < 3; ++k) {
      size_t from = ids[k], to = ids[(k + 1) % 3];
      auto &edge = edges[std::make_pair(std::min(from, to), std::max(from, to))];
      ++edge.count;
      edge.direction += from < to ? 1 : -1;
    }
  }
  for (const auto &edge : edges) {
    if (edge.second.count != 2 || edge.second.direction != 0)
      throw std::runtime_error("Water extrusion must form a closed, consistently oriented mesh.");
  }
}

Result build_part(Context &context, const Footprint &polygon, const Terrain &terrain, double zscale,
                  double tolerance, double clearance, std::vector<size_t> &seen, size_t stamp) {
  Result output;
  auto ids = candidate_faces(polygon, terrain, seen, stamp);
  if (ids.empty())
    return output;
  auto footprint = make_polygon(context, polygon.rings);
  std::vector<Geometry> clipped;
  std::vector<double> heights, shore, minimum, maximum;
  double supported_area = 0;

  // Clip against actual rendered terrain faces, not a regular raster proxy.
  // Keep repeated ring endpoints in the elevation samples, as the R path does.
  for (int id : ids) {
    if (RcppThread::isInterrupted())
      return Result();
    const auto &t = terrain.triangles[id];
    Rings triangle{{{t.a[0], t.a[2]}, {t.b[0], t.b[2]}, {t.c[0], t.c[2]}, {t.a[0], t.a[2]}}};
    auto face = make_polygon(context, triangle);
    auto intersection =
        own(context, GEOSIntersection_r(context.handle, face.get(), footprint.get()));
    double low = std::numeric_limits<double>::infinity(), high = -low;
    auto collect = [&](const GEOSGeometry *part) {
      double area = 0;
      context.require(GEOSArea_r(context.handle, part, &area), "GEOS area failed.");
      supported_area += area;
      clipped.push_back(own(context, GEOSGeom_clone_r(context.handle, part)));
      for (const auto &ring : read_polygon(context, part)) {
        for (const auto &p : ring) {
          double height = sample_height(t, p);
          heights.push_back(height);
          low = std::min(low, height);
          high = std::max(high, height);
          if (on_shore(p, polygon.rings[0], 1e-8 * terrain.magnitude))
            shore.push_back(height);
        }
      }
    };
    visit_polygons(context, intersection.get(), collect);
    if (std::isfinite(low)) {
      minimum.push_back(low);
      maximum.push_back(high);
    }
  }
  if (heights.empty())
    return output;
  // Heights are in scene units. The bottom covers all supporting terrain; an
  // inferred top prefers the original exterior shoreline over interior samples.
  double bottom = *std::min_element(heights.begin(), heights.end());
  double level = polygon.level / zscale;
  if (!std::isfinite(level))
    level = shore.empty() ? median(heights) : median(shore);
  for (size_t i = 0; i < minimum.size(); ++i) {
    // Compare with the original supplied/inferred level for every patch. A
    // raised top must not progressively recruit other terrain patches.
    if (std::abs(minimum[i] - level) <= tolerance && std::abs(maximum[i] - level) <= tolerance) {
      double highest = maximum[i];
      for (size_t j = i + 1; j < minimum.size(); ++j) {
        if (std::abs(minimum[j] - level) <= tolerance && std::abs(maximum[j] - level) <= tolerance)
          highest = std::max(highest, maximum[j]);
      }
      level = std::max(level, highest + clearance);
      break;
    }
  }
  if (level <= bottom)
    return output;

  // The hidden bottom cap must sit below its supporting terrain. Coincident
  // caps otherwise compete with the terrain at ray intersections, producing
  // dark triangulation patterns even with zero water absorption. Reuse the
  // scene-scaled float clearance; the visible top and shoreline stay fixed.
  bottom -= clearance;

  // Union only supported area, retaining holes and disconnected terrain. The
  // simplification tolerance removes double-roundoff fragments, not real detail.
  std::vector<GEOSGeometry *> pointers;
  for (auto &part : clipped)
    pointers.push_back(part.get());
  auto collection =
      own(context, GEOSGeom_createCollection_r(context.handle, GEOS_GEOMETRYCOLLECTION,
                                               pointers.data(), pointers.size()));
  for (auto &part : clipped)
    part.release();
  auto united = own(context, GEOSUnaryUnion_r(context.handle, collection.get()));
  auto simplified =
      own(context, GEOSTopologyPreserveSimplify_r(context.handle, united.get(),
                                                  8 * std::numeric_limits<double>::epsilon() *
                                                      terrain.magnitude));

  // Terrain triangles partition a heightfield, so union must conserve the
  // sum of supported areas. Older GEOS overlays can silently lose a piece.
  // Reject that result for main-thread recovery rather than returning a
  // plausible-looking but incomplete shoreline. Overlapping input surfaces
  // also take that established recovery path. The relative tolerance permits
  // only accumulated floating-point/simplification noise, not visible detail.
  double union_area = 0;
  context.require(GEOSArea_r(context.handle, simplified.get(), &union_area), "GEOS area failed.");
  if (std::abs(union_area - supported_area) > 1e-9 * std::max(1.0, supported_area))
    throw std::runtime_error("Water footprint union did not preserve supported area.");

  // Lines follow the simplified shoreline; cap/wall normalization may remove
  // redundant collinear vertices, but must not alter the footprint itself.
  auto emit = [&](const GEOSGeometry *part) {
    auto rings = read_polygon(context, part);
    for (const auto &ring : rings) {
      for (size_t i = 1; i < ring.size(); ++i) {
        output.lines.push_back({ring[i - 1][0], level, ring[i - 1][1]});
        output.lines.push_back({ring[i][0], level, ring[i][1]});
      }
    }
    extrude(rings, level, bottom, output);
  };
  visit_polygons(context, simplified.get(), emit);
  validate_closed_mesh(output);
  return output;
}

// Geometry of the valid terrain is shared as ordinary coordinates, never as a
// GEOS object across contexts. Mask construction discards paired interior edges;
// the remaining linework contains the outside outline and every missing-data
// hole. Building an area from this small boundary avoids unioning terrain faces.
void prepare_coverage(Terrain &terrain, const std::vector<Footprint> &polygons) {
  // Sparse requests should not pay to sort the entire scene's edges. The union
  // of footprint-bbox candidates contains every supporting face, so restricting
  // the mask to those faces cannot change any requested water footprint.
  // Above one eighth of the terrain, use its full (usually simpler) boundary:
  // many scattered local masks can cost more to node and prepare than that rim.
  std::vector<int> coverage_faces;
  std::vector<size_t> seen(terrain.triangles.size(), 0);
  std::vector<bool> included(terrain.triangles.size(), false);
  for (size_t i = 0; i < polygons.size(); ++i) {
    for (int id : candidate_faces(polygons[i], terrain, seen, i + 1)) {
      if (!included[id]) {
        included[id] = true;
        coverage_faces.push_back(id);
      }
    }
    if (coverage_faces.size() >= std::max(size_t(1), terrain.triangles.size() / 8)) {
      coverage_faces.resize(terrain.triangles.size());
      std::iota(coverage_faces.begin(), coverage_faces.end(), 0);
      break;
    }
    Rcpp::checkUserInterrupt();
  }
  terrain.coverage_faces = coverage_faces.size();

  struct Edge {
    Point a, b;
    int direction;
  };
  std::vector<Edge> edges;
  edges.reserve(coverage_faces.size() * 3);
  double terrain_area = 0;
  for (size_t i = 0; i < coverage_faces.size(); ++i) {
    const auto &t = terrain.triangles[coverage_faces[i]];
    std::array<Point, 3> p{{{t.a[0], t.a[2]}, {t.b[0], t.b[2]}, {t.c[0], t.c[2]}}};
    double cross =
        (p[1][0] - p[0][0]) * (p[2][1] - p[0][1]) - (p[1][1] - p[0][1]) * (p[2][0] - p[0][0]);
    terrain_area += std::abs(cross) / 2;
    if (cross < 0)
      std::swap(p[1], p[2]);
    for (int k = 0; k < 3; ++k) {
      Point a = p[k], b = p[(k + 1) % 3];
      int direction = a < b ? 1 : -1;
      if (b < a)
        std::swap(a, b);
      edges.push_back({a, b, direction});
    }
    if ((i & 65535) == 0)
      Rcpp::checkUserInterrupt();
  }

  // Sorting exact endpoint pairs removes only genuine shared edges: nearby
  // vertices are not welded, so a narrow hole cannot silently disappear.
  std::sort(edges.begin(), edges.end(),
            [](const Edge &a, const Edge &b) { return a.a < b.a || (a.a == b.a && a.b < b.b); });
  Rcpp::checkUserInterrupt();

  Context context;
  std::vector<Geometry> boundary;
  for (size_t i = 0; i < edges.size();) {
    size_t end = i + 1;
    int direction = edges[i].direction;
    while (end < edges.size() && edges[end].a == edges[i].a && edges[end].b == edges[i].b) {
      direction += edges[end].direction;
      ++end;
    }
    if (end - i == 1) {
      auto sequence = GEOSCoordSeq_create_r(context.handle, 2, 2);
      context.require(sequence != nullptr, "Terrain boundary allocation failed.");
      GEOSCoordSeq_setX_r(context.handle, sequence, 0, edges[i].a[0]);
      GEOSCoordSeq_setY_r(context.handle, sequence, 0, edges[i].a[1]);
      GEOSCoordSeq_setX_r(context.handle, sequence, 1, edges[i].b[0]);
      GEOSCoordSeq_setY_r(context.handle, sequence, 1, edges[i].b[1]);
      boundary.push_back(own(context, GEOSGeom_createLineString_r(context.handle, sequence)));
    } else if (end - i != 2 || direction != 0) {
      throw std::runtime_error("Terrain has overlapping or nonmanifold projected faces.");
    }
    i = end;
  }

  terrain.boundary_edges = boundary.size();
  if (boundary.empty())
    throw std::runtime_error("Terrain has no closed coverage boundary.");
  std::vector<GEOSGeometry *> pointers;
  for (auto &edge : boundary)
    pointers.push_back(edge.get());
  auto collection = own(context, GEOSGeom_createCollection_r(context.handle, GEOS_MULTILINESTRING,
                                                             pointers.data(), pointers.size()));
  for (auto &edge : boundary)
    edge.release();
  // Noding accommodates collinear boundary subdivisions (including T-junctions)
  // without imposing a new precision grid or welding nearby terrain features.
  Rcpp::checkUserInterrupt();
  auto noded = own(context, GEOSNode_r(context.handle, collection.get()));
  auto area = own(context, GEOSBuildArea_r(context.handle, noded.get()));
  double coverage_area = 0;
  context.require(GEOSArea_r(context.handle, area.get(), &coverage_area),
                  "Terrain mask area failed.");
  if (std::abs(coverage_area - terrain_area) > 1e-9 * std::max(1.0, terrain_area))
    throw std::runtime_error("Terrain mask did not conserve projected terrain area.");
  auto collect = [&](const GEOSGeometry *part) {
    terrain.coverage.push_back(read_polygon(context, part));
  };
  visit_polygons(context, area.get(), collect);
}

// Each worker reconstructs/prepares the shared mask once in its own GEOS
// context. The prepared index quickly accepts footprints wholly inside valid
// terrain; crossing an outside boundary or a hole needs one polygon overlay.
struct PreparedCoverage {
  Context &context;
  Geometry geometry;
  const GEOSPreparedGeometry *prepared;

  PreparedCoverage(Context &context, const Terrain &terrain)
      : context(context), geometry(nullptr, GeometryDeleter{&context}), prepared(nullptr) {
    std::vector<Geometry> parts;
    std::vector<GEOSGeometry *> pointers;
    for (const auto &rings : terrain.coverage) {
      parts.push_back(make_polygon(context, rings));
      pointers.push_back(parts.back().get());
    }
    geometry = own(context, GEOSGeom_createCollection_r(context.handle, GEOS_MULTIPOLYGON,
                                                        pointers.data(), pointers.size()));
    for (auto &part : parts)
      part.release();
    prepared = GEOSPrepare_r(context.handle, geometry.get());
    context.require(prepared != nullptr, "Terrain mask preparation failed.");
  }
  ~PreparedCoverage() {
    if (prepared)
      GEOSPreparedGeom_destroy_r(context.handle, prepared);
  }
  PreparedCoverage(const PreparedCoverage &) = delete;
  PreparedCoverage &operator=(const PreparedCoverage &) = delete;
};

// Index shoreline segments in horizontal Z bands. Containment ray crossings and
// triangle/shoreline tests inspect nearby segments instead of rescanning a long
// river boundary for every terrain face. Rings use scene XZ as planar XY here.
class ShorelineIndex {
  struct Edge {
    Point a, b;
    double xmin, xmax, zmin, zmax;
    bool exterior;
    int first_band;
  };
  std::vector<Edge> edges;
  std::vector<std::vector<size_t>> bands;
  double origin, step;

  int band(double z) const {
    return static_cast<int>(
        std::max(0.0, std::min(double(bands.size() - 1), std::floor((z - origin) / step))));
  }

  static bool intersects_triangle(const Edge &segment, const Triangle &t, double margin) {
    std::array<Point, 3> p{{{t.a[0], t.a[2]}, {t.b[0], t.b[2]}, {t.c[0], t.c[2]}}};
    double determinant =
        (p[1][0] - p[0][0]) * (p[2][1] - p[0][1]) - (p[1][1] - p[0][1]) * (p[2][0] - p[0][0]);
    double sign = determinant > 0 ? 1 : -1;
    double lower = 0, upper = 1;
    // Clip the segment against the three oriented triangle half-planes. The
    // margin includes all vertices eligible for the legacy shoreline tolerance,
    // so an almost-touching interior face cannot lose a shoreline sample.
    for (int i = 0; i < 3; ++i) {
      const Point &a = p[i], &b = p[(i + 1) % 3];
      double dx = b[0] - a[0], dz = b[1] - a[1];
      double f0 = sign * (dx * (segment.a[1] - a[1]) - dz * (segment.a[0] - a[0]));
      double f1 = sign * (dx * (segment.b[1] - a[1]) - dz * (segment.b[0] - a[0]));
      double limit = -margin * std::hypot(dx, dz);
      if (f0 < limit && f1 < limit)
        return false;
      if (f0 >= limit && f1 >= limit)
        continue;
      double fraction = (limit - f0) / (f1 - f0);
      if (f0 < limit)
        lower = std::max(lower, fraction);
      else
        upper = std::min(upper, fraction);
      if (lower > upper)
        return false;
    }
    return true;
  }

public:
  explicit ShorelineIndex(const Footprint &polygon) {
    size_t count = 0;
    for (const auto &ring : polygon.rings)
      count += ring.size() - 1;
    bands.resize(std::max(1, std::min(128, static_cast<int>(std::sqrt(count)))));
    origin = polygon.zmin;
    step = std::max(polygon.zmax - polygon.zmin, std::numeric_limits<double>::epsilon()) /
           bands.size();
    for (size_t r = 0; r < polygon.rings.size(); ++r) {
      const auto &ring = polygon.rings[r];
      for (size_t i = 1; i < ring.size(); ++i) {
        Edge e{ring[i - 1],
               ring[i],
               std::min(ring[i - 1][0], ring[i][0]),
               std::max(ring[i - 1][0], ring[i][0]),
               std::min(ring[i - 1][1], ring[i][1]),
               std::max(ring[i - 1][1], ring[i][1]),
               r == 0,
               0};
        e.first_band = band(e.zmin);
        size_t id = edges.size();
        edges.push_back(e);
        for (int k = e.first_band; k <= band(e.zmax); ++k)
          bands[k].push_back(id);
      }
    }
  }

  bool contains_point(const Point &point) const {
    bool inside = false;
    for (size_t id : bands[band(point[1])]) {
      const auto &e = edges[id];
      if ((e.a[1] > point[1]) == (e.b[1] > point[1]))
        continue;
      double crossing = e.a[0] + (e.b[0] - e.a[0]) * (point[1] - e.a[1]) / (e.b[1] - e.a[1]);
      if (crossing > point[0])
        inside = !inside;
    }
    return inside;
  }

  bool meets_triangle(const Triangle &t, double margin) const {
    int first = band(t.zmin - margin), last = band(t.zmax + margin);
    for (int k = first; k <= last; ++k) {
      for (size_t id : bands[k]) {
        const auto &e = edges[id];
        if (k != first && e.first_band != k)
          continue;
        if (e.xmin > t.xmax + margin || e.xmax < t.xmin - margin || e.zmin > t.zmax + margin ||
            e.zmax < t.zmin - margin)
          continue;
        if (intersects_triangle(e, t, margin))
          return true;
      }
    }
    return false;
  }

  bool on_exterior(const Point &point, double margin) const {
    int first = band(point[1] - margin), last = band(point[1] + margin);
    for (int k = first; k <= last; ++k) {
      for (size_t id : bands[k]) {
        const auto &e = edges[id];
        if (!e.exterior || (k != first && e.first_band != k) || point[0] < e.xmin - margin ||
            point[0] > e.xmax + margin)
          continue;
        double dx = e.b[0] - e.a[0], dz = e.b[1] - e.a[1];
        double length2 = dx * dx + dz * dz;
        double fraction =
            length2 == 0 ? 0 : ((point[0] - e.a[0]) * dx + (point[1] - e.a[1]) * dz) / length2;
        double distance;
        if (fraction <= 0)
          distance = std::hypot(point[0] - e.a[0], point[1] - e.a[1]);
        else if (fraction >= 1)
          distance = std::hypot(point[0] - e.b[0], point[1] - e.b[1]);
        else
          distance = std::abs(((e.a[1] - point[1]) * dx - (e.a[0] - point[0]) * dz) / length2) *
                     std::sqrt(length2);
        if (distance <= margin)
          return true;
      }
    }
    return false;
  }
};

Result build_boundary_part(Context &context, PreparedCoverage &coverage, const Footprint &polygon,
                           const Terrain &terrain, double zscale, double tolerance,
                           double clearance, std::vector<size_t> &seen, size_t stamp) {
  Result output;
  auto ids = candidate_faces(polygon, terrain, seen, stamp);
  if (ids.empty())
    return output;

  // Clip the footprint once to valid terrain, including its missing-data holes.
  // Most polygons are fully covered and need no constructive GEOS overlay.
  auto original = make_polygon(context, polygon.rings);
  char covered = GEOSPreparedCovers_r(context.handle, coverage.prepared, original.get());
  context.require(covered != 2, "Terrain mask containment failed.");
  auto footprint = covered ? own(context, GEOSGeom_clone_r(context.handle, original.get()))
                           : own(context, GEOSIntersection_r(context.handle, original.get(),
                                                             coverage.geometry.get()));
  double footprint_area = 0;
  context.require(GEOSArea_r(context.handle, footprint.get(), &footprint_area),
                  "Water area failed.");
  if (footprint_area == 0)
    return output;

  // Sample elevations independently from footprint construction. The original
  // exterior remains the level-inference shoreline, even when a mask clips it.
  ShorelineIndex shoreline(polygon);
  double shore_tolerance = 1e-8 * terrain.magnitude;
  std::vector<double> shore;
  std::vector<std::pair<double, double>> ranges;
  double supported_area = 0;
  double bottom = std::numeric_limits<double>::infinity(), highest = -bottom;
  for (int id : ids) {
    if (RcppThread::isInterrupted())
      return Result();
    const auto &t = terrain.triangles[id];
    double low = std::numeric_limits<double>::infinity(), high = -low;
    if (shoreline.meets_triangle(t, shore_tolerance)) {
      ++output.boundary_faces;
      // Only faces near the original shoreline use a full overlay. Keeping
      // these samples and closing endpoints preserves existing level weights.
      Rings triangle{{{t.a[0], t.a[2]}, {t.b[0], t.b[2]}, {t.c[0], t.c[2]}, {t.a[0], t.a[2]}}};
      auto face = make_polygon(context, triangle);
      auto intersection =
          own(context, GEOSIntersection_r(context.handle, face.get(), original.get()));
      auto collect = [&](const GEOSGeometry *part) {
        double area = 0;
        context.require(GEOSArea_r(context.handle, part, &area), "Water support area failed.");
        supported_area += area;
        for (const auto &ring : read_polygon(context, part)) {
          for (const auto &p : ring) {
            double height = sample_height(t, p);
            low = std::min(low, height);
            high = std::max(high, height);
            if (shoreline.on_exterior(p, shore_tolerance))
              shore.push_back(height);
          }
        }
      };
      visit_polygons(context, intersection.get(), collect);
    } else if (shoreline.contains_point(
                   {(t.a[0] + t.b[0] + t.c[0]) / 3, (t.a[2] + t.b[2] + t.c[2]) / 3})) {
      // No polygon edge enters this triangle (including holes or a tiny pond
      // contained in it). Its centroid therefore classifies the entire face.
      // Linear terrain reaches its extrema at the existing triangle vertices.
      ++output.interior_faces;
      low = std::min({t.a[1], t.b[1], t.c[1]});
      high = std::max({t.a[1], t.b[1], t.c[1]});
      supported_area +=
          std::abs((t.b[0] - t.a[0]) * (t.c[2] - t.a[2]) - (t.b[2] - t.a[2]) * (t.c[0] - t.a[0])) /
          2;
    }
    if (std::isfinite(low)) {
      ranges.emplace_back(low, high);
      bottom = std::min(bottom, low);
      highest = std::max(highest, high);
    }
  }
  if (ranges.empty())
    return output;
  if (std::abs(supported_area - footprint_area) > 1e-9 * std::max(1.0, footprint_area))
    throw std::runtime_error("Terrain mask and shoreline support disagree in area.");

  // Preserve inferred elevations and the existing near-coincident-top rule.
  double level = polygon.level / zscale;
  if (!std::isfinite(level)) {
    if (!shore.empty()) {
      level = median(shore);
    } else if (bottom == highest) {
      level = bottom;
    } else {
      // With no original shoreline over terrain, the legacy rule weights all
      // clipped terrain vertices. Preserve that rare inference case explicitly.
      return build_part(context, polygon, terrain, zscale, tolerance, clearance, seen, stamp + 1);
    }
  }
  double coincident_top = -std::numeric_limits<double>::infinity();
  for (const auto &range : ranges) {
    if (std::abs(range.first - level) <= tolerance && std::abs(range.second - level) <= tolerance)
      coincident_top = std::max(coincident_top, range.second);
  }
  if (std::isfinite(coincident_top))
    level = std::max(level, coincident_top + clearance);
  if (level <= bottom)
    return output;

  // The hidden bottom cap must sit below its supporting terrain. Coincident
  // caps otherwise compete with the terrain at ray intersections, producing
  // dark triangulation patterns even with zero water absorption. Reuse the
  // scene-scaled float clearance; the visible top and shoreline stay fixed.
  bottom -= clearance;

  // Mask-derived rings include new scene-edge and hole boundaries. Extruding
  // every ring creates closed sidewalls along those cuts as well as shorelines.
  auto simplified =
      own(context, GEOSTopologyPreserveSimplify_r(context.handle, footprint.get(),
                                                  8 * std::numeric_limits<double>::epsilon() *
                                                      terrain.magnitude));
  auto emit = [&](const GEOSGeometry *part) {
    auto rings = read_polygon(context, part);
    for (const auto &ring : rings) {
      for (size_t i = 1; i < ring.size(); ++i) {
        output.lines.push_back({ring[i - 1][0], level, ring[i - 1][1]});
        output.lines.push_back({ring[i][0], level, ring[i][1]});
      }
    }
    extrude(rings, level, bottom, output);
  };
  visit_polygons(context, simplified.get(), emit);
  validate_closed_mesh(output);
  return output;
}

// The only R-to-native conversion boundary. All objects below are ordinary
// C++ values owned for the duration of the call; workers never touch a SEXP.
Terrain pack_terrain(const Rcpp::List &source) {
  Terrain terrain;
  Rcpp::NumericMatrix a = source["a"], b = source["b"], c = source["c"];
  if (a.ncol() != 3 || b.ncol() != 3 || c.ncol() != 3 || a.nrow() != b.nrow() ||
      a.nrow() != c.nrow())
    Rcpp::stop("Invalid terrain matrices.");
  terrain.triangles.resize(a.nrow());
  for (int i = 0; i < a.nrow(); ++i) {
    auto &t = terrain.triangles[i];
    for (int j = 0; j < 3; ++j) {
      t.a[j] = a(i, j);
      t.b[j] = b(i, j);
      t.c[j] = c(i, j);
    }
    t.xmin = std::min({t.a[0], t.b[0], t.c[0]});
    t.xmax = std::max({t.a[0], t.b[0], t.c[0]});
    t.zmin = std::min({t.a[2], t.b[2], t.c[2]});
    t.zmax = std::max({t.a[2], t.b[2], t.c[2]});
    if ((i & 65535) == 0)
      Rcpp::checkUserInterrupt();
  }
  terrain.bins = Rcpp::as<int>(source["bins"]);
  if (terrain.bins < 1)
    Rcpp::stop("Invalid terrain bin count.");
  Rcpp::NumericVector origin = source["origin"], step = source["step"];
  if (origin.size() != 2 || step.size() != 2 || step[0] <= 0 || step[1] <= 0)
    Rcpp::stop("Invalid terrain grid.");
  terrain.origin = {origin[0], origin[1]};
  terrain.step = {step[0], step[1]};
  terrain.magnitude = Rcpp::as<double>(source["magnitude"]);
  terrain.faces.resize(terrain.bins * terrain.bins);
  Rcpp::List bins = source["faces"];
  Rcpp::CharacterVector names = bins.names();
  for (int i = 0; i < bins.size(); ++i) {
    int bin = std::stoi(Rcpp::as<std::string>(names[i]));
    if (bin < 0 || bin >= static_cast<int>(terrain.faces.size()))
      Rcpp::stop("Invalid terrain bin index.");
    Rcpp::IntegerVector ids = bins[i];
    for (int id : ids) {
      if (id < 1 || id > a.nrow())
        Rcpp::stop("Invalid terrain face index.");
      terrain.faces[bin].push_back(id - 1);
    }
  }
  return terrain;
}

std::vector<Footprint> pack_footprints(const Rcpp::List &source) {
  Rcpp::List geometry = source["geometry"];
  Rcpp::NumericVector levels = source["levels"];
  if (levels.size() != geometry.size())
    Rcpp::stop("Incompatible polygon elevations.");
  std::vector<Footprint> result(geometry.size());
  for (int i = 0; i < geometry.size(); ++i) {
    auto &p = result[i];
    p.level = levels[i];
    p.xmin = p.zmin = std::numeric_limits<double>::infinity();
    p.xmax = p.zmax = -p.xmin;
    Rcpp::List rings = geometry[i];
    for (int j = 0; j < rings.size(); ++j) {
      Rcpp::NumericMatrix ring = rings[j];
      if (ring.ncol() != 2 || ring.nrow() < 4)
        Rcpp::stop("Expected closed XY polygon rings.");
      p.rings.emplace_back();
      for (int k = 0; k < ring.nrow(); ++k) {
        p.rings.back().push_back({ring(k, 0), ring(k, 1)});
        p.xmin = std::min(p.xmin, ring(k, 0));
        p.xmax = std::max(p.xmax, ring(k, 0));
        p.zmin = std::min(p.zmin, ring(k, 1));
        p.zmax = std::max(p.zmax, ring(k, 1));
      }
    }
  }
  return result;
}

Rcpp::NumericMatrix matrix_from_points(const std::vector<XYZ> &points, const char *height_label) {
  Rcpp::NumericMatrix result(points.size(), 3);
  Rcpp::colnames(result) = Rcpp::CharacterVector::create("", height_label, "");
  for (size_t i = 0; i < points.size(); ++i)
    for (int axis = 0; axis < 3; ++axis)
      result(i, axis) = points[i][axis];
  return result;
}

} // namespace water_threads

// [[Rcpp::export(rng = false)]]
Rcpp::List build_polygon_water_meshes_cpp(Rcpp::List footprints, Rcpp::List terrain, double zscale,
                                          double tolerance, double clearance, int threads) {
  using namespace water_threads;
  if (!std::isfinite(zscale) || zscale <= 0 || !std::isfinite(tolerance) || tolerance < 0 ||
      !std::isfinite(clearance) || clearance < 0 || threads < 1)
    Rcpp::stop("Invalid water construction settings.");
  auto native_terrain = pack_terrain(terrain);
  auto polygons = pack_footprints(footprints);
  if (polygons.empty())
    return Rcpp::List();

  // Precompute once per terrain snapshot, before workers start. Rebuilding on
  // each call also respects terrain edits without a stale rgl geometry cache.
  // Unsupported overlapping meshes retain the full-overlay recovery path.
  try {
    prepare_coverage(native_terrain, polygons);
  } catch (const std::exception &error) {
    native_terrain.coverage.clear();
    native_terrain.coverage_error = error.what();
  }

  std::vector<Result> results(polygons.size());
  std::atomic<size_t> next{0};
  int workers = std::min(static_cast<size_t>(threads), polygons.size());

  // One long-lived task per worker retains its GEOS context and scratch marks.
  // Dynamic atomic assignment balances tiny ponds against large river polygons.
  // Each result slot has one writer, preserving deterministic input order.
  RcppThread::ThreadPool pool(workers == 1 ? 0 : workers);
  std::vector<std::future<void>> tasks;
  for (int worker = 0; worker < workers; ++worker) {
    tasks.push_back(pool.pushReturn([&]() {
      Context context;
      std::unique_ptr<PreparedCoverage> coverage;
      if (native_terrain.coverage_error.empty())
        coverage.reset(new PreparedCoverage(context, native_terrain));
      std::vector<size_t> seen(native_terrain.triangles.size(), 0);
      while (!RcppThread::isInterrupted()) {
        size_t i = next.fetch_add(1, std::memory_order_relaxed);
        if (i >= polygons.size())
          break;
        try {
          context.error.clear();
          results[i] = coverage
                           ? build_boundary_part(context, *coverage, polygons[i], native_terrain,
                                                 zscale, tolerance, clearance, seen, 2 * i + 1)
                           : build_part(context, polygons[i], native_terrain, zscale, tolerance,
                                        clearance, seen, 2 * i + 1);
        } catch (const std::exception &error) {
          results[i] = Result();
          results[i].error = error.what();
        }
      }
    }));
  }
  pool.wait();
  for (auto &task : tasks)
    task.get();
  RcppThread::checkUserInterrupt();

  // R allocation and diagnostic assembly happen only after worker completion.
  Rcpp::List records(results.size());
  for (size_t i = 0; i < results.size(); ++i) {
    const auto &result = results[i];
    Rcpp::RObject mesh = R_NilValue;
    if (!result.vertices.empty()) {
      mesh = Rcpp::List::create(Rcpp::_["vertices"] = matrix_from_points(result.vertices, "low"),
                                Rcpp::_["lines"] = matrix_from_points(result.lines, "level"));
    }
    records[i] = Rcpp::List::create(Rcpp::_["mesh"] = mesh, Rcpp::_["error"] = result.error);
  }
  size_t boundary_faces = 0, interior_faces = 0;
  for (const auto &result : results) {
    boundary_faces += result.boundary_faces;
    interior_faces += result.interior_faces;
  }
  records.attr("work") = Rcpp::NumericVector::create(
      Rcpp::_["boundary_faces"] = static_cast<double>(boundary_faces),
      Rcpp::_["interior_faces"] = static_cast<double>(interior_faces),
      Rcpp::_["mask_faces"] = static_cast<double>(native_terrain.coverage_faces),
      Rcpp::_["mask_edges"] = static_cast<double>(native_terrain.boundary_edges),
      Rcpp::_["mask_parts"] = static_cast<double>(native_terrain.coverage.size()));
  records.attr("coverage_error") = native_terrain.coverage_error;
  return records;
}
