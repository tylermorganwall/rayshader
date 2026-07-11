#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

using namespace Rcpp;

namespace {

struct Point2 {
  double x;
  double z;
};

struct ClipRecord {
  int key;
  int kind;
  int id;
  double x;
  double h;
  double z;
  int edge_id;
  double t;
};

struct BoundaryEntry {
  int count;
  ClipRecord a;
  ClipRecord b;
  int kind;
  int edge_id;
};

struct EdgeIntersections {
  std::vector<unsigned char> has;
  std::vector<double> t;
  std::vector<double> x;
  std::vector<double> z;
};

inline bool finite_double(double value) {
  return std::isfinite(value);
}

inline int x_edge_id(int row0, int col0, int nr) {
  return col0 * (nr - 1) + row0 + 1;
}

inline int z_edge_id(int row0, int col0, int nr, int x_edge_count) {
  return x_edge_count + col0 * nr + row0 + 1;
}

inline int diag_edge_id(int cell_id, int x_edge_count, int z_edge_count) {
  return x_edge_count + z_edge_count + cell_id;
}

inline int slot_to_face(const std::vector<int>& slot_to_face_id, int slot) {
  if (slot <= 0 || slot > static_cast<int>(slot_to_face_id.size())) {
    return 0;
  }
  return slot_to_face_id[slot - 1];
}

double triangle_sublevel_area_scalar(
  double h_a,
  double h_b,
  double h_c,
  double face_area,
  double water_level,
  double height_tol
) {
  double h0 = std::min(h_a, std::min(h_b, h_c));
  double h2 = std::max(h_a, std::max(h_b, h_c));
  double h1 = h_a + h_b + h_c - h0 - h2;

  if (std::abs(h2 - h0) <= height_tol) {
    return water_level > h0 + height_tol ? face_area : 0.0;
  }
  if (water_level <= h0 + height_tol) {
    return 0.0;
  }
  if (water_level >= h2 - height_tol) {
    return face_area;
  }

  double fraction;
  if (water_level < h1) {
    if (std::abs(h1 - h0) <= height_tol) {
      double ratio = (h2 - water_level) / (h2 - h0);
      fraction = 1.0 - ratio * ratio;
    } else {
      fraction = ((water_level - h0) / (h1 - h0)) *
        ((water_level - h0) / (h2 - h0));
    }
  } else if (std::abs(h2 - h1) <= height_tol) {
    double ratio = (water_level - h0) / (h2 - h0);
    fraction = ratio * ratio;
  } else {
    fraction = 1.0 -
      ((h2 - water_level) / (h2 - h0)) *
        ((h2 - water_level) / (h2 - h1));
  }

  return std::max(0.0, std::min(1.0, fraction)) * face_area;
}

double polygon_area_xz(const std::vector<Point2>& polygon) {
  const int n = static_cast<int>(polygon.size());
  if (n < 3) {
    return 0.0;
  }
  double area = 0.0;
  for (int i = 0; i < n; ++i) {
    int j = (i + 1) % n;
    area += polygon[i].x * polygon[j].z - polygon[i].z * polygon[j].x;
  }
  return std::abs(area) / 2.0;
}

double records_projected_area(const std::vector<ClipRecord>& records) {
  const int n = static_cast<int>(records.size());
  if (n < 3) {
    return 0.0;
  }
  double area = 0.0;
  for (int i = 0; i < n; ++i) {
    int j = (i + 1) % n;
    area += records[i].x * records[j].z - records[i].z * records[j].x;
  }
  return std::abs(area) / 2.0;
}

double triangle_normal_y(
  double ax,
  double az,
  double bx,
  double bz,
  double cx,
  double cz
) {
  double first_x = bx - ax;
  double first_z = bz - az;
  double second_x = cx - ax;
  double second_z = cz - az;
  return first_z * second_x - first_x * second_z;
}

double triangle_area3d(
  double ax,
  double ay,
  double az,
  double bx,
  double by,
  double bz,
  double cx,
  double cy,
  double cz
) {
  double ux = bx - ax;
  double uy = by - ay;
  double uz = bz - az;
  double vx = cx - ax;
  double vy = cy - ay;
  double vz = cz - az;
  double cross_x = uy * vz - uz * vy;
  double cross_y = uz * vx - ux * vz;
  double cross_z = ux * vy - uy * vx;
  return std::sqrt(
    cross_x * cross_x + cross_y * cross_y + cross_z * cross_z
  ) / 2.0;
}

ClipRecord original_vertex_record(
  const NumericMatrix& vertices,
  int vertex_id
) {
  int vertex_id0 = vertex_id - 1;
  return ClipRecord{
    vertex_id,
    0,
    vertex_id,
    vertices(vertex_id0, 0),
    vertices(vertex_id0, 1),
    vertices(vertex_id0, 2),
    NA_INTEGER,
    NA_REAL
  };
}

EdgeIntersections make_edge_intersections(
  const NumericMatrix& vertices,
  const IntegerMatrix& edge_vertices,
  double water_level,
  double height_tol
) {
  int edge_count = edge_vertices.nrow();
  EdgeIntersections out;
  out.has.assign(edge_count, 0);
  out.t.assign(edge_count, 0.0);
  out.x.assign(edge_count, 0.0);
  out.z.assign(edge_count, 0.0);
  for (int edge = 0; edge < edge_count; ++edge) {
    int v0 = edge_vertices(edge, 0) - 1;
    int v1 = edge_vertices(edge, 1) - 1;
    double h0 = vertices(v0, 1);
    double h1 = vertices(v1, 1);
    double delta = h1 - h0;
    bool crosses =
      finite_double(h0) &&
      finite_double(h1) &&
      std::abs(delta) > height_tol &&
      std::min(h0, h1) < water_level - height_tol &&
      std::max(h0, h1) > water_level + height_tol;
    if (crosses) {
      out.has[edge] = 1;
      out.t[edge] = (water_level - h0) / delta;
      out.t[edge] = std::max(0.0, std::min(1.0, out.t[edge]));
    }
    double t = out.t[edge];
    out.x[edge] = (1.0 - t) * vertices(v0, 0) + t * vertices(v1, 0);
    out.z[edge] = (1.0 - t) * vertices(v0, 2) + t * vertices(v1, 2);
  }
  return out;
}

ClipRecord edge_plane_intersection_record(
  const NumericMatrix& vertices,
  const IntegerMatrix& edge_vertices,
  const EdgeIntersections& edge_intersections,
  int edge_id,
  int vertex_a,
  int vertex_b,
  double water_level,
  double height_tol,
  double t_tol
) {
  double height_a = vertices(vertex_a - 1, 1);
  double height_b = vertices(vertex_b - 1, 1);
  double delta = height_b - height_a;
  double t_ab = std::abs(delta) <= height_tol ?
    0.0 :
    (water_level - height_a) / delta;
  if (t_ab <= t_tol) {
    return original_vertex_record(vertices, vertex_a);
  }
  if (t_ab >= 1.0 - t_tol) {
    return original_vertex_record(vertices, vertex_b);
  }

  int edge_id0 = edge_id - 1;
  double t;
  double x;
  double z;
  if (edge_intersections.has[edge_id0]) {
    t = edge_intersections.t[edge_id0];
    x = edge_intersections.x[edge_id0];
    z = edge_intersections.z[edge_id0];
  } else {
    int edge_vertex_a = edge_vertices(edge_id0, 0) - 1;
    int edge_vertex_b = edge_vertices(edge_id0, 1) - 1;
    double edge_delta = vertices(edge_vertex_b, 1) - vertices(edge_vertex_a, 1);
    t = std::abs(edge_delta) <= height_tol ?
      0.5 :
      (water_level - vertices(edge_vertex_a, 1)) / edge_delta;
    x = (1.0 - t) * vertices(edge_vertex_a, 0) +
      t * vertices(edge_vertex_b, 0);
    z = (1.0 - t) * vertices(edge_vertex_a, 2) +
      t * vertices(edge_vertex_b, 2);
  }
  return ClipRecord{
    vertices.nrow() + edge_id,
    1,
    edge_id,
    x,
    water_level,
    z,
    edge_id,
    t
  };
}

std::vector<ClipRecord> clean_records(
  const std::vector<ClipRecord>& records,
  double height_tol,
  double length_tol
) {
  int n = static_cast<int>(records.size());
  if (n == 0) {
    return records;
  }
  std::vector<ClipRecord> out;
  out.reserve(n);
  for (int i = 0; i < n; ++i) {
    int previous = (i == 0) ? n - 1 : i - 1;
    bool same_key = records[i].key == records[previous].key;
    double dx = records[i].x - records[previous].x;
    double dz = records[i].z - records[previous].z;
    bool same_point =
      std::sqrt(dx * dx + dz * dz) <= length_tol &&
      std::abs(records[i].h - records[previous].h) <= height_tol;
    if (!(same_key || same_point)) {
      out.push_back(records[i]);
    }
  }
  return out;
}

std::vector<ClipRecord> clip_face_records(
  const NumericMatrix& vertices,
  const IntegerMatrix& faces,
  const IntegerMatrix& face_edges,
  const IntegerMatrix& edge_vertices,
  const EdgeIntersections& edge_intersections,
  int face_id,
  double water_level,
  double height_tol,
  double t_tol,
  double length_tol
) {
  int face_id0 = face_id - 1;
  int face_vertices[3] = {
    faces(face_id0, 0),
    faces(face_id0, 1),
    faces(face_id0, 2)
  };
  double heights[3] = {
    vertices(face_vertices[0] - 1, 1),
    vertices(face_vertices[1] - 1, 1),
    vertices(face_vertices[2] - 1, 1)
  };
  bool below[3];
  bool on_plane[3];
  bool inside[3];
  bool any_below = false;
  bool all_on_plane = true;
  for (int i = 0; i < 3; ++i) {
    below[i] = heights[i] < water_level - height_tol;
    on_plane[i] = std::abs(heights[i] - water_level) <= height_tol;
    inside[i] = below[i] || on_plane[i];
    any_below = any_below || below[i];
    all_on_plane = all_on_plane && on_plane[i];
  }
  if (!any_below || all_on_plane) {
    return std::vector<ClipRecord>();
  }

  std::vector<ClipRecord> records;
  records.reserve(4);
  ClipRecord original_records[3] = {
    original_vertex_record(vertices, face_vertices[0]),
    original_vertex_record(vertices, face_vertices[1]),
    original_vertex_record(vertices, face_vertices[2])
  };
  if (inside[0] && inside[1] && inside[2]) {
    records.push_back(original_records[0]);
    records.push_back(original_records[1]);
    records.push_back(original_records[2]);
    return clean_records(records, height_tol, length_tol);
  }

  for (int i = 0; i < 3; ++i) {
    int next = (i == 2) ? 0 : i + 1;
    bool current_inside = inside[i];
    bool next_inside = inside[next];
    if (current_inside && next_inside) {
      records.push_back(original_records[next]);
    } else if (current_inside && !next_inside) {
      records.push_back(edge_plane_intersection_record(
        vertices,
        edge_vertices,
        edge_intersections,
        face_edges(face_id0, i),
        face_vertices[i],
        face_vertices[next],
        water_level,
        height_tol,
        t_tol
      ));
    } else if (!current_inside && next_inside) {
      records.push_back(edge_plane_intersection_record(
        vertices,
        edge_vertices,
        edge_intersections,
        face_edges(face_id0, i),
        face_vertices[i],
        face_vertices[next],
        water_level,
        height_tol,
        t_tol
      ));
      records.push_back(original_records[next]);
    }
  }
  return clean_records(records, height_tol, length_tol);
}

int original_vertices_edge_id_cpp(
  int vertex_a,
  int vertex_b,
  int nr,
  int x_edge_count,
  int z_edge_count
) {
  if (vertex_a == vertex_b || vertex_a <= 0 || vertex_b <= 0) {
    return NA_INTEGER;
  }
  int row_a = (vertex_a - 1) % nr + 1;
  int row_b = (vertex_b - 1) % nr + 1;
  int col_a = (vertex_a - 1) / nr + 1;
  int col_b = (vertex_b - 1) / nr + 1;
  if (col_a == col_b && std::abs(row_a - row_b) == 1) {
    return (col_a - 1) * (nr - 1) + std::min(row_a, row_b);
  }
  if (row_a == row_b && std::abs(col_a - col_b) == 1) {
    return x_edge_count + (std::min(col_a, col_b) - 1) * nr + row_a;
  }
  if (std::abs(row_a - row_b) == 1 && std::abs(col_a - col_b) == 1) {
    int left_row = col_a < col_b ? row_a : row_b;
    int right_row = col_a < col_b ? row_b : row_a;
    if (left_row > right_row) {
      int cell_row = right_row;
      int cell_col = std::min(col_a, col_b);
      return x_edge_count + z_edge_count + cell_row +
        (cell_col - 1) * (nr - 1);
    }
  }
  return NA_INTEGER;
}

int records_shared_original_edge_id(
  const ClipRecord& a,
  const ClipRecord& b,
  const IntegerMatrix& edge_vertices,
  int nr,
  int x_edge_count,
  int z_edge_count
) {
  if (a.kind == 1 && b.kind == 1) {
    return a.edge_id == b.edge_id ? a.edge_id : NA_INTEGER;
  }
  if (a.kind == 0 && b.kind == 0) {
    return original_vertices_edge_id_cpp(
      a.id,
      b.id,
      nr,
      x_edge_count,
      z_edge_count
    );
  }
  const ClipRecord& vertex_record = a.kind == 0 ? a : b;
  const ClipRecord& edge_record = a.kind == 1 ? a : b;
  int edge_id0 = edge_record.edge_id - 1;
  if (
    edge_id0 >= 0 &&
      edge_id0 < edge_vertices.nrow() &&
      (
        vertex_record.id == edge_vertices(edge_id0, 0) ||
          vertex_record.id == edge_vertices(edge_id0, 1)
      )
  ) {
    return edge_record.edge_id;
  }
  return NA_INTEGER;
}

uint64_t boundary_key(int key_a, int key_b) {
  uint32_t lo = static_cast<uint32_t>(std::min(key_a, key_b));
  uint32_t hi = static_cast<uint32_t>(std::max(key_a, key_b));
  return (static_cast<uint64_t>(lo) << 32) | hi;
}

void append_sidewall(
  std::vector<double>& side_vertices,
  const ClipRecord& a,
  const ClipRecord& b,
  double water_level,
  double surface_area_tol
) {
  double top_a[3] = {a.x, water_level, a.z};
  double top_b[3] = {b.x, water_level, b.z};
  double bot_a[3] = {a.x, a.h, a.z};
  double bot_b[3] = {b.x, b.h, b.z};
  if (
    triangle_area3d(
      top_a[0], top_a[1], top_a[2],
      bot_a[0], bot_a[1], bot_a[2],
      top_b[0], top_b[1], top_b[2]
    ) > surface_area_tol
  ) {
    side_vertices.insert(
      side_vertices.end(),
      {
        top_a[0], top_a[1], top_a[2],
        bot_a[0], bot_a[1], bot_a[2],
        top_b[0], top_b[1], top_b[2]
      }
    );
  }
  if (
    triangle_area3d(
      top_b[0], top_b[1], top_b[2],
      bot_a[0], bot_a[1], bot_a[2],
      bot_b[0], bot_b[1], bot_b[2]
    ) > surface_area_tol
  ) {
    side_vertices.insert(
      side_vertices.end(),
      {
        top_b[0], top_b[1], top_b[2],
        bot_a[0], bot_a[1], bot_a[2],
        bot_b[0], bot_b[1], bot_b[2]
      }
    );
  }
}

std::vector<Point2> clean_polygon(
  const std::vector<Point2>& polygon,
  double length_tol
) {
  const int n = static_cast<int>(polygon.size());
  if (n == 0) {
    return polygon;
  }
  std::vector<Point2> out;
  out.reserve(n);
  for (int i = 0; i < n; ++i) {
    int previous = (i == 0) ? n - 1 : i - 1;
    double dx = polygon[i].x - polygon[previous].x;
    double dz = polygon[i].z - polygon[previous].z;
    if (std::sqrt(dx * dx + dz * dz) > length_tol) {
      out.push_back(polygon[i]);
    }
  }
  return out;
}

std::vector<Point2> face_clipped_xz_polygon(
  const IntegerMatrix& faces,
  const NumericMatrix& vertices,
  int face_id0,
  double water_level,
  double height_tol,
  double length_tol
) {
  int vertex_ids[3] = {
    faces(face_id0, 0) - 1,
    faces(face_id0, 1) - 1,
    faces(face_id0, 2) - 1
  };
  double heights[3] = {
    vertices(vertex_ids[0], 1),
    vertices(vertex_ids[1], 1),
    vertices(vertex_ids[2], 1)
  };
  bool below[3];
  bool on_plane[3];
  bool inside[3];
  bool any_below = false;
  bool all_on_plane = true;
  for (int i = 0; i < 3; ++i) {
    below[i] = heights[i] < water_level - height_tol;
    on_plane[i] = std::abs(heights[i] - water_level) <= height_tol;
    inside[i] = below[i] || on_plane[i];
    any_below = any_below || below[i];
    all_on_plane = all_on_plane && on_plane[i];
  }
  if (!any_below || all_on_plane) {
    return std::vector<Point2>();
  }

  Point2 points[3] = {
    {vertices(vertex_ids[0], 0), vertices(vertex_ids[0], 2)},
    {vertices(vertex_ids[1], 0), vertices(vertex_ids[1], 2)},
    {vertices(vertex_ids[2], 0), vertices(vertex_ids[2], 2)}
  };
  if (inside[0] && inside[1] && inside[2]) {
    return clean_polygon(
      std::vector<Point2>{points[0], points[1], points[2]},
      length_tol
    );
  }

  std::vector<Point2> clipped;
  clipped.reserve(4);
  for (int i = 0; i < 3; ++i) {
    int next = (i == 2) ? 0 : i + 1;
    bool current_inside = inside[i];
    bool next_inside = inside[next];
    if (current_inside && next_inside) {
      clipped.push_back(points[next]);
    } else if (current_inside != next_inside) {
      double delta = heights[next] - heights[i];
      double t = std::abs(delta) <= height_tol ?
        0.0 :
        (water_level - heights[i]) / delta;
      t = std::max(0.0, std::min(1.0, t));
      clipped.push_back({
        (1.0 - t) * points[i].x + t * points[next].x,
        (1.0 - t) * points[i].z + t * points[next].z
      });
      if (!current_inside && next_inside) {
        clipped.push_back(points[next]);
      }
    }
  }
  return clean_polygon(clipped, length_tol);
}

std::vector<Point2> clip_polygon_axis(
  const std::vector<Point2>& polygon,
  double value,
  bool keep_greater,
  bool use_x
) {
  const int n = static_cast<int>(polygon.size());
  if (n == 0) {
    return polygon;
  }
  std::vector<Point2> out;
  out.reserve(n + 1);

  auto coord = [use_x](const Point2& point) {
    return use_x ? point.x : point.z;
  };
  auto inside = [value, keep_greater, coord](const Point2& point) {
    return keep_greater ? coord(point) >= value : coord(point) <= value;
  };

  Point2 previous = polygon[n - 1];
  bool previous_inside = inside(previous);
  for (int i = 0; i < n; ++i) {
    Point2 current = polygon[i];
    bool current_inside = inside(current);
    if (current_inside != previous_inside) {
      double denominator = coord(current) - coord(previous);
      if (std::abs(denominator) > std::numeric_limits<double>::epsilon()) {
        double t = (value - coord(previous)) / denominator;
        out.push_back({
          previous.x + t * (current.x - previous.x),
          previous.z + t * (current.z - previous.z)
        });
      }
    }
    if (current_inside) {
      out.push_back(current);
    }
    previous = current;
    previous_inside = current_inside;
  }
  return out;
}

std::vector<Point2> clip_polygon_to_bounds(
  std::vector<Point2> polygon,
  double x0,
  double x1,
  double z0,
  double z1
) {
  polygon = clip_polygon_axis(polygon, x0, true, true);
  polygon = clip_polygon_axis(polygon, x1, false, true);
  polygon = clip_polygon_axis(polygon, z0, true, false);
  polygon = clip_polygon_axis(polygon, z1, false, false);
  return polygon;
}

bool seed_face_has_positive_overlap_cpp(
  const IntegerMatrix& faces,
  const IntegerVector& face_cell_row,
  const IntegerVector& face_cell_col,
  const NumericMatrix& vertices,
  const LogicalMatrix& mask,
  int nr,
  int nc,
  int face_id0,
  double water_level,
  double height_tol,
  double length_tol,
  double area_tol
) {
  int face_vertices[3] = {
    faces(face_id0, 0),
    faces(face_id0, 1),
    faces(face_id0, 2)
  };
  for (int i = 0; i < 3; ++i) {
    int vertex_id0 = face_vertices[i] - 1;
    int row0 = vertex_id0 % nr;
    int col0 = vertex_id0 / nr;
    if (
      mask(row0, col0) == TRUE &&
        vertices(vertex_id0, 1) < water_level - height_tol
    ) {
      return true;
    }
  }

  std::vector<Point2> polygon = face_clipped_xz_polygon(
    faces,
    vertices,
    face_id0,
    water_level,
    height_tol,
    length_tol
  );
  if (polygon.size() < 3) {
    return false;
  }

  int row0 = face_cell_row[face_id0] - 1;
  int col0 = face_cell_col[face_id0] - 1;
  int candidate_rows[4] = {row0, row0 + 1, row0, row0 + 1};
  int candidate_cols[4] = {col0, col0, col0 + 1, col0 + 1};
  double x_min_scene = -(static_cast<double>(nr) - 1.0) / 2.0;
  double x_max_scene = (static_cast<double>(nr) - 1.0) / 2.0;
  double z_min_scene = -(static_cast<double>(nc) - 1.0) / 2.0;
  double z_max_scene = (static_cast<double>(nc) - 1.0) / 2.0;

  for (int i = 0; i < 4; ++i) {
    int mask_row = candidate_rows[i];
    int mask_col = candidate_cols[i];
    if (
      mask_row < 0 || mask_row >= nr ||
        mask_col < 0 || mask_col >= nc ||
        mask(mask_row, mask_col) != TRUE
    ) {
      continue;
    }
    double x_center = static_cast<double>(mask_row) - (nr - 1.0) / 2.0;
    double z_center = static_cast<double>(mask_col) - (nc - 1.0) / 2.0;
    double x0 = std::max(x_center - 0.5, x_min_scene);
    double x1 = std::min(x_center + 0.5, x_max_scene);
    double z0 = std::max(z_center - 0.5, z_min_scene);
    double z1 = std::min(z_center + 0.5, z_max_scene);
    std::vector<Point2> clipped = clip_polygon_to_bounds(
      polygon,
      x0,
      x1,
      z0,
      z1
    );
    if (clipped.size() >= 3 && polygon_area_xz(clipped) > area_tol) {
      return true;
    }
  }
  return false;
}

List empty_traversal_result(
  int seed_candidate_count,
  bool return_face_ids = false
) {
  return List::create(
    _["area"] = 0.0,
    _["rejected"] = false,
    _["face_ids"] = IntegerVector(),
    _["diagnostics"] = List::create(
      _["seed_candidate_count"] = seed_candidate_count,
      _["seed_face_count"] = 0,
      _["visited_face_count"] = 0,
      _["rejected_early"] = false,
      _["geometry_face_count"] = return_face_ids ? 0 : 0
    )
  );
}

} // namespace

// [[Rcpp::export]]
List make_spatial_water_fixed_grid_terrain_mesh_cpp(
  const NumericMatrix& heightmap
) {
  int nr = heightmap.nrow();
  int nc = heightmap.ncol();
  if (nr < 2 || nc < 2) {
    NumericMatrix vertices(0, 3);
    vertices.attr("dimnames") = List::create(
      R_NilValue,
      CharacterVector::create("x", "h", "z")
    );
    return List::create(
      _["vertices"] = vertices,
      _["faces"] = IntegerMatrix(0, 3),
      _["face_edges"] = IntegerMatrix(0, 3),
      _["face_neighbors"] = IntegerMatrix(0, 3),
      _["face_cells"] = IntegerMatrix(0, 2),
      _["face_cell_row"] = IntegerVector(),
      _["face_cell_col"] = IntegerVector(),
      _["face_cell_id"] = IntegerVector(),
      _["face_type"] = IntegerVector(),
      _["face_heights"] = NumericMatrix(0, 3),
      _["face_projected_area"] = NumericVector(),
      _["cell_face_id"] = IntegerMatrix(0, 2),
      _["edge_vertices"] = IntegerMatrix(0, 2),
      _["edge_face_count"] = IntegerVector(),
      _["edge_first_face"] = IntegerVector(),
      _["edge_first_face_edge"] = IntegerVector(),
      _["edge_min_height"] = NumericVector(),
      _["nr"] = nr,
      _["nc"] = nc,
      _["x_edge_count"] = 0,
      _["z_edge_count"] = 0,
      _["diag_edge_count"] = 0
    );
  }

  int vertex_count = nr * nc;
  NumericMatrix vertices(vertex_count, 3);
  vertices.attr("dimnames") = List::create(
    R_NilValue,
    CharacterVector::create("x", "h", "z")
  );
  double x_offset = (nr - 1.0) / 2.0;
  double z_offset = (nc - 1.0) / 2.0;
  for (int col = 0; col < nc; ++col) {
    for (int row = 0; row < nr; ++row) {
      int vertex_id0 = row + col * nr;
      vertices(vertex_id0, 0) = row - x_offset;
      vertices(vertex_id0, 1) = heightmap(row, col);
      vertices(vertex_id0, 2) = col - z_offset;
    }
  }

  int cell_count = (nr - 1) * (nc - 1);
  std::vector<int> render_cells;
  render_cells.reserve(cell_count);
  for (int col = 0; col < nc - 1; ++col) {
    for (int row = 0; row < nr - 1; ++row) {
      double h00 = heightmap(row, col);
      double h01 = heightmap(row, col + 1);
      double h10 = heightmap(row + 1, col);
      double h11 = heightmap(row + 1, col + 1);
      if (
        finite_double(h00) &&
          finite_double(h01) &&
          finite_double(h10) &&
          finite_double(h11)
      ) {
        render_cells.push_back(row + col * (nr - 1) + 1);
      }
    }
  }

  int x_edge_count = (nr - 1) * nc;
  int z_edge_count = nr * (nc - 1);
  int diag_edge_count = cell_count;
  int edge_count = x_edge_count + z_edge_count + diag_edge_count;
  IntegerMatrix edge_vertices(edge_count, 2);
  for (int col = 0; col < nc; ++col) {
    for (int row = 0; row < nr - 1; ++row) {
      int edge_id0 = col * (nr - 1) + row;
      int v0 = row + col * nr + 1;
      edge_vertices(edge_id0, 0) = v0;
      edge_vertices(edge_id0, 1) = v0 + 1;
    }
  }
  for (int col = 0; col < nc - 1; ++col) {
    for (int row = 0; row < nr; ++row) {
      int edge_id0 = x_edge_count + col * nr + row;
      int v0 = row + col * nr + 1;
      edge_vertices(edge_id0, 0) = v0;
      edge_vertices(edge_id0, 1) = v0 + nr;
    }
  }
  for (int col = 0; col < nc - 1; ++col) {
    for (int row = 0; row < nr - 1; ++row) {
      int cell_id = row + col * (nr - 1) + 1;
      int edge_id0 = x_edge_count + z_edge_count + cell_id - 1;
      int v00 = row + col * nr + 1;
      edge_vertices(edge_id0, 0) = v00 + 1;
      edge_vertices(edge_id0, 1) = v00 + nr;
    }
  }

  int face_count = static_cast<int>(render_cells.size()) * 2;
  IntegerMatrix faces(face_count, 3);
  IntegerMatrix face_edges(face_count, 3);
  IntegerMatrix face_neighbors(face_count, 3);
  IntegerMatrix face_cells(face_count, 2);
  IntegerVector face_cell_row(face_count);
  IntegerVector face_cell_col(face_count);
  IntegerVector face_cell_id(face_count);
  IntegerVector face_type(face_count);
  NumericMatrix face_heights(face_count, 3);
  NumericVector face_projected_area(face_count, 0.5);
  IntegerMatrix cell_face_id(cell_count, 2);
  std::vector<int> slot_to_face_id(cell_count * 2, 0);

  for (int render_index = 0; render_index < static_cast<int>(render_cells.size()); ++render_index) {
    int cell_id = render_cells[render_index];
    int cell_id0 = cell_id - 1;
    int row = cell_id0 % (nr - 1);
    int col = cell_id0 / (nr - 1);
    int top_face0 = render_index * 2;
    int bottom_face0 = top_face0 + 1;
    int top_face = top_face0 + 1;
    int bottom_face = bottom_face0 + 1;
    int v00 = row + col * nr + 1;
    int v01 = v00 + nr;
    int v10 = v00 + 1;
    int v11 = v01 + 1;

    slot_to_face_id[2 * cell_id0] = top_face;
    slot_to_face_id[2 * cell_id0 + 1] = bottom_face;
    cell_face_id(cell_id0, 0) = top_face;
    cell_face_id(cell_id0, 1) = bottom_face;

    faces(top_face0, 0) = v00;
    faces(top_face0, 1) = v01;
    faces(top_face0, 2) = v10;
    faces(bottom_face0, 0) = v10;
    faces(bottom_face0, 1) = v01;
    faces(bottom_face0, 2) = v11;

    face_edges(top_face0, 0) = z_edge_id(row, col, nr, x_edge_count);
    face_edges(top_face0, 1) = diag_edge_id(cell_id, x_edge_count, z_edge_count);
    face_edges(top_face0, 2) = x_edge_id(row, col, nr);
    face_edges(bottom_face0, 0) = diag_edge_id(cell_id, x_edge_count, z_edge_count);
    face_edges(bottom_face0, 1) = x_edge_id(row, col + 1, nr);
    face_edges(bottom_face0, 2) = z_edge_id(row + 1, col, nr, x_edge_count);

    face_cell_row[top_face0] = row + 1;
    face_cell_row[bottom_face0] = row + 1;
    face_cell_col[top_face0] = col + 1;
    face_cell_col[bottom_face0] = col + 1;
    face_cell_id[top_face0] = cell_id;
    face_cell_id[bottom_face0] = cell_id;
    face_type[top_face0] = 1;
    face_type[bottom_face0] = 2;
    face_cells(top_face0, 0) = row + 1;
    face_cells(top_face0, 1) = col + 1;
    face_cells(bottom_face0, 0) = row + 1;
    face_cells(bottom_face0, 1) = col + 1;

    face_heights(top_face0, 0) = vertices(v00 - 1, 1);
    face_heights(top_face0, 1) = vertices(v01 - 1, 1);
    face_heights(top_face0, 2) = vertices(v10 - 1, 1);
    face_heights(bottom_face0, 0) = vertices(v10 - 1, 1);
    face_heights(bottom_face0, 1) = vertices(v01 - 1, 1);
    face_heights(bottom_face0, 2) = vertices(v11 - 1, 1);
  }

  for (int render_index = 0; render_index < static_cast<int>(render_cells.size()); ++render_index) {
    int cell_id = render_cells[render_index];
    int cell_id0 = cell_id - 1;
    int row = cell_id0 % (nr - 1);
    int col = cell_id0 / (nr - 1);
    int top_face0 = render_index * 2;
    int bottom_face0 = top_face0 + 1;
    int top_slot = 2 * cell_id - 1;
    int bottom_slot = 2 * cell_id;

    face_neighbors(top_face0, 0) = row > 0 ?
      slot_to_face(slot_to_face_id, 2 * (cell_id - 1)) :
      0;
    face_neighbors(top_face0, 1) = slot_to_face(slot_to_face_id, bottom_slot);
    face_neighbors(top_face0, 2) = col > 0 ?
      slot_to_face(slot_to_face_id, 2 * (cell_id - (nr - 1))) :
      0;

    face_neighbors(bottom_face0, 0) = slot_to_face(slot_to_face_id, top_slot);
    face_neighbors(bottom_face0, 1) = col < nc - 2 ?
      slot_to_face(slot_to_face_id, 2 * (cell_id + (nr - 1)) - 1) :
      0;
    face_neighbors(bottom_face0, 2) = row < nr - 2 ?
      slot_to_face(slot_to_face_id, 2 * (cell_id + 1) - 1) :
      0;
  }

  IntegerVector edge_face_count(edge_count);
  IntegerVector edge_first_face(edge_count);
  IntegerVector edge_first_face_edge(edge_count);
  for (int face = 0; face < face_count; ++face) {
    for (int edge_index = 0; edge_index < 3; ++edge_index) {
      int edge_id = face_edges(face, edge_index);
      if (edge_id > 0) {
        edge_face_count[edge_id - 1] += 1;
        edge_first_face[edge_id - 1] = face + 1;
        edge_first_face_edge[edge_id - 1] = edge_index + 1;
      }
    }
  }

  NumericVector edge_min_height(edge_count);
  for (int edge = 0; edge < edge_count; ++edge) {
    double h0 = vertices(edge_vertices(edge, 0) - 1, 1);
    double h1 = vertices(edge_vertices(edge, 1) - 1, 1);
    edge_min_height[edge] = std::min(h0, h1);
  }

  return List::create(
    _["vertices"] = vertices,
    _["faces"] = faces,
    _["face_edges"] = face_edges,
    _["face_neighbors"] = face_neighbors,
    _["face_cells"] = face_cells,
    _["face_cell_row"] = face_cell_row,
    _["face_cell_col"] = face_cell_col,
    _["face_cell_id"] = face_cell_id,
    _["face_type"] = face_type,
    _["face_heights"] = face_heights,
    _["face_projected_area"] = face_projected_area,
    _["cell_face_id"] = cell_face_id,
    _["edge_vertices"] = edge_vertices,
    _["edge_face_count"] = edge_face_count,
    _["edge_first_face"] = edge_first_face,
    _["edge_first_face_edge"] = edge_first_face_edge,
    _["edge_min_height"] = edge_min_height,
    _["nr"] = nr,
    _["nc"] = nc,
    _["x_edge_count"] = x_edge_count,
    _["z_edge_count"] = z_edge_count,
    _["diag_edge_count"] = diag_edge_count
  );
}

// [[Rcpp::export]]
NumericVector spatial_water_face_sublevel_area_cpp(
  List terrain_mesh,
  IntegerVector face_ids,
  double water_level,
  double height_tol
) {
  NumericMatrix face_heights = terrain_mesh["face_heights"];
  NumericVector face_projected_area = terrain_mesh["face_projected_area"];
  int n = face_ids.size();
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    int face_id0 = face_ids[i] - 1;
    if (face_id0 < 0 || face_id0 >= face_heights.nrow()) {
      out[i] = NA_REAL;
      continue;
    }
    out[i] = triangle_sublevel_area_scalar(
      face_heights(face_id0, 0),
      face_heights(face_id0, 1),
      face_heights(face_id0, 2),
      face_projected_area[face_id0],
      water_level,
      height_tol
    );
  }
  return out;
}

// [[Rcpp::export]]
List spatial_water_traverse_seeded_clipped_faces_cpp(
  List terrain_mesh,
  List component_seed,
  double water_level,
  double target_area_limit,
  double height_tol,
  double length_tol,
  double area_tol,
  bool return_face_ids = false
) {
  IntegerMatrix faces = terrain_mesh["faces"];
  int face_count = faces.nrow();
  IntegerVector seed_face_ids = component_seed["seed_face_ids"];
  int seed_candidate_count = seed_face_ids.size();
  if (face_count == 0 || seed_candidate_count == 0) {
    return empty_traversal_result(seed_candidate_count, return_face_ids);
  }

  NumericMatrix face_heights = terrain_mesh["face_heights"];
  NumericVector face_projected_area = terrain_mesh["face_projected_area"];
  IntegerMatrix face_neighbors = terrain_mesh["face_neighbors"];
  IntegerMatrix face_edges = terrain_mesh["face_edges"];
  NumericVector edge_min_height = terrain_mesh["edge_min_height"];

  bool full_scene = Rcpp::as<bool>(component_seed["full_scene"]);
  if (full_scene) {
    double area = 0.0;
    bool rejected = false;
    std::vector<int> selected_faces;
    if (return_face_ids) {
      selected_faces.reserve(face_count);
    }
    int visited_count = 0;
    int seed_face_count = 0;
    for (int face = 0; face < face_count; ++face) {
      double face_area = triangle_sublevel_area_scalar(
        face_heights(face, 0),
        face_heights(face, 1),
        face_heights(face, 2),
        face_projected_area[face],
        water_level,
        height_tol
      );
      if (face_area <= area_tol) {
        continue;
      }
      ++seed_face_count;
      ++visited_count;
      area += face_area;
      if (return_face_ids) {
        selected_faces.push_back(face + 1);
      }
      if (
        std::isfinite(target_area_limit) &&
          area > target_area_limit + area_tol
      ) {
        rejected = true;
        break;
      }
    }
    IntegerVector face_ids;
    if (return_face_ids) {
      face_ids = wrap(selected_faces);
    }
    return List::create(
      _["area"] = area,
      _["rejected"] = rejected,
      _["face_ids"] = face_ids,
      _["diagnostics"] = List::create(
        _["seed_candidate_count"] = face_count,
        _["seed_face_count"] = seed_face_count,
        _["visited_face_count"] = visited_count,
        _["rejected_early"] = rejected,
        _["geometry_face_count"] = return_face_ids ? static_cast<int>(selected_faces.size()) : 0
      )
    );
  }

  NumericMatrix vertices = terrain_mesh["vertices"];
  IntegerVector face_cell_row = terrain_mesh["face_cell_row"];
  IntegerVector face_cell_col = terrain_mesh["face_cell_col"];
  int nr = Rcpp::as<int>(terrain_mesh["nr"]);
  int nc = Rcpp::as<int>(terrain_mesh["nc"]);
  LogicalMatrix mask = component_seed["mask"];
  Environment state = component_seed["state"];

  IntegerVector visited_generation;
  if (
    state.exists("visited_generation") &&
      Rf_length(state["visited_generation"]) == face_count
  ) {
    visited_generation = state["visited_generation"];
  } else {
    visited_generation = IntegerVector(face_count);
  }

  IntegerVector queue;
  if (state.exists("queue") && Rf_length(state["queue"]) == face_count) {
    queue = state["queue"];
  } else {
    queue = IntegerVector(face_count);
  }

  int generation = 0;
  if (state.exists("generation")) {
    generation = Rcpp::as<int>(state["generation"]);
  }
  generation += 1;
  if (generation <= 0) {
    std::fill(visited_generation.begin(), visited_generation.end(), 0);
    generation = 1;
  }
  state["generation"] = generation;

  int queue_head = 0;
  int queue_tail = 0;
  int seed_face_count = 0;

  for (int i = 0; i < seed_candidate_count; ++i) {
    int face_id = seed_face_ids[i];
    int face_id0 = face_id - 1;
    if (face_id0 < 0 || face_id0 >= face_count) {
      continue;
    }
    if (visited_generation[face_id0] == generation) {
      continue;
    }
    double face_area = triangle_sublevel_area_scalar(
      face_heights(face_id0, 0),
      face_heights(face_id0, 1),
      face_heights(face_id0, 2),
      face_projected_area[face_id0],
      water_level,
      height_tol
    );
    if (face_area <= area_tol) {
      continue;
    }
    if (!seed_face_has_positive_overlap_cpp(
      faces,
      face_cell_row,
      face_cell_col,
      vertices,
      mask,
      nr,
      nc,
      face_id0,
      water_level,
      height_tol,
      length_tol,
      area_tol
    )) {
      continue;
    }
    queue[queue_tail] = face_id;
    queue_tail += 1;
    seed_face_count += 1;
    visited_generation[face_id0] = generation;
  }

  if (queue_tail == 0) {
    state["visited_generation"] = visited_generation;
    state["queue"] = queue;
    return empty_traversal_result(seed_candidate_count, return_face_ids);
  }

  double accumulated_area = 0.0;
  bool rejected = false;
  while (queue_head < queue_tail) {
    int face_id = queue[queue_head];
    int face_id0 = face_id - 1;
    queue_head += 1;
    accumulated_area += triangle_sublevel_area_scalar(
      face_heights(face_id0, 0),
      face_heights(face_id0, 1),
      face_heights(face_id0, 2),
      face_projected_area[face_id0],
      water_level,
      height_tol
    );
    if (
      std::isfinite(target_area_limit) &&
        accumulated_area > target_area_limit + area_tol
    ) {
      rejected = true;
      break;
    }
    for (int edge_index = 0; edge_index < 3; ++edge_index) {
      int neighbor = face_neighbors(face_id0, edge_index);
      if (neighbor <= 0) {
        continue;
      }
      int neighbor0 = neighbor - 1;
      if (visited_generation[neighbor0] == generation) {
        continue;
      }
      int edge_id = face_edges(face_id0, edge_index);
      if (edge_min_height[edge_id - 1] < water_level - height_tol) {
        queue[queue_tail] = neighbor;
        queue_tail += 1;
        visited_generation[neighbor0] = generation;
      }
    }
  }

  state["visited_generation"] = visited_generation;
  state["queue"] = queue;

  int returned_face_count = return_face_ids ? queue_tail : 0;
  IntegerVector face_ids(returned_face_count);
  for (int i = 0; i < returned_face_count; ++i) {
    face_ids[i] = queue[i];
  }

  return List::create(
    _["area"] = accumulated_area,
    _["rejected"] = rejected,
    _["face_ids"] = face_ids,
    _["diagnostics"] = List::create(
      _["seed_candidate_count"] = seed_candidate_count,
      _["seed_face_count"] = seed_face_count,
      _["visited_face_count"] = rejected ? queue_head : queue_tail,
      _["rejected_early"] = rejected,
      _["geometry_face_count"] = return_face_ids ? queue_tail : 0
    )
  );
}

// [[Rcpp::export]]
List build_spatial_water_full_terrain_geometry_cpp(
  List terrain_mesh,
  double water_level,
  double surface_area_tol
) {
  NumericMatrix vertices = terrain_mesh["vertices"];
  IntegerMatrix faces = terrain_mesh["faces"];
  IntegerMatrix edge_vertices = terrain_mesh["edge_vertices"];
  IntegerVector edge_face_count = terrain_mesh["edge_face_count"];
  IntegerVector edge_first_face = terrain_mesh["edge_first_face"];
  IntegerVector edge_first_face_edge = terrain_mesh["edge_first_face_edge"];

  int vertex_count = vertices.nrow();
  int face_count = faces.nrow();
  NumericMatrix top_vertex_table(vertex_count, 3);
  for (int vertex = 0; vertex < vertex_count; ++vertex) {
    top_vertex_table(vertex, 0) = vertices(vertex, 0);
    top_vertex_table(vertex, 1) = water_level;
    top_vertex_table(vertex, 2) = vertices(vertex, 2);
  }

  NumericMatrix top_vertices(face_count * 3, 3);
  for (int face = 0; face < face_count; ++face) {
    for (int point = 0; point < 3; ++point) {
      int vertex_id0 = faces(face, point) - 1;
      int out_row = face * 3 + point;
      top_vertices(out_row, 0) = top_vertex_table(vertex_id0, 0);
      top_vertices(out_row, 1) = top_vertex_table(vertex_id0, 1);
      top_vertices(out_row, 2) = top_vertex_table(vertex_id0, 2);
    }
  }

  std::vector<int> boundary_edge_ids;
  boundary_edge_ids.reserve(edge_face_count.size());
  for (int edge = 0; edge < edge_face_count.size(); ++edge) {
    if (edge_face_count[edge] == 1) {
      boundary_edge_ids.push_back(edge + 1);
    }
  }

  NumericMatrix lines(boundary_edge_ids.size() * 2, 3);
  std::vector<double> side_values;
  side_values.reserve(boundary_edge_ids.size() * 18);
  IntegerVector boundary_v1(boundary_edge_ids.size());
  IntegerVector boundary_v2(boundary_edge_ids.size());
  CharacterVector boundary_kind(boundary_edge_ids.size());
  IntegerVector boundary_edge_id(boundary_edge_ids.size());
  LogicalVector boundary_wall(boundary_edge_ids.size());

  for (int i = 0; i < static_cast<int>(boundary_edge_ids.size()); ++i) {
    int edge_id = boundary_edge_ids[i];
    int edge_id0 = edge_id - 1;
    int line_a = edge_vertices(edge_id0, 0);
    int line_b = edge_vertices(edge_id0, 1);
    lines(i * 2, 0) = top_vertex_table(line_a - 1, 0);
    lines(i * 2, 1) = top_vertex_table(line_a - 1, 1);
    lines(i * 2, 2) = top_vertex_table(line_a - 1, 2);
    lines(i * 2 + 1, 0) = top_vertex_table(line_b - 1, 0);
    lines(i * 2 + 1, 1) = top_vertex_table(line_b - 1, 1);
    lines(i * 2 + 1, 2) = top_vertex_table(line_b - 1, 2);

    int face_id0 = edge_first_face[edge_id0] - 1;
    int face_edge0 = edge_first_face_edge[edge_id0] - 1;
    int next_edge = face_edge0 == 2 ? 0 : face_edge0 + 1;
    int vertex_a = faces(face_id0, face_edge0);
    int vertex_b = faces(face_id0, next_edge);
    ClipRecord record_a = original_vertex_record(vertices, vertex_a);
    ClipRecord record_b = original_vertex_record(vertices, vertex_b);
    std::size_t before = side_values.size();
    append_sidewall(
      side_values,
      record_a,
      record_b,
      water_level,
      surface_area_tol
    );

    boundary_v1[i] = vertex_a;
    boundary_v2[i] = vertex_b;
    boundary_kind[i] = "original";
    boundary_edge_id[i] = edge_id;
    boundary_wall[i] = side_values.size() > before;
  }

  NumericMatrix side_vertices(side_values.size() / 3, 3);
  for (int i = 0; i < static_cast<int>(side_values.size() / 3); ++i) {
    side_vertices(i, 0) = side_values[i * 3];
    side_vertices(i, 1) = side_values[i * 3 + 1];
    side_vertices(i, 2) = side_values[i * 3 + 2];
  }

  DataFrame boundary_edges = DataFrame::create(
    _["v1"] = boundary_v1,
    _["v2"] = boundary_v2,
    _["kind"] = boundary_kind,
    _["edge_id"] = boundary_edge_id,
    _["wall"] = boundary_wall,
    _["stringsAsFactors"] = false
  );

  return List::create(
    _["top_vertices"] = top_vertices,
    _["side_vertices"] = side_vertices,
    _["lines"] = lines,
    _["top_vertex_table"] = top_vertex_table,
    _["top_faces"] = faces,
    _["boundary_edges"] = boundary_edges
  );
}

// [[Rcpp::export]]
List build_spatial_water_triangle_clipped_geometry_cpp(
  List terrain_mesh,
  IntegerVector selected_face_ids,
  double water_level,
  double height_tol,
  double t_tol,
  double length_tol,
  double area_tol,
  double surface_area_tol
) {
  NumericMatrix vertices = terrain_mesh["vertices"];
  IntegerMatrix faces = terrain_mesh["faces"];
  IntegerMatrix face_edges = terrain_mesh["face_edges"];
  IntegerMatrix edge_vertices = terrain_mesh["edge_vertices"];
  IntegerVector edge_face_count = terrain_mesh["edge_face_count"];
  int nr = Rcpp::as<int>(terrain_mesh["nr"]);
  int x_edge_count = Rcpp::as<int>(terrain_mesh["x_edge_count"]);
  int z_edge_count = Rcpp::as<int>(terrain_mesh["z_edge_count"]);
  int face_count = faces.nrow();
  if (selected_face_ids.size() == 0 || face_count == 0) {
    return List::create(
      _["top_vertices"] = NumericMatrix(0, 3),
      _["side_vertices"] = NumericMatrix(0, 3),
      _["lines"] = NumericMatrix(0, 3),
      _["top_vertex_table"] = NumericMatrix(0, 3),
      _["top_faces"] = IntegerMatrix(0, 3),
      _["boundary_edges"] = DataFrame::create(
        _["v1"] = IntegerVector(),
        _["v2"] = IntegerVector(),
        _["kind"] = CharacterVector(),
        _["edge_id"] = IntegerVector(),
        _["wall"] = LogicalVector(),
        _["stringsAsFactors"] = false
      )
    );
  }

  EdgeIntersections edge_intersections = make_edge_intersections(
    vertices,
    edge_vertices,
    water_level,
    height_tol
  );
  int max_key = vertices.nrow() + edge_vertices.nrow();
  std::vector<int> top_index(max_key + 1, 0);
  std::vector<double> top_x;
  std::vector<double> top_z;
  top_x.reserve(selected_face_ids.size() * 2);
  top_z.reserve(selected_face_ids.size() * 2);
  std::vector<int> top_faces;
  top_faces.reserve(selected_face_ids.size() * 6);
  std::unordered_map<uint64_t, BoundaryEntry> boundary;
  boundary.reserve(selected_face_ids.size() * 3);

  auto add_top_vertex = [&](const ClipRecord& record) {
    int key = record.key;
    if (key <= 0 || key > max_key) {
      stop("Invalid spatial water vertex key generated during clipping.");
    }
    if (top_index[key] == 0) {
      top_index[key] = static_cast<int>(top_x.size()) + 1;
      top_x.push_back(record.x);
      top_z.push_back(record.z);
    }
    return top_index[key];
  };

  auto register_boundary = [&](const ClipRecord& a, const ClipRecord& b) {
    if (a.key == b.key) {
      return;
    }
    uint64_t key = boundary_key(a.key, b.key);
    int original_edge_id = records_shared_original_edge_id(
      a,
      b,
      edge_vertices,
      nr,
      x_edge_count,
      z_edge_count
    );
    auto found = boundary.find(key);
    if (found == boundary.end()) {
      BoundaryEntry entry;
      entry.count = 0;
      entry.a = a;
      entry.b = b;
      entry.kind = original_edge_id == NA_INTEGER ? 1 : 0;
      entry.edge_id = original_edge_id;
      found = boundary.emplace(key, entry).first;
    }
    found->second.count += 1;
  };

  for (int i = 0; i < selected_face_ids.size(); ++i) {
    int face_id = selected_face_ids[i];
    if (face_id <= 0 || face_id > face_count) {
      continue;
    }
    std::vector<ClipRecord> records = clip_face_records(
      vertices,
      faces,
      face_edges,
      edge_vertices,
      edge_intersections,
      face_id,
      water_level,
      height_tol,
      t_tol,
      length_tol
    );
    if (records.size() < 3 || records_projected_area(records) <= area_tol) {
      continue;
    }

    std::vector<int> vertex_indices;
    vertex_indices.reserve(records.size());
    for (const ClipRecord& record : records) {
      vertex_indices.push_back(add_top_vertex(record));
    }
    for (int triangle = 0; triangle < static_cast<int>(vertex_indices.size()) - 2; ++triangle) {
      int a = vertex_indices[0];
      int b = vertex_indices[triangle + 1];
      int c = vertex_indices[triangle + 2];
      double normal_y = triangle_normal_y(
        top_x[a - 1],
        top_z[a - 1],
        top_x[b - 1],
        top_z[b - 1],
        top_x[c - 1],
        top_z[c - 1]
      );
      if (std::abs(normal_y) <= area_tol) {
        continue;
      }
      if (normal_y < 0) {
        std::swap(b, c);
      }
      top_faces.push_back(a);
      top_faces.push_back(b);
      top_faces.push_back(c);
    }
    for (int point = 0; point < static_cast<int>(records.size()); ++point) {
      int next = point == static_cast<int>(records.size()) - 1 ? 0 : point + 1;
      register_boundary(records[point], records[next]);
    }
  }

  int top_vertex_count = static_cast<int>(top_x.size());
  int top_face_count = static_cast<int>(top_faces.size()) / 3;
  if (top_vertex_count == 0 || top_face_count == 0) {
    return List::create(
      _["top_vertices"] = NumericMatrix(0, 3),
      _["side_vertices"] = NumericMatrix(0, 3),
      _["lines"] = NumericMatrix(0, 3),
      _["top_vertex_table"] = NumericMatrix(0, 3),
      _["top_faces"] = IntegerMatrix(0, 3),
      _["boundary_edges"] = DataFrame::create(
        _["v1"] = IntegerVector(),
        _["v2"] = IntegerVector(),
        _["kind"] = CharacterVector(),
        _["edge_id"] = IntegerVector(),
        _["wall"] = LogicalVector(),
        _["stringsAsFactors"] = false
      )
    );
  }

  NumericMatrix top_vertex_table(top_vertex_count, 3);
  for (int vertex = 0; vertex < top_vertex_count; ++vertex) {
    top_vertex_table(vertex, 0) = top_x[vertex];
    top_vertex_table(vertex, 1) = water_level;
    top_vertex_table(vertex, 2) = top_z[vertex];
  }

  IntegerMatrix top_faces_matrix(top_face_count, 3);
  NumericMatrix top_vertices(top_face_count * 3, 3);
  for (int face = 0; face < top_face_count; ++face) {
    for (int point = 0; point < 3; ++point) {
      int top_vertex_id = top_faces[face * 3 + point];
      top_faces_matrix(face, point) = top_vertex_id;
      int out_row = face * 3 + point;
      top_vertices(out_row, 0) = top_vertex_table(top_vertex_id - 1, 0);
      top_vertices(out_row, 1) = top_vertex_table(top_vertex_id - 1, 1);
      top_vertices(out_row, 2) = top_vertex_table(top_vertex_id - 1, 2);
    }
  }

  std::vector<double> line_values;
  std::vector<double> side_values;
  std::vector<int> boundary_v1;
  std::vector<int> boundary_v2;
  std::vector<std::string> boundary_kind;
  std::vector<int> boundary_edge_id;
  std::vector<int> boundary_wall;
  line_values.reserve(boundary.size() * 6);
  side_values.reserve(boundary.size() * 9);

  for (const auto& item : boundary) {
    const BoundaryEntry& entry = item.second;
    if (entry.count != 1) {
      continue;
    }
    int index_a = top_index[entry.a.key];
    int index_b = top_index[entry.b.key];
    if (index_a == 0 || index_b == 0) {
      continue;
    }
    line_values.insert(
      line_values.end(),
      {
        top_vertex_table(index_a - 1, 0),
        top_vertex_table(index_a - 1, 1),
        top_vertex_table(index_a - 1, 2),
        top_vertex_table(index_b - 1, 0),
        top_vertex_table(index_b - 1, 1),
        top_vertex_table(index_b - 1, 2)
      }
    );
    bool has_wall = false;
    if (
      entry.kind == 0 &&
        entry.edge_id != NA_INTEGER &&
        edge_face_count[entry.edge_id - 1] == 1
    ) {
      std::size_t before = side_values.size();
      append_sidewall(
        side_values,
        entry.a,
        entry.b,
        water_level,
        surface_area_tol
      );
      has_wall = side_values.size() > before;
    }
    boundary_v1.push_back(index_a);
    boundary_v2.push_back(index_b);
    boundary_kind.push_back(entry.kind == 0 ? "original" : "contour");
    boundary_edge_id.push_back(
      entry.edge_id == NA_INTEGER ? NA_INTEGER : entry.edge_id
    );
    boundary_wall.push_back(has_wall ? TRUE : FALSE);
  }

  NumericMatrix lines(line_values.size() / 3, 3);
  for (int i = 0; i < static_cast<int>(line_values.size() / 3); ++i) {
    lines(i, 0) = line_values[i * 3];
    lines(i, 1) = line_values[i * 3 + 1];
    lines(i, 2) = line_values[i * 3 + 2];
  }
  NumericMatrix side_vertices(side_values.size() / 3, 3);
  for (int i = 0; i < static_cast<int>(side_values.size() / 3); ++i) {
    side_vertices(i, 0) = side_values[i * 3];
    side_vertices(i, 1) = side_values[i * 3 + 1];
    side_vertices(i, 2) = side_values[i * 3 + 2];
  }

  DataFrame boundary_edges = DataFrame::create(
    _["v1"] = wrap(boundary_v1),
    _["v2"] = wrap(boundary_v2),
    _["kind"] = wrap(boundary_kind),
    _["edge_id"] = wrap(boundary_edge_id),
    _["wall"] = LogicalVector(boundary_wall.begin(), boundary_wall.end()),
    _["stringsAsFactors"] = false
  );

  return List::create(
    _["top_vertices"] = top_vertices,
    _["side_vertices"] = side_vertices,
    _["lines"] = lines,
    _["top_vertex_table"] = top_vertex_table,
    _["top_faces"] = top_faces_matrix,
    _["boundary_edges"] = boundary_edges
  );
}
