#include <Rcpp.h>
#include <RcppThread.h>

#include <algorithm>
#include <array>
#include <atomic>
#include <cmath>
#include <cstdio>
#include <exception>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>
#include <utility>
#include <vector>

using namespace Rcpp;

namespace {

inline bool normalize_road_vector(
    double x,
    double z,
    double& normalized_x,
    double& normalized_z) {
  const double magnitude = std::sqrt(x * x + z * z);
  if (!std::isfinite(magnitude) ||
      magnitude <= std::sqrt(std::numeric_limits<double>::epsilon())) {
    normalized_x = NA_REAL;
    normalized_z = NA_REAL;
    return false;
  }
  normalized_x = x / magnitude;
  normalized_z = z / magnitude;
  return std::isfinite(normalized_x) && std::isfinite(normalized_z);
}

std::vector<double> unique_render_line_t_cpp(
    const std::vector<double>& values) {
  const double tolerance =
      std::sqrt(std::numeric_limits<double>::epsilon());
  constexpr double rounding_scale = 1e12;
  std::vector<double> result;
  result.reserve(values.size());
  for (double value : values) {
    if (!std::isfinite(value) ||
        value < -tolerance ||
        value > 1.0 + tolerance) {
      continue;
    }
    value = std::min(std::max(value, 0.0), 1.0);
    result.push_back(std::nearbyint(value * rounding_scale) / rounding_scale);
  }
  std::sort(result.begin(), result.end());
  result.erase(std::unique(result.begin(), result.end()), result.end());
  return result;
}

void append_render_line_axis_boundary_t_cpp(
    double start,
    double end,
    int lower,
    int upper,
    std::vector<double>& values) {
  const double tolerance =
      std::sqrt(std::numeric_limits<double>::epsilon());
  const double delta = end - start;
  if (!std::isfinite(delta) || std::abs(delta) <= tolerance) {
    return;
  }
  const double minimum = std::min(start, end);
  const double maximum = std::max(start, end);
  const int boundary_min = static_cast<int>(std::max(
      static_cast<double>(lower),
      std::ceil(minimum)));
  const int boundary_max = static_cast<int>(std::min(
      static_cast<double>(upper),
      std::floor(maximum)));
  for (int boundary = boundary_min; boundary <= boundary_max; ++boundary) {
    if (boundary > minimum + tolerance &&
        boundary < maximum - tolerance) {
      values.push_back((boundary - start) / delta);
    }
  }
}

std::vector<double> calculate_render_line_triangle_boundary_t_cpp(
    double start_x,
    double start_z,
    double end_x,
    double end_z,
    int row_count,
    int column_count) {
  if (row_count < 2 || column_count < 2) {
    return {0.0, 1.0};
  }
  const double row_offset = (row_count - 1.0) / 2.0 + 1.0;
  const double column_offset = (column_count - 1.0) / 2.0 + 1.0;
  const double row0 = start_x + row_offset;
  const double row1 = end_x + row_offset;
  const double col0 = start_z + column_offset;
  const double col1 = end_z + column_offset;
  std::vector<double> grid_values = {0.0, 1.0};
  append_render_line_axis_boundary_t_cpp(
      row0, row1, 1, row_count, grid_values);
  append_render_line_axis_boundary_t_cpp(
      col0, col1, 1, column_count, grid_values);
  std::vector<double> grid_t = unique_render_line_t_cpp(grid_values);

  const double tolerance =
      std::sqrt(std::numeric_limits<double>::epsilon());
  const double row_delta = row1 - row0;
  const double col_delta = col1 - col0;
  const double diagonal_delta = row_delta + col_delta;
  if (!std::isfinite(diagonal_delta) ||
      std::abs(diagonal_delta) <= tolerance) {
    return grid_t;
  }
  std::vector<double> combined = grid_t;
  for (std::size_t index = 0; index + 1 < grid_t.size(); ++index) {
    const double interval_start = grid_t[index];
    const double interval_end = grid_t[index + 1];
    if (interval_end - interval_start <= tolerance) {
      continue;
    }
    const double interval_mid = (interval_start + interval_end) / 2.0;
    const double row_mid = row0 + row_delta * interval_mid;
    const double col_mid = col0 + col_delta * interval_mid;
    if (row_mid < 1.0 || row_mid > row_count ||
        col_mid < 1.0 || col_mid > column_count) {
      continue;
    }
    const double row_cell = std::min(
        std::max(std::floor(row_mid), 1.0),
        row_count - 1.0);
    const double col_cell = std::min(
        std::max(std::floor(col_mid), 1.0),
        column_count - 1.0);
    const double target_sum = row_cell + col_cell + 1.0;
    const double crossing_t =
        (target_sum - row0 - col0) / diagonal_delta;
    if (std::isfinite(crossing_t) &&
        crossing_t > interval_start + tolerance &&
        crossing_t < interval_end - tolerance) {
      combined.push_back(crossing_t);
    }
  }
  return unique_render_line_t_cpp(combined);
}

struct RoadVec3 {
  double x;
  double y;
  double z;
};

inline RoadVec3 road_vec3_add(const RoadVec3& first, const RoadVec3& second) {
  return {
      first.x + second.x,
      first.y + second.y,
      first.z + second.z};
}

inline RoadVec3 road_vec3_subtract(
    const RoadVec3& first,
    const RoadVec3& second) {
  return {
      first.x - second.x,
      first.y - second.y,
      first.z - second.z};
}

inline RoadVec3 road_vec3_scale(const RoadVec3& value, double scale) {
  return {value.x * scale, value.y * scale, value.z * scale};
}

inline RoadVec3 road_vec3_cross(const RoadVec3& first, const RoadVec3& second) {
  return {
      first.y * second.z - first.z * second.y,
      first.z * second.x - first.x * second.z,
      first.x * second.y - first.y * second.x};
}

inline double road_vec3_dot(const RoadVec3& first, const RoadVec3& second) {
  return first.x * second.x + first.y * second.y + first.z * second.z;
}

inline double road_vec3_length(const RoadVec3& value) {
  return std::sqrt(road_vec3_dot(value, value));
}

inline bool road_vec3_finite(const RoadVec3& value) {
  return std::isfinite(value.x) &&
      std::isfinite(value.y) &&
      std::isfinite(value.z);
}

inline bool normalize_road_vec3(const RoadVec3& value, RoadVec3& result) {
  const double length = road_vec3_length(value);
  if (!std::isfinite(length) ||
      length <= std::sqrt(std::numeric_limits<double>::epsilon())) {
    const double missing = std::numeric_limits<double>::quiet_NaN();
    result = {missing, missing, missing};
    return false;
  }
  result = road_vec3_scale(value, 1.0 / length);
  return road_vec3_finite(result);
}

std::vector<RoadVec3> copy_road_vec3_matrix(const NumericMatrix& values) {
  std::vector<RoadVec3> result(values.nrow());
  for (int row = 0; row < values.nrow(); ++row) {
    result[row] = {values(row, 0), values(row, 1), values(row, 2)};
  }
  return result;
}

NumericMatrix wrap_road_vec3_matrix(const std::vector<RoadVec3>& values) {
  NumericMatrix result(values.size(), 3);
  for (std::size_t row = 0; row < values.size(); ++row) {
    result(row, 0) = values[row].x;
    result(row, 1) = values[row].y;
    result(row, 2) = values[row].z;
  }
  return result;
}

RoadVec3 resolve_road_shared_surface_normal_cpp(
    const std::array<RoadVec3, 3>& face_normals,
    int face_count,
    const RoadVec3& fallback,
    bool& invalid_hemisphere,
    double& minimum_final_dot) {
  const double tolerance =
      std::sqrt(std::numeric_limits<double>::epsilon());
  const double minimum_dot = 1e-8;
  std::array<RoadVec3, 3> valid_faces;
  std::array<RoadVec3, 3> unit_faces;
  int valid_face_count = 0;
  for (int face_index = 0; face_index < face_count; ++face_index) {
    const RoadVec3& face = face_normals[face_index];
    RoadVec3 unit;
    if (!road_vec3_finite(face) || !normalize_road_vec3(face, unit)) {
      continue;
    }
    valid_faces[valid_face_count] = face;
    unit_faces[valid_face_count] = unit;
    ++valid_face_count;
  }
  if (!valid_face_count) {
    return fallback;
  }
  RoadVec3 candidate = {0.0, 0.0, 0.0};
  for (int face_index = 0; face_index < valid_face_count; ++face_index) {
    candidate = road_vec3_add(candidate, valid_faces[face_index]);
  }
  double candidate_length = road_vec3_length(candidate);
  if (!std::isfinite(candidate_length) || candidate_length <= tolerance) {
    candidate = {0.0, 0.0, 0.0};
    for (int face_index = 0; face_index < valid_face_count; ++face_index) {
      candidate = road_vec3_add(candidate, unit_faces[face_index]);
    }
    candidate_length = road_vec3_length(candidate);
  }
  if (!std::isfinite(candidate_length) || candidate_length <= tolerance) {
    candidate = fallback;
  } else {
    candidate = road_vec3_scale(candidate, 1.0 / candidate_length);
  }
  for (int iteration = 0; iteration < 32; ++iteration) {
    std::array<double, 3> face_dots;
    std::array<unsigned char, 3> violations = {0, 0, 0};
    bool has_violation = false;
    for (int index = 0; index < valid_face_count; ++index) {
      const double face_dot = road_vec3_dot(unit_faces[index], candidate);
      face_dots[index] = face_dot;
      if (!std::isfinite(face_dot) || face_dot >= minimum_dot) {
        continue;
      }
      violations[index] = 1;
      has_violation = true;
    }
    if (!has_violation) {
      break;
    }
    for (int index = 0; index < valid_face_count; ++index) {
      if (!violations[index]) {
        continue;
      }
      candidate = road_vec3_add(
          candidate,
          road_vec3_scale(
              unit_faces[index], minimum_dot - face_dots[index]));
    }
  }
  candidate_length = road_vec3_length(candidate);
  if (!std::isfinite(candidate_length) || candidate_length <= tolerance) {
    candidate = fallback;
  } else {
    candidate = road_vec3_scale(candidate, 1.0 / candidate_length);
  }
  minimum_final_dot = std::numeric_limits<double>::infinity();
  for (int face_index = 0; face_index < valid_face_count; ++face_index) {
    const double final_dot = road_vec3_dot(unit_faces[face_index], candidate);
    minimum_final_dot = std::min(minimum_final_dot, final_dot);
    if (!std::isfinite(final_dot) || final_dot <= 0.0) {
      invalid_hemisphere = true;
    }
  }
  return candidate;
}

struct RoadSurfaceNormals {
  std::vector<RoadVec3> left;
  std::vector<RoadVec3> right;
  std::vector<RoadVec3> first_face;
  std::vector<RoadVec3> second_face;
  bool invalid_hemisphere = false;
  double minimum_final_dot = std::numeric_limits<double>::infinity();
};

RoadSurfaceNormals calculate_road_surface_normals_native(
    const std::vector<RoadVec3>& left_vertices,
    const std::vector<RoadVec3>& right_vertices,
    bool closed,
    double outward_sign) {
  const int point_count = static_cast<int>(left_vertices.size());
  const int segment_count = closed ? point_count : point_count - 1;
  RoadSurfaceNormals result;
  result.left.resize(point_count);
  result.right.resize(point_count);
  result.first_face.resize(segment_count);
  result.second_face.resize(segment_count);
  for (int segment = 0; segment < segment_count; ++segment) {
    if ((segment & 255) == 0 && RcppThread::isInterrupted()) {
      throw RcppThread::UserInterruptException();
    }
    const int next = closed ? (segment + 1) % point_count : segment + 1;
    result.first_face[segment] = road_vec3_scale(
        road_vec3_cross(
            road_vec3_subtract(left_vertices[next], left_vertices[segment]),
            road_vec3_subtract(right_vertices[next], left_vertices[segment])),
        outward_sign);
    result.second_face[segment] = road_vec3_scale(
        road_vec3_cross(
            road_vec3_subtract(right_vertices[next], left_vertices[segment]),
            road_vec3_subtract(right_vertices[segment], left_vertices[segment])),
        outward_sign);
  }

  std::vector<std::array<RoadVec3, 3>> left_faces(point_count);
  std::vector<std::array<RoadVec3, 3>> right_faces(point_count);
  std::vector<int> left_face_count(point_count, 0);
  std::vector<int> right_face_count(point_count, 0);
  const auto append_face = [](
                               std::vector<std::array<RoadVec3, 3>>& faces,
                               std::vector<int>& counts,
                               int point,
                               const RoadVec3& face) {
    faces[point][counts[point]++] = face;
  };
  for (int segment = 0; segment < segment_count; ++segment) {
    const int next = closed ? (segment + 1) % point_count : segment + 1;
    append_face(
        left_faces, left_face_count, segment, result.first_face[segment]);
    append_face(
        left_faces, left_face_count, segment, result.second_face[segment]);
    append_face(left_faces, left_face_count, next, result.first_face[segment]);
    append_face(right_faces, right_face_count, next, result.first_face[segment]);
    append_face(
        right_faces, right_face_count, next, result.second_face[segment]);
    append_face(
        right_faces, right_face_count, segment, result.second_face[segment]);
  }
  const RoadVec3 fallback = {0.0, outward_sign > 0.0 ? 1.0 : -1.0, 0.0};
  std::vector<unsigned char> invalid(point_count * 2, 0);
  std::vector<double> minimum_dot(point_count * 2,
                                  std::numeric_limits<double>::infinity());
  for (int index = 0; index < point_count * 2; ++index) {
    if ((index & 255) == 0 && RcppThread::isInterrupted()) {
      throw RcppThread::UserInterruptException();
    }
    const bool is_right = index >= point_count;
    const int point = is_right ? index - point_count : index;
    bool invalid_point = false;
    double point_minimum_dot = std::numeric_limits<double>::infinity();
    RoadVec3 normal = resolve_road_shared_surface_normal_cpp(
        is_right ? right_faces[point] : left_faces[point],
        is_right ? right_face_count[point] : left_face_count[point],
        fallback,
        invalid_point,
        point_minimum_dot);
    if (is_right) {
      result.right[point] = normal;
    } else {
      result.left[point] = normal;
    }
    invalid[index] = invalid_point ? 1 : 0;
    minimum_dot[index] = point_minimum_dot;
  }
  for (int index = 0; index < point_count * 2; ++index) {
    if (invalid[index]) {
      result.invalid_hemisphere = true;
      result.minimum_final_dot = std::min(
          result.minimum_final_dot,
          minimum_dot[index]);
    }
  }
  return result;
}

inline void set_road_array_vec3(
    std::vector<double>& values,
    int row,
    const RoadVec3& value) {
  const std::size_t offset = static_cast<std::size_t>(row) * 3;
  values[offset] = value.x;
  values[offset + 1] = value.y;
  values[offset + 2] = value.z;
}

inline RoadVec3 get_road_array_vec3(
    const std::vector<double>& values,
    int row) {
  const std::size_t offset = static_cast<std::size_t>(row) * 3;
  return {values[offset], values[offset + 1], values[offset + 2]};
}

inline void set_road_array_vec2(
    std::vector<double>& values,
    int row,
    double first,
    double second) {
  const std::size_t offset = static_cast<std::size_t>(row) * 2;
  values[offset] = first;
  values[offset + 1] = second;
}

inline std::array<double, 2> get_road_array_vec2(
    const std::vector<double>& values,
    int row) {
  const std::size_t offset = static_cast<std::size_t>(row) * 2;
  return {values[offset], values[offset + 1]};
}

inline void fill_road_quad_vec3(
    std::vector<double>& values,
    int quad,
    const RoadVec3& first,
    const RoadVec3& second,
    const RoadVec3& third,
    const RoadVec3& fourth) {
  const int start = quad * 4;
  set_road_array_vec3(values, start, first);
  set_road_array_vec3(values, start + 1, second);
  set_road_array_vec3(values, start + 2, third);
  set_road_array_vec3(values, start + 3, fourth);
}

inline void fill_road_quad_vec2(
    std::vector<double>& values,
    int quad,
    double first_u,
    double first_v,
    double second_u,
    double second_v,
    double third_u,
    double third_v,
    double fourth_u,
    double fourth_v) {
  const int start = quad * 4;
  set_road_array_vec2(values, start, first_u, first_v);
  set_road_array_vec2(values, start + 1, second_u, second_v);
  set_road_array_vec2(values, start + 2, third_u, third_v);
  set_road_array_vec2(values, start + 3, fourth_u, fourth_v);
}

}  // namespace

// [[Rcpp::export]]
List calculate_render_road_vertex_frames_cpp(
    const NumericMatrix& points,
    bool closed,
    double miter_limit) {
  const int point_count = points.nrow();
  const int segment_count = closed ? point_count : point_count - 1;
  NumericMatrix segment_tangent(segment_count, 2);
  NumericVector segment_length(segment_count);

  for (int segment = 0; segment < segment_count; ++segment) {
    const int next = closed ? (segment + 1) % point_count : segment + 1;
    const double delta_x = points(next, 0) - points(segment, 0);
    const double delta_z = points(next, 2) - points(segment, 2);
    const double length = std::sqrt(
        delta_x * delta_x + delta_z * delta_z);
    if (!std::isfinite(length) || length <= 0.0) {
      stop("Road vertex frames contain a zero-length segment.");
    }
    segment_length[segment] = length;
    segment_tangent(segment, 0) = delta_x / length;
    segment_tangent(segment, 1) = delta_z / length;
  }

  NumericMatrix incoming_tangent(point_count, 2);
  NumericMatrix outgoing_tangent(point_count, 2);
  NumericMatrix side(point_count, 2);
  NumericVector miter_scale(point_count);
  NumericVector turn_cross(point_count);
  NumericVector turn_dot(point_count);
  CharacterVector join_style(point_count);
  const double stability_tolerance =
      std::sqrt(std::numeric_limits<double>::epsilon());

  for (int point = 0; point < point_count; ++point) {
    const int incoming_segment = closed
        ? (point + segment_count - 1) % segment_count
        : std::max(point - 1, 0);
    const int outgoing_segment = closed
        ? point
        : std::min(point, segment_count - 1);
    incoming_tangent(point, 0) = segment_tangent(incoming_segment, 0);
    incoming_tangent(point, 1) = segment_tangent(incoming_segment, 1);
    outgoing_tangent(point, 0) = segment_tangent(outgoing_segment, 0);
    outgoing_tangent(point, 1) = segment_tangent(outgoing_segment, 1);

    const bool endpoint = !closed &&
        (point == 0 || point == point_count - 1);
    if (endpoint) {
      const double tangent_x = point == 0
          ? outgoing_tangent(point, 0)
          : incoming_tangent(point, 0);
      const double tangent_z = point == 0
          ? outgoing_tangent(point, 1)
          : incoming_tangent(point, 1);
      side(point, 0) = -tangent_z;
      side(point, 1) = tangent_x;
      miter_scale[point] = 1.0;
      turn_cross[point] = 0.0;
      turn_dot[point] = 1.0;
      join_style[point] = "endpoint";
      continue;
    }

    double incoming_x;
    double incoming_z;
    double outgoing_x;
    double outgoing_z;
    const bool finite_incoming = normalize_road_vector(
        incoming_tangent(point, 0),
        incoming_tangent(point, 1),
        incoming_x,
        incoming_z);
    const bool finite_outgoing = normalize_road_vector(
        outgoing_tangent(point, 0),
        outgoing_tangent(point, 1),
        outgoing_x,
        outgoing_z);
    const double incoming_side_x = -incoming_z;
    const double incoming_side_z = incoming_x;
    const double outgoing_side_x = -outgoing_z;
    const double outgoing_side_z = outgoing_x;
    double miter_side_x;
    double miter_side_z;
    const bool finite_miter = normalize_road_vector(
        incoming_side_x + outgoing_side_x,
        incoming_side_z + outgoing_side_z,
        miter_side_x,
        miter_side_z);
    const double denominator =
        miter_side_x * outgoing_side_x +
        miter_side_z * outgoing_side_z;
    const double scale = 1.0 / denominator;
    const bool stable = finite_incoming &&
        finite_outgoing &&
        finite_miter &&
        std::isfinite(denominator) &&
        std::isfinite(scale) &&
        denominator > stability_tolerance &&
        scale <= miter_limit;

    side(point, 0) = stable ? miter_side_x : outgoing_side_x;
    side(point, 1) = stable ? miter_side_z : outgoing_side_z;
    miter_scale[point] = stable ? scale : NA_REAL;
    turn_cross[point] =
        incoming_x * outgoing_z - incoming_z * outgoing_x;
    turn_dot[point] =
        incoming_x * outgoing_x + incoming_z * outgoing_z;
    join_style[point] = stable ? "miter" : "round";
  }

  colnames(side) = CharacterVector::create("side_x", "side_z");
  return List::create(
      _["incoming_tangent"] = incoming_tangent,
      _["outgoing_tangent"] = outgoing_tangent,
      _["side"] = side,
      _["miter_scale"] = miter_scale,
      _["join_style"] = join_style,
      _["turn_cross"] = turn_cross,
      _["turn_dot"] = turn_dot,
      _["segment_length"] = segment_length);
}

// [[Rcpp::export]]
NumericMatrix densify_render_highquality_path_xz_cpp(
    const NumericMatrix& points,
    const NumericMatrix& left_edge,
    const NumericMatrix& right_edge,
    const NumericVector& center_offset,
    int row_count,
    int column_count) {
  const int point_count = points.nrow();
  if (point_count < 2 ||
      points.ncol() < 3 ||
      left_edge.nrow() != point_count ||
      right_edge.nrow() != point_count ||
      left_edge.ncol() < 3 ||
      right_edge.ncol() < 3 ||
      center_offset.size() != point_count) {
    stop("Road path densification inputs do not match.");
  }
  const int segment_count = point_count - 1;
  std::vector<std::vector<double>> segment_t_values(segment_count);
  std::size_t output_count = 0;
  for (int segment = 0; segment < segment_count; ++segment) {
    std::vector<double> combined;
    const std::vector<double> center_t =
        calculate_render_line_triangle_boundary_t_cpp(
            points(segment, 0),
            points(segment, 2),
            points(segment + 1, 0),
            points(segment + 1, 2),
            row_count,
            column_count);
    const std::vector<double> left_t =
        calculate_render_line_triangle_boundary_t_cpp(
            left_edge(segment, 0),
            left_edge(segment, 2),
            left_edge(segment + 1, 0),
            left_edge(segment + 1, 2),
            row_count,
            column_count);
    const std::vector<double> right_t =
        calculate_render_line_triangle_boundary_t_cpp(
            right_edge(segment, 0),
            right_edge(segment, 2),
            right_edge(segment + 1, 0),
            right_edge(segment + 1, 2),
            row_count,
            column_count);
    combined.reserve(center_t.size() + left_t.size() + right_t.size());
    combined.insert(combined.end(), center_t.begin(), center_t.end());
    combined.insert(combined.end(), left_t.begin(), left_t.end());
    combined.insert(combined.end(), right_t.begin(), right_t.end());
    segment_t_values[segment] = unique_render_line_t_cpp(combined);
    if (segment > 0 && !segment_t_values[segment].empty()) {
      segment_t_values[segment].erase(segment_t_values[segment].begin());
    }
    output_count += segment_t_values[segment].size();
  }

  NumericMatrix result(output_count, 3);
  std::size_t output_index = 0;
  for (int segment = 0; segment < segment_count; ++segment) {
    const double delta_x = points(segment + 1, 0) - points(segment, 0);
    const double delta_z = points(segment + 1, 2) - points(segment, 2);
    const double delta_offset =
        center_offset[segment + 1] - center_offset[segment];
    for (double segment_t : segment_t_values[segment]) {
      result(output_index, 0) =
          points(segment, 0) + delta_x * segment_t;
      result(output_index, 1) =
          points(segment, 2) + delta_z * segment_t;
      result(output_index, 2) =
          center_offset[segment] + delta_offset * segment_t;
      ++output_index;
    }
  }
  return result;
}

namespace {

class RoadMeshGeometryError : public std::runtime_error {
 public:
  explicit RoadMeshGeometryError(const std::string& message)
      : std::runtime_error(message) {}
};

struct RoadMeshSectionInput {
  std::vector<int> indices;
  std::vector<double> texture_v;
  double closing_v = 0.0;
  bool cap_start = false;
  bool cap_end = false;
  bool closed = false;
};

struct RoadMeshJob {
  std::vector<RoadVec3> left_bottom;
  std::vector<RoadVec3> right_bottom;
  std::vector<RoadVec3> left_top;
  std::vector<RoadVec3> right_top;
  std::vector<std::array<double, 2>> incoming;
  std::vector<std::array<double, 2>> outgoing;
  RoadVec3 center;
  bool closed = false;
  std::vector<RoadMeshSectionInput> mesh_sections;
};

struct RoadMeshData {
  std::vector<double> vertices;
  std::vector<double> vertex_normals;
  std::vector<double> texcoords;
  std::vector<int> indices;
  int input_quad_count = 0;
  int retained_quad_count = 0;
  int non_finite_quad_count = 0;
  double minimum_triangle_area = std::numeric_limits<double>::quiet_NaN();
  double minimum_uv_triangle_area = std::numeric_limits<double>::quiet_NaN();
};

struct RoadMeshJobResult {
  bool success = true;
  std::string error;
  std::vector<RoadMeshData> meshes;
};

struct RoadTerrain {
  int row_count = 0;
  int column_count = 0;
  double height_scale = 1.0;
  std::vector<double> height;

  bool available() const {
    return row_count >= 2 && column_count >= 2 && !height.empty();
  }

  double value(int row, int column) const {
    return height[static_cast<std::size_t>(row) +
                  static_cast<std::size_t>(row_count) * column] *
        height_scale;
  }

  double sample(double x, double z) const {
    if (!available()) {
      return std::numeric_limits<double>::quiet_NaN();
    }
    double row = x + (row_count - 1.0) / 2.0;
    double column = z + (column_count - 1.0) / 2.0;
    row = std::min(std::max(row, 0.0), row_count - 1.0);
    column = std::min(std::max(column, 0.0), column_count - 1.0);
    const int row0 = std::min(
        std::max(static_cast<int>(std::floor(row)), 0), row_count - 2);
    const int row1 = row0 + 1;
    const int column0 = std::min(
        std::max(static_cast<int>(std::floor(column)), 0), column_count - 2);
    const int column1 = column0 + 1;
    const double row_weight = row - row0;
    const double column_weight = column - column0;
    const double height00 = value(row0, column0);
    const double height10 = value(row1, column0);
    const double height01 = value(row0, column1);
    const double height11 = value(row1, column1);
    double result;
    if (row_weight + column_weight <= 1.0) {
      result = height00 + row_weight * (height10 - height00) +
          column_weight * (height01 - height00);
    } else {
      result = height11 +
          (1.0 - column_weight) * (height10 - height11) +
          (1.0 - row_weight) * (height01 - height11);
    }
    if (std::isfinite(result)) {
      return result;
    }
    const int nearest_row = std::min(
        std::max(static_cast<int>(std::nearbyint(row)), 0), row_count - 1);
    const int nearest_column = std::min(
        std::max(static_cast<int>(std::nearbyint(column)), 0),
        column_count - 1);
    return value(nearest_row, nearest_column);
  }

  RoadVec3 normal(double x, double z) const {
    const double dx = (sample(x + 1.0, z) - sample(x - 1.0, z)) / 2.0;
    const double dz = (sample(x, z + 1.0) - sample(x, z - 1.0)) / 2.0;
    RoadVec3 result;
    if (!normalize_road_vec3({-dx, 1.0, -dz}, result)) {
      return {0.0, 1.0, 0.0};
    }
    return result;
  }
};

struct RoadDensifyJob {
  std::vector<RoadVec3> points;
  double width = 0.0;
  bool terrain_following = false;
};

struct RoadSectionJob {
  std::vector<RoadVec3> points;
  std::vector<double> left_distance;
  std::vector<double> right_distance;
  std::vector<std::array<double, 2>> side;
  std::vector<double> miter_scale;
  bool terrain_following = false;
};

struct RoadSectionData {
  std::vector<RoadVec3> left_bottom;
  std::vector<RoadVec3> right_bottom;
  std::vector<RoadVec3> left_top;
  std::vector<RoadVec3> right_top;
  std::vector<RoadVec3> left_normal;
  std::vector<RoadVec3> right_normal;
};

void check_road_mesh_interrupt(int index) {
  if ((index & 255) == 0 && RcppThread::isInterrupted()) {
    throw RcppThread::UserInterruptException();
  }
}

RoadTerrain copy_road_terrain(
    const NumericMatrix& heightmap,
    double zscale) {
  RoadTerrain terrain;
  terrain.row_count = heightmap.nrow();
  terrain.column_count = heightmap.ncol();
  terrain.height_scale = std::isfinite(zscale) && zscale > 0.0
      ? 1.0 / zscale
      : 1.0;
  terrain.height.assign(heightmap.begin(), heightmap.end());
  return terrain;
}

std::vector<RoadVec3> densify_road_path_native(
    const RoadDensifyJob& job,
    const RoadTerrain& terrain) {
  if (!job.terrain_following || !terrain.available() ||
      job.points.size() < 2) {
    return job.points;
  }
  const int point_count = static_cast<int>(job.points.size());
  std::vector<double> center_height(point_count);
  std::vector<double> center_offset(point_count);
  std::vector<RoadVec3> normal(point_count);
  for (int point = 0; point < point_count; ++point) {
    check_road_mesh_interrupt(point);
    center_height[point] = terrain.sample(
        job.points[point].x, job.points[point].z);
    center_offset[point] = job.points[point].y - center_height[point];
    normal[point] = terrain.normal(job.points[point].x, job.points[point].z);
  }

  std::vector<RoadVec3> tangent(point_count);
  for (int point = 0; point < point_count; ++point) {
    RoadVec3 raw;
    if (point == 0) {
      raw = road_vec3_subtract(job.points[1], job.points[0]);
    } else if (point == point_count - 1) {
      raw = road_vec3_subtract(
          job.points[point_count - 1], job.points[point_count - 2]);
    } else {
      raw = road_vec3_subtract(job.points[point + 1], job.points[point - 1]);
    }
    raw = road_vec3_subtract(
        raw, road_vec3_scale(normal[point], road_vec3_dot(raw, normal[point])));
    if (!normalize_road_vec3(raw, tangent[point])) {
      tangent[point] = {1.0, 0.0, 0.0};
    }
  }

  std::vector<RoadVec3> left_edge(point_count);
  std::vector<RoadVec3> right_edge(point_count);
  const double half_width = job.width / 2.0;
  for (int point = 0; point < point_count; ++point) {
    RoadVec3 side;
    if (!normalize_road_vec3(
            road_vec3_cross(tangent[point], normal[point]), side)) {
      side = {0.0, 0.0, 1.0};
    }
    left_edge[point] = road_vec3_add(
        job.points[point], road_vec3_scale(side, half_width));
    right_edge[point] = road_vec3_subtract(
        job.points[point], road_vec3_scale(side, half_width));
  }

  const int segment_count = point_count - 1;
  std::vector<std::vector<double>> segment_t_values(segment_count);
  std::size_t output_count = 0;
  for (int segment = 0; segment < segment_count; ++segment) {
    check_road_mesh_interrupt(segment);
    std::vector<double> combined;
    const std::vector<double> center_t =
        calculate_render_line_triangle_boundary_t_cpp(
            job.points[segment].x,
            job.points[segment].z,
            job.points[segment + 1].x,
            job.points[segment + 1].z,
            terrain.row_count,
            terrain.column_count);
    const std::vector<double> left_t =
        calculate_render_line_triangle_boundary_t_cpp(
            left_edge[segment].x,
            left_edge[segment].z,
            left_edge[segment + 1].x,
            left_edge[segment + 1].z,
            terrain.row_count,
            terrain.column_count);
    const std::vector<double> right_t =
        calculate_render_line_triangle_boundary_t_cpp(
            right_edge[segment].x,
            right_edge[segment].z,
            right_edge[segment + 1].x,
            right_edge[segment + 1].z,
            terrain.row_count,
            terrain.column_count);
    combined.reserve(center_t.size() + left_t.size() + right_t.size());
    combined.insert(combined.end(), center_t.begin(), center_t.end());
    combined.insert(combined.end(), left_t.begin(), left_t.end());
    combined.insert(combined.end(), right_t.begin(), right_t.end());
    segment_t_values[segment] = unique_render_line_t_cpp(combined);
    if (segment > 0 && !segment_t_values[segment].empty()) {
      segment_t_values[segment].erase(segment_t_values[segment].begin());
    }
    output_count += segment_t_values[segment].size();
  }

  std::vector<RoadVec3> output;
  output.reserve(output_count);
  for (int segment = 0; segment < segment_count; ++segment) {
    const RoadVec3 delta = road_vec3_subtract(
        job.points[segment + 1], job.points[segment]);
    const double offset_delta =
        center_offset[segment + 1] - center_offset[segment];
    for (double segment_t : segment_t_values[segment]) {
      const double x = job.points[segment].x + delta.x * segment_t;
      const double z = job.points[segment].z + delta.z * segment_t;
      const double offset =
          center_offset[segment] + offset_delta * segment_t;
      output.push_back({x, terrain.sample(x, z) + offset, z});
    }
  }
  return output;
}

RoadSectionData sample_road_sections_native(
    const RoadSectionJob& job,
    const RoadTerrain& terrain) {
  const int point_count = static_cast<int>(job.points.size());
  RoadSectionData result;
  result.left_bottom.resize(point_count);
  result.right_bottom.resize(point_count);
  result.left_top.resize(point_count);
  result.right_top.resize(point_count);
  result.left_normal.resize(point_count);
  result.right_normal.resize(point_count);
  const bool sample_terrain = job.terrain_following && terrain.available();
  constexpr double road_height = 0.11;
  constexpr double road_surface_clearance = 0.055;
  for (int point = 0; point < point_count; ++point) {
    check_road_mesh_interrupt(point);
    const RoadVec3 side = {
        job.side[point][0], 0.0, job.side[point][1]};
    RoadVec3 left_surface = road_vec3_add(
        job.points[point],
        road_vec3_scale(
            side, job.left_distance[point] * job.miter_scale[point]));
    RoadVec3 right_surface = road_vec3_subtract(
        job.points[point],
        road_vec3_scale(
            side, job.right_distance[point] * job.miter_scale[point]));
    if (sample_terrain) {
      const double center_height = terrain.sample(
          job.points[point].x, job.points[point].z);
      const double center_offset = job.points[point].y - center_height;
      left_surface.y =
          terrain.sample(left_surface.x, left_surface.z) + center_offset;
      right_surface.y =
          terrain.sample(right_surface.x, right_surface.z) + center_offset;
      result.left_normal[point] =
          terrain.normal(left_surface.x, left_surface.z);
      result.right_normal[point] =
          terrain.normal(right_surface.x, right_surface.z);
    } else {
      result.left_normal[point] = {0.0, 1.0, 0.0};
      result.right_normal[point] = {0.0, 1.0, 0.0};
    }
    result.left_top[point] = road_vec3_add(
        left_surface,
        road_vec3_scale(
            result.left_normal[point], road_surface_clearance));
    result.right_top[point] = road_vec3_add(
        right_surface,
        road_vec3_scale(
            result.right_normal[point], road_surface_clearance));
    result.left_bottom[point] = road_vec3_subtract(
        result.left_top[point],
        road_vec3_scale(result.left_normal[point], road_height));
    result.right_bottom[point] = road_vec3_subtract(
        result.right_top[point],
        road_vec3_scale(result.right_normal[point], road_height));
  }
  return result;
}

std::vector<std::array<double, 2>> copy_road_vec2_matrix(
    const NumericMatrix& values) {
  std::vector<std::array<double, 2>> result(values.nrow());
  for (int row = 0; row < values.nrow(); ++row) {
    result[row] = {values(row, 0), values(row, 1)};
  }
  return result;
}

std::vector<double> copy_road_numeric_vector(const NumericVector& values) {
  return std::vector<double>(values.begin(), values.end());
}

template <typename Value>
std::vector<Value> subset_road_values(
    const std::vector<Value>& values,
    const std::vector<int>& indices) {
  std::vector<Value> result;
  result.reserve(indices.size());
  for (int index : indices) {
    result.push_back(values[index]);
  }
  return result;
}

RoadMeshData build_road_section_mesh_native(
    const std::vector<RoadVec3>& left_bottom,
    const std::vector<RoadVec3>& right_bottom,
    const std::vector<RoadVec3>& left_top,
    const std::vector<RoadVec3>& right_top,
    const std::vector<std::array<double, 2>>& incoming,
    const std::vector<std::array<double, 2>>& outgoing,
    const std::vector<double>& texture_values,
    double closing_v,
    const RoadVec3& center,
    const std::vector<RoadVec3>& top_left_normal,
    const std::vector<RoadVec3>& top_right_normal,
    const std::vector<RoadVec3>& bottom_left_normal,
    const std::vector<RoadVec3>& bottom_right_normal,
    bool cap_start,
    bool cap_end,
    bool closed,
    double geometry_tolerance,
    double uv_tolerance) {
  const int point_count = static_cast<int>(left_top.size());
  const int segment_count = closed ? point_count : point_count - 1;
  const int start_cap_count = !closed && cap_start ? 1 : 0;
  const int end_cap_count = !closed && cap_end ? 1 : 0;
  const int input_quad_count =
      segment_count * 4 + start_cap_count + end_cap_count;
  const int input_vertex_count = input_quad_count * 4;
  std::vector<double> vertices(
      static_cast<std::size_t>(input_vertex_count) * 3);
  std::vector<double> vertex_normals(
      static_cast<std::size_t>(input_vertex_count) * 3);
  std::vector<double> texcoords(
      static_cast<std::size_t>(input_vertex_count) * 2);
  const double side_u0 = 0.01;
  const double side_u1 = 0.02;

  for (int segment = 0; segment < segment_count; ++segment) {
    check_road_mesh_interrupt(segment);
    const int next = closed ? (segment + 1) % point_count : segment + 1;
    const double v0 = texture_values[segment];
    const double v1 = closed && segment == segment_count - 1
        ? closing_v
        : texture_values[next];
    const int top_quad = segment;
    const int bottom_quad = segment_count + segment;
    const int left_quad = segment_count * 2 + segment;
    const int right_quad = segment_count * 3 + segment;

    fill_road_quad_vec3(
        vertices,
        top_quad,
        left_top[segment],
        left_top[next],
        right_top[next],
        right_top[segment]);
    fill_road_quad_vec3(
        vertices,
        bottom_quad,
        left_bottom[segment],
        right_bottom[segment],
        right_bottom[next],
        left_bottom[next]);
    fill_road_quad_vec3(
        vertices,
        left_quad,
        left_bottom[segment],
        left_bottom[next],
        left_top[next],
        left_top[segment]);
    fill_road_quad_vec3(
        vertices,
        right_quad,
        right_bottom[segment],
        right_top[segment],
        right_top[next],
        right_bottom[next]);

    fill_road_quad_vec3(
        vertex_normals,
        top_quad,
        top_left_normal[segment],
        top_left_normal[next],
        top_right_normal[next],
        top_right_normal[segment]);
    fill_road_quad_vec3(
        vertex_normals,
        bottom_quad,
        bottom_left_normal[segment],
        bottom_right_normal[segment],
        bottom_right_normal[next],
        bottom_left_normal[next]);
    RoadVec3 left_wall_normal;
    if (!normalize_road_vec3(
            road_vec3_cross(
                road_vec3_subtract(left_bottom[next], left_bottom[segment]),
                road_vec3_subtract(left_top[segment], left_bottom[segment])),
            left_wall_normal)) {
      left_wall_normal = {0.0, 0.0, 1.0};
    }
    RoadVec3 right_wall_normal;
    if (!normalize_road_vec3(
            road_vec3_cross(
                road_vec3_subtract(right_top[segment], right_bottom[segment]),
                road_vec3_subtract(right_bottom[next], right_bottom[segment])),
            right_wall_normal)) {
      right_wall_normal = {0.0, 0.0, -1.0};
    }
    fill_road_quad_vec3(
        vertex_normals,
        left_quad,
        left_wall_normal,
        left_wall_normal,
        left_wall_normal,
        left_wall_normal);
    fill_road_quad_vec3(
        vertex_normals,
        right_quad,
        right_wall_normal,
        right_wall_normal,
        right_wall_normal,
        right_wall_normal);

    fill_road_quad_vec2(
        texcoords, top_quad, 0.0, v0, 0.0, v1, 1.0, v1, 1.0, v0);
    fill_road_quad_vec2(
        texcoords, bottom_quad, 0.0, v0, 1.0, v0, 1.0, v1, 0.0, v1);
    fill_road_quad_vec2(
        texcoords,
        left_quad,
        side_u0,
        v0,
        side_u0,
        v1,
        side_u1,
        v1,
        side_u1,
        v0);
    fill_road_quad_vec2(
        texcoords,
        right_quad,
        side_u0,
        v0,
        side_u1,
        v0,
        side_u1,
        v1,
        side_u0,
        v1);
  }

  const double cap_v_span = 1e-4;
  int cap_quad = segment_count * 4;
  if (!closed && cap_start) {
    const RoadVec3 cap_normal = {
        -outgoing[0][0], 0.0, -outgoing[0][1]};
    fill_road_quad_vec3(
        vertices,
        cap_quad,
        left_bottom[0],
        left_top[0],
        right_top[0],
        right_bottom[0]);
    fill_road_quad_vec3(
        vertex_normals,
        cap_quad,
        cap_normal,
        cap_normal,
        cap_normal,
        cap_normal);
    const double start_v = texture_values[0];
    fill_road_quad_vec2(
        texcoords,
        cap_quad,
        side_u0,
        start_v,
        side_u0,
        start_v + cap_v_span,
        side_u1,
        start_v + cap_v_span,
        side_u1,
        start_v);
    ++cap_quad;
  }
  if (!closed && cap_end) {
    const int last = point_count - 1;
    const RoadVec3 cap_normal = {
        incoming[last][0], 0.0, incoming[last][1]};
    fill_road_quad_vec3(
        vertices,
        cap_quad,
        left_bottom[last],
        right_bottom[last],
        right_top[last],
        left_top[last]);
    fill_road_quad_vec3(
        vertex_normals,
        cap_quad,
        cap_normal,
        cap_normal,
        cap_normal,
        cap_normal);
    const double end_v = texture_values[last];
    fill_road_quad_vec2(
        texcoords,
        cap_quad,
        side_u0,
        end_v,
        side_u1,
        end_v,
        side_u1,
        end_v - cap_v_span,
        side_u0,
        end_v - cap_v_span);
  }

  for (int quad = 0; quad < input_quad_count; ++quad) {
    check_road_mesh_interrupt(quad);
    const int start = quad * 4;
    const RoadVec3 first_edge = road_vec3_subtract(
        get_road_array_vec3(vertices, start + 1),
        get_road_array_vec3(vertices, start));
    const RoadVec3 second_edge = road_vec3_subtract(
        get_road_array_vec3(vertices, start + 2),
        get_road_array_vec3(vertices, start));
    const RoadVec3 third_edge = road_vec3_subtract(
        get_road_array_vec3(vertices, start + 3),
        get_road_array_vec3(vertices, start));
    RoadVec3 first_face;
    RoadVec3 second_face;
    const bool first_valid = normalize_road_vec3(
        road_vec3_cross(first_edge, second_edge), first_face);
    const bool second_valid = normalize_road_vec3(
        road_vec3_cross(second_edge, third_edge), second_face);
    RoadVec3 fallback_normal;
    if (!normalize_road_vec3(
            road_vec3_add(first_face, second_face), fallback_normal)) {
      fallback_normal = {0.0, 1.0, 0.0};
    }
    bool invalid = !first_valid || !second_valid;
    const int first_rows[3] = {start, start + 1, start + 2};
    const int second_rows[3] = {start, start + 2, start + 3};
    for (int corner = 0; corner < 3; ++corner) {
      invalid = invalid ||
          road_vec3_dot(
              get_road_array_vec3(vertex_normals, first_rows[corner]),
              first_face) <= 0.0 ||
          road_vec3_dot(
              get_road_array_vec3(vertex_normals, second_rows[corner]),
              second_face) <= 0.0;
    }
    if (invalid) {
      for (int corner = 0; corner < 4; ++corner) {
        set_road_array_vec3(vertex_normals, start + corner, fallback_normal);
      }
    }
  }

  std::vector<unsigned char> finite_quad(input_quad_count, 0);
  std::vector<unsigned char> keep_quad(input_quad_count, 0);
  std::vector<double> first_area(input_quad_count);
  std::vector<double> second_area(input_quad_count);
  std::vector<double> first_uv_area(input_quad_count);
  std::vector<double> second_uv_area(input_quad_count);
  for (int quad = 0; quad < input_quad_count; ++quad) {
    check_road_mesh_interrupt(quad);
    const int start = quad * 4;
    const RoadVec3 first_cross = road_vec3_cross(
        road_vec3_subtract(
            get_road_array_vec3(vertices, start + 1),
            get_road_array_vec3(vertices, start)),
        road_vec3_subtract(
            get_road_array_vec3(vertices, start + 2),
            get_road_array_vec3(vertices, start)));
    const RoadVec3 second_cross = road_vec3_cross(
        road_vec3_subtract(
            get_road_array_vec3(vertices, start + 2),
            get_road_array_vec3(vertices, start)),
        road_vec3_subtract(
            get_road_array_vec3(vertices, start + 3),
            get_road_array_vec3(vertices, start)));
    first_area[quad] = road_vec3_length(first_cross) / 2.0;
    second_area[quad] = road_vec3_length(second_cross) / 2.0;
    const std::array<double, 2> uv0 = get_road_array_vec2(texcoords, start);
    const std::array<double, 2> uv1 = get_road_array_vec2(texcoords, start + 1);
    const std::array<double, 2> uv2 = get_road_array_vec2(texcoords, start + 2);
    const std::array<double, 2> uv3 = get_road_array_vec2(texcoords, start + 3);
    first_uv_area[quad] = std::abs(
        (uv1[0] - uv0[0]) * (uv2[1] - uv0[1]) -
        (uv1[1] - uv0[1]) * (uv2[0] - uv0[0])) / 2.0;
    second_uv_area[quad] = std::abs(
        (uv2[0] - uv0[0]) * (uv3[1] - uv0[1]) -
        (uv2[1] - uv0[1]) * (uv3[0] - uv0[0])) / 2.0;
    bool finite = true;
    for (int corner = 0; corner < 4; ++corner) {
      finite = finite &&
          road_vec3_finite(get_road_array_vec3(vertices, start + corner)) &&
          road_vec3_finite(
              get_road_array_vec3(vertex_normals, start + corner));
      const std::array<double, 2> uv =
          get_road_array_vec2(texcoords, start + corner);
      finite = finite && std::isfinite(uv[0]) && std::isfinite(uv[1]);
    }
    finite_quad[quad] = finite ? 1 : 0;
    keep_quad[quad] = finite &&
            first_area[quad] > geometry_tolerance &&
            second_area[quad] > geometry_tolerance &&
            first_uv_area[quad] > uv_tolerance &&
            second_uv_area[quad] > uv_tolerance
        ? 1
        : 0;
  }

  std::vector<int> kept_quads;
  kept_quads.reserve(input_quad_count);
  int non_finite_quad_count = 0;
  double minimum_triangle_area = std::numeric_limits<double>::infinity();
  double minimum_uv_triangle_area = std::numeric_limits<double>::infinity();
  for (int quad = 0; quad < input_quad_count; ++quad) {
    if (!finite_quad[quad]) {
      ++non_finite_quad_count;
    }
    if (!keep_quad[quad]) {
      continue;
    }
    kept_quads.push_back(quad);
    minimum_triangle_area = std::min(
        minimum_triangle_area,
        std::min(first_area[quad], second_area[quad]));
    minimum_uv_triangle_area = std::min(
        minimum_uv_triangle_area,
        std::min(first_uv_area[quad], second_uv_area[quad]));
  }

  RoadMeshData output;
  output.input_quad_count = input_quad_count;
  output.retained_quad_count = static_cast<int>(kept_quads.size());
  output.non_finite_quad_count = non_finite_quad_count;
  if (output.retained_quad_count) {
    output.minimum_triangle_area = minimum_triangle_area;
    output.minimum_uv_triangle_area = minimum_uv_triangle_area;
  }
  output.vertices.resize(
      static_cast<std::size_t>(output.retained_quad_count) * 12);
  output.vertex_normals.resize(
      static_cast<std::size_t>(output.retained_quad_count) * 12);
  output.texcoords.resize(
      static_cast<std::size_t>(output.retained_quad_count) * 8);
  output.indices.resize(
      static_cast<std::size_t>(output.retained_quad_count) * 6);
  for (int output_quad = 0;
       output_quad < output.retained_quad_count;
       ++output_quad) {
    const int input_quad = kept_quads[output_quad];
    for (int corner = 0; corner < 4; ++corner) {
      const int input_row = input_quad * 4 + corner;
      const int output_row = output_quad * 4 + corner;
      RoadVec3 vertex = get_road_array_vec3(vertices, input_row);
      vertex = road_vec3_subtract(vertex, center);
      set_road_array_vec3(output.vertices, output_row, vertex);
      set_road_array_vec3(
          output.vertex_normals,
          output_row,
          get_road_array_vec3(vertex_normals, input_row));
      const std::array<double, 2> uv =
          get_road_array_vec2(texcoords, input_row);
      set_road_array_vec2(output.texcoords, output_row, uv[0], uv[1]);
    }
    const int start = output_quad * 4 + 1;
    const int second_triangle = output.retained_quad_count * 3 +
        output_quad * 3;
    output.indices[output_quad * 3] = start;
    output.indices[output_quad * 3 + 1] = start + 1;
    output.indices[output_quad * 3 + 2] = start + 2;
    output.indices[second_triangle] = start;
    output.indices[second_triangle + 1] = start + 2;
    output.indices[second_triangle + 2] = start + 3;
  }
  return output;
}

std::string road_surface_normal_error(double minimum_final_dot) {
  char buffer[256];
  std::snprintf(
      buffer,
      sizeof(buffer),
      "A road surface vertex has no common outward shading hemisphere "
      "(minimum face dot %.6g).",
      minimum_final_dot);
  return std::string(buffer);
}

bool road_mesh_section_uses_complete_job(
    const RoadMeshSectionInput& section,
    std::size_t point_count,
    bool job_closed) {
  if (section.closed != job_closed || section.indices.size() != point_count) {
    return false;
  }
  for (std::size_t index = 0; index < point_count; ++index) {
    if (section.indices[index] != static_cast<int>(index)) {
      return false;
    }
  }
  return true;
}

RoadMeshJobResult run_road_mesh_job(const RoadMeshJob& job) {
  RoadMeshJobResult result;
  try {
    const RoadSurfaceNormals top = calculate_road_surface_normals_native(
        job.left_top,
        job.right_top,
        job.closed,
        1.0);
    if (top.invalid_hemisphere) {
      throw RoadMeshGeometryError(
          road_surface_normal_error(top.minimum_final_dot));
    }
    const RoadSurfaceNormals bottom = calculate_road_surface_normals_native(
        job.left_bottom,
        job.right_bottom,
        job.closed,
        -1.0);
    if (bottom.invalid_hemisphere) {
      throw RoadMeshGeometryError(
          road_surface_normal_error(bottom.minimum_final_dot));
    }
    result.meshes.reserve(job.mesh_sections.size());
    for (std::size_t section_index = 0;
         section_index < job.mesh_sections.size();
      ++section_index) {
      check_road_mesh_interrupt(static_cast<int>(section_index));
      const RoadMeshSectionInput& section = job.mesh_sections[section_index];
      if (road_mesh_section_uses_complete_job(
              section, job.left_top.size(), job.closed)) {
        result.meshes.push_back(build_road_section_mesh_native(
            job.left_bottom,
            job.right_bottom,
            job.left_top,
            job.right_top,
            job.incoming,
            job.outgoing,
            section.texture_v,
            section.closing_v,
            job.center,
            top.left,
            top.right,
            bottom.left,
            bottom.right,
            section.cap_start,
            section.cap_end,
            section.closed,
            1e-12,
            1e-14));
      } else {
        result.meshes.push_back(build_road_section_mesh_native(
            subset_road_values(job.left_bottom, section.indices),
            subset_road_values(job.right_bottom, section.indices),
            subset_road_values(job.left_top, section.indices),
            subset_road_values(job.right_top, section.indices),
            subset_road_values(job.incoming, section.indices),
            subset_road_values(job.outgoing, section.indices),
            section.texture_v,
            section.closing_v,
            job.center,
            subset_road_values(top.left, section.indices),
            subset_road_values(top.right, section.indices),
            subset_road_values(bottom.left, section.indices),
            subset_road_values(bottom.right, section.indices),
            section.cap_start,
            section.cap_end,
            section.closed,
            1e-12,
            1e-14));
      }
    }
  } catch (const RoadMeshGeometryError& error) {
    result.success = false;
    result.error = error.what();
    result.meshes.clear();
  }
  return result;
}

void validate_road_section_mesh_input(
    int point_count,
    const NumericMatrix& left_bottom_matrix,
    const NumericMatrix& right_bottom_matrix,
    const NumericMatrix& left_top_matrix,
    const NumericMatrix& right_top_matrix,
    const NumericMatrix& incoming_tangent,
    const NumericMatrix& outgoing_tangent,
    const NumericVector& texture_v,
    const NumericVector& bbox_center,
    const NumericMatrix& top_left_normal_matrix,
    const NumericMatrix& top_right_normal_matrix,
    const NumericMatrix& bottom_left_normal_matrix,
    const NumericMatrix& bottom_right_normal_matrix,
    bool closed,
    double closing_v,
    double geometry_tolerance,
    double uv_tolerance) {
  const auto valid_vec3_matrix = [&](const NumericMatrix& matrix) {
    return matrix.nrow() == point_count && matrix.ncol() == 3;
  };
  if (point_count < (closed ? 3 : 2) ||
      left_top_matrix.ncol() != 3 ||
      !valid_vec3_matrix(left_bottom_matrix) ||
      !valid_vec3_matrix(right_bottom_matrix) ||
      !valid_vec3_matrix(right_top_matrix) ||
      !valid_vec3_matrix(top_left_normal_matrix) ||
      !valid_vec3_matrix(top_right_normal_matrix) ||
      !valid_vec3_matrix(bottom_left_normal_matrix) ||
      !valid_vec3_matrix(bottom_right_normal_matrix) ||
      incoming_tangent.nrow() != point_count ||
      outgoing_tangent.nrow() != point_count ||
      incoming_tangent.ncol() < 2 ||
      outgoing_tangent.ncol() < 2 ||
      texture_v.size() != point_count ||
      bbox_center.size() < 3) {
    stop("Road section mesh inputs do not match.");
  }
  if (!std::isfinite(closing_v) ||
      !std::isfinite(geometry_tolerance) ||
      geometry_tolerance < 0.0 ||
      !std::isfinite(uv_tolerance) ||
      uv_tolerance < 0.0) {
    stop("Road section mesh tolerances and texture extent must be finite.");
  }
}

RoadMeshJob parse_road_mesh_job(const List& input) {
  const List sections = input["sections"];
  const NumericMatrix left_bottom = sections["left_bottom"];
  const NumericMatrix right_bottom = sections["right_bottom"];
  const NumericMatrix left_top = sections["left_top"];
  const NumericMatrix right_top = sections["right_top"];
  const List frames = sections["frames"];
  const NumericMatrix incoming = frames["incoming_tangent"];
  const NumericMatrix outgoing = frames["outgoing_tangent"];
  const NumericVector bbox_center = input["bbox_center"];
  const bool closed = as<bool>(input["closed"]);
  const int point_count = left_top.nrow();
  const auto valid_vec3_matrix = [&](const NumericMatrix& matrix) {
    return matrix.nrow() == point_count && matrix.ncol() == 3;
  };
  if (point_count < (closed ? 3 : 2) ||
      !valid_vec3_matrix(left_bottom) ||
      !valid_vec3_matrix(right_bottom) ||
      !valid_vec3_matrix(left_top) ||
      !valid_vec3_matrix(right_top) ||
      incoming.nrow() != point_count ||
      outgoing.nrow() != point_count ||
      incoming.ncol() < 2 ||
      outgoing.ncol() < 2 ||
      bbox_center.size() < 3) {
    stop("Road mesh batch job inputs do not match.");
  }

  RoadMeshJob job;
  job.left_bottom = copy_road_vec3_matrix(left_bottom);
  job.right_bottom = copy_road_vec3_matrix(right_bottom);
  job.left_top = copy_road_vec3_matrix(left_top);
  job.right_top = copy_road_vec3_matrix(right_top);
  job.incoming = copy_road_vec2_matrix(incoming);
  job.outgoing = copy_road_vec2_matrix(outgoing);
  job.center = {bbox_center[0], bbox_center[1], bbox_center[2]};
  job.closed = closed;

  const List mesh_sections = input["mesh_sections"];
  if (!mesh_sections.size()) {
    stop("Road mesh batch jobs require at least one material section.");
  }
  job.mesh_sections.reserve(mesh_sections.size());
  for (int section_number = 0;
       section_number < mesh_sections.size();
       ++section_number) {
    const List section_input = mesh_sections[section_number];
    const IntegerVector indices = section_input["section_index"];
    const NumericVector texture_v = section_input["texture_v"];
    RoadMeshSectionInput section;
    section.closed = as<bool>(section_input["closed"]);
    section.cap_start = as<bool>(section_input["cap_start"]);
    section.cap_end = as<bool>(section_input["cap_end"]);
    section.closing_v = as<double>(section_input["closing_v"]);
    if (indices.size() < (section.closed ? 3 : 2) ||
        texture_v.size() != indices.size() ||
        !std::isfinite(section.closing_v)) {
      stop("Road mesh batch material-section inputs do not match.");
    }
    section.indices.reserve(indices.size());
    for (int index : indices) {
      if (index == NA_INTEGER || index < 1 || index > point_count) {
        stop("Road mesh batch material-section indices are invalid.");
      }
      section.indices.push_back(index - 1);
    }
    section.texture_v = copy_road_numeric_vector(texture_v);
    job.mesh_sections.push_back(std::move(section));
  }
  return job;
}

RoadDensifyJob parse_road_densify_job(const List& input) {
  const NumericMatrix points = input["points"];
  const double width = as<double>(input["width"]);
  if (points.nrow() < 2 || points.ncol() != 3 ||
      !std::isfinite(width) || width <= 0.0) {
    stop("Road densification batch job inputs do not match.");
  }
  RoadDensifyJob job;
  job.points = copy_road_vec3_matrix(points);
  job.width = width;
  job.terrain_following = as<bool>(input["terrain_following"]);
  return job;
}

RoadSectionJob parse_road_section_job(const List& input) {
  const NumericMatrix points = input["points"];
  const NumericVector left_distance = input["left_distance"];
  const NumericVector right_distance = input["right_distance"];
  const NumericMatrix side = input["side"];
  const NumericVector miter_scale = input["miter_scale"];
  const int point_count = points.nrow();
  if (point_count < 2 || points.ncol() != 3 ||
      left_distance.size() != point_count ||
      right_distance.size() != point_count ||
      side.nrow() != point_count || side.ncol() < 2 ||
      miter_scale.size() != point_count) {
    stop("Road terrain-section batch job inputs do not match.");
  }
  RoadSectionJob job;
  job.points = copy_road_vec3_matrix(points);
  job.left_distance = copy_road_numeric_vector(left_distance);
  job.right_distance = copy_road_numeric_vector(right_distance);
  job.side = copy_road_vec2_matrix(side);
  job.miter_scale = copy_road_numeric_vector(miter_scale);
  job.terrain_following = as<bool>(input["terrain_following"]);
  return job;
}

NumericMatrix wrap_road_array_matrix(
    const std::vector<double>& values,
    int column_count) {
  const int row_count = static_cast<int>(values.size()) / column_count;
  NumericMatrix result(row_count, column_count);
  for (int row = 0; row < row_count; ++row) {
    for (int column = 0; column < column_count; ++column) {
      result(row, column) =
          values[static_cast<std::size_t>(row) * column_count + column];
    }
  }
  return result;
}

List wrap_road_mesh_data(const RoadMeshData& mesh) {
  IntegerMatrix indices(mesh.retained_quad_count * 2, 3);
  for (int row = 0; row < indices.nrow(); ++row) {
    for (int column = 0; column < 3; ++column) {
      indices(row, column) = mesh.indices[row * 3 + column];
    }
  }
  return List::create(
      _["vertices"] = wrap_road_array_matrix(mesh.vertices, 3),
      _["vertex_normals"] =
          wrap_road_array_matrix(mesh.vertex_normals, 3),
      _["texcoords"] = wrap_road_array_matrix(mesh.texcoords, 2),
      _["indices"] = indices,
      _["diagnostics"] = List::create(
          _["input_quad_count"] = mesh.input_quad_count,
          _["retained_quad_count"] = mesh.retained_quad_count,
          _["removed_quad_count"] =
              mesh.input_quad_count - mesh.retained_quad_count,
          _["non_finite_quad_count"] = mesh.non_finite_quad_count,
          _["minimum_triangle_area"] = mesh.minimum_triangle_area,
          _["minimum_uv_triangle_area"] =
              mesh.minimum_uv_triangle_area));
}

List wrap_road_section_data(const RoadSectionData& sections) {
  return List::create(
      _["left_bottom"] = wrap_road_vec3_matrix(sections.left_bottom),
      _["right_bottom"] = wrap_road_vec3_matrix(sections.right_bottom),
      _["left_top"] = wrap_road_vec3_matrix(sections.left_top),
      _["right_top"] = wrap_road_vec3_matrix(sections.right_top),
      _["left_normal"] = wrap_road_vec3_matrix(sections.left_normal),
      _["right_normal"] = wrap_road_vec3_matrix(sections.right_normal));
}

}  // namespace

// [[Rcpp::export]]
List calculate_render_road_surface_normals_cpp(
    const NumericMatrix& left_vertices,
    const NumericMatrix& right_vertices,
    bool closed,
    double outward_sign,
    bool parallel) {
  const int point_count = left_vertices.nrow();
  if (right_vertices.nrow() != point_count ||
      left_vertices.ncol() != 3 ||
      right_vertices.ncol() != 3 ||
      point_count < (closed ? 3 : 2)) {
    stop("Road surface boundaries do not define a valid strip.");
  }
  if (!std::isfinite(outward_sign) || outward_sign == 0.0) {
    stop("`outward_sign` must be finite and nonzero.");
  }
  (void)parallel;
  const std::vector<RoadVec3> left = copy_road_vec3_matrix(left_vertices);
  const std::vector<RoadVec3> right = copy_road_vec3_matrix(right_vertices);
  RoadSurfaceNormals normals = calculate_road_surface_normals_native(
      left,
      right,
      closed,
      outward_sign);
  if (normals.invalid_hemisphere) {
    stop(road_surface_normal_error(normals.minimum_final_dot));
  }
  return List::create(
      _["left"] = wrap_road_vec3_matrix(normals.left),
      _["right"] = wrap_road_vec3_matrix(normals.right),
      _["first_face"] = wrap_road_vec3_matrix(normals.first_face),
      _["second_face"] = wrap_road_vec3_matrix(normals.second_face));
}

// [[Rcpp::export]]
List build_render_road_section_mesh_cpp(
    const NumericMatrix& left_bottom_matrix,
    const NumericMatrix& right_bottom_matrix,
    const NumericMatrix& left_top_matrix,
    const NumericMatrix& right_top_matrix,
    const NumericMatrix& incoming_tangent,
    const NumericMatrix& outgoing_tangent,
    const NumericVector& texture_v,
    double closing_v,
    const NumericVector& bbox_center,
    const NumericMatrix& top_left_normal_matrix,
    const NumericMatrix& top_right_normal_matrix,
    const NumericMatrix& bottom_left_normal_matrix,
    const NumericMatrix& bottom_right_normal_matrix,
    bool cap_start,
    bool cap_end,
    bool closed,
    bool parallel,
    double geometry_tolerance = 1e-12,
    double uv_tolerance = 1e-14) {
  const int point_count = left_top_matrix.nrow();
  validate_road_section_mesh_input(
      point_count,
      left_bottom_matrix,
      right_bottom_matrix,
      left_top_matrix,
      right_top_matrix,
      incoming_tangent,
      outgoing_tangent,
      texture_v,
      bbox_center,
      top_left_normal_matrix,
      top_right_normal_matrix,
      bottom_left_normal_matrix,
      bottom_right_normal_matrix,
      closed,
      closing_v,
      geometry_tolerance,
      uv_tolerance);
  (void)parallel;
  return wrap_road_mesh_data(build_road_section_mesh_native(
      copy_road_vec3_matrix(left_bottom_matrix),
      copy_road_vec3_matrix(right_bottom_matrix),
      copy_road_vec3_matrix(left_top_matrix),
      copy_road_vec3_matrix(right_top_matrix),
      copy_road_vec2_matrix(incoming_tangent),
      copy_road_vec2_matrix(outgoing_tangent),
      copy_road_numeric_vector(texture_v),
      closing_v,
      {bbox_center[0], bbox_center[1], bbox_center[2]},
      copy_road_vec3_matrix(top_left_normal_matrix),
      copy_road_vec3_matrix(top_right_normal_matrix),
      copy_road_vec3_matrix(bottom_left_normal_matrix),
      copy_road_vec3_matrix(bottom_right_normal_matrix),
      cap_start,
      cap_end,
      closed,
      geometry_tolerance,
      uv_tolerance));
}

// [[Rcpp::export]]
List densify_render_road_paths_batch_cpp(
    const List& input_jobs,
    const NumericMatrix& heightmap,
    double zscale,
    bool parallel,
    bool verbose) {
  const RoadTerrain terrain = copy_road_terrain(heightmap, zscale);
  std::vector<RoadDensifyJob> jobs;
  jobs.reserve(input_jobs.size());
  for (int index = 0; index < input_jobs.size(); ++index) {
    jobs.push_back(parse_road_densify_job(as<List>(input_jobs[index])));
  }
  std::vector<std::vector<RoadVec3>> results(jobs.size());
  std::unique_ptr<RcppThread::ProgressCounter> progress;
  if (verbose && !jobs.empty()) {
    progress.reset(new RcppThread::ProgressCounter(
        jobs.size(),
        1,
        "Densifying road paths: "));
  }
  const auto run_job = [&](std::size_t index) {
    results[index] = densify_road_path_native(jobs[index], terrain);
    if (progress) {
      ++(*progress);
    }
  };
  if (parallel && jobs.size() > 1) {
    RcppThread::ThreadPool pool;
    for (std::size_t index = 0; index < jobs.size(); ++index) {
      pool.push(run_job, index);
    }
    pool.wait();
  } else {
    for (std::size_t index = 0; index < jobs.size(); ++index) {
      RcppThread::checkUserInterrupt();
      run_job(index);
    }
  }
  List output(results.size());
  for (std::size_t index = 0; index < results.size(); ++index) {
    output[index] = wrap_road_vec3_matrix(results[index]);
  }
  return output;
}

// [[Rcpp::export]]
List sample_render_road_sections_batch_cpp(
    const List& input_jobs,
    const NumericMatrix& heightmap,
    double zscale,
    bool parallel,
    bool verbose) {
  const RoadTerrain terrain = copy_road_terrain(heightmap, zscale);
  std::vector<RoadSectionJob> jobs;
  jobs.reserve(input_jobs.size());
  for (int index = 0; index < input_jobs.size(); ++index) {
    jobs.push_back(parse_road_section_job(as<List>(input_jobs[index])));
  }
  std::vector<RoadSectionData> results(jobs.size());
  std::unique_ptr<RcppThread::ProgressCounter> progress;
  if (verbose && !jobs.empty()) {
    progress.reset(new RcppThread::ProgressCounter(
        jobs.size(),
        1,
        "Sampling road terrain sections: "));
  }
  const auto run_job = [&](std::size_t index) {
    results[index] = sample_road_sections_native(jobs[index], terrain);
    if (progress) {
      ++(*progress);
    }
  };
  if (parallel && jobs.size() > 1) {
    RcppThread::ThreadPool pool;
    for (std::size_t index = 0; index < jobs.size(); ++index) {
      pool.push(run_job, index);
    }
    pool.wait();
  } else {
    for (std::size_t index = 0; index < jobs.size(); ++index) {
      RcppThread::checkUserInterrupt();
      run_job(index);
    }
  }
  List output(results.size());
  for (std::size_t index = 0; index < results.size(); ++index) {
    output[index] = wrap_road_section_data(results[index]);
  }
  return output;
}

// [[Rcpp::export]]
List build_render_highquality_road_mesh_batch_cpp(
    const List& input_jobs,
    bool parallel,
    bool verbose) {
  std::vector<RoadMeshJob> jobs;
  jobs.reserve(input_jobs.size());
  for (int index = 0; index < input_jobs.size(); ++index) {
    jobs.push_back(parse_road_mesh_job(as<List>(input_jobs[index])));
  }
  std::vector<RoadMeshJobResult> results(jobs.size());
  std::unique_ptr<RcppThread::ProgressCounter> progress;
  if (verbose && !jobs.empty()) {
    progress.reset(new RcppThread::ProgressCounter(
        jobs.size(),
        1,
        "Converting roads to meshes: "));
  }
  const auto run_job = [&](std::size_t index) {
    results[index] = run_road_mesh_job(jobs[index]);
    if (progress) {
      ++(*progress);
    }
  };
  if (parallel && jobs.size() > 1) {
    RcppThread::ThreadPool pool;
    for (std::size_t index = 0; index < jobs.size(); ++index) {
      pool.push(run_job, index);
    }
    pool.wait();
  } else {
    for (std::size_t index = 0; index < jobs.size(); ++index) {
      RcppThread::checkUserInterrupt();
      run_job(index);
    }
  }

  List output(results.size());
  for (std::size_t index = 0; index < results.size(); ++index) {
    const RoadMeshJobResult& result = results[index];
    if (!result.success) {
      output[index] = List::create(
          _["success"] = false,
          _["error"] = result.error,
          _["meshes"] = List::create());
      continue;
    }
    List meshes(result.meshes.size());
    for (std::size_t mesh_index = 0;
         mesh_index < result.meshes.size();
         ++mesh_index) {
      meshes[mesh_index] = wrap_road_mesh_data(result.meshes[mesh_index]);
    }
    output[index] = List::create(
        _["success"] = true,
        _["error"] = R_NilValue,
        _["meshes"] = meshes);
  }
  return output;
}
