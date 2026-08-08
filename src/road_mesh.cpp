#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <limits>
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
  std::vector<double> result;
  result.reserve(values.size());
  for (double value : values) {
    if (!std::isfinite(value) ||
        value < -tolerance ||
        value > 1.0 + tolerance) {
      continue;
    }
    value = std::min(std::max(value, 0.0), 1.0);
    result.push_back(R::fround(value, 12.0));
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
