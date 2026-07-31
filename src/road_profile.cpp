#include <Rcpp.h>

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <limits>
#include <map>
#include <string>
#include <utility>
#include <vector>

using namespace Rcpp;

namespace {

constexpr double quadratic_tolerance = 1e-14;
constexpr int terrain_floor_request = 1;
constexpr int no_dip_chord_request = 2;
constexpr int overlap_clearance_request = 3;

struct ProfileEvaluation {
  double distance;
  double height;
  double grade;
  int control_a;
  int control_b;
};

struct ProfileQuadratic {
  double quadratic;
  double linear;
  double constant;
};

struct AdaptiveRequest {
  int type;
  int fragment_a;
  double distance_a;
  int fragment_b;
  double distance_b;
  int event_id;
  double clearance;
  double source_margin;
};

// This is a transient view over an ordinary R list. Slice offsets, dense
// fragment indices, and control rows are zero-based in C++; fragment, span,
// arc, and overlap IDs remain the stable external R identifiers. All station,
// elevation, clearance, and tolerance values are expressed in metres.
struct AuditSpecification {
  IntegerVector fragment_id;
  IntegerVector fragment_component;
  IntegerVector control_start;
  IntegerVector control_count;
  IntegerVector control_row;
  NumericVector control_distance;
  NumericVector control_tolerance;
  LogicalVector underground;
  IntegerVector terrain_start;
  IntegerVector terrain_count;
  NumericVector terrain_distance;
  NumericVector terrain_elevation;
  IntegerVector chord_span_id;
  IntegerVector chord_fragment_index;
  NumericVector chord_span_offset;
  IntegerVector chord_orientation;
  NumericVector chord_fragment_length;
  IntegerVector arc_span_id;
  IntegerVector arc_start_control;
  IntegerVector arc_end_control;
  NumericVector arc_start_station;
  NumericVector arc_end_station;
  NumericVector arc_length;
  NumericVector arc_span_length;
  LogicalVector arc_closed;
  IntegerVector arc_id;
  IntegerVector overlap_id;
  IntegerVector overlap_lower_fragment_index;
  IntegerVector overlap_upper_fragment_index;
  NumericVector overlap_lower_start;
  NumericVector overlap_lower_end;
  NumericVector overlap_upper_start;
  NumericVector overlap_upper_end;
  NumericVector overlap_clearance;
  IntegerVector prior_overlap_id;
  NumericVector prior_lower_distance;
  NumericVector prior_upper_distance;
  bool finite_geometry;
  bool finite_control_terrain;

  explicit AuditSpecification(const List& specification)
      : fragment_id(specification["fragment_id"]),
        fragment_component(specification["fragment_component"]),
        control_start(specification["control_start"]),
        control_count(specification["control_count"]),
        control_row(specification["control_row"]),
        control_distance(specification["control_distance"]),
        control_tolerance(specification["control_tolerance"]),
        underground(specification["underground"]),
        terrain_start(specification["terrain_start"]),
        terrain_count(specification["terrain_count"]),
        terrain_distance(specification["terrain_distance"]),
        terrain_elevation(specification["terrain_elevation"]),
        chord_span_id(specification["chord_span_id"]),
        chord_fragment_index(specification["chord_fragment_index"]),
        chord_span_offset(specification["chord_span_offset"]),
        chord_orientation(specification["chord_orientation"]),
        chord_fragment_length(specification["chord_fragment_length"]),
        arc_span_id(specification["arc_span_id"]),
        arc_start_control(specification["arc_start_control"]),
        arc_end_control(specification["arc_end_control"]),
        arc_start_station(specification["arc_start_station"]),
        arc_end_station(specification["arc_end_station"]),
        arc_length(specification["arc_length"]),
        arc_span_length(specification["arc_span_length"]),
        arc_closed(specification["arc_closed"]),
        arc_id(specification["arc_id"]),
        overlap_id(specification["overlap_id"]),
        overlap_lower_fragment_index(
            specification["overlap_lower_fragment_index"]),
        overlap_upper_fragment_index(
            specification["overlap_upper_fragment_index"]),
        overlap_lower_start(specification["overlap_lower_start"]),
        overlap_lower_end(specification["overlap_lower_end"]),
        overlap_upper_start(specification["overlap_upper_start"]),
        overlap_upper_end(specification["overlap_upper_end"]),
        overlap_clearance(specification["overlap_clearance"]),
        prior_overlap_id(specification["prior_overlap_id"]),
        prior_lower_distance(specification["prior_lower_distance"]),
        prior_upper_distance(specification["prior_upper_distance"]),
        finite_geometry(as<bool>(specification["finite_geometry"])),
        finite_control_terrain(
            as<bool>(specification["finite_control_terrain"])) {}
};

inline bool finite_number(double value) {
  return std::isfinite(value);
}

ProfileEvaluation evaluate_profile(
    const AuditSpecification& specification,
    const NumericVector& height,
    const NumericVector& grade,
    int fragment_index,
    double distance) {
  const int start = specification.control_start[fragment_index];
  const int count = specification.control_count[fragment_index];
  const double* first_distance =
      specification.control_distance.begin() + start;
  const double* last_distance = first_distance + count;
  const double profile_length = *(last_distance - 1);
  const double clamped_distance =
      std::min(std::max(distance, 0.0), profile_length);
  const double* upper =
      std::upper_bound(first_distance, last_distance, clamped_distance);
  int interval = static_cast<int>(upper - first_distance) - 1;
  interval = std::max(interval, 0);
  interval = std::min(interval, count - 2);
  const int flat_a = start + interval;
  const int flat_b = flat_a + 1;
  const int control_a = specification.control_row[flat_a];
  const int control_b = specification.control_row[flat_b];
  const double interval_length =
      specification.control_distance[flat_b] -
      specification.control_distance[flat_a];
  const double local_distance =
      clamped_distance - specification.control_distance[flat_a];
  const double grade_change = grade[control_b] - grade[control_a];
  const double evaluated_height =
      height[control_a] + grade[control_a] * local_distance +
      grade_change * local_distance * local_distance /
          (2.0 * interval_length);
  const double evaluated_grade =
      grade[control_a] + grade_change * local_distance / interval_length;
  return {
      clamped_distance,
      evaluated_height,
      evaluated_grade,
      control_a,
      control_b};
}

ProfileQuadratic profile_quadratic(
    const AuditSpecification& specification,
    const NumericVector& height,
    const NumericVector& grade,
    int fragment_index,
    double distance_start,
    double distance_rate,
    double selection_distance) {
  const int start = specification.control_start[fragment_index];
  const int count = specification.control_count[fragment_index];
  const double* first_distance =
      specification.control_distance.begin() + start;
  const double* last_distance = first_distance + count;
  const double profile_length = *(last_distance - 1);
  const double clamped_selection =
      std::min(std::max(selection_distance, 0.0), profile_length);
  const double* upper =
      std::upper_bound(first_distance, last_distance, clamped_selection);
  int interval = static_cast<int>(upper - first_distance) - 1;
  interval = std::max(interval, 0);
  interval = std::min(interval, count - 2);
  const int flat_a = start + interval;
  const int flat_b = flat_a + 1;
  const int control_a = specification.control_row[flat_a];
  const int control_b = specification.control_row[flat_b];
  const double interval_length =
      specification.control_distance[flat_b] -
      specification.control_distance[flat_a];
  const double grade_change = grade[control_b] - grade[control_a];
  const double base_quadratic =
      grade_change / (2.0 * interval_length);
  const double local_start =
      distance_start - specification.control_distance[flat_a];
  return {
      base_quadratic * distance_rate * distance_rate,
      grade[control_a] * distance_rate +
          2.0 * base_quadratic * local_start * distance_rate,
      height[control_a] + grade[control_a] * local_start +
          base_quadratic * local_start * local_start};
}

double interpolate_terrain(
    const AuditSpecification& specification,
    int fragment_index,
    double distance) {
  const int start = specification.terrain_start[fragment_index];
  const int count = specification.terrain_count[fragment_index];
  const double* first_distance =
      specification.terrain_distance.begin() + start;
  const double* last_distance = first_distance + count;
  if (distance <= *first_distance) {
    return specification.terrain_elevation[start];
  }
  if (distance >= *(last_distance - 1)) {
    return specification.terrain_elevation[start + count - 1];
  }
  const double* upper =
      std::upper_bound(first_distance, last_distance, distance);
  const int interval = static_cast<int>(upper - first_distance) - 1;
  const int first = start + interval;
  const int second = first + 1;
  const double run = specification.terrain_distance[second] -
                     specification.terrain_distance[first];
  const double fraction =
      (distance - specification.terrain_distance[first]) / run;
  return specification.terrain_elevation[first] +
         fraction * (specification.terrain_elevation[second] -
                     specification.terrain_elevation[first]);
}

double span_station(
    const AuditSpecification& specification,
    int chord_index,
    double distance) {
  return specification.chord_span_offset[chord_index] +
         (specification.chord_orientation[chord_index] == 1
              ? distance
              : specification.chord_fragment_length[chord_index] - distance);
}

int resolve_support_arc(
    const AuditSpecification& specification,
    int span_id,
    double station,
    double& fraction) {
  const double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  bool closed = false;
  double span_length = 0.0;
  for (R_xlen_t arc = 0; arc < specification.arc_span_id.size(); ++arc) {
    if (specification.arc_span_id[arc] == span_id) {
      closed = specification.arc_closed[arc];
      span_length = specification.arc_span_length[arc];
      break;
    }
  }
  std::vector<double> candidate_value;
  if (closed) {
    double normalized = std::fmod(station, span_length);
    if (normalized < 0.0) {
      normalized += span_length;
    }
    candidate_value.push_back(normalized);
    candidate_value.push_back(normalized + span_length);
  } else {
    candidate_value.push_back(station);
  }
  for (double candidate : candidate_value) {
    for (R_xlen_t arc = 0; arc < specification.arc_span_id.size(); ++arc) {
      if (specification.arc_span_id[arc] != span_id) {
        continue;
      }
      if (candidate >= specification.arc_start_station[arc] - tolerance &&
          candidate < specification.arc_end_station[arc] - tolerance) {
        fraction =
            (candidate - specification.arc_start_station[arc]) /
            specification.arc_length[arc];
        return static_cast<int>(arc);
      }
    }
  }
  const double terminal = candidate_value.back();
  int terminal_arc = -1;
  for (R_xlen_t arc = 0; arc < specification.arc_span_id.size(); ++arc) {
    if (specification.arc_span_id[arc] == span_id &&
        std::abs(terminal - specification.arc_end_station[arc]) <= tolerance) {
      terminal_arc = static_cast<int>(arc);
    }
  }
  if (terminal_arc < 0) {
    stop(
        "Station %.6f is outside profile span %d.",
        station,
        span_id);
  }
  fraction =
      (specification.arc_end_station[terminal_arc] -
       specification.arc_start_station[terminal_arc]) /
      specification.arc_length[terminal_arc];
  return terminal_arc;
}

List make_request_list(const std::vector<AdaptiveRequest>& candidates) {
  std::map<std::pair<int, int>, int> single_best;
  std::map<int, int> overlap_best;
  for (std::size_t index = 0; index < candidates.size(); ++index) {
    const AdaptiveRequest& request = candidates[index];
    if (request.type == overlap_clearance_request) {
      auto found = overlap_best.find(request.event_id);
      if (found == overlap_best.end() ||
          request.source_margin <
              candidates[found->second].source_margin) {
        overlap_best[request.event_id] = static_cast<int>(index);
      }
    } else {
      const std::pair<int, int> key(request.type, request.fragment_a);
      auto found = single_best.find(key);
      if (found == single_best.end() ||
          request.source_margin <
              candidates[found->second].source_margin) {
        single_best[key] = static_cast<int>(index);
      }
    }
  }
  std::vector<int> selected;
  selected.reserve(single_best.size() + overlap_best.size());
  for (const auto& entry : single_best) {
    selected.push_back(entry.second);
  }
  for (const auto& entry : overlap_best) {
    selected.push_back(entry.second);
  }
  std::sort(selected.begin(), selected.end());
  selected.erase(std::unique(selected.begin(), selected.end()), selected.end());

  IntegerVector type(selected.size());
  IntegerVector fragment_a(selected.size());
  NumericVector distance_a(selected.size());
  IntegerVector fragment_b(selected.size());
  NumericVector distance_b(selected.size());
  IntegerVector event_id(selected.size());
  NumericVector clearance(selected.size());
  NumericVector source_margin(selected.size());
  for (std::size_t row = 0; row < selected.size(); ++row) {
    const AdaptiveRequest& request = candidates[selected[row]];
    type[row] = request.type;
    fragment_a[row] = request.fragment_a;
    distance_a[row] = request.distance_a;
    fragment_b[row] = request.fragment_b;
    distance_b[row] = request.distance_b;
    event_id[row] = request.event_id;
    clearance[row] = request.clearance;
    source_margin[row] = request.source_margin;
  }
  return List::create(
      _["type"] = type,
      _["fragment_a"] = fragment_a,
      _["distance_a"] = distance_a,
      _["fragment_b"] = fragment_b,
      _["distance_b"] = distance_b,
      _["event_id"] = event_id,
      _["clearance"] = clearance,
      _["source_margin"] = source_margin);
}

}  // namespace

// [[Rcpp::export]]
List evaluate_render_road_profiles_cpp(
    IntegerVector fragment_index,
    NumericVector distance,
    IntegerVector control_start,
    IntegerVector control_count,
    IntegerVector control_row,
    NumericVector control_distance,
    NumericVector height,
    NumericVector grade) {
  if (fragment_index.size() != distance.size()) {
    stop("`fragment_index` and `distance` must have equal lengths.");
  }
  if (height.size() != grade.size() ||
      height.size() != control_row.size() ||
      control_distance.size() != control_row.size()) {
    stop("Road-profile solution vectors do not match the audit specification.");
  }
  NumericVector evaluated_distance(distance.size());
  NumericVector evaluated_height(distance.size());
  NumericVector evaluated_grade(distance.size());
  IntegerVector control_a(distance.size());
  IntegerVector control_b(distance.size());
  for (R_xlen_t query = 0; query < distance.size(); ++query) {
    if (fragment_index[query] < 0 ||
        fragment_index[query] >= control_start.size()) {
      stop("Unknown road-profile fragment index.");
    }
    if (NumericVector::is_na(distance[query]) ||
        std::isnan(distance[query])) {
      evaluated_distance[query] = NA_REAL;
      evaluated_height[query] = NA_REAL;
      evaluated_grade[query] = NA_REAL;
      control_a[query] = NA_INTEGER;
      control_b[query] = NA_INTEGER;
      continue;
    }
    const int start = control_start[fragment_index[query]];
    const int count = control_count[fragment_index[query]];
    const double* first_distance = control_distance.begin() + start;
    const double* last_distance = first_distance + count;
    const double profile_length = *(last_distance - 1);
    const double clamped_distance =
        std::min(std::max(distance[query], 0.0), profile_length);
    const double* upper =
        std::upper_bound(first_distance, last_distance, clamped_distance);
    int interval = static_cast<int>(upper - first_distance) - 1;
    interval = std::max(interval, 0);
    interval = std::min(interval, count - 2);
    const int flat_a = start + interval;
    const int flat_b = flat_a + 1;
    const int first = control_row[flat_a];
    const int second = control_row[flat_b];
    const double interval_length =
        control_distance[flat_b] - control_distance[flat_a];
    const double local_distance =
        clamped_distance - control_distance[flat_a];
    const double grade_change = grade[second] - grade[first];
    evaluated_distance[query] = clamped_distance;
    evaluated_height[query] =
        height[first] + grade[first] * local_distance +
        grade_change * local_distance * local_distance /
            (2.0 * interval_length);
    evaluated_grade[query] =
        grade[first] + grade_change * local_distance / interval_length;
    control_a[query] = first + 1;
    control_b[query] = second + 1;
  }
  return List::create(
      _["distance"] = evaluated_distance,
      _["height"] = evaluated_height,
      _["grade"] = evaluated_grade,
      _["control_a"] = control_a,
      _["control_b"] = control_b);
}

// [[Rcpp::export]]
List audit_render_road_profiles_cpp(
    List specification_list,
    NumericVector height,
    NumericVector grade,
    double tolerance,
    bool diagnostics = false) {
  AuditSpecification specification(specification_list);
  if (height.size() != grade.size() ||
      height.size() != specification.control_row.size()) {
    stop("Road-profile solution vectors do not match the audit specification.");
  }
  bool finite_profile_coordinates =
      specification.finite_geometry &&
      specification.finite_control_terrain;
  for (R_xlen_t row = 0; row < height.size(); ++row) {
    finite_profile_coordinates =
        finite_profile_coordinates &&
        finite_number(height[row]) &&
        finite_number(grade[row]) &&
        finite_number(specification.control_distance[row]);
  }

  double terrain_margin = R_PosInf;
  double chord_margin = R_PosInf;
  double overlap_margin = R_PosInf;
  std::vector<AdaptiveRequest> request_candidates;

  std::vector<int> terrain_fragment;
  std::vector<double> terrain_check_distance;
  std::vector<double> terrain_height;
  std::vector<double> terrain_reference;
  std::vector<double> terrain_check_margin;

  std::vector<int> chord_span;
  std::vector<int> chord_arc;
  std::vector<int> chord_fragment;
  std::vector<double> chord_distance;
  std::vector<double> chord_station;
  std::vector<double> chord_height;
  std::vector<double> chord_reference;
  std::vector<double> chord_check_margin;

  std::vector<int> overlap_check_id;
  std::vector<double> overlap_parameter;
  std::vector<int> overlap_lower_fragment;
  std::vector<int> overlap_upper_fragment;
  std::vector<double> overlap_lower_distance;
  std::vector<double> overlap_upper_distance;
  std::vector<double> overlap_check_margin;
  request_candidates.reserve(
      specification.fragment_id.size() * 2 +
      specification.overlap_id.size());
  if (diagnostics) {
    const std::size_t terrain_reserve =
        specification.control_distance.size() +
        specification.terrain_distance.size();
    terrain_fragment.reserve(terrain_reserve);
    terrain_check_distance.reserve(terrain_reserve);
    terrain_height.reserve(terrain_reserve);
    terrain_reference.reserve(terrain_reserve);
    terrain_check_margin.reserve(terrain_reserve);
    chord_span.reserve(specification.control_distance.size());
    chord_arc.reserve(specification.control_distance.size());
    chord_fragment.reserve(specification.control_distance.size());
    chord_distance.reserve(specification.control_distance.size());
    chord_station.reserve(specification.control_distance.size());
    chord_height.reserve(specification.control_distance.size());
    chord_reference.reserve(specification.control_distance.size());
    chord_check_margin.reserve(specification.control_distance.size());
    const std::size_t overlap_reserve =
        specification.control_distance.size() +
        specification.overlap_id.size();
    overlap_check_id.reserve(overlap_reserve);
    overlap_parameter.reserve(overlap_reserve);
    overlap_lower_fragment.reserve(overlap_reserve);
    overlap_upper_fragment.reserve(overlap_reserve);
    overlap_lower_distance.reserve(overlap_reserve);
    overlap_upper_distance.reserve(overlap_reserve);
    overlap_check_margin.reserve(overlap_reserve);
  }

  int prior_component = NA_INTEGER;
  for (R_xlen_t fragment_index = 0;
       fragment_index < specification.fragment_id.size();
       ++fragment_index) {
    if (fragment_index == 0 ||
        specification.fragment_component[fragment_index] !=
            prior_component) {
      Rcpp::checkUserInterrupt();
      prior_component = specification.fragment_component[fragment_index];
    }
    const int terrain_begin = specification.terrain_start[fragment_index];
    const int terrain_end =
        terrain_begin + specification.terrain_count[fragment_index];
    for (int terrain_row = terrain_begin;
         terrain_row < terrain_end;
         ++terrain_row) {
      const ProfileEvaluation evaluation = evaluate_profile(
          specification,
          height,
          grade,
          fragment_index,
          specification.terrain_distance[terrain_row]);
      finite_profile_coordinates =
          finite_profile_coordinates &&
          finite_number(evaluation.height) &&
          finite_number(evaluation.grade);
    }
    if (specification.underground[fragment_index]) {
      continue;
    }
    const int control_begin = specification.control_start[fragment_index];
    const int control_end =
        control_begin + specification.control_count[fragment_index] - 1;
    int terrain_cursor = terrain_begin;
    for (int control = control_begin; control < control_end; ++control) {
      const int control_a = specification.control_row[control];
      const int control_b = specification.control_row[control + 1];
      const double interval_start =
          specification.control_distance[control];
      const double interval_end =
          specification.control_distance[control + 1];
      const double interval_length = interval_end - interval_start;
      const double quadratic_coefficient =
          (grade[control_b] - grade[control_a]) /
          (2.0 * interval_length);
      while (
          terrain_cursor < terrain_end - 1 &&
          specification.terrain_distance[terrain_cursor + 1] <
              interval_start) {
        ++terrain_cursor;
      }
      for (int terrain_row = terrain_cursor;
           terrain_row < terrain_end - 1 &&
           specification.terrain_distance[terrain_row] <= interval_end;
           ++terrain_row) {
        const double terrain_start =
            specification.terrain_distance[terrain_row];
        const double terrain_end_distance =
            specification.terrain_distance[terrain_row + 1];
        const double check_start =
            std::max(interval_start, terrain_start);
        const double check_end =
            std::min(interval_end, terrain_end_distance);
        if (check_end < check_start) {
          continue;
        }
        const double terrain_slope =
            (specification.terrain_elevation[terrain_row + 1] -
             specification.terrain_elevation[terrain_row]) /
            (terrain_end_distance - terrain_start);
        double candidate[3] = {check_start, check_end, 0.0};
        int candidate_count = 2;
        if (std::abs(quadratic_coefficient) > quadratic_tolerance) {
          const double stationary_local =
              (terrain_slope - grade[control_a]) /
              (2.0 * quadratic_coefficient);
          const double stationary = interval_start + stationary_local;
          if (stationary > check_start && stationary < check_end) {
            candidate[candidate_count++] = stationary;
          }
        }
        std::sort(candidate, candidate + candidate_count);
        candidate_count = static_cast<int>(
            std::unique(candidate, candidate + candidate_count) - candidate);
        int worst = 0;
        double worst_height = NA_REAL;
        double worst_terrain = NA_REAL;
        double worst_margin = R_PosInf;
        for (int index = 0; index < candidate_count; ++index) {
          const ProfileEvaluation evaluation = evaluate_profile(
              specification,
              height,
              grade,
              fragment_index,
              candidate[index]);
          const double terrain_value = interpolate_terrain(
              specification, fragment_index, candidate[index]);
          const double margin = evaluation.height - terrain_value;
          finite_profile_coordinates =
              finite_profile_coordinates &&
              finite_number(evaluation.height) &&
              finite_number(evaluation.grade);
          if (index == 0 || margin < worst_margin) {
            worst = index;
            worst_height = evaluation.height;
            worst_terrain = terrain_value;
            worst_margin = margin;
          }
        }
        terrain_margin = std::min(terrain_margin, worst_margin);
        if (diagnostics) {
          terrain_fragment.push_back(
              specification.fragment_id[fragment_index]);
          terrain_check_distance.push_back(candidate[worst]);
          terrain_height.push_back(worst_height);
          terrain_reference.push_back(worst_terrain);
          terrain_check_margin.push_back(worst_margin);
        }
        if (worst_margin < -tolerance) {
          bool existing = false;
          for (int row = control_begin; row <= control_end; ++row) {
            if (std::abs(
                    specification.control_distance[row] -
                    candidate[worst]) <=
                specification.control_tolerance[fragment_index]) {
              existing = true;
              break;
            }
          }
          if (!existing) {
            request_candidates.push_back({
                terrain_floor_request,
                specification.fragment_id[fragment_index],
                candidate[worst],
                NA_INTEGER,
                NA_REAL,
                NA_INTEGER,
                NA_REAL,
                worst_margin});
          }
        }
      }
    }
  }

  int prior_span = NA_INTEGER;
  for (R_xlen_t member = 0;
       member < specification.chord_span_id.size();
       ++member) {
    if (member == 0 || specification.chord_span_id[member] != prior_span) {
      Rcpp::checkUserInterrupt();
      prior_span = specification.chord_span_id[member];
    }
    const int fragment_index =
        specification.chord_fragment_index[member];
    const int control_begin = specification.control_start[fragment_index];
    const int control_end =
        control_begin + specification.control_count[fragment_index] - 1;
    for (int control = control_begin; control < control_end; ++control) {
      const int control_a = specification.control_row[control];
      const int control_b = specification.control_row[control + 1];
      const double interval_start =
          specification.control_distance[control];
      const double interval_end =
          specification.control_distance[control + 1];
      const double interval_length = interval_end - interval_start;
      const double quadratic_coefficient =
          (grade[control_b] - grade[control_a]) /
          (2.0 * interval_length);
      const double midpoint = span_station(
          specification,
          member,
          (interval_start + interval_end) / 2.0);
      double midpoint_fraction = 0.0;
      const int midpoint_arc = resolve_support_arc(
          specification,
          specification.chord_span_id[member],
          midpoint,
          midpoint_fraction);
      const int start_control =
          specification.arc_start_control[midpoint_arc];
      const int end_control = specification.arc_end_control[midpoint_arc];
      const double span_slope =
          (height[end_control] - height[start_control]) /
          specification.arc_length[midpoint_arc];
      const double chord_local_slope =
          span_slope * specification.chord_orientation[member];
      double candidate[3] = {interval_start, interval_end, 0.0};
      int candidate_count = 2;
      if (std::abs(quadratic_coefficient) > quadratic_tolerance) {
        const double stationary_local =
            (chord_local_slope - grade[control_a]) /
            (2.0 * quadratic_coefficient);
        const double stationary = interval_start + stationary_local;
        if (stationary > interval_start && stationary < interval_end) {
          candidate[candidate_count++] = stationary;
        }
      }
      std::sort(candidate, candidate + candidate_count);
      candidate_count = static_cast<int>(
          std::unique(candidate, candidate + candidate_count) - candidate);
      int worst = 0;
      int worst_arc = -1;
      double worst_station = NA_REAL;
      double worst_height = NA_REAL;
      double worst_chord = NA_REAL;
      double worst_margin = R_PosInf;
      for (int index = 0; index < candidate_count; ++index) {
        const ProfileEvaluation evaluation = evaluate_profile(
            specification,
            height,
            grade,
            fragment_index,
            candidate[index]);
        const double station =
            span_station(specification, member, candidate[index]);
        double fraction = 0.0;
        const int arc = resolve_support_arc(
            specification,
            specification.chord_span_id[member],
            station,
            fraction);
        const double chord =
            height[specification.arc_start_control[arc]] +
            fraction *
                (height[specification.arc_end_control[arc]] -
                 height[specification.arc_start_control[arc]]);
        const double margin = evaluation.height - chord;
        finite_profile_coordinates =
            finite_profile_coordinates &&
            finite_number(evaluation.height) &&
            finite_number(evaluation.grade);
        if (index == 0 || margin < worst_margin) {
          worst = index;
          worst_arc = arc;
          worst_station = station;
          worst_height = evaluation.height;
          worst_chord = chord;
          worst_margin = margin;
        }
      }
      chord_margin = std::min(chord_margin, worst_margin);
      if (diagnostics) {
        chord_span.push_back(specification.chord_span_id[member]);
        chord_arc.push_back(specification.arc_id[worst_arc]);
        chord_fragment.push_back(
            specification.fragment_id[fragment_index]);
        chord_distance.push_back(candidate[worst]);
        chord_station.push_back(worst_station);
        chord_height.push_back(worst_height);
        chord_reference.push_back(worst_chord);
        chord_check_margin.push_back(worst_margin);
      }
      if (worst_margin < -tolerance) {
        bool existing = false;
        for (int row = control_begin; row <= control_end; ++row) {
          if (std::abs(
                  specification.control_distance[row] -
                  candidate[worst]) <=
              specification.control_tolerance[fragment_index]) {
            existing = true;
            break;
          }
        }
        if (!existing) {
          request_candidates.push_back({
              no_dip_chord_request,
              specification.fragment_id[fragment_index],
              candidate[worst],
              NA_INTEGER,
              NA_REAL,
              specification.chord_span_id[member],
              NA_REAL,
              worst_margin});
        }
      }
    }
  }

  for (R_xlen_t relation = 0;
       relation < specification.overlap_id.size();
       ++relation) {
    Rcpp::checkUserInterrupt();
    const int lower_fragment_index =
        specification.overlap_lower_fragment_index[relation];
    const int upper_fragment_index =
        specification.overlap_upper_fragment_index[relation];
    const double lower_delta =
        specification.overlap_lower_end[relation] -
        specification.overlap_lower_start[relation];
    const double upper_delta =
        specification.overlap_upper_end[relation] -
        specification.overlap_upper_start[relation];
    std::vector<double> parameter_break = {0.0, 1.0};
    const int lower_start =
        specification.control_start[lower_fragment_index];
    const int lower_count =
        specification.control_count[lower_fragment_index];
    const int upper_start =
        specification.control_start[upper_fragment_index];
    const int upper_count =
        specification.control_count[upper_fragment_index];
    if (std::abs(lower_delta) > 0.0) {
      for (int row = lower_start; row < lower_start + lower_count; ++row) {
        const double value =
            (specification.control_distance[row] -
             specification.overlap_lower_start[relation]) /
            lower_delta;
        if (finite_number(value) && value >= 0.0 && value <= 1.0) {
          parameter_break.push_back(value);
        }
      }
    }
    if (std::abs(upper_delta) > 0.0) {
      for (int row = upper_start; row < upper_start + upper_count; ++row) {
        const double value =
            (specification.control_distance[row] -
             specification.overlap_upper_start[relation]) /
            upper_delta;
        if (finite_number(value) && value >= 0.0 && value <= 1.0) {
          parameter_break.push_back(value);
        }
      }
    }
    std::sort(parameter_break.begin(), parameter_break.end());
    parameter_break.erase(
        std::unique(parameter_break.begin(), parameter_break.end()),
        parameter_break.end());
    for (std::size_t interval = 0;
         interval + 1 < parameter_break.size();
         ++interval) {
      const double parameter_start = parameter_break[interval];
      const double parameter_end = parameter_break[interval + 1];
      const double parameter_rate = parameter_end - parameter_start;
      const double parameter_mid =
          (parameter_start + parameter_end) / 2.0;
      const double lower_distance_start =
          specification.overlap_lower_start[relation] +
          lower_delta * parameter_start;
      const double upper_distance_start =
          specification.overlap_upper_start[relation] +
          upper_delta * parameter_start;
      const ProfileQuadratic lower_quadratic = profile_quadratic(
          specification,
          height,
          grade,
          lower_fragment_index,
          lower_distance_start,
          lower_delta * parameter_rate,
          specification.overlap_lower_start[relation] +
              lower_delta * parameter_mid);
      const ProfileQuadratic upper_quadratic = profile_quadratic(
          specification,
          height,
          grade,
          upper_fragment_index,
          upper_distance_start,
          upper_delta * parameter_rate,
          specification.overlap_upper_start[relation] +
              upper_delta * parameter_mid);
      const double coefficient_a =
          upper_quadratic.quadratic - lower_quadratic.quadratic;
      const double coefficient_b =
          upper_quadratic.linear - lower_quadratic.linear;
      const double coefficient_c =
          upper_quadratic.constant -
          lower_quadratic.constant -
          specification.overlap_clearance[relation];
      finite_profile_coordinates =
          finite_profile_coordinates &&
          finite_number(coefficient_a) &&
          finite_number(coefficient_b) &&
          finite_number(coefficient_c);
      double local_candidate[3] = {0.0, 1.0, 0.0};
      int candidate_count = 2;
      if (std::abs(coefficient_a) > quadratic_tolerance) {
        const double stationary =
            -coefficient_b / (2.0 * coefficient_a);
        if (stationary > 0.0 && stationary < 1.0) {
          local_candidate[candidate_count++] = stationary;
        }
      }
      std::sort(local_candidate, local_candidate + candidate_count);
      candidate_count = static_cast<int>(
          std::unique(
              local_candidate,
              local_candidate + candidate_count) -
          local_candidate);
      int worst = 0;
      double worst_margin = R_PosInf;
      for (int index = 0; index < candidate_count; ++index) {
        const double margin =
            coefficient_a * local_candidate[index] *
                local_candidate[index] +
            coefficient_b * local_candidate[index] +
            coefficient_c;
        if (index == 0 || margin < worst_margin) {
          worst = index;
          worst_margin = margin;
        }
      }
      const double parameter =
          parameter_start +
          (parameter_end - parameter_start) * local_candidate[worst];
      const double lower_distance =
          specification.overlap_lower_start[relation] +
          lower_delta * parameter;
      const double upper_distance =
          specification.overlap_upper_start[relation] +
          upper_delta * parameter;
      overlap_margin = std::min(overlap_margin, worst_margin);
      if (diagnostics) {
        overlap_check_id.push_back(specification.overlap_id[relation]);
        overlap_parameter.push_back(parameter);
        overlap_lower_fragment.push_back(
            specification.fragment_id[lower_fragment_index]);
        overlap_upper_fragment.push_back(
            specification.fragment_id[upper_fragment_index]);
        overlap_lower_distance.push_back(lower_distance);
        overlap_upper_distance.push_back(upper_distance);
        overlap_check_margin.push_back(worst_margin);
      }
      if (worst_margin < -tolerance) {
        bool already_requested = false;
        for (R_xlen_t prior = 0;
             prior < specification.prior_overlap_id.size();
             ++prior) {
          if (specification.prior_overlap_id[prior] ==
                  specification.overlap_id[relation] &&
              std::abs(
                  specification.prior_lower_distance[prior] -
                  lower_distance) <=
                  specification.control_tolerance[lower_fragment_index] &&
              std::abs(
                  specification.prior_upper_distance[prior] -
                  upper_distance) <=
                  specification.control_tolerance[upper_fragment_index]) {
            already_requested = true;
            break;
          }
        }
        if (!already_requested) {
          request_candidates.push_back({
              overlap_clearance_request,
              specification.fragment_id[lower_fragment_index],
              lower_distance,
              specification.fragment_id[upper_fragment_index],
              upper_distance,
              specification.overlap_id[relation],
              specification.overlap_clearance[relation],
              worst_margin});
        }
      }
    }
  }

  List result = List::create(
      _["continuous_terrain_margin"] = terrain_margin,
      _["continuous_chord_margin"] = chord_margin,
      _["continuous_overlap_clearance_margin"] = overlap_margin,
      _["finite_profile_coordinates"] = finite_profile_coordinates,
      _["requests"] = make_request_list(request_candidates));
  if (diagnostics) {
    result["terrain"] = DataFrame::create(
        _["render_road_fragment_id"] = wrap(terrain_fragment),
        _["distance"] = wrap(terrain_check_distance),
        _["height"] = wrap(terrain_height),
        _["terrain"] = wrap(terrain_reference),
        _["margin"] = wrap(terrain_check_margin),
        _["stringsAsFactors"] = false);
    result["chord"] = DataFrame::create(
        _["span_id"] = wrap(chord_span),
        _["support_arc_id"] = wrap(chord_arc),
        _["render_road_fragment_id"] = wrap(chord_fragment),
        _["distance"] = wrap(chord_distance),
        _["span_station"] = wrap(chord_station),
        _["height"] = wrap(chord_height),
        _["chord"] = wrap(chord_reference),
        _["margin"] = wrap(chord_check_margin),
        _["stringsAsFactors"] = false);
    result["overlap"] = DataFrame::create(
        _["overlap_id"] = wrap(overlap_check_id),
        _["parameter"] = wrap(overlap_parameter),
        _["lower_fragment_id"] = wrap(overlap_lower_fragment),
        _["upper_fragment_id"] = wrap(overlap_upper_fragment),
        _["lower_distance"] = wrap(overlap_lower_distance),
        _["upper_distance"] = wrap(overlap_upper_distance),
        _["margin"] = wrap(overlap_check_margin),
        _["stringsAsFactors"] = false);
  }
  return result;
}

namespace {

constexpr int point_crossing_flag = 1;
constexpr int point_junction_flag = 2;
constexpr int point_conflict_flag = 3;

constexpr int constraint_quadratic_interval = 1;
constexpr int constraint_grade_rate = 2;
constexpr int constraint_grade_bound = 3;
constexpr int constraint_terrain_floor = 4;
constexpr int constraint_ground_anchor = 5;
constexpr int constraint_crossing_clearance = 6;
constexpr int constraint_junction_height = 7;
constexpr int constraint_overlap_clearance = 8;
constexpr int constraint_overlap_clearance_adaptive = 9;
constexpr int constraint_continuation_height = 10;
constexpr int constraint_continuation_grade = 11;
constexpr int constraint_continuation_gap_interval = 12;
constexpr int constraint_continuation_gap_grade_rate = 13;
constexpr int constraint_no_dip_span_chord = 14;

struct CompilerSpecification {
  IntegerVector fragment_id;
  IntegerVector feature_id;
  IntegerVector component_id;
  NumericVector fragment_length;
  NumericVector layer;
  LogicalVector underground;
  IntegerVector fragment_span_id;
  NumericVector fragment_span_offset;
  IntegerVector fragment_span_orientation;
  IntegerVector terrain_start;
  IntegerVector terrain_count;
  NumericVector terrain_distance;
  NumericVector terrain_elevation;
  IntegerVector initial_fragment;
  NumericVector initial_distance;
  IntegerVector point_pair_id;
  IntegerVector point_fragment_a;
  NumericVector point_distance_a;
  IntegerVector point_fragment_b;
  NumericVector point_distance_b;
  IntegerVector point_flag;
  IntegerVector span_id;
  NumericVector span_length;
  IntegerVector span_start_fragment;
  IntegerVector span_start_side;
  IntegerVector span_end_fragment;
  IntegerVector span_end_side;
  LogicalVector span_closed;
  LogicalVector span_no_dip;
  IntegerVector span_reference;
  IntegerVector anchor_endpoint_id;
  IntegerVector anchor_fragment;
  IntegerVector anchor_side;
  NumericVector anchor_distance;
  NumericVector anchor_terrain_grade;
  IntegerVector crossing_id;
  IntegerVector crossing_pair_id;
  IntegerVector crossing_lower_fragment;
  IntegerVector crossing_upper_fragment;
  NumericVector crossing_lower_distance;
  NumericVector crossing_upper_distance;
  NumericVector crossing_lower_rank;
  NumericVector crossing_upper_rank;
  NumericVector crossing_clearance;
  IntegerVector junction_id;
  IntegerVector junction_pair_id;
  IntegerVector junction_fragment_a;
  IntegerVector junction_fragment_b;
  NumericVector junction_distance_a;
  NumericVector junction_distance_b;
  IntegerVector overlap_id;
  IntegerVector overlap_lower_fragment;
  IntegerVector overlap_upper_fragment;
  NumericVector overlap_lower_start;
  NumericVector overlap_lower_end;
  NumericVector overlap_upper_start;
  NumericVector overlap_upper_end;
  NumericVector overlap_clearance;
  IntegerVector continuation_id;
  IntegerVector continuation_fragment_a;
  IntegerVector continuation_fragment_b;
  NumericVector continuation_distance_a;
  NumericVector continuation_distance_b;
  NumericVector continuation_sign_a;
  NumericVector continuation_sign_b;
  NumericVector continuation_gap;
  LogicalVector continuation_exact;
  double maximum_grade;
  double maximum_grade_rate;
  double curvature_weight;
  double grade_weight;
  double terrain_reference_weight;
  double underground_reference_depth;
  double underground_reference_weight;
  double anchor_grade_weight;
  double uplift_weight;
  double control_tolerance;

  explicit CompilerSpecification(const List& specification) {
    const List fragment = specification["fragment"];
    fragment_id = fragment["id"];
    feature_id = fragment["feature_id"];
    component_id = fragment["component_id"];
    fragment_length = fragment["length"];
    layer = fragment["layer"];
    underground = fragment["underground"];
    fragment_span_id = fragment["span_id"];
    fragment_span_offset = fragment["span_station_offset"];
    fragment_span_orientation = fragment["span_orientation"];
    const List terrain = specification["terrain"];
    terrain_start = terrain["start"];
    terrain_count = terrain["count"];
    terrain_distance = terrain["distance"];
    terrain_elevation = terrain["elevation"];
    const List initial_control = specification["initial_control"];
    initial_fragment = initial_control["fragment_index"];
    initial_distance = initial_control["distance"];
    const List point_relation = specification["point_relation"];
    point_pair_id = point_relation["pair_id"];
    point_fragment_a = point_relation["fragment_a"];
    point_distance_a = point_relation["distance_a"];
    point_fragment_b = point_relation["fragment_b"];
    point_distance_b = point_relation["distance_b"];
    point_flag = point_relation["flag"];
    const List span = specification["span"];
    span_id = span["id"];
    span_length = span["length"];
    span_start_fragment = span["start_fragment"];
    span_start_side = span["start_side"];
    span_end_fragment = span["end_fragment"];
    span_end_side = span["end_side"];
    span_closed = span["closed"];
    span_no_dip = span["no_dip"];
    span_reference = span["reference"];
    const List anchor = specification["anchor"];
    anchor_endpoint_id = anchor["endpoint_id"];
    anchor_fragment = anchor["fragment"];
    anchor_side = anchor["side"];
    anchor_distance = anchor["distance"];
    anchor_terrain_grade = anchor["terrain_grade"];
    const List crossing = specification["crossing"];
    crossing_id = crossing["crossing_id"];
    crossing_pair_id = crossing["pair_id"];
    crossing_lower_fragment = crossing["lower_fragment"];
    crossing_upper_fragment = crossing["upper_fragment"];
    crossing_lower_distance = crossing["lower_distance"];
    crossing_upper_distance = crossing["upper_distance"];
    crossing_lower_rank = crossing["lower_rank"];
    crossing_upper_rank = crossing["upper_rank"];
    crossing_clearance = crossing["clearance"];
    const List junction = specification["junction"];
    junction_id = junction["junction_id"];
    junction_pair_id = junction["pair_id"];
    junction_fragment_a = junction["fragment_a"];
    junction_fragment_b = junction["fragment_b"];
    junction_distance_a = junction["distance_a"];
    junction_distance_b = junction["distance_b"];
    const List overlap = specification["overlap"];
    overlap_id = overlap["overlap_id"];
    overlap_lower_fragment = overlap["lower_fragment"];
    overlap_upper_fragment = overlap["upper_fragment"];
    overlap_lower_start = overlap["lower_start"];
    overlap_lower_end = overlap["lower_end"];
    overlap_upper_start = overlap["upper_start"];
    overlap_upper_end = overlap["upper_end"];
    overlap_clearance = overlap["clearance"];
    const List continuation = specification["continuation"];
    continuation_id = continuation["continuation_id"];
    continuation_fragment_a = continuation["fragment_a"];
    continuation_fragment_b = continuation["fragment_b"];
    continuation_distance_a = continuation["distance_a"];
    continuation_distance_b = continuation["distance_b"];
    continuation_sign_a = continuation["sign_a"];
    continuation_sign_b = continuation["sign_b"];
    continuation_gap = continuation["gap"];
    continuation_exact = continuation["exact"];
    const NumericVector settings = specification["settings"];
    maximum_grade = settings["maximum_grade"];
    maximum_grade_rate = settings["maximum_grade_rate"];
    curvature_weight = settings["curvature_weight"];
    grade_weight = settings["grade_weight"];
    terrain_reference_weight = settings["terrain_reference_weight"];
    underground_reference_depth = settings["underground_reference_depth"];
    underground_reference_weight = settings["underground_reference_weight"];
    anchor_grade_weight = settings["anchor_grade_weight"];
    uplift_weight = settings["uplift_weight"];
    control_tolerance = settings["control_tolerance"];
  }
};

struct AdaptiveCompilerInput {
  IntegerVector type;
  IntegerVector fragment_a;
  NumericVector distance_a;
  IntegerVector fragment_b;
  NumericVector distance_b;
  IntegerVector event_id;
  NumericVector clearance;

  explicit AdaptiveCompilerInput(const List& adaptive)
      : type(adaptive["type"]),
        fragment_a(adaptive["fragment_a"]),
        distance_a(adaptive["distance_a"]),
        fragment_b(adaptive["fragment_b"]),
        distance_b(adaptive["distance_b"]),
        event_id(adaptive["event_id"]),
        clearance(adaptive["clearance"]) {}
};

struct CompilerControls {
  std::vector<int> start;
  std::vector<int> count;
  std::vector<int> fragment;
  std::vector<double> distance;
  std::vector<double> tolerance;
  std::vector<double> terrain;
  std::vector<double> span_station;
  std::vector<bool> endpoint;
  std::vector<bool> crossing;
  std::vector<bool> junction;
  std::vector<bool> conflict;
  std::vector<bool> overlap;
  std::vector<bool> adaptive;
  std::vector<double> station_weight;
};

struct SupportArc {
  int id;
  int span;
  int start_control;
  int end_control;
  double start_station;
  double end_station;
  double arc_length;
  double span_length;
  bool closed;
};

struct ResolvedArc {
  const SupportArc* arc;
  double fraction;
};

struct ConstraintCompiler {
  std::vector<int> a_i;
  std::vector<int> a_j;
  std::vector<double> a_x;
  std::vector<double> lower;
  std::vector<double> upper;
  std::vector<int> type;
  std::vector<int> component;
  std::vector<int> fragment_a;
  std::vector<int> fragment_b;
  std::vector<int> event_id;
  std::vector<double> clearance;
  std::vector<double> distance_a;
  std::vector<double> distance_b;

  void reserve(std::size_t constraints, std::size_t nonzero) {
    a_i.reserve(nonzero);
    a_j.reserve(nonzero);
    a_x.reserve(nonzero);
    lower.reserve(constraints);
    upper.reserve(constraints);
    type.reserve(constraints);
    component.reserve(constraints);
    fragment_a.reserve(constraints);
    fragment_b.reserve(constraints);
    event_id.reserve(constraints);
    clearance.reserve(constraints);
    distance_a.reserve(constraints);
    distance_b.reserve(constraints);
  }

  void add(
      std::initializer_list<int> index,
      std::initializer_list<double> value,
      double lower_value,
      double upper_value,
      int type_value,
      int component_value,
      int fragment_a_value = NA_INTEGER,
      int fragment_b_value = NA_INTEGER,
      int event_id_value = NA_INTEGER,
      double clearance_value = NA_REAL,
      double distance_a_value = NA_REAL,
      double distance_b_value = NA_REAL) {
    const int row = static_cast<int>(lower.size()) + 1;
    auto value_iterator = value.begin();
    for (const int variable : index) {
      a_i.push_back(row);
      a_j.push_back(variable + 1);
      a_x.push_back(*value_iterator++);
    }
    lower.push_back(lower_value);
    upper.push_back(upper_value);
    type.push_back(type_value);
    component.push_back(component_value);
    fragment_a.push_back(fragment_a_value);
    fragment_b.push_back(fragment_b_value);
    event_id.push_back(event_id_value);
    clearance.push_back(clearance_value);
    distance_a.push_back(distance_a_value);
    distance_b.push_back(distance_b_value);
  }
};

struct CurvatureTerm {
  int grade_a;
  int grade_b;
  double sign_a;
  double sign_b;
  double length;
};

struct ObjectiveCompiler {
  std::vector<int> i;
  std::vector<int> j;
  std::vector<double> x;

  void add(int row, int column, double value) {
    i.push_back(row + 1);
    j.push_back(column + 1);
    x.push_back(value);
  }
};

inline bool valid_fragment(int fragment, int fragment_count) {
  return fragment != NA_INTEGER && fragment >= 0 && fragment < fragment_count;
}

double interpolate_compiler_terrain(
    const CompilerSpecification& specification,
    int fragment,
    double distance) {
  const int start = specification.terrain_start[fragment];
  const int count = specification.terrain_count[fragment];
  const double* first = specification.terrain_distance.begin() + start;
  const double* last = first + count;
  if (distance <= *first) {
    return specification.terrain_elevation[start];
  }
  if (distance >= *(last - 1)) {
    return specification.terrain_elevation[start + count - 1];
  }
  const double* upper = std::upper_bound(first, last, distance);
  const int right = static_cast<int>(upper - first);
  const int left = right - 1;
  const double fraction =
      (distance - first[left]) / (first[right] - first[left]);
  return specification.terrain_elevation[start + left] +
         fraction *
             (specification.terrain_elevation[start + right] -
              specification.terrain_elevation[start + left]);
}

int match_compiler_control(
    const CompilerControls& controls,
    int fragment,
    double distance,
    const char* context) {
  const int start = controls.start[fragment];
  const int count = controls.count[fragment];
  const auto first = controls.distance.begin() + start;
  const auto last = first + count;
  const auto lower = std::lower_bound(first, last, distance);
  int match;
  if (lower == first) {
    match = start;
  } else if (lower == last) {
    match = start + count - 1;
  } else {
    const int right = start + static_cast<int>(lower - first);
    const int left = right - 1;
    match = std::abs(controls.distance[left] - distance) <=
                    std::abs(controls.distance[right] - distance)
                ? left
                : right;
  }
  const double separation = std::abs(controls.distance[match] - distance);
  if (!finite_number(separation) ||
      separation > controls.tolerance[match]) {
    stop(
        "No profile control matched fragment index %d at %.12g m for %s "
        "within %.3g m; nearest separation was %.3g m.",
        fragment,
        distance,
        context,
        controls.tolerance[match],
        separation);
  }
  return match;
}

ResolvedArc resolve_compiler_arc(
    const std::vector<SupportArc>& arcs,
    int span,
    double station) {
  const double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  const SupportArc* first_arc = nullptr;
  for (const SupportArc& arc : arcs) {
    if (arc.span == span) {
      first_arc = &arc;
      break;
    }
  }
  if (first_arc == nullptr) {
    stop("Profile span index %d has no support chord.", span);
  }
  double candidate[2] = {station, station};
  int candidate_count = 1;
  if (first_arc->closed) {
    double normalized = std::fmod(station, first_arc->span_length);
    if (normalized < 0.0) {
      normalized += first_arc->span_length;
    }
    candidate[0] = normalized;
    candidate[1] = normalized + first_arc->span_length;
    candidate_count = 2;
  }
  for (int candidate_index = 0;
       candidate_index < candidate_count;
       ++candidate_index) {
    for (const SupportArc& arc : arcs) {
      if (arc.span == span &&
          candidate[candidate_index] >= arc.start_station - tolerance &&
          candidate[candidate_index] < arc.end_station - tolerance) {
        return {
            &arc,
            (candidate[candidate_index] - arc.start_station) /
                arc.arc_length};
      }
    }
  }
  const double terminal = candidate[candidate_count - 1];
  const SupportArc* terminal_arc = nullptr;
  for (const SupportArc& arc : arcs) {
    if (arc.span == span &&
        std::abs(terminal - arc.end_station) <= tolerance) {
      terminal_arc = &arc;
    }
  }
  if (terminal_arc == nullptr) {
    stop("Station %.12g is outside profile span index %d.", station, span);
  }
  return {
      terminal_arc,
      (terminal - terminal_arc->start_station) /
          terminal_arc->arc_length};
}

List compiler_controls_list(
    const CompilerSpecification& specification,
    const CompilerControls& controls) {
  const int count = static_cast<int>(controls.distance.size());
  IntegerVector fragment_id(count);
  IntegerVector feature_id(count);
  IntegerVector component_id(count);
  NumericVector layer(count);
  IntegerVector span_id(count);
  IntegerVector height_variable(count);
  IntegerVector grade_variable(count);
  for (int control = 0; control < count; ++control) {
    const int fragment = controls.fragment[control];
    fragment_id[control] = specification.fragment_id[fragment];
    feature_id[control] = specification.feature_id[fragment];
    component_id[control] = specification.component_id[fragment];
    layer[control] = specification.layer[fragment];
    span_id[control] = specification.fragment_span_id[fragment];
    height_variable[control] = control + 1;
    grade_variable[control] = count + control + 1;
  }
  return List::create(
      _["fragment_id"] = fragment_id,
      _["feature_id"] = feature_id,
      _["component_id"] = component_id,
      _["distance"] = wrap(controls.distance),
      _["tolerance"] = wrap(controls.tolerance),
      _["terrain"] = wrap(controls.terrain),
      _["layer"] = layer,
      _["span_id"] = span_id,
      _["span_station"] = wrap(controls.span_station),
      _["endpoint"] = wrap(controls.endpoint),
      _["crossing"] = wrap(controls.crossing),
      _["junction"] = wrap(controls.junction),
      _["conflict"] = wrap(controls.conflict),
      _["overlap"] = wrap(controls.overlap),
      _["adaptive"] = wrap(controls.adaptive),
      _["station_weight"] = wrap(controls.station_weight),
      _["height_variable"] = height_variable,
      _["grade_variable"] = grade_variable);
}

}  // namespace

// The compiler consumes only ordinary, immutable R vectors. Dense fragment
// indices and variables are zero-based internally. Stable IDs are retained for
// diagnostics, while every sparse triplet index returned to R is one-based.
// Distances, elevations, clearances, grades, and weights use existing solver
// units.
//
// [[Rcpp::export]]
List compile_render_road_profile_problem_cpp(
    List specification_list,
    List adaptive_list) {
  const CompilerSpecification specification(specification_list);
  const AdaptiveCompilerInput adaptive(adaptive_list);
  const int fragment_count = specification.fragment_id.size();
  const int span_count = specification.span_id.size();

  std::vector<std::vector<double>> station(fragment_count);
  for (int fragment = 0; fragment < fragment_count; ++fragment) {
    station[fragment].reserve(8);
    station[fragment].push_back(0.0);
    station[fragment].push_back(specification.fragment_length[fragment]);
  }
  for (R_xlen_t control = 0;
       control < specification.initial_fragment.size();
       ++control) {
    const int fragment = specification.initial_fragment[control];
    if (valid_fragment(fragment, fragment_count)) {
      station[fragment].push_back(specification.initial_distance[control]);
    }
  }
  for (R_xlen_t relation = 0;
       relation < specification.overlap_id.size();
       ++relation) {
    const int lower = specification.overlap_lower_fragment[relation];
    const int upper = specification.overlap_upper_fragment[relation];
    station[lower].push_back(specification.overlap_lower_start[relation]);
    station[lower].push_back(specification.overlap_lower_end[relation]);
    station[upper].push_back(specification.overlap_upper_start[relation]);
    station[upper].push_back(specification.overlap_upper_end[relation]);
  }
  for (R_xlen_t request = 0; request < adaptive.type.size(); ++request) {
    if (valid_fragment(adaptive.fragment_a[request], fragment_count)) {
      station[adaptive.fragment_a[request]].push_back(
          adaptive.distance_a[request]);
    }
    if (valid_fragment(adaptive.fragment_b[request], fragment_count)) {
      station[adaptive.fragment_b[request]].push_back(
          adaptive.distance_b[request]);
    }
  }

  CompilerControls controls;
  controls.start.resize(fragment_count);
  controls.count.resize(fragment_count);
  std::size_t estimated_controls = 0;
  for (const auto& fragment_station : station) {
    estimated_controls += fragment_station.size();
  }
  controls.fragment.reserve(estimated_controls);
  controls.distance.reserve(estimated_controls);
  controls.tolerance.reserve(estimated_controls);
  controls.terrain.reserve(estimated_controls);
  controls.span_station.reserve(estimated_controls);
  controls.endpoint.reserve(estimated_controls);
  for (int fragment = 0; fragment < fragment_count; ++fragment) {
    if ((fragment & 127) == 0) {
      checkUserInterrupt();
    }
    std::vector<double>& fragment_station = station[fragment];
    fragment_station.erase(
        std::remove_if(
            fragment_station.begin(),
            fragment_station.end(),
            [](double value) { return !finite_number(value); }),
        fragment_station.end());
    const double fragment_length =
        specification.fragment_length[fragment];
    for (double& value : fragment_station) {
      value = std::min(std::max(value, 0.0), fragment_length);
    }
    std::sort(fragment_station.begin(), fragment_station.end());
    const double tolerance = std::max(
        specification.control_tolerance,
        fragment_length * 1e-10);
    std::vector<double> deduplicated;
    deduplicated.reserve(fragment_station.size());
    for (std::size_t index = 0;
         index < fragment_station.size();
         ++index) {
      if (index == 0 ||
          fragment_station[index] - fragment_station[index - 1] >
              tolerance) {
        deduplicated.push_back(fragment_station[index]);
      }
    }
    if (deduplicated.size() < 2) {
      stop("Every fragment requires at least two distinct controls.");
    }
    controls.start[fragment] =
        static_cast<int>(controls.distance.size());
    controls.count[fragment] =
        static_cast<int>(deduplicated.size());
    for (const double value : deduplicated) {
      controls.fragment.push_back(fragment);
      controls.distance.push_back(value);
      controls.tolerance.push_back(tolerance);
      controls.terrain.push_back(
          interpolate_compiler_terrain(specification, fragment, value));
      controls.span_station.push_back(
          specification.fragment_span_offset[fragment] +
          (specification.fragment_span_orientation[fragment] == 1
               ? value
               : fragment_length - value));
      controls.endpoint.push_back(
          value <= tolerance ||
          value >= fragment_length - tolerance);
    }
  }
  const int control_count = static_cast<int>(controls.distance.size());
  controls.crossing.assign(control_count, false);
  controls.junction.assign(control_count, false);
  controls.conflict.assign(control_count, false);
  controls.overlap.assign(control_count, false);
  controls.adaptive.assign(control_count, false);
  controls.station_weight.assign(control_count, 0.0);

  for (R_xlen_t relation = 0;
       relation < specification.point_pair_id.size();
       ++relation) {
    const int control_a = match_compiler_control(
        controls,
        specification.point_fragment_a[relation],
        specification.point_distance_a[relation],
        "point relation side a");
    const int control_b = match_compiler_control(
        controls,
        specification.point_fragment_b[relation],
        specification.point_distance_b[relation],
        "point relation side b");
    const int flag = specification.point_flag[relation];
    if (flag == point_crossing_flag) {
      controls.crossing[control_a] = true;
      controls.crossing[control_b] = true;
    } else if (flag == point_junction_flag) {
      controls.junction[control_a] = true;
      controls.junction[control_b] = true;
    } else if (flag == point_conflict_flag) {
      controls.conflict[control_a] = true;
      controls.conflict[control_b] = true;
    }
  }
  for (R_xlen_t relation = 0;
       relation < specification.overlap_id.size();
       ++relation) {
    const int lower = specification.overlap_lower_fragment[relation];
    const int upper = specification.overlap_upper_fragment[relation];
    controls.overlap[match_compiler_control(
        controls,
        lower,
        specification.overlap_lower_start[relation],
        "overlap lower start")] = true;
    controls.overlap[match_compiler_control(
        controls,
        lower,
        specification.overlap_lower_end[relation],
        "overlap lower end")] = true;
    controls.overlap[match_compiler_control(
        controls,
        upper,
        specification.overlap_upper_start[relation],
        "overlap upper start")] = true;
    controls.overlap[match_compiler_control(
        controls,
        upper,
        specification.overlap_upper_end[relation],
        "overlap upper end")] = true;
  }
  for (R_xlen_t request = 0; request < adaptive.type.size(); ++request) {
    controls.adaptive[match_compiler_control(
        controls,
        adaptive.fragment_a[request],
        adaptive.distance_a[request],
        "adaptive request side a")] = true;
    if (valid_fragment(adaptive.fragment_b[request], fragment_count)) {
      controls.adaptive[match_compiler_control(
          controls,
          adaptive.fragment_b[request],
          adaptive.distance_b[request],
          "adaptive request side b")] = true;
    }
  }
  for (int fragment = 0; fragment < fragment_count; ++fragment) {
    const int start = controls.start[fragment];
    const int count = controls.count[fragment];
    for (int local = 0; local < count - 1; ++local) {
      const int first = start + local;
      const int second = first + 1;
      const double length =
          controls.distance[second] - controls.distance[first];
      controls.station_weight[first] += length / 2.0;
      controls.station_weight[second] += length / 2.0;
    }
  }
  for (R_xlen_t relation = 0;
       relation < specification.continuation_id.size();
       ++relation) {
    const double gap = specification.continuation_gap[relation];
    if (!finite_number(gap) || gap <= 0.0) {
      continue;
    }
    const int control_a = match_compiler_control(
        controls,
        specification.continuation_fragment_a[relation],
        specification.continuation_distance_a[relation],
        "continuation side a");
    const int control_b = match_compiler_control(
        controls,
        specification.continuation_fragment_b[relation],
        specification.continuation_distance_b[relation],
        "continuation side b");
    controls.station_weight[control_a] += gap / 2.0;
    controls.station_weight[control_b] += gap / 2.0;
  }
  for (const double weight : controls.station_weight) {
    if (!finite_number(weight) || weight <= 0.0) {
      stop("Every road-profile control requires a positive station weight.");
    }
  }

  std::vector<int> span_start_control(span_count, NA_INTEGER);
  std::vector<int> span_end_control(span_count, NA_INTEGER);
  std::vector<int> span_periodic_control(span_count, NA_INTEGER);
  std::vector<SupportArc> arcs;
  arcs.reserve(span_count + specification.overlap_id.size() * 2);
  for (int span = 0; span < span_count; ++span) {
    if ((span & 127) == 0) {
      checkUserInterrupt();
    }
    if (specification.span_closed[span]) {
      std::vector<int> support;
      for (int control = 0; control < control_count; ++control) {
        if (specification.fragment_span_id[controls.fragment[control]] ==
                specification.span_id[span] &&
            (controls.crossing[control] || controls.overlap[control])) {
          support.push_back(control);
        }
      }
      if (support.empty()) {
        for (int control = 0; control < control_count; ++control) {
          if (specification.fragment_span_id[controls.fragment[control]] ==
                  specification.span_id[span] &&
              controls.endpoint[control]) {
            support.push_back(control);
          }
        }
      }
      if (support.empty()) {
        stop("A closed profile span has no periodic support control.");
      }
      std::sort(
          support.begin(),
          support.end(),
          [&controls](int first, int second) {
            if (controls.span_station[first] ==
                controls.span_station[second]) {
              return first < second;
            }
            return controls.span_station[first] <
                   controls.span_station[second];
          });
      std::vector<int> deduplicated;
      double prior_key = NA_REAL;
      for (const int control : support) {
        const double key = std::nearbyint(
            controls.span_station[control] /
            specification.control_tolerance);
        if (deduplicated.empty() || key != prior_key) {
          deduplicated.push_back(control);
          prior_key = key;
        }
      }
      support.swap(deduplicated);
      span_start_control[span] = support.front();
      span_end_control[span] = support.front();
      if (support.size() == 1) {
        span_periodic_control[span] = support.front();
        arcs.push_back({
            static_cast<int>(arcs.size()),
            span,
            support.front(),
            support.front(),
            controls.span_station[support.front()],
            controls.span_station[support.front()] +
                specification.span_length[span],
            specification.span_length[span],
            specification.span_length[span],
            true});
      } else {
        for (std::size_t index = 0; index < support.size(); ++index) {
          const std::size_t next = (index + 1) % support.size();
          const double start_station =
              controls.span_station[support[index]];
          double end_station = controls.span_station[support[next]];
          if (next == 0) {
            end_station += specification.span_length[span];
          }
          arcs.push_back({
              static_cast<int>(arcs.size()),
              span,
              support[index],
              support[next],
              start_station,
              end_station,
              end_station - start_station,
              specification.span_length[span],
              true});
        }
      }
    } else {
      const int start_fragment =
          specification.span_start_fragment[span];
      const int end_fragment = specification.span_end_fragment[span];
      const double start_distance =
          specification.span_start_side[span] == 0
              ? 0.0
              : specification.fragment_length[start_fragment];
      const double end_distance =
          specification.span_end_side[span] == 0
              ? 0.0
              : specification.fragment_length[end_fragment];
      span_start_control[span] = match_compiler_control(
          controls,
          start_fragment,
          start_distance,
          "span start");
      span_end_control[span] = match_compiler_control(
          controls,
          end_fragment,
          end_distance,
          "span end");
      arcs.push_back({
          static_cast<int>(arcs.size()),
          span,
          span_start_control[span],
          span_end_control[span],
          0.0,
          specification.span_length[span],
          specification.span_length[span],
          specification.span_length[span],
          false});
    }
  }

  ConstraintCompiler constraint;
  constraint.reserve(
      static_cast<std::size_t>(control_count) * 4,
      static_cast<std::size_t>(control_count) * 8);
  std::vector<int> interval_component;
  std::vector<int> interval_fragment;
  std::vector<int> interval_control_a;
  std::vector<int> interval_control_b;
  std::vector<double> interval_length;
  std::vector<CurvatureTerm> interval_curvature;
  interval_component.reserve(control_count - fragment_count);
  interval_fragment.reserve(control_count - fragment_count);
  interval_control_a.reserve(control_count - fragment_count);
  interval_control_b.reserve(control_count - fragment_count);
  interval_length.reserve(control_count - fragment_count);
  interval_curvature.reserve(control_count - fragment_count);
  for (int fragment = 0; fragment < fragment_count; ++fragment) {
    const int start = controls.start[fragment];
    const int count = controls.count[fragment];
    for (int local = 0; local < count - 1; ++local) {
      const int first = start + local;
      const int second = first + 1;
      const double length =
          controls.distance[second] - controls.distance[first];
      if (!finite_number(length) || length <= 0.0) {
        stop(
            "Fragment %d contains a non-positive control interval.",
            specification.fragment_id[fragment]);
      }
      const int component = specification.component_id[fragment];
      interval_component.push_back(component);
      interval_fragment.push_back(specification.fragment_id[fragment]);
      interval_control_a.push_back(first + 1);
      interval_control_b.push_back(second + 1);
      interval_length.push_back(length);
      constraint.add(
          {first, second, control_count + first, control_count + second},
          {-1.0, 1.0, -length / 2.0, -length / 2.0},
          0.0,
          0.0,
          constraint_quadratic_interval,
          component,
          specification.fragment_id[fragment],
          NA_INTEGER,
          NA_INTEGER,
          NA_REAL,
          controls.distance[first],
          controls.distance[second]);
      constraint.add(
          {control_count + first, control_count + second},
          {-1.0, 1.0},
          -specification.maximum_grade_rate * length,
          specification.maximum_grade_rate * length,
          constraint_grade_rate,
          component,
          specification.fragment_id[fragment],
          NA_INTEGER,
          NA_INTEGER,
          NA_REAL,
          controls.distance[first],
          controls.distance[second]);
      interval_curvature.push_back({
          control_count + first,
          control_count + second,
          1.0,
          1.0,
          length});
    }
  }
  for (int control = 0; control < control_count; ++control) {
    const int fragment = controls.fragment[control];
    const int external_fragment = specification.fragment_id[fragment];
    const int component = specification.component_id[fragment];
    constraint.add(
        {control_count + control},
        {1.0},
        -specification.maximum_grade,
        specification.maximum_grade,
        constraint_grade_bound,
        component,
        external_fragment,
        NA_INTEGER,
        NA_INTEGER,
        NA_REAL,
        controls.distance[control],
        NA_REAL);
    if (!specification.underground[fragment]) {
      constraint.add(
          {control},
          {1.0},
          controls.terrain[control],
          R_PosInf,
          constraint_terrain_floor,
          component,
          external_fragment,
          NA_INTEGER,
          NA_INTEGER,
          NA_REAL,
          controls.distance[control],
          NA_REAL);
    }
  }

  std::vector<int> anchor_control;
  std::vector<double> anchor_terrain;
  std::vector<int> anchor_component;
  anchor_control.reserve(specification.anchor_endpoint_id.size());
  anchor_terrain.reserve(specification.anchor_endpoint_id.size());
  anchor_component.reserve(specification.anchor_endpoint_id.size());
  for (R_xlen_t anchor = 0;
       anchor < specification.anchor_endpoint_id.size();
       ++anchor) {
    const int fragment = specification.anchor_fragment[anchor];
    const int control = match_compiler_control(
        controls,
        fragment,
        specification.anchor_distance[anchor],
        "ground anchor");
    const int component = specification.component_id[fragment];
    anchor_control.push_back(control);
    anchor_terrain.push_back(controls.terrain[control]);
    anchor_component.push_back(component);
    constraint.add(
        {control},
        {1.0},
        controls.terrain[control],
        R_PosInf,
        constraint_ground_anchor,
        component,
        specification.fragment_id[fragment],
        NA_INTEGER,
        NA_INTEGER,
        NA_REAL,
        specification.anchor_distance[anchor],
        NA_REAL);
  }

  std::vector<int> clearance_type;
  std::vector<int> clearance_event_id;
  std::vector<int> clearance_pair_id;
  std::vector<int> clearance_lower_fragment;
  std::vector<int> clearance_upper_fragment;
  std::vector<int> clearance_lower_control;
  std::vector<int> clearance_upper_control;
  std::vector<double> clearance_lower_distance;
  std::vector<double> clearance_upper_distance;
  std::vector<double> clearance_lower_rank;
  std::vector<double> clearance_upper_rank;
  std::vector<double> clearance_value;
  std::vector<int> clearance_component;
  auto add_clearance_metadata = [&](
      int type,
      int event,
      int pair,
      int lower_fragment,
      int upper_fragment,
      int lower_control,
      int upper_control,
      double lower_distance,
      double upper_distance,
      double lower_rank,
      double upper_rank,
      double clearance,
      int component) {
    clearance_type.push_back(type);
    clearance_event_id.push_back(event);
    clearance_pair_id.push_back(pair);
    clearance_lower_fragment.push_back(lower_fragment);
    clearance_upper_fragment.push_back(upper_fragment);
    clearance_lower_control.push_back(lower_control + 1);
    clearance_upper_control.push_back(upper_control + 1);
    clearance_lower_distance.push_back(lower_distance);
    clearance_upper_distance.push_back(upper_distance);
    clearance_lower_rank.push_back(lower_rank);
    clearance_upper_rank.push_back(upper_rank);
    clearance_value.push_back(clearance);
    clearance_component.push_back(component);
  };

  for (R_xlen_t crossing = 0;
       crossing < specification.crossing_id.size();
       ++crossing) {
    const int lower_fragment =
        specification.crossing_lower_fragment[crossing];
    const int upper_fragment =
        specification.crossing_upper_fragment[crossing];
    const int lower_control = match_compiler_control(
        controls,
        lower_fragment,
        specification.crossing_lower_distance[crossing],
        "crossing lower");
    const int upper_control = match_compiler_control(
        controls,
        upper_fragment,
        specification.crossing_upper_distance[crossing],
        "crossing upper");
    const int component = specification.component_id[upper_fragment];
    if (component != specification.component_id[lower_fragment]) {
      stop("A crossing constraint spans solve components.");
    }
    constraint.add(
        {lower_control, upper_control},
        {-1.0, 1.0},
        specification.crossing_clearance[crossing],
        R_PosInf,
        constraint_crossing_clearance,
        component,
        specification.fragment_id[lower_fragment],
        specification.fragment_id[upper_fragment],
        specification.crossing_id[crossing],
        specification.crossing_clearance[crossing],
        specification.crossing_lower_distance[crossing],
        specification.crossing_upper_distance[crossing]);
    add_clearance_metadata(
        1,
        specification.crossing_id[crossing],
        specification.crossing_pair_id[crossing],
        specification.fragment_id[lower_fragment],
        specification.fragment_id[upper_fragment],
        lower_control,
        upper_control,
        specification.crossing_lower_distance[crossing],
        specification.crossing_upper_distance[crossing],
        specification.crossing_lower_rank[crossing],
        specification.crossing_upper_rank[crossing],
        specification.crossing_clearance[crossing],
        component);
  }

  std::vector<int> junction_control_a;
  std::vector<int> junction_control_b;
  std::vector<int> junction_component;
  junction_control_a.reserve(specification.junction_id.size());
  junction_control_b.reserve(specification.junction_id.size());
  junction_component.reserve(specification.junction_id.size());
  for (R_xlen_t junction = 0;
       junction < specification.junction_id.size();
       ++junction) {
    const int fragment_a = specification.junction_fragment_a[junction];
    const int fragment_b = specification.junction_fragment_b[junction];
    const int control_a = match_compiler_control(
        controls,
        fragment_a,
        specification.junction_distance_a[junction],
        "junction side a");
    const int control_b = match_compiler_control(
        controls,
        fragment_b,
        specification.junction_distance_b[junction],
        "junction side b");
    const int component = specification.component_id[fragment_a];
    if (component != specification.component_id[fragment_b]) {
      stop("A junction constraint spans solve components.");
    }
    junction_control_a.push_back(control_a + 1);
    junction_control_b.push_back(control_b + 1);
    junction_component.push_back(component);
    constraint.add(
        {control_a, control_b},
        {-1.0, 1.0},
        0.0,
        0.0,
        constraint_junction_height,
        component,
        specification.fragment_id[fragment_a],
        specification.fragment_id[fragment_b],
        specification.junction_id[junction],
        NA_REAL,
        specification.junction_distance_a[junction],
        specification.junction_distance_b[junction]);
  }

  for (R_xlen_t overlap = 0;
       overlap < specification.overlap_id.size();
       ++overlap) {
    const int lower_fragment =
        specification.overlap_lower_fragment[overlap];
    const int upper_fragment =
        specification.overlap_upper_fragment[overlap];
    const int component = specification.component_id[upper_fragment];
    if (component != specification.component_id[lower_fragment]) {
      stop("An overlap constraint spans solve components.");
    }
    const double lower_distance[2] = {
        specification.overlap_lower_start[overlap],
        specification.overlap_lower_end[overlap]};
    const double upper_distance[2] = {
        specification.overlap_upper_start[overlap],
        specification.overlap_upper_end[overlap]};
    for (int endpoint = 0; endpoint < 2; ++endpoint) {
      const int lower_control = match_compiler_control(
          controls,
          lower_fragment,
          lower_distance[endpoint],
          "overlap lower endpoint");
      const int upper_control = match_compiler_control(
          controls,
          upper_fragment,
          upper_distance[endpoint],
          "overlap upper endpoint");
      constraint.add(
          {lower_control, upper_control},
          {-1.0, 1.0},
          specification.overlap_clearance[overlap],
          R_PosInf,
          constraint_overlap_clearance,
          component,
          specification.fragment_id[lower_fragment],
          specification.fragment_id[upper_fragment],
          specification.overlap_id[overlap],
          specification.overlap_clearance[overlap],
          lower_distance[endpoint],
          upper_distance[endpoint]);
      add_clearance_metadata(
          endpoint == 0 ? 2 : 3,
          specification.overlap_id[overlap],
          NA_INTEGER,
          specification.fragment_id[lower_fragment],
          specification.fragment_id[upper_fragment],
          lower_control,
          upper_control,
          lower_distance[endpoint],
          upper_distance[endpoint],
          NA_REAL,
          NA_REAL,
          specification.overlap_clearance[overlap],
          component);
    }
  }
  for (R_xlen_t request = 0; request < adaptive.type.size(); ++request) {
    if (adaptive.type[request] != overlap_clearance_request) {
      continue;
    }
    const int lower_fragment = adaptive.fragment_a[request];
    const int upper_fragment = adaptive.fragment_b[request];
    const int lower_control = match_compiler_control(
        controls,
        lower_fragment,
        adaptive.distance_a[request],
        "adaptive overlap lower");
    const int upper_control = match_compiler_control(
        controls,
        upper_fragment,
        adaptive.distance_b[request],
        "adaptive overlap upper");
    const int component = specification.component_id[upper_fragment];
    if (component != specification.component_id[lower_fragment]) {
      stop("An adaptive overlap constraint spans solve components.");
    }
    constraint.add(
        {lower_control, upper_control},
        {-1.0, 1.0},
        adaptive.clearance[request],
        R_PosInf,
        constraint_overlap_clearance_adaptive,
        component,
        specification.fragment_id[lower_fragment],
        specification.fragment_id[upper_fragment],
        adaptive.event_id[request],
        adaptive.clearance[request],
        adaptive.distance_a[request],
        adaptive.distance_b[request]);
    add_clearance_metadata(
        4,
        adaptive.event_id[request],
        NA_INTEGER,
        specification.fragment_id[lower_fragment],
        specification.fragment_id[upper_fragment],
        lower_control,
        upper_control,
        adaptive.distance_a[request],
        adaptive.distance_b[request],
        NA_REAL,
        NA_REAL,
        adaptive.clearance[request],
        component);
  }

  std::vector<int> continuation_control_a;
  std::vector<int> continuation_control_b;
  std::vector<int> continuation_component;
  std::vector<CurvatureTerm> continuation_curvature;
  continuation_control_a.reserve(specification.continuation_id.size());
  continuation_control_b.reserve(specification.continuation_id.size());
  continuation_component.reserve(specification.continuation_id.size());
  continuation_curvature.reserve(specification.continuation_id.size());
  for (R_xlen_t continuation = 0;
       continuation < specification.continuation_id.size();
       ++continuation) {
    const int fragment_a =
        specification.continuation_fragment_a[continuation];
    const int fragment_b =
        specification.continuation_fragment_b[continuation];
    const int control_a = match_compiler_control(
        controls,
        fragment_a,
        specification.continuation_distance_a[continuation],
        "continuation side a");
    const int control_b = match_compiler_control(
        controls,
        fragment_b,
        specification.continuation_distance_b[continuation],
        "continuation side b");
    const int component = specification.component_id[fragment_a];
    if (component != specification.component_id[fragment_b]) {
      stop("A continuation constraint spans solve components.");
    }
    continuation_control_a.push_back(control_a + 1);
    continuation_control_b.push_back(control_b + 1);
    continuation_component.push_back(component);
    const double sign_a = specification.continuation_sign_a[continuation];
    const double sign_b = specification.continuation_sign_b[continuation];
    const double gap = specification.continuation_gap[continuation];
    if (specification.continuation_exact[continuation]) {
      constraint.add(
          {control_a, control_b},
          {-1.0, 1.0},
          0.0,
          0.0,
          constraint_continuation_height,
          component,
          specification.fragment_id[fragment_a],
          specification.fragment_id[fragment_b],
          specification.continuation_id[continuation],
          NA_REAL,
          specification.continuation_distance_a[continuation],
          specification.continuation_distance_b[continuation]);
      constraint.add(
          {control_count + control_a, control_count + control_b},
          {sign_a, -sign_b},
          0.0,
          0.0,
          constraint_continuation_grade,
          component,
          specification.fragment_id[fragment_a],
          specification.fragment_id[fragment_b],
          specification.continuation_id[continuation],
          NA_REAL,
          specification.continuation_distance_a[continuation],
          specification.continuation_distance_b[continuation]);
    } else {
      constraint.add(
          {
              control_a,
              control_b,
              control_count + control_a,
              control_count + control_b},
          {
              -1.0,
              1.0,
              -gap * sign_a / 2.0,
              -gap * sign_b / 2.0},
          0.0,
          0.0,
          constraint_continuation_gap_interval,
          component,
          specification.fragment_id[fragment_a],
          specification.fragment_id[fragment_b],
          specification.continuation_id[continuation],
          NA_REAL,
          specification.continuation_distance_a[continuation],
          specification.continuation_distance_b[continuation]);
      constraint.add(
          {control_count + control_a, control_count + control_b},
          {-sign_a, sign_b},
          -specification.maximum_grade_rate * gap,
          specification.maximum_grade_rate * gap,
          constraint_continuation_gap_grade_rate,
          component,
          specification.fragment_id[fragment_a],
          specification.fragment_id[fragment_b],
          specification.continuation_id[continuation],
          NA_REAL,
          specification.continuation_distance_a[continuation],
          specification.continuation_distance_b[continuation]);
      continuation_curvature.push_back({
          control_count + control_a,
          control_count + control_b,
          sign_a,
          sign_b,
          gap});
    }
  }

  std::vector<int> chord_span;
  std::vector<int> chord_arc;
  std::vector<int> chord_control;
  std::vector<int> chord_start_control;
  std::vector<int> chord_end_control;
  std::vector<double> chord_fraction;
  for (int span = 0; span < span_count; ++span) {
    if (!specification.span_no_dip[span]) {
      continue;
    }
    for (int control = 0; control < control_count; ++control) {
      if (specification.fragment_span_id[controls.fragment[control]] !=
          specification.span_id[span]) {
        continue;
      }
      const ResolvedArc resolved =
          resolve_compiler_arc(arcs, span, controls.span_station[control]);
      const int start_control = resolved.arc->start_control;
      const int end_control = resolved.arc->end_control;
      if (control == start_control || control == end_control) {
        continue;
      }
      constraint.add(
          {start_control, control, end_control},
          {-(1.0 - resolved.fraction), 1.0, -resolved.fraction},
          0.0,
          R_PosInf,
          constraint_no_dip_span_chord,
          specification.component_id[controls.fragment[control]],
          specification.fragment_id[controls.fragment[control]],
          NA_INTEGER,
          specification.span_id[span],
          NA_REAL,
          controls.distance[control],
          NA_REAL);
      chord_span.push_back(specification.span_id[span]);
      chord_arc.push_back(resolved.arc->id + 1);
      chord_control.push_back(control + 1);
      chord_start_control.push_back(start_control + 1);
      chord_end_control.push_back(end_control + 1);
      chord_fraction.push_back(resolved.fraction);
    }
  }

  std::vector<CurvatureTerm> curvature = interval_curvature;
  curvature.insert(
      curvature.end(),
      continuation_curvature.begin(),
      continuation_curvature.end());
  const int variable_count = control_count * 2;
  std::vector<double> objective_q(variable_count, 0.0);
  ObjectiveCompiler objective_p;
  objective_p.i.reserve(
      static_cast<std::size_t>(control_count) * 5 +
      curvature.size() * 4);
  objective_p.j.reserve(objective_p.i.capacity());
  objective_p.x.reserve(objective_p.i.capacity());
  for (int control = 0; control < control_count; ++control) {
    const int fragment = controls.fragment[control];
    const int span = specification.fragment_span_id[fragment] - 1;
    const int reference = specification.span_reference[span];
    const double station_weight = controls.station_weight[control];
    const int height_variable = control;
    const int grade_variable = control_count + control;
    const bool underground_reference = reference == 2;
    if (!underground_reference) {
      objective_q[height_variable] +=
          specification.uplift_weight * station_weight;
    }
    if (specification.grade_weight > 0.0) {
      objective_p.add(
          grade_variable,
          grade_variable,
          2.0 * specification.grade_weight * station_weight);
    }
    if (underground_reference) {
      const double weight =
          specification.underground_reference_weight * station_weight;
      const double reference_height =
          controls.terrain[control] -
          specification.underground_reference_depth;
      objective_p.add(height_variable, height_variable, 2.0 * weight);
      objective_q[height_variable] -=
          2.0 * weight * reference_height;
    } else if (
        specification.terrain_reference_weight > 0.0 &&
        (reference == 3 || reference == 4)) {
      const ResolvedArc resolved =
          resolve_compiler_arc(arcs, span, controls.span_station[control]);
      std::map<int, double> coefficient;
      coefficient[height_variable] += 1.0;
      coefficient[resolved.arc->start_control] -=
          1.0 - resolved.fraction;
      coefficient[resolved.arc->end_control] -= resolved.fraction;
      for (auto iterator = coefficient.begin();
           iterator != coefficient.end();) {
        if (std::abs(iterator->second) <= quadratic_tolerance) {
          iterator = coefficient.erase(iterator);
        } else {
          ++iterator;
        }
      }
      const double weight =
          specification.terrain_reference_weight * station_weight;
      for (const auto& first : coefficient) {
        for (const auto& second : coefficient) {
          objective_p.add(
              first.first,
              second.first,
              2.0 * weight * first.second * second.second);
        }
      }
    } else if (specification.terrain_reference_weight > 0.0) {
      const double weight =
          specification.terrain_reference_weight * station_weight;
      objective_p.add(height_variable, height_variable, 2.0 * weight);
      objective_q[height_variable] -=
          2.0 * weight * controls.terrain[control];
    }
  }
  if (specification.anchor_grade_weight > 0.0) {
    for (R_xlen_t anchor = 0;
         anchor < specification.anchor_endpoint_id.size();
         ++anchor) {
      const int variable = control_count + anchor_control[anchor];
      objective_p.add(
          variable,
          variable,
          2.0 * specification.anchor_grade_weight);
      objective_q[variable] -=
          2.0 * specification.anchor_grade_weight *
          specification.anchor_terrain_grade[anchor];
    }
  }
  if (specification.curvature_weight > 0.0) {
    for (const CurvatureTerm& term : curvature) {
      const int variable[2] = {term.grade_a, term.grade_b};
      const double coefficient[2] = {-term.sign_a, term.sign_b};
      const double weight = specification.curvature_weight / term.length;
      for (int first = 0; first < 2; ++first) {
        for (int second = 0; second < 2; ++second) {
          objective_p.add(
              variable[first],
              variable[second],
              2.0 * weight *
                  coefficient[first] * coefficient[second]);
        }
      }
    }
  }

  IntegerVector support_span(arcs.size());
  IntegerVector support_start(arcs.size());
  IntegerVector support_end(arcs.size());
  NumericVector support_start_station(arcs.size());
  NumericVector support_end_station(arcs.size());
  NumericVector support_length(arcs.size());
  NumericVector support_span_length(arcs.size());
  LogicalVector support_closed(arcs.size());
  IntegerVector support_id(arcs.size());
  for (std::size_t arc = 0; arc < arcs.size(); ++arc) {
    support_span[arc] = specification.span_id[arcs[arc].span];
    support_start[arc] = arcs[arc].start_control + 1;
    support_end[arc] = arcs[arc].end_control + 1;
    support_start_station[arc] = arcs[arc].start_station;
    support_end_station[arc] = arcs[arc].end_station;
    support_length[arc] = arcs[arc].arc_length;
    support_span_length[arc] = arcs[arc].span_length;
    support_closed[arc] = arcs[arc].closed;
    support_id[arc] = arcs[arc].id + 1;
  }
  IntegerVector span_start(span_count);
  IntegerVector span_end(span_count);
  IntegerVector span_periodic(span_count);
  for (int span = 0; span < span_count; ++span) {
    span_start[span] = span_start_control[span] + 1;
    span_end[span] = span_end_control[span] + 1;
    span_periodic[span] =
        span_periodic_control[span] == NA_INTEGER
            ? NA_INTEGER
            : span_periodic_control[span] + 1;
  }
  IntegerVector variable_component(variable_count);
  for (int control = 0; control < control_count; ++control) {
    const int component =
        specification.component_id[controls.fragment[control]];
    variable_component[control] = component;
    variable_component[control_count + control] = component;
  }
  IntegerVector curvature_grade_a(curvature.size());
  IntegerVector curvature_grade_b(curvature.size());
  NumericVector curvature_sign_a(curvature.size());
  NumericVector curvature_sign_b(curvature.size());
  NumericVector curvature_length(curvature.size());
  for (std::size_t term = 0; term < curvature.size(); ++term) {
    curvature_grade_a[term] = curvature[term].grade_a + 1;
    curvature_grade_b[term] = curvature[term].grade_b + 1;
    curvature_sign_a[term] = curvature[term].sign_a;
    curvature_sign_b[term] = curvature[term].sign_b;
    curvature_length[term] = curvature[term].length;
  }
  IntegerVector overlap_lower_id(specification.overlap_id.size());
  IntegerVector overlap_upper_id(specification.overlap_id.size());
  for (R_xlen_t relation = 0;
       relation < specification.overlap_id.size();
       ++relation) {
    overlap_lower_id[relation] = specification.fragment_id[
        specification.overlap_lower_fragment[relation]];
    overlap_upper_id[relation] = specification.fragment_id[
        specification.overlap_upper_fragment[relation]];
  }
  IntegerVector anchor_fragment_id(specification.anchor_endpoint_id.size());
  IntegerVector anchor_control_id(specification.anchor_endpoint_id.size());
  for (R_xlen_t anchor = 0;
       anchor < specification.anchor_endpoint_id.size();
       ++anchor) {
    anchor_fragment_id[anchor] =
        specification.fragment_id[specification.anchor_fragment[anchor]];
    anchor_control_id[anchor] = anchor_control[anchor] + 1;
  }
  IntegerVector continuation_fragment_a_id(
      specification.continuation_id.size());
  IntegerVector continuation_fragment_b_id(
      specification.continuation_id.size());
  for (R_xlen_t relation = 0;
       relation < specification.continuation_id.size();
       ++relation) {
    continuation_fragment_a_id[relation] = specification.fragment_id[
        specification.continuation_fragment_a[relation]];
    continuation_fragment_b_id[relation] = specification.fragment_id[
        specification.continuation_fragment_b[relation]];
  }
  IntegerVector junction_fragment_a_id(specification.junction_id.size());
  IntegerVector junction_fragment_b_id(specification.junction_id.size());
  for (R_xlen_t relation = 0;
       relation < specification.junction_id.size();
       ++relation) {
    junction_fragment_a_id[relation] =
        specification.fragment_id[specification.junction_fragment_a[relation]];
    junction_fragment_b_id[relation] =
        specification.fragment_id[specification.junction_fragment_b[relation]];
  }
  const int constraint_count = constraint.lower.size();
  IntegerVector constraint_id(constraint_count);
  for (int row = 0; row < constraint_count; ++row) {
    constraint_id[row] = row + 1;
  }
  IntegerVector interval_id(interval_length.size());
  for (R_xlen_t row = 0; row < interval_id.size(); ++row) {
    interval_id[row] = row + 1;
  }

  return List::create(
      _["controls"] = compiler_controls_list(specification, controls),
      _["P"] = List::create(
          _["i"] = wrap(objective_p.i),
          _["j"] = wrap(objective_p.j),
          _["x"] = wrap(objective_p.x)),
      _["q"] = wrap(objective_q),
      _["A"] = List::create(
          _["i"] = wrap(constraint.a_i),
          _["j"] = wrap(constraint.a_j),
          _["x"] = wrap(constraint.a_x)),
      _["lower"] = wrap(constraint.lower),
      _["upper"] = wrap(constraint.upper),
      _["variable_component"] = variable_component,
      _["constraint_metadata"] = List::create(
          _["constraint_id"] = constraint_id,
          _["type"] = wrap(constraint.type),
          _["solve_component_id"] = wrap(constraint.component),
          _["fragment_a"] = wrap(constraint.fragment_a),
          _["fragment_b"] = wrap(constraint.fragment_b),
          _["event_id"] = wrap(constraint.event_id),
          _["clearance"] = wrap(constraint.clearance),
          _["distance_a"] = wrap(constraint.distance_a),
          _["distance_b"] = wrap(constraint.distance_b),
          _["lower"] = wrap(constraint.lower),
          _["upper"] = wrap(constraint.upper)),
      _["interval_metadata"] = List::create(
          _["interval_id"] = interval_id,
          _["solve_component_id"] = wrap(interval_component),
          _["render_road_fragment_id"] = wrap(interval_fragment),
          _["control_a"] = wrap(interval_control_a),
          _["control_b"] = wrap(interval_control_b),
          _["length"] = wrap(interval_length)),
      _["span_controls"] = List::create(
          _["start"] = span_start,
          _["end"] = span_end,
          _["periodic"] = span_periodic),
      _["support_arcs"] = List::create(
          _["span_id"] = support_span,
          _["start_control_id"] = support_start,
          _["end_control_id"] = support_end,
          _["start_station"] = support_start_station,
          _["end_station"] = support_end_station,
          _["arc_length"] = support_length,
          _["span_length"] = support_span_length,
          _["closed"] = support_closed,
          _["support_arc_id"] = support_id),
      _["anchors"] = List::create(
          _["render_road_endpoint_id"] =
              specification.anchor_endpoint_id,
          _["render_road_fragment_id"] = anchor_fragment_id,
          _["endpoint_side"] = specification.anchor_side,
          _["control_id"] = anchor_control_id,
          _["terrain"] = wrap(anchor_terrain),
          _["terrain_grade"] = specification.anchor_terrain_grade,
          _["solve_component_id"] = wrap(anchor_component)),
      _["clearances"] = List::create(
          _["type"] = wrap(clearance_type),
          _["event_id"] = wrap(clearance_event_id),
          _["pair_id"] = wrap(clearance_pair_id),
          _["lower_fragment_id"] = wrap(clearance_lower_fragment),
          _["upper_fragment_id"] = wrap(clearance_upper_fragment),
          _["lower_control_id"] = wrap(clearance_lower_control),
          _["upper_control_id"] = wrap(clearance_upper_control),
          _["lower_distance"] = wrap(clearance_lower_distance),
          _["upper_distance"] = wrap(clearance_upper_distance),
          _["lower_rank"] = wrap(clearance_lower_rank),
          _["upper_rank"] = wrap(clearance_upper_rank),
          _["clearance"] = wrap(clearance_value),
          _["solve_component_id"] = wrap(clearance_component)),
      _["overlap_relations"] = List::create(
          _["overlap_id"] = specification.overlap_id,
          _["lower_fragment_id"] = overlap_lower_id,
          _["upper_fragment_id"] = overlap_upper_id,
          _["lower_distance_start"] =
              specification.overlap_lower_start,
          _["lower_distance_end"] = specification.overlap_lower_end,
          _["upper_distance_start"] =
              specification.overlap_upper_start,
          _["upper_distance_end"] = specification.overlap_upper_end,
          _["clearance"] = specification.overlap_clearance),
      _["junction_equalities"] = List::create(
          _["junction_id"] = specification.junction_id,
          _["pair_id"] = specification.junction_pair_id,
          _["fragment_a"] = junction_fragment_a_id,
          _["fragment_b"] = junction_fragment_b_id,
          _["control_a"] = wrap(junction_control_a),
          _["control_b"] = wrap(junction_control_b),
          _["solve_component_id"] = wrap(junction_component)),
      _["continuation_equalities"] = List::create(
          _["continuation_id"] = specification.continuation_id,
          _["fragment_a"] = continuation_fragment_a_id,
          _["fragment_b"] = continuation_fragment_b_id,
          _["control_a"] = wrap(continuation_control_a),
          _["control_b"] = wrap(continuation_control_b),
          _["sign_a"] = specification.continuation_sign_a,
          _["sign_b"] = specification.continuation_sign_b,
          _["gap"] = specification.continuation_gap,
          _["exact_endpoint"] = specification.continuation_exact,
          _["solve_component_id"] = wrap(continuation_component)),
      _["chord_controls"] = List::create(
          _["span_id"] = wrap(chord_span),
          _["support_arc_id"] = wrap(chord_arc),
          _["control_id"] = wrap(chord_control),
          _["start_control_id"] = wrap(chord_start_control),
          _["end_control_id"] = wrap(chord_end_control),
          _["fraction"] = wrap(chord_fraction)),
      _["curvature_terms"] = List::create(
          _["grade_a"] = curvature_grade_a,
          _["grade_b"] = curvature_grade_b,
          _["sign_a"] = curvature_sign_a,
          _["sign_b"] = curvature_sign_b,
          _["length"] = curvature_length),
      _["diagnostics"] = List::create(
          _["fragment_count"] = fragment_count,
          _["control_count"] = control_count,
          _["constraint_count"] = constraint_count,
          _["P_triplet_count"] =
              static_cast<int>(objective_p.x.size()),
          _["A_triplet_count"] =
              static_cast<int>(constraint.a_x.size())));
}

namespace {

using RoadProfileClock = std::chrono::steady_clock;

double road_profile_elapsed(
    const RoadProfileClock::time_point& start,
    const RoadProfileClock::time_point& end) {
  return std::chrono::duration<double>(end - start).count();
}

struct NativeAdaptiveState {
  std::vector<int> type;
  std::vector<int> fragment_a;
  std::vector<double> distance_a;
  std::vector<int> fragment_b;
  std::vector<double> distance_b;
  std::vector<int> event_id;
  std::vector<double> clearance;
  std::vector<double> source_margin;

  std::size_t size() const {
    return type.size();
  }
};

NativeAdaptiveState parse_native_adaptive(const List& specification) {
  NativeAdaptiveState adaptive;
  if (!specification.containsElementNamed("adaptive")) {
    return adaptive;
  }
  const List input = specification["adaptive"];
  const IntegerVector type = input["type"];
  const IntegerVector fragment_a = input["fragment_a"];
  const NumericVector distance_a = input["distance_a"];
  const IntegerVector fragment_b = input["fragment_b"];
  const NumericVector distance_b = input["distance_b"];
  const IntegerVector event_id = input["event_id"];
  const NumericVector clearance = input["clearance"];
  const NumericVector source_margin = input["source_margin"];
  const R_xlen_t count = type.size();
  if (fragment_a.size() != count ||
      distance_a.size() != count ||
      fragment_b.size() != count ||
      distance_b.size() != count ||
      event_id.size() != count ||
      clearance.size() != count ||
      source_margin.size() != count) {
    stop("Native adaptive road-profile vectors must have equal lengths.");
  }
  adaptive.type.assign(type.begin(), type.end());
  adaptive.fragment_a.assign(fragment_a.begin(), fragment_a.end());
  adaptive.distance_a.assign(distance_a.begin(), distance_a.end());
  adaptive.fragment_b.assign(fragment_b.begin(), fragment_b.end());
  adaptive.distance_b.assign(distance_b.begin(), distance_b.end());
  adaptive.event_id.assign(event_id.begin(), event_id.end());
  adaptive.clearance.assign(clearance.begin(), clearance.end());
  adaptive.source_margin.assign(
      source_margin.begin(), source_margin.end());
  return adaptive;
}

List compiler_adaptive_list(const NativeAdaptiveState& adaptive) {
  return List::create(
      _["type"] = wrap(adaptive.type),
      _["fragment_a"] = wrap(adaptive.fragment_a),
      _["distance_a"] = wrap(adaptive.distance_a),
      _["fragment_b"] = wrap(adaptive.fragment_b),
      _["distance_b"] = wrap(adaptive.distance_b),
      _["event_id"] = wrap(adaptive.event_id),
      _["clearance"] = wrap(adaptive.clearance),
      _["source_margin"] = wrap(adaptive.source_margin));
}

List external_adaptive_list(
    const NativeAdaptiveState& adaptive,
    const IntegerVector& fragment_id) {
  IntegerVector fragment_a(adaptive.size());
  IntegerVector fragment_b(adaptive.size());
  for (std::size_t row = 0; row < adaptive.size(); ++row) {
    fragment_a[row] =
        adaptive.fragment_a[row] == NA_INTEGER
            ? NA_INTEGER
            : fragment_id[adaptive.fragment_a[row]];
    fragment_b[row] =
        adaptive.fragment_b[row] == NA_INTEGER
            ? NA_INTEGER
            : fragment_id[adaptive.fragment_b[row]];
  }
  return List::create(
      _["type"] = wrap(adaptive.type),
      _["fragment_a"] = fragment_a,
      _["distance_a"] = wrap(adaptive.distance_a),
      _["fragment_b"] = fragment_b,
      _["distance_b"] = wrap(adaptive.distance_b),
      _["event_id"] = wrap(adaptive.event_id),
      _["clearance"] = wrap(adaptive.clearance),
      _["source_margin"] = wrap(adaptive.source_margin));
}

double significant_road_profile_value(double value) {
  if (!finite_number(value) || value == 0.0) {
    return value;
  }
  const double exponent = std::floor(std::log10(std::abs(value)));
  const double scale = std::pow(10.0, 11.0 - exponent);
  if (!finite_number(scale) || scale == 0.0) {
    return value;
  }
  return std::nearbyint(value * scale) / scale;
}

bool equal_road_profile_integer(int first, int second) {
  return first == second ||
         (first == NA_INTEGER && second == NA_INTEGER);
}

bool equal_road_profile_number(double first, double second) {
  return first == second ||
         (NumericVector::is_na(first) && NumericVector::is_na(second)) ||
         (std::isnan(first) && std::isnan(second));
}

bool duplicate_native_adaptive(
    const NativeAdaptiveState& adaptive,
    int type,
    int fragment_a,
    double distance_a,
    int fragment_b,
    double distance_b,
    int event_id) {
  const double key_distance_a =
      significant_road_profile_value(distance_a);
  const double key_distance_b =
      significant_road_profile_value(distance_b);
  for (std::size_t row = 0; row < adaptive.size(); ++row) {
    if (adaptive.type[row] == type &&
        equal_road_profile_integer(
            adaptive.fragment_a[row], fragment_a) &&
        equal_road_profile_number(
            significant_road_profile_value(adaptive.distance_a[row]),
            key_distance_a) &&
        equal_road_profile_integer(
            adaptive.fragment_b[row], fragment_b) &&
        equal_road_profile_number(
            significant_road_profile_value(adaptive.distance_b[row]),
            key_distance_b) &&
        equal_road_profile_integer(adaptive.event_id[row], event_id)) {
      return true;
    }
  }
  return false;
}

int dense_fragment_index(
    const std::map<int, int>& fragment_index,
    int fragment_id) {
  if (fragment_id == NA_INTEGER) {
    return NA_INTEGER;
  }
  const auto found = fragment_index.find(fragment_id);
  if (found == fragment_index.end()) {
    stop(
        "Adaptive controls reference inactive fragment %d.",
        fragment_id);
  }
  return found->second;
}

int append_native_requests(
    NativeAdaptiveState& adaptive,
    const List& requests,
    const std::map<int, int>& fragment_index) {
  const IntegerVector type = requests["type"];
  const IntegerVector fragment_a_id = requests["fragment_a"];
  const NumericVector distance_a = requests["distance_a"];
  const IntegerVector fragment_b_id = requests["fragment_b"];
  const NumericVector distance_b = requests["distance_b"];
  const IntegerVector event_id = requests["event_id"];
  const NumericVector clearance = requests["clearance"];
  const NumericVector source_margin = requests["source_margin"];
  int added = 0;
  for (R_xlen_t row = 0; row < type.size(); ++row) {
    const int fragment_a =
        dense_fragment_index(fragment_index, fragment_a_id[row]);
    const int fragment_b =
        dense_fragment_index(fragment_index, fragment_b_id[row]);
    if (duplicate_native_adaptive(
            adaptive,
            type[row],
            fragment_a,
            distance_a[row],
            fragment_b,
            distance_b[row],
            event_id[row])) {
      continue;
    }
    adaptive.type.push_back(type[row]);
    adaptive.fragment_a.push_back(fragment_a);
    adaptive.distance_a.push_back(distance_a[row]);
    adaptive.fragment_b.push_back(fragment_b);
    adaptive.distance_b.push_back(distance_b[row]);
    adaptive.event_id.push_back(event_id[row]);
    adaptive.clearance.push_back(clearance[row]);
    adaptive.source_margin.push_back(source_margin[row]);
    ++added;
  }
  return added;
}

bool active_no_dip_span(
    int span_id,
    const IntegerVector& ids,
    const LogicalVector& no_dip) {
  for (R_xlen_t span = 0; span < ids.size(); ++span) {
    if (ids[span] == span_id) {
      return no_dip[span];
    }
  }
  return false;
}

List make_native_audit_specification(
    const List& specification,
    const List& compiled,
    const NativeAdaptiveState& adaptive) {
  const List fragment = specification["fragment"];
  const IntegerVector fragment_id = fragment["id"];
  const IntegerVector fragment_component = fragment["component_id"];
  const LogicalVector underground = fragment["underground"];
  const int fragment_count = fragment_id.size();
  std::map<int, int> fragment_index;
  for (int fragment_row = 0;
       fragment_row < fragment_count;
       ++fragment_row) {
    fragment_index[fragment_id[fragment_row]] = fragment_row;
  }

  const List control = compiled["controls"];
  const IntegerVector control_fragment = control["fragment_id"];
  const NumericVector control_distance = control["distance"];
  const NumericVector control_tolerance_value = control["tolerance"];
  IntegerVector control_start(fragment_count);
  IntegerVector control_count(fragment_count);
  IntegerVector control_row(control_fragment.size());
  NumericVector control_tolerance(fragment_count);
  std::fill(
      control_start.begin(), control_start.end(), NA_INTEGER);
  for (R_xlen_t row = 0; row < control_fragment.size(); ++row) {
    const auto found = fragment_index.find(control_fragment[row]);
    if (found == fragment_index.end()) {
      stop("Compiled controls contain an inactive fragment.");
    }
    const int dense_fragment = found->second;
    if (control_start[dense_fragment] == NA_INTEGER) {
      control_start[dense_fragment] = row;
    }
    ++control_count[dense_fragment];
    control_row[row] = row;
    control_tolerance[dense_fragment] = std::max(
        control_tolerance[dense_fragment],
        control_tolerance_value[row]);
  }
  for (int fragment_row = 0;
       fragment_row < fragment_count;
       ++fragment_row) {
    if (control_count[fragment_row] < 2) {
      stop(
          "Fragment %d does not have two profile controls.",
          fragment_id[fragment_row]);
    }
  }

  const List terrain = specification["terrain"];
  const List span = specification["span"];
  const IntegerVector span_id = span["id"];
  const LogicalVector span_no_dip = span["no_dip"];
  const List member = specification["span_member"];
  const IntegerVector member_span = member["span_id"];
  const IntegerVector member_fragment = member["fragment"];
  const IntegerVector member_orientation = member["orientation"];
  const NumericVector member_offset = member["offset"];
  const NumericVector member_length = member["fragment_length"];
  std::vector<int> chord_span;
  std::vector<int> chord_fragment;
  std::vector<double> chord_offset;
  std::vector<int> chord_orientation;
  std::vector<double> chord_length;
  for (R_xlen_t span_row = 0;
       span_row < span_id.size();
       ++span_row) {
    if (!span_no_dip[span_row]) {
      continue;
    }
    for (R_xlen_t member_row = 0;
         member_row < member_span.size();
         ++member_row) {
      if (member_span[member_row] == span_id[span_row]) {
        chord_span.push_back(member_span[member_row]);
        chord_fragment.push_back(member_fragment[member_row]);
        chord_offset.push_back(member_offset[member_row]);
        chord_orientation.push_back(member_orientation[member_row]);
        chord_length.push_back(member_length[member_row]);
      }
    }
  }

  const List support = compiled["support_arcs"];
  const IntegerVector support_span = support["span_id"];
  const IntegerVector support_start = support["start_control_id"];
  const IntegerVector support_end = support["end_control_id"];
  const NumericVector support_start_station = support["start_station"];
  const NumericVector support_end_station = support["end_station"];
  const NumericVector support_length = support["arc_length"];
  const NumericVector support_span_length = support["span_length"];
  const LogicalVector support_closed = support["closed"];
  const IntegerVector support_id = support["support_arc_id"];
  std::vector<int> arc_span;
  std::vector<int> arc_start;
  std::vector<int> arc_end;
  std::vector<double> arc_start_station;
  std::vector<double> arc_end_station;
  std::vector<double> arc_length;
  std::vector<double> arc_span_length;
  std::vector<bool> arc_closed;
  std::vector<int> arc_id;
  for (R_xlen_t arc = 0; arc < support_span.size(); ++arc) {
    if (!active_no_dip_span(
            support_span[arc], span_id, span_no_dip)) {
      continue;
    }
    arc_span.push_back(support_span[arc]);
    arc_start.push_back(support_start[arc] - 1);
    arc_end.push_back(support_end[arc] - 1);
    arc_start_station.push_back(support_start_station[arc]);
    arc_end_station.push_back(support_end_station[arc]);
    arc_length.push_back(support_length[arc]);
    arc_span_length.push_back(support_span_length[arc]);
    arc_closed.push_back(support_closed[arc]);
    arc_id.push_back(support_id[arc]);
  }

  const List overlap = specification["overlap"];
  const IntegerVector overlap_id = overlap["overlap_id"];
  std::vector<int> prior_overlap_id;
  std::vector<double> prior_lower_distance;
  std::vector<double> prior_upper_distance;
  for (std::size_t row = 0; row < adaptive.size(); ++row) {
    if (adaptive.type[row] == overlap_clearance_request) {
      prior_overlap_id.push_back(adaptive.event_id[row]);
      prior_lower_distance.push_back(adaptive.distance_a[row]);
      prior_upper_distance.push_back(adaptive.distance_b[row]);
    }
  }

  bool finite_control_terrain = true;
  for (R_xlen_t row = 0; row < control_distance.size(); ++row) {
    finite_control_terrain =
        finite_control_terrain &&
        finite_number(control_distance[row]) &&
        finite_number(as<NumericVector>(control["terrain"])[row]);
  }
  const NumericVector terrain_distance = terrain["distance"];
  const NumericVector terrain_elevation = terrain["elevation"];
  for (R_xlen_t row = 0; row < terrain_distance.size(); ++row) {
    finite_control_terrain =
        finite_control_terrain &&
        finite_number(terrain_distance[row]) &&
        finite_number(terrain_elevation[row]);
  }
  bool finite_geometry = true;
  if (specification.containsElementNamed("validity")) {
    const List validity = specification["validity"];
    finite_geometry = as<bool>(validity["finite_geometry"]);
    finite_control_terrain =
        finite_control_terrain &&
        as<bool>(validity["finite_control_terrain"]);
  }

  return List::create(
      _["fragment_id"] = fragment_id,
      _["fragment_component"] = fragment_component,
      _["control_start"] = control_start,
      _["control_count"] = control_count,
      _["control_row"] = control_row,
      _["control_distance"] = control_distance,
      _["control_tolerance"] = control_tolerance,
      _["underground"] = underground,
      _["terrain_start"] = terrain["start"],
      _["terrain_count"] = terrain["count"],
      _["terrain_distance"] = terrain_distance,
      _["terrain_elevation"] = terrain_elevation,
      _["chord_span_id"] = wrap(chord_span),
      _["chord_fragment_index"] = wrap(chord_fragment),
      _["chord_span_offset"] = wrap(chord_offset),
      _["chord_orientation"] = wrap(chord_orientation),
      _["chord_fragment_length"] = wrap(chord_length),
      _["arc_span_id"] = wrap(arc_span),
      _["arc_start_control"] = wrap(arc_start),
      _["arc_end_control"] = wrap(arc_end),
      _["arc_start_station"] = wrap(arc_start_station),
      _["arc_end_station"] = wrap(arc_end_station),
      _["arc_length"] = wrap(arc_length),
      _["arc_span_length"] = wrap(arc_span_length),
      _["arc_closed"] = wrap(arc_closed),
      _["arc_id"] = wrap(arc_id),
      _["overlap_id"] = overlap_id,
      _["overlap_lower_fragment_index"] = overlap["lower_fragment"],
      _["overlap_upper_fragment_index"] = overlap["upper_fragment"],
      _["overlap_lower_start"] = overlap["lower_start"],
      _["overlap_lower_end"] = overlap["lower_end"],
      _["overlap_upper_start"] = overlap["upper_start"],
      _["overlap_upper_end"] = overlap["upper_end"],
      _["overlap_clearance"] = overlap["clearance"],
      _["prior_overlap_id"] = wrap(prior_overlap_id),
      _["prior_lower_distance"] = wrap(prior_lower_distance),
      _["prior_upper_distance"] = wrap(prior_upper_distance),
      _["finite_geometry"] = finite_geometry,
      _["finite_control_terrain"] = finite_control_terrain);
}

std::string lower_road_profile_status(std::string status) {
  std::transform(
      status.begin(),
      status.end(),
      status.begin(),
      [](unsigned char value) {
        return static_cast<char>(std::tolower(value));
      });
  return status;
}

List native_timing_list(
    const RoadProfileClock::time_point& total_start,
    double compile_elapsed,
    double callback_elapsed,
    double solver_elapsed,
    double audit_elapsed,
    double conversion_elapsed,
    int callback_count) {
  const double total_elapsed =
      road_profile_elapsed(total_start, RoadProfileClock::now());
  return List::create(
      _["total_elapsed"] = total_elapsed,
      _["compile_elapsed"] = compile_elapsed,
      _["solver_elapsed"] = solver_elapsed,
      _["native_audit_elapsed"] = audit_elapsed,
      _["callback_count"] = callback_count,
      _["callback_elapsed"] = callback_elapsed,
      _["callback_overhead_elapsed"] =
          callback_elapsed - solver_elapsed,
      _["Rcpp_conversion_elapsed"] = conversion_elapsed);
}

List native_component_payload(
    const List& compiled,
    int component_id,
    std::vector<int>& variable,
    std::vector<int>& constraint_row) {
  const IntegerVector variable_component =
      compiled["variable_component"];
  const List constraint_metadata = compiled["constraint_metadata"];
  const IntegerVector constraint_component =
      constraint_metadata["solve_component_id"];
  const int variable_count = variable_component.size();
  std::vector<int> variable_local(variable_count, -1);
  variable.clear();
  for (int index = 0; index < variable_count; ++index) {
    if (variable_component[index] == component_id) {
      variable_local[index] = variable.size();
      variable.push_back(index);
    }
  }
  constraint_row.clear();
  std::vector<int> constraint_local(
      constraint_component.size(), -1);
  for (R_xlen_t row = 0; row < constraint_component.size(); ++row) {
    if (constraint_component[row] == component_id) {
      constraint_local[row] = constraint_row.size();
      constraint_row.push_back(row);
    }
  }

  const List full_p = compiled["P"];
  const IntegerVector full_p_i = full_p["i"];
  const IntegerVector full_p_j = full_p["j"];
  const NumericVector full_p_x = full_p["x"];
  std::vector<int> p_i;
  std::vector<int> p_j;
  std::vector<double> p_x;
  for (R_xlen_t entry = 0; entry < full_p_x.size(); ++entry) {
    const int global_i = full_p_i[entry] - 1;
    const int global_j = full_p_j[entry] - 1;
    if (global_i <= global_j &&
        variable_local[global_i] >= 0 &&
        variable_local[global_j] >= 0) {
      p_i.push_back(variable_local[global_i] + 1);
      p_j.push_back(variable_local[global_j] + 1);
      p_x.push_back(full_p_x[entry]);
    }
  }

  const List full_a = compiled["A"];
  const IntegerVector full_a_i = full_a["i"];
  const IntegerVector full_a_j = full_a["j"];
  const NumericVector full_a_x = full_a["x"];
  std::vector<int> a_i;
  std::vector<int> a_j;
  std::vector<double> a_x;
  for (R_xlen_t entry = 0; entry < full_a_x.size(); ++entry) {
    const int global_row = full_a_i[entry] - 1;
    const int global_column = full_a_j[entry] - 1;
    if (constraint_local[global_row] < 0) {
      continue;
    }
    if (variable_local[global_column] < 0) {
      stop(
          "A road profile component constraint references another "
          "component.");
    }
    a_i.push_back(constraint_local[global_row] + 1);
    a_j.push_back(variable_local[global_column] + 1);
    a_x.push_back(full_a_x[entry]);
  }

  const NumericVector full_q = compiled["q"];
  NumericVector q(variable.size());
  for (std::size_t index = 0; index < variable.size(); ++index) {
    q[index] = full_q[variable[index]];
  }
  const NumericVector full_lower = compiled["lower"];
  const NumericVector full_upper = compiled["upper"];
  NumericVector lower(constraint_row.size());
  NumericVector upper(constraint_row.size());
  for (std::size_t row = 0; row < constraint_row.size(); ++row) {
    lower[row] = full_lower[constraint_row[row]];
    upper[row] = full_upper[constraint_row[row]];
  }
  return List::create(
      _["component_id"] = component_id,
      _["variable_count"] = static_cast<int>(variable.size()),
      _["constraint_count"] =
          static_cast<int>(constraint_row.size()),
      _["P"] = List::create(
          _["i"] = wrap(p_i),
          _["j"] = wrap(p_j),
          _["x"] = wrap(p_x)),
      _["q"] = q,
      _["A"] = List::create(
          _["i"] = wrap(a_i),
          _["j"] = wrap(a_j),
          _["x"] = wrap(a_x)),
      _["lower"] = lower,
      _["upper"] = upper);
}

double native_constraint_violation(
    const List& compiled,
    const NumericVector& solution) {
  const NumericVector lower = compiled["lower"];
  const NumericVector upper = compiled["upper"];
  NumericVector activity(lower.size());
  const List matrix = compiled["A"];
  const IntegerVector row = matrix["i"];
  const IntegerVector column = matrix["j"];
  const NumericVector value = matrix["x"];
  for (R_xlen_t entry = 0; entry < value.size(); ++entry) {
    activity[row[entry] - 1] +=
        value[entry] * solution[column[entry] - 1];
  }
  double maximum = 0.0;
  for (R_xlen_t constraint = 0;
       constraint < activity.size();
       ++constraint) {
    maximum = std::max(
        maximum,
        std::max(
            lower[constraint] - activity[constraint],
            activity[constraint] - upper[constraint]));
  }
  return maximum;
}

double negative_margin_violation(double margin) {
  return std::max(-margin, 0.0);
}

List native_engineering_audit(
    const List& compiled,
    const NumericVector& solution,
    const List& continuous,
    double tolerance) {
  const double maximum_constraint =
      native_constraint_violation(compiled, solution);
  const double continuous_violation = std::max(
      negative_margin_violation(
          as<double>(continuous["continuous_terrain_margin"])),
      std::max(
          negative_margin_violation(
              as<double>(continuous["continuous_chord_margin"])),
          negative_margin_violation(as<double>(
              continuous[
                  "continuous_overlap_clearance_margin"]))));
  const bool finite =
      as<bool>(continuous["finite_profile_coordinates"]);
  const double maximum = std::max(
      std::max(maximum_constraint, continuous_violation),
      finite ? 0.0 : R_PosInf);
  return List::create(
      _["passed"] =
          finite_number(maximum) && maximum <= tolerance && finite,
      _["tolerance"] = tolerance,
      _["maximum_violation"] = maximum,
      _["maximum_constraint_violation"] = maximum_constraint,
      _["continuous_terrain_margin"] =
          continuous["continuous_terrain_margin"],
      _["continuous_chord_margin"] =
          continuous["continuous_chord_margin"],
      _["continuous_overlap_clearance_margin"] =
          continuous[
              "continuous_overlap_clearance_margin"],
      _["finite_profile_coordinates"] = finite);
}

List native_rendered_elevation(
    const List& audit_specification,
    const NumericVector& height,
    const NumericVector& grade) {
  const AuditSpecification audit(audit_specification);
  NumericVector elevation(audit.terrain_distance.size());
  for (R_xlen_t fragment = 0;
       fragment < audit.fragment_id.size();
       ++fragment) {
    const int begin = audit.terrain_start[fragment];
    const int end = begin + audit.terrain_count[fragment];
    for (int row = begin; row < end; ++row) {
      elevation[row] = evaluate_profile(
          audit,
          height,
          grade,
          fragment,
          audit.terrain_distance[row])
                           .height;
    }
  }
  return List::create(
      _["offset"] = audit.terrain_start,
      _["elevation"] = elevation);
}

List trim_native_refinement_trace(
    const List& trace,
    int count) {
  List result(count);
  for (int iteration = 0; iteration < count; ++iteration) {
    result[iteration] = trace[iteration];
  }
  return result;
}

}  // namespace

// The adaptive solver retains no native state after returning. Compilation,
// profile evaluation, auditing, and request deduplication stay in C++; the only
// C++-to-R callback is one main-thread invocation for one OSQP component solve.
//
// [[Rcpp::export]]
List solve_render_road_profiles_cpp(
    List specification,
    Function solve_component,
    double profile_tolerance,
    int maximum_refinement_iterations,
    bool diagnostics = false) {
  if (!finite_number(profile_tolerance) || profile_tolerance < 0.0) {
    stop("`profile_tolerance` must be non-negative and finite.");
  }
  if (maximum_refinement_iterations < 0) {
    stop(
        "`maximum_refinement_iterations` must be a non-negative "
        "integer.");
  }
  const RoadProfileClock::time_point total_start =
      RoadProfileClock::now();
  double compile_elapsed = 0.0;
  double callback_elapsed = 0.0;
  double solver_elapsed = 0.0;
  double audit_elapsed = 0.0;
  double conversion_elapsed = 0.0;
  int callback_count = 0;

  const List fragment = specification["fragment"];
  const IntegerVector fragment_id = fragment["id"];
  std::map<int, int> fragment_index;
  for (R_xlen_t row = 0; row < fragment_id.size(); ++row) {
    fragment_index[fragment_id[row]] = row;
  }
  const List component = specification["component"];
  const IntegerVector component_id = component["id"];
  NativeAdaptiveState adaptive = parse_native_adaptive(specification);

  List compiled;
  List continuous;
  List engineering_audit;
  NumericVector solution;
  NumericVector height;
  NumericVector grade;
  List component_results;
  CharacterVector component_status;
  IntegerVector component_iterations;
  NumericVector component_objective;
  NumericVector component_primal;
  NumericVector component_dual;
  List refinement_requests(maximum_refinement_iterations + 1);

  for (int refinement_iteration = 0;
       refinement_iteration <= maximum_refinement_iterations;
       ++refinement_iteration) {
    checkUserInterrupt();
    const RoadProfileClock::time_point compile_start =
        RoadProfileClock::now();
    compiled = compile_render_road_profile_problem_cpp(
        specification, compiler_adaptive_list(adaptive));
    compile_elapsed += road_profile_elapsed(
        compile_start, RoadProfileClock::now());

    const NumericVector q = compiled["q"];
    solution = NumericVector(q.size(), NA_REAL);
    component_results = List(component_id.size());
    component_status = CharacterVector(component_id.size());
    component_iterations = IntegerVector(component_id.size());
    component_objective = NumericVector(component_id.size());
    component_primal = NumericVector(component_id.size());
    component_dual = NumericVector(component_id.size());
    for (R_xlen_t component_index = 0;
         component_index < component_id.size();
         ++component_index) {
      checkUserInterrupt();
      std::vector<int> variable;
      std::vector<int> constraint_row;
      const RoadProfileClock::time_point payload_start =
          RoadProfileClock::now();
      const List payload = native_component_payload(
          compiled,
          component_id[component_index],
          variable,
          constraint_row);
      conversion_elapsed += road_profile_elapsed(
          payload_start, RoadProfileClock::now());

      const RoadProfileClock::time_point callback_start =
          RoadProfileClock::now();
      const List result = solve_component(payload);
      callback_elapsed += road_profile_elapsed(
          callback_start, RoadProfileClock::now());
      ++callback_count;

      const RoadProfileClock::time_point parse_start =
          RoadProfileClock::now();
      if (!result.containsElementNamed("status") ||
          !result.containsElementNamed("status_message") ||
          !result.containsElementNamed("x")) {
        stop(
            "The road-profile solver callback returned a malformed "
            "result.");
      }
      const NumericVector component_solution = result["x"];
      const std::string status =
          as<std::string>(result["status"]);
      const std::string status_message =
          as<std::string>(result["status_message"]);
      const std::string normalized_status =
          lower_road_profile_status(status);
      const int iterations =
          result.containsElementNamed("iterations")
              ? as<int>(result["iterations"])
              : NA_INTEGER;
      const double objective =
          result.containsElementNamed("objective")
              ? as<double>(result["objective"])
              : NA_REAL;
      const double primal =
          result.containsElementNamed("primal_residual")
              ? as<double>(result["primal_residual"])
              : NA_REAL;
      const double dual =
          result.containsElementNamed("dual_residual")
              ? as<double>(result["dual_residual"])
              : NA_REAL;
      const double elapsed =
          result.containsElementNamed("elapsed")
              ? as<double>(result["elapsed"])
              : NA_REAL;
      if (finite_number(elapsed)) {
        solver_elapsed += elapsed;
      }
      bool finite_solution =
          component_solution.size() ==
          static_cast<R_xlen_t>(variable.size());
      for (R_xlen_t index = 0;
           finite_solution && index < component_solution.size();
           ++index) {
        finite_solution =
            finite_solution &&
            finite_number(component_solution[index]);
      }
      component_results[component_index] = result;
      component_status[component_index] = status_message;
      component_iterations[component_index] = iterations;
      component_objective[component_index] = objective;
      component_primal[component_index] = primal;
      component_dual[component_index] = dual;
      conversion_elapsed += road_profile_elapsed(
          parse_start, RoadProfileClock::now());
      if ((normalized_status != "solved" &&
           normalized_status != "solved inaccurate") ||
          !finite_solution) {
        return List::create(
            _["success"] = false,
            _["failure_type"] = "solver",
            _["component_id"] = component_id[component_index],
            _["refinement_iteration"] = refinement_iteration,
            _["solver_result"] = result,
            _["compiled"] = compiled,
            _["adaptive"] =
                external_adaptive_list(adaptive, fragment_id),
            _["timing"] = native_timing_list(
                total_start,
                compile_elapsed,
                callback_elapsed,
                solver_elapsed,
                audit_elapsed,
                conversion_elapsed,
                callback_count));
      }
      for (std::size_t index = 0; index < variable.size(); ++index) {
        solution[variable[index]] = component_solution[index];
      }
    }

    const List compiled_controls = compiled["controls"];
    const int control_count =
        as<IntegerVector>(compiled_controls["fragment_id"]).size();
    height = NumericVector(control_count);
    grade = NumericVector(control_count);
    for (int control = 0; control < control_count; ++control) {
      height[control] = solution[control];
      grade[control] = solution[control_count + control];
    }

    const RoadProfileClock::time_point audit_start =
        RoadProfileClock::now();
    const List audit_specification =
        make_native_audit_specification(
            specification, compiled, adaptive);
    continuous = audit_render_road_profiles_cpp(
        audit_specification,
        height,
        grade,
        profile_tolerance,
        false);
    audit_elapsed += road_profile_elapsed(
        audit_start, RoadProfileClock::now());
    const List requests = continuous["requests"];
    refinement_requests[refinement_iteration] = requests;
    const IntegerVector request_type = requests["type"];
    if (request_type.size() > 0) {
      if (
          refinement_iteration >=
          maximum_refinement_iterations) {
        const RoadProfileClock::time_point detailed_start =
            RoadProfileClock::now();
        continuous = audit_render_road_profiles_cpp(
            audit_specification,
            height,
            grade,
            profile_tolerance,
            true);
        audit_elapsed += road_profile_elapsed(
            detailed_start, RoadProfileClock::now());
        return List::create(
            _["success"] = false,
            _["failure_type"] = "refinement",
            _["failure_reason"] = "iteration_limit",
            _["refinement_iteration"] = refinement_iteration,
            _["solution"] = solution,
            _["height"] = height,
            _["grade"] = grade,
            _["components"] = List::create(
                _["solve_component_id"] = component_id,
                _["status"] = component_status,
                _["iterations"] = component_iterations,
                _["objective"] = component_objective,
                _["primal_residual"] = component_primal,
                _["dual_residual"] = component_dual),
            _["solver_results"] = component_results,
            _["continuous_diagnostics"] = continuous,
            _["compiled"] = compiled,
            _["adaptive"] =
                external_adaptive_list(adaptive, fragment_id),
            _["refinement_requests"] =
                trim_native_refinement_trace(
                    refinement_requests,
                    refinement_iteration + 1),
            _["timing"] = native_timing_list(
                total_start,
                compile_elapsed,
                callback_elapsed,
                solver_elapsed,
                audit_elapsed,
                conversion_elapsed,
                callback_count));
      }
      const int added = append_native_requests(
          adaptive, requests, fragment_index);
      if (added == 0) {
        const RoadProfileClock::time_point detailed_start =
            RoadProfileClock::now();
        continuous = audit_render_road_profiles_cpp(
            audit_specification,
            height,
            grade,
            profile_tolerance,
            true);
        audit_elapsed += road_profile_elapsed(
            detailed_start, RoadProfileClock::now());
        return List::create(
            _["success"] = false,
            _["failure_type"] = "refinement",
            _["failure_reason"] = "duplicate_request",
            _["refinement_iteration"] = refinement_iteration,
            _["solution"] = solution,
            _["height"] = height,
            _["grade"] = grade,
            _["components"] = List::create(
                _["solve_component_id"] = component_id,
                _["status"] = component_status,
                _["iterations"] = component_iterations,
                _["objective"] = component_objective,
                _["primal_residual"] = component_primal,
                _["dual_residual"] = component_dual),
            _["solver_results"] = component_results,
            _["continuous_diagnostics"] = continuous,
            _["compiled"] = compiled,
            _["adaptive"] =
                external_adaptive_list(adaptive, fragment_id),
            _["refinement_requests"] =
                trim_native_refinement_trace(
                    refinement_requests,
                    refinement_iteration + 1),
            _["timing"] = native_timing_list(
                total_start,
                compile_elapsed,
                callback_elapsed,
                solver_elapsed,
                audit_elapsed,
                conversion_elapsed,
                callback_count));
      }
      continue;
    }

    engineering_audit = native_engineering_audit(
        compiled, solution, continuous, profile_tolerance);
    if (!as<bool>(engineering_audit["passed"])) {
      int worst_component = 0;
      for (R_xlen_t component_index = 1;
           component_index < component_primal.size();
           ++component_index) {
        if (component_primal[component_index] >
            component_primal[worst_component]) {
          worst_component = component_index;
        }
      }
      bool inaccurate = false;
      for (R_xlen_t component_index = 0;
           component_index < component_status.size();
           ++component_index) {
        inaccurate =
            inaccurate ||
            lower_road_profile_status(
                as<std::string>(
                    component_status[component_index])) ==
                "solved inaccurate";
      }
      const RoadProfileClock::time_point detailed_start =
          RoadProfileClock::now();
      continuous = audit_render_road_profiles_cpp(
          audit_specification,
          height,
          grade,
          profile_tolerance,
          true);
      audit_elapsed += road_profile_elapsed(
          detailed_start, RoadProfileClock::now());
      return List::create(
          _["success"] = false,
          _["failure_type"] = "engineering",
          _["failure_status"] =
              inaccurate
                  ? "solved inaccurate; engineering audit failed"
                  : "engineering audit failed",
          _["component_id"] = component_id[worst_component],
          _["refinement_iteration"] = refinement_iteration,
          _["solution"] = solution,
          _["height"] = height,
          _["grade"] = grade,
          _["components"] = List::create(
              _["solve_component_id"] = component_id,
              _["status"] = component_status,
              _["iterations"] = component_iterations,
              _["objective"] = component_objective,
              _["primal_residual"] = component_primal,
              _["dual_residual"] = component_dual),
          _["solver_results"] = component_results,
          _["continuous_diagnostics"] = continuous,
          _["engineering_audit"] = engineering_audit,
          _["compiled"] = compiled,
          _["adaptive"] =
              external_adaptive_list(adaptive, fragment_id),
          _["refinement_requests"] =
              trim_native_refinement_trace(
                  refinement_requests,
                  refinement_iteration + 1),
          _["timing"] = native_timing_list(
              total_start,
              compile_elapsed,
              callback_elapsed,
              solver_elapsed,
              audit_elapsed,
              conversion_elapsed,
              callback_count));
    }

    if (diagnostics) {
      const RoadProfileClock::time_point detailed_start =
          RoadProfileClock::now();
      continuous = audit_render_road_profiles_cpp(
          audit_specification,
          height,
          grade,
          profile_tolerance,
          true);
      audit_elapsed += road_profile_elapsed(
          detailed_start, RoadProfileClock::now());
    }
    return List::create(
        _["success"] = true,
        _["solution"] = solution,
        _["controls"] = List::create(
            _["height"] = height,
            _["grade"] = grade),
        _["components"] = List::create(
            _["solve_component_id"] = component_id,
            _["status"] = component_status,
            _["iterations"] = component_iterations,
            _["objective"] = component_objective,
            _["primal_residual"] = component_primal,
            _["dual_residual"] = component_dual),
        _["solver_results"] = component_results,
        _["continuous_diagnostics"] = continuous,
        _["engineering_audit"] = engineering_audit,
        _["refinement_iterations"] = refinement_iteration,
        _["rendered_elevation"] = native_rendered_elevation(
            audit_specification, height, grade),
        _["compiled"] = compiled,
        _["adaptive"] =
            external_adaptive_list(adaptive, fragment_id),
        _["refinement_requests"] =
            trim_native_refinement_trace(
                refinement_requests,
                refinement_iteration + 1),
        _["timing"] = native_timing_list(
            total_start,
            compile_elapsed,
            callback_elapsed,
            solver_elapsed,
            audit_elapsed,
            conversion_elapsed,
            callback_count));
  }
  stop("Road-profile refinement ended unexpectedly.");
}
