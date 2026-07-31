#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
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
