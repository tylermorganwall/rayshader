#include <Rcpp.h>
#include <cmath>
#include <limits>

using namespace Rcpp;

// Keep multiplication and addition separate, as in the R transformation path.
// Fusing them can change rounding in the resulting vertex coordinates.
#ifdef __clang__
#pragma clang fp contract(off)
#endif

// Batch unrotated OBJ instances. Templates are singleton ray_mesh objects
// prepared by rayvertex; topology/materials are immutable shared R objects.
// Each instance owns its transformed vertex/normal buffers. No per-instance
// R transformation or material functions are called.
// [[Rcpp::export]]
List instance_obj_cpp(List templates, IntegerVector template_id,
                      NumericMatrix xyz, NumericMatrix scale) {
  // Validate the batch layout before indexing any per-instance inputs.
  // Rows correspond across template_id, xyz and scale; XYZ uses scene units.
  const int count = xyz.nrow();

  if (templates.size() == 0 || xyz.ncol() != 3 || scale.ncol() != 3 ||
      scale.nrow() != count || template_id.size() != count) {
    stop("Incompatible OBJ instance dimensions.");
  }

  // Allocate the final component lists once. Entries retain instance order so
  // downstream mesh merging keeps topology, material IDs and attributes aligned.
  List shapes(count),
       vertices(count),
       texcoords(count),
       normals(count),
       materials(count);
  CharacterVector hashes(count);

  for (int k = 0; k < count; ++k) {
    if ((k & 1023) == 0) {
      checkUserInterrupt();
    }

    // Select the template using R's one-based index. The R caller has already
    // prepared the material variant; repeated instances can share its topology,
    // UV coordinates and material objects without copying or modifying them.
    const int template_index = template_id[k];

    if (template_index < 1 || template_index > templates.size()) {
      stop("Invalid OBJ template index.");
    }

    List mesh = templates[template_index - 1];
    List source_shapes = mesh["shapes"],
         source_vertices = mesh["vertices"],
         source_texcoords = mesh["texcoords"],
         source_normals = mesh["normals"],
         source_materials = mesh["materials"];

    if (source_shapes.size() != 1 || source_vertices.size() != 1 ||
        source_texcoords.size() != 1 || source_normals.size() != 1 ||
        source_materials.size() != 1) {
      stop("OBJ template must contain one shape.");
    }

    NumericMatrix v = source_vertices[0],
                  n = source_normals[0];

    if (v.ncol() != 3 || n.ncol() != 3) {
      stop("OBJ coordinates must have three columns.");
    }

    bool scaled = false;

    for (int axis = 0; axis < 3; ++axis) {
      if (!R_finite(xyz(k, axis)) || !R_finite(scale(k, axis))) {
        stop("OBJ instance transforms must be finite.");
      }

      scaled = scaled || scale(k, axis) != 1;
    }

    // Transform positions in the same order as scale_mesh() then
    // translate_mesh(): scale about the origin, then add the scene position.
    // Fill columns contiguously to match R's matrix storage order.
    NumericMatrix placed(v.nrow(), 3);

    for (int axis = 0; axis < 3; ++axis) {
      for (int row = 0; row < v.nrow(); ++row) {
        const double value = scaled ? v(row, axis) * scale(k, axis) : v(row, axis);
        placed(row, axis) = value + xyz(k, axis);
      }
    }

    if (v.hasAttribute("dimnames")) {
      placed.attr("dimnames") = v.attr("dimnames");
    }

    // Normals use the inverse transpose of the scale matrix. For a diagonal
    // scale this is a per-axis reciprocal, followed by normalization. Translation
    // does not affect normals; unscaled or empty normal buffers can be shared.
    if (scaled && n.nrow() > 0) {
      NumericMatrix transformed(n.nrow(), 3);
      double inverse[3] = {1 / scale(k, 0), 1 / scale(k, 1), 1 / scale(k, 2)};

      for (int row = 0; row < n.nrow(); ++row) {
        // Match R's sum(): squared terms are doubles, accumulated in long double.
        long double squared_length = 0;

        for (int axis = 0; axis < 3; ++axis) {
          const double value = n(row, axis) * inverse[axis];
          transformed(row, axis) = value;
          squared_length += value * value;
        }

        // Keep zero-length normals at zero. Singular scales otherwise retain
        // the existing R path's nonfinite results rather than inventing a normal.
        double length = std::sqrt(static_cast<double>(squared_length));

        if (length == 0) {
          length = 1;
        }

        for (int axis = 0; axis < 3; ++axis) {
          transformed(row, axis) /= length;
        }
      }

      if (n.hasAttribute("dimnames")) {
        transformed.attr("dimnames") = n.attr("dimnames");
      }

      normals[k] = transformed;
    } else {
      normals[k] = n;
    }

    // Store the owned position buffer alongside shared, unchanged components.
    // Keep the template's material hash so later material deduplication behaves
    // exactly as it does for independently constructed rayvertex instances.
    shapes[k] = source_shapes[0];
    vertices[k] = placed;
    texcoords[k] = source_texcoords[0];
    materials[k] = source_materials[0];

    CharacterVector source_hashes = mesh.attr("material_hashes");

    if (source_hashes.size() != 1) {
      stop("OBJ template must contain one material hash.");
    }

    hashes[k] = source_hashes[0];
  }

  // Restore rayvertex's component classes on the assembled lists. The result
  // has the same ray_mesh structure expected by the existing merge/render path.
  List prototype = templates[0];
  List result = List::create(
    _["shapes"] = shapes,
    _["vertices"] = vertices,
    _["texcoords"] = texcoords,
    _["normals"] = normals,
    _["materials"] = materials
  );

  for (const char* component : {"shapes", "vertices", "texcoords", "normals"}) {
    RObject target = result[component],
            source = prototype[component];
    target.attr("class") = source.attr("class");
  }

  result.attr("class") = prototype.attr("class");
  result.attr("material_hashes") = hashes;

  return result;
}
