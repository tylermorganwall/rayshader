#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix construct_matrix(NumericMatrix image_reference,
                               int number_rows, int number_cols,
                               const IntegerVector& index_x,
                               const IntegerVector& index_y) {
  if (number_rows < 0 || number_cols < 0 || image_reference.nrow() == 0 ||
      image_reference.ncol() == 0 || index_x.size() != index_y.size() ||
      index_x.size() != static_cast<R_xlen_t>(number_rows) * number_cols) {
    stop("Texture lookup dimensions do not match the supplied indices.");
  }
  NumericMatrix returnimage(number_cols, number_rows);
  for (R_xlen_t i = 0; i < index_x.size(); ++i) {
    // Keep malformed internal calls from becoming out-of-bounds native reads.
    if (index_x[i] == NA_INTEGER || index_y[i] == NA_INTEGER ||
        index_x[i] < 0 || index_x[i] >= image_reference.nrow() ||
        index_y[i] < 0 || index_y[i] >= image_reference.ncol()) {
      stop("Texture indices must be within zero-based image bounds.");
    }
    returnimage[i] = image_reference(index_x[i], index_y[i]);
  }
  return returnimage;
}
