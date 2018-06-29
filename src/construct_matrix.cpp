#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix construct_matrix(NumericMatrix image_reference,
                      int number_rows, int number_cols,
                      const IntegerVector& index_x, const IntegerVector& index_y) {
  NumericMatrix returnimage(number_cols,number_rows);
  
  for(int i = 0; i < index_x.length(); i++) {
    returnimage[i] = image_reference(index_x(i),index_y(i));
  }
  return(returnimage);
}