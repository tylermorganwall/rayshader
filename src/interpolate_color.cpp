#include <Rcpp.h>
using namespace Rcpp;

double bilinear_interp_colors(double q11, double q12, double q21, double q22, 
                              double x1, double x2, double y1, double y2, double x, double y) {
  double x2x, y2y, yy1, xx1;
  x2x = x2 - x;
  y2y = y2 - y;
  yy1 = y - y1;
  xx1 = x - x1;
  return(q11 * x2x * y2y + q21 * xx1 * y2y + q12 * x2x * yy1 + q22 * xx1 * yy1);
}

// [[Rcpp::export]]
NumericMatrix interpolate_color(double color_nw, double color_ne, double color_se, double color_sw) {
  NumericMatrix colormat(256, 256);
  for(int i = 0; i < 256; i++) {
    for(int j = 0; j < 256; j++) {
      colormat(i,j) = bilinear_interp_colors(color_sw, color_nw, color_sw, color_nw,
                                             0, 1, 0, 1, i, j) * 256;
    }
  }
             
  return(colormat);
}