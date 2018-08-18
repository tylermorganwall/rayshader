#include <Rcpp.h>
using namespace Rcpp;

inline double bilinearinterp(double q11, double q12, double q21, double q22, double x, double y) {
  double x2x, y2y, yy1, xx1;
  x2x = 1 - x;
  y2y = 1 - y;
  yy1 = y;
  xx1 = x;
  return(q11 * x2x * y2y + q21 * xx1 * y2y + q12 * x2x * yy1 + q22 * xx1 * yy1);
}

// [[Rcpp::export]]
NumericMatrix bilineargrid(NumericMatrix &colorarray) {
  NumericMatrix output(512,512);
  double i,j;
  for(i=0; i < 256; i++) {
    for(j=0; j < 256; j++) {
      output(i,j) = bilinearinterp(colorarray(0,0),colorarray(0,1),colorarray(1,0),colorarray(1,1),i/256,j/256);
    }
  }
  for(i=256; i < 512; i++) {
    for(j=0; j < 256; j++) {
      output(i,j) = bilinearinterp(colorarray(1,0),colorarray(1,1),colorarray(2,0),colorarray(2,1),(i-256)/256,j/256);
    }
  }
  for(i=0; i < 256; i++) {
    for(j=256; j < 512; j++) {
      output(i,j) = bilinearinterp(colorarray(0,1),colorarray(0,2),colorarray(1,1),colorarray(1,2),i/256,(j-256)/256);
    }
  }
  for(i=256; i < 512; i++) {
    for(j=256; j < 512; j++) {
      output(i,j) = bilinearinterp(colorarray(1,1),colorarray(1,2),colorarray(2,1),colorarray(2,2),(i-256)/256,(j-256)/256);
    }
  }
  return(output);
}