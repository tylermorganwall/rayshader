#include <Rcpp.h>
using namespace Rcpp;

#define INF 1E20;

template <class T>
inline T square(const T &x) { return x*x; };

static NumericVector dt(NumericVector f, int n) {
  NumericVector d(n);
  IntegerVector v(n);
  NumericVector z(n+1);
  int k = 0;
  v[0] = 0;
  z[0] = -INF;
  z[1] = +INF;
  for (int q = 1; q <= n-1; q++) {
    float s  = ((f[q]+square(q))-(f[v[k]]+square(v[k])))/(2*q-2*v[k]);
    while (s <= z[k]) {
      k--;
      s  = ((f[q]+square(q))-(f[v[k]]+square(v[k])))/(2*q-2*v[k]);
    }
    k++;
    v[k] = q;
    z[k] = s;
    z[k+1] = +INF;
  }
  
  k = 0;
  for (int q = 0; q <= n-1; q++) {
    while (z[k+1] < q) {
      k++;
    }
    d[q] = square(q-v[k]) + f[v[k]];
  }
  return(d);
}

static void dt(NumericMatrix& im) {
  int width = im.nrow();
  int height = im.ncol();
  NumericVector f(std::max(width,height));
  
  // transform along columns
  for (int x = 0; x < width; x++) {
    for (int y = 0; y < height; y++) {
      f(y) = im(x, y);
    }
    NumericVector d = dt(f, height);
    for (int y = 0; y < height; y++) {
      im(x, y) = d(y);
    }
  }
  
  // transform along rows
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      f(x) = im(x, y);
    }
    NumericVector d = dt(f, width);
    for (int x = 0; x < width; x++) {
      im(x, y) = d(x);
    }
  }
}

// [[Rcpp::export]]
NumericMatrix get_boolean_distance(LogicalMatrix input) {
  int width = input.nrow();
  int height = input.ncol();
  
  NumericMatrix out(width, height);
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      if (input(x, y) == 1) {
        out(x, y) = 0;
      } else {
        out(x, y) = INF;
      }
    }
  }
  dt(out);
  for(int i = 0; i < width; i++) {
    for(int j = 0; j < height; j++) {
      out(i,j) = sqrt(out(i,j));
    }
  }
  return(out);
}

