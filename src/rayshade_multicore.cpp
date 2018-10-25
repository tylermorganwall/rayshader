#include <Rcpp.h>
using namespace Rcpp;

inline double bilinearinterp(double q11, double q12, double q21, double q22, double x1, double x2, double y1, double y2, double x, double y) {
  double x2x, y2y, yy1, xx1;
  x2x = x2 - x;
  y2y = y2 - y;
  yy1 = y - y1;
  xx1 = x - x1;
  return(q11 * x2x * y2y + q21 * xx1 * y2y + q12 * x2x * yy1 + q22 * xx1 * yy1);
}

bool ray_intersects_multi(NumericMatrix& heightmap, NumericVector& tanangles,
                    int i, int j, int angentry, 
                    double maxheight, double precisionval,
                    double cossunangle, double sinsunangle, 
                    int numbercols, int numberrows,
                    double zscale, double maxdist) {
  bool breakloop;
  double xcoord, ycoord, tanangheight;
  double ceilxcoord,ceilycoord,floorxcoord,floorycoord;
  for(int k = 1; k < maxdist; k++) {
    xcoord = i + sinsunangle * k;
    ycoord = j + cossunangle * k;
    tanangheight = heightmap(i, j) + tanangles[angentry] * k * zscale;
    if(xcoord > numberrows-1 || ycoord > numbercols-1 || xcoord < 0 || ycoord < 0 || tanangheight > maxheight) {
      break;
    } else {
      ceilxcoord = ceil(xcoord);
      ceilycoord = ceil(ycoord);
      floorxcoord = floor(xcoord);
      floorycoord = floor(ycoord);
      
      // Get case where xcoord and ycoord integer number
      if(floorxcoord == ceilxcoord && floorycoord == ceilycoord) {
        if (tanangheight < heightmap(xcoord,ycoord)) {
          return(true);
        }
      }
      
      if(fabs(floorxcoord - ceilxcoord) < precisionval && floorycoord != ceilycoord) {
        if (tanangheight < (heightmap(floorxcoord,ceilycoord) - heightmap(floorxcoord,floorycoord))*(ycoord-floorycoord) + heightmap(floorxcoord,floorycoord)) {
          return(true);
        }
      }
      
      if(floorxcoord != ceilxcoord && fabs(floorycoord - ceilycoord) < precisionval) {
        if (tanangheight < (heightmap(ceilxcoord,floorycoord) - heightmap(floorxcoord,floorycoord))*(xcoord-floorxcoord) + heightmap(floorxcoord,floorycoord)) {
          return(true);
        }
      }
      
      if (heightmap(ceilxcoord, ceilycoord) < tanangheight &&
          heightmap(floorxcoord, ceilycoord) < tanangheight &&
          heightmap(ceilxcoord, floorycoord) < tanangheight &&
          heightmap(floorxcoord, floorycoord) < tanangheight) {
        continue;
      }
      
      if (tanangheight < bilinearinterp(heightmap(floorxcoord, floorycoord),
                                        heightmap(floorxcoord, ceilycoord),
                                        heightmap(ceilxcoord, floorycoord),
                                        heightmap(ceilxcoord, ceilycoord),
                                        floorxcoord, ceilxcoord,
                                        floorycoord, ceilycoord,
                                        xcoord, ycoord)) {
        return(true);
      }
      if(k == (maxdist - 1)) breakloop = true;
    }
  }
  return(false);
}

// [[Rcpp::export]]
NumericMatrix rayshade_multicore(double sunangle, NumericVector anglebreaks, NumericMatrix heightmap, 
                                 double zscale, double maxsearch, int row, NumericVector& cache_mask) {
  double precisionval = 1e-10;
  
  //Cache trig functions
  double sinsunangle = sin(sunangle);
  double cossunangle = cos(sunangle);
  int numberangles = anglebreaks.size();
  NumericVector tanangles(numberangles);
  for(int i = 0; i < numberangles; i++) {
    tanangles(i) = tan(anglebreaks[i]);
  }
  int i = row;
  
  int numbercols = heightmap.ncol();
  int numberrows = heightmap.nrow();
  NumericMatrix shadowmatrix(1,numbercols);
  std::fill(shadowmatrix.begin(), shadowmatrix.end(), 1.0);
  double maxdist = maxsearch;
  int current_min_entry = 0;
  int current_max_entry = numberangles - 1;
  int current_entry = current_max_entry/2;
  bool anyfound;
  double maxheight = max(heightmap);

  double invnumberangles = 1 / (double)numberangles;
  
  for(int j = 0; j < numbercols; j++) {
    Rcpp::checkUserInterrupt();
    if(cache_mask(j)) {
      anyfound = false;
      if(numberangles < 3) {
        for(int ang = 0; ang < numberangles; ang++) {
          if(ray_intersects_multi(heightmap,tanangles,
                            i, j, ang,
                            maxheight, precisionval,
                            cossunangle, sinsunangle, 
                            numbercols, numberrows,
                            zscale, maxdist)) {
            shadowmatrix(1,j) = 1 - ((double)ang + 1) * invnumberangles;
          } 
        }
      } else {
        while(current_min_entry != current_entry && current_max_entry != current_entry) {
          if(ray_intersects_multi(heightmap,tanangles,
                            i, j, current_entry, 
                            maxheight, precisionval,
                            cossunangle, sinsunangle, 
                            numbercols, numberrows,
                            zscale, maxdist)) {
            current_min_entry = current_entry;
            current_entry = (current_max_entry + current_entry)/2;
            anyfound = true;
          } else {
            current_max_entry = current_entry;
            current_entry = (current_min_entry + current_entry)/2;
          }
        }
        if(anyfound) {
          shadowmatrix(1,j) = 1 - ((double)current_entry + 1) * invnumberangles;
        }
        current_min_entry = 0;
        current_max_entry =  numberangles - 1;
        current_entry = current_max_entry/2;
      }
    }
  }
  return(shadowmatrix);
}