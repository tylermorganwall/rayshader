#include <Rcpp.h>
#include <RProgress.h>
using namespace Rcpp;

inline double bilinearinterp(double q11, double q12, double q21, double q22, double x1, double x2, double y1, double y2, double x, double y) {
  double x2x, y2y, yy1, xx1;
  x2x = x2 - x;
  y2y = y2 - y;
  yy1 = y - y1;
  xx1 = x - x1;
  return(q11 * x2x * y2y + q21 * xx1 * y2y + q12 * x2x * yy1 + q22 * xx1 * yy1);
}

// [[Rcpp::export]]
NumericMatrix rayshade_cpp(double sunangle, NumericVector anglebreaks, NumericMatrix& heightmap, 
                           double zscale, double maxsearch, const NumericMatrix cache_mask,
                           bool progbar) {
  
  double precisionval = 1e-10;
  double sinsunangle = sin(sunangle);
  double cossunangle = cos(sunangle);
  int numbercols = heightmap.ncol();
  int numberrows = heightmap.nrow();
  NumericMatrix shadowmatrix(numberrows,numbercols);
  std::fill(shadowmatrix.begin(), shadowmatrix.end(), 1.0);
  double xcoord, ycoord, tanangheight;
  double ceilxcoord,ceilycoord,floorxcoord,floorycoord;
  double maxdist = maxsearch;
  int numberangles = anglebreaks.size();
  bool breakloop;
  double maxheight = max(heightmap);
  RProgress::RProgress pb("Raytracing [:bar] ETA: :eta");
  
  if(progbar) {
    pb.set_total(numberrows);
  }
  
  for(int i = 0; i < numberrows; i++) {
    Rcpp::checkUserInterrupt();
    if(progbar) {
      pb.tick();
    }
    for(int j = 0; j < numbercols; j++) {
      if(cache_mask(i,j)) {
        for (int angentry = 0; angentry < numberangles; angentry++) {
          breakloop = FALSE;
          for(int k = 1; k < maxdist; k++) {
            xcoord = i + sinsunangle * k;
            ycoord = j + cossunangle * k;
            tanangheight = heightmap(i, j) + tan(anglebreaks[angentry]) * k * zscale;
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
                  shadowmatrix(i, j) =  shadowmatrix(i, j) - 1.0 / numberangles;
                  break;
                }
              }
              
              if(fabs(floorxcoord - ceilxcoord) < precisionval && floorycoord != ceilycoord) {
                if (tanangheight < (heightmap(floorxcoord,ceilycoord) - heightmap(floorxcoord,floorycoord))*(ycoord-floorycoord) + heightmap(floorxcoord,floorycoord)) {
                  shadowmatrix(i, j) =  shadowmatrix(i, j) - 1.0 / numberangles;
                  break;
                }
              }
              
              if(floorxcoord != ceilxcoord && fabs(floorycoord - ceilycoord) < precisionval) {
                if (tanangheight < (heightmap(ceilxcoord,floorycoord) - heightmap(floorxcoord,floorycoord))*(xcoord-floorxcoord) + heightmap(floorxcoord,floorycoord)) {
                  shadowmatrix(i, j) =  shadowmatrix(i, j) - 1.0 / numberangles;
                  break;
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
                shadowmatrix(i, j) =  shadowmatrix(i, j) - 1.0 / numberangles;
                break;
              } 
              if(k == (maxdist - 1)) breakloop = true;
            }
          }
          if(breakloop) break;
        }
      }
    }
  }
  return(shadowmatrix);
}