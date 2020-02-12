#include <Rcpp.h>
#include <RProgress.h>
using namespace Rcpp;

//assumes matrix already normalized
NumericVector vcrossnorm2(const NumericVector& a, const NumericVector& b) {
  NumericVector result(3);
  result(0) = a(1) * b(2) - a(2) * b(1);
  result(1) = a(2) * b(0) - a(0) * b(2);
  result(2) = a(0) * b(1) - a(1) * b(0);
  return(result);
}

// [[Rcpp::export]]
List calculate_normal_cpp(const NumericMatrix& heightmap, bool progbar) {
  NumericMatrix shaded_matrix(heightmap.nrow(),heightmap.ncol());
  NumericMatrix xmat(heightmap.nrow(),heightmap.ncol());
  NumericMatrix ymat(heightmap.nrow(),heightmap.ncol());
  NumericMatrix zmat(heightmap.nrow(),heightmap.ncol());
  NumericVector storevector1(3);
  NumericVector storevector2(3);
  NumericVector storevector3(3);
  NumericVector tempvector1 = NumericVector::create(-1,0,0);
  NumericVector tempvector2 = NumericVector::create(0,1,0);
  NumericVector tempvector3 = NumericVector::create(1,0,0);
  NumericVector tempvector4 = NumericVector::create(0,-1,0);
  RProgress::RProgress pb("Calculating Surface Normal [:bar] ETA: :eta");
  if(progbar) {
    pb.set_total(heightmap.ncol());
  }
  bool first_na  = false;
  bool second_na = false;
  bool third_na = false;
  bool fourth_na = false;
  
  for(int col = 0; col < heightmap.ncol(); col++) {
    if(progbar) {
      pb.tick();
    }
    for(int row = 0; row < heightmap.nrow(); row++) {
      if(row != 0 && col != 0 && row != heightmap.nrow() - 1 && col != heightmap.ncol() - 1) {
        tempvector1(2) = heightmap(row,col)-heightmap(row,col+1);
        tempvector2(2) = heightmap(row,col)-heightmap(row+1,col);
        tempvector3(2) = heightmap(row,col)-heightmap(row,col-1);
        tempvector4(2) = heightmap(row,col)-heightmap(row-1,col);
        first_na = std::isnan(tempvector1(2));
        second_na = std::isnan(tempvector2(2));
        third_na = std::isnan(tempvector3(2));
        fourth_na = std::isnan(tempvector4(2));
        if(!first_na && !second_na && !third_na && !fourth_na) {
          storevector1 = vcrossnorm2(tempvector1,tempvector2);
          storevector2 = vcrossnorm2(tempvector3,tempvector4);
          storevector3 = (storevector1 + storevector2);
        } else if (!first_na && !second_na && !third_na && fourth_na) {
          storevector1 = vcrossnorm2(tempvector1,tempvector2);
          storevector2 = vcrossnorm2(tempvector2,tempvector3);
          storevector3 = (storevector1 + storevector2);
        } else if (!first_na && !second_na && third_na && !fourth_na) {
          storevector1 = vcrossnorm2(tempvector4,tempvector1);
          storevector2 = vcrossnorm2(tempvector1,tempvector2);
          storevector3 = (storevector1 + storevector2);
        } else if (!first_na && second_na && !third_na && !fourth_na) {
          storevector1 = vcrossnorm2(tempvector3,tempvector4);
          storevector2 = vcrossnorm2(tempvector4,tempvector1);
          storevector3 = (storevector1 + storevector2);
        } else if (first_na && !second_na && !third_na && !fourth_na) {
          storevector1 = vcrossnorm2(tempvector2,tempvector3);
          storevector2 = vcrossnorm2(tempvector3,tempvector4);
          storevector3 = (storevector1 + storevector2);
        } else if (!first_na && !second_na) {
          storevector1 = vcrossnorm2(tempvector1,tempvector2);
          storevector2 = vcrossnorm2(tempvector1,tempvector2);
          storevector3 = (storevector1 + storevector2);
        } else if (!third_na && !fourth_na) {
          storevector1 = vcrossnorm2(tempvector3,tempvector4);
          storevector2 = vcrossnorm2(tempvector3,tempvector4);
          storevector3 = (storevector1 + storevector2);
        }
        storevector3 = storevector3/sqrt(pow(storevector3(0),2) + pow(storevector3(1),2) + pow(storevector3(2),2));
      } else {
        if((row == 0 || col == 0) && (row != heightmap.nrow() - 1 && col != heightmap.ncol() - 1)) {
          tempvector1(2) = heightmap(row,col)-heightmap(row,col+1);
          tempvector2(2) = heightmap(row,col)-heightmap(row+1,col);
          storevector3 = vcrossnorm2(tempvector1,tempvector2);
        } 
        if((row != 0 && col != 0) && (row == heightmap.nrow() - 1 || col == heightmap.ncol() - 1)) {
          tempvector3(2) = heightmap(row,col)-heightmap(row,col-1);
          tempvector4(2) = heightmap(row,col)-heightmap(row-1,col);
          storevector3 = vcrossnorm2(tempvector3,tempvector4);
        }
      }
      xmat(row,col) = storevector3(0);
      ymat(row,col) = storevector3(1);
      zmat(row,col) = -storevector3(2);
      first_na = false;
      second_na = false;
    }
  }
  tempvector2(2) = heightmap(0,heightmap.ncol()-1)-heightmap(1,heightmap.ncol()-1);
  tempvector3(2) = heightmap(0,heightmap.ncol()-1)-heightmap(0,heightmap.ncol()-2);
  storevector3 = vcrossnorm2(tempvector3,tempvector2);
  xmat(0,heightmap.ncol()-1) = storevector3(0);
  ymat(0,heightmap.ncol()-1) = storevector3(1);
  zmat(0,heightmap.ncol()-1) = -storevector3(2);
  tempvector1(2) = heightmap(heightmap.nrow()-1,0)-heightmap(heightmap.nrow()-1,1);
  tempvector4(2) = heightmap(heightmap.nrow()-1,0)-heightmap(heightmap.nrow()-2,0);
  storevector3 = vcrossnorm2(tempvector4,tempvector1);
  xmat(heightmap.nrow()-1,0) = storevector3(0);
  ymat(heightmap.nrow()-1,0) = storevector3(1);
  zmat(heightmap.nrow()-1,0) = -storevector3(2);
  return(List::create(_["x"] = xmat, _["y"] = ymat, _["z"] = zmat));
}