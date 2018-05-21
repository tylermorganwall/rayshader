#include <Rcpp.h>
using namespace Rcpp;

//assumes matrix already normalized
NumericVector vcrossnorm(NumericVector a, NumericVector b) {
  NumericVector result(3);
  result(0) = a(1) * b(2) - a(2) * b(1);
  result(1) = a(2) * b(0) - a(0) * b(2);
  result(2) = a(0) * b(1) - a(1) * b(0);
  result = result/sqrt(pow(result(0),2) + pow(result(1),2) + pow(result(2),2));
  return(result);
}

double dot_product(const NumericVector x, const NumericVector y) {
  return std::inner_product(x.begin(), x.end(), y.begin(), 0.0);
}

// [[Rcpp::export]]
NumericMatrix lambshade_cpp(NumericMatrix heightmap, NumericVector rayvector) {
  NumericMatrix shaded_matrix(heightmap.nrow(),heightmap.ncol());
  NumericVector storevector1(3);
  NumericVector storevector2(3);
  NumericVector tempvector1 = NumericVector::create(-1,0,0);
  NumericVector tempvector2 = NumericVector::create(0,1,0);
  NumericVector tempvector3 = NumericVector::create(1,0,0);
  NumericVector tempvector4 = NumericVector::create(0,-1,0);
  for(int col = 0; col < heightmap.ncol(); col++) {
    for(int row = 0; row < heightmap.nrow(); row++) {
      if(row != 0 && col != 0 && row < heightmap.nrow() - 1 && col < heightmap.ncol() - 1) {
        tempvector1(2) = heightmap(row,col)-heightmap(row,col+1);
        tempvector2(2) = heightmap(row,col)-heightmap(row+1,col);
        tempvector3(2) = heightmap(row,col)-heightmap(row,col-1);
        tempvector4(2) = heightmap(row,col)-heightmap(row-1,col);
        storevector1 = vcrossnorm(tempvector1,tempvector2);
        storevector2 = vcrossnorm(tempvector3,tempvector4);
        shaded_matrix(row,col) = dot_product((storevector1 + storevector2)/2, rayvector);
      } else {
        if((row == 0 || col == 0) && (row != heightmap.nrow() - 1 || col != heightmap.ncol() - 1)) {
          tempvector1(2) = heightmap(row,col)-heightmap(row,col+1);
          tempvector2(2) = heightmap(row,col)-heightmap(row+1,col);
          shaded_matrix(row,col) = dot_product(vcrossnorm(tempvector1,tempvector2), rayvector);
        } 
        if((row != 0 || col != 0) && (row == heightmap.nrow() - 1 || col == heightmap.ncol() - 1)) {
          tempvector3(2) = heightmap(row,col)-heightmap(row,col-1);
          tempvector4(2) = heightmap(row,col)-heightmap(row-1,col);
          shaded_matrix(row,col) = dot_product(vcrossnorm(tempvector3,tempvector4), rayvector);
        }
      }
    }
  }
  tempvector2(2) = heightmap(0,heightmap.ncol())-heightmap(1,heightmap.ncol());
  tempvector3(2) = heightmap(0,heightmap.ncol())-heightmap(0,heightmap.ncol()-1);
  shaded_matrix(0,heightmap.ncol()-1) = dot_product(vcrossnorm(tempvector3,tempvector2), rayvector);
  tempvector1(2) = heightmap(heightmap.nrow(),0)-heightmap(heightmap.nrow(),1);
  tempvector4(2) = heightmap(heightmap.nrow(),0)-heightmap(heightmap.nrow()-1,0);
  shaded_matrix(heightmap.nrow()-1,0) = dot_product(vcrossnorm(tempvector4,tempvector1), rayvector);
  
  return(shaded_matrix);
}