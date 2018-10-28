#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
Matrix<RTYPE> vec2matrix(Vector<RTYPE> x, int nrow, int ncol) {
  return Matrix<RTYPE>(nrow, ncol, x.begin());
}


// [[Rcpp::export]]
List make_base_cpp(NumericVector side1, NumericVector side2, 
                            NumericVector side3, NumericVector side4,
                            NumericMatrix& heightmap,
                            double basedepth) {
  std::vector<NumericMatrix> vertices;
  for(int i = 0; i < side1.length()-1; i++) {
    vertices.push_back(vec2matrix(NumericVector::create(1,1,1, side1(i),basedepth,basedepth, -i-1,-i-2,-i-1),3,3));
    vertices.push_back(vec2matrix(NumericVector::create(1,1,1, side1(i),side1(i+1),basedepth, -i-1,-i-2,-i-2),3,3));
  }
  for(int i = 0; i < side2.length()-1; i++) {
    vertices.push_back(vec2matrix(NumericVector::create(i+1,i+1,i+2, side2(i),basedepth,basedepth,  -1,-1,-1),3,3));
    vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+2, side2(i),basedepth,side2(i+1), -1,-1,-1),3,3));
  }
  for(int i = 0; i < side3.length()-1; i++) {
    vertices.push_back(vec2matrix(NumericVector::create(side4.length(),side4.length(),side4.length(), side3(i),basedepth,basedepth, -i-1, -i-1, -i-2),3,3));
    vertices.push_back(vec2matrix(NumericVector::create(side4.length(),side4.length(),side4.length(), side3(i),basedepth,side3(i+1), -i-1,-i-2,-i-2),3,3));
  }
  for(int i = 0; i < side4.length()-1; i++) {
    vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+1, side4(i),basedepth,basedepth,-side3.length(),-side3.length(),-side3.length()),3,3));
    vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+2,  side4(i),side4(i+1),basedepth, -side3.length(),-side3.length(),-side3.length()),3,3));
  }
  vertices.push_back(vec2matrix(NumericVector::create(1,heightmap.nrow(),heightmap.nrow(), basedepth,basedepth,basedepth,-1,-heightmap.ncol(),-1),3,3));
  vertices.push_back(vec2matrix(NumericVector::create(1,heightmap.nrow(),1,basedepth,basedepth,basedepth,-heightmap.ncol(),-heightmap.ncol(),-1),3,3));
  List vectorlist = wrap(vertices);
  return(vectorlist);
}

// [[Rcpp::export]]
List make_water_cpp(NumericVector side1, NumericVector side2, 
                   NumericVector side3, NumericVector side4,
                   NumericMatrix& heightmap,
                   double waterheight) {
  std::vector<NumericMatrix> vertices;
  double endcoord, begincoord, heighttemp;
  for(int i = 0; i < side1.length()-1; i++) {
    if(side1(i) < waterheight) {
      if(side1(i+1) > waterheight) {
        endcoord = -(double)i - 1 - (waterheight - side1(i))/(side1(i+1)-side1(i));
      } else {
        endcoord = -i - 2;
      }
      vertices.push_back(vec2matrix(NumericVector::create(1,1,1, side1(i),waterheight,waterheight, -i-1,-i-1,endcoord),3,3));
    }
    if(side1(i+1) < waterheight) {
      if(side1(i) > waterheight) {
        begincoord = -(double)i - 1 - (waterheight - side1(i))/(side1(i+1)-side1(i));
        heighttemp = waterheight;
      } else {
        begincoord = -i-1;
        heighttemp = side1(i);
      }
      vertices.push_back(vec2matrix(NumericVector::create(1,1,1, heighttemp,waterheight,side1(i+1), begincoord,-i-2,-i-2),3,3));
    }
  }
  for(int i = 0; i < side2.length()-1; i++) {
    if(side2(i) < waterheight) {
      if(side2(i+1) > waterheight) {
        endcoord = (double)i + 1 + (waterheight - side2(i))/(side2(i+1)-side2(i));
      } else {
        endcoord = i+2;
      }
      vertices.push_back(vec2matrix(NumericVector::create(i+1,endcoord,i+1, side2(i),waterheight,waterheight,  -1,-1,-1),3,3));
    }
    if(side2(i+1) < waterheight) {
      if(side2(i) > waterheight) {
        begincoord = (double)i + 1 + (waterheight - side2(i))/(side2(i+1)-side2(i));
        heighttemp = waterheight;
      } else {
        begincoord = i+1;
        heighttemp = side2(i);
      }
      vertices.push_back(vec2matrix(NumericVector::create(begincoord,i+2,i+2, heighttemp,side2(i+1),waterheight, -1,-1,-1),3,3));
    }
  }
  for(int i = 0; i < side3.length()-1; i++) {
    if(side3(i) < waterheight) {
      if(side3(i+1) > waterheight) {
        endcoord = -(double)i - 1 - (waterheight - side3(i))/(side3(i+1)-side3(i));
      } else {
        endcoord = -i - 2;
      }
      vertices.push_back(vec2matrix(NumericVector::create(heightmap.nrow(),heightmap.nrow(),heightmap.nrow(), side3(i),waterheight,waterheight, -i-1, endcoord, -i-1),3,3));
    }
    if(side3(i+1) < waterheight) {
      if(side3(i) > waterheight) {
        begincoord = -(double)i - 1 - (waterheight - side3(i))/(side3(i+1)-side3(i));
        heighttemp = waterheight;
      } else {
        begincoord = -i-1;
        heighttemp = side3(i);
      }
      vertices.push_back(vec2matrix(NumericVector::create(heightmap.nrow(),heightmap.nrow(),heightmap.nrow(), heighttemp,side3(i+1),waterheight, begincoord,-i-2,-i-2),3,3));
    }
  }
  for(int i = 0; i < side4.length()-1; i++) {
    if(side4(i) < waterheight) {
      if(side4(i+1) > waterheight) {
        endcoord = (double)i + 1 + (waterheight - side4(i))/(side4(i+1)-side4(i));
      } else {
        endcoord = i+2;
      }
      vertices.push_back(vec2matrix(NumericVector::create(i+1,i+1,endcoord, side4(i),waterheight,waterheight,-heightmap.ncol(),-heightmap.ncol(),-heightmap.ncol()),3,3));
    }
    if(side4(i+1) < waterheight) {
      if(side4(i) > waterheight) {
        begincoord = (double)i + 1 + (waterheight - side4(i))/(side4(i+1)-side4(i));
        heighttemp = waterheight;
      } else {
        begincoord = i+1;
        heighttemp = side4(i);
      }
      vertices.push_back(vec2matrix(NumericVector::create(begincoord,i+2,i+2,  heighttemp,waterheight,side4(i+1), -heightmap.ncol(),-heightmap.ncol(),-heightmap.ncol()),3,3));
    }
  }
  List vectorlist = wrap(vertices);
  return(vectorlist);
}

// [[Rcpp::export]]
List make_waterlines_cpp(NumericVector side1, NumericVector side2, 
                    NumericVector side3, NumericVector side4,
                    NumericMatrix& heightmap,
                    double waterdepth) {
  std::vector<NumericMatrix> vertices;
  bool drawing = false;
  double startcoord, endcoord = 1;
  
  for(int i = 0; i < side1.length(); i++) {
    if(side1(i) < waterdepth && !drawing) {
      if(i != 0) {
        startcoord = ((double)i-1) + (waterdepth - side1(i-1))/(side1(i)-side1(i-1));
      } else {
        startcoord = 0;
      }
      drawing = true;
    }
    if((side1(i) > waterdepth || i == side1.length()-1) && drawing) {
      drawing = false;
      if(i != side1.length()-1) {
        endcoord = -(double)i - (waterdepth - side1(i-1))/(side1(i)-side1(i-1));
      } else {
        endcoord = -side1.length();
      }
      vertices.push_back(vec2matrix(NumericVector::create(1,1,waterdepth,waterdepth,-startcoord-1,endcoord),2,3));
    }
  }
  drawing = false;
  startcoord = 1;
  for(int i = 0; i < side2.length(); i++) {
    if(side2(i) < waterdepth && !drawing) {
      if(i != 0) {
        startcoord = ((double)i-1) + (waterdepth - side2(i-1))/(side2(i)-side2(i-1));
      } else {
        startcoord = 0;
      }
      drawing = true;
    }
    if((side2(i)  > waterdepth || i == side2.length()-1) && drawing) {
      drawing = false;
      if(i != side2.length()-1) {
        endcoord = (double)i + (waterdepth - side2(i-1))/(side2(i)-side2(i-1));
      } else {
        endcoord = side2.length();
      }
      vertices.push_back(vec2matrix(NumericVector::create(startcoord+1,endcoord,waterdepth,waterdepth,-1,-1),2,3));
    }
  }
  drawing = false;
  startcoord = 1;
  for(int i = 0; i < side3.length(); i++) {
    if(side3(i) < waterdepth && !drawing) {
      if(i != 0) {
        startcoord = ((double)i-1) + (waterdepth - side3(i-1))/(side3(i)-side3(i-1));
      } else {
        startcoord = 0;
      }
      drawing = true;
    }
    if((side3(i)  > waterdepth || i == side3.length()-1) && drawing) {
      drawing = false;
      if(i != side3.length()-1) {
        endcoord = -(double)i - (waterdepth - side3(i-1))/(side3(i)-side3(i-1));
      } else {
        endcoord = -side3.length();
      }
      vertices.push_back(vec2matrix(NumericVector::create(heightmap.nrow(),heightmap.nrow(),waterdepth,waterdepth,-startcoord-1,endcoord),2,3));
    }
  }
  drawing = false;
  startcoord = 1;
  for(int i = 0; i < side4.length(); i++) {
    if(side4(i) < waterdepth && !drawing) { 
      if(i != 0) {
        startcoord = ((double)i-1) + (waterdepth - side4(i-1))/(side4(i)-side4(i-1));
      } else {
        startcoord = 0;
      }
      drawing = true;
    } 
    if((side4(i) > waterdepth || i == side4.length()-1) && drawing) {
      drawing = false;
      if(i != side4.length()-1) {
        endcoord = (double)i + (waterdepth - side4(i-1))/(side4(i)-side4(i-1));
      } else {
        endcoord = side4.length();
      }
      vertices.push_back(vec2matrix(NumericVector::create(startcoord+1,endcoord,waterdepth,waterdepth,-heightmap.ncol(),-heightmap.ncol()),2,3));
    }
  }
  List vectorlist = wrap(vertices);
  return(vectorlist);
}