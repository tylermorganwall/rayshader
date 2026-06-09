#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
using namespace Rcpp;

template <int RTYPE>
Matrix<RTYPE> vec2matrix(Vector<RTYPE> x, int nrow, int ncol) {
  return Matrix<RTYPE>(nrow, ncol, x.begin());
}

// [[Rcpp::export]]
List make_surface_cpp(NumericMatrix& heightmap,
                      LogicalMatrix& na_matrix,
                      NumericMatrix& normalsx,
                      NumericMatrix& normalsy,
                      NumericMatrix& normalsz,
                      double basedepth) {
  std::vector<NumericMatrix> vertices;
  std::vector<NumericMatrix> normals;
  std::vector<NumericMatrix> texcoords;
  
  int rows = heightmap.nrow();
  int cols = heightmap.ncol();
  for(int j = 0; j < rows-1; j++) {
    for(int i = 0; i < cols-1; i++) {
      if(!na_matrix(j,i) && !na_matrix(j, i + 1) && !na_matrix(j+1, i) && !na_matrix(j+1,i+1)) {
        vertices.push_back(vec2matrix(NumericVector::create(j,j+1,j, 
                                                            heightmap(j,i),heightmap(j+1,i),heightmap(j,i+1), 
                                                            i,i,i+1),3,3));
        vertices.push_back(vec2matrix(NumericVector::create(j+1,j+1,j, 
                                                            heightmap(j+1,i),heightmap(j+1,i+1),heightmap(j,i+1), 
                                                            i,i+1,i+1),3,3));
        normals.push_back(vec2matrix(NumericVector::create(normalsx(j,i),normalsx(j+1,i),normalsx(j,i+1), 
                                                           normalsy(j,i),normalsy(j+1,i),normalsy(j,i+1), 
                                                           normalsz(j,i),normalsz(j+1,i),normalsz(j,i+1)),3,3));
        normals.push_back(vec2matrix(NumericVector::create(normalsx(j+1,i),normalsx(j+1,i+1),normalsx(j,i+1), 
                                                           normalsy(j+1,i),normalsy(j+1,i+1),normalsy(j,i+1), 
                                                           normalsz(j+1,i),normalsz(j+1,i+1),normalsz(j,i+1)),3,3));
        texcoords.push_back(vec2matrix(NumericVector::create((float)j/(float)rows, (float)(j+1)/(float)rows,(float)j/(float)rows,
                                                             (float)i/(float)cols,(float)i/(float)cols,(float)(i+1)/(float)cols),3,2));
        texcoords.push_back(vec2matrix(NumericVector::create((float)(j+1)/(float)rows,(float)(j+1)/(float)rows,(float)j/(float)rows,
                                                               (float)i/(float)cols,(float)(i+1)/(float)cols,(float)(i+1)/(float)cols),3,2));
      } 
    }
  }
  List vectorlist = wrap(vertices);
  List normallist = wrap(normals);
  List texcoordlist = wrap(texcoords);
  
  return(List::create(_["vertices"] = vectorlist, _["normals"] = normallist, _["texcoords"] = texcoordlist));
}

// [[Rcpp::export]]
List make_base_cpp(NumericMatrix& heightmap,
                    LogicalMatrix& na_matrix,
                    double basedepth) {
  std::vector<NumericMatrix> vertices;
  // std::vector<NumericMatrix> normals;
  std::vector<bool> horizontal;
  std::vector<double> height;
  
  int rows = heightmap.nrow();
  int cols = heightmap.ncol();
  for(int j = 0; j < rows-1; j++) {
    for(int i = 0; i < cols-1; i++) {
      if(j == 0) {
        if(!std::isnan(heightmap(0+j,i)) && !std::isnan(heightmap(0+j,i+1))) {
          if(!na_matrix(j,i) && !na_matrix(j, i + 1)) {
            vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,1+j, heightmap(0+j,i),basedepth,basedepth, -i-1,-i-2,-i-1),3,3));
            vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,1+j, heightmap(0+j,i),heightmap(0+j,i+1),basedepth, -i-1,-i-2,-i-2),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(-1,-1,-1, 0,0,0, 0,0,0),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(-1,-1,-1, 0,0,0, 0,0,0),3,3));
            horizontal.push_back(true);
            horizontal.push_back(true);
            height.push_back( heightmap(0+j,i) - basedepth);
            height.push_back( heightmap(0+j,i+1) - basedepth);

          }
        }
      } else {
        if(!std::isnan(heightmap(0+j,i)) && !std::isnan(heightmap(0+j,i+1))) {
          if((!na_matrix(j,i) && na_matrix(j - 1, i) && !na_matrix(j, i+1)) || (!na_matrix(j,i+1) && na_matrix(j - 1, i+1))) {
            vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,1+j, heightmap(0+j,i),basedepth,basedepth, -i-1,-i-2,-i-1),3,3));
            vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,1+j, heightmap(0+j,i),heightmap(0+j,i+1),basedepth, -i-1,-i-2,-i-2),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(-1,-1,-1, 0,0,0, 0,0,0),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(-1,-1,-1, 0,0,0, 0,0,0),3,3));
            horizontal.push_back(true);
            horizontal.push_back(true);
            height.push_back( heightmap(0+j,i)  - basedepth);
            height.push_back( heightmap(0+j,i+1)  - basedepth);
          }
        }
      }
    }
  }
  
  for(int j = 0; j < rows; j++) {
    for(int i = 0; i < cols-1; i++) {
      if(j == rows - 1) {
        if(!std::isnan(heightmap(j,i)) && !std::isnan(heightmap(j,i+1))) {
          if(!na_matrix(j,i+1)) {
            vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,1+j, heightmap(j,i),basedepth,basedepth, -i-1, -i-1, -i-2),3,3));
            vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,1+j, heightmap(j,i),basedepth,heightmap(j,i+1), -i-1,-i-2,-i-2),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(1,1,1, 0,0,0, 0,0,0),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(1,1,1, 0,0,0, 0,0,0),3,3));
            horizontal.push_back(true);
            horizontal.push_back(true);
            height.push_back( heightmap(j,i)  - basedepth);
            height.push_back( heightmap(j,i+1)  - basedepth);
          }
        }
      } else {
        if(!std::isnan(heightmap(j,i)) && !std::isnan(heightmap(j,i+1))) {
          if((!na_matrix(j,i) && na_matrix(j+1, i) && !na_matrix(j, i+1)) || (!na_matrix(j,i+1) && na_matrix(j+1, i+1))) {
            vertices.push_back(vec2matrix(NumericVector::create(j+1,j+1,j+1, heightmap(j,i),basedepth,basedepth, -i-1, -i-1, -i-2),3,3));
            vertices.push_back(vec2matrix(NumericVector::create(j+1,j+1,j+1, heightmap(j,i),basedepth,heightmap(j,i+1), -i-1,-i-2,-i-2),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(1,1,1, 0,0,0, 0,0,0),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(1,1,1, 0,0,0, 0,0,0),3,3));
            horizontal.push_back(true);
            horizontal.push_back(true);
            height.push_back( heightmap(0+j,i)  - basedepth);
            height.push_back( heightmap(0+j,i+1)  - basedepth);
          }
        }
      }
    }
  }
  for(int j = 0; j < cols-1; j++) {
    for(int i = 0; i < rows-1; i++) {
      if(j == 0) {
        if(!std::isnan(heightmap(i,j)) && !std::isnan(heightmap(i+1,j))) {
          if(!na_matrix(i+1,j)) {
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+1,i+2, heightmap(i,0+j),basedepth,basedepth,  -1+j,-1+j,-1+j),3,3));
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+2, heightmap(i,0+j),basedepth,heightmap(i+1,0+j), -1+j,-1+j,-1+j),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(0,0,0, 0,0,0,-1,-1,-1),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(0,0,0, 0,0,0,-1,-1,-1),3,3));
            horizontal.push_back(false);
            horizontal.push_back(false);
            height.push_back( heightmap(i,j) - basedepth);
            height.push_back( heightmap(i+1,j) - basedepth);
          }
        }
      } else {
        if(!std::isnan(heightmap(i,j)) && !std::isnan(heightmap(i+1,j))) {
          if((!na_matrix(i,j) && na_matrix(i,j - 1) && !na_matrix(i+1,j)) || (!na_matrix(i,j) && na_matrix(i + 1, j-1) && !na_matrix(i+1,j))) {
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+1,i+2, heightmap(i,0+j),basedepth,basedepth,  -1-j,-1-j,-1-j),3,3));
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+2, heightmap(i,0+j),basedepth,heightmap(i+1,0+j), -1-j,-1-j,-1-j),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(0,0,0, 0,0,0,-1,-1,-1),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(0,0,0, 0,0,0,-1,-1,-1),3,3));
            horizontal.push_back(false);
            horizontal.push_back(false);
            height.push_back( heightmap(i,j) - basedepth);
            height.push_back( heightmap(i+1,j) - basedepth);
          }
        }
      }
    }
  }
  for(int j = 0; j < cols; j++) {
    for(int i = 0; i < rows-1; i++) {
      if(j == cols - 1) {
        if(!std::isnan(heightmap(i,j)) && !std::isnan(heightmap(i+1,j))) {
          if(!na_matrix(i,j) && !na_matrix(i+1,j)) {
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+1, heightmap(i,j),basedepth,basedepth,-j-1,-j-1,-j-1),3,3));
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+2, heightmap(i,j),heightmap(i+1,j),basedepth, -j-1,-j-1,-j-1),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(0,0,0, 0,0,0,1,1,1),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(0,0,0, 0,0,0,1,1,1),3,3));
            horizontal.push_back(false);
            horizontal.push_back(false);
            height.push_back( heightmap(i,j) - basedepth);
            height.push_back( heightmap(i+1,j) - basedepth);
          }
        }
      } else {
        if(!std::isnan(heightmap(i,j)) && !std::isnan(heightmap(i+1,j))) {
          if((!na_matrix(i,j) && na_matrix(i,j + 1) && !na_matrix(i+1,j)) || (!na_matrix(i,j) && na_matrix(i+1, j+1) && !na_matrix(i+1,j))) {
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+1, heightmap(i,j),basedepth,basedepth,-j-1,-j-1,-j-1),3,3));
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+2,i+2, heightmap(i,j),heightmap(i+1,j),basedepth, -j-1,-j-1,-j-1),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(0,0,0, 0,0,0,1,1,1),3,3));
            // normals.push_back(vec2matrix(NumericVector::create(0,0,0, 0,0,0,1,1,1),3,3));
            horizontal.push_back(false);
            horizontal.push_back(false);
            height.push_back( heightmap(i,j) - basedepth);
            height.push_back( heightmap(i+1,j) - basedepth);
          }
        }
      }
    }
  }
  List vectorlist = wrap(vertices);
  // List normallist = wrap(normals);
  LogicalVector directionlist = wrap(horizontal);
  NumericVector heights = wrap(height);
  
  return(List::create(_["vertices"] = vectorlist, //_["normals"] = normallist, 
                      _["is_horizontal"] = directionlist, _["edge_heights"] = heights));
}

// [[Rcpp::export]]
List make_water_cpp(NumericMatrix& heightmap,
                    LogicalMatrix& na_matrix,
                    double waterheight) {
  int rows = heightmap.nrow();
  int cols = heightmap.ncol();
  std::vector<NumericMatrix> vertices;
  double endcoord, begincoord, heighttemp;
  int offset = 1;
  double adjust;
  for(int j = 0; j < rows - 1; j++) {
    offset = 0;
    if(j != 0) {
      offset = 1;
    }
    for(int i = 0; i < cols - 1; i++) {
      if(!std::isnan(heightmap(j,i)) && !std::isnan(heightmap(j,i+1))) {
        if(((na_matrix(j-offset,i) && !na_matrix(j,i) && !na_matrix(j,i+1)) || (na_matrix(j-offset,i+1) && !na_matrix(j,i+1))) || j == 0) {
          if(heightmap(0+j,i) < waterheight && heightmap(0+j,i+1) < waterheight) {
            adjust = (waterheight - heightmap(0+j,i))/(heightmap(0+j,i+1)-heightmap(0+j,i));
            if(heightmap(0+j,i+1) > waterheight && fabs(adjust) < 1) {
              endcoord = -(double)i - adjust;
            } else {
              endcoord = -i - 1;
            }
            vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,1+j, heightmap(0+j,i),waterheight,waterheight, -i,-i,endcoord),3,3));
            if(heightmap(0+j,i) > waterheight && fabs(adjust) < 1) {
              begincoord = -(double)i - adjust;
              heighttemp = waterheight;
            } else {
              begincoord = -i;
              heighttemp = heightmap(0+j,i);
            }
            vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,1+j, heighttemp,waterheight,heightmap(0+j,i+1), begincoord,-i-1,-i-1),3,3));
          }
        }
      }
    }
  }
  for(int j = 0; j < cols - 1; j++) {
    offset = 0;
    if(j != 0) {
      offset = 1;
    }
    for(int i = 0; i < rows-1; i++) {
      if(!std::isnan(heightmap(i,j)) && !std::isnan(heightmap(i+1,j))) {
        if(((na_matrix(i,j-offset) && !na_matrix(i,j) && !na_matrix(i+1,j)) || (na_matrix(i+1,j-offset) && !na_matrix(i+1,j))) || j == 0) {
          if(heightmap(i,0+j) < waterheight && heightmap(i+1,0+j) < waterheight) {
            adjust = (waterheight - heightmap(i,0+j))/(heightmap(i+1,0+j)-heightmap(i,0+j));
            if(heightmap(i+1,0+j) > waterheight && fabs(adjust) < 1) {
              endcoord = (double)i + 1 + adjust;
            } else {
              endcoord = i+2;
            }
            vertices.push_back(vec2matrix(NumericVector::create(i+1,endcoord,i+1, heightmap(i,0+j),waterheight,waterheight,  -j,-j,-j),3,3));
            if(heightmap(i,0+j) > waterheight && fabs(adjust) < 1) {
              begincoord = (double)i + 1 + adjust;
              heighttemp = waterheight;
            } else {
              begincoord = i+1;
              heighttemp = heightmap(i,0+j);
            }
            vertices.push_back(vec2matrix(NumericVector::create(begincoord,i+2,i+2, heighttemp,heightmap(i+1,0+j),waterheight, -j,-j,-j),3,3));
          }
        }
      }
    }
  }
  for(int j = 0; j < rows; j++) {
    offset = 0;
    if(j != rows - 1) {
      offset = 1;
    }
    for(int i = 0; i < cols - 1; i++) {
      if(!std::isnan(heightmap(j,i)) && !std::isnan(heightmap(j,i+1))) {
        if(((na_matrix(j+offset,i) && !na_matrix(j,i) && !na_matrix(j,i+1)) || (na_matrix(j+offset,i+1) && !na_matrix(j,i+1))) || j == rows-1) {
          if(heightmap(j,i) < waterheight && heightmap(j,i+1) < waterheight) {
            adjust = (waterheight - heightmap(j,i))/(heightmap(j,i+1)-heightmap(j,i));
            if(heightmap(j,i+1) > waterheight && fabs(adjust) < 1) {
              endcoord = -(double)i - adjust;
            } else {
              endcoord = -i - 1;
            }
            vertices.push_back(vec2matrix(NumericVector::create(j+1,j+1,j+1, heightmap(j,i),waterheight,waterheight, -i, endcoord, -i),3,3));
            if(heightmap(j,i) > waterheight && fabs(adjust) < 1) {
              begincoord = -(double)i - adjust;
              heighttemp = waterheight;
            } else {
              begincoord = -i;
              heighttemp = heightmap(j,i);
            }
            vertices.push_back(vec2matrix(NumericVector::create(j+1,j+1,j+1, heighttemp,heightmap(j,i+1),waterheight, begincoord,-i-1,-i-1),3,3));
          }
        }
      }
    }
  }
  for(int j = 0; j < cols; j++) {
    offset = 0;
    if(j != cols - 1) {
      offset = 1;
    }
    for(int i = 0; i < rows-1; i++) {
      if(!std::isnan(heightmap(i,j)) && !std::isnan(heightmap(i+1,j))) {
        if(((na_matrix(i,j+offset) && !na_matrix(i,j) && !na_matrix(i+1,j)) || (na_matrix(i+1,j+offset) && !na_matrix(i+1,j))) || j == cols-1) {
          if(heightmap(i,j) < waterheight && heightmap(i+1,j) < waterheight) {
            adjust = (waterheight - heightmap(i,j))/(heightmap(i+1,j)-heightmap(i,j));
            if(heightmap(i+1,j) > waterheight && fabs(adjust) < 1) {
              endcoord = (double)i + adjust;
            } else {
              endcoord = i+2;
            }
            vertices.push_back(vec2matrix(NumericVector::create(i+1,i+1,endcoord, heightmap(i,j),waterheight,waterheight,-j,-j,-j),3,3));
            if(heightmap(i,j) > waterheight && fabs(adjust) < 1) {
              begincoord = (double)i  + adjust;
              heighttemp = waterheight;
            } else {
              begincoord = i+1;
              heighttemp = heightmap(i,j);
            }
            vertices.push_back(vec2matrix(NumericVector::create(begincoord,i+2,i+2,  heighttemp,waterheight,heightmap(i+1,j), -j,-j,-j),3,3));
          }
        }
      }
    }
  }
  List vectorlist = wrap(vertices);
  return(vectorlist);
}

struct WaterMeshVertex {
  double row;
  double col;
  double terrain_height;
  double water_height;
  int edge_mask;

  WaterMeshVertex() :
    row(0),
    col(0),
    terrain_height(0),
    water_height(0),
    edge_mask(0) {}

  WaterMeshVertex(double row_value,
                  double col_value,
                  double terrain_height_value,
                  double water_height_value,
                  int edge_mask_value) :
    row(row_value),
    col(col_value),
    terrain_height(terrain_height_value),
    water_height(water_height_value),
    edge_mask(edge_mask_value) {}
};

struct WaterMeshPiece {
  std::vector<WaterMeshVertex> polygon;
  std::vector<WaterMeshVertex> top_triangles;
  std::vector<size_t> edge_keys;
  int cell_row;
  int cell_col;
  int component;

  WaterMeshPiece() :
    cell_row(0),
    cell_col(0),
    component(0) {}
};

struct WaterMeshDisjointSet {
  std::vector<int> parent;
  std::vector<int> rank;

  explicit WaterMeshDisjointSet(int n) : parent(n), rank(n, 0) {
    for(int i = 0; i < n; i++) {
      parent[i] = i;
    }
  }

  int find(int x) {
    if(parent[x] != x) {
      parent[x] = find(parent[x]);
    }
    return(parent[x]);
  }

  void unite(int a, int b) {
    int root_a = find(a);
    int root_b = find(b);
    if(root_a == root_b) {
      return;
    }
    if(rank[root_a] < rank[root_b]) {
      std::swap(root_a, root_b);
    }
    parent[root_b] = root_a;
    if(rank[root_a] == rank[root_b]) {
      rank[root_a]++;
    }
  }
};

static const double WATER_MESH_EPS = 1e-12;

bool water_mesh_finite(double value) {
  return(std::isfinite(value));
}

bool water_mesh_same_point(const WaterMeshVertex& a, const WaterMeshVertex& b) {
  return(std::fabs(a.row - b.row) < WATER_MESH_EPS &&
         std::fabs(a.col - b.col) < WATER_MESH_EPS);
}

double water_mesh_triangle_area2(const WaterMeshVertex& a,
                                 const WaterMeshVertex& b,
                                 const WaterMeshVertex& c) {
  return((b.row - a.row) * (c.col - a.col) -
         (b.col - a.col) * (c.row - a.row));
}

size_t water_mesh_edge_key(int edge, int cell_row, int cell_col, int rows, int cols) {
  int orientation = 0;
  int row = cell_row;
  int col = cell_col;
  if(edge == 0) {
    orientation = 0;
    row = cell_row;
    col = cell_col;
  } else if(edge == 1) {
    orientation = 1;
    row = cell_row;
    col = cell_col + 1;
  } else if(edge == 2) {
    orientation = 0;
    row = cell_row + 1;
    col = cell_col;
  } else {
    orientation = 1;
    row = cell_row;
    col = cell_col;
  }
  return((static_cast<size_t>(orientation) * static_cast<size_t>(rows + 1) +
          static_cast<size_t>(row)) *
         static_cast<size_t>(cols + 1) +
         static_cast<size_t>(col));
}

bool water_mesh_neighbor_valid(const std::vector<std::vector<int> >& valid_cells,
                               int edge,
                               int cell_row,
                               int cell_col,
                               int cell_rows,
                               int cell_cols) {
  int neighbor_row = cell_row;
  int neighbor_col = cell_col;
  if(edge == 0) {
    neighbor_row = cell_row - 1;
  } else if(edge == 1) {
    neighbor_col = cell_col + 1;
  } else if(edge == 2) {
    neighbor_row = cell_row + 1;
  } else {
    neighbor_col = cell_col - 1;
  }
  if(neighbor_row < 0 || neighbor_col < 0 ||
     neighbor_row >= cell_rows || neighbor_col >= cell_cols) {
    return(false);
  }
  return(valid_cells[neighbor_row][neighbor_col] != 0);
}

WaterMeshVertex water_mesh_interpolate_vertex(const WaterMeshVertex& a,
                                              const WaterMeshVertex& b,
                                              double diff_a,
                                              double diff_b,
                                              int edge) {
  double denom = diff_b - diff_a;
  double t = 0.5;
  if(std::fabs(denom) > WATER_MESH_EPS && water_mesh_finite(denom)) {
    t = -diff_a / denom;
  }
  if(t < 0) {
    t = 0;
  } else if(t > 1) {
    t = 1;
  }
  WaterMeshVertex vertex;
  vertex.row = a.row + t * (b.row - a.row);
  vertex.col = a.col + t * (b.col - a.col);
  vertex.terrain_height = a.terrain_height +
    t * (b.terrain_height - a.terrain_height);
  vertex.water_height = a.water_height + t * (b.water_height - a.water_height);
  vertex.edge_mask = 1 << edge;
  return(vertex);
}

void water_mesh_clean_polygon(std::vector<WaterMeshVertex>& polygon) {
  std::vector<WaterMeshVertex> cleaned;
  for(size_t i = 0; i < polygon.size(); i++) {
    if(cleaned.empty() || !water_mesh_same_point(cleaned.back(), polygon[i])) {
      cleaned.push_back(polygon[i]);
    } else {
      cleaned.back().edge_mask |= polygon[i].edge_mask;
    }
  }
  if(cleaned.size() > 1 && water_mesh_same_point(cleaned.front(), cleaned.back())) {
    cleaned.front().edge_mask |= cleaned.back().edge_mask;
    cleaned.pop_back();
  }
  polygon = cleaned;
}

bool water_mesh_add_unique_edge(std::vector<size_t>& edge_keys, size_t edge_key) {
  if(std::find(edge_keys.begin(), edge_keys.end(), edge_key) == edge_keys.end()) {
    edge_keys.push_back(edge_key);
    return(true);
  }
  return(false);
}

std::vector<WaterMeshVertex> water_mesh_polygon3(const WaterMeshVertex& a,
                                                 const WaterMeshVertex& b,
                                                 const WaterMeshVertex& c) {
  std::vector<WaterMeshVertex> polygon;
  polygon.reserve(3);
  polygon.push_back(a);
  polygon.push_back(b);
  polygon.push_back(c);
  return(polygon);
}

void water_mesh_append_top_triangle(WaterMeshPiece& piece,
                                    WaterMeshVertex a,
                                    WaterMeshVertex b,
                                    WaterMeshVertex c) {
  double area2 = water_mesh_triangle_area2(a, b, c);
  if(std::fabs(area2) < WATER_MESH_EPS) {
    return;
  }
  if(area2 > 0) {
    std::swap(b, c);
  }
  piece.top_triangles.push_back(a);
  piece.top_triangles.push_back(b);
  piece.top_triangles.push_back(c);
}

void water_mesh_append_piece(std::vector<WaterMeshPiece>& pieces,
                             std::vector<std::vector<int> >& edge_piece_indices,
                             std::vector<WaterMeshVertex> polygon,
                             int cell_row,
                             int cell_col,
                             int rows,
                             int cols) {
  water_mesh_clean_polygon(polygon);
  if(polygon.size() < 3) {
    return;
  }
  WaterMeshPiece piece;
  piece.polygon = polygon;
  piece.cell_row = cell_row;
  piece.cell_col = cell_col;
  for(size_t i = 1; i + 1 < polygon.size(); i++) {
    water_mesh_append_top_triangle(piece, polygon[0], polygon[i], polygon[i + 1]);
  }
  if(piece.top_triangles.empty()) {
    return;
  }
  int piece_index = pieces.size();
  for(size_t i = 0; i < polygon.size(); i++) {
    size_t next_index = (i + 1) % polygon.size();
    int common_edges = polygon[i].edge_mask & polygon[next_index].edge_mask;
    for(int edge = 0; edge < 4; edge++) {
      if(common_edges & (1 << edge)) {
        size_t edge_key = water_mesh_edge_key(edge, cell_row, cell_col, rows, cols);
        if(water_mesh_add_unique_edge(piece.edge_keys, edge_key)) {
          edge_piece_indices[edge_key].push_back(piece_index);
        }
        break;
      }
    }
  }
  pieces.push_back(piece);
}

void water_mesh_append_vertex_values(std::vector<double>& values,
                                     const WaterMeshVertex& vertex,
                                     bool bottom,
                                     double row_center,
                                     double col_center) {
  values.push_back(vertex.row - row_center);
  values.push_back(bottom ? vertex.terrain_height : vertex.water_height);
  values.push_back(vertex.col - col_center);
}

NumericMatrix water_mesh_values_to_matrix(const std::vector<double>& values) {
  int vertex_count = values.size() / 3;
  NumericMatrix matrix(vertex_count, 3);
  for(int i = 0; i < vertex_count; i++) {
    matrix(i, 0) = values[3 * i];
    matrix(i, 1) = values[3 * i + 1];
    matrix(i, 2) = values[3 * i + 2];
  }
  return(matrix);
}

// [[Rcpp::export]]
List make_water_mesh_cpp(NumericMatrix& heightmap,
                         NumericMatrix& waterheight) {
  int rows = heightmap.nrow();
  int cols = heightmap.ncol();
  if(waterheight.nrow() != rows || waterheight.ncol() != cols) {
    stop("`waterheight` must have the same dimensions as `heightmap`.");
  }
  if(rows < 2 || cols < 2) {
    return(List::create(_["vertices"] = List::create(),
                        _["lines"] = NumericMatrix(0, 3)));
  }

  int cell_rows = rows - 1;
  int cell_cols = cols - 1;
  size_t edge_key_count = 2 * static_cast<size_t>(rows + 1) *
    static_cast<size_t>(cols + 1);
  std::vector<std::vector<int> > valid_cells(cell_rows, std::vector<int>(cell_cols, 0));
  std::vector<std::vector<int> > edge_piece_indices(edge_key_count);
  std::vector<WaterMeshPiece> pieces;

  for(int row = 0; row < cell_rows; row++) {
    for(int col = 0; col < cell_cols; col++) {
      valid_cells[row][col] =
        water_mesh_finite(heightmap(row, col)) &&
        water_mesh_finite(heightmap(row, col + 1)) &&
        water_mesh_finite(heightmap(row + 1, col + 1)) &&
        water_mesh_finite(heightmap(row + 1, col)) &&
        water_mesh_finite(waterheight(row, col)) &&
        water_mesh_finite(waterheight(row, col + 1)) &&
        water_mesh_finite(waterheight(row + 1, col + 1)) &&
        water_mesh_finite(waterheight(row + 1, col));
    }
  }

  for(int row = 0; row < cell_rows; row++) {
    for(int col = 0; col < cell_cols; col++) {
      if(!valid_cells[row][col]) {
        continue;
      }
      WaterMeshVertex corners[4];
      corners[0] = WaterMeshVertex(
        static_cast<double>(row),
        static_cast<double>(col),
        heightmap(row, col),
        waterheight(row, col),
        (1 << 3) | (1 << 0)
      );
      corners[1] = WaterMeshVertex(
        static_cast<double>(row),
        static_cast<double>(col + 1),
        heightmap(row, col + 1),
        waterheight(row, col + 1),
        (1 << 0) | (1 << 1)
      );
      corners[2] = WaterMeshVertex(
        static_cast<double>(row + 1),
        static_cast<double>(col + 1),
        heightmap(row + 1, col + 1),
        waterheight(row + 1, col + 1),
        (1 << 1) | (1 << 2)
      );
      corners[3] = WaterMeshVertex(
        static_cast<double>(row + 1),
        static_cast<double>(col),
        heightmap(row + 1, col),
        waterheight(row + 1, col),
        (1 << 2) | (1 << 3)
      );

      double diff[4];
      bool wet[4];
      int wet_count = 0;
      for(int i = 0; i < 4; i++) {
        diff[i] = corners[i].terrain_height - corners[i].water_height;
        wet[i] = diff[i] < 0;
        if(wet[i]) {
          wet_count++;
        }
      }
      if(wet_count == 0) {
        continue;
      }
      WaterMeshVertex intersections[4];
      for(int edge = 0; edge < 4; edge++) {
        int next = (edge + 1) % 4;
        intersections[edge] = water_mesh_interpolate_vertex(
          corners[edge],
          corners[next],
          diff[edge],
          diff[next],
          edge
        );
      }

      if(wet_count == 2 && wet[0] && wet[2] && !wet[1] && !wet[3]) {
        water_mesh_append_piece(
          pieces,
          edge_piece_indices,
          water_mesh_polygon3(corners[0], intersections[0], intersections[3]),
          row,
          col,
          rows,
          cols
        );
        water_mesh_append_piece(
          pieces,
          edge_piece_indices,
          water_mesh_polygon3(corners[2], intersections[2], intersections[1]),
          row,
          col,
          rows,
          cols
        );
        continue;
      }
      if(wet_count == 2 && wet[1] && wet[3] && !wet[0] && !wet[2]) {
        water_mesh_append_piece(
          pieces,
          edge_piece_indices,
          water_mesh_polygon3(corners[1], intersections[1], intersections[0]),
          row,
          col,
          rows,
          cols
        );
        water_mesh_append_piece(
          pieces,
          edge_piece_indices,
          water_mesh_polygon3(corners[3], intersections[3], intersections[2]),
          row,
          col,
          rows,
          cols
        );
        continue;
      }

      std::vector<WaterMeshVertex> polygon;
      for(int i = 0; i < 4; i++) {
        int next = (i + 1) % 4;
        if(wet[i]) {
          polygon.push_back(corners[i]);
        }
        if(wet[i] != wet[next]) {
          polygon.push_back(intersections[i]);
        }
      }
      water_mesh_append_piece(pieces, edge_piece_indices, polygon, row, col, rows, cols);
    }
  }

  if(pieces.empty()) {
    return(List::create(_["vertices"] = List::create(),
                        _["lines"] = NumericMatrix(0, 3)));
  }

  WaterMeshDisjointSet disjoint_set(pieces.size());
  for(size_t key = 0; key < edge_piece_indices.size(); key++) {
    if(edge_piece_indices[key].size() < 2) {
      continue;
    }
    int first_piece = edge_piece_indices[key][0];
    for(size_t i = 1; i < edge_piece_indices[key].size(); i++) {
      disjoint_set.unite(first_piece, edge_piece_indices[key][i]);
    }
  }

  std::vector<int> roots(pieces.size());
  std::vector<int> component_roots;
  for(size_t i = 0; i < pieces.size(); i++) {
    roots[i] = disjoint_set.find(i);
    std::vector<int>::iterator root_it = std::find(
      component_roots.begin(),
      component_roots.end(),
      roots[i]
    );
    if(root_it == component_roots.end()) {
      component_roots.push_back(roots[i]);
      pieces[i].component = component_roots.size() - 1;
    } else {
      pieces[i].component = root_it - component_roots.begin();
    }
  }
  for(size_t i = 0; i < pieces.size(); i++) {
    std::vector<int>::iterator root_it = std::find(
      component_roots.begin(),
      component_roots.end(),
      roots[i]
    );
    pieces[i].component = root_it - component_roots.begin();
  }

  std::vector<std::vector<double> > component_values(component_roots.size());
  std::vector<double> line_values;
  double row_center = (rows - 1) / 2.0;
  double col_center = (cols - 1) / 2.0;

  for(size_t piece_index = 0; piece_index < pieces.size(); piece_index++) {
    WaterMeshPiece& piece = pieces[piece_index];
    std::vector<double>& values = component_values[piece.component];
    for(size_t i = 0; i < piece.top_triangles.size(); i++) {
      water_mesh_append_vertex_values(
        values,
        piece.top_triangles[i],
        false,
        row_center,
        col_center
      );
    }

    int cell_row = piece.cell_row;
    int cell_col = piece.cell_col;

    for(size_t vertex_index = 0; vertex_index < piece.polygon.size(); vertex_index++) {
      size_t next_index = (vertex_index + 1) % piece.polygon.size();
      WaterMeshVertex first = piece.polygon[vertex_index];
      WaterMeshVertex second = piece.polygon[next_index];
      if(water_mesh_same_point(first, second)) {
        continue;
      }
      int common_edges = first.edge_mask & second.edge_mask;
      bool shared_grid_edge = false;
      bool internal_edge = false;
      bool neighbor_valid = false;
      for(int edge = 0; edge < 4; edge++) {
        if(common_edges & (1 << edge)) {
          size_t edge_key = water_mesh_edge_key(edge, cell_row, cell_col, rows, cols);
          shared_grid_edge = true;
          internal_edge = edge_piece_indices[edge_key].size() > 1;
          neighbor_valid = water_mesh_neighbor_valid(
            valid_cells,
            edge,
            cell_row,
            cell_col,
            cell_rows,
            cell_cols
          );
          break;
        }
      }
      if(internal_edge) {
        continue;
      }
      water_mesh_append_vertex_values(line_values, first, false, row_center, col_center);
      water_mesh_append_vertex_values(line_values, second, false, row_center, col_center);
      if(shared_grid_edge && !neighbor_valid) {
        WaterMeshVertex first_bottom = first;
        WaterMeshVertex second_bottom = second;
        water_mesh_append_vertex_values(values, first, false, row_center, col_center);
        water_mesh_append_vertex_values(values, first_bottom, true, row_center, col_center);
        water_mesh_append_vertex_values(values, second, false, row_center, col_center);
        water_mesh_append_vertex_values(values, second, false, row_center, col_center);
        water_mesh_append_vertex_values(values, first_bottom, true, row_center, col_center);
        water_mesh_append_vertex_values(values, second_bottom, true, row_center, col_center);
      }
    }
  }

  List component_list(component_values.size());
  for(size_t i = 0; i < component_values.size(); i++) {
    component_list[i] = water_mesh_values_to_matrix(component_values[i]);
  }

  return(List::create(_["vertices"] = component_list,
                      _["lines"] = water_mesh_values_to_matrix(line_values)));
}

// [[Rcpp::export]]
List make_waterlines_cpp(NumericMatrix& heightmap,
                        LogicalMatrix& na_matrix,
                         double waterdepth) {
  std::vector<NumericMatrix> vertices;
  int rows = heightmap.nrow();
  int cols = heightmap.ncol();
  int offset, offset2 = 0;
  int offsetside, offsetside2 = 0;
  bool drawing = false;
  double startcoord, endcoord = 1;
  double adjust;
  
  for(int j = 0; j < rows; j++) {
    drawing = false;
    if(j != 0) {
      offset = 1;
    } else {
      offset = 0;
    }
    if(j != rows-1) {
      offset2 = 1;
    } else {
      offset2 = 0;
    }
    for(int i = 0; i < cols; i++) {
      if(i != 0) {
        offsetside = 1;
      } else {
        offsetside = 0;
      }
      if(i != cols-1) {
        offsetside2 = 1;
      } else {
        offsetside2 = 0;
      }
      //Edges
      if(drawing && (j == 0 || j == rows - 1)) {
        if((heightmap(j,i) > waterdepth || i == cols-1 ) || na_matrix(j,i)) {
          drawing = false;
          if((heightmap(j,i) > waterdepth || i == cols-1 ) && !na_matrix(j,i)) {
            if(i != cols-1) {
              double diff = heightmap(j,i)-heightmap(j,i-1);
              double adjustment_factor;
              if(diff == 0) {
                adjustment_factor = 0;
              } else {
                adjustment_factor = (waterdepth - heightmap(j,i-1))/diff;
              }
              endcoord = -(double)i - adjustment_factor;
            } else {
              endcoord = -cols;
            }
          } else {
            if(i != cols-1) {
              endcoord = -(double)i;
            } else {
              endcoord = -cols;
            }
          }
          vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,waterdepth,waterdepth,-startcoord-1,endcoord),2,3));
        }
      }
      if(!drawing && (j == 0 || j == rows - 1)) {
        if(((heightmap(j,i) < waterdepth) || 
           (na_matrix(j,i-offsetside) && heightmap(j,i+offsetside2) < waterdepth)) &&
           ((j == 0 && !na_matrix(1, i)) || (j == rows - 1 && !na_matrix(rows - 2, i)))) {
          if(!na_matrix(j,i-offsetside)) {
            if(i != 0) {
              double diff = heightmap(j,i)-heightmap(j,i-1);
              double adjustment_factor;
              if(diff == 0) {
                adjustment_factor = 0;
              } else {
                adjustment_factor = (waterdepth - heightmap(j,i-1))/diff;
              }
              startcoord = ((double)i-1) + adjustment_factor;
            } else {
              startcoord = 0;
            }
          } else {
            if(i != 0) {
              if(na_matrix(j,i)) {
                startcoord = (double)i+1;
              } else {
                startcoord = (double)i;
              }
            } else {
              startcoord = 0;
            }
          }
          drawing = true;
        }
      }
      //Interior
      if(drawing && j != 0 && j != rows - 1) {
        //Finish drawing if not NA AND
          //the back left or back right entries are NA AND the front left and front right entries are NOT NA OR
          //It is NA right in front of that entry
        if((!na_matrix(j,i) &&
           (((na_matrix(j+offset2,i-offsetside) || na_matrix(j-offset,i-offsetside)) && (!na_matrix(j+offset2,i+offsetside2) || !na_matrix(j-offset,i+offsetside2))) ||
           na_matrix(j,i+offsetside))) || i == cols - 1) {
          drawing = false;
          if(i != cols-1) {
            adjust = (waterdepth - heightmap(j,i-1))/(heightmap(j,i)-heightmap(j,i-1));
            if(heightmap(j,i) > waterdepth && fabs(adjust) < 1) {
              endcoord = (double)i + adjust;
            } else {
              endcoord = (double)i+1;
            }
          } else {
            endcoord = cols;
          }
          vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,waterdepth,waterdepth,-startcoord-1,-endcoord),2,3));
        }
      }
      if(!drawing && j != 0 && j != rows - 1) {
        //Start drawing if under water or the next space is underwater, but only if:
        //The matrix is not NA in the next entry AND
        //the current entry is not NA AND
        //the left OR right OR left front OR right front is NA
        if((heightmap(j,i) < waterdepth || (heightmap(j,i) >= waterdepth && heightmap(j,i+offsetside2) < waterdepth)) &&
           !na_matrix(j,i+offsetside2) &&
           (!na_matrix(j,i) && (na_matrix(j-offset,i) || na_matrix(j+offset2,i) || na_matrix(j+offset2,i+offsetside2) || na_matrix(j-offset,i+offsetside2)))) {
          if(i != 0) {
            adjust = (waterdepth - heightmap(j,i-1))/(heightmap(j,i)-heightmap(j,i-1));
            if(heightmap(j,i) > waterdepth && fabs(adjust) < 1) {
              startcoord = ((double)i-1) + adjust;
            } else {
              startcoord = (double)i;
            }
          } else {
            startcoord = 0;
          }
          drawing = true;
        }
      }
    }
  }
  for(int j = 0; j < cols; j++) {
    drawing = false;
    if(j != 0) {
      offset = 1;
    } else {
      offset = 0;
    }
    if(j != cols-1) {
      offset2 = 1;
    } else {
      offset2 = 0;
    }
    for(int i = 0; i < rows; i++) {
      if(i != 0) {
        offsetside = 1;
      } else {
        offsetside = 0;
      }
      if(i != rows-1) {
        offsetside2 = 1;
      } else {
        offsetside2 = 0;
      }
      //Edges
      if(drawing && (j == 0 || j == cols - 1)) {
        if(heightmap(i,j)  > waterdepth || i == rows-1 || na_matrix(i,j)) {
          drawing = false;
          if((heightmap(i,j)  > waterdepth || i == rows-1) && !na_matrix(i,j)) {
            if(i != rows-1) {
              double diff = heightmap(i,j)-heightmap(i-1,j);
              double adjustment_factor;
              if(diff == 0) {
                adjustment_factor = 0;
              } else {
                adjustment_factor = (waterdepth - heightmap(i-1,j))/diff;
              }
              endcoord = (double)i + adjustment_factor;
            } else {
              endcoord = rows;
            }
          } else {
            if(i != rows-1) {
              endcoord = (double)i;
            } else {
              endcoord = rows;
            }
          }
          vertices.push_back(vec2matrix(NumericVector::create(startcoord+1,endcoord,waterdepth,waterdepth,-1-j,-1-j),2,3));
        }
      }
      if(!drawing && (j == 0 || j == cols - 1)) {
        if((heightmap(i,j) < waterdepth || 
           (na_matrix(i - offsetside,j) && heightmap(i + offsetside2,j) < waterdepth)) &&
           ((j == 0 && !na_matrix(i, 1)) || (j == cols - 1 && !na_matrix(i, cols - 2)))) {
          if(!na_matrix(i-offsetside,j)) {
            if(i != 0) {
              double diff = heightmap(i,j)-heightmap(i-1,j);
              double adjustment_factor;
              if(diff == 0) {
                adjustment_factor = 0;
              } else {
                adjustment_factor = (waterdepth - heightmap(i-1,j))/diff;
              }
              startcoord = ((double)i-1) + adjustment_factor;
            } else {
              startcoord = 0;
            }
          } else {
            if(i != 0) {
              if(na_matrix(i,j)) {
                startcoord = (double)i+1;
              } else {
                startcoord = (double)i;
              }
            } else {
              startcoord = 0;
            }
          }
          drawing = true;
        }
      }
      //Interior
      if(drawing && j != 0 && j != cols - 1) {
        //Finish drawing if not NA AND
          //the back left or back right entries are NA AND the front left and front right entries are NOT NA OR
          //It is NA right in front of that entry
        if((!na_matrix(i,j) &&
           (((na_matrix(i-offsetside,j-offset) || na_matrix(i-offsetside,j+offset2)) && (!na_matrix(i+offsetside2,j-offset) || !na_matrix(i+offsetside2,j+offset2))) ||
           na_matrix(i+offsetside,j))) || i == rows - 1) {
          drawing = false;
          if(i != rows-1) {
            adjust = (waterdepth - heightmap(i-1,j))/(heightmap(i,j)-heightmap(i-1,j));
            if(heightmap(i,j) > waterdepth && fabs(adjust) < 1) {
              endcoord = (double)i + adjust;
            } else {
              endcoord = (double)i+1;
            }
          } else {
            endcoord = rows;
          }
          vertices.push_back(vec2matrix(NumericVector::create(startcoord+1,endcoord,waterdepth,waterdepth,-1-j,-1-j),2,3));
        }
      }
      if(!drawing && j != 0 && j != cols - 1) {
        //Start drawing if under water or the next space is underwater, but only if:
        //The matrix is not NA in the next entry AND
        //the current entry is not NA AND
        //the left OR right OR left front OR right front is NA
        if((heightmap(i,j) < waterdepth || (heightmap(i,j) >= waterdepth && heightmap(i+offsetside2,j) < waterdepth) ) && //Check depths
           !na_matrix(i+offsetside2,j) && //Not NA in the next entry
           (!na_matrix(i,j) && (na_matrix(i,j-offset) || na_matrix(i,j+offset2) || na_matrix(i+offsetside2,j-offset) || na_matrix(i+offsetside2,j+offset2)))) {
          if(i != 0) {
            adjust = (waterdepth - heightmap(i-1,j))/(heightmap(i,j)-heightmap(i-1,j));
            if(heightmap(i,j) > waterdepth && fabs(adjust) < 1) {
              startcoord = (double)i-1 + adjust;
            } else {
              startcoord = (double)i;
            }
          } else {
            startcoord = 0;
          }
          drawing = true;
          }
        }
      }
    }
  List vectorlist = wrap(vertices);
  return(vectorlist);
}


// [[Rcpp::export]]
List make_baselines_cpp(NumericMatrix& heightmap,
                         LogicalMatrix& na_matrix,
                         double waterdepth) {
  std::vector<NumericMatrix> vertices;
  int rows = heightmap.nrow();
  int cols = heightmap.ncol();
  int offset, offset2 = 0;
  int offsetside, offsetside2 = 0;
  bool drawing = false;
  double startcoord, endcoord = 1;

  for(int j = 0; j < rows; j++) {
    drawing = false;
    if(j != 0) {
      offset = 1;
    } else {
      offset = 0;
    }
    if(j != rows-1) {
      offset2 = 1;
    } else {
      offset2 = 0;
    }
    for(int i = 0; i < cols; i++) {
      if(i != 0) {
        offsetside = 1;
      } else {
        offsetside = 0;
      }
      if(i != cols-1) {
        offsetside2 = 1;
      } else {
        offsetside2 = 0;
      }
      //Edges
      if(drawing && (j == 0 || j == rows - 1)) {
        if(i == cols-1 || na_matrix(j,i)) {
          drawing = false;
          if(i != cols-1) {
            endcoord = -(double)i;
          } else {
            endcoord = -cols;
          }
          vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,waterdepth,waterdepth,-startcoord-1,endcoord),2,3));
        }
      }
      if(!drawing && (j == 0 || j == rows - 1)) {
        if((i == 0 || na_matrix(j,i-offsetside)) && !na_matrix(j,i)) {
          if(i != 0) {
            startcoord = (double)i;
          } else {
            startcoord = 0;
          }
          drawing = true;
        }
      }
      //Interior
      if(drawing && j != 0 && j != rows - 1) {
        //Finish drawing if not NA AND
        //the back left or back right entries are NA AND the front left and front right entries are NOT NA OR
        //It is NA right in front of that entry
        if((!na_matrix(j,i) &&
           (((na_matrix(j+offset2,i-offsetside) || na_matrix(j-offset,i-offsetside)) && (!na_matrix(j+offset2,i+offsetside2) || !na_matrix(j-offset,i+offsetside2))) ||
           na_matrix(j,i+offsetside2))) || i == cols - 1) {
          drawing = false;
          if(i != cols-1) {
            endcoord = (double)i+1;
          } else {
            endcoord = cols;
          }
          vertices.push_back(vec2matrix(NumericVector::create(1+j,1+j,waterdepth,waterdepth,-startcoord-1,-endcoord),2,3));
        }
      }
      if(!drawing && j != 0 && j != rows - 1) {
        //Start drawing if under water or the next space is underwater, but only if:
        //The matrix is not NA in the next entry AND
        //the current entry is not NA AND
        //the left OR right OR left front OR right front is NA
        if((!na_matrix(j,i+offsetside2) &&
           (!na_matrix(j,i) && (na_matrix(j-offset,i) ||
           na_matrix(j+offset2,i) ||
           na_matrix(j+offset2,i+offsetside2) ||
           na_matrix(j-offset,i+offsetside2))))) {
          if(i != 0) {
            startcoord = (double)i;
          } else {
            startcoord = 0;
          }
          drawing = true;
        }
      }
    }
  }
  for(int j = 0; j < cols; j++) {
    drawing = false;
    if(j != 0) {
      offset = 1;
    } else {
      offset = 0;
    }
    if(j != cols-1) {
      offset2 = 1;
    } else {
      offset2 = 0;
    }
    for(int i = 0; i < rows; i++) {
      if(i != 0) {
        offsetside = 1;
      } else {
        offsetside = 0;
      }
      if(i != rows-1) {
        offsetside2 = 1;
      } else {
        offsetside2 = 0;
      }
      //Edges
      if(drawing && (j == 0 || j == cols - 1)) {
        if(i == rows-1 || na_matrix(i,j)) {
          drawing = false;
          if(i != rows-1) {
            endcoord = (double)i;
          } else {
            endcoord = rows;
          }
          vertices.push_back(vec2matrix(NumericVector::create(startcoord+1,endcoord,waterdepth,waterdepth,-1-j,-1-j),2,3));
        }
      }
      if(!drawing && (j == 0 || j == cols - 1)) {
        if((i == 0 || na_matrix(i-offsetside,j)) && !na_matrix(i,j)) {
          if(i != 0) {
            startcoord = (double)i;
          } else {
            startcoord = 0;
          }
          drawing = true;
        }
      }
      //Interior
      if(drawing && j != 0 && j != cols - 1) {
        //Finish drawing if not NA AND
        //the back left or back right entries are NA AND the front left and front right entries are NOT NA OR
        //It is NA right in front of that entry
        if((!na_matrix(i,j) &&
           (((na_matrix(i-offsetside,j-offset) || na_matrix(i-offsetside,j+offset2)) && (!na_matrix(i+offsetside2,j-offset) || !na_matrix(i+offsetside2,j+offset2))) ||
           na_matrix(i+offsetside2,j)))  || i == rows - 1) {
          drawing = false;
          if(i != rows-1) {
            endcoord = (double)i+1;
          } else {
            endcoord = rows;
          }
          vertices.push_back(vec2matrix(NumericVector::create(startcoord+1,endcoord,waterdepth,waterdepth,-1-j,-1-j),2,3));
        }
      }
      if(!drawing && j != 0 && j != cols - 1) {
        //Start drawing if under water or the next space is underwater, but only if:
        //The matrix is not NA in the next entry AND
        //the current entry is not NA AND
        //the left OR right OR left front OR right front is NA
        if(!na_matrix(i+offsetside2,j) &&
           (!na_matrix(i,j) && (na_matrix(i,j-offset) || na_matrix(i,j+offset2) || na_matrix(i+offsetside2,j-offset) || na_matrix(i+offsetside2,j+offset2)))) {
          if(i != 0) {
            startcoord = (double)i;
          } else {
            startcoord = 0;
          }
          drawing = true;
        }
      }
    }
  }
  List vectorlist = wrap(vertices);
  return(vectorlist);
}
