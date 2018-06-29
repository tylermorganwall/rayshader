#include <Rcpp.h>
using namespace Rcpp;

void check_entries(NumericMatrix& storematrix, int row, int col, int value, int replacement) {
  while(storematrix(row,col+1) != 0) {
    storematrix(row,col++) = replacement;
  }
  while(storematrix(row,col-1) != 0) {
    storematrix(row,col--) = replacement;
  }
}

struct QueueEntry {
  int row;
  int col;
};

void queue_fill(NumericMatrix& max_z_matrix, NumericMatrix& storematrix,
                int row, int col, int group_value) {
  if(max_z_matrix(row,col) != 0 && storematrix(row,col) == 0) {
    std::list<QueueEntry> queue;
    storematrix(row,col) = group_value;
    QueueEntry first_entry;
    first_entry.row = row;
    first_entry.col = col;
    queue.push_back(first_entry);
    while(!queue.empty()) {
      if(max_z_matrix(queue.front().row + 1, queue.front().col) != 0 && 
         storematrix(queue.front().row + 1, queue.front().col) == 0) {
        storematrix(queue.front().row + 1, queue.front().col) = group_value;
        QueueEntry entry;
        entry.row = queue.front().row + 1;
        entry.col = queue.front().col;
        queue.push_back(entry);
      }
      if(max_z_matrix(queue.front().row,queue.front().col+1) != 0 && 
         storematrix(queue.front().row,queue.front().col+1) == 0) {
        storematrix(queue.front().row, queue.front().col+1) = group_value;
        QueueEntry entry;
        entry.row = queue.front().row;
        entry.col = queue.front().col + 1;
        queue.push_back(entry);
      }
      if(max_z_matrix(queue.front().row-1,queue.front().col) != 0 && 
         storematrix(queue.front().row-1,queue.front().col) == 0) {
        storematrix(queue.front().row-1, queue.front().col) = group_value;
        QueueEntry entry;
        entry.row = queue.front().row - 1;
        entry.col = queue.front().col;
        queue.push_back(entry);
      }
      if(max_z_matrix(queue.front().row,queue.front().col-1) != 0 && 
         storematrix(queue.front().row,queue.front().col-1) == 0) {
        storematrix(queue.front().row, queue.front().col-1) = group_value;
        QueueEntry entry;
        entry.row = queue.front().row;
        entry.col = queue.front().col - 1;
        queue.push_back(entry);
      }
      queue.pop_front();
    }
  }
}

// [[Rcpp::export]]
NumericMatrix fill_find_groups(NumericMatrix max_z_matrix) {
  NumericMatrix store(max_z_matrix.nrow(),max_z_matrix.ncol());
  
  int counter = 1;
  for(int i=1; i < max_z_matrix.nrow()-2; i++) {
      for(int j=1; j < max_z_matrix.ncol()-2; j++) {
      if(max_z_matrix(i,j) != 0) {
        queue_fill(max_z_matrix, store, i, j, counter);
        counter++;
      }
    }
  }
  return(store);
}


// [[Rcpp::export]]
NumericMatrix find_groups_cpp(NumericMatrix max_z_matrix) {
  NumericMatrix store(max_z_matrix.nrow(),max_z_matrix.ncol());
  
  int counter = 0;
  
  for(int i=1; i < max_z_matrix.nrow()-2; i++) {
    for(int j=1; j < max_z_matrix.ncol()-2; j++) {
      if(max_z_matrix(i,j-1) == 0 && max_z_matrix(i,j) == 1){
        counter++;
        store(i,j) = counter;
      } 
      if(max_z_matrix(i,j-1) == 1 && max_z_matrix(i,j) == 1){
        store(i,j) = counter;
      }
    }
  }
  for(int i=1; i < store.nrow()-3; i++) {
    for(int j=1; j < store.ncol()-3; j++) {
      if(store(i,j)  && store(i+1,j) && store(i,j) != store(i+1,j)) {
        check_entries(store, i+1, j, store(i+1,j), store(i,j));
        store(i+1,j) = store(i,j);
      }
    }
  }
  for(int i=store.nrow()-3; i > 1; i--) {
    for(int j=store.ncol()-3; j > 1; j--) {
      if(store(i,j) && store(i,j-1) && store(i,j) != store(i,j-1)) {
        store(i,j-1) = store(i,j);
      }
      if(store(i,j)  && store(i-1,j) && store(i,j) != store(i-1,j)) {
        check_entries(store, i-1, j, store(i-1,j), store(i,j));
        store(i-1,j) = store(i,j);
      }
    }
  }
  return(store);
}