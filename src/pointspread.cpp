#include <RcppArmadillo.h>
#include <RProgress.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double cubic_interpolate(double p0, double p1, double p2, double p3, double x) {
  return(p1 + 0.5 * x*(p2 - p0 + 
         x*(2.0*p0 - 5.0*p1 + 4.0*p2 - p3 + 
         x*(3.0*(p1 - p2) + p3 - p0))));
}

// [[Rcpp::export]]
double bicubic_interpolate(NumericMatrix p, double x, double y) {
  double arr[4];
  arr[0] = cubic_interpolate(p(0,0),p(0,1),p(0,2),p(0,3), y);
  arr[1] = cubic_interpolate(p(1,0),p(1,1),p(1,2),p(1,3), y);
  arr[2] = cubic_interpolate(p(2,0),p(2,1),p(2,2),p(2,3), y);
  arr[3] = cubic_interpolate(p(3,0),p(3,1),p(3,2),p(3,3), y);
  return(cubic_interpolate(arr[0], arr[1], arr[2], arr[3], x));
}


// [[Rcpp::export]]
arma::mat subsample(arma::mat& circle, int size) {
  int binsize = circle.n_cols/size;
  arma::mat subsampled(size,size,arma::fill::zeros);
  if(size == 1) {
    subsampled(0,0) = 1;
    return(subsampled);
  }
  arma::mat temp;
  for(int i = 0; i < size; i++) {
    for(int j = 0; j < size; j++) {
      if(i != size - 1 && j != size - 1) {
        temp = circle.submat(binsize*i,binsize*j,binsize*(i+1),binsize*(j+1));
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (i == size - 1 && j != size - 1) {
        temp = circle.submat(binsize*i,binsize*j,circle.n_rows-1,binsize*(j+1));
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (j == size - 1 && i != size - 1) {
        temp = circle.submat(binsize*i,binsize*j,binsize*(i+1),circle.n_cols-1);
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (i == size - 1 && j == size - 1) {
        temp = circle.submat(binsize*i,binsize*j,circle.n_rows-1,circle.n_cols-1);
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      }
    }
  }
  return(subsampled);
}

// [[Rcpp::export]]
arma::mat subsample_rect(arma::mat& rect, int binsx, int binsy) {
  int binsizex = rect.n_rows/binsx;
  int binsizey = rect.n_cols/binsy;
  arma::mat subsampled(binsx,binsy,arma::fill::zeros);
  arma::mat temp;
  for(int i = 0; i < binsx; i++) {
    for(int j = 0; j < binsy; j++) {
      if(i != binsx - 1 && j != binsy - 1) {
        temp = rect.submat(binsizex*i,binsizey*j,binsizex*(i+1),binsizey*(j+1));
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (i == binsx - 1 && j != binsy - 1) {
        temp = rect.submat(binsizex*i,binsizey*j,rect.n_rows-1,binsizey*(j+1));
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (j == binsy - 1 && i != binsx - 1) {
        temp = rect.submat(binsizex*i,binsizey*j,binsizex*(i+1),rect.n_cols-1);
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      } else if (i == binsx - 1 && j == binsy - 1) {
        temp = rect.submat(binsizex*i,binsizey*j,rect.n_rows-1,rect.n_cols-1);
        subsampled(i,j) = accu(temp)/(temp.n_elem);
      }
    }
  }
  return(subsampled);
}

// [[Rcpp::export]]
arma::mat gen_circle_psf(const double radius) {
  int size = ceil(radius * 2);
  if(size < 6) {
    size = 7;
  }
  if((size % 2) == 0) {
    size++;
  }
  arma::mat kernel(size,size);
  if(radius == 0) {
    arma::mat zero(1,1,arma::fill::ones);
    return(zero);
  }
  double mean = (size-1)/2;
  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size; ++j) {
      kernel(i,j) = pow(((double)i - mean),2.0) + pow(((double)j - mean),2.0) < pow(radius,2.0) ? 1.0 : 0.0;
    }
  }
  return(kernel);
}

// [[Rcpp::export]]
arma::mat gen_ellipse(const double intensity, double width, double height) {
  arma::mat ellipse(width,height);
  for (int i = 0; i < width; ++i) {
    for (int j = 0; j < height; ++j) {
      ellipse(i,j) = pow(((double)i - width/2 + 0.5), 2.0) * pow(height/2,2.0) + 
                    pow(((double)j - height/2 + 0.5),2.0) * pow(width/2,2.0) > pow(width * height, 2.0)/16 ? intensity : 0.0;
    }
  }
  return(ellipse);
}
// [[Rcpp::export]]
bool is_inside(double sizehex, double positionx, double positiony, double sinval, double cosval) {
  double num1 = fabs(cosval* (positionx - sizehex) - sinval* (positiony - sizehex));
  double num2 = fabs(sinval* (positionx - sizehex) + cosval* (positiony - sizehex));
  double minval = sizehex - num1 < sizehex / 2 ?  sizehex - num1 : sizehex / 2;
  return(num2 < sqrt(3.0) * minval);
}

// [[Rcpp::export]]
arma::mat gen_hex_psf(const double radius, const double rotation) {
  int size = ceil(radius * 2);
  if(size < 6) {
    size = 7;
  }
  if((size % 2) == 0) {
    size++;
  }
  arma::mat kernel(size,size);
  if(radius == 0) {
    arma::mat zero(1,1,arma::fill::zeros);
    return(zero);
  }
  const double sinval = sin(rotation);
  const double cosval = cos(rotation);
  double mean = (size-1)/2;
  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size; ++j) {
      kernel(i,j) = is_inside(mean, i, j, sinval, cosval) ? 1.0 : 0.0;
    }
  }
  return(kernel);
}

//TODO: Create submat of depth buffer, boolean 1-0 if within "mixing" distance, multiply by temp2

// [[Rcpp::export]]
arma::mat psf(const arma::mat& image, const IntegerMatrix blurmatrix,
              const arma::mat& depthmap, double depth, const arma::mat custombokeh,
              int type, double bokehintensity, double bokehlimit,
              double rotation, bool progbar, int channel) {
  int maxsteps = max(blurmatrix);
  int rows = image.n_rows;
  int cols = image.n_cols;
  arma::mat tempkernel;
  std::vector<arma::mat> kernels;
  std::vector<arma::mat> alphakernels;
  arma::mat mask;
  if(type == 0) {
    mask = gen_circle_psf(500);
  } else if (type == 1) {
    mask = gen_hex_psf(500, rotation);
  } else if (type == 2) {
    mask = custombokeh;
  }
  int counter = 0;
  for(int i = 0; i < maxsteps+1; i++) {
    tempkernel = subsample(mask, 2*i+1);
    tempkernel = tempkernel/accu(tempkernel);
    kernels.push_back(tempkernel);
    counter++;
  }
  int halfwidth = (kernels[counter-1].n_cols-1)/2;
  arma::mat temp;
  arma::mat temp2;
  arma::mat depthmask;
  arma::mat blurmask;
  arma::mat result(image.n_rows, image.n_cols, arma::fill::zeros);
  arma::mat normalize(result.n_rows, result.n_cols, arma::fill::zeros);
  std::string pbtext;
  if(channel == 1) {
    pbtext =  "Rendering Bokeh 1/3 [:bar] ETA: :eta";
  } else if(channel == 2) {
    pbtext =  "Rendering Bokeh 2/3 [:bar] ETA: :eta";
  } else if(channel == 3) {
    pbtext =  "Rendering Bokeh 3/3 [:bar] ETA: :eta";
  }
  RProgress::RProgress pb(pbtext);
  if(progbar) {
    pb.set_total(rows*cols);
  }
  
  int begini, beginj, endi, endj, temphalfi, temphalfj;
  int beginslicei, endslicei, beginslicej, endslicej;
  for (int i = halfwidth; i < rows-halfwidth; ++i) {
    
    Rcpp::checkUserInterrupt();
    for (int j = halfwidth; j < cols-halfwidth; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      begini = i - (temp.n_rows-1)/2;
      beginj = j - (temp.n_cols-1)/2;
      endi = i + (temp.n_rows-1)/2;
      endj = j + (temp.n_cols-1)/2;
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp;
      normalize.submat(begini, beginj, endi, endj) += temp;
    }
  }
  
  for (int i = 0; i < halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < cols; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      temphalfi =  (temp.n_rows-1)/2;
      temphalfj =  (temp.n_cols-1)/2;
      beginslicei = 0;
      endslicei = temp.n_rows-1;
      beginslicej = 0;
      endslicej = temp.n_cols-1;
      endi = i + temphalfi;
      endj = j + temphalfj;
      begini =  i - temphalfi;
      beginj =  j - temphalfj;
      if(i  - temphalfi < 0) {
        beginslicei = temphalfi - i;
        begini = 0;
      }
      if(i  + temphalfi > rows - 1) {
        endslicei = endslicei - (i  + temphalfi - rows + 1);
        endi = rows - 1;
      }
      if(j  - temphalfj < 0) {
        beginslicej = temphalfj - j;
        beginj = 0;
      }
      if(j  + temphalfj > cols - 1) {
        endslicej = endslicej - (j  + temphalfj - cols + 1);
        endj = cols - 1;
      }
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      temp2 = temp.submat(beginslicei,beginslicej,endslicei,endslicej);
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
      normalize.submat(begini, beginj, endi, endj) += temp2;
    }
  }
  for (int i = halfwidth; i < rows-halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < halfwidth; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      temphalfi =  (temp.n_rows-1)/2;
      temphalfj =  (temp.n_cols-1)/2;
      beginslicei = 0;
      endslicei = temp.n_rows-1;
      beginslicej = 0;
      endslicej = temp.n_cols-1;
      endi = i + temphalfi;
      endj = j + temphalfj;
      begini =  i - temphalfi;
      beginj =  j - temphalfj;
      if(i  - temphalfi < 0) {
        beginslicei = temphalfi - i;
        begini = 0;
      }
      if(i  + temphalfi > rows - 1) {
        endslicei = endslicei - (i  + temphalfi - rows + 1);
        endi = rows - 1;
      }
      if(j  - temphalfj < 0) {
        beginslicej = temphalfj - j;
        beginj = 0;
      }
      if(j  + temphalfj > cols - 1) {
        endslicej = endslicej - (j  + temphalfj - cols + 1);
        endj = cols - 1;
      }
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      temp2 = temp.submat(beginslicei,beginslicej,endslicei,endslicej);
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
      normalize.submat(begini, beginj, endi, endj) += temp2;
    }
  }
  for (int i = halfwidth; i < rows-halfwidth; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = cols-halfwidth; j < cols; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      temphalfi =  (temp.n_rows-1)/2;
      temphalfj =  (temp.n_cols-1)/2;
      beginslicei = 0;
      endslicei = temp.n_rows-1;
      beginslicej = 0;
      endslicej = temp.n_cols-1;
      endi = i + temphalfi;
      endj = j + temphalfj;
      begini =  i - temphalfi;
      beginj =  j - temphalfj;
      if(i  - temphalfi < 0) {
        beginslicei = temphalfi - i;
        begini = 0;
      }
      if(i  + temphalfi > rows - 1) {
        endslicei = endslicei - (i  + temphalfi - rows + 1);
        endi = rows - 1;
      }
      if(j  - temphalfj < 0) {
        beginslicej = temphalfj - j;
        beginj = 0;
      }
      if(j  + temphalfj > cols - 1) {
        endslicej = endslicej - (j  + temphalfj - cols + 1);
        endj = cols - 1;
      }
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      temp2 = temp.submat(beginslicei,beginslicej,endslicei,endslicej);
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
      normalize.submat(begini, beginj, endi, endj) += temp2;
    }
  }
  for (int i = rows-halfwidth; i < rows; ++i) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < cols; ++j) {
      if(progbar) {
        pb.tick();
      }
      temp = kernels[blurmatrix(i,j)]/pow(depthmap(i,j),2);
      if(temp.n_cols == 1) {
        normalize(i,j) += 1/pow(depthmap(i,j),2);
        result(i,j) += image(i,j)/pow(depthmap(i,j),2);
        continue;
      }
      temphalfi =  (temp.n_rows-1)/2;
      temphalfj =  (temp.n_cols-1)/2;
      beginslicei = 0;
      endslicei = temp.n_rows-1;
      beginslicej = 0;
      endslicej = temp.n_cols-1;
      endi = i + temphalfi;
      endj = j + temphalfj;
      begini =  i - temphalfi;
      beginj =  j - temphalfj;
      if(i  - temphalfi < 0) {
        beginslicei = temphalfi - i;
        begini = 0;
      }
      if(i  + temphalfi > rows - 1) {
        endslicei = endslicei - (i  + temphalfi - rows + 1);
        endi = rows - 1;
      }
      if(j  - temphalfj < 0) {
        beginslicej = temphalfj - j;
        beginj = 0;
      }
      if(j  + temphalfj > cols - 1) {
        endslicej = endslicej - (j  + temphalfj - cols + 1);
        endj = cols - 1;
      }
      if(image(i,j) > bokehlimit) {
        temp = temp * bokehintensity;
      }
      temp2 = temp.submat(beginslicei,beginslicej,endslicei,endslicej);
      result.submat(begini, beginj, endi, endj) += image(i,j) * temp2;
      normalize.submat(begini, beginj, endi, endj) += temp2;
    }
  }
  return(result/normalize);
}
