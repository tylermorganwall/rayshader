#'@title make_lines
#'
#'@description Makes the lines in the corner of the base.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param basedepth Default `0`.
#'@param linecolor Default `grey40`. 
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param alpha Default `1`. Transparency.
#'@param linewidth Default `2`. Linewidth
#'@param solid Default `TRUE`. Whether it's solid or water.
#'@keywords internal
make_lines = function(heightmap,basedepth=0,linecolor="grey20",zscale=1,alpha=1,linewidth = 2,solid=TRUE) {
  heightmap = heightmap/zscale
  nr = nrow(heightmap)
  nc = ncol(heightmap)
  heightval3 = heightmap[1, 1 ]
  heightval4 = heightmap[nr,1 ]
  heightval1 = heightmap[1, nc]
  heightval2 = heightmap[nr,nc]
  heightlist = list()
  if(all(!is.na(heightmap))) {
    if(solid) {
      heightlist[[1]] = matrix(c(1,1,basedepth,heightval3,1,1),2,3)
      heightlist[[2]] = matrix(c(nr,nr,basedepth,heightval4,1,1),2,3)
      heightlist[[3]] = matrix(c(1,1,basedepth,heightval1,nc,nc),2,3)
      heightlist[[4]] = matrix(c(nr,nr,basedepth,heightval2,nc,nc),2,3)
      heightlist[[5]] = matrix(c(1,1,basedepth,basedepth,1,nc),2,3)
      heightlist[[6]] = matrix(c(1,nr,basedepth,basedepth,nc,nc),2,3)
      heightlist[[7]] = matrix(c(nr,nr,basedepth,basedepth,nc,1),2,3)
      heightlist[[8]] = matrix(c(nr,1,basedepth,basedepth,1,1),2,3)
    } else {
      basedepth = basedepth/zscale
      counter = 1
      if(basedepth > heightval1) {
        heightlist[[counter]] = matrix(c(1,1,basedepth,heightval1,1,1),2,3)
        counter = counter + 1
      }
      if(basedepth > heightval2) {
        heightlist[[counter]] = matrix(c(nr,nr,basedepth,heightval2,1,1),2,3)
        counter = counter + 1
      }
      if(basedepth > heightval3) {
        heightlist[[counter]] = matrix(c(1,1,basedepth,heightval3,nc,nc),2,3)
        counter = counter + 1
      }
      if(basedepth > heightval4) {
        heightlist[[counter]] = matrix(c(nr,nr,basedepth,heightval4,nc,nc),2,3)
        counter = counter + 1
      }
    }
    reverse_z = FALSE
  } else {
    heightlist = make_baselines_cpp(heightmap,is.na(heightmap),basedepth)
    reverse_z = TRUE
  }
  if(length(heightlist) > 0) {
    segmentlist = do.call(rbind,heightlist)
    if(reverse_z) {
      segmentlist[,3] = -segmentlist[,3]
    }
    segmentlist[,1] = segmentlist[,1] - 1
    segmentlist[,3] = segmentlist[,3] - 1
    segmentlist[,1] = segmentlist[,1] - (nr-1)/2
    segmentlist[,3] = segmentlist[,3] - (nc-1)/2 
    rgl::segments3d(segmentlist,color=linecolor,lwd=linewidth,alpha=alpha,depth_mask=TRUE, 
                    line_antialias=FALSE, depth_test="lequal",tag = ifelse(solid,"lines","waterlines"))
  }
}