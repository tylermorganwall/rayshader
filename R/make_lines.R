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
  heightval3 = heightmap[1,1]
  heightval4 = heightmap[nrow(heightmap),1]
  heightval1 = heightmap[1,ncol(heightmap)]
  heightval2 = heightmap[nrow(heightmap),ncol(heightmap)]
  heightlist = list()
  if(all(!is.na(heightmap))) {
    if(solid) {
      heightlist[[1]] = matrix(c(1,1,basedepth,heightval3,-1,-1),2,3)
      heightlist[[2]] = matrix(c(nrow(heightmap),nrow(heightmap),basedepth,heightval4,-1,-1),2,3)
      heightlist[[3]] = matrix(c(1,1,basedepth,heightval1,-ncol(heightmap),-ncol(heightmap)),2,3)
      heightlist[[4]] = matrix(c(nrow(heightmap),nrow(heightmap),basedepth,heightval2,-ncol(heightmap),-ncol(heightmap)),2,3)
      heightlist[[5]] = matrix(c(1,1,basedepth,basedepth,-1,-ncol(heightmap)),2,3)
      heightlist[[6]] = matrix(c(1,nrow(heightmap),basedepth,basedepth,-ncol(heightmap),-ncol(heightmap)),2,3)
      heightlist[[7]] = matrix(c(nrow(heightmap),nrow(heightmap),basedepth,basedepth,-ncol(heightmap),-1),2,3)
      heightlist[[8]] = matrix(c(nrow(heightmap),1,basedepth,basedepth,-1,-1),2,3)
    } else {
      basedepth = basedepth
      counter = 1
      if(basedepth > heightval1) {
        heightlist[[counter]] = matrix(c(1,1,basedepth,heightval1,-1,-1),2,3)
        counter = counter + 1
      }
      if(basedepth > heightval2) {
        heightlist[[counter]] = matrix(c(nrow(heightmap),nrow(heightmap),basedepth,heightval2,-1,-1),2,3)
        counter = counter + 1
      }
      if(basedepth > heightval3) {
        heightlist[[counter]] = matrix(c(1,1,basedepth,heightval3,-ncol(heightmap),-ncol(heightmap)),2,3)
        counter = counter + 1
      }
      if(basedepth > heightval4) {
        heightlist[[counter]] = matrix(c(nrow(heightmap),nrow(heightmap),basedepth,heightval4,-ncol(heightmap),-ncol(heightmap)),2,3)
        counter = counter + 1
      }
    }
  } else {
    heightlist = make_baselines_cpp(heightmap,is.na(heightmap),basedepth)
  }
  if(length(heightlist) > 0) {
    segmentlist = do.call(rbind,heightlist)
    segmentlist[,1] = segmentlist[,1] - nrow(heightmap)/2
    segmentlist[,3] = -segmentlist[,3] - ncol(heightmap)/2 
    rgl::segments3d(segmentlist,color=linecolor,lwd=linewidth,alpha=alpha,depth_mask=TRUE, 
                    line_antialias=FALSE, depth_test="lequal",ambient = ifelse(solid,"#000004","#000005"))
  }
}