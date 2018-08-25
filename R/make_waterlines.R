#'@title make_waterlines
#'
#'@description Makes the edge lines of 
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param waterdepth Default `0`.
#'@param linecolor Default `grey40`. 
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param alpha Default `1`. Transparency of lines.
#'@param lwd Default `2`. Water line width.
#'@keywords internal
make_waterlines = function(heightmap,waterdepth=0,linecolor="grey40",zscale=1,alpha=1,lwd=2) {
  heightmap = heightmap[,ncol(heightmap):1]/zscale
  heightval1 = heightmap[1,]
  heightval2 = heightmap[,1]
  heightval3 = heightmap[nrow(heightmap),]
  heightval4 = heightmap[,ncol(heightmap)]
  heightlist = list()
  counter = 1
  drawing = FALSE
  startcoord = 1
  for(i in 1:length(heightval1)) {
    if(heightval1[i] < waterdepth && !drawing) { 
      startcoord = i 
      drawing = TRUE
    } 
    if((heightval1[i] > waterdepth || i == length(heightval1)) && drawing) {
      drawing = FALSE
      heightlist[[counter]] = matrix(c(1,1,waterdepth,waterdepth,-startcoord,-i),2,3)
      counter = counter + 1
    }
  }
  drawing = FALSE
  startcoord = 1
  for(i in 1:length(heightval2)) {
    if(heightval2[i] < waterdepth && !drawing) { 
      startcoord = i 
      drawing = TRUE
    } 
    if((heightval2[i] > waterdepth || i == length(heightval2)) && drawing) {
      drawing = FALSE
      heightlist[[counter]] = matrix(c(startcoord,i,waterdepth,waterdepth,-1,-1),2,3)
      counter = counter + 1
    }
  }
  drawing = FALSE
  startcoord = 1
  for(i in 1:length(heightval3)) {
    if(heightval3[i] < waterdepth && !drawing) { 
      startcoord = i 
      drawing = TRUE
    } 
    if((heightval3[i] > waterdepth || i == length(heightval3)) && drawing) {
      drawing = FALSE
      heightlist[[counter]] = matrix(c(nrow(heightmap),nrow(heightmap),waterdepth,waterdepth,-startcoord,-i),2,3)  
      counter = counter + 1
    }
  }
  drawing = FALSE
  startcoord = 1
  for(i in 1:length(heightval4)) {
    if(heightval4[i] < waterdepth && !drawing) { 
      startcoord = i 
      drawing = TRUE
    } 
    if((heightval4[i] > waterdepth || i == length(heightval4)) && drawing) {
      drawing = FALSE
      heightlist[[counter]] = matrix(c(startcoord,i,waterdepth,waterdepth,-ncol(heightmap),-ncol(heightmap)),2,3)
      counter = counter + 1
    }
  }
  for(i in 1:length(heightlist)) {
    rgl::lines3d(heightlist[[i]],color=linecolor,lwd=lwd,alpha=alpha,depth_mask=FALSE)
  }
}