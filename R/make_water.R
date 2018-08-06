#'@title make_water
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param waterheight Default `0`.
#'@param watercolor Default `blue`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param wateralpha Default `0.5`. Water transparency.
make_water = function(heightmap,waterheight=mean(heightmap),watercolor="lightblue",zscale=1,wateralpha=0.5) {
  heightmap = heightmap[,ncol(heightmap):1]/zscale
  heightmap1 = heightmap[1,]
  heightmap2 = heightmap[,1]
  heightmap3 = heightmap[nrow(heightmap),]
  heightmap4 = heightmap[,ncol(heightmap)]
  heightlist1 = list()
  heightlist2 = list()
  heightlist3 = list()
  heightlist4 = list()
  heightlist5 = list()
  for(i in 1:(length(heightmap1)-1)) {
    if(heightmap1[i] < waterheight) {
      heightlist1[[i]] = matrix(c(1,1,1, heightmap1[i],waterheight,waterheight, i,i,i+1),3,3)[c(1,3,2),]
      heightlist1[[i+length(heightmap1)]] = matrix(c(1,1,1, heightmap1[i],waterheight,heightmap1[i+1], i,i+1,i+1),3,3)[c(1,3,2),]
    }
  }
  heightmat1 = do.call(rbind,heightlist1)
  for(i in 1:(length(heightmap2)-1)) {
    if(heightmap2[i] < waterheight) {
      heightlist2[[i]] = matrix(c(i,i+1,i,  heightmap2[i],waterheight,waterheight, 1,1,1),3,3)[c(1,3,2),]
      heightlist2[[i+length(heightmap2)]] = matrix(c(i,i+1,i+1,  heightmap2[i],heightmap2[i+1],waterheight, 1,1,1),3,3)[c(1,3,2),]
    }
  }
  heightmat2 = do.call(rbind,heightlist2)
  for(i in 1:(length(heightmap3)-1)) {
    if(heightmap3[i] < waterheight) {
      heightlist3[[i]] = matrix(c(nrow(heightmap),nrow(heightmap),nrow(heightmap),  heightmap3[i],waterheight,waterheight, i,i+1,i),3,3)[c(1,3,2),]
      heightlist3[[i+length(heightmap3)]] = matrix(c(nrow(heightmap),nrow(heightmap),nrow(heightmap),  heightmap3[i],heightmap3[i+1],waterheight, i,i+1,i+1),3,3)[c(1,3,2),]
    }
  }
  heightmat3 = do.call(rbind,heightlist3)
  for(i in 1:(length(heightmap4)-1)) {
    if(heightmap4[i] < waterheight) {
      heightlist4[[i]] = matrix(c(i,i,i+1, heightmap4[i],waterheight,waterheight, ncol(heightmap),ncol(heightmap),ncol(heightmap)),3,3)[c(1,3,2),]
      heightlist4[[i+length(heightmap4)]] = matrix(c(i,i+1,i+1, heightmap4[i],waterheight,heightmap4[i+1], ncol(heightmap),ncol(heightmap),ncol(heightmap)),3,3)[c(1,3,2),]
    }
  }
  heightmat4 = do.call(rbind,heightlist4)
  fullsides = rbind(heightmat1,heightmat2,heightmat3,heightmat4)
  rgl::triangles3d(fullsides,lit=FALSE,color=watercolor,alpha=wateralpha,front="fill",back="culled")
}
