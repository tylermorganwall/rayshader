#'@title make_base
#'
#'@description Makes the base below the 3D elevation map.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param basedepth Default `0`.
#'@param basecolor Default `grey20`.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@keywords internal
make_base = function(heightmap,basedepth=0,basecolor="grey20",zscale=1, soil = FALSE,
                     soil_freq = 0.1, soil_levels = 8, soil_color1 = "black", soil_color2 = "black",
                     soil_gradient = 0, gradient_darken = 1) {
  heightmap = heightmap/zscale
  edge_vals = unique(c(heightmap[1,], heightmap[,1],heightmap[nrow(heightmap),],heightmap[,ncol(heightmap)]))
  if(length(edge_vals) == 1 && all(!is.na(edge_vals))) {
    heightlist = list()
    nc = ncol(heightmap)
    nr = nrow(heightmap)
    heightlist[[1]] = matrix(c(1, nr, nr, basedepth,basedepth,basedepth,-1, -nc,-1),3,3)
    heightlist[[2]] = matrix(c(1, nr,1,basedepth,basedepth,basedepth, -nc, -nc,-1),3,3)
    heightlist[[3]] = matrix(c(1, nr,1, basedepth,basedepth,edge_vals,-1,-1,-1),3,3)
    heightlist[[4]] = matrix(c( nr, nr,1,basedepth,edge_vals,edge_vals,-1,-1,-1),3,3)
    heightlist[[5]] = matrix(c(1, nr, 1, edge_vals,basedepth,basedepth, -nc, -nc, -nc),3,3)
    heightlist[[6]] = matrix(c( 1, nr, nr,edge_vals,edge_vals,basedepth, -nc, -nc, -nc),3,3)
    heightlist[[7]] = matrix(c(nr, nr, nr, basedepth,basedepth,edge_vals,-1,-nc,-1),3,3)
    heightlist[[8]] = matrix(c(nr, nr, nr,basedepth,edge_vals,edge_vals,-nc,-nc,-1),3,3)
    heightlist[[9]] = matrix( c(1,1,1, edge_vals,basedepth,basedepth, -1, -nc, -1),3,3)
    heightlist[[10]] = matrix(c(1,1,1, edge_vals,edge_vals,basedepth, -1, -nc, -nc),3,3)
    fullsides = do.call(rbind,heightlist)
    fullsides[,1] = fullsides[,1] - nrow(heightmap)/2
    fullsides[,3] = -fullsides[,3] - ncol(heightmap)/2
    fullsides = fullsides[nrow(fullsides):1,]
    rgl::triangles3d(fullsides, 
                     texture = NULL,
                     lit=FALSE,color=basecolor,front="filled",back="culled",tag = "base")
  } else if(all(!is.na(heightmap))) {
    na_matrix = is.na(heightmap)
    baselist = make_base_cpp(heightmap, na_matrix, basedepth)
    heightlist = baselist$vertices
    edge_heights = as.vector(t(cbind(baselist$edge_heights,baselist$edge_heights,baselist$edge_heights)))
    direction_vec = as.vector(t(cbind(baselist$is_horizontal,baselist$is_horizontal,baselist$is_horizontal)))
    
    heightlist[[length(heightlist)+1]] = matrix(c(1,nrow(heightmap),nrow(heightmap), basedepth,basedepth,basedepth,-1,-ncol(heightmap),-1),3,3)
    heightlist[[length(heightlist)+1]] = matrix(c(1,nrow(heightmap),1,basedepth,basedepth,basedepth,-ncol(heightmap),-ncol(heightmap),-1),3,3)
    direction_vec = c(direction_vec, rep(FALSE,6))
    direction_vec = rev(direction_vec)

    edge_heights = c(edge_heights,rep(0,6))
    edge_heights = rev(edge_heights)

    fullsides = do.call(rbind,heightlist)
    fullsides[,1] = fullsides[,1] - nrow(heightmap)/2
    fullsides[,3] = -fullsides[,3] - ncol(heightmap)/2
    fullsides = fullsides[nrow(fullsides):1,]

    if(soil) {
      horizontal_sides = fullsides[!direction_vec,]
      horizontal_heights = edge_heights[!direction_vec]

      vertical_sides = fullsides[direction_vec,]
      vertical_heights = edge_heights[direction_vec]
      
      textures = generate_soil_textures(heightmap, base_depth = max(fullsides[,2],na.rm=TRUE) - basedepth, zscale=zscale,
                                        freq=soil_freq, levels = soil_levels,  color1= soil_color1, color2= soil_color2,
                                        soil_gradient = soil_gradient, gradient_darken = gradient_darken)
      
      min_hside_y = min(horizontal_sides[,2],na.rm=TRUE)
      max_hside_y = max(horizontal_sides[,2],na.rm=TRUE)
      
      horizontal_texcoords_x = (horizontal_sides[,1] + nrow(heightmap)/2-1)/(nrow(heightmap)-1)
      # horizontal_texcoords_y = (horizontal_sides[,2]-min_hside_y)/(max_hside_y-min_hside_y)
      
      horizontal_texcoords_y = rep(0,length(horizontal_sides[,2]))
      # horizontal_texcoords_y[horizontal_sides[,2] == basedepth] = horizontal_heights[horizontal_sides[,2] == basedepth]/(max_hside_y-min_hside_y)
      horizontal_texcoords_y[horizontal_sides[,2] == basedepth] = 1

      #Set bottom texcoords
      horizontal_texcoords_x[1:6] = 0.99
      horizontal_texcoords_y[1:6] = 0.99
      
      min_vside_y = min(vertical_sides[,2],na.rm=TRUE)
      max_vside_y = max(vertical_sides[,2],na.rm=TRUE)
      
      vertical_texcoords_x = (vertical_sides[,3] + ncol(heightmap)/2-1)/(ncol(heightmap)-1)
      # vertical_texcoords_y = (vertical_sides[,2]-min_vside_y)/(max_vside_y-min_vside_y)
      vertical_texcoords_y = rep(0,length(vertical_sides[,2]))
      # vertical_texcoords_y[vertical_sides[,2] == basedepth] = vertical_heights[vertical_sides[,2] == basedepth]/(max_vside_y-min_vside_y)
      vertical_texcoords_y[vertical_sides[,2] == basedepth] = 1
      
      rgl::triangles3d(horizontal_sides, color="white",
                       texture = textures[1], texcoords = cbind(horizontal_texcoords_x,horizontal_texcoords_y),
                       lit=FALSE,front="filled",back="cull",tag = "base_soil1")
      
      rgl::triangles3d(vertical_sides,  color="white",
                       texture = textures[2],texcoords = cbind(vertical_texcoords_x,vertical_texcoords_y),
                       lit=FALSE,front="filled",back="cull",tag = "base_soil2")
      
    } else {
      rgl::triangles3d(fullsides, 
                       lit=FALSE,color=basecolor,front="filled",back="cull",tag = "base")
    }
  } else {
    na_matrix = is.na(heightmap)
    baselist = make_base_cpp(heightmap, na_matrix, basedepth)
    heightlist = baselist$vertices
    edge_heights = rev(as.vector(t(cbind(baselist$edge_heights,baselist$edge_heights,baselist$edge_heights))))
    
    direction_vec = rev(as.vector(t(cbind(baselist$is_horizontal,baselist$is_horizontal,baselist$is_horizontal))))
    
    fullsides = do.call(rbind,heightlist)
    fullsides[,1] = fullsides[,1] - nrow(heightmap)/2
    fullsides[,3] = -fullsides[,3] - ncol(heightmap)/2
    fullsides = fullsides[nrow(fullsides):1,]
    basemat = matrix(basedepth,nrow(heightmap),ncol(heightmap))
    basemat[is.na(heightmap)] = NA
    normalmat = matrix(0,nrow(heightmap),ncol(heightmap))
    xznormals = fliplr(heightmap)
    ynormals = fliplr(heightmap)
    xznormals[!is.na(xznormals)] = 0
    ynormals[!is.na(ynormals)] = -1
    
    if(soil) {
      horizontal_sides = fullsides[!direction_vec,]
      horizontal_heights = edge_heights[!direction_vec]

      vertical_sides = fullsides[direction_vec,]
      vertical_heights = edge_heights[direction_vec]

      textures = generate_soil_textures(heightmap, base_depth = max(fullsides[,2],na.rm=TRUE) - basedepth, zscale=zscale,
                                        freq=soil_freq, levels = soil_levels,  color1= soil_color1, color2= soil_color2,
                                        soil_gradient = soil_gradient, gradient_darken = gradient_darken)
      
      min_hside_y = min(horizontal_sides[,2],na.rm=TRUE)
      max_hside_y = max(horizontal_sides[,2],na.rm=TRUE)
      
      horizontal_texcoords_x = (horizontal_sides[,1] + nrow(heightmap)/2-1)/(nrow(heightmap)-1)
      # horizontal_texcoords_y = (horizontal_sides[,2]-min_hside_y)/(max_hside_y-min_hside_y)
      
      horizontal_texcoords_y = rep(0,length(horizontal_sides[,2]))
      horizontal_texcoords_y[horizontal_sides[,2] == basedepth] = horizontal_heights[horizontal_sides[,2] == basedepth]/(max_hside_y-min_hside_y)
      
      #Set bottom texcoords
      horizontal_texcoords_x[1:6] = 0.99
      horizontal_texcoords_y[1:6] = 0.99
      
      min_vside_y = min(vertical_sides[,2],na.rm=TRUE)
      max_vside_y = max(vertical_sides[,2],na.rm=TRUE)
      
      vertical_texcoords_x = (vertical_sides[,3] + ncol(heightmap)/2-1)/(ncol(heightmap)-1)
      # vertical_texcoords_y = (vertical_sides[,2]-min_vside_y)/(max_vside_y-min_vside_y)
      vertical_texcoords_y = rep(0,length(vertical_sides[,2]))
      vertical_texcoords_y[vertical_sides[,2] == basedepth] = vertical_heights[vertical_sides[,2] == basedepth]/(max_vside_y-min_vside_y)
      
      rgl::triangles3d(horizontal_sides, 
                       texture = textures[1], texcoords = cbind(horizontal_texcoords_x,horizontal_texcoords_y),
                       lit=FALSE,front="filled",back="cull",tag = "base_soil1",
                       color = "white")
      
      rgl::triangles3d(vertical_sides, 
                       texture = textures[2],texcoords = cbind(vertical_texcoords_x,vertical_texcoords_y),
                       lit=FALSE,front="filled",back="cull",tag = "base_soil2",
                       color = "white")
    } else {
      rgl::triangles3d(fullsides,
                       texture = NULL,
                       lit=FALSE,color=basecolor,front="filled",back="filled",tag = "base")
    }
    rgl::surface3d(x=1:nrow(basemat)-nrow(basemat)/2,
                   z=1:ncol(basemat)-ncol(basemat)/2,
                   y=basemat,
                   color=basecolor, 
                   lit=FALSE,back="filled",front="filled",tag = "basebottom",
                   normal_x = xznormals, normal_z = xznormals, normal_y = ynormals)
    
  }
}
