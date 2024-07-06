#' Get Distance Along Bezier Curve
#'
#' @param points 3D points to interpolate
#' @param n Number of interpolation breaks
#' @return Data frame of points along path, along with distances
#' 
#' @keywords internal
get_interpolated_points_path = function(points, n = 360, use_altitude = FALSE) {
  points_lead = points[-1,,drop=FALSE]
  points_lag = points[-nrow(points),,drop=FALSE]
  if(use_altitude) {
    path_distance = c(0,cumsum(sqrt((points_lead[,1] - points_lag[,1])^2 +
                                    (points_lead[,2] - points_lag[,2])^2 +
                                    (points_lead[,3] - points_lag[,3])^2)))
  } else {
    path_distance = c(0,cumsum(sqrt((points_lead[,1] - points_lag[,1])^2 +
                                    (points_lead[,3] - points_lag[,3])^2)))
  }
  
  interval_dists = seq(0, max(path_distance), length.out = n)
  new_points = vector("list", length = n)
  for(i in seq_len(n)) {
    abs_dists = abs(path_distance - interval_dists[i])
    min_index = which.min(abs_dists)[1]
    sing_dist =  interval_dists[i] - path_distance[min_index]
    
    #Return if exact match in distances
    if(sing_dist == 0) {
      new_points[[i]] = points[min_index,]
      next
    } else if(sing_dist > 0) {
      max_index = min_index + 1
    } else {
      max_index = min_index
      min_index = min_index - 1
    }
    min_dist = path_distance[min_index]
    max_dist = path_distance[max_index]
    while(max_dist - min_dist == 0) {
      if(max_index + 1 < length(path_distance)) {
        max_index = max_index + 1
        max_dist = path_distance[max_index]
      } else {
        min_dist = min_dist - 1
        min_dist = path_distance[min_dist]
      }
    }
    t_val = (interval_dists[i] - min_dist) / (max_dist - min_dist)
    new_points[[i]] = points[min_index,] * (1 - t_val) + points[max_index,] * t_val
  }
  return(do.call(rbind,new_points))
}
