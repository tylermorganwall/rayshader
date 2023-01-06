#' Reorder Lines
#' 
#' @return data
#' @keywords internal
ray_merge_reorder = function(sf_data, start_index = 1, merge_tolerance = 0.1 ,
                             duplicate_tolerance = 0.1) {
  if(!inherits(sf_data, "sf")) {
    stop("Reordering and merging lines can only be performed on {sf} objects")
  }
  sf_data = sf_data[!sf::st_is_empty(sf_data),]
  start_points = list()
  end_points = list()
  tolerance = merge_tolerance
  
  for(i in seq_len(nrow(sf_data))) {
    temp_data = sf::st_coordinates(sf_data[i,])
    start_points[[i]] = temp_data[1,1:2]
    end_points[[i]] = temp_data[nrow(temp_data),1:2]
  }
  
  #Remove duplicate points by comparing endpoints (check for reverse orientation)
  remove_points = c()
  original_points = c()
  for(i in seq_len(nrow(sf_data)-1)) {
    for(j in seq(i+1, nrow(sf_data))) {
      if((sum((start_points[[i]] - start_points[[j]])^2) < duplicate_tolerance^2 &&
         sum((end_points[[i]] - end_points[[j]])^2) < duplicate_tolerance^2) ||
         (sum((start_points[[i]] - end_points[[j]])^2) < duplicate_tolerance^2 &&
         sum((end_points[[i]] - start_points[[j]])^2) < duplicate_tolerance^2)) {
        if(!(j %in% remove_points)) {
          remove_points = c(remove_points, j)
          original_points = c(original_points,i)
        }
      }
    }
  }
  
  if(length(remove_points) > 0) {
    sf_data = sf_data[-remove_points,]
    start_points[remove_points] = NULL
    end_points[remove_points] = NULL
  }
  
  forwards_list = list()
  backwards_list = list()
  
  forwards_list[[1]] = sf_data[start_index,]
  
  queue_vals = rep(start_index, nrow(sf_data))
  
  temp_index = start_index
  counter_forward = 1
  counter_backward = 1
  counter_all = 1
  prev_counter = 1
  
  forwards = TRUE
  while(counter_all < nrow(sf_data)) {
    reverse_line = rep(FALSE, nrow(sf_data))
    first_backwards = FALSE
    prev_counter = counter_all
    min_dist = Inf
    best_candidate = NA
    start_indices = seq_len(length(start_points))
    start_indices = start_indices[-queue_vals]
    if(forwards) {
      for(i in start_indices) {
        temp_dist = sum((end_points[[temp_index]] - start_points[[i]])^2)
        temp_dist2 = sum((end_points[[temp_index]] - end_points[[i]])^2)
        if((temp_dist < min_dist || temp_dist2 < min_dist) && 
           !(i %in% queue_vals) && 
           (temp_dist < tolerance^2 || temp_dist2 < tolerance^2)) {
          if(temp_dist2 < temp_dist) {
            reverse_line[i] = TRUE
            min_dist = temp_dist2
          } else {
            min_dist = temp_dist
          }
          best_candidate = i
        } 
      }
    } else {
      for(i in start_indices) {
        temp_dist = sum((start_points[[temp_index]] - end_points[[i]])^2)
        temp_dist2 = sum((start_points[[temp_index]] - start_points[[i]])^2)
        if((temp_dist < min_dist || temp_dist2 < min_dist) && 
           !(i %in% queue_vals) && 
           (temp_dist < tolerance^2 || temp_dist2 < tolerance^2)) {
          if(temp_dist2 < temp_dist) {
            reverse_line[i] = TRUE
            min_dist = temp_dist2
          } else {
            min_dist = temp_dist
          }
          best_candidate = i
        } 
      }
    }
    #Swap start and end?
    if(!is.na(best_candidate)) {
      temp_index = best_candidate
      if(reverse_line[best_candidate]) {
        sf_data[best_candidate,] = sf::st_reverse(sf_data[best_candidate,])
        temp_point = start_points[[best_candidate]]
        start_points[[best_candidate]] = end_points[[best_candidate]]
        end_points[[best_candidate]] = temp_point
      }
      if(forwards) {
        counter_forward = counter_forward + 1
        forwards_list[[counter_forward]] = sf_data[best_candidate,]
      } else {
        backwards_list[[counter_backward]] = sf_data[best_candidate,]
        counter_backward = counter_backward + 1
      }
      queue_vals[counter_all] = best_candidate
      counter_all = counter_all + 1
      best_candidate = NA
    } else if (forwards) {
      forwards = FALSE
      first_backwards = TRUE
      temp_index = start_index
    }
    if(prev_counter == counter_all && counter_all < nrow(sf_data) && !first_backwards) {
      final_list = c(rev(backwards_list), forwards_list)
      return(do.call(rbind,final_list))
    }
  }
  final_list = c(forwards_list, (backwards_list))
  return(do.call(rbind,final_list))
}
