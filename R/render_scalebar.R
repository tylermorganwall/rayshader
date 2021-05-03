#'@title Render Scale Bar
#'
#'@description Places a compass on the map to specify the North direction.
#'
#
#'@param limits The distance represented by the scale bar. If a numeric vector greater than length 1, 
#'this will specify the breaks along the scale bar to place labels, with the maximum value in 
#'limits assumed to be the last label. Must be non-negative.
#'@param position Default `W`. A string representing a direction. Can be `N`, `E`, `S`, and `W`.
#'@param y Default `NULL`. The height of the scale bar, automatically calculated if `NULL`.
#'@param scale_length Default `1`. Length of the scale bar, relative to the 
#'side of the map specified in `position`. If a length-2 vector, the first number specifies the start
#'and stop points along the side.
#'@param label_unit Default `NULL`. The distance unit for the label.
#'@param offset Default `NULL`. The distance away from the edge to place the scale bar.
#'If `NULL`, automatically calculated.
#'@param segments Default `10`. Number of colored segments in the scalebar.
#'@param radius Default `NULL`. The radius of the cylinder representing the scale bar.
#'If `NULL`, automatically calculated. 
#'@param color_first Default `darkred`. Primary color in the scale bar.
#'@param color_second Default `grey90`. Seconary color in the scale bar.
#'@param color_text Default `black`. Color of the text.
#'@param text_switch_side Default `FALSE`. Switches the order of the text.
#'@param text_x_offset Default `0`. Distance offset for text in the x direction.
#'@param text_y_offset Default `0`. Distance offset for text in the y direction.
#'@param text_z_offset Default `0`. Distance offset for text in the z direction.
#'@param clear_scalebar Default `FALSE`. Clears the scale bar(s) on the map.
#'
#'@return Displays snapshot of current rgl plot (or saves to disk).
#'@export
#'@examples
#'if(interactive()) {
#'#Add a scale bar to the montereybay dataset, here representing about 80km
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,theta=45, water=TRUE)
#'render_scalebar(limits=c(0, 80), label_unit = "km")
#'render_snapshot()
#'
#'#This function works with `render_highquality()`
#'
#'render_highquality(lightdirection=250, lightaltitude=40, scale_text_size=24,clamp_value=10)
#'render_scalebar(clear_scalebar = TRUE)
#'
#'#We can change the position by specifying a cardinal direction to `position`, and the 
#'#color by setting `color_first` and `color_second`
#'
#'render_scalebar(limits=c(0,80), label_unit = "km", position = "N",
#'                color_first = "darkgreen", color_second = "lightgreen")
#'render_snapshot()
#'render_scalebar(clear_scalebar = TRUE)
#'
#'#And switch the orientation by setting `text_switch_side = TRUE`
#'render_scalebar(limits=c(0,80), label_unit = "km", position = "N", text_switch_side = TRUE,
#'                color_first = "darkgreen", color_second = "lightgreen")
#'render_snapshot()
#'render_scalebar(clear_scalebar = TRUE)
#'
#'#We can add additional breaks by specifying additional distances in `limits`
#'
#'render_scalebar(limits=c(0,40,80), label_unit = "km")
#'render_snapshot()
#'render_scalebar(clear_scalebar = TRUE)
#'
#'#We can also manually specify the height by setting the `y` argument:
#'
#'render_scalebar(limits=c(0,40,80), y=-70, label_unit = "km")
#'render_snapshot()
#'render_scalebar(clear_scalebar = TRUE)
#'
#'#Here we change the total size by specifying a start and end point along the side,
#'#and set the number of colored `segments`:
#'
#'render_scalebar(limits=c(0,20, 40), segments = 4, scale_length = c(0.5,1), label_unit = "km")
#'render_scalebar(limits=c(0,20, 40), segments = 4, position = "N", text_switch_side = TRUE,
#'                scale_length = c(0.25,0.75), label_unit = "km")
#'render_snapshot()
#'render_scalebar(clear_scalebar = TRUE)
#'
#'#Change the radius of the scale bar with `radius`. Here, the autopositioning doesn't work well with
#'#the labels, so we provide additional offsets with `text_y_offset` and `text_x_offset` to fix it.
#'
#'render_scalebar(limits=c(0,20, 40), segments = 4, scale_length = c(0.5,1), 
#'                label_unit = "km", radius=10,text_y_offset=-20,text_x_offset=20)
#'render_snapshot(clear=TRUE)
#'}
#'}
render_scalebar = function(limits, position = "W", y = NULL,
                          segments = 10, scale_length = 1, label_unit = "",
                          offset = NULL, radius = NULL,
                          color_first = "darkred", color_second = "grey80", color_text = "black",
                          text_switch_side = FALSE, 
                          text_x_offset = 0, text_y_offset = 0, text_z_offset = 0, 
                          clear_scalebar = FALSE) {
  if(clear_scalebar) {
    ids = get_ids_with_labels(c("scalebar_col1","scalebar_col2","text_scalebar"))$id
    rgl::rgl.pop(id=ids)
    return(invisible())
  }
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(length(scale_length) > 2) {
    stop("scale_length argument must be less than length 2")
  } else if (length(scale_length) == 1) {
    scale_length = c(0,scale_length)
  }
  if(any(scale_length < 0) || any(scale_length > 1)) {
    stop("scale_length argument must be between 0 and 1")
  }
  if(any(limits < 0)) {
    stop("limits must be greater than (or equal to) 0")
  }
  
  id_base = get_ids_with_labels("surface")$id
  if(length(id_base) == 0) {
    id_base = get_ids_with_labels("surface_tris")$id
  }
  fullverts = rgl::rgl.attrib(id_base,"vertices")
  xyz_range = apply(fullverts,2,range) 
  widths = xyz_range[2,c(1,3)] - xyz_range[1,c(1,3)]
  
  if(is.null(offset)) {
    if(position %in% c("N","S")) {
      offset = widths[1]/10
    } else if(position %in% c("E","W")) {
      offset = widths[2]/10
    }
  }
  if(is.null(radius)) {
    radius = offset/8
  }
  if(is.null(y)) {
    y = xyz_range[2,2]
  }
  if(position %in% c("N","S")) {
    xstart = xyz_range[2,1] * scale_length[2] + (1 - scale_length[2]) * xyz_range[1,1] 
    xend = xyz_range[1,1] * (1 - scale_length[1]) + xyz_range[2,1] * scale_length[1]
  } else {
    xstart = xyz_range[2,1] 
    xend = xyz_range[1,1]
  }
  if(position %in% c("E","W")) {
    zstart = xyz_range[2,3] * scale_length[2] + (1 - scale_length[2]) * xyz_range[1,3] 
    zend = xyz_range[1,3] * (1 - scale_length[1]) + xyz_range[2,3] * scale_length[1]
  } else {
    zstart = xyz_range[2,3]
    zend = xyz_range[1,3]
  }
  x_break_length = (xend - xstart)/segments
  z_break_length = (zend - zstart)/segments
  meshlist1 = list()
  meshlist2 = list()
  counter1 = 1
  counter2 = 1
  if(position == "N") {
    temp = xstart
    for(i in 1:segments) {
      if(i %% 2 == 1) {
        meshlist1[[counter1]] = rgl::cylinder3d(center = matrix(c(temp,temp+x_break_length, y, y, zend-offset, zend-offset),ncol=3,nrow=2),
                                              radius=radius,closed = -2)
        counter1 = counter1 + 1
        temp = temp + x_break_length
      } else {
        meshlist2[[counter2]] = rgl::cylinder3d(center = matrix(c(temp,temp+x_break_length, y, y, zend-offset, zend-offset),ncol=3,nrow=2),
                                                radius=radius,closed = -2)
        counter2 = counter2 + 1
        temp = temp + x_break_length
      }
    }
  } else if (position == "W") {
    temp = zstart
    for(i in 1:segments) {
      if(i %% 2 == 1) {
        meshlist1[[counter1]] = rgl::cylinder3d(center = matrix(c(xend-offset,xend-offset, y, y, temp, temp+z_break_length),ncol=3,nrow=2),
                                                radius=radius,closed = -2)
        counter1 = counter1 + 1
        temp = temp + z_break_length
      } else {
        meshlist2[[counter2]] = rgl::cylinder3d(center = matrix(c(xend-offset,xend-offset, y, y, temp, temp+z_break_length),ncol=3,nrow=2),
                                                radius=radius,closed = -2)
        counter2 = counter2 + 1
        temp = temp + z_break_length
      }
    }
  } else if (position == "S") {
    temp = xstart
    for(i in 1:segments) {
      if(i %% 2 == 1) {
        meshlist1[[counter1]] = rgl::cylinder3d(center = matrix(c(temp,temp+x_break_length, y, y, zstart+offset, zstart+offset),ncol=3,nrow=2),
                                                radius=radius,closed = -2)
        counter1 = counter1 + 1
        temp = temp + x_break_length
      } else {
        meshlist2[[counter2]] = rgl::cylinder3d(center = matrix(c(temp,temp+x_break_length, y, y, zstart+offset, zstart+offset),ncol=3,nrow=2),
                                                radius=radius,closed = -2)
        counter2 = counter2 + 1
        temp = temp + x_break_length
      }
    }
  } else if (position == "E") {
    temp = zstart
    for(i in 1:segments) {
      if(i %% 2 == 1) {
        meshlist1[[counter1]] = rgl::cylinder3d(center = matrix(c(xstart+offset,xstart+offset, y, y, temp, temp+z_break_length),ncol=3,nrow=2),
                                                radius=radius,closed = -2)
        counter1 = counter1 + 1
        temp = temp + z_break_length
      } else {
        meshlist2[[counter2]] = rgl::cylinder3d(center = matrix(c(xstart+offset,xstart+offset, y, y, temp, temp+z_break_length),ncol=3,nrow=2),
                                                radius=radius,closed = -2)
        counter2 = counter2 + 1
        temp = temp + z_break_length
      }
    }
  } 
  shapelist3d(meshlist1, lit=FALSE, ambient = "#000014", color = color_first, plot=TRUE)
  shapelist3d(meshlist2, lit=FALSE, ambient = "#000015", color = color_second, plot=TRUE)
  
  max_distance = max(limits)
  breakpoints = limits/max_distance 
  for(i in 1:length(breakpoints)) {
    if(position == "N") {
      if(text_switch_side) {
        break_dist = breakpoints[i] * xend + (1-breakpoints[i]) * xstart
        text3d(x=break_dist+text_x_offset, y=y+text_y_offset+radius*3, z=zend-offset+text_z_offset-radius*5, 
               texts = paste0(c(as.character(limits[i]), label_unit),collapse=""), color = color_text, ambient = "#000016")
      } else {
        break_dist = breakpoints[i] * xstart + (1-breakpoints[i]) * xend
        text3d(x=break_dist+text_x_offset, y=y+text_y_offset+radius*3, z=zend-offset+text_z_offset-radius*5, 
               texts = paste0(c(as.character(limits[i]), label_unit),collapse=""), color = color_text, ambient = "#000016")
      }
    } else if(position == "W") {
      if(text_switch_side) {
        break_dist = breakpoints[i] * zstart + (1-breakpoints[i]) * zend
        text3d(x=xend-offset+text_x_offset-radius*5, y=y+text_y_offset+radius*3, z=break_dist+text_z_offset, 
               texts = paste0(c(as.character(limits[i]), label_unit),collapse=""), color = color_text, ambient = "#000016")
      } else {
        break_dist = breakpoints[i] * zend + (1-breakpoints[i]) * zstart
        text3d(x=xend-offset+text_x_offset-radius*5, y=y+text_y_offset+radius*3, z=break_dist+text_z_offset, 
               texts = paste0(c(as.character(limits[i]), label_unit),collapse=""), color = color_text, ambient = "#000016")
      }
    } else if(position == "S") {
      if(text_switch_side) {
        break_dist = breakpoints[i] * xstart + (1-breakpoints[i]) * xend
        text3d(x=break_dist+text_x_offset, y=y+text_y_offset+radius*3, z=zstart+offset+text_z_offset+radius*5, 
               texts = paste0(c(as.character(limits[i]), label_unit),collapse=""), color = color_text, ambient = "#000016")
      } else {
        break_dist = breakpoints[i] * xend + (1-breakpoints[i]) * xstart
        text3d(x=break_dist+text_x_offset, y=y+text_y_offset+radius*3, z=zstart+offset+text_z_offset+radius*5, 
               texts = paste0(c(as.character(limits[i]), label_unit),collapse=""), color = color_text, ambient = "#000016")
      }
    } else if(position == "E") {
      if(text_switch_side) {
        break_dist = breakpoints[i] * zend + (1-breakpoints[i]) * zstart
        text3d(x=xstart+offset+text_x_offset+radius*5, y=y+text_y_offset+radius*3, z=break_dist+text_z_offset,
               texts = paste0(c(as.character(limits[i]), label_unit),collapse=""), color = color_text, ambient = "#000016")
      } else {
        break_dist = breakpoints[i] * zstart + (1-breakpoints[i]) * zend
        text3d(x=xstart+offset+text_x_offset+radius*5, y=y+text_y_offset+radius*3, z=break_dist+text_z_offset, 
               texts = paste0(c(as.character(limits[i]), label_unit),collapse=""), color = color_text, ambient = "#000016")
      }
    }
  }

}
