#'@title Save PNG
#'
#'@description Writes the hillshaded map to file.
#'
#'@param hillshade Array (or matrix) of hillshade to be written.
#'@param filename String with the filename. If `.png` is not at the end of the string, it will be appended automatically.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using `magick::image_annotate()`.
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica",
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_style Default `normal`. Font style (e.g. `italic`).
#'@param title_bar_color Default `NULL`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param title_position Default `northwest`. Position of the title.
#'@param rotate Default 0. Rotates the output. Possible values: 0, 90, 180, 270.
#'@param asp Default `1`. Aspect ratio of the resulting plot. Use `asp = 1/cospi(mean_latitude/180)` to rescale
#'lat/long at higher latitudes to the correct the aspect ratio.
#'@export
#'@examples
#'filename_map = tempfile()
#'
#'#Save the map into `filename_map`
#'montereybay %>%
#'  sphere_shade() %>%
#'  save_png(filename_map)
#'  
#'#Rotate the map 180 degrees:
#'
#'montereybay %>%
#'  sphere_shade() %>%
#'  save_png(filename_map,rotate=180)
save_png = function(hillshade, filename, 
                    title_text = NA, title_offset = c(20,20),
                    title_color = "black", title_size = 30,
                    title_font = "sans", title_style = "normal",
                    title_bar_color = NULL, title_bar_alpha = 0.5, title_position = "northwest",
                    rotate = 0, asp = 1) {
  if(is.null(filename)) {
    stop("save_png requires a filename")
  }
  has_title = !is.na(title_text)
  if(!("rayimage" %in% rownames(utils::installed.packages())) && has_title) {
    warning("`rayimage` package required for title text")
    has_title = FALSE
  }
  rotatef = function(x) t(apply(x, 2, rev))
  if(!(rotate %in% c(0,90,180,270))) {
    warning(paste0("Rotation value ",rotate," not in c(0,90,180,270). Ignoring"))
    number_of_rots = 0
  } else {
    number_of_rots = rotate/90
  }
  if(substring(filename, nchar(filename)-3,nchar(filename)) != ".png") {
    filename = paste0(filename,".png")
  }
  if(length(dim(hillshade)) == 2) {
    if(number_of_rots != 0) {
      for(j in 1:number_of_rots) {
        hillshade = rotatef(hillshade)
      }
    }
    final = array(t(hillshade[,ncol(hillshade):1]),dim=c(ncol(hillshade),nrow(hillshade),3))
    if(has_title) {
      final = rayimage::add_title(final, title_text = title_text, title_offset = title_offset,
                                           title_color = title_color, title_size = title_size,
                                           title_font = title_font, title_style = title_style,
                                           title_bar_color = title_bar_color, title_bar_alpha = title_bar_alpha, 
                                           title_position = title_position)
    }
    png::writePNG(final,filename, asp = asp, text=c("source"="rayshader"))
  } else {
    if(number_of_rots != 0) {
      newarray = hillshade
      newarrayt = array(0,dim=c(ncol(hillshade),nrow(hillshade),3))
      for(i in 1:number_of_rots) {
        for(j in 1:3) {
          if(i == 2) {
            newarray[,,j] = rotatef(newarrayt[,,j])
          } else {
            newarrayt[,,j] = rotatef(newarray[,,j])
          }
        }
      }
      if(number_of_rots == 2) {
        hillshade = newarray
      } else {
        hillshade = newarrayt
      }
    }
    if(asp != 1) {
      dims = dim(hillshade)
      dimensions = c(dims[1], dims[2] * 1/asp, 3)
      temp_hillshade = array(0, dim = dimensions)
      for(i in 1:3) {
        temp_hillshade[,,i] = resize_matrix(hillshade[,,i], width = ncol(temp_hillshade), height=nrow(temp_hillshade))
      }
      temp_hillshade[temp_hillshade > 1] = 1
      temp_hillshade[temp_hillshade < 0] = 0
      hillshade = temp_hillshade
    }
    if(has_title) {
      hillshade = rayimage::add_title(hillshade, title_text = title_text, title_offset = title_offset,
                                  title_color = title_color, title_size = title_size,
                                  title_font = title_font, title_style = title_style,
                                  title_bar_color = title_bar_color, title_bar_alpha = title_bar_alpha, 
                                  title_position = title_position)
    }
    png::writePNG(hillshade, filename, text=c("source"="rayshader"))
  } 
}
