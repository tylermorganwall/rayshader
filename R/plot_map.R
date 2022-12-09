#'@title Plot Map
#'
#'@description Displays the map in the current device.
#'
#'@param hillshade Hillshade to be plotted.
#'@param rotate Default `0`. Rotates the output. Possible values: `0`, `90`, `180`, `270`.
#'@param asp Default `1`. Aspect ratio of the resulting plot. Use `asp = 1/cospi(mean_latitude/180)` to rescale
#'lat/long at higher latitudes to the correct the aspect ratio.
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
#'@param keep_user_par Default `TRUE`. Whether to keep the user's `par()` settings. Set to `FALSE` if you 
#'want to set up a multi-pane plot (e.g. set `par(mfrow)`).
#'@param ... Additional arguments to pass to the `raster::plotRGB` function that displays the map.
#'@export
#'@examples
#'#Plotting the Monterey Bay dataset with bathymetry data
#'if(rayshader:::run_documentation()) {
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay, texture = water_palette)
#'#For compass text
#'par(family = "Arial")
#'
#'#Set everything below 0m to water palette
#'montereybay %>%
#'  sphere_shade(zscale=10) %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map()
#'}
#'#Correcting the aspect ratio for the latitude of Monterey Bay
#'
#'extent_mb = attr(montereybay,"extent")
#'mean_latitude = mean(c(extent_mb@ymax,extent_mb@ymin))
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  sphere_shade(zscale=10) %>%
#'  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0))  %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_map(asp = 1/cospi(mean_latitude/180))
#'}
plot_map = function(hillshade, rotate=0, asp = 1, 
                    title_text = NA, title_offset = c(20,20),
                    title_color = "black", title_size = 30,
                    title_font = "sans", title_style = "normal", 
                    title_bar_color = NULL, title_bar_alpha = 0.5, title_position = "northwest",
                    keep_user_par = FALSE, ...) {
  if(keep_user_par) {
    old.par = graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  has_title = !is.na(title_text)
  if(!(length(find.package("rayimage", quiet = TRUE)) > 0) && has_title) {
    warning("`rayimage` package required for title text")
    has_title = FALSE
  }
  rotatef = function(x) t(apply(x, 2, rev))
  if(!(rotate %in% c(0,90,180,270))) {
    if(length(rotate) == 1) {
      warning(paste0("Rotation value ",rotate," not in c(0,90,180,270). Ignoring"))
    } else {
      warning(paste0("Rotation argument `rotate` not in c(0,90,180,270). Ignoring"))
    }
    number_of_rots = 0
  } else {
    number_of_rots = rotate/90
  }
  if(length(dim(hillshade)) == 3) {
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
    if(has_title) {
      hillshade = rayimage::add_title(hillshade, title_text = title_text, title_offset = title_offset,
                          title_color = title_color, title_size = title_size,
                          title_font = title_font, title_style = title_style,
                          title_bar_color = title_bar_color, title_bar_alpha = title_bar_alpha, 
                          title_position = title_position)
    }
    rayimage::plot_image(hillshade, asp = asp, ...)
  } else if(length(dim(hillshade)) == 2) {
    if(number_of_rots != 0) {
      for(j in 1:number_of_rots) {
        hillshade = rotatef(hillshade)
      }
    }
    array_from_mat = array(fliplr(t(hillshade)),dim=c(ncol(hillshade),nrow(hillshade),3))
    if(has_title) {
      array_from_mat = rayimage::add_title(array_from_mat, title_text = title_text, title_offset = title_offset,
                          title_color = title_color, title_size = title_size,
                          title_font = title_font, title_style = title_style,
                          title_bar_color = title_bar_color, title_bar_alpha = title_bar_alpha, 
                          title_position = title_position)
    }
    rayimage::plot_image(array_from_mat, asp = asp, ...)
  } else {
    stop("`hillshade` is neither array nor matrix--convert to either to plot.")
  }
}
