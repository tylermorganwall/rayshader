#'@title Render Compass Symbol
#'
#'@description Places a compass on the map to specify the North direction.
#'
#
#'@param angle Default `0`. The direction the arrow should be facing.
#'@param position Default `SE`. A string representing a cardinal direction. Ignored if `x`, `y`, and `z`
#'are manually specified.
#'@param altitude Default `NULL`. Altitude of the compass, defaults to maximum height in the map.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. 
#'Only used in combination with `altitude`.
#'@param x Default `NULL`. X position. If not entered, automatically calculated using `position` argument. 
#'@param y Default `NULL`. Y position. If not entered, automatically calculated using `position` argument. 
#'@param z Default `NULL`. Z position. If not entered, automatically calculated using `position` argument. 
#'@param compass_radius Default `NULL`. The radius of the compass. If not entered, automatically calculated. 
#'Increase or decrease the size of the compass.
#'@param scale_distance Default `1`. Multiplier that moves the compass away from the center of the map.
#'@param color_n Default `darkred`. Color of the letter N.
#'@param color_arrow Default `grey90`. Color of the arrow.
#'@param color_background Default `grey20`. Color of the area right under the arrow.
#'@param color_bevel Default `grey20`. Color of the bevel.
#'@param position_circular Default `FALSE`. If `TRUE`, will place compass at a constant radius away from
#'the map, as opposed to directly next to it. Overridden if user manually specifies position.
#'@param clear_compass Default `FALSE`. Clears the compass symbol(s) on the map.
#'
#'@return Adds compass to map. No return value.
#'@export
#'@examples
#'#Add a North arrow to the map, by default in the bottom right (SE)
#'if(rayshader:::run_documentation()) {
#'montereybay %>% 
#'  sphere_shade() %>%
#'  plot_3d(montereybay,theta=-45, water=TRUE)
#'render_compass()
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'#Remove the existing symbol with `clear_compass = TRUE`
#'render_compass(clear_compass = TRUE)
#'
#'#Point the N towards the light, at 315 degrees:
#'render_compass(angle = 315)
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'render_compass(clear_compass = TRUE)
#'
#'#We can change the position by specifying a direction (here are three):
#'render_camera(theta=45,phi=45)
#'render_compass(position = "NW")
#'render_compass(position = "E")
#'render_compass(position = "S")
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'render_compass(clear_compass = TRUE)
#'
#'#We can also change the distance away from the edge by setting the `scale_distance` argument.
#'render_compass(position = "NW", scale_distance = 1.4)
#'render_compass(position = "E", scale_distance = 1.4)
#'render_compass(position = "S", scale_distance = 1.4)
#'
#'#Zoom in slightly:
#'render_camera(theta=45,phi=45,zoom=0.7)
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'render_compass(clear_compass = TRUE)
#'
#'#We can also specify the radius directly with `compass_radius`:
#'render_camera(theta=0,phi=45,zoom=1)
#'render_compass(position = "N", scale_distance = 1.5, compass_radius=200)
#'render_compass(position = "E", scale_distance = 1.4, compass_radius=50)
#'render_compass(position = "S", scale_distance = 1.3, compass_radius=25)
#'render_compass(position = "W", scale_distance = 1.2, compass_radius=10)
#'render_snapshot()
#'
#'render_compass(clear_compass = TRUE)
#'}
#'if(rayshader:::run_documentation()) {
#'#We can also adjust the position manually, be specifying all x, y and z arguments.
#'render_camera(theta=-45,phi=45,zoom=0.9)
#'render_compass(x = 150, y = 50, z = 150)
#'render_snapshot()
#'}
#'if(rayshader:::run_documentation()) {
#'# Compass support is also included in render_highquality()
#'render_highquality(clamp_value=10, min_variance = 0, sample_method = "sobol_blue")
#'}
#'if(rayshader:::run_documentation()) {
#'render_compass(clear_compass = TRUE)
#'
#'#We can change the colors in the compass, and also set it a constant distance away with
#'#`position_circular = TRUE`:
#'
#'render_camera(theta=0,phi=45,zoom=0.75)
#'render_compass(position = "N", color_n = "#55967a", color_arrow = "#fff673", 
#'             color_background = "#cfe0a9", color_bevel = "#8fb28a", position_circular = TRUE)
#'render_compass(position = "NE", color_n = "black", color_arrow = "grey90", 
#'             color_background = "grey50", color_bevel = "grey20", position_circular = TRUE)
#'render_compass(position = "E", color_n = "red", color_arrow = "blue",
#'             color_background = "yellow", color_bevel = "purple", position_circular = TRUE)
#'render_compass(position = "SE", color_n = c(0.7,0.5,0.9), color_arrow = c(0.8,0.8,1), 
#'             color_background = c(0.2,0.2,1), color_bevel = c(0.6,0.4,0.6), 
#'             position_circular = TRUE)
#'render_compass(position = "S", color_n = "#ffe3b3", color_arrow = "#6a463a", 
#'             color_background = "#abaf98", color_bevel = "grey20", position_circular = TRUE)
#'render_compass(position = "SW", color_n = "#ffe3a3", color_arrow = "#f1c3a9", 
#'             color_background = "#abaf98", color_bevel = "#66615e", position_circular = TRUE)
#'render_compass(position = "W", color_n = "#e9e671", color_arrow = "#cbb387", 
#'             color_background = "#7c9695", color_bevel = "#cbb387", position_circular = TRUE)
#'render_compass(position = "NW", color_n = c(0.7,0,0), color_arrow = c(0.3,0,0), 
#'             color_background = c(0.7,0.5,0.5), color_bevel = c(0.2,0,0), position_circular = TRUE)
#'render_snapshot()
#'}
render_compass = function(angle = 0, position = "SE", altitude = NULL, zscale = 1,
                        x = NULL, y = NULL, z = NULL, compass_radius = NULL, scale_distance = 1,
                        color_n = "darkred", color_arrow = "grey90",
                        color_background = "grey60", color_bevel = "grey20",
                        position_circular = FALSE, clear_compass = FALSE) {
  if(clear_compass) {
    rgl::pop3d(tag = c("north_symbol","arrow_symbol","bevel_symbol","background_symbol"))
    return(invisible())
  }
  if(rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  radius = 1.3
  if(is.null(compass_radius)) {
    id_base = get_ids_with_labels("surface")$id
    if(length(id_base) == 0) {
      id_base = get_ids_with_labels("surface_tris")$id
    }
    fullverts = rgl::rgl.attrib(id_base,"vertices")
    xyz_range = apply(fullverts,2,range,na.rm=TRUE)
    widths = xyz_range[2,c(1,3)] - xyz_range[1,c(1,3)]
    maxwidth = max(widths) 
    compass_radius = c(maxwidth/10,maxwidth/10,maxwidth/10)
    radius = maxwidth/10
  } else if (length(compass_radius) == 1) {
    radius = compass_radius / 1.5
    compass_radius = c(radius,radius,radius)
  } else {
    stop("radius must be NULL or numeric vector of length 1")
  }
  if(is.null(x) || is.null(y) || is.null(z)) {
    id_shadow = get_ids_with_labels("shadow")$id
    if(length(id_shadow)  < 1) {
      id_base = get_ids_with_labels("surface")$id
      if(length(id_base) == 0) {
        id_base = get_ids_with_labels("surface_tris")$id
      }
      fullverts = rgl::rgl.attrib(id_base,"vertices")
    } else {
      fullverts = rgl::rgl.attrib(id_shadow,"vertices")
    }
    xyz_range = apply(fullverts,2,range,na.rm=TRUE) * scale_distance * 
      matrix(c(1,1,1/scale_distance,1/scale_distance,1,1),ncol=3,nrow=2)
    radial_dist = sqrt((xyz_range[1,1] - radius)^2 + (xyz_range[1,3] - radius)^2)
    if(is.null(altitude)) {
      y = xyz_range[2,2]
    } else {
      y = altitude/zscale
    }
    if(position == "N") {
      x = 0
      if(position_circular) {
        z = -radial_dist
      } else {
        z = xyz_range[1,3] - radius
      }
    } else if (position == "NE") {
      x = xyz_range[2,1] + radius
      z = xyz_range[1,3] - radius
    } else if (position == "E") {
      if(position_circular) {
        x = radial_dist
      } else {
        x = xyz_range[2,1] + radius
      }
      z = 0
    } else if (position == "SE") {
      x = xyz_range[2,1] + radius
      z = xyz_range[2,3] + radius
    } else if (position == "S") {
      x = 0
      if(position_circular) {
        z = radial_dist
      } else {
        z = xyz_range[2,3] + radius 
      }
    } else if (position == "SW") {
      x = xyz_range[1,1] - radius
      z = xyz_range[2,3] + radius
    } else if (position == "W") {
      if(position_circular) {
        x = -radial_dist
      } else {
        x = xyz_range[1,1] - radius
      }
      z = 0
    } else if (position == "NW") {
      x = xyz_range[1,1] - radius
      z = xyz_range[1,3] - radius
    }
  }
  north_symbol = .north_symbol_rgl
  change_color_shape = function(shapes, color, shape_index) {
    color = convert_color(color, as_hex = TRUE)
    shapes[[shape_index]]$material$color = color
    shapes
  }
  rotate_vertices = function(shapes, angle) {
    shapes$vb = apply(shapes$vb, 2, `%*%`, 
                      rgl::rotationMatrix(angle*pi/180,0,1,0))
    shapes
  }
  north_symbol = change_color_shape(north_symbol, color_n, 1)
  north_symbol = change_color_shape(north_symbol, color_arrow, 2)
  north_symbol = change_color_shape(north_symbol, color_bevel, 3)
  north_symbol = change_color_shape(north_symbol, color_background, 4)
  shade3d(translate3d(scale3d(rotate_vertices(north_symbol[[1]],angle),
                              compass_radius[1],compass_radius[2],compass_radius[3]),
                      x, y, z), 
          lit=FALSE, tag = "north_symbol", skipRedraw = FALSE)

  shade3d(translate3d(scale3d(rotate_vertices(north_symbol[[2]],angle),
                              compass_radius[1],compass_radius[2],compass_radius[3]),
                      x, y, z), 
          lit=FALSE, tag = "arrow_symbol", skipRedraw = FALSE)
  shade3d(translate3d(scale3d(rotate_vertices(north_symbol[[3]],angle),
                              compass_radius[1],compass_radius[2],compass_radius[3]),
                      x, y, z), 
          lit=FALSE, tag = "bevel_symbol", skipRedraw = FALSE)
  shade3d(translate3d(scale3d(rotate_vertices(north_symbol[[4]],angle),
                              compass_radius[1],compass_radius[2],compass_radius[3]),
                      x, y, z), 
          lit=FALSE, tag = "background_symbol", skipRedraw = FALSE)
}
