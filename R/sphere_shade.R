#'@title Calculate Surface Color Map
#'
#'@description Calculates a color for each point on the surface using the surface normals and
#' hemispherical UV mapping. This uses either a texture map provided by the user (as an RGB array),
#' or a built-in color texture.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param sunangle Default `315` (NW). The direction of the main highlight color (derived from the built-in palettes or the `create_texture` function).
#'@param texture Default `imhof1`. Either a square matrix indicating the spherical texture mapping, or a string indicating one 
#'of the built-in palettes (`imhof1`,`imhof2`,`imhof3`,`imhof4`,`desert`, `bw`, and `unicorn`). 
#'@param normalvectors Default `NULL`. Cache of the normal vectors (from `calculate_normal` function). Supply this to speed up texture mapping.
#'@param colorintensity Default `1`. The intensity of the color mapping. Higher values will increase the intensity of the color mapping.
#'@param zscale Default `1/colorintensity`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. 
#'Ignored unless `colorintensity` missing.
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar.
#'@return RGB array of hillshaded texture mappings.
#'@export
#'@examples
#'#Basic example:
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_map()
#'  
#'#Decrease the color intensity:
#'montereybay %>%
#'  sphere_shade(colorintensity=0.1) %>%
#'  plot_map()
#'  
#'#Change to a built-in color texture:
#'montereybay %>%
#'  sphere_shade(texture="desert") %>%
#'  plot_map()
#'
#'#Change the highlight angle:
#'montereybay %>%
#'  sphere_shade(texture="desert", sunangle = 45) %>%
#'  plot_map()
#'
#'#Create our own texture using the `create_texture` function:
#'montereybay %>%
#'  sphere_shade(zscale=10,texture=create_texture("#E9C68D","#AF7F38",
#'                                                "#674F30","#494D30",
#'                                                "#B3BEA3")) %>%
#'  plot_map()
sphere_shade = function(heightmap, sunangle=315, texture="imhof1", 
                        normalvectors = NULL, colorintensity = 1, zscale=1, progbar = interactive()) {
  if(missing(zscale)) {
    if(colorintensity > 0 && is.numeric(colorintensity)) {
      zscale = 1/colorintensity
    } else {
      stop("colorintensity (value: ", colorintensity,") must be a number greater than 0")
    }
  }
  sunangle = sunangle/180*pi
  if(is.null(normalvectors)) {
    normalvectors = calculate_normal(heightmap = heightmap, zscale = zscale, progbar = progbar)
  } 
  heightmap = add_padding(heightmap)
  if(methods::is(texture,"character")) {
    if(texture %in% c("imhof1","imhof2","imhof3","imhof4","desert","bw","unicorn")) {
      if(texture == "imhof1") {
        texture = create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9")
      } else if(texture == "imhof2") {
        texture = create_texture("#f5dfca","#63372c","#dfa283","#195f67","#83a6a0")
      } else if(texture == "imhof3") {
        texture = create_texture("#e9e671","#7f3231","#cbb387","#607080","#7c9695")
      } else if(texture == "imhof4") {
        texture = create_texture("#ffe3b3","#66615e","#f1c3a9","#ac9988","#abaf98")
      } else if(texture == "bw") {
        texture = create_texture("white","black","grey75","grey25","grey50")
      } else if(texture == "desert") {
        texture = create_texture("#ffe3b3","#6a463a","#dbaf70","#9c9988","#c09c7c")
      } else if(texture == "unicorn") {
        texture = create_texture("red","green","blue","yellow","white")
      } 
    } else {
      stop("Built-in texture palette not recognized: possible choices are `imhof1`,`imhof2`,`imhof3`,`imhof4`,`bw`,`desert`, and `unicorn`")
    }
  }
  center = dim(texture)[1:2]/2
  heightmap = flipud(t(heightmap)) / zscale
  distancemat = (1 - normalvectors[["z"]]) * center[1]
  lengthmat = sqrt(1 - (normalvectors[["z"]])^2)
  image_x_nocenter = ((-normalvectors[["x"]] / lengthmat * distancemat) )
  image_y_nocenter = ((normalvectors[["y"]] / lengthmat * distancemat) )
  image_x = floor(cos(sunangle)*image_x_nocenter - sin(sunangle)*image_y_nocenter) + center[1]
  image_y = floor(sin(sunangle)*image_x_nocenter + cos(sunangle)*image_y_nocenter) + center[2]
  image_x[is.na(image_x)] = center[1]
  image_y[is.na(image_y)] = center[2]
  image_x[is.nan(image_x)] = center[1]
  image_y[is.nan(image_y)] = center[2]
  image_x[is.infinite(image_x)] = center[1]
  image_y[is.infinite(image_y)] = center[2]
  image_x[image_x > dim(texture)[1]] = dim(texture)[1]
  image_y[image_y > dim(texture)[2]] = dim(texture)[2]
  image_x[image_x == 0] = 1
  image_y[image_y == 0] = 1
  returnimage = array(dim=c(nrow(heightmap),ncol(heightmap),3))
  returnimage[,,1] = construct_matrix(texture[,,1],nrow(heightmap),ncol(heightmap), image_x, image_y)
  returnimage[,,2] = construct_matrix(texture[,,2],nrow(heightmap),ncol(heightmap), image_x, image_y)
  returnimage[,,3] = construct_matrix(texture[,,3],nrow(heightmap),ncol(heightmap), image_x, image_y)
  returnimageslice = array(dim=c(nrow(heightmap)-2,ncol(heightmap)-2,3))
  returnimageslice[,,1] = returnimage[c(-1,-nrow(heightmap)),c(-1,-ncol(heightmap)),1]
  returnimageslice[,,2] = returnimage[c(-1,-nrow(heightmap)),c(-1,-ncol(heightmap)),2]
  returnimageslice[,,3] = returnimage[c(-1,-nrow(heightmap)),c(-1,-ncol(heightmap)),3]
  return(returnimageslice)
}
