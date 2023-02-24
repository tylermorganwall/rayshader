#'@title Render Contours
#'
#'@description Adds 3D contours to the current scene, using the heightmap of the 3D surface.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All grid points are assumed to be evenly spaced.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param levels Default `NA`. Automatically generated with 10 levels. This argument specifies the exact height levels of each contour.
#'@param nlevels Default `NA`. Controls the auto-generation of levels. If levels is length-2, 
#'this will automatically generate `nlevels` breaks between `levels[1]` and `levels[2]`.
#'@param linewidth Default `3`. The line width.
#'@param antialias Default `FALSE`. If `TRUE`, the line with be have anti-aliasing applied. NOTE: anti-aliasing can cause some unpredictable behavior with transparent surfaces.
#'@param color Default `black`. Color of the line.
#'@param palette Default `NULL`. Overrides `color`. Either a function that returns a color palette 
#'of `n` colors, or a character vector with colors that specifies each color manually.
#'@param offset Default `5`. Offset of the track from the surface, if `altitude = NULL`.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing paths.
#'@export
#'@examples
#'#Add contours to the montereybay dataset
#'if(rayshader:::run_documentation()) {
#'montereybay %>%
#'  height_shade() %>%
#'  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
#'  plot_3d(montereybay, theta = -45, zscale=50)
#'render_contours(montereybay, zscale = 50, offset = 100)
#'render_snapshot()
#'}
#'
#'if(rayshader:::run_documentation()) {
#'#Specify the number of levels
#'render_contours(montereybay, zscale = 50, offset = 100, nlevels = 30,
#'                clear_previous = TRUE)
#'render_snapshot()
#'}
#'
#'if(rayshader:::run_documentation()) {
#'#Manually specify the breaks with levels
#'render_contours(montereybay, linewidth = 2,  offset = 100, zscale = 50,
#'                levels = seq(-2000, 0, 100), clear_previous = TRUE)
#'render_snapshot()
#'}
#'
#'if(rayshader:::run_documentation()) {
#'#Use a color palette for the contours 
#'volcano |> 
#'  constant_shade() |> 
#'  plot_3d(volcano, zscale = 2, solid = FALSE, zoom = 0.8)
#'palette = grDevices::colorRampPalette(c("red", "purple", "pink"))
#'render_contours(volcano, offset = 1, palette = palette, zscale = 2, nlevels = 20)
#'render_snapshot()
#'}
#'
#'if(rayshader:::run_documentation()) {
#'#Render using `render_highquality()` for a neon light effect
#'render_highquality(light = FALSE, smooth_line = TRUE, 
#'                   line_radius = 0.1, sample_method="sobol_blue",
#'                   path_material = rayrender::light, ground_size = 0,
#'                   path_material_args = list(importance_sample = FALSE,
#'                                             color = "purple", intensity = 2))
#'}
render_contours = function(heightmap = NULL, 
                           zscale = 1, 
                           levels = NA, 
                           nlevels = NA, 
                           linewidth = 3, 
                           color = "black", 
                           palette = NULL,
                           antialias = FALSE, 
                           offset = 0,
                           clear_previous = FALSE) {
  if(clear_previous) {
    rgl::pop3d(tag = "contour3d")
    if(missing(heightmap)) {
      return(invisible())
    }
  }
  if(!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop("`sf` package required for generate_contour_overlay()")
  }
  if(!(length(find.package("isoband", quiet = TRUE)) > 0)) {
    stop("`isoband` package required for generate_contour_overlay()")
  }
  if(is.na(levels[1])) {
    if(is.na(nlevels[1])) {
      nlevels = 10
    }
    rangelevels = range(heightmap,na.rm=TRUE)
    levels = seq(rangelevels[1], rangelevels[2], length.out = nlevels + 2)
  } else if (length(levels) == 2 && !is.na(nlevels)) {
    rangelevels = range(levels, na.rm=TRUE)
    levels = seq(rangelevels[1], rangelevels[2], length.out = nlevels + 2)
  }
  extent_heightmap = c(1,nrow(heightmap),1,ncol(heightmap))
  
  levels = levels[levels > min(heightmap,na.rm = TRUE)] 
  levels = levels[levels < max(heightmap,na.rm = TRUE)] 
  heightmap = flipud(t(heightmap))
  isolineval = isoband::isolines(x = 1:ncol(heightmap), 
                                 y = 1:nrow(heightmap), 
                                 z = heightmap, 
                                 levels=levels)
  contour_heights = as.numeric(names(isolineval))
  if(!is.null(palette)) {
    if(is.function(palette)) {
      color = palette(length(isolineval))
    } else {
      if(length(palette) == length(isolineval) && is.character(palette)) {
        color = palette
      }
    }
  } else {
    color = rep(color,length(isolineval))
  }
  for(i in seq_len(length(isolineval))) {
    contour_height = contour_heights[i] + offset
    render_path(lat = isolineval[[i]]$y, long = isolineval[[i]]$x,
                groups = isolineval[[i]]$id, altitude = contour_height,
                extent = extent_heightmap, tag = "contour3d", 
                zscale = zscale, linewidth = linewidth,
                offset = offset, antialias = antialias, color = color[i])
  }
}
