#'@title Render Depth of Field
#'
#'@description Adds depth of field to the current RGL scene by simulating a synthetic aperture. 
#'
#'The size of the circle of confusion is determined by the following formula (z_depth is from the image's depth map).
#'
#'\code{abs(z_depth-focus)*focal_length^2/(f_stop*z_depth*(focus - focal_length))}
#'
#'@param focus Defaults `0.5`. Depth in which to blur. Minimum 0, maximum 1.
#'@param focallength Default `1`. Focal length of the virtual camera.
#'@param fstop Default `1`. F-stop of the virtual camera.
#'@param filename The filename of the image to be saved. If this is not given, the image will be plotted instead.
#'@param bokehshape Default `circle`. Also built-in: `hex`. The shape of the bokeh. 
#'@param bokehintensity Default `3`. Intensity of the bokeh when the pixel intensity is greater than `bokehlimit`.
#'@param bokehlimit Default `0.8`. Limit after which the bokeh intensity is increased by `bokehintensity`. 
#'@param rotation Default `0`. Number of degrees to rotate the hexagon bokeh shape.
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@param transparent_water Default `FALSE`. If `TRUE`, depth is determined without water layer. User will have to re-render the water
#'layer with `render_water()` if they want to recreate the water layer.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using magick::image_annotate. 
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in 
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica", 
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param image_overlay Default `NULL`. Either a string indicating the location of a png image to overlay
#'over the image (transparency included), or a 4-layer RGBA array. This image will be resized to the 
#'dimension of the image if it does not match exactly.
#'@param progbar Default `TRUE` if in an interactive session. Displays a progress bar. 
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param bring_to_front Default `FALSE`. Whether to bring the window to the front when rendering the snapshot.
#'@param ... Additional parameters to pass to magick::image_annotate. 
#'@return 4-layer RGBA array.
#'@export
#'@examples
#'\donttest{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90)
#'  
#'render_depth(focallength = 30)
#'render_depth(focallength = 30,fstop=2)
#'render_depth(focallength = 30,fstop=2, bokehshape = "hex")
#'
#'#Add a title
#'render_depth(focallength = 30,fstop=2, clear = TRUE, 
#'             title_text = "Monterey Bay, with Depth of Field",title_offset = c(10,0))
#'}
render_depth = function(focus = 0.5, focallength = 100, fstop = 4, filename=NULL,
                     bokehshape = "circle", bokehintensity = 1, bokehlimit=0.8, 
                     rotation = 0, gamma_correction = TRUE,
                     transparent_water = FALSE, 
                     title_text = NULL, title_offset = c(20,20), 
                     title_color = "black", title_size = 30, title_font = "sans",
                     image_overlay = NULL, progbar = interactive(), clear = FALSE, 
                     bring_to_front = FALSE, ...) {
  if(focallength < 1) {
    stop("focal length must be greater than 1")
  }
  temp = paste0(tempfile(),".png")
  rgl::snapshot3d(filename=temp, top = bring_to_front)
  if(transparent_water) {
    idlist = get_ids_with_labels()
    remove_ids = idlist$id[idlist$raytype == "water"]
    rgl::pop3d(id=remove_ids)
  }
  #bokehshape 0: circle, 1: circle, 2: custom
  if(is.matrix(bokehshape)) {
    custombokeh = bokehshape
    bokehshape = 2
  } else {
    if(bokehshape == "circle") {
      bokehshape = 0
    } else {
      bokehshape = 1
    }
    custombokeh = matrix(1,1,1)
  }
  rotation = (rotation %% 360)/180*pi
  flipud = function(x) {
    x[nrow(x):1,]
  }
  dimensions = par3d()$bbox
  height = dimensions[3]
  width1 = abs(dimensions[2]-dimensions[1])
  width2 = abs(dimensions[6]-dimensions[5])
  calc_circle = function(z,zfocus,f,N) {
    abs(f^2*abs(z-zfocus)/((zfocus - f)*z*N))
  }
  depthmap = rgl::rgl.pixels(component = "depth")
  if(transparent_water) {
    idlist = get_ids_with_labels()
    remove_ids = idlist$id[idlist$raytype == "waterlines"]
    rgl::pop3d(id=remove_ids)
  }
  tempmap = png::readPNG(temp)
  if(gamma_correction) {
    tempmap = tempmap^2.2
  }
  depthmap2 = calc_circle(depthmap,focus,focallength, fstop)
  for(i in 1:3) {
    tempmap[,,i] = flipud(t(psf(t(flipud(tempmap[,,i])),depthmap2, 
                                depthmap, focus, bokehshape, custombokeh = custombokeh,
                                bokehintensity, bokehlimit, rotation, progbar = progbar,channel = i)))
  }
  if(gamma_correction) {
    tempmap = tempmap ^ (1/2.2)
  }
  tempmap[tempmap > 1] = 1
  tempmap[tempmap < 0] = 0
  png::writePNG(tempmap,temp)
  if(!is.null(title_text)) {
    has_title = TRUE
  } else {
    has_title = FALSE
  }
  if(length(title_offset) != 2) {
    stop("`title_offset` needs to be length-2 vector")
  }
  if(!is.null(image_overlay)) {
    if("character" %in% class(image_overlay)) {
      image_overlay_file = image_overlay
      has_overlay = TRUE
    } else if("array" %in% class(image_overlay)) {
      image_overlay_file = tempfile()
      png::writePNG(image_overlay_file)
      has_overlay = TRUE
    }
  } else {
    has_overlay = FALSE
  }
  dimensions = dim(tempmap)
  if(has_overlay) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding overlay")
    }
    magick::image_read(temp) %>%
      magick::image_composite(
        magick::image_scale(magick::image_read(image_overlay_file),
                            paste0(dimensions[1],"x",dimensions[2]))
      ) %>%
      magick::image_write(path = temp, format = "png")
  }
  if(has_title) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding title")
    }
    magick::image_read(temp) %>%
      magick::image_annotate(title_text, 
                             location = paste0("+", title_offset[1],"+",title_offset[2]),
                             size = title_size, color = title_color, 
                             font = title_font, ...) %>%
      magick::image_write(path = temp, format = "png")
  }
  tempmap = png::readPNG(temp)
  if(is.null(filename)) {
    plot_map(tempmap)
  } else {
    save_png(tempmap,filename)
  }
  if(clear) {
    rgl::rgl.clear()
  }
}
