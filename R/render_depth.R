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
#'@param progbar Default `TRUE` if in an interactive session. Displays a progress bar. 
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
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
#'render_depth(focallength = 30,fstop=2, clear = TRUE)
#'}
render_depth = function(focus = 0.5, focallength = 100, fstop = 4, filename=NULL,
                     bokehshape = "circle", bokehintensity = 1, bokehlimit=0.8, 
                     rotation = 0, gamma_correction = TRUE,
                     transparent_water = FALSE, progbar = interactive(),
                     clear = FALSE){
  if(focallength < 1) {
    stop("focal length must be greater than 1")
  }
  temp = paste0(tempfile(),".png")
  rgl::snapshot3d(filename=temp)
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

  if(is.null(filename)) {
    plot_map(tempmap)
  } else {
    save_png(tempmap,filename)
  }
  if(clear) {
    rgl::rgl.clear()
  }
}
