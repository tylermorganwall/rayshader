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
#'@param preview_focus Default `FALSE`. If `TRUE`, a red line will be drawn across the image
#'showing where the camera will be focused.
#'@param bokehshape Default `circle`. Also built-in: `hex`. The shape of the bokeh. 
#'@param bokehintensity Default `3`. Intensity of the bokeh when the pixel intensity is greater than `bokehlimit`.
#'@param bokehlimit Default `0.8`. Limit after which the bokeh intensity is increased by `bokehintensity`. 
#'@param rotation Default `0`. Number of degrees to rotate the hexagon bokeh shape.
#'@param aberration Default `0`. Adds chromatic aberration to the image. Maximum of `1`. 
#'@param gamma_correction Default `TRUE`. Controls gamma correction when adding colors. Default exponent of 2.2.
#'@param transparent_water Default `FALSE`. If `TRUE`, depth is determined without water layer. User will have to re-render the water
#'layer with `render_water()` if they want to recreate the water layer.
#'@param heightmap Default `NULL`. The height matrix for the scene. Passing this will allow `render_depth()` 
#'to automatically redraw the water layer if `transparent_water = TRUE`.
#'@param zscale Default `NULL`. The zscale value for the heightmap. Passing this will allow `render_depth()` 
#'to automatically redraw the water layer if `transparent_water = TRUE`.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using magick::image_annotate. 
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in 
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica", 
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_bar_color Default `NULL`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param image_overlay Default `NULL`. Either a string indicating the location of a png image to overlay
#'over the image (transparency included), or a 4-layer RGBA array. This image will be resized to the 
#'dimension of the image if it does not match exactly.
#'@param vignette Default `FALSE`. If `TRUE` or numeric, a camera vignetting effect will be added to the image.
#'`1` is the darkest vignetting, while `0` is no vignetting. If vignette is a length-2 vector, the second entry will
#'control the blurriness of the vignette effect.
#'@param progbar Default `TRUE` if in an interactive session. Displays a progress bar. 
#'@param instant_capture Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, a slight delay is added 
#'before taking the snapshot. This can help stop prevent rendering issues when running scripts.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param bring_to_front Default `FALSE`. Whether to bring the window to the front when rendering the snapshot.
#'@param ... Additional parameters to pass to magick::image_annotate. 
#'@return 4-layer RGBA array.
#'@export
#'@examples
#'\donttest{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50, water=TRUE, waterlinecolor="white",
#'          zoom=0.3,theta=-135,fov=70, phi=20)
#'  
#'#Preview where the focal plane lies
#'render_depth(focus=0.75, preview_focus=TRUE)
#'
#'#Render the depth of field effect
#'render_depth(focus=0.75, focallength = 100)
#'
#'#Add a chromatic aberration effect
#'render_depth(focus=0.75, focallength = 100, aberration = 0.3)
#'
#'#Render the depth of field effect, ignoring water and re-drawing the waterlayer
#'render_depth(focus=0.9, preview_focus=TRUE, 
#'             heightmap = montereybay, zscale=50, transparent_water=TRUE)
#'render_depth(focus=0.9, heightmap = montereybay, zscale=50, transparent_water=TRUE)
#'rgl::rgl.close()
#'
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50, water=TRUE, waterlinecolor="white",
#'          zoom=0.7,phi=30,fov=60,theta=-90)
#'          
#'render_camera(theta=45,zoom=0.15,phi=20)
#'
#'#Change the bokeh shape and intensity
#'render_depth(focus=0.7, bokehshape = "circle",focallength=200,bokehintensity=30,
#'             title_text = "Circular Bokeh", title_size = 20, title_color = "white", 
#'             title_bar_color = "white")
#'render_depth(focus=0.7, bokehshape = "hex",focallength=200,bokehintensity=30,
#'             title_text = "Hexagonal Bokeh", title_size = 20, title_color = "white", 
#'             title_bar_color = "white")
#'
#'#Add a title and vignette effect.
#'render_camera(theta=0,zoom=0.7,phi=30)
#'render_depth(focus = 0.75,focallength = 100, title_text = "Monterey Bay, CA", 
#'             title_size = 20, title_color = "white", title_bar_color = "black", vignette = TRUE)
#'             
#'#
#'rgl::rgl.close()
#'}
render_depth = function(focus = 0.5, focallength = 100, fstop = 4, filename=NULL,
                     preview_focus = FALSE, bokehshape = "circle", bokehintensity = 1, bokehlimit=0.8, 
                     rotation = 0, gamma_correction = TRUE, aberration = 0,
                     transparent_water = FALSE, heightmap = NULL, zscale = NULL, 
                     title_text = NULL, title_offset = c(20,20), 
                     title_color = "black", title_size = 30, title_font = "sans",
                     title_bar_color = NULL, title_bar_alpha = 0.5,
                     image_overlay = NULL, vignette = FALSE,
                     progbar = interactive(), 
                     instant_capture = interactive(), clear = FALSE, bring_to_front = FALSE, ...) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(!instant_capture) {
    Sys.sleep(0.5)
  }
  if(focallength < 1) {
    stop("focal length must be greater than 1")
  }
  temp = paste0(tempfile(),".png")
  rgl::snapshot3d(filename=temp, top = bring_to_front)
  if(transparent_water) {
    idlist = get_ids_with_labels(typeval = c("water","waterlines"))
    waterid = idlist$id[idlist$raytype == "water"][1]
    waterdepthval = max(rgl::rgl.attrib(waterid, "vertices")[1:3,2],na.rm=TRUE)
    has_waterlines = FALSE
    water_color = idlist$water_color[idlist$raytype == "water"][1]
    water_alpha = idlist$water_alpha[idlist$raytype == "water"][1]
    if("waterlines" %in% idlist$raytype) {
      has_waterlines = TRUE
      water_line_color = idlist$waterline_color[idlist$raytype == "waterlines"][1]
      water_line_alpha = idlist$waterline_alpha[idlist$raytype == "waterlines"][1]
    }
    rgl::pop3d(id=idlist$id)
  }
  if(preview_focus) {
    arraydepth = png::readPNG(temp)
    depthmap = flipud(t(rgl::rgl.pixels(component = "depth")))
    maxval = max(depthmap[depthmap != 1])
    depthmap[depthmap == 1] = maxval
    range_depth_high = focus + range(depthmap)[2]/200
    range_depth_low  = focus - range(depthmap)[1]/200
    if(range_depth_high >= maxval) {
      range_depth_high = range_depth_high - range(depthmap)[2]/200
    }
    if(any(depthmap < range_depth_high) & any(depthmap > range_depth_low)) {
      arraydepth[,,1][depthmap < range_depth_high & depthmap > range_depth_low] = 1 
      arraydepth[,,2][depthmap < range_depth_high & depthmap > range_depth_low] = 0
      arraydepth[,,2][depthmap < range_depth_high & depthmap > range_depth_low] = 0 
      print(sprintf("Focal range: %g-%g", range(depthmap)[1], range(depthmap)[2]))
      plot_map(arraydepth)
    } else {
      print(sprintf("Focus point (%g) not in focal range: %g-%g", focus, range(depthmap)[1],range(depthmap)[2]))
    }
    if(transparent_water && !is.null(heightmap)) {
      if(is.null(zscale)) {
        zscale = 1
      }
      if(has_waterlines) {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     waterlinealpha = water_line_alpha, waterlinecolor = water_line_color,
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      } else {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      }
    }
  } else {
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
    if(aberration >= 1 || aberration <= -1) {
      stop("aberration value must be less than 1 and greater than -1")
    }
    calc_circle = function(z,zfocus,f,N,ramp) {
      ifelse(z-zfocus < 0, abs(f^2*abs(z-zfocus)*ramp/((zfocus - f)*z*N)),
             abs(f^2*abs(z-zfocus)/((zfocus - f)*z*N*ramp)))
    }
    depthmap = rgl::rgl.pixels(component = "depth")
    if(transparent_water) {
      remove_ids = get_ids_with_labels(typeval = "waterlines")$id
      rgl::pop3d(id=remove_ids)
    }
    tempmap = png::readPNG(temp)
    if(transparent_water && !is.null(heightmap)) {
      if(is.null(zscale)) {
        zscale = 1
      }
      if(has_waterlines) {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     waterlinealpha = water_line_alpha, waterlinecolor = water_line_color,
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      } else {
        render_water(heightmap=heightmap, waterdepth = waterdepthval, 
                     watercolor = water_color, wateralpha = water_alpha,
                     zscale = zscale)
      }
    }
    if(gamma_correction) {
      tempmap = tempmap^2.2
    }
    for(i in 1:3) {
      if(i == 1) {
        depthmap2 = calc_circle(depthmap,focus,focallength, fstop,1+aberration)
      } else if(i ==2) {
        depthmap2 = calc_circle(depthmap,focus,focallength, fstop,1)
      } else {
        depthmap2 = calc_circle(depthmap,focus,focallength, fstop,1-aberration)
      }
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
    if(vignette || is.numeric(vignette)) {
      if(!("magick" %in% rownames(utils::installed.packages()))) {
        stop("`magick` package required for adding overlay")
      }
      if(length(vignette) > 1) {
        if(vignette[2] < 0) {
          stop("vignette[2] must be greater than 0")
        }
        radiusval = min(c(dimensions[1],dimensions[2]))/2 * vignette[2]
      } else {
        radiusval = min(c(dimensions[1],dimensions[2]))/2
      }
      if(is.numeric(vignette)) {
        if(vignette[1] > 1 || vignette[1] < 0) {
          stop("vignette value (", vignette[1],") must be between 0 and 1.")
        }
      } else {
        vignette = 0.4
      }
      imagefile = make_vignette_overlay(dimensions[1],dimensions[2], vignette, radiusval)
      magick::image_read(temp) %>%
        magick::image_composite(magick::image_read(imagefile)) %>%
        magick::image_write(path = temp, format = "png")
    }
    if(has_title) {
      if(!is.null(title_bar_color)) {
        title_bar_color = col2rgb(title_bar_color)/255
        title_bar = array(0,c(dimensions[1],dimensions[2],4))
        title_bar_width = 2 * title_offset[1] + title_size
        title_bar[1:title_bar_width,,1] = title_bar_color[1]
        title_bar[1:title_bar_width,,2] = title_bar_color[2]
        title_bar[1:title_bar_width,,3] = title_bar_color[3]
        title_bar[1:title_bar_width,,4] = title_bar_alpha
        title_bar_temp = paste0(tempfile(),".png")
        png::writePNG(title_bar,title_bar_temp)
        magick::image_read(temp) %>%
          magick::image_composite(magick::image_read(title_bar_temp),
          ) %>%
          magick::image_write(path = temp, format = "png")
      }
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
}
