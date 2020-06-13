#'@title Render Movie
#'
#'@description Renders a movie using the \pkg{av} package. Moves the camera around a 3D visualization 
#'using either a standard orbit, or accepts vectors listing user-defined values for each camera parameter. If the latter,
#'the values must be equal in length to `frames` (or of length `1`, in which the value will be fixed).
#'
#'@param filename Filename. If not appended with `.mp4`, it will be appended automatically.
#'@param type Default `orbit`, which orbits the 3D object at the user-set camera settings `phi`, `zoom`, and `fov`. 
#'Other options are `oscillate` (sine wave around `theta` value, covering 90 degrees), or `custom` (which uses the values from the 
#'`theta`, `phi`, `zoom`, and `fov` vectors passed in by the user).
#'@param frames Default `360`. Number of frames to render.
#'@param fps Default `30`. Frames per second. Recommmend either 30 or 60 for web.
#'@param phi Defaults to current view. Azimuth values, in degrees. 
#'@param theta Default to current view. Theta values, in degrees. 
#'@param zoom Defaults to the current view. Zoom value, between `0` and `1`. 
#'@param fov Defaults to the current view. Field of view values, in degrees.
#'@param title_text Default `NULL`. Text. Adds a title to the movie, using magick::image_annotate. 
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in 
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica", 
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_bar_color Default `NULL`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param title_position Default `northwest`. Position of the title.
#'@param image_overlay Default `NULL`. Either a string indicating the location of a png image to overlay
#'over the whole movie (transparency included), or a 4-layer RGBA array. This image will be resized to the 
#'dimension of the movie if it does not match exactly.
#'@param vignette Default `FALSE`. If `TRUE` or numeric, a camera vignetting effect will be added to the image.
#'`1` is the darkest vignetting, while `0` is no vignetting. If vignette is a length-2 vector, the second entry will
#'control the blurriness of the vignette effect.
#'@param audio Default `NULL`. Optional file with audio to add to the video.
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar. 
#'Will display a progress bar when adding an overlay or title.
#'@param ... Additional parameters to pass to magick::image_annotate. 
#'@export
#'@examples
#'filename_movie = tempfile()
#'
#'#By default, the function produces a 12 second orbit at 30 frames per second, at 30 degrees azimuth.
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", 
#'          waterlinecolor="white", waterlinealpha=0.5)
#'#Un-comment the following to run:
#'#render_movie(filename = filename_movie)
#'}    
#'filename_movie = tempfile()
#'
#'#You can change to an oscillating orbit. The magnification is increased and azimuth angle set to 30.
#'#A title has also been added using the title_text argument.
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", 
#'          waterlinecolor="white", waterlinealpha=0.5)
#'#Un-comment the following to run:
#'#render_movie(filename = filename_movie, type = "oscillate", 
#'#             frames = 60,  phi = 30, zoom = 0.8, theta = -90,
#'#             title_text = "Monterey Bay: Oscillating")
#'}             
#'filename_movie = tempfile()
#'
#'#Finally, you can pass your own set of values to the 
#'#camera parameters as a vector with type = "custom".
#'
#'phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
#'phivecfull = c(phivechalf, rev(phivechalf))
#'thetavec = -90 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
#'zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
#'zoomvecfull = c(zoomvec, rev(zoomvec))
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", 
#'          waterlinecolor="white", waterlinealpha=0.5)
#'#Un-comment the following to run
#'#render_movie(filename = filename_movie, type = "custom", 
#'#             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
#'rgl::rgl.close()
#'}
render_movie = function(filename, type = "orbit", frames = 360, fps = 30, 
                        phi = 30, theta = 0, zoom = NULL, fov = NULL, 
                        title_text = NULL, title_offset = c(20,20), 
                        title_color = "black", title_size = 30, title_font = "sans",
                        title_bar_color = NULL, title_bar_alpha = 0.5,
                        image_overlay = NULL, vignette = FALSE, title_position = "northwest",
                        audio=NULL, progbar = interactive(), ...) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(!("av" %in% rownames(utils::installed.packages()))) {
    stop("`av` package required for render_movie()")
  }
  if(is.null(filename)) {
    stop("render_movie requires a filename")
  }
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
  if(substring(filename, nchar(filename)-3,nchar(filename)) != ".mp4") {
    filename = paste0(filename,".mp4")
  }
  windowsize = rgl::par3d()$viewport
  if(is.null(fov)) {
    fov = rgl::par3d()$FOV
  }
  if(is.null(zoom)) {
    zoom = rgl::par3d()$zoom
  }
  if(is.null(phi) || is.null(theta)) {
    rotmat = rot_to_euler(rgl::par3d()$userMatrix)
    if(is.null(phi)) {
      phi = rotmat[1]
    }
    if(is.null(theta)) {
      if(0.001 > abs(abs(rotmat[3]) - 180)) {
        theta = -rotmat[2] + 180
      } else {
        theta = rotmat[2]
      }
    }
  }
  png_files = file.path(tempdir(), sprintf("image%d.png", seq_len(frames)))
  on.exit(unlink(png_files))
  if(type == "orbit") {
    theta_vector = seq(0,360,length.out = frames+1)[-(frames+1)]
    for(i in seq_len(frames)) {
      render_camera(theta = theta_vector[i], phi = phi, zoom = zoom, fov = fov)
      rgl::snapshot3d(filename = png_files[i], top = FALSE)
    }
  } else if (type == "oscillate") {
    theta_vector = theta + 45 * sin(seq(0,360,length.out = frames+1)[-(frames+1)]*pi/180)
    for(i in seq_len(frames)) {
      render_camera(theta = theta_vector[i], phi = phi, zoom = zoom, fov = fov)
      rgl::snapshot3d(filename = png_files[i], top = FALSE)
    }
  } else if (type == "custom") {
    if(length(theta) == 1) theta = rep(theta, frames)
    if(length(phi) == 1) phi = rep(phi, frames)
    if(length(zoom) == 1) zoom = rep(zoom, frames)
    if(length(fov) == 1) fov = rep(fov, frames)
    if(!all(c(length(theta), length(phi), length(zoom),length(fov)) == frames)) {
      stop("All camera vectors must be the same length (or fixed values)")
    }
    for(i in seq_len(frames)) {
      render_camera(theta = theta[i], phi = phi[i], zoom = zoom[i], fov = fov[i])
      rgl::snapshot3d(filename = png_files[i], top = FALSE)
    }
  } else {
    stop("Unknown type: ", type)
  }
  temp = png::readPNG(png_files[1])
  dimensions = dim(temp)
  if(dimensions[1] %% 2 != 0) {
    dimensions[1] = dimensions[1] - 1
  }
  if(has_overlay) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding overlay")
    }
    if(progbar) {
      pb = progress::progress_bar$new(
        format = "  Adding overlay image [:bar] :percent eta: :eta",
        total = frames, width= 60)
    }
    for(i in seq_len(frames)) {
      if(progbar) {
        pb$tick()
      }
      rayimage::add_image_overlay(png_files[i], image_overlay = image_overlay_file,
                                            filename = png_files[i])
    }
  }
  if(vignette || is.numeric(vignette)) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding overlay")
    }
    if(progbar) {
      pb = progress::progress_bar$new(
        format = "  Adding vignetting [:bar] :percent eta: :eta",
        total = frames, width = 60)
    }
    for(i in seq_len(frames)) {
      if(progbar) {
        pb$tick()
      }
      rayimage::add_vignette(png_files[i], vignette = vignette, filename = png_files[i])
    }
  }
  if(has_title) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding title")
    }
    if(progbar) {
      pb = progress::progress_bar$new(
        format = "  Adding title text [:bar] :percent eta: :eta",
        total = frames, width= 60)
    }
    for(i in seq_len(frames)) {
      if(progbar) {
        pb$tick()
      }
      rayimage::add_title(png_files[i], filename = png_files[i], title_text = title_text, 
                          title_bar_color = title_bar_color,title_bar_alpha = title_bar_alpha,
                          title_offset = title_offset, title_color = title_color,
                          title_position = title_position,
                          title_size = title_size, title_font = title_font)
    }
  }
  av::av_encode_video(png_files, output = filename, framerate = fps, 
                      vfilter = paste0("scale=",dimensions[1],":-2"), audio=audio)
}
