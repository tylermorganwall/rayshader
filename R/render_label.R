#'@title Render Label
#'
#'@description Adds a marker and label to the current 3D plot
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param text The label text. 
#'@param lat A latitude for the text. Must provide an `raster::extent` object to argument `extent` for the map.
#'@param long A latitude for the text. Must provide an `raster::extent` object to argument `extent` for the map.
#'@param altitude Default `NULL`. Elevation of the label, in units of the elevation matrix (scaled by zscale). If none is passed, this will default to 10 percent above the maximum altitude in the heightmap.
#'@param extent Either an object representing the spatial extent of the scene 
#' (either from the `raster`, `terra`, `sf`, or `sp` packages), 
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from 
#' the previously aforementioned packages) which will be automatically converted to an extent object. 
#'@param x Default `NULL`. Directly specify the `x` index in the matrix to place the label.
#'@param y Default `NULL`. Directly specify the `y` index in the matrix to place the label.
#'@param z Default `NULL`. Elevation of the label, in units of the elevation matrix (scaled by zscale).
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'@param relativez Default `TRUE`. Whether `z` should be measured in relation to the underlying elevation at that point in the heightmap, or set absolutely (`FALSE`).
#'@param offset Elevation above the surface (at the label point) to start drawing the line.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing text and lines rendered with `render_label()`. If no
#'other arguments are passed to `render_label()`, this will just remove all existing lines.
#'@param textsize Default `1`. A numeric character expansion value.
#'@param dashed Default `FALSE`. If `TRUE`, the label line is dashed.
#'@param dashlength Default `auto`. Length, in units of the elevation matrix (scaled by `zscale`) of the dashes if `dashed = TRUE`.
#'@param linewidth Default `3`. The line width.
#'@param antialias Default `FALSE`. If `TRUE`, the line with be have anti-aliasing applied. NOTE: anti-aliasing can cause some unpredictable behavior with transparent surfaces.
#'@param alpha Default `1`. Transparency of the label line.
#'@param textalpha Default `1`. Transparency of the label text.
#'@param freetype Default `TRUE`. Set to `FALSE` if freetype is not installed (freetype enables anti-aliased fonts). NOTE: There are occasionally transparency issues when positioning Freetype fonts in front and behind a transparent surface.
#'@param adjustvec Default `c(0.5,-0.5)`. The horizontal and vertical offset for the text. If `freetype = FALSE` and on macOS/Linux, this is adjusted to `c(0.33,-0.5)` to keep the type centered.
#'@param family Default `"sans"`. Font family. Choices are `c("serif", "sans", "mono", "symbol")`.
#'@param fonttype Default `"standard"`. The font type. Choices are `c("standard", "bold", "italic", "bolditalic")`. NOTE: These require FreeType fonts, which may not be installed on your system. See the documentation for rgl::text3d() for more information.
#'@param linecolor Default `black`. Color of the line.
#'@param textcolor Default `black`. Color of the text.
#'@export
#'@examples
#'if(run_documentation()) {
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,water=TRUE, watercolor="#233aa1")
#'render_snapshot() 
#'}
#'
#'santa_cruz = c(36.962957, -122.021033) 
#'#We want to add a label to Santa Cruz, so we use the x and y matrix coordinate (x=220 and y=330)
#'if(run_documentation()) {
#'render_label(montereybay,lat = santa_cruz[1], long = santa_cruz[2],
#'             extent = attr(montereybay, "extent"),
#'             altitude=12000, zscale=50, text = "Santa Cruz")
#'render_snapshot()
#'}
#'
#'monterey = c(36.603053, -121.892933)
#'#We can also change the linetype to dashed by setting `dashed = TRUE` (additional options allow
#'#the user to control the dash length). You can clear the existing lines by setting 
#'#`clear_previous = TRUE`.
#'if(run_documentation()) {
#'render_label(montereybay, lat = monterey[1], long = monterey[2], altitude = 10000, 
#'             extent = attr(montereybay, "extent"),
#'             zscale = 50, text = "Monterey", textcolor = "white", linecolor="darkred",
#'             dashed = TRUE, clear_previous = TRUE)
#'render_snapshot()
#'}
#'
#'canyon = c(36.621049, -122.333912)
#'#By default, z specifies the altitude above that point on the elevation matrix. We can also specify 
#'#an absolute height by setting `relativez=FALSE`.
#'if(run_documentation()) {
#'render_label(montereybay,lat=canyon[1], long = canyon[2], altitude = 2000,
#'             extent = attr(montereybay,"extent"),
#'             zscale=50,text = "Monterey Canyon", relativez=FALSE)
#'render_snapshot()
#'}
#'
#'#We can also render labels in high quality with `render_highquality()`, specifying a custom
#'#line radius. By default, the labels point towards the camera, but you can fix their angle with
#'#argument `text_angle`.
#'if(run_documentation()) {
#'render_camera(theta=35, phi = 35, zoom = 0.80, fov=60)
#'render_label(montereybay, lat = monterey[1], long = monterey[2], altitude = 10000, 
#'             extent = attr(montereybay, "extent"),
#'             zscale = 50, text = "Monterey", textcolor = "white", linecolor="darkred",
#'             dashed = TRUE, clear_previous = TRUE)
#'                
#'render_label(montereybay,lat=canyon[1], long = canyon[2], altitude = 2000, zscale=50,
#'             extent = attr(montereybay,"extent"), textcolor = "white", linecolor="white",
#'             text = "Monterey Canyon", relativez=FALSE)
#'             
#'render_highquality(samples = 128,text_size = 24, line_radius = 2, text_offset = c(0, 20, 0),
#'                   lightdirection = 180, clamp_value = 10, min_variance = 0,
#'                   sample_method = "sobol_blue")
#'}
#'if(run_documentation()) {
#'#Fixed text angle
#'render_highquality(samples = 128,text_size = 24, line_radius = 2, text_offset = c(0, 20, 0),
#'                   lightdirection = 180, text_angle = 0, clamp_value=10, min_variance = 0,
#'                   sample_method = "sobol_blue")
#'}
#'#We can remove all existing labels by calling `render_label(clear_previous = TRUE)`
#'if(run_documentation()) {
#'render_label(clear_previous = TRUE) 
#'render_snapshot()
#'}
render_label = function(heightmap, text, lat, long, altitude=NULL, extent=NULL, 
                        x=NULL, y=NULL, z=NULL, zscale=1, 
                        relativez=TRUE, offset = 0, clear_previous = FALSE, 
                        textsize=1, dashed=FALSE,dashlength = "auto", linewidth =3, antialias = FALSE,
                        alpha = 1, textalpha = 1, freetype = TRUE, adjustvec = NULL, 
                        family = "sans", fonttype = "standard", 
                        linecolor = "black", textcolor = "black") {
  exit_early = FALSE
  if(clear_previous) {
    rgl::pop3d(tag = c("textline", "raytext"))
    if(missing(text)) {
      exit_early = TRUE
    }
  }
  if(!exit_early) {
    if(!is.null(altitude)) {
      z = altitude
    }
    if(is.null(z)) {
      z = max(heightmap,na.rm=TRUE)*1.1
    }
    if(is.null(extent) && (!missing(lat) || !missing(long)) && (!is.null(x) && !is.null(y))) {
      stop("extent required when using lat/long instead of x/y")
    }
    if(!is.null(extent)) {
      e = get_extent(extent)
      x = (long-e["xmin"])/(e["xmax"] - e["xmin"]) * nrow(heightmap)
      y = ncol(heightmap) - (lat-e["ymin"])/(e["ymax"] - e["ymin"]) * ncol(heightmap)
    }
    if(rgl::cur3d() == 0) {
      stop("No rgl window currently open.")
    }
    if(.Platform$OS.type == "unix") {
      windows = FALSE
    } else {
      windows = TRUE
    }
    fontlist = list("standard"=1,"bold"=2,"italic"=3,"bolditalic"=4)
    fonttype = fontlist[[fonttype]]
    in_bounds = TRUE
    if(x > nrow(heightmap) || x < 1 || y < 1 || y > ncol(heightmap) || is.na(heightmap[x,y])) {
      in_bounds = FALSE
      shadow_id = get_ids_with_labels("shadow")$id
      if(length(shadow_id) > 0) {
        shadow_vertices = rgl::rgl.attrib(shadow_id, "vertices")
        startline = min(shadow_vertices[,2],na.rm=TRUE)
      } else {
        offset = startline
      }
    } 
    
    z=z/zscale
    offset = offset/zscale
    if(in_bounds) {
      startline = heightmap[x,y]/zscale
    } 
    if(relativez && in_bounds)  {
      z = z + startline
    }
    if(dashlength == "auto") {
      dashlength = (z - startline + offset)/20
    } else {
      dashlength = as.numeric(dashlength) 
    }
    # dashlength = dashlength/zscale
    ignoreex = par3d()$ignoreExtent
    ignoreex = par3d(ignoreExtent = TRUE)
    linelist = list()
    x = x - nrow(heightmap)/2
    y = y - ncol(heightmap)/2
    if(dashed) {
      counter = 1
      while(startline + dashlength < z) {
        linelist[[counter]] = matrix(c(x, x, startline + dashlength + offset, startline + offset, y,y),2,3)
        startline =  startline + dashlength*2
        counter = counter + 1
      }
      linelist[[counter]] = matrix(c(x, x, z + offset, startline + offset, y, y),2,3)
    } else {
      linelist[[1]] = matrix(c(x, x, z+offset, startline + offset, y,y),2,3)
    }
    for(i in 1:length(linelist)) {
      rgl::lines3d(linelist[[i]], color = linecolor, 
                   lwd = linewidth, lit = FALSE, line_antialias = antialias,
                   depth_test = "less", alpha = alpha, tag = "textline")
    }
    if(freetype) {
      seriflist = c("fonts/FreeSerif.ttf", "fonts/FreeSerifBold.ttf", 
                "fonts/FreeSerifItalic.ttf","fonts/FreeSerifBoldItalic.ttf")
      sanslist  = c("fonts/FreeSans.ttf", "fonts/FreeSansBold.ttf", 
                "fonts/FreeSansOblique.ttf","fonts/FreeSansBoldOblique.ttf")
      monolist  = c("fonts/FreeMono.ttf", "fonts/FreeMonoBold.ttf", 
                "fonts/FreeMonoOblique.ttf","fonts/FreeMonoBoldOblique.ttf")
      symbollist = c("fonts/ESSTIX10.TTF", "fonts/ESSTIX12.TTF", 
                "fonts/ESSTIX9_.TTF", "fonts/ESSTIX11.TTF")
      seriflist2 = unlist(lapply(seriflist,system.file,package="rgl"))
      sanslist2 = unlist(lapply(sanslist,system.file,package="rgl"))
      monolist2 = unlist(lapply(monolist,system.file,package="rgl"))
      symbollist2 = unlist(lapply(symbollist,system.file,package="rgl"))
      rglFonts(serif = seriflist2, sans = sanslist2, mono = monolist2, symbol = symbollist2)
      warningstring = " "
      if(family == "serif") {
        if(nchar(seriflist2[[fonttype]]) == 0) {
          family="bitmap"
          if(fonttype != 1) {
            warningstring = ", setting fonttype to \"standard\", "
          }
          freetype=FALSE
          if(!windows) {
            textsize=1
            windowsstring = "and setting textsize to 1."
          } else {
            windowsstring = "."
          }
          warning(paste0(seriflist[[fonttype]]," not found. Turning freetype off",warningstring,windowsstring))
          fonttype=1
        }
      }
      if(family == "sans") {
        if(nchar(sanslist2[[fonttype]]) == 0) {
          family="bitmap"
          if(fonttype != 1) {
            warningstring = ", setting fonttype to \"standard\", "
          }
          if(!windows) {
            textsize=1
            windowsstring = "and setting textsize to 1."
          } else {
            windowsstring = "."
          }
          freetype=FALSE
          warning(paste0(sanslist[[fonttype]]," not found. Turning freetype off",warningstring,windowsstring))
          fonttype=1
        }
      }
      if(family == "mono") {
        if(nchar(monolist2[[fonttype]]) == 0) {
          family="bitmap"
          if(fonttype != 1) {
            warningstring = ", setting fonttype to \"standard\", "
          }
          if(!windows) {
            textsize=1
            windowsstring = "and setting textsize to 1."
          } else {
            windowsstring = "."
          }
          freetype=FALSE
          warning(paste0(monolist[[fonttype]]," not found. Turning freetype off",warningstring,windowsstring))
          fonttype=1
        }
      }
      if(family == "symbol") {
        if(nchar(symbollist2[[fonttype]]) == 0) {
          family="bitmap"
          if(fonttype != 1) {
            warningstring = ", setting fonttype to \"standard\", "
          }
          if(!windows) {
            textsize=1
            windowsstring = "and setting textsize to 1."
          } else {
            windowsstring = "."
          }
          freetype=FALSE
          warning(paste0(symbollist[[fonttype]]," not found. Turning freetype off",warningstring,windowsstring))
          fonttype=1
        }
      }
    } else {
      warningstring = ""
      family = "bitmap"
      if(fonttype != 1) {
        warningstring = " and fonttype to \"standard\""
        fonttype = 1
      } 
      freetype=FALSE
      if(textsize != 1 && !windows) {
        warning("Bitmap fonts do not support variable text sizes--setting textsize back to 1",warningstring,".")
        textsize = 1
      }
    }
    if(is.null(adjustvec)) {
      if(freetype || windows) {
        adjustvec = c(0.5,-0.5)
      } else {
        adjustvec = c(0.33,-0.5)
      }
    }
    text3d(x, z+offset, y, 
           text,color=textcolor,adj=adjustvec,useFreeType=freetype,
           alpha=textalpha,family=family,font=fonttype,cex=textsize,
           depth_test="less", tag = "raytext", lit=FALSE)
    par3d(ignoreExtent = ignoreex)
  }
}
