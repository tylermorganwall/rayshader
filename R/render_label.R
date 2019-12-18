#'@title Render Label
#'
#'@description Adds a marker and label to the current 3D plot
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param text The label text. 
#'@param x Either the `x` coordinate in the matrix.
#'@param y Either the `y` coordinate in the matrix.
#'@param z Elevation of the label, in units of the elevation matrix (scaled by zscale).
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
#'\dontrun{
#'montereybay %>%
#'  sphere_shade() %>%
#'  plot_3d(montereybay,zscale=50,water=TRUE)
#'render_snapshot()
#'}
#'  
#'#We want to add a label to Santa Cruz, so we use the x and y matrix coordinate (x=220 and y=330)
#'\dontrun{
#'render_label(montereybay,x=220,y=330, z=10000,zscale=50,text = "Santa Cruz")
#'render_snapshot()
#'}
#'
#'#We can also change the linetype to dashed by setting `dashed = TRUE` (additional options allow
#'#the user to control the dash length). You can clear the existing lines by setting 
#'#`clear_previous = TRUE`.
#'\dontrun{
#'render_label(montereybay, x = 300, y = 120, z = 10000, zscale = 50, text = "Monterey",
#'             textcolor = "darkred", linecolor="darkred",dashed = TRUE, clear_previous = TRUE)
#'render_snapshot()
#'}
#'
#'#By default, z specifies the altitude above that point on the elevation matrix. We can also specify 
#'#an absolute height by setting `relativez=FALSE`.
#'\dontrun{
#'render_label(montereybay,x=50,y=130, z=2000,zscale=50,text = "Monterey Canyon",relativez=FALSE)
#'render_snapshot()
#'}
#'
#'#We can remove all existing labels by calling `render_label(clear_previous = TRUE)`
#'\dontrun{
#'render_label(clear_previous = TRUE)
#'render_snapshot()
#'}
  render_label = function(heightmap, text, x, y, z, zscale=1, relativez=TRUE, offset = 0, clear_previous = FALSE, 
                        textsize=1, dashed=FALSE,dashlength = "auto", linewidth =3, antialias = FALSE,
                        alpha = 1, textalpha = 1, freetype = TRUE, adjustvec = NULL, 
                        family = "sans", fonttype = "standard", 
                        linecolor = "black", textcolor = "black") {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if(.Platform$OS.type == "unix") {
    windows = FALSE
  } else {
    windows = TRUE
  }
  exit_early = FALSE
  if(clear_previous) {
    ray_text_ids = get_ids_with_labels(c("textline", "raytext"))
    if(nrow(ray_text_ids) > 0 || missing(text)) {
      remove_ids = ray_text_ids$id
      rgl::pop3d(id = remove_ids)
      if(missing(text)) {
        exit_early = TRUE
      }
    }
  }
  if(!exit_early) {
    fontlist = list("standard"=1,"bold"=2,"italic"=3,"bolditalic"=4)
    fonttype = fontlist[[fonttype]]
    z=z/zscale
    offset = offset/zscale
    startline = heightmap[x,y]/zscale
    if(relativez)  {
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
    y = -y
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
      rgl::rgl.material(color=linecolor)
      rgl::lines3d(linelist[[i]], color = linecolor, 
                   lwd = linewidth, lit = FALSE, line_antialias = antialias,
                   depth_test = "less", alpha = alpha, ambient = "#000008")
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
           depth_test="less", ambient = "#000009", lit=FALSE)
    par3d(ignoreExtent = ignoreex)
  }
}
