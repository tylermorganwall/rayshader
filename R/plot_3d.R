#'@title Plot 3D 
#'
#'@description Displays the shaded map in 3D with the `rgl` package. 
#'
#'@param hillshade Hillshade/image to be added to 3D surface map.
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
#'@param baseshape Default `rectangle`. Shape of the base. Options are c("rectangle","circle","hex").
#'@param solid Default `TRUE`. If `FALSE`, just the surface is rendered.
#'@param soliddepth Default `auto`, which sets it to the lowest elevation in the matrix minus one unit (scaled by zscale). Depth of the solid base.
#'@param solidcolor Default `grey20`. Base color.
#'@param solidlinecolor Default `grey30`. Base edge line color.
#'@param shadow Default `TRUE`. If `FALSE`, no shadow is rendered.
#'@param shadowdepth Default `auto`, which sets it to `soliddepth - soliddepth/10`. Depth of the shadow layer.
#'@param shadowcolor Default `grey50`. Color of the shadow.
#'@param shadowwidth Default `auto`, which sizes it to 1/10th the smallest dimension of `heightmap`. Width of the shadow in units of the matrix. 
#'@param water Default `FALSE`. If `TRUE`, a water layer is rendered.
#'@param waterdepth Default `0`. Water level.
#'@param watercolor Default `lightblue`. Color of the water.
#'@param wateralpha Default `0.5`. Water transparency.
#'@param waterlinecolor Default `NULL`. Color of the lines around the edges of the water layer.
#'@param waterlinealpha Default `1`. Water line tranparency. 
#'@param linewidth Default `2`. Width of the edge lines in the scene.
#'@param lineantialias Default `FALSE`. Whether to anti-alias the lines in the scene.
#'@param theta Default `45`. Rotation around z-axis.
#'@param phi Default `45`. Azimuth angle.
#'@param fov Default `0`--isometric. Field-of-view angle.
#'@param zoom Default `1`. Zoom factor.
#'@param background Default `grey10`. Color of the background.
#'@param calculate_normals Default `TRUE`. If `FALSE`, will not calculate per-vertex normals. Can also pass the
#'output of the function `calculate_normals` to use pre-computed normals.
#'@param litbase Default `FALSE`. If `TRUE`, the base will be a glossy material lit using the 
#'`lit = TRUE` parameter in `rgl`. If calling `render_highquality()`, set this to `TRUE` to make sure
#'rgl exports the sizes of the model.
#'@param windowsize Default `600`. Position, width, and height of the `rgl` device displaying the plot. 
#'If a single number, viewport will be a square and located in upper left corner. 
#'If two numbers, (e.g. `c(600,800)`), user will specify width and height separately. 
#'If four numbers (e.g. `c(200,0,600,800)`), the first two coordinates 
#'specify the location of the x-y coordinates of the bottom-left corner of the viewport on the screen,
#'and the next two (or one, if square) specify the window size. NOTE: The absolute positioning of the
#'window does not currently work on macOS (tested on Mojave), but the size can still be specified.
#'@param ... Additional arguments to pass to the `rgl::par3d` function.
#'@import rgl
#'@export
#'@examples
#'#Plotting a spherical texture map of the built-in `montereybay` dataset.
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="desert") %>%
#'  plot_3d(montereybay,zscale=50)
#'render_snapshot(clear = TRUE)
#'}
#'
#'#With a water layer  
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="imhof2") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof2", 
#'          waterlinecolor="white", waterlinealpha=0.5)
#'render_snapshot(clear = TRUE)
#'}
#'
#'#We can also change the base by setting "baseshape" to "hex" or "circle"
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", theta=-45, zoom=0.7,
#'          waterlinecolor="white", waterlinealpha=0.5,baseshape="circle")
#'render_snapshot(clear = TRUE)
#'}
#'
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1", theta=-45, zoom=0.7,
#'          waterlinecolor="white", waterlinealpha=0.5,baseshape="hex")
#'render_snapshot(clear = TRUE)
#'}
#'
#'#Or we can carve out the region of interest ourselves, by setting those entries to NA
#'#to the elevation map passed into `plot_3d`
#'
#'#Here, we only include the deep bathymetry data by setting all points greater than -10
#'#in the copied elevation matrix to NA.
#'
#'mb_water = montereybay
#'mb_water[mb_water > -10] = NA
#'
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="imhof1") %>%
#'  plot_3d(mb_water, zscale=50, water = TRUE, watercolor="imhof1", theta=-45,
#'          waterlinecolor="white", waterlinealpha=0.5)
#'render_snapshot(clear = TRUE)
#'}
plot_3d = function(hillshade, heightmap, zscale=1, baseshape="rectangle",
                   solid = TRUE, soliddepth="auto", solidcolor="grey20",solidlinecolor="grey30",
                   shadow = TRUE, shadowdepth = "auto", shadowcolor = "grey50", shadowwidth = "auto", 
                   water = FALSE, waterdepth = 0, watercolor="lightblue", wateralpha = 0.5, 
                   waterlinecolor=NULL, waterlinealpha = 1, 
                   linewidth = 2, lineantialias = FALSE,
                   theta=45, phi = 45, fov=0, zoom = 1, 
                   background="white", calculate_normals = TRUE, litbase = FALSE,
                   windowsize = 600, ...) {
  #setting default zscale if montereybay is used and tell user about zscale
  argnameschar = unlist(lapply(as.list(sys.call()),as.character))[-1]
  argnames = as.list(sys.call())
  if(!is.null(attr(heightmap,"rayshader_data"))) {
    if (!("zscale" %in% as.character(names(argnames)))) {
      if(length(argnames) <= 3) {
        zscale = 50
        message("`montereybay` dataset used with no zscale--setting `zscale=50`. For a realistic depiction, raise `zscale` to 200.")
      } else {
        if (!is.numeric(argnames[[4]]) || !is.null(names(argnames))) {
          if(names(argnames)[4] != "")  {
            zscale = 50
            message("`montereybay` dataset used with no zscale--setting `zscale=50`.  For a realistic depiction, raise `zscale` to 200.")
          }
        }
      }
    }
  }
  #Set window size and position
  if(length(windowsize) == 1) {
    windowsize = c(0,0,windowsize,windowsize)
  } else if (length(windowsize) == 2) {
    windowsize = c(0,0,windowsize)
  } else if (length(windowsize) == 3) {
    windowsize = c(windowsize[1],windowsize[2],windowsize[1]+windowsize[3],windowsize[2]+windowsize[3])
  } else if (length(windowsize) == 4) {
    windowsize = c(windowsize[1],windowsize[2],windowsize[1]+windowsize[3],windowsize[2]+windowsize[4])
  } else {
    stop(paste0("Don't know what to do with `windowsize` argument of length ",length(windowsize)))
  }
  if(baseshape == "circle") {
    radius = ifelse(nrow(heightmap) <= ncol(heightmap),nrow(heightmap)/2-1,ncol(heightmap)/2-1)
    radmat = gen_circle_psf(radius+1)
    if(min(dim(heightmap)) != min(dim(radmat))) {
      radmat = radmat[2:nrow(radmat),2:ncol(radmat)]
    }
    if(max(dim(heightmap)) != max(dim(radmat))) {
      difference = max(dim(heightmap)) - max(dim(radmat))
      radtemp = matrix(0,nrow=nrow(heightmap),ncol=ncol(heightmap))
      if(ncol(heightmap) != ncol(radmat)) {
        radtemp[,(difference/2):(difference/2+ncol(radmat)-1)] = radmat
      } else {
        radtemp[(difference/2):(difference/2+nrow(radmat)-1),] = radmat
      }
      radmat = radtemp
    }
    heightmap[radmat == 0] = NA
  } else if(baseshape == "hex") {
    radius = ifelse(nrow(heightmap) <= ncol(heightmap),nrow(heightmap)/2-1,ncol(heightmap)/2-1)
    radmat = gen_hex_psf(radius+1,rotation = 0)
    if(min(dim(heightmap)) != min(dim(radmat))) {
      radmat = radmat[2:nrow(radmat),2:ncol(radmat)]
    }
    if(max(dim(heightmap)) != max(dim(radmat))) {
      difference = max(dim(heightmap)) - max(dim(radmat))
      radtemp = matrix(0,nrow=nrow(heightmap),ncol=ncol(heightmap))
      if(ncol(heightmap) != ncol(radmat)) {
        radtemp[,(difference/2):(difference/2+ncol(radmat)-1)] = radmat
      } else {
        radtemp[(difference/2):(difference/2+nrow(radmat)-1),] = radmat
      }
      radmat = radtemp
    }
    heightmap[radmat == 0] = NA
  }
  if(any(hillshade > 1 || hillshade < 0, na.rm = TRUE)) {
    stop("Argument `hillshade` must not contain any entries less than 0 or more than 1")
  }
  flipud = function(x) {
    x[nrow(x):1,]
  }
  if(is.null(heightmap)) {
    stop("heightmap argument missing--need to input both hillshade and original elevation matrix")
  }
  if(soliddepth == "auto") {
    soliddepth = min(heightmap,na.rm = TRUE)/zscale - (max(heightmap,na.rm = TRUE)/zscale-min(heightmap,na.rm = TRUE)/zscale)/5
  }
  if(shadowdepth == "auto") {
    shadowdepth = soliddepth - (max(heightmap,na.rm = TRUE)/zscale-min(heightmap,na.rm = TRUE)/zscale)/5
  }
  if(shadowwidth == "auto") {
    shadowwidth = floor(min(dim(heightmap))/10)
  }
  if(water) {
    if (watercolor == "imhof1") {
      watercolor = "#defcf5"
    } else if (watercolor == "imhof2") {
      watercolor = "#337c73"
    } else if (watercolor == "imhof3") {
      watercolor = "#4e7982"
    } else if (watercolor == "imhof4") {
      watercolor = "#638d99"
    } else if (watercolor == "desert") {
      watercolor = "#caf0f7"
    } else if (watercolor == "bw") {
      watercolor = "#dddddd"
    } else if (watercolor == "unicorn") {
      watercolor = "#ff00ff"
    }
    if (is.null(waterlinecolor)) {
      
    } else if (waterlinecolor == "imhof1") {
      waterlinecolor = "#f9fffb"
    } else if (waterlinecolor == "imhof2") {
      waterlinecolor = "#8accc4"
    } else if (waterlinecolor == "imhof3") {
      waterlinecolor = "#8cd4e2"
    } else if (waterlinecolor == "imhof4") {
      waterlinecolor = "#c7dfe5"
    } else if (waterlinecolor == "desert") {
      waterlinecolor = "#cde3f2"
    } else if (waterlinecolor == "bw") {
      waterlinecolor = "#ffffff"
    } else if (waterlinecolor == "unicorn") {
      waterlinecolor = "#ffd1fb"
    }
  }
  tempmap = tempfile()
  save_png(hillshade,tempmap)
  precomputed = FALSE
  if(is.list(calculate_normals)) {
    normals = calculate_normals
    calculate_normals = TRUE
    precomputed = TRUE
  }
  if(calculate_normals) {
    if(!precomputed) {
      normals = calculate_normal(heightmap,zscale=zscale)
    }
    normalsx = (t(normals$x[c(-1,-nrow(normals$x)),c(-1,-ncol(normals$x))]))
    normalsy = (t(normals$z[c(-1,-nrow(normals$z)),c(-1,-ncol(normals$z))]))
    normalsz = (t(normals$y[c(-1,-nrow(normals$y)),c(-1,-ncol(normals$y))]))
    rgl.surface(x=1:nrow(heightmap)-nrow(heightmap)/2,z=(1:ncol(heightmap)-ncol(heightmap)/2),
                y=heightmap/zscale,
                normal_x = normalsz, normal_y = normalsy, normal_z = -normalsx,
                texture=paste0(tempmap,".png"),lit=FALSE,ambient = "#000001")
  } else {
    rgl.surface(x=1:nrow(heightmap)-nrow(heightmap)/2,z=(1:ncol(heightmap)-ncol(heightmap)/2),
                y=heightmap/zscale,
                texture=paste0(tempmap,".png"),lit=FALSE,ambient = "#000001")
  }
  bg3d(color = background,texture=NULL)
  rgl.viewpoint(zoom=zoom,phi=phi,theta=theta,fov=fov)
  par3d(windowRect = windowsize,...)
  if(solid) {
    make_base(heightmap,basedepth=soliddepth,basecolor=solidcolor,zscale=zscale, litbase=litbase)
  }
  if(!is.null(solidlinecolor) && solid) {
    rgl::rgl.material(color=solidlinecolor, lit=FALSE)
    make_lines(heightmap,basedepth=soliddepth,linecolor=solidlinecolor,zscale=zscale,linewidth = linewidth)
  }
  if(shadow) {
    make_shadow(heightmap, shadowdepth, shadowwidth, background, shadowcolor)
  }
  if(water) {
    make_water(heightmap,waterheight=waterdepth,wateralpha=wateralpha,watercolor=watercolor,zscale=zscale)
  }
  if(!is.null(waterlinecolor) && water) {
    if(all(!is.na(heightmap))) {
      rgl::rgl.material(color=waterlinecolor,lit=FALSE)
      make_lines(fliplr(heightmap),basedepth=waterdepth,linecolor=waterlinecolor,zscale=zscale,linewidth = linewidth,alpha=waterlinealpha,solid=FALSE)
    }
    rgl::rgl.material(color=waterlinecolor,lit=FALSE)
    make_waterlines(heightmap,waterdepth=waterdepth,linecolor=waterlinecolor,zscale=zscale,alpha=waterlinealpha,linewidth=linewidth,antialias=lineantialias)
  }
}