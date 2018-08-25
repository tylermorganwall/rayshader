#'@title plot_3d
#'
#'@description Displays the shaded map in 3D with the `rgl` package. 
#'
#'@param hillshade Hillshade/image to be added to 3D surface map.
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
#'@param solid Default `TRUE`. If `FALSE`, just the surface is rendered.
#'@param soliddepth Default `auto`, which sets it to the lowest elevation in the matrix minus one unit (scaled by zscale). Depth of the solid base.
#'@param solidcolor Default `grey20`. Base color.
#'@param solidlinecolor Default `grey40`. Base edge line color.
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
#'@param theta Default `45`. Rotation around z-axis.
#'@param phi Default `45`. Azimuth angle.
#'@param fov Default `0`--isometric. Field-of-view angle.
#'@param zoom Default `1`. Zoom factor.
#'@param background Default `grey10`. Color of the background.
#'@param windowsize Default `c(600,600)`. Width and height of the `rgl` device displaying the plot.
#'@param ... Additional arguments to pass to the `rgl::par3d` function.
#'@import rgl
#'@export
#'@examples
#'#Plotting a spherical texture map of the built-in `montereybay` dataset.
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="desert") %>%
#'  plot_3d(montereybay,zscale=50)
#'}
#'
#'#With a water layer  
#'\donttest{
#'montereybay %>%
#'  sphere_shade(texture="imhof2") %>%
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof2", 
#'          waterlinecolor="white", waterlinealpha=0.5)
#'}
plot_3d = function(hillshade, heightmap, zscale=1, 
                   solid = TRUE, soliddepth="auto", solidcolor="grey20",solidlinecolor="grey40",
                   shadow = TRUE, shadowdepth = "auto", shadowcolor = "grey50", shadowwidth = "auto", 
                   water = FALSE, waterdepth = 0, watercolor="lightblue", wateralpha = 0.5, 
                   waterlinecolor=NULL, waterlinealpha = 1, 
                   linewidth = 2,
                   theta=45, phi = 45, fov=0, zoom = 1, 
                   background="white", windowsize= c(600,600), ...) {
  #setting default zscale if montereybay is used and tell user about zscale
  argnameschar = unlist(lapply(as.list(sys.call()),as.character))[-1]
  argnames = as.list(sys.call())
  if(argnameschar[2] == "montereybay" || (!is.na(argnameschar["heightmap"]) && argnameschar["heightmap"] == "montereybay")) {
    if (!("zscale" %in% as.character(names(argnames)))) {
      if(length(argnames) <= 3) {
        zscale = 200
        message("`montereybay` dataset used with no zscale--setting `zscale=200` for a realistic depiction. Lower zscale (i.e. to 50) in `plot_3d` to emphasize vertical features.")
      } else {
        if (!is.numeric(argnames[[4]]) || !is.null(names(argnames))) {
          if(names(argnames)[4] != "")  {
            zscale = 200
            message("`montereybay` dataset used with no zscale--setting `zscale=200` for a realistic depiction. Lower zscale (i.e. to 50) in `plot_3d` to emphasize vertical features.")
          }
        }
      }
    }
  }
  flipud = function(x) {
    x[,ncol(x):1]
  }
  heightmap = flipud(heightmap)
  if(is.null(heightmap)) {
    stop("heightmap argument missing--need to input both hillshade and original elevation matrix")
  }
  if(soliddepth == "auto") {
    soliddepth = min(heightmap)/zscale - (max(heightmap)/zscale-min(heightmap)/zscale)/5
  }
  if(shadowdepth == "auto") {
    shadowdepth = soliddepth - (max(heightmap)/zscale-min(heightmap)/zscale)/5
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
  write_png(hillshade,tempmap)
  rgl.surface(1:nrow(heightmap),1:ncol(heightmap),heightmap[,ncol(heightmap):1]/zscale,texture=paste0(tempmap,".png"),lit=FALSE)
  if(water) {
    rgl.surface(c(1,nrow(heightmap)),c(1,ncol(heightmap)),matrix(waterdepth,2,2),color=watercolor,lit=FALSE,alpha=wateralpha)
    make_water(heightmap/zscale,waterheight=waterdepth,wateralpha=wateralpha,watercolor=watercolor)
  }
  if(!is.null(waterlinecolor) && water) {
    make_lines(heightmap,basedepth=waterdepth,linecolor=waterlinecolor,zscale=zscale,linewidth = linewidth,alpha=waterlinealpha)
    make_waterlines(heightmap,waterdepth=waterdepth,linecolor=waterlinecolor,zscale=zscale,alpha=waterlinealpha,lwd=linewidth)
  }
  rgl.bg(color=background)
  rgl.viewpoint(zoom=zoom,phi=phi,theta=theta,fov=fov)
  par3d("windowRect" = c(0,0,windowsize), ...)
  if(solid) {
    make_base(heightmap,basedepth=soliddepth,basecolor=solidcolor,zscale=zscale)
  }
  if(!is.null(solidlinecolor) && solid) {
    make_lines(heightmap,basedepth=soliddepth,linecolor=solidlinecolor,zscale=zscale)
  }
  if(shadow) {
    make_shadow(nrow(heightmap),  ncol(heightmap), shadowdepth, shadowwidth, background, shadowcolor)
  }
}