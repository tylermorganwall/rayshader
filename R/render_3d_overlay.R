#'@title Render Floating overlay
#'
#'@description Render a 2D/3D floating overlay over the map. 
#'
#'Note: Multiple layers with transparency can cause rendering issues in rgl.
#'
#'@param overlay Overlay to be added to the 3D map, eit
#'@param altitude Altitude to place the overlay.
#'@param heightma A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
#'
#'@return Adds a 3D floating layer to the map. No return value.
#'@export
#'@examples
#'\dontrun{
#'montereybay %>% 
#'  sphere_shade() %>%
#'  plot_3d(montereybay,theta=-45, water=TRUE)
#'render_compass()
#'render_snapshot()
#'}
render_floating_overlay = function(overlay, altitude, heightmap = NULL, zscale=1, 
                                   alpha = 1,
                                   triangulate = TRUE, max_error = 0, max_tri = 0,
                                   reorient = TRUE,
                                   verbose=FALSE) {
  if(!is.null(heightmap)) {
    reorient = FALSE
  }
  overlay = load_image(overlay, reorient)
  if(alpha != 1 && length(dim(overlay)) == 3) {
    if(dim(overlay)[3] == 4) {
      overlay[,,4] = overlay[,,4] * alpha
    }
    if(dim(overlay)[3] == 2) {
      overlay[,,2] = overlay[,,2] * alpha
    }
  }
  if(missing(altitude)) {
    stop("Must pass altitude value")
  }
  
  if(any(overlay > 1 | overlay < 0, na.rm = TRUE)) {
    stop("Argument `overlay` must not contain any entries less than 0 or more than 1")
  }
  flipud = function(x) {
    x[nrow(x):1,]
  }
  flat = FALSE
  if(is.null(heightmap) && !is.null(altitude)) {
    flat = TRUE
    heightmap = matrix(altitude,nrow=dim(overlay)[1],ncol=dim(overlay)[2])
  } else {
    heightmap = heightmap + altitude
  }
  tempmap = tempfile(fileext = ".png")
  save_png(overlay,tempmap)
  dim(heightmap) = unname(dim(heightmap))
  if(triangulate && any(is.na(heightmap))) {
    if(interactive()) {
      message("`triangulate = TRUE` cannot be currently set if any NA values present--settings `triangulate = FALSE`")
    }
    triangulate = FALSE
  }
  if(!flat) {
    if(!triangulate) {
      rgl.surface(x=1:nrow(heightmap)-nrow(heightmap)/2,z=(1:ncol(heightmap)-ncol(heightmap)/2),
                  y=heightmap/zscale,
                  texture=tempmap,lit=FALSE, tag = "3d_overlay",textype = "rgba")
    } else {
      tris = terrainmeshr::triangulate_matrix(heightmap, maxError = max_error, 
                                              maxTriangles = max_tri, start_index = 0, 
                                              verbose = verbose)
      tris[,2] =  tris[,2]/zscale
      nr = nrow(heightmap)
      nc = ncol(heightmap)
      rn = tris[,1]+1
      cn = tris[,3]+1
      texcoords = tris[,c(1,3)]
      texcoords[,1] = texcoords[,1]/nrow(heightmap)
      texcoords[,2] = texcoords[,2]/ncol(heightmap)
      tris[,1] = tris[,1] - nrow(heightmap)/2 +1
      tris[,3] = tris[,3] - ncol(heightmap)/2
      tris[,3] = -tris[,3]
      rgl.triangles(tris, texcoords = texcoords, 
                    texture=tempmap,lit=FALSE,tag = "3d_overlay_tris",textype = "rgba")
    }
  } else {
    rows = nrow(heightmap)
    cols = ncol(heightmap)
    rowmin = min((+1):(rows) - rows/2)
    rowmax = max((+1):(rows) - rows/2)
    colmin = min(-(+1):-(cols) + cols/2+1)
    colmax = max(-(+1):-(cols) + cols/2+1)
    depth = heightmap[1]/zscale
    
    tri1 = matrix(c(rowmax,rowmax,rowmin,depth,depth,depth,colmax,colmin,colmin), nrow=3,ncol=3)
    tri2 = matrix(c(rowmin,rowmax,rowmin,depth,depth,depth,colmax,colmax,colmin), nrow=3,ncol=3)
    
    rgl.triangles(rbind(tri1,tri2), texcoords = matrix(c(1,1,0,0,1,0,1,0,0,1,1,0),nrow=6,ncol=2),
                  texture=tempmap,
                  lit=FALSE,tag = "3d_overlay_tris",textype = "rgba")
  }
}