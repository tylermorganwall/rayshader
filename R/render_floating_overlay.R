#'@title Render Floating overlay
#'
#'@description Render a 2D floating overlay over the map. 
#'
#'Note: Multiple layers with transparency can cause rendering issues in rgl.
#'
#'@param overlay Overlay (4D RGBA array) to be rendered on the 3D map.
#'@param altitude Altitude to place the overlay.
#'@param heightmap The underlying surface. A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
#'@param alpha Default `1`. Multiplies the layer's transparency by this factor. 0 is completely transparent.
#'@param baseshape Default `rectangle`. Shape of the overlay. Options are `c("rectangle", "circle", "hex")`.
#'@param clear_layers Default `FALSE`. Clears all existing floating layers on the visualization.
#'@param remove_na Default `TRUE`. Whether to make the overlay transparent above empty spaces (represented by `NA` values) in the underlying elevation matrix.
#'@param reorient Default `TRUE`. Whether to reorient the image array to match the 3D plot.
#'@param horizontal_offset Default `c(0,0)`. Distance (in 3D space) to offset the floating offset in the x/y directions.
#'@param ... Additional arguments to pass to `rgl::triangles3d()`.
#'@return Adds a 3D floating layer to the map. No return value.
#'@export
#'@examples
#'if(rayshader:::run_documentation()) {
#'#Render the road network as a floating overlay layer, along with a label annotation and a floating
#'#point annotation
#'if(all(length(find.package("sf", quiet = TRUE)) > 0,
#'       length(find.package("magick", quiet = TRUE)) > 0)) {
#'  monterey = c(-121.892933,36.603053)
#'  monterey_city = sf::st_sfc(sf::st_point(monterey))
#'  
#'  #Generate Overlays
#'  road_overlay = generate_line_overlay(monterey_roads_sf, attr(montereybay,"extent"), 
#'                                       heightmap = montereybay)
#'  point_overlay = generate_point_overlay(monterey_city, color="red", size=12,
#'                                         attr(montereybay,"extent"), heightmap = montereybay)
#'                                         
#'  #Create 3D plot (water transparency set to 1 because multiple transparency layers can interfere)
#'  montereybay |>
#'    height_shade() |>
#'    add_shadow(ray_shade(montereybay,zscale=50),0.3) |> 
#'    plot_3d(montereybay, water = T, wateralpha = 1, windowsize = 800, watercolor = "lightblue")
#'  render_camera(theta=-55,phi=45,zoom=0.8)
#'  
#'  #Render label
#'  render_label(montereybay, lat = monterey[2], long = monterey[1], altitude = 9900,
#'               extent = attr(montereybay, "extent"),
#'               zscale = 50, text = "Monterey", textcolor = "black", linecolor="darkred")
#'  
#'  #Render Floating Overlays
#'  render_floating_overlay(road_overlay, altitude = 10000,zscale = 50)
#'  render_floating_overlay(point_overlay, altitude = 100,zscale = 50)
#'  render_snapshot()
#'}
#'}
render_floating_overlay = function(overlay = NULL, altitude = NULL, heightmap = NULL, zscale=1, 
                                   alpha = 1, baseshape="rectangle", remove_na = TRUE, 
                                   reorient = TRUE, clear_layers = FALSE, horizontal_offset = c(0,0),
                                   ...) {
  if(clear_layers) {
    rgl::pop3d(tag = c("floating_overlay","floating_overlay_tris"))
    if(is.null(overlay)) {
      return(invisible())
    }
  }
  if(is.null(overlay)) {
    stop("Must specify overlay")
  }
  if(is.null(altitude)) {
    stop("Must specify altitude")
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

  hm = matrix(altitude,nrow=dim(overlay)[1],ncol=dim(overlay)[2])
  tempmap = tempfile(fileext = ".png")
  if(baseshape != "rectangle") {
    overlay_alpha = is.na(generate_base_shape(overlay[,,1], baseshape, angle=30*pi/180))
    overlay[,,4][overlay_alpha] = 0
  }
  if(remove_na && !is.null(heightmap)) {
    if(any(dim(overlay)[1:2] != dim(heightmap)[1:2]) && any(is.na(heightmap))) {
      stop("If `remove_na = TRUE`, `heightmap` and `overlay` must have same number of rows and columns to make overlay transparent at those points")
    }
    if(any(is.na(heightmap))) {
      overlay[,,4][is.na(heightmap)] = 0
    }
  }
  if(is.null(heightmap)) {
    rows = nrow(hm)
    cols = ncol(hm)
  } else {
    rows = nrow(heightmap)
    cols = ncol(heightmap)
  }

  save_png(overlay,tempmap)
  dim(heightmap) = unname(dim(heightmap))
  rowmin = min((+1):(rows) - rows/2) + horizontal_offset[1]
  rowmax = max((+1):(rows) - rows/2) + horizontal_offset[1]
  colmin = min(-(+1):-(cols) + cols/2+1) + horizontal_offset[2]
  colmax = max(-(+1):-(cols) + cols/2+1) + horizontal_offset[2]
  depth = altitude/zscale
  
  tri1 = matrix(c(rowmax,rowmax,rowmin,
                  depth,depth,depth,
                  colmax,colmin,colmin), nrow=3,ncol=3)
  tri2 = matrix(c(rowmin,rowmax,rowmin,
                  depth,depth,depth,
                  colmax,colmax,colmin), nrow=3,ncol=3)
  rgl::triangles3d(x=rbind(tri1,tri2), 
                   texcoords = matrix(c(1,1,0,0,1,0,0,1,1,0,0,1),nrow=6,ncol=2),
                   normals = matrix(c(0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0), nrow=6,ncol=3),
                   texture=tempmap,  color = "white",
                   lit=FALSE,tag = "floating_overlay_tris",textype = "rgba", ...)
}
