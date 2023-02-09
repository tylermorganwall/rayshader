#'@title Calculate Constant Color Map
#'
#'@description Generates a constant color layer.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. 
#'@param color Default `"white"`. Color for the constant layer.
#'@param alpha Default `1`, the alpha transparency.
#'@return RGB array of a single color layer.
#'@export
#'@examples
#'if(rayshader:::run_documentation()) {
#'#Shade a red map
#'montereybay %>%
#'  constant_shade("red") %>%
#'  add_shadow(lamb_shade(montereybay),0) |> 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Shade a green map
#'montereybay %>%
#'  constant_shade("green") %>%
#'  add_shadow(lamb_shade(montereybay),0) |> 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Add a blue tint
#'montereybay %>%
#'  height_shade() |> 
#'  add_overlay(constant_shade(montereybay, "dodgerblue", alpha=0.25)) %>%
#'  add_shadow(lamb_shade(montereybay,zscale=50),0) |> 
#'  plot_map()
#'}
#'if(rayshader:::run_documentation()) {
#'#Use a blank map on which to draw other data
#'montereybay %>%
#'  constant_shade() %>%
#'  add_overlay(generate_line_overlay(monterey_roads_sf, linewidth=5, color="black",
#'                                    attr(montereybay,"extent"), width = 1080, height = 1080),
#'                                    alphalayer=0.8)  %>%
#'  add_water(detect_water(montereybay < 0), "dodgerblue") %>%
#'  plot_map()
#'}
constant_shade = function(heightmap, color = "white", alpha = 1) {
  return_array = array(alpha, dim = c(nrow(heightmap),ncol(heightmap),4))
  const_col = convert_color(color)
  return_array[,,1] = const_col[1]
  return_array[,,2] = const_col[2]
  return_array[,,3] = const_col[3]
  return(return_array)
}
