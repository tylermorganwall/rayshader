#'@title Save 3D Print 
#'
#'@description Writes a stereolithography (STL) file that can be used in 3D printing.
#'
#'@param filename String with the filename. If `.stl` is not at the end of the string, it will be appended automatically.
#'@param maxwidth Default `125`. Desired maximum width of the 3D print in millimeters. Uses the units set in `unit` argument. Can also pass in a string, "125mm" or "5in".
#'@param unit Default `mm`. Units of the `maxwidth` argument. Can also be set to inches with `in`. 
#'@param rotate Default `TRUE`. If `FALSE`, the map will be printing on its side. This may improve resolution for some 3D printing types.
#'@return Writes an STL file to `filename`. Regardless of the unit displayed, the output STL is in millimeters.
#'@export
#'@examples
#'filename_stl = tempfile()
#'
#'#Save the STL file into `filename_stl`
#'if(run_documentation()) {
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale=3)
#'render_snapshot()
#'save_3dprint(filename_stl)
#'}
#'
#'#Save the STL file into `filename_stl`, setting maximum width to 100 mm
#'if(run_documentation()) {
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale=3)
#'render_snapshot()
#'save_3dprint(filename_stl, maxwidth = 100)
#'}
#'
#'#'#Save the STL file into `filename_stl`, setting maximum width to 4 inches
#'if(run_documentation()) {
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale=3)
#'render_snapshot()
#'save_3dprint(filename_stl, maxwidth = 4, unit = "in")
#'}
#'#'#'#Save the STL file into `filename_stl`, setting maximum width (character) to 120mm
#'if(run_documentation()) {
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale=3)
#'render_snapshot()
#'save_3dprint(filename_stl, maxwidth = "120mm")
#'}
save_3dprint = function(filename,maxwidth=125,unit="mm", rotate = FALSE) {
  if(substring(filename, nchar(filename)-3,nchar(filename)) != ".stl") {
    filename = paste0(filename,".stl")
  }
  inch2mm = function(inch) {
    inch/0.0393
  }
  if(methods::is(maxwidth,"character")) {
    unit =  substr(maxwidth,nchar(maxwidth)-1,nchar(maxwidth))
    maxwidth = as.numeric(substr(maxwidth,1,nchar(maxwidth)-2))
  }
  if(unit == "in") {
    maxwidth = inch2mm(maxwidth)
  }
  if(!(unit %in% c("in", "mm"))) {
    stop(paste0("unit: ",unit," not recognized: use `mm` or `in`."))
  }
  write_stl(filename, rotate = rotate, maxwidth = maxwidth, unit=unit)
}
