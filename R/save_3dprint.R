#'@title Save 3D Print 
#'
#'@description Writes a stereolithography (STL) file that can be used in 3D printing.
#'
#'@param filename String with the filename. If `.stl` is not at the end of the string, it will be appended automatically.
#'@param maxwidth Default `125`. Desired maximum width of the 3D print in millimeters. Uses the units set in `unit` argument. Can also pass in a string, "125mm" or "5in".
#'@param unit Default `mm`. Units of the `maxwidth` argument. Can also be set to inches with `in`. 
#'@param rotate Default `TRUE`. If `FALSE`, the map will be printing on its side. This may improve resolution for some 3D printing types.
#'@param remove_extras Default `TRUE`. Removes non-topographic features from base: lines, water, labels, and the shadow.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@return Writes an STL file to `filename`. Regardless of the unit displayed, the output STL is in millimeters.
#'@export
#'@examples
#'filename_stl = tempfile()
#'
#'#Save the STL file into `filename_stl`
#'\donttest{
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale=3)
#'render_snapshot()
#'save_3dprint(filename_stl, clear=TRUE)
#'}
#'
#'#Save the STL file into `filename_stl`, setting maximum width to 100 mm
#'\donttest{
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale=3)
#'render_snapshot()
#'save_3dprint(filename_stl, maxwidth = 100, clear=TRUE)
#'}
#'
#'#'#Save the STL file into `filename_stl`, setting maximum width to 4 inches
#'\donttest{
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale=3)
#'render_snapshot()
#'save_3dprint(filename_stl, maxwidth = 4, unit = "in", clear=TRUE)
#'}
#'#'#'#Save the STL file into `filename_stl`, setting maximum width (character) to 120mm
#'\donttest{
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(volcano,zscale=3)
#'render_snapshot()
#'save_3dprint(filename_stl, maxwidth = "120mm", clear=TRUE)
#'}
save_3dprint = function(filename,maxwidth=125,unit="mm",rotate=TRUE,remove_extras = TRUE,
                        clear=FALSE) {
  if(remove_extras) {
    idlist = get_ids_with_labels()
    remove_ids = idlist$id[!(idlist$raytype %in% c("surface","base"))]
    rgl::pop3d(id=remove_ids)
  }
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
  if(rotate) {
    zrot = matrix(0,3,3)
    zrot[1,1] = 1
    zrot[2,2] = 0
    zrot[3,3] = 0
    zrot[2,3] = 1
    zrot[3,2] = -1
  } else {
    zrot = matrix(0,3,3)
    zrot[1,1] = 1
    zrot[2,2] = 1
    zrot[3,3] = 1
  }
  rot_z_90 = function(vec) {
    vec %*% zrot
  }
  temp = paste0(tempfile(),".stl")
  rgl::writeSTL(temp)
  
  #Read STL file and manipulate
  stlfile = file(temp, "rb") 
  header = readChar(stlfile, nchars = 80)
  
  numbertriangles = readBin(stlfile, integer(),size=4, endian = "little")
  
  vertexmatrix = matrix(0,nrow=numbertriangles*3,ncol=3)
  normalmatrix = matrix(0,nrow=numbertriangles,ncol=3)
  zeros = list()
  
  for(i in 1:numbertriangles) {
    normalmatrix[i,] = readBin(stlfile, "double",size=4,endian = "little", n=3)
    vertexmatrix[3*(i-1)+1,] = readBin(stlfile, "double",size=4,endian = "little", n=3)
    vertexmatrix[3*(i-1)+2,] = readBin(stlfile, "double",size=4,endian = "little", n=3)
    vertexmatrix[3*(i-1)+3,] = readBin(stlfile, "double",size=4,endian = "little", n=3)
    zeros[[i]] = readBin(stlfile, "integer",size=2,endian = "little", n=1)
  }
  
  close.connection(stlfile)
  dim1width = abs(min(vertexmatrix[,1],na.rm = TRUE)-max(vertexmatrix[,1],na.rm = TRUE))
  dim2width = abs(min(vertexmatrix[,3],na.rm = TRUE)-max(vertexmatrix[,3],na.rm = TRUE))
  dim3width = abs(min(vertexmatrix[,2],na.rm = TRUE)-max(vertexmatrix[,2],na.rm = TRUE))
  maxdim = max(dim1width,dim2width)
  multiplier = maxwidth/maxdim
  
  stlfilewrite = file(filename, "wb")
  
  writeChar(header,stlfilewrite,nchars = 80,eos=NULL)
  adjustednumbertriangles = 0L
  for(i in 1:numbertriangles) {
    if(all(!is.nan(normalmatrix[i,,drop=FALSE])) && all(!is.nan(vertexmatrix[3*(i-1)+1,,drop=FALSE])) &&
       all(!is.nan(vertexmatrix[3*(i-1)+2,,drop=FALSE])) && all(!is.nan(vertexmatrix[3*(i-1)+3,,drop=FALSE]))) {
      adjustednumbertriangles = adjustednumbertriangles + 1L
    }
  }
  writeBin(adjustednumbertriangles, stlfilewrite, size=4, endian = "little")
  
  for(i in 1:numbertriangles) {
    if(all(!is.nan(normalmatrix[i,,drop=FALSE])) && all(!is.nan(vertexmatrix[3*(i-1)+1,,drop=FALSE])) &&
    all(!is.nan(vertexmatrix[3*(i-1)+2,,drop=FALSE])) && all(!is.nan(vertexmatrix[3*(i-1)+3,,drop=FALSE]))) {
      writeBin(as.double(rot_z_90(normalmatrix[i,,drop=FALSE])), stlfilewrite,endian = "little",size=4)
      writeBin(as.double(rot_z_90(vertexmatrix[3*(i-1)+1,,drop=FALSE])*multiplier), stlfilewrite, endian = "little",size=4)
      writeBin(as.double(rot_z_90(vertexmatrix[3*(i-1)+2,,drop=FALSE])*multiplier), stlfilewrite, endian = "little",size=4)
      writeBin(as.double(rot_z_90(vertexmatrix[3*(i-1)+3,,drop=FALSE])*multiplier), stlfilewrite, endian = "little",size=4)
      writeBin(0L, stlfilewrite,size=2, endian = "little")
    }
  }
  close.connection(stlfilewrite)
  if(!rotate) {
    temp1 = dim2width
    dim2width = dim3width
    dim3width = temp1
  }
  if(unit == "mm") {
    message(sprintf("Dimensions of model are: %1.1f mm x %1.1f mm x %1.1f mm",dim1width*multiplier,dim2width*multiplier,dim3width*multiplier))
  } else {
    message(sprintf("Dimensions of model are: %1.2f in x %1.2f in x %1.2f in",dim1width*0.0393*multiplier,dim2width*0.0393*multiplier,dim3width*0.0393*multiplier))
  }
  if(clear) {
    rgl::rgl.clear()
  }
}