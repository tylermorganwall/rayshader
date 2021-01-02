#'@title Calculate Texture Shading Map
#'
#'@description Calculates a shadow for each point on the surface using the method described by
#'Leland Brown in "Texture Shading: A New Technique for Depicting Terrain Relief."
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. 
#'@param detail Default `0.5`. Amount of detail in texture shading algorithm. `0` is the least detail, 
#'while `1` is the most.
#'@param contrast Default `1`,standard brightness. Amount of contrast in the texture shading. 
#'This transforms the resulting darkness using the formula `tanh(input * contrast + brightness)`.
#'@param brightness Default `0`, standard brightness. Higher values will brighten the texture hillshade, 
#'while lower values will darken it. 
#'@param transform Default `TRUE`. Whether to apply the `tanh(input * contrast + brightness)` transformation.
#'This transforms the resulting darkness using the formula `tanh(input * contrast + brightness)`.
#'@param dx Default `1`. The distance between each row of data (compared to the height axis).
#'@param dy Default `1`. The distance between each column of data (compared to the height axis).
#'@param pad Default `50`. The amount to pad the heightmap so edge effects don't appear from the
#'fourier transform. Only increase this if you encounter boundary effects.
#'@return 2D matrix of hillshade values.
#'@export
#'@examples
#' #Create a direct mapping of elevation to color:
#' \donttest{
#'
#' #Plut using default values
#' montereybay %>% 
#'   texture_shade() %>% 
#'   plot_map()
#'   
#' #Increase the level of detail
#' montereybay %>% 
#'   texture_shade(detail=1) %>% 
#'   plot_map()
#'   
#' #Decrease the level of detail
#' montereybay %>% 
#'   texture_shade(detail=0) %>% 
#'   plot_map()
#'   
#' #Increase the level of contrast
#' montereybay %>% 
#'   texture_shade(contrast=3) %>% 
#'   plot_map()
#'   
#' #Increase the brightness for this level of contrast
#' montereybay %>% 
#'   texture_shade(contrast=5, brightness = 2) %>% 
#'   plot_map()
#'   
#' #Add a texture_shade() layer into a map
#' montbay = montereybay
#' montbay[montbay < 0] = 0
#'
#' montbay %>%
#'   height_shade() %>%
#'   add_water(detect_water(montbay), color="dodgerblue") %>%
#'   add_shadow(texture_shade(montbay, detail=1/3, contrast = 5, brightness = 6),0) %>%
#'   add_shadow(lamb_shade(montbay,zscale=50),0) %>% 
#'   plot_map()
#'}
texture_shade = function(heightmap, detail=0.5, 
                         contrast = 1, brightness = 0, transform = TRUE,
                         dx = 1, dy = 1, pad = 50) {
  heightmap[is.na(heightmap)] = mean(heightmap,na.rm=TRUE)
  if(detail < 0 || detail > 1) {
    stop("`detail` should be a number between 0 and 1.")
  }
  if(dx <= 0 || dy <= 0) {
    stop("`dx` and `dy` should be greater than zero.")
  }
  if(pad < 0) {
    stop("`pad` should be an integer greater than zero.")
  }
  heightmap = heightmap - min(heightmap,na.rm=TRUE)
  heightmap = add_multi_padding(heightmap,pad)
  odd_padded_cols = FALSE
  odd_padded_rows = FALSE
  if(ncol(heightmap) %% 2 != 0) {
    odd_padded_cols = TRUE
    heightmap = cbind(heightmap[,1], heightmap)
  }
  if(nrow(heightmap) %% 2 != 0) {
    odd_padded_rows = TRUE
    heightmap = rbind(heightmap[1,], heightmap)
  }
  sfunc = function(v, dim) {
    v=v-dim/2+0.5
    v=v/dim
    3*(sinpi(v))^4/(pi^2*(2 + cospi(2*v)))*1/v^4
  }
  mfunc = function(i,j,d,dx,dy,dimx,dimy) {
    i = i - dimx/2
    j = j - dimy/2
    (2*pi)^d * ((i/dx)^2+(j/dy)^2)^(d/2)
  }
  shift_fft = function(fft_mat) {
    fftcorn_nw = fft_mat[1:(nrow(fft_mat)/2),1:(ncol(fft_mat)/2)]
    fftcorn_ne = fft_mat[1:(nrow(fft_mat)/2),(ncol(fft_mat)/2+1):ncol(fft_mat)]
    fftcorn_sw = fft_mat[(nrow(fft_mat)/2+1):nrow(fft_mat),1:(ncol(fft_mat)/2)]
    fftcorn_se = fft_mat[(nrow(fft_mat)/2+1):nrow(fft_mat),(ncol(fft_mat)/2+1):ncol(fft_mat)]
    rbind(cbind(fftcorn_se,fftcorn_sw), cbind(fftcorn_ne,fftcorn_nw))
  }
  fftmat = shift_fft(stats::fft(heightmap))
  
  conv1 = matrix(0,nrow(fftmat),ncol(fftmat))
  conv2 = matrix(0,nrow(fftmat),ncol(fftmat))

  nr = nrow(conv1)
  nc = ncol(conv1)
  
  for(i in seq_len(nr)) {
    for(j in seq_len(nc)) {
      conv1[i,j] = sfunc(i,nr) * sfunc(j,nc)
      conv2[i,j] = mfunc(i,j,d=detail,dx,dy,nr,nc)
    }
  }

  vals = flipud(abs(stats::fft(shift_fft(fftmat * conv1 * conv2), inverse = TRUE)))
  if(odd_padded_cols) {
    vals = vals[,-1]
  }
  if(odd_padded_rows) {
    vals = vals[-1,]
  }
  vals = trim_padding(vals, pad)
  if(transform) {
    vals = scales::rescale(vals, to=c(-1,1))
    return((tanh(vals*contrast + brightness) + 1 )/ 2)
  } else {
    vals = scales::rescale(vals, to=c(0,1))
    return(vals)
  }
}
