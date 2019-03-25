#' Calculates relative difference from mean value from bathymetry raster
#'
#' Calculates relative difference from mean value from bathymetry raster. Rages from -1 to 1+ indicates a crest and - values indicating a depression. Calculated over a n x n window as (bathy - local_mean)/local_range. Depth is assumed to be negative values in calculation. If any values within window are NA, calculated value is NA.
#' (Lecours et al; 2017, Towards a framework for terrain attribute selection in environmental studies)
#' @param bathy bathymetry raster
#' @param window_size odd integer specifying dimensions of window (default is 3x3)
#' @importFrom raster focal
#' @export
RDMV<- function(bathy, window_size=3){
  local_mean<- focal(x = bathy, w= matrix(1, nc=window_size, nr=window_size), fun=mean, na.rm = FALSE)
  local_max<- focal(x = bathy, w= matrix(1, nc=window_size, nr=window_size), fun=max, na.rm = FALSE)
  local_min<- focal(x = bathy, w= matrix(1, nc=window_size, nr=window_size), fun=min, na.rm = FALSE)
  rdmv<- (bathy - local_mean)/(local_max-local_min)
  return(rdmv)
}
