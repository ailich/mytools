#' Calculates relative difference from mean value from bathymetry raster
#'
#' Calculates relative difference from mean value from bathymetry raster. Rages from -1 to 1+ indicates a crest and - values indicating a depression. Calculated over a n x n window as (bathy - local_mean)/local_range. Depth is assumed to be negative values in calculation. By default if any values within window are NA, calculated value is NA.
#' (Lecours et al; 2017, Towards a framework for terrain attribute selection in environmental studies)
#' @param bathy bathymetry raster
#' @param window_size vector (or integer) specifying dimensions of window in rows and columns (default is 3x3). Window size must be odd
#' @param na.rm logical. If TRUE, NA will be removed from focal computations. The result will only be NA if all focal cells are NA
#' @importFrom raster focal
#' @export
RDMV<- function(bathy, window_size=c(3,3), na.rm=FALSE){
  if(length(window_size)==1){window_size<- rep(window_size,2)}
  local_mean<- focal(x = bathy, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=mean, na.rm = na.rm)
  local_max<- focal(x = bathy, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=max, na.rm = na.rm)
  local_min<- focal(x = bathy, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=min, na.rm = na.rm)
  rdmv<- (bathy - local_mean)/(local_max-local_min)
  rdmv[local_max==local_min]<- 0 #Fix divide by 0 issue (These areas are flat so should be 0)
  return(rdmv)
}
