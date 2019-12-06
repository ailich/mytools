#' Calculates TASSE terrain attributes with 3x3 window
#'
#' Calculates TASSE terrain attributes using a 3x3 window including relative deviation from mean value (rdmv), aspect, eastness, norhtnessm slope, standard deviation, and local mean. Depth is assumed to be negative values in calculation. By default if any values within window are NA, calculated value is NA. Slope and aspect are returned in degrees.
#' (Lecours et al; 2017, Towards a framework for terrain attribute selection in environmental studies)
#' @param bathy bathymetry (or dem) raster
#' @param na.rm whether to remove na's (default is FALSE)
#' @param return_aspect logical indicating whether to return aspect (default is FALSE)
#' @importFrom raster focal
#' @importFrom raster terrain
#' @importFrom raster stack

TASSE<- function(bathy, na.rm=FALSE, return_aspect=FALSE){
  localmean<- focal(x = bathy, w= matrix(1, nrow = 3, ncol = 3), fun=mean, na.rm = na.rm)
  names(localmean)<- "localmean"
  localmax<- focal(x = bathy, w= matrix(1, nrow = 3, ncol = 3), fun=max, na.rm = na.rm)
  localmin<- focal(x = bathy, w= matrix(1, nrow = 3, ncol = 3), fun=min, na.rm = na.rm)
  rdmv<- (bathy - localmean)/(localmax-localmin)
  names(rdmv)<- "rdmv"
  slope<- raster::terrain(x = bathy, opt = "slope", unit = "degrees", neighbors = 8)
  aspect<- raster::terrain(x = bathy, opt = "aspect", unit = "radians", neighbors = 8)
  northness<- cos(aspect)
  names(northness)<- "northness"
  eastness<- sin(aspect)
  names(eastness)<- "eastness"
  stdev<- focal(x = bathy, w= matrix(1, nrow = 3, ncol = 3), fun=sd, na.rm = na.rm)
  names(stdev)<- "stdev"
  if (return_aspect){
    aspect<- aspect * (180/pi)
    output<- raster::stack(rdmv, stdev, localmean, slope, eastness, northness, aspect)} else{
      output<- raster::stack(rdmv, stdev, localmean, slope, eastness, northness)}
}

