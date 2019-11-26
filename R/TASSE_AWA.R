#' Calculates TASSE terrain attributes with various window sizes
#'
#' Calculates TASSE terrain attributes using an nxn window including relative deviation from mean value (rdmv), aspect, eastness, norhtnessm slope, standard deviation, and local mean. Depth is assumed to be negative values in calculation. By default if any values within window are NA, calculated value is NA. Slope and aspect are returned in degrees.
#' (Lecours et al; 2017, Towards a framework for terrain attribute selection in environmental studies)
#' @param bathy bathymetry (or dem) raster
#' @param na.rm whether to remove na's (default is FALSE)
#' @param return_aspect logical indicating whether to return aspect (default is FALSE)
#' @importFrom raster focal
#' @importFrom raster terrain
#' @importFrom raster stack

TASSE_AWA<- function(bathy, window_size = NULL, na.rm=FALSE, opt= c(1,2), return_aspect=FALSE){
  if(length(window_size)==1){window_size<- rep(window_size, 2)}
  suffix<- paste0(as.character(window_size[1]),"x",as.character(window_size[2]))

  localmean<- focal(x = bathy, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=mean, na.rm = na.rm)
  names(localmean)<- paste0("localmean_", suffix)
  localmax<- focal(x = bathy, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=max, na.rm = na.rm)
  localmin<- focal(x = bathy, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=min, na.rm = na.rm)
  rdmv<- (bathy - localmean)/(localmax-localmin)
  names(rdmv)<- paste0("rdmv_", suffix)

  stdev<- focal(x = bathy, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=sd, na.rm = na.rm)
  names(stdev)<- paste0("stdev_", suffix)

  if(sum(opt==1)>0){
    slope_lm<- raster::terrain(x = localmean, opt = "slope", unit = "degrees", neighbors = 8)
    names(slope_lm)<- paste0("slopeLM_", suffix)
    aspect_lm<- raster::terrain(x = localmean, opt = "aspect", unit = "radians", neighbors = 8)
    northness_lm<- cos(aspect_lm)
    names(northness_lm)<- paste0("northnessLM_", suffix)
    eastness_lm<- sin(aspect_lm)
    names(eastness_lm)<- paste0("eastnessLM_", suffix)
  }

  if(sum(opt==2)>0 | prod(window_size==c(3,3))==1){
    slope_raw<- raster::terrain(x = bathy, opt = "slope", unit = "degrees", neighbors = 8)
    names(slope_raw)<- "slopeRaw_3x3"
    aspect_raw<- raster::terrain(x = bathy, opt = "aspect", unit = "radians", neighbors = 8)
    names(aspect_raw)<- "aspectRaw_3x3"
    northness_raw<- cos(aspect_raw)
    names(northness_raw)<- "northnessRaw_3x3"
    eastness_raw<- sin(aspect_raw)
    names(eastness_raw)<- "eastnessRaw_3x3"}

  if(sum(opt==2)>0){
    slope_atv<- focal(x = slope_raw, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=mean, na.rm = na.rm)
    names(slope_atv)<- paste0("slopeATV_", suffix)

    northness_atv<- focal(x = northness_raw, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=mean, na.rm = na.rm)
    names(northness_atv)<- paste0("northnessATV_", suffix)

    eastness_atv<- focal(x = eastness_raw, w= matrix(1, nrow = window_size[1], ncol = window_size[2]), fun=mean, na.rm = na.rm)
    names(eastness_atv)<- paste0("eastnessATV_", suffix)
    if(return_aspect){
      aspect_atv<- atan(eastness_atv/northness_atv)
      aspect_atv[aspect_atv<0]<- aspect_atv[aspect_atv<0] + (2*pi)
      names(aspect_atv)<- paste0("aspectATV_", suffix)
    }
  }

  output<- raster::stack(rdmv, stdev, localmean)
  if(prod(window_size==c(3,3))==1){
    output<- raster::stack(output, slope_raw, northness_raw, eastness_raw)
    if(return_aspect){
      aspect_raw<- aspect_raw*(180/pi)
      output<- raster::stack(output, aspect_raw)}}
  if(sum(opt==1)>0){
    output<- raster::stack(output, slope_lm, northness_lm, eastness_lm)
    if (return_aspect){
      aspect_lm<- aspect_lm * (180/pi)
      output<- raster::stack(output, aspect_lm)}}
  if(sum(opt==2)>0){
    output<- raster::stack(output, slope_atv, northness_atv, eastness_atv)
    if(return_aspect){
      aspect_atv<- aspect_atv*(180/pi)
      output<- raster::stack(output,aspect_atv)}}
  return(output)}
