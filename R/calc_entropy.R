#' Calculates Shannon entropy of a raster stack of class probabilities.
#'
#' Calculates Shannon entropy of a raster stack of class probabilities. Not to be confused with that requires categorical/integer raster values fo rlooking at agreement among an ensemble of models. Defaults to using the natural logarithm.
#' @param raster_stack A raster stack/brick object containing layers that correspond to class probabilities
#' @param  base base of logarithm (default is natural logarithm)
#' @export

calc_entropy<- function(raster_stack, base=exp(1)){
  pLog<- log(raster_stack, base=base)
  pLog[pLog==-Inf]<- 0
  raster_entropy<- -1*sum(raster_stack*pLog)
  raster_entropy[min(raster_stack)==0]<- 0 #In case pLog[pLog == -Inf] <- 0 which it seems to for large rasters
  return(raster_entropy)
}
