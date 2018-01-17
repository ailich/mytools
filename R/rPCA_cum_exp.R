#' rPCA_cum_exp
#'
#' Calculates cumulative variation explained from rasterPCA
#' @param raster_pca output of rasterPCA function
#' @keywords rasterPCA
#' @export
rPCA_cum_exp<- function(raster_pca){
  sd<- raster_pca$model$sdev
  prop_exp<- (sd^2)/sum(sd^2)
  cum_exp<- cumsum(prop_exp)
  return(cum_exp)
}