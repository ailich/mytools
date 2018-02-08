#' Proportion of Variation Explained by each layer in a Raster PCA
#'
#' Calculates the Proportion of Variation Explained by each layer in a Raster PCA
#' @param raster_pca output of rasterPCA function
#' @keywords rasterPCA
#' @export
rPCA_prop_exp<- function(raster_pca){
  sd<- raster_pca$model$sdev
  prop_exp<- (sd^2)/sum(sd^2)
  return(prop_exp)
}
