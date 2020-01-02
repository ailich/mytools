#' Calculates Bathymetric Position Index
#'
#' Calculates Bathymetric Position Index (the difference between a central pixel and the mean of the values in the specified annulus). Positive values inidcate a crest, while negative values indicate a depression. 0 value is flat.
#' CURRENTLY DOES NOT MATCH VALUES FROM BENTHIC TERRAIN MODELLER
#'
#' @param bathy bathymetry raster
#' @param inner_rad Inner radius of annulus
#' @param outer_rad Outer radius of annulus
#' @param pad logical. If TRUE, additional 'virtual' rows and columns are padded to x such that there are no edge effects. Only applicable if na.rm=TRUE. Value of virtual cells is set to NA. Default is FALSE
#' @importFrom raster focal
#' @importFrom raster focalWeight
#' @importFrom raster res
#' @export

BPI<- function(bathy, inner_rad, outer_rad, rad_units="cell", na.rm=FALSE, pad=FALSE){
  out_name<- paste0("BPI_", as.character(inner_rad),"x", as.character(outer_rad), "_", rad_units)
  if (rad_units!="cell" & rad_units!="map"){
    message("rad_units must be 'cell' or 'map'")
    stop()}
  if((raster::res(bathy)[1]!=raster::res(bathy)[2]) & rad_units== "cell"){
    message("x and y res must be equal if rad_units is cell")
    stop()}
  if(raster::res(bathy)[1]!=raster::res(bathy)[2]){ warning("x and y res are unequal")}
  if(rad_units=="cell"){
    inner_rad<- inner_rad*(raster::res(bathy)[1])
    outer_rad<- outer_rad*(raster::res(bathy)[1])} #convert to map units
  inner_w<- raster::focalWeight(x=bathy, d=inner_rad, type='circle')
  inner_w[inner_w>0]<- 1
  inner_w[ceiling(0.5 * length(inner_w))] <- 0
  outer_w<- raster::focalWeight(x=bathy, d=outer_rad, type='circle')
  outer_w[outer_w>0]<- 1
  outer_w[ceiling(0.5 * length(outer_w))] <- 0
  diff_rows<- nrow(outer_w) - nrow(inner_w)
  diff_cols<- ncol(outer_w) - ncol(inner_w)
  inner_w<- cbind(matrix(data = 0,nrow = nrow(inner_w),ncol = diff_cols/2), inner_w, matrix(data = 0,nrow = nrow(inner_w),ncol = diff_cols/2))
  inner_w<- rbind(matrix(data = 0,nrow = diff_rows/2,ncol = ncol(inner_w)), inner_w, matrix(data = 0,nrow = diff_rows/2,ncol = ncol(inner_w)))
  if(!identical(dim(inner_w), dim(outer_w))){
    message("algorithm failed to create windows properly")
    stop()}
  w<- outer_w - inner_w #Annulus window
  if(!na.rm){
    w[w>0]<- 1/sum(w>0) #Set weights to sum to 1
    output<- bathy - raster::focal(x=bathy, w=w, fun=sum, na.rm=FALSE, pad=FALSE) #Calculate BPI
      } else {
        output<- bathy - raster::focal(x=bathy, w=w, na.rm=TRUE, pad=pad, fun = function(x, ...) {
          valXweights<- x
          my_weights<- as.vector(t(w))
          my_vals<- valXweights/my_weights
          sum(my_vals,na.rm=TRUE)/sum(my_weights, na.rm=FALSE) #weighted mean
        }) #Calculate BPI
        }
  names(output)<- out_name
  return(output)
}
