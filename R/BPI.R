#' Calculates Bathymetric Position Index
#'
#' Calculates Bathymetric Position Index (the difference between a central pixel and the mean of the values in the specified annulus). Positive values inidcate a crest, while negative values indicate a depression. 0 value is flat.
#' CURRENTLY DOES NOT MATCH VALUES FROM BENTHIC TERRAIN MODELLER
#'
#' @param bathy bathymetry raster
#' @param inner_rad Inner radius of annulus
#' @param outer_rad Outer radius of annulus
#' @param unit unit for the radii. Either "cell" (default) or "map"
#' @param na.rm logical. If TRUE, NA will be removed from focal computations. The result will only be NA if all focal cells are NA. (Note: na.rm=TRUE will be much slower). Default is FALSE.
#' @param pad logical. If TRUE, additional 'virtual' rows and columns are padded to x such that there are no edge effects. Only applicable if na.rm=TRUE. Value of virtual cells is set to NA. Default is FALSE
#' #' @param na.rm_type default is 1. If set to 2, when na.rm=FALSE, it will remove all zero weights values from focal window before calculating so that NA's in zero weight cells do not cause calculation to evaluate to NA (Only applicable for na.rm=FALSE, and much slower than na.rm_type=1 with na.rm=FALSE)
#' @importFrom raster focal
#' @importFrom raster res
#' @export

BPI<- function(bathy, inner_rad, outer_rad, unit="cell", na.rm=FALSE, pad=FALSE, na.rm_type=1){
  out_name<- paste0("BPI_", as.character(inner_rad),"x", as.character(outer_rad), "_", unit)
  if (unit!="cell" & unit!="map"){
    message("unit must be 'cell' or 'map'")
    stop()}
  if((raster::res(bathy)[1]!=raster::res(bathy)[2]) & unit== "cell"){
    message("x and y res must be equal if unit is cell")
    stop()}
  if(raster::res(bathy)[1]!=raster::res(bathy)[2]){ warning("x and y res are unequal")}
  inner_w<- mytools::circle_window(radius = inner_rad, resolution=res(bathy), unit= unit, type=1)
  inner_w[ceiling(0.5 * length(inner_w))] <- 0
  outer_w<- mytools::circle_window(radius = outer_rad, resolution=res(bathy), unit= unit, type=1)
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
    if(na.rm_type==1){
    w[w>0]<- 1/sum(w>0) #Set weights to sum to 1
    output<- bathy - raster::focal(x=bathy, w=w, fun=sum, na.rm=FALSE, pad=FALSE) #Calculate BPI
    } else{
      output<- bathy - raster::focal(x=bathy, w=w, na.rm=FALSE, pad=FALSE,
                                     fun=function(x,...){
                                       my_vals <- x
                                       my_weights<- as.vector(t(w))
                                       idx<- my_weights!=0
                                       my_vals<- my_vals[idx] #Remove values with zero weights
                                       mean(my_vals, na.rm = FALSE)})}}
  if(na.rm) {
        output<- bathy - raster::focal(x = bathy, w = w, na.rm = TRUE,
                                       pad = pad, fun = function(x, ...) {
                                         valXweights <- x
                                         my_weights <- as.vector(t(w))
                                         my_vals <- valXweights/my_weights
                                         idx<- !(is.na(my_vals) | (my_weights==0))
                                         my_vals<- my_vals[idx]
                                         my_weights<- my_weights[idx]
                                         mean(my_vals)}) #Calculate BPI
        }
  names(output)<- out_name
  print(w)
  return(output)
}
