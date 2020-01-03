#' Calculates Topographic Position Index
#'
#' Calculates Topographic Position Index (the difference between a central pixel and the mean of its surrounding cells). Unit is the unit of the input raster. Positive values inidcate a crest, while negative values indicate a depression. 0 value is flat.
#' @param bathy bathymetry raster
#' @param d For rectangular window, it is a vector (or integer) specifying dimensions of window in rows and columns (default is 3x3). For a circular neighborhood this value represents the radius.
#' @param na.rm logical. If TRUE, NA will be removed from focal computations. The result will only be NA if all focal cells are NA. (Note: na.rm=TRUE will be much slower). Default is FALSE.
#' @param use_circle logical specifying whether or not to use a circular neighborhood (default is FALSE)
#' @param unit unit for d if using circular window. Either "cell" (default) or "map"
#' @param pad logical. If TRUE, additional 'virtual' rows and columns are padded to x such that there are no edge effects. Only applicable if na.rm=TRUE. Value of virtual cells is set to NA. Default is FALSE
#' @param na.rm_type default is 1. If set to 2, when na.rm=FALSE, it will remove all zero weights values from focal window before calculating so that NA's in zero weight cells do not cause calculation to evaluate to NA (Only applicable for circle and na.rm=FALSE, and much slower than na.rm_type=1 with na.rm=FALSE)
#' @importFrom raster focal
#' @importFrom raster res
#' @export

TPI<- function(bathy, d=c(3,3), na.rm=FALSE, use_circle=FALSE, unit="cell", pad=FALSE, na.rm_type=1){
  if(raster::res(bathy)[1]!=raster::res(bathy)[2]){ warning("x and y res are unequal")}
  if (!use_circle){
    if(length(d)==1){d<- rep(d, 2)}
    w <- matrix(1, nrow=d[1], ncol=d[2])
    w[ceiling(0.5 * length(w))] <- 0} else{
      w<- mytools::circle_window(d, resolution=res(bathy), unit= unit, type=1)
      w[ceiling(0.5 * length(w))] <- 0}
  if(!na.rm){
    if(na.rm_type==1 | use_circle==FALSE){
      w[w>0]<- 1/sum(w>0)
      output<- bathy - raster::focal(x=bathy, w=w, fun=sum, na.rm=FALSE, pad=FALSE)} else{
        output<- bathy - raster::focal(x=bathy, w=w, na.rm=FALSE, pad=FALSE,
                                     fun=function(x,...){
                                       my_vals <- x
                                       my_weights<- as.vector(t(w))
                                       idx<- my_weights!=0
                                       my_vals<- my_vals[idx] #Remove values with zero weights
                                       mean(my_vals, na.rm = FALSE)})}
   }
  if(na.rm){
      output<- bathy - raster::focal(x = bathy, w = w, na.rm = TRUE,
                                     pad = pad, fun = function(x, ...) {
                                       my_vals <- x
                                       my_weights <- as.vector(t(w))
                                       idx<- !(is.na(my_vals) | (my_weights==0))
                                       my_vals<- my_vals[idx]
                                       my_weights<- my_weights[idx]
                                       mean(my_vals)})
      }
  if(!use_circle){
    names(output)<- paste0("TPI_", as.character(d[1]),"x", as.character(d[2]))} else{
      names(output)<- paste0("TPI_", "circle_radius", as.character(d[1]),"_", unit)}
  return(output)
}
