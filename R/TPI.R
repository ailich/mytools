#' Calculates Topographic Position Index
#'
#' Calculates Topographic Position Index (the difference between a central pixel and the mean of its surrounding cells). Unit is the unit of the input raster. Positive values inidcate a crest, while negative values indicate a depression. 0 value is flat.
#' @param bathy bathymetry raster
#' @param window_size For rectangular window, it is a vector (or integer) specifying dimensions of window in rows and columns (default is 3x3). Window size must be odd. For a circular neighborhood this value represents diameter in map units (e.g. meters).
#' @param na.rm logical. If TRUE, NA will be removed from focal computations. The result will only be NA if all focal cells are NA. (Note: na.rm=TRUE will be much slower). Default is FALSE.
#' @param use_circle logical specifying whether or not to use a circular neighborhood (default is FALSE)
#' @param pad logical. If TRUE, additional 'virtual' rows and columns are padded to x such that there are no edge effects. Only applicable if na.rm=TRUE. Value of virtual cells is set to NA. Default is FALSE
#' @importFrom raster focal
#' @importFrom raster focalWeight
#' @importFrom raster res
#' @export

TPI<- function(bathy, window_size=c(3,3), na.rm=FALSE, use_circle=FALSE, pad=FALSE){
  if(raster::res(bathy)[1]!=raster::res(bathy)[2]){ warning("x and y res are unequal")}
  if (!use_circle){
    if(length(window_size)==1){window_size<- rep(window_size, 2)}
    w <- matrix(1, nrow=window_size[1], ncol=window_size[2])
    w[ceiling(0.5 * length(w))] <- 0} else{
      focal_rad<- (window_size[1]-res(bathy)[1])/2
      w<- raster::focalWeight(x=bathy, d=focal_rad, type='circle')
      w[w>0]<- 1
      w[ceiling(0.5 * length(w))] <- 0}
  if(!na.rm){
    w[w>0]<- 1/sum(w>0)
    output<- bathy - raster::focal(x=bathy, w=w, fun=sum, na.rm=FALSE, pad=FALSE)} else{
      output<- bathy - raster::focal(r, w = w , na.rm= TRUE, pad= pad, fun = function(x, ...) {
        valXweights<- x
        my_weights<- as.vector(t(w))
        my_vals<- valXweights/my_weights
        sum(my_vals,na.rm=TRUE)/sum(my_weights, na.rm=FALSE) #weighted mean
      })
    }
  if(!use_circle){
    names(output)<- paste0("TPI_", as.character(window_size[1]),"x", as.character(window_size[2]))} else{
      names(output)<- paste0("TPI_", "circle_diam", as.character(window_size[1]))}
  return(output)
}
