#' Creates circular focal window
#'
#' Creates circular focal window around central pixel.
#' https://desktop.arcgis.com/en/arcmap/10.3/analyze/arcpy-spatial-analyst/nbrcircle-class.htm

#' @param radius radius of circular window
#' @param resolution resolution of intended raster layer (one number or a vector of length 2). Only necessary if unit= "map"
#' @param unit unit for radius. Either "cell" (number of cells, the default) or "map" for map units (e.g. meters).
#' @param type Either 1 or 2. If type = "1" then weights will be 0 or 1. if type ="2", weights will sum to 1. Default is 1.
#' @export

circle_window<- function(radius, resolution=NA, unit= "cell", type=1){
  if (length(resolution)==1){resolution<- rep(resolution,2)}
  if (unit != "map" & unit != "cell"){
    message("unit must equal 'map' or 'cell'")
    stop()}
  if(length(radius)!=1){
    message("radius must be a single integer")
    stop()}
  if (type != 1 & type != 2){
    message("type must be equal to 1 or 2")
    stop()}
  if(unit=="map"){
    nrows<- floor((radius/resolution[2])*2+1)
    ncols<- floor((radius/resolution[1])*2+1)
    if(nrows %% 2 ==0){
      nrows<- nrows+1}
    if(ncols %% 2 ==0){
      ncols<- ncols+1} #nrow and ncol must be odd to have central pixel
    x<- matrix(seq(1:ncols), nrow = nrows, ncol =ncols, byrow = TRUE)  - ((ncols+1)/2)
    y<- matrix(seq(1:nrows), nrow=nrows, ncol=ncols, byrow = FALSE) - ((nrows+1)/2)
    x<- x * resolution[1]
    y<- y * resolution[2]
    dis_mat<- sqrt((y^2)+(x^2))} #Distance from center of window
  if(unit=="cell"){
    nrows<- radius*2+1
    ncols<- radius*2+1
    x<- matrix(seq(1:ncols), nrow = nrows, ncol =ncols, byrow = TRUE)  - ((ncols+1)/2)
    y<- matrix(seq(1:nrows), nrow=nrows, ncol=ncols, byrow = FALSE) - ((nrows+1)/2)
    dis_mat<- sqrt((y^2)+(x^2))} #Distance from center of window
  output<- matrix(0, nrow = nrow(dis_mat), ncol= ncol(dis_mat))
  idx<- dis_mat <= radius
  output[idx]<-1
  if(type==2){
    output[idx]<- 1/sum(output)}
  return(output)
}
