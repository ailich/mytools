#' Interpolate Ship Position
#'
#' Interpolates the ship's approximately 2Hz data to 1Hz. Will only work if there are not Multiple NA's in a row and uniform intervals
#' @param my_data column of ship's position data in Easting or Northing
#' @keywords position
#' @export

interp<- function(my_data){
  new_data<- my_data
  idx_min<- min(which(!is.na(my_data)))
  idx<- which(is.na(my_data))
  idx<- idx[idx_min:length(idx)]
  if (idx[length(idx)]==length(my_data)){idx<- idx[-length(idx)]}
  for (i in idx) {
    new_data[i]<- mean(c(my_data[i-1], my_data[i+1]))}
  return(new_data)}
