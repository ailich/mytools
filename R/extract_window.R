#' Extracts relevant window from raster based on position of central pixel and window size
#'
#' @param r a raster
#' @param w A vector of length 2 specifying the dimensions of the rectangular window to use where the first number is the number of rows and the second number is the number of columns. Window size must be an odd number.
#' @param idx  A vector of length 2 representing the row and column position of the central pixel of the window you wish to extract from raster r
#' @return A matrix containg the extracted window around the central pixel from raster r
#' @export

extract_window<- function(r, w=c(3,3), idx){
  rast_row<- idx[1]
  rast_col<-idx[2]
  r_idx<- matrix(data= rep(1:w[1], each= w[2]), nrow = w[1], ncol=w[2], byrow=TRUE)
  r_idx<- r_idx + (rast_row - r_idx[((w[1]+1)/2),])
  r_idx[r_idx<1]<- NA #Matrix or row indices corresponding to raster r

  c_idx<- matrix(data= rep(1:w[2], each= w[1]), nrow = w[1], ncol=w[2], byrow=FALSE)
  c_idx<- c_idx + (rast_col - c_idx[,((w[2]+1)/2)])
  c_idx[c_idx<1]<- NA #Matrix or column indices corresponding to raster r
  dat<- matrix(data=NA, nrow=w[1], ncol=w[2])
  for (i in 1:length(r_idx)) {
    if((!is.na(r_idx[i])) & (!is.na(c_idx[i])) & (r_idx[i]<=nrow(r)) & (c_idx[i]<=ncol(r))){
      dat[i]<- r[r_idx[i], c_idx[i]]}
  }
  return(dat) #extracted window as a matrix
}
