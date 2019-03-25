#' Helper function for calculating wood slope
#'
#' DOES NOT GIVE SIMILAR VALUES TO QGIS. NEED TO CHECK!!!!!!!!!!!!
#' Helper function for calculating wood slope
#' @param depth_vector passed from raster::focal
#' @importFrom stats lm

wood_slp<- function(depth_vector){
  if (sum(is.na(depth_vector))>0) {slp<- NA_real_} else{
    window_size<- sqrt(length(depth_vector))
    n_vals<- window_size^2
    max_coord<- (window_size-1)/2
    min_coord<- -max_coord
    x<- rep(NA_integer_, n_vals)
    y<- rep(NA_integer_, n_vals)
    x_curr<- min_coord
    y_curr<- max_coord
    for (i in 1:n_vals) {
      x[i]<- x_curr
      y[i]<- y_curr
      if (x_curr < max_coord) {x_curr<- x_curr+1} else{
        x_curr<- min_coord
        y_curr<- y_curr-1}
    }
    my_model<- lm(formula = depth_vector ~ I(x^2) + I(y^2) + x:y + x + y)
    d<- my_model$coefficients[[4]]
    e<- my_model$coefficients[[5]]
    slp<- degrees(atan(sqrt(d^2+e^2)))
    if (slp<0) {slp<- slp+360}}
  return(slp)
}
