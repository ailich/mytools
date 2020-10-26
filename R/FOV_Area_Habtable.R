#' Calculates area per habitat frame
#'
#' Calculates area per habitat frame using the observed habitat as the midpoint and average altitude and pitch over that time period. (First and last row will be NA)
#' @param alt vector of altitudes from Master 1Hz
#' @param pitch vector of pitches from Master 1Hz
#' @param speed vector of CBASS speeds in knots from Master 1Hz
#' @param all_timestamps vector of timestamps from Master 1Hz that corresponding to alt, pitch, and speed
#' @param hab_timestamps vector of timestamps of habitat frames
#' @param cam_angle camera angle in degrees
#' @param H_AFOV_air Horizontal angular field of view in air in degrees for the camera system (depends on lens and image sensor size)
#' @param bin_size bin size in seconds (if not specified will be calculated)
#' @param use_median Use median instead of mean for alt, pitch, and speed respectively
#' @export
AFOV_Area_Habtable<- function(hab_timestamps, all_timestamps, alt, pitch, speed, cam_angle=32.8, H_AFOV_air, bin_size=NULL, use_median=c(FALSE, FALSE, FALSE)){
  area<- rep(NA_real_, length(hab_timestamps)) #Dimension Variable
  for (i in 2:(length(hab_timestamps)-1)) {
    mid_time<- hab_timestamps[i]
    if(is.null(bin_size)) {
      low_time<- mid_time-((hab_timestamps[i]-hab_timestamps[i-1])/2)
      high_time<- mid_time+((hab_timestamps[i+1]-hab_timestamps[i])/2)
    } else {
      low_time= mid_time - bin_size/2
      high_time= mid_time + bin_size/2}
    idx<- (all_timestamps >= low_time) & (all_timestamps <= high_time)
    if(use_median[1]){alt_avg<- median(alt[idx], na.rm = TRUE)} else{
      alt_avg<- mean(alt[idx], na.rm = TRUE)}
    if(use_median[2]){pitch_avg<- median(pitch[idx], na.rm = TRUE)} else{
      pitch_avg<- mean(pitch[idx], na.rm = TRUE)}
    if(use_median[3]){speed_avg<- median(speed[idx], na.rm = TRUE)}else{
      speed_avg<- mean(speed[idx], na.rm = TRUE)}
    width<- calc_width(alt = alt_avg , pitch = pitch_avg)
    distance<- calc_dist(speed_avg, speed_units="knots", duration=as.numeric(high_time-low_time))
    area[i]<- width*distance
  }
  return(area)
}
