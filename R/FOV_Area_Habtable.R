#' Calculates area per habitat frame
#'
#' Calculates area per habitat frame using the observed habitat as the midpoint and average altitude and pitch over that time period. (First and last row will be NA)
#' @param alt vector of altitudes from Master 1Hz
#' @param pitch vector of pitches from Master 1Hz
#' @param speed vector of CBASS speeds in knots from Master 1Hz
#' @param all_timestamps vector of timestamps from Master 1Hz that correspong to altitude and pitches
#' @param hab_timestamps vector of timestamps of habitat frames
#' @param cam_angle camera angle in degrees
#' @param HFOV_air HFOV in air for given camera (default is that of blackfly camera)
#' @export
FOV_Area_Habtable<- function(hab_timestamps, all_timestamps, alt, pitch, speed, cam_angle=32.8, HFOV_air= 82.4){
  area<- rep(NA_real_, length(hab_timestamps))
  for (i in 2:(length(hab_timestamps)-1)) {
    mid_time<- hab_timestamps[i]
    low_time<- mid_time-floor((hab_timestamps[i]-hab_timestamps[i-1])/2)
    high_time<- mid_time+floor((hab_timestamps[i+1]-hab_timestamps[i])/2)
    idx<- which(all_timestamps==low_time):which(all_timestamps==high_time)
    alt_avg<- mean(alt[idx])
    pitch_avg<- mean(pitch[idx])
    speed_avg<- mean(na.omit(speed[idx]))
    width<- calc_width(alt = alt_avg , pitch = pitch_avg)
    duration<- ((hab_timestamps[i]-hab_timestamps[i-1])/2)+((hab_timestamps[i+1]-hab_timestamps[i])/2)
    distance<- calc_dist(speed_avg, speed_units="knots", duration=as.numeric(duration))
    area[i]<- width*distance
  }
  return(area)
}
