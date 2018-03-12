#' Calculates width of video frame
#'
#' Calculates width of video frame in meters
#' @param alt altitude
#' @param pitch pitch
#' @param cam_angle camera angle in degrees
#' @param HFOV_air HFOV in air for given camera
#' @export
calc_width<- function(alt, pitch, cam_angle=32.8, HFOV_air= 82.4){
  idx_ref_air<- 1.000277
  idx_ref_seawater<- 4/3
  HFOV_water<- asin(sin((HFOV_air/2)*pi/180)*(idx_ref_air/idx_ref_seawater))*(180/pi)*2
  alt_adj<- cos(pitch*(pi/180))*alt
  cam_angle_to_ground<- cam_angle-pitch
  center_dist<- alt_adj/sin(cam_angle_to_ground*(pi/180))
  width<- 2*center_dist*tan((HFOV_water/2)*(pi/180))
  return(width)
}
