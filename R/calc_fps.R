#' Calculates frame-rate of Blackfly Camera
#'
#' Calculates frame-rate of Blackfly Camera
#'
#' @param cam_timestamp vector of camera timestamps (must be same length as cam_filepath)
#' @param cam_filepath vector of filepaths from balckfly camera (must be same length as cam_timestamp)
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import stringr
#' @export

calc_fps<- function(cam_timestamp, cam_filepath){
  if (check_if_reset(cam_filepath)) {
    message("Error: camera was reset")
    stop()}
  my_pattern<- detect_cam_pattern(cam_filepath=cam_filepath) #Detect pattern in cam_filepath
  cam<- bind_cols(as_tibble(cam_timestamp), as_tibble(cam_filepath))
  names(cam)<- c("cam_timestamp", "cam_filepath")
  start_pt<- cam[9,] #Start Point
  end_pt<- tail(cam)[1,] #End Point (Edges trimmed similar to #2's code. Seems to result in fps very close (2e-5) to rate use by Alex #2)
  cam<- rbind(start_pt,end_pt)
  cam<- cam %>% mutate(gigegrab= str_extract(cam$cam_filepath, my_pattern))
  cam<- cam %>% mutate(gigegrab_num=as.numeric(str_extract(cam$gigegrab, "\\d+")))
  n_frames<- (cam$gigegrab_num[2]-cam$gigegrab_num[1])+1
  n_secs<-  as.numeric(difftime(cam$cam_timestamp[2], cam$cam_timestamp[1], units = "secs"))
  fps<- n_frames/n_secs
  return(fps)
}
