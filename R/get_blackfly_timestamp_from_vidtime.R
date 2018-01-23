#' Returns Blackfly Timestamp based on Video Number and Seconds in
#'
#' Returns a vector of Blackfly Timestamps corresponding to video_col and video_sec by using those two parameters to approximate frame number and then mathc that frame number to a table to see when that frame was recorded.
#' @param video_col vector of video numbers (must be same length as seconds_col)
#' @param seconds_col vector of seconds in video (must be same length as video_col)
#' @param cam_timestamp vector of camera timestamps (must be same length as cam_filepath)
#' @param cam_filepath vector of filepaths from balckfly camera (must be same length as cam_timestamp)
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import stringr
#' @export

get_blackfly_timestamp_from_vidtime<- function(video_col,seconds_col,cam_timestamp, cam_filepath){
  video<- bind_cols(as.tibble(video_col),as.tibble(seconds_col))
  names(video)<- c("video_col","seconds_col")
  video<- video %>% dplyr::mutate(frame_num=(video_col*720)+(seconds_col*12)) #calculate approximate frame number
  cam<- bind_cols(as.tibble(cam_timestamp), as.tibble(cam_filepath))
  names(cam)<- c("cam_timestamp", "cam_filepath")
  cam<- cam %>% filter(grepl(pattern = "GigEGrabEx-[\\d]*", x = cam$cam_filepath)) #remove any rows without GigEGrabEx
  cam<- cam %>% mutate(gigegrab= str_extract(cam$cam_filepath,"GigEGrabEx-[\\d]*"))
  cam<- cam %>% mutate(gigegrab_num=as.numeric(str_extract(cam$gigegrab, "\\d+"))) %>% mutate(frame_num=gigegrab_num-gigegrab_num[1]) #Extract frame number from filename accounting for if gigegrab_num doesn't start at 0
  both<- left_join(video, cam, by="frame_num") #Join video and cam by frame number
  output<- both$cam_timestamp #Return vector of timestamps
  return(output)
}