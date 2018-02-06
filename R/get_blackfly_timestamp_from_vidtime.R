#' Returns Blackfly Timestamp based on Video Number and Seconds in
#'
#' Frame number is grabbed from gigegrab and adjusted based on gigegrab number in first frame
#' @param video_col vector of video numbers (must be same length as seconds_col)
#' @param seconds_col vector of seconds in video (must be same length as video_col)
#' @param cam_timestamp vector of camera timestamps (must be same length as cam_filepath)
#' @param cam_filepath vector of filepaths from balckfly camera (must be same length as cam_timestamp)
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import stringr

get_blackfly_timestamp_from_vidtime_1<- function(video_col,seconds_col,cam_timestamp, cam_filepath){
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

#' Returns Blackfly Timestamp based on Video Number and Seconds in
#'
#' Frame number starts at 0 and increases by 1. This is a backup in case camera is restarted mid-transect, and is used as a check
#' @param video_col vector of video numbers (must be same length as seconds_col)
#' @param seconds_col vector of seconds in video (must be same length as video_col)
#' @param cam_timestamp vector of camera timestamps (must be same length as cam_filepath)
#' @param cam_filepath vector of filepaths from blackfly camera (must be same length as cam_timestamp)
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import stringr

get_blackfly_timestamp_from_vidtime_2<- function(video_col,seconds_col,cam_timestamp, cam_filepath){
  video<- bind_cols(as.tibble(video_col),as.tibble(seconds_col))
  names(video)<- c("video_col","seconds_col")
  video<- video %>% dplyr::mutate(frame_num=(video_col*720)+(seconds_col*12)) #calculate approximate frame number
  cam<- bind_cols(as.tibble(cam_timestamp), as.tibble(cam_filepath))
  names(cam)<- c("cam_timestamp", "cam_filepath")
  cam<- cam %>% filter(grepl(pattern = "GigEGrabEx-[\\d]*", x = cam$cam_filepath)) #remove any rows without GigEGrabEx
  cam<- cam %>% mutate(frame_num=0:(nrow(cam)-1)) #Increase frame by 1 with row
  both<- left_join(video, cam, by="frame_num") #Join video and cam by frame number
  output<- both$cam_timestamp #Return vector of timestamps
  return(output)
}

#' Checks if camera has been restarted during transect
#'
#' Does that by checking if gigegrab number from file decreases at any point
#' @param cam_filepath vector of filepaths from balckfly camera (must be same length as cam_timestamp)
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import stringr
check_if_reset<- function(cam_filepath){
  cam<- tibble(cam_filepath=cam_filepath)
  cam<- cam %>% filter(grepl(pattern = "GigEGrabEx-[\\d]*", x = cam$cam_filepath))
  cam<- cam %>% mutate(gigegrab= str_extract(cam$cam_filepath,"GigEGrabEx-[\\d]*"))
  cam<- cam %>% mutate(gigegrab_num=as.numeric(str_extract(cam$gigegrab, "\\d+")))
  cam<- cam %>% mutate(reset=FALSE)
  for ( i in 2:nrow(cam)){
    if(cam$gigegrab_num[i]<=cam$gigegrab_num[i-1]){cam$reset[i]<- TRUE}
  }
  output<- sum(cam$reset)>0 #Outputs whether camera has been reset
}

#' Returns Blackfly Timestamp based on Video Number and Seconds in
#'
#' Returns a vector of Blackfly Timestamps corresponding to video_col and video_sec by using video Number and seconds in frame number and then match that frame number to a table to see when that frame was recorded. If the camera was reset at some point, frame number will start at 0 and increment by one each observation from the camera. Timestamps and filepaths vectors must correspond to eachother.
#' @param video_col vector of video numbers (must be same length as seconds_col)
#' @param seconds_col vector of seconds in video (must be same length as video_col)
#' @param cam_timestamp vector of camera timestamps (must be same length as cam_filepath)
#' @param cam_filepath vector of filepaths from blackfly camera (must be same length as cam_timestamp)
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import stringr
#' @export

get_blackfly_timestamp_from_vidtime<- function(video_col,seconds_col,cam_timestamp, cam_filepath){
was_reset<- check_if_reset(cam_filepath)
if(!was_reset){
  output1<-get_blackfly_timestamp_from_vidtime_1(video_col,seconds_col,cam_timestamp, cam_filepath)
  output2<- get_blackfly_timestamp_from_vidtime_2(video_col,seconds_col,cam_timestamp, cam_filepath)
  if(!identical(output1,output2)){warning("Something may have went wrong")}
  return(output1)
}
if(was_reset){output= get_blackfly_timestamp_from_vidtime_2(video_col,seconds_col,cam_timestamp, cam_filepath)
warning("Camera was reset. Timestamps aquired using alternative method")
return(output)}}
