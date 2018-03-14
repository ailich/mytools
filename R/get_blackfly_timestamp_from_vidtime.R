#' Returns Blackfly Timestamp based on Video Number and Seconds in
#'
#' Returns a vector of Blackfly Timestamps corresponding to video_col and video_sec by using those two parameters to approximate frame number (or can use Total Frame Number directly) and then matches that frame number to a table to see when that frame was recorded.
#' @param video_col vector of video numbers (must be same length as seconds_col)
#' @param seconds_col vector of seconds in video (must be same length as video_col)
#' @param Total_Frame Total Frame Number can be used instead of video_col and seconds_col
#' @param cam_timestamp vector of camera timestamps (must be same length as cam_filepath)
#' @param cam_filepath vector of filepaths from balckfly camera (must be same length as cam_timestamp)
#' @param test if TRUE output will be a tibble that can be used to check if the function is working correctly
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import stringr
#' @export

get_blackfly_timestamp_from_vidtime<- function(video_col,seconds_col,Total_Frame=NULL, cam_timestamp, cam_filepath, test=FALSE){
  if (check_if_reset(cam_filepath)) {
    message("Error: camera was reset")
    stop()}
  if(is.null(Total_Frame)){
    video<- bind_cols(as_tibble(video_col),as_tibble(seconds_col))
    names(video)<- c("video_col","seconds_col")
    video<- video %>% dplyr::mutate(frame_num=(video_col*720)+(seconds_col*12)) #calculate approximate frame number
  } else{video<- tibble(frame_num=Total_Frame)}
  cam<- bind_cols(as_tibble(cam_timestamp), as_tibble(cam_filepath))
  names(cam)<- c("cam_timestamp", "cam_filepath")
  cam<- cam %>% filter(grepl(pattern = "GigEGrabEx-[\\d]*", x = cam$cam_filepath)) #remove any rows without GigEGrabEx
  cam<- cam %>% mutate(gigegrab= str_extract(cam$cam_filepath,"GigEGrabEx-[\\d]*"))
  cam<- cam %>% mutate(gigegrab_num=as.numeric(str_extract(cam$gigegrab, "\\d+"))) %>% mutate(frame_num=gigegrab_num-gigegrab_num[1]) #Extract frame number from filename accounting for if gigegrab_num doesn't start at 0
  cam<- cam %>% mutate(frame_num_check=0:(nrow(cam)-1)) #Frame numbers just incremented by 1
  if(sum(cam$frame_num!=cam$frame_num_check)!=0){warning("Something may have went wrong")} #Check both methods of getting frame match
  both<- left_join(video, cam, by="frame_num") #Join video and cam by frame number
  if(test){output=both #Return tibble
    } else{output<- both$cam_timestamp} #Return vector of timestamps
  return(output)
}
