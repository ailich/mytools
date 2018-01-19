#' Retrieves Blackfly Timestamp
#'
#' Retrieves Blackfly Timestamp from approximate video time
#' @param hab habitat spreadsheet
#' @param cam table of blackfly camera data
#' @param video_col column of video number in hab
#' @param seconds_col column of seconds in hab
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import stringr
#' @export

get_blackfly_timestamp_from_vidtime<- function(hab,cam,video_col,seconds_col){
  names(hab)[c(video_col,seconds_col)]=c("video_num", "video_sec") #rename video and seconds columns
  video<- cbind(hab[,video_col],hab[,seconds_col])
  names(video)<- c("video_num", "video_sec")
  video<- video %>% dplyr::mutate(frame_num=(video_num*720)+(video_sec*12)) #calculate approximate frame number
  idx<- grepl(pattern = "GigEGrabEx-[\\d]*", x = cam$file_path)
  cam<- cam[idx,] #remove any rows without GigEGrabEx
  cam<- cam %>% mutate(gigegrab= str_extract(file_path,"GigEGrabEx-[\\d]*")) %>% mutate(gigegrab_num=as.numeric(str_extract(gigegrab, "\\d+"))) %>% mutate(frame_num=gigegrab_num-gigegrab_num[1]) %>% select(-c(gigegrab, gigegrab_num)) #Extract frame number from filename accounting for if gigegrab_num doesn't start at 0
  both<- left_join(video, cam, by="frame_num") #Join video and cam by frame number
  output<- left_join(select(both,c(video_num, video_sec, timestamp)),hab,by=c("video_num","video_sec")) %>% filter(!is.na(timestamp)) %>% as.tibble() #Merge with hab table and remove rows where no timestamp
  return(output)
}
