#' Gets frames per second from video info
#'
#' Parses fps of video from output of video_info function
#' @param vid_info output from video_info function
#' @import magrittr
#' @export

parse_video_fps<-function(vid_info){
  fps_check<- sum(grepl(pattern = "Frame rate", vid_info),na.rm=TRUE) > 0
  if(fps_check == FALSE){
    message("Error: No Frame rate Field Present in Video")
    stop()}
  vid_fps<- vid_info[grepl(pattern = "Frame rate", vid_info)]
  vid_fps
  parse_check<- sum(grepl(pattern = "FPS", vid_info),na.rm=TRUE) > 0
  parse_check
  if(parse_check==FALSE){
    message("Error: Could not parse frame rate")
    stop()}
  vid_fps<- vid_fps %>%
    str_extract("\\d+\\.\\d+") %>%
    as.numeric()
  return(vid_fps)}
