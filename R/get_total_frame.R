#' Calculates Total Frame Number
#'
#' Calculates Total Frame Number by video number and seconds in
#' @param video_num vector video_numbers
#' @param video_sec vector of seconds in video
#' @param fps frames per second
#' @param vid_length video length in minutes
#' @export
get_total_frame<- function(video_num, video_sec, fps=12, vid_length=1){
  fpv<- fps*(vid_length*60) #frames per video
  Total_Frame<- (video_num*fpv)+(video_sec*fps)
}
