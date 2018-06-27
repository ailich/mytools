#' Calculates Total Frame Number
#'
#' Calculates Total Frame Number by video number and seconds or frame number in
#' @param video_num vector video_numbers
#' @param video_sec vector of seconds in video (alternativelly could use video_frame)
#' @param video_frame vector of frames into video (alternativelly could use video_sec)
#' @param fps frames per second
#' @param vid_length video length in minutes
#' @export
get_total_frame<- function(video_num, video_sec, video_frame=NULL, fps=12, vid_length=1){
  fpv<- fps*(vid_length*60) #frames per video
  if(is.null(video_frame)){Total_Frame<- (video_num*fpv)+(video_sec*fps)
  } else{Total_Frame<- (video_num*fpv)+video_frame}
  return(Total_Frame)
}
