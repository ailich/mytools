#' Gets video details
#'
#' Uses program Media Info to extract video details
#' @param vid_path file path of video
#' @param Media_Info_Path Path to Media Info program
#' @import stringr
#' @export

video_info<- function(vid_path, Media_Info_Path = "C:/Users/socce/Documents/Grad School/Software/Media_Info/MediaInfo.exe"){
  vid_path<-  str_replace_all(string = vid_path, pattern = "/{2,}", replacement = "/") #Remove multiple "/" that are in a row for command line interface in system call
  vid_info <- system(paste(shQuote(Media_Info_Path), shQuote(vid_path)), intern = TRUE)
  return(vid_info)}
