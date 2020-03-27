#' Gets duration from video info
#'
#' Parses duration of video in seconds from output of video_info function
#' @param vid_info output from video_info function
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract_all
#' @import dplyr
#' @export

parse_video_time<-function(vid_info){
  dur_check<- sum(grepl(pattern = "Duration", vid_info),na.rm=TRUE) > 0
  if(dur_check == FALSE){
    message("Error: No Duration Field Present in Video")
    stop()}
  vid_dur<- vid_info[grepl(pattern = "Duration", vid_info)][[1]]
  units_list=c("min", "s", "ms")
  parse_check<- vid_dur %>%
    str_remove("Duration") %>%
    str_remove_all("\\d") %>%
    str_remove(":") %>%
    str_remove(" min") %>%
    str_remove(" ms") %>%
    str_remove(" s") %>%
    str_remove_all(" ") #Should result in empty string
  if(parse_check!=""){
    message("Error: Could not parse video time")
    stop()}
  units_idx<- c(grepl(x = vid_dur, pattern = "\\d min"), grepl(x = vid_dur, pattern = "\\d s"), grepl(x = vid_dur, pattern = "\\d ms"))
  units<- units_list[units_idx]
  vals<- as.numeric(str_extract_all(string = vid_dur, pattern = "\\d+")[[1]]) #Values in corresponding units
  vals_secs<- rep(NA_real_, length(vals)) #Values in seconds
  for (i in 1:length(units)){
    if(units[i]=="min"){vals_secs[i]<-60*vals[i]}
    if(units[i]=="s"){vals_secs[i]<-vals[i]}
    if(units[i]=="ms"){vals_secs[i]<-vals[i]/1000} #COnvert vals to seconds
  }
  vals_secs<- sum(vals_secs) #Sum vals now that all in same units
  return(vals_secs)
}

