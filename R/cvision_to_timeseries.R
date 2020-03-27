#' Function to convertmerged_cvision_csv to fish time-series for all species
#'
#' Uses a sliding window of specified size to convert a merged_cvision_csv to fish time-series for all species by summing all observations within that window
#' @param merged_cvision_csv Output from merge_cvison_csv
#' @param window_size size of sliding window in frames
#' @param vid_start number of start video (default=0)
#' @param vid_end number of end video
#' @param vid_length length of video in minutes (default=1)
#' @param fps frames per second of videos
#' @param by_species If TRUE (default) will generate a time-series for each species. If FALSE will create one general time series
#' @import dplyr
#' @export

cvision_to_timeseries<- function(merged_cvision_csv, window_size,vid_start=0,vid_end, vid_length=1, fps, by_species=TRUE){
  Total_Frame<- merged_cvision_csv$Total_Frame
  Frame_min=vid_start*(vid_length*60)*fps
  Frame_max=((vid_end+1)*(vid_length*60)*fps)-1
  Start_Frames= Frame_min:(Frame_max-(window_size-1))
  if(by_species==FALSE){
    output<- fish_to_timeseries(merged_cvision_csv, window_size, vid_start, vid_end, vid_length, fps, Start_Frames)
    output<- cbind(Start_Frames, output)
    output<- as_tibble(output)
    names(output)<- c("Start_Frames", "n")}
  if (by_species==TRUE){
    spec_list<- unique(merged_cvision_csv$Fish_Type)
    output<- Start_Frames
    for (j in 1:length(spec_list)) {
      spec_j<- spec_list[j]
      spec_j_timeseries<- fish_to_timeseries(filter(merged_cvision_csv, Fish_Type==spec_j), window_size,vid_start, vid_end, vid_length, fps, Start_Frames)
      output<- cbind(output, spec_j_timeseries)
    }
    output<- as_tibble(output)
    names(output)<- c("Start_Frames", spec_list)}
  return(output)
}


#' Helper function to convert merged_merged_cvision_csv to fish time-series for a single species
#' convert merged_merged_cvision_csv to fish time-series for a single species

fish_to_timeseries<- function(merged_cvision_csv, window_size, vid_start, vid_end, vid_length, fps, Start_Frames){
  Total_Frame<- merged_cvision_csv$Total_Frame
  output= rep(NA_real_, length(Start_Frames))
  for (i in 1:length(output)){
    s = Start_Frames[i]
    e = s + window_size -1
    n<- sum(Total_Frame>=s & Total_Frame<=e)
    output[i]<-n
  }
  return(output)}
