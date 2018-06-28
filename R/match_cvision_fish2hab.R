#' Matches fish recorded in C-Vision to closest habitat frame
#'
#' Matches fish recorded in C-Vision to closest habitat frame. Returns a tibble of species by site counts.
#' @param hab_totalframe vector of total frame number of habitat observation (Alternatively could use video_num, video-sec, video_frame, fps, and vid_length)
#' @param merged_cvision_csv output of merge_cvision_csv
#' @param include_framenum Logical indicating whether or not to include frame number in output table
#' @param video_num vector video_numbers from habitat dataframe (Not needed if hab_totalframe suppled)
#' @param video_sec vector of seconds in video from habitat dataframe (Not needed if hab_totalframe suppled)
#' @param fps frames per second (Not needed if hab_totalframe suppled)
#' @param vid_length video length in minutes (Not needed if hab_totalframe suppled)
#' @param include_vid_sec Logical indicating whether or not to include Video Number and Seconds of habitat frames in output table
#' @import dplyr
#' @import magrittr
#' @export
match_cvision_fish2hab2<-function(hab_totalframe=NULL, merged_cvision_csv, include_framenum=FALSE, video_num, video_sec, fps, vid_length=1, include_vid_sec=FALSE){
  if(is.null(hab_totalframe)){
    hab_totalframe<- get_total_frame(video_num = video_num, video_sec = video_sec, fps = fps, vid_length = vid_length)}
  if(min(hab_totalframe)>=min(merged_cvision_csv$Total_Frame)|max(hab_totalframe)<=max(merged_cvision_csv$Total_Frame)){
    message("Error:Fish beyond bounds of Habitat Data")
    stop()}
  fish_names<- sort_species(unique(merged_cvision_csv$Fish_Type))
  new_df<- matrix(nrow = length(hab_totalframe), ncol=length(fish_names)+1) %>% as_tibble() #Species by Site Matrix
  names(new_df)<- c("Total_Frame", fish_names)
  new_df<- sapply(X = new_df, as.integer) %>% as_tibble
  new_df$Total_Frame<- hab_totalframe
  merged_cvision_csv$closest_hab_frame<- NA_integer_
  for (i in 1:nrow(merged_cvision_csv)) {
    diffs<- abs(merged_cvision_csv$Total_Frame[i]-hab_totalframe)
    idx<- which(diffs==min(diffs))[1]
    merged_cvision_csv$closest_hab_frame[i]<-hab_totalframe[idx]
  }
  grouped_counts<- merged_cvision_csv %>% group_by(Fish_Type, closest_hab_frame) %>% count() %>% ungroup() #Group counts by Fish Type and nearest habitat frame
  for (i in 1:nrow(grouped_counts)) { #Fill in the species by site matrix
    row_num<- which(new_df$Total_Frame==grouped_counts$closest_hab_frame[i])
    col_name<- grouped_counts$Fish_Type[i]
    fish_count<- grouped_counts$n[i]
    new_df[row_num, col_name]<- fish_count
  }
  new_df[is.na(new_df)]<- 0
  if (include_vid_sec){
    new_df<- new_df %>% left_join(bind_cols(Total_Frame=hab_totalframe, Video=video_num, Seconds= video_sec), by="Total_Frame")
    new_df<- new_df %>% select(Video, Seconds, everything())}
  if (include_framenum==FALSE){new_df<- new_df %>% select(-c(Total_Frame))}
  return(new_df)
}
