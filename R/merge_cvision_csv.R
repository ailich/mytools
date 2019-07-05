#' Merge C-Vision CSV's
#'
#' Merges all C-Vision CSV's into 1 CSV using a list of files or will merge all csv's in current working directory. NOTE: Use total frame for blackfly/AVT and Total Time for axis when trying to match to timestamp or position
#' @param file_list list of cvision files to be merged
#' @param frames_per_sec frames per second
#' @param vid_length Length of video in minutes
#' @export
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import readr

merge_cvision_csv<- function(file_list=list.files(pattern = "\\.csv$"), frames_per_sec, vid_length){
  fish<- tibble("Trip_ID" =NA_integer_, "Tow_Number"= NA_integer_, "Reviewer"=NA_character_, "Tow_Type"=NA_character_, "Fish_Number"=NA_integer_, "Fish_Type"=NA_character_, "Species"=NA_character_, "Frame"=NA_integer_, "Time_In_Video"=NA_real_, file_name=NA_character_) #Initialize accumulator
  for (i in 1:length(file_list)){
    new_fish<- suppressWarnings(suppressMessages(read_csv(file = file_list[i],col_types=cols(.default="c", Trip_ID="i", Tow_Number="i", Fish_Number="i", Frame="i", Time_In_Video="d"))))
    new_fish<- new_fish[,1:9]
    if(!identical(names(new_fish), c("Trip_ID", "Tow_Number", "Reviewer", "Tow_Type", "Fish_Number", "Fish_Type", "Species", "Frame", "Time_In_Video"))){
      message(paste("Columns of file",  file_list[i], "not as expected"))
      stop()} #Throw error if col names not as expected
    new_fish<- new_fish %>% mutate(file_name= file_list[i])
    fish<- bind_rows(fish,new_fish)
    rm(i,new_fish)
  }
  fish<- fish %>% select(file_name, Reviewer, Fish_Number, Fish_Type, Time_In_Video, Frame)
  fish<- fish %>% mutate (short_filename =basename(file_name))
  fish<- fish %>% mutate (video =NA_real_) #Initialize video column

  for (j in 1:nrow(fish)) {
    curr_vid_num<- fish$short_filename[j] %>%
      str_remove(pattern = "ax\\d-") %>% #Remove axis number
      str_extract_all(pattern = "\\d+", simplify = TRUE)
    if(length(curr_vid_num)==1){
      fish$video[j]<- as.numeric(curr_vid_num)} else{
        message(paste("Error: Could not extract video number from file name", fish$short_filename[j]))
        stop()}
    rm(j, curr_vid_num)
  }
  fish<- fish %>% mutate(Total_Frame= video*(frames_per_sec*(vid_length*60))+Frame)
  fish<- fish %>% mutate(Total_Time_In= (video*(vid_length*60))+Time_In_Video)
  fish<- fish %>% select(file_name,video,Reviewer,Fish_Number,Fish_Type,Time_In_Video, Frame, Total_Time_In, Total_Frame)
  fish<- fish[-1,] #Remove first row of NA's
  dup_check<- fish %>% group_by(video, file_name) %>% count()
  if(sum(duplicated(dup_check$video))>0){
    message("Error: one video has multiple associated csvs")
    stop()}
  return(fish)
}

