#' Merge C-Vision CSV's
#'
#' Merges all C-Vision CSV's into 1 CSV using a list of files or will merge all csv's in current working directory. NOTE: Use total frame for blackfly/AVT and Total Time for axis when trying to match to timestamp or position
#' @param file_list list of cvision files to be merged
#' @param frame_per_sec frames per second
#' @param Video Length in Minutes
#' @export
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import readr

merge_cvision_csv<- function(file_list=list.files(pattern = "\\.csv$"), frames_per_sec, vid_length){
  fish<- suppressWarnings(suppressMessages(read_csv(file = file_list[1],col_types=cols(.default="c", Trip_ID="i", Tow_Number="i", Fish_Number="i", Frame="i", Time_In_Video="d"))))
  fish<- fish[,1:9]
  fish<- fish %>% mutate(file_name= file_list[1])
  for (i in 1:length(file_list)){
    new_fish<- suppressWarnings(suppressMessages(read_csv(file = file_list[i],col_types=cols(.default="c", Trip_ID="i", Tow_Number="i", Fish_Number="i", Frame="i", Time_In_Video="d"))))
    new_fish<- new_fish[,1:9]
    new_fish<- new_fish %>% mutate(file_name= file_list[i])
    fish<- bind_rows(fish,new_fish)
    rm(i,new_fish)
  }
  fish<- fish %>% select(file_name, Reviewer, Fish_Number, Fish_Type, Time_In_Video, Frame)
  fish$short_filename<- NA_character_
  for (i in 1:nrow(fish)) {
    idx<- gregexpr("\\/", fish$file_name[i])[[1]]
    start_pos<- idx[length(idx)]+1
    end_pos<- nchar(fish$file_name[i])
    fish$short_filename[i]<-substr(x = fish$file_name[i], start = start_pos, stop=end_pos)
  }
  fish<- fish %>% mutate(video= as.numeric(str_extract(string = short_filename,pattern = "\\d+")))
  fish<- fish %>% mutate(Total_Frame= video*(frames_per_sec*(vid_length*60))+Frame)
  fish<- fish %>% mutate(Total_Time_In= (video*(vid_length*60))+Time_In_Video)
  fish<- fish %>% select(file_name,video,Reviewer,Fish_Number,Fish_Type,Time_In_Video, Frame, Total_Time_In, Total_Frame)
  return(fish)
}

