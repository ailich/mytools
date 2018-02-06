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

merge_cvision_csv<- function(file_list=NULL, frames_per_sec, vid_length){
  if(is.null(file_list)){file_list<- list.files(pattern = "\\.csv$")}
  new_file_list<- rep(x = NA_character_,length(file_list)) #Initialize new file list
  for (i in 1:length(file_list)) {
    fish_file<- read_csv(file_list[i])
    if (nrow(fish_file)>0) {new_file_list[i]<- file_list[i]}
    rm(i,fish_file)
  }
  new_file_list<- new_file_list[!is.na(new_file_list)] #Create a new file list of only ones that contain fish

  fish<- read_csv(file = new_file_list[1])
  fish<- fish %>% mutate(file_name= new_file_list[1])
  for (i in 2:length(new_file_list)) {
    new_fish<- read_csv(new_file_list[i])
    new_fish<- new_fish[,1:9]
    new_fish<- new_fish %>% mutate(file_name= new_file_list[i])
    fish<- bind_rows(fish,new_fish)
    rm(i,new_fish)
  }

  fish<- fish %>% select(file_name, Reviewer, Fish_Number, Fish_Type, Time_In_Video, Frame)
  fish<- fish %>% mutate(video= as.numeric(str_extract(string = file_name,pattern = "\\d+")))
  fish<- fish %>% mutate(Total_Frame= video*(frames_per_sec*(vid_length*60))+Frame)
  fish<- fish %>% mutate(Total_Time= (video*(vid_length*60))+Time_In_Video)
  fish<- fish %>% select(file_name,video,Reviewer,Fish_Number,Fish_Type,Time_In_Video, Frame, Total_Time, Total_Frame)
  return(fish)
  }

