#' Check if camera table is jumbled
#'
#' Check if camera table is jumbled (arranging by time and arranging by frame number are different). If true that may mean that the clock was resynched.
#'
#' @param camera_table camera table
#' @param display_warning Logical indicating whether a warning should be displayed
#' @import dplyr
#' @import magrittr
#' @export

check_jumbled<- function(camera_table, display_warning=TRUE){
  my_pattern<- detect_cam_pattern(camera_table$file_path)
  camera_table<- camera_table %>% filter(grepl(pattern = my_pattern, x = file_path))
  camera_table<- camera_table %>% mutate(base_name = basename(file_path))
  camera_table2<- camera_table %>% arrange(timestamp, u_second) #ordered by time
  camera_table3<- camera_table %>% arrange(file_path) #ordered by frame
  is_jumbled<- !(identical(camera_table2,camera_table3) & identical(camera_table,camera_table2))
  if (is_jumbled & display_warning){warning("Warning: Camera table is jumbled")}
  return(is_jumbled)}
