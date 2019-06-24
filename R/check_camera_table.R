#' QC for camera table
#'
#' Sequentially checks if camera table has one pattern, if it has been reset, and if it is jumbled (arranging by time and arranging by frame number are different). Returns what the first issue encounted is, which is either "pattern", "reset," or "jumbled," or "none.
#'
#' @param camera_table camera table
#' @param display_warning Logical indicating whether a warning should be displayed
#' @export

check_camera_table<- function(camera_table, display_warning=TRUE){
  pattern_issue<- !check_pattern(camera_table$file_path, display_warning=display_warning)
  reset_issue<- check_if_reset(camera_table$file_path, display_warning = display_warning)
  jumbled_issue<- check_jumbled(camera_table, display_warning=display_warning)
  idx<- c(pattern_issue, reset_issue, jumbled_issue)
  issues<- c("pattern", "reset", "jumbled")
  table_issue<- issues[idx][1]#Choose first issue
  if(is.na(table_issue)){table_issue<- "none"}
  return(table_issue)}


