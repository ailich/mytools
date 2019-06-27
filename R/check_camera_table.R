#' QC for camera table
#'
#' Sequentially checks if camera table has one pattern, if it has been reset, and if it is jumbled (arranging by time and arranging by frame number are different). Returns what the first issue encounted is, which is either "pattern", "reset," or "jumbled," or "none.
#'
#' @param camera_table camera table
#' @param display_warning Logical indicating whether a warning should be displayed
#' @export

check_camera_table<- function(camera_table, display_warning=TRUE){
  pattern_issue<- !check_pattern(camera_table$file_path, display_warning=display_warning)
  if((!is.logical(pattern_issue)) | (is.na(pattern_issue))){
    message("Error checking camera table")}
  if(pattern_issue){return("pattern")}

  reset_issue<- check_if_reset(camera_table$file_path, display_warning = display_warning)
  if((!is.logical(reset_issue)) | (is.na(reset_issue))){
    message("Error checking camera table")}
  if(reset_issue){return("reset")}

  jumbled_issue<- check_jumbled(camera_table, display_warning=display_warning)
  if((!is.logical(jumbled_issue)) | (is.na(jumbled_issue))){
    message("Error checking camera table")}
  if(jumbled_issue){return("jumbled")}

  if(pattern_issue==FALSE & reset_issue==FALSE & jumbled_issue==FALSE){
    return("none")} else{message("Error checking camera table")}
  }


