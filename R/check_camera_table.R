#' QC for camera table
#'
#' Sequentially checks if camera table has one pattern, if it has been reset, and if it is jumbled (arranging by time and arranging by frame number are different). Returns what the first issue encounted is, which is either "pattern", "reset," or "jumbled," or "none.
#'
#' @param camera_table camera table
#' @param display_warning Logical indicating whether a warning should be displayed
#' @export

check_camera_table<- function(camera_table, display_warning=TRUE){
  one_pattern<- check_pattern(camera_table$file_path, display_warning=display_warning)
  if(!one_pattern){
    return("pattern")} else if(one_pattern){
      was_reset<- check_if_reset(camera_table$file_path, display_warning = display_warning)
      if(was_reset){
        return("reset")}} else if (!was_reset){
          is_jumbled<- check_jumbled(camera_table, display_warning=display_warning)
          if(is_jumbled){return("jumbled")}} else {
            return("none")}}


