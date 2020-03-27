#' Checks if camera table has more than one pattern
#'
#' Checks if camera table has more than one pattern (e.g. camera or image file extension type). TRUE indicates one pattern found which is what it should be.
#' @param cam_filepath vector of filepaths from blackfly camera
#' @param display_warning Logical indicating whether a warning should be displayed if issue detected
#' @import dplyr
#' @importFrom stringr str_replace_all
#' @export

check_pattern<- function(cam_filepath, display_warning=TRUE){
  patterns<- unique(str_replace_all(string = basename(cam_filepath), pattern = "\\d+", replacement = "\\\\d+"))
  patterns
  my_pattern<- patterns[patterns != "\\d+-\\d+" & (!grepl(pattern = "*\\.txt", x = patterns))]
  my_pattern
  if(length(my_pattern)==1){
    output<- TRUE} else{
      output<- FALSE}
  if((length(my_pattern) < 1) & display_warning){warning("Warning: no patterns avialable")}
  if((length(my_pattern) > 1) & display_warning){warning("Warning: Multiple patterns avialable")}
  return(output)
  }
