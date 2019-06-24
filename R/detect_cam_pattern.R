#' Function for detecting string type in camera table
#'
#' Function for detecting string type in camera table
#' @param cam_filepath vector of filepaths from blackfly camera
#' @export

detect_cam_pattern<- function(cam_filepath){
  patterns<- unique(str_replace_all(string = basename(cam_filepath), pattern = "\\d+", replacement = "\\\\d+"))
  patterns
  my_pattern<- patterns[patterns != "\\d+-\\d+" & (!grepl(pattern = "*\\.txt", x = patterns))]
  my_pattern
  if(length(my_pattern)>1){
    message("Error: multiple patterns available")
    stop()}
  if(length(my_pattern)<1){
    message("Error: no patterns available")
    stop()}
  return(my_pattern)}
