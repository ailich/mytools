#' Internal Function for detecting string type in camera table
#'
#' Internal Function for detecting string type in camera table
#' @param cam_filepath vector of filepaths from blackfly camera

detect_cam_pattern<- function(cam_filepath){
if(sum(grepl(pattern = "GigEGrabEx-[\\d]*", x = cam_filepath))>0){
  my_pattern<- "GigEGrabEx-[\\d]*"} else if(sum(grepl(pattern = "Blackfly-[\\d]*", x = cam_filepath))>0){
  my_pattern<- "Blackfly-[\\d]*"} else{
    message("Error: filepath pattern not recognized")
    stop()}
return(my_pattern)}
