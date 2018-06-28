#' Checks if blackfly has been restarted during transect
#'
#' Checks if blackfly has been restarted during transect seeing if gigegrab number goes to 0 anywhere that is not the start
#' @param cam_filepath vector of filepaths from blackfly camera
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @export
check_if_reset<- function(cam_filepath){
  my_pattern<- detect_cam_pattern(cam_filepath=cam_filepath)
  cam<- tibble(cam_filepath=cam_filepath)
  cam<- cam %>% filter(grepl(pattern = my_pattern, x = cam$cam_filepath))
  cam<- cam %>% mutate(gigegrab= str_extract(cam$cam_filepath,my_pattern))
  cam<- cam %>% mutate(gigegrab_num=as.numeric(str_extract(cam$gigegrab, "\\d+")))
  idx<- which(cam$gigegrab_num==0)
  idx<- idx[idx!=1]
  output<- length(idx)!=0 #Outputs whether camera has been reset
  if (output){
    for (i in 1:length(idx)) {
      message(paste("Camera was reset after", as.character(cam$gigegrab[idx[i]-1])))}
  }
  return(output)
}
