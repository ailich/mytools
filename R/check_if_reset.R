#' Checks if blackfly has been restarted during transect
#'
#' Does that by checking if gigegrab number from file decreases at any point
#' @param cam_filepath vector of filepaths from blackfly camera
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @export
check_if_reset<- function(cam_filepath){
  cam<- tibble(cam_filepath=cam_filepath)
  cam<- cam %>% filter(grepl(pattern = "GigEGrabEx-[\\d]*", x = cam$cam_filepath))
  cam<- cam %>% mutate(gigegrab= str_extract(cam$cam_filepath,"GigEGrabEx-[\\d]*"))
  cam<- cam %>% mutate(gigegrab_num=as.numeric(str_extract(cam$gigegrab, "\\d+")))
  cam<- cam %>% mutate(reset=FALSE)
  for ( i in 2:nrow(cam)){
    if(cam$gigegrab_num[i]<=cam$gigegrab_num[i-1]){cam$reset[i]<- TRUE}
  }
  output<- sum(cam$reset)>0 #Outputs whether camera has been reset
  if (output){
    message(paste("Camera was reset after", as.character(cam$gigegrab[which(cam$reset==TRUE)-1])))
  }
  return(output)
}
