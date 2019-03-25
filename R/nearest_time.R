#' Match to nearest time
#'
#' Finds nearest time stamp in time 2 for each row of time 1. Optionally can include difference.
#' @param time1 vector of timestamps (class of "POSIXct" "POSIXt")
#' @param time2 vector of timestamps to campare time 1 to (class of "POSIXct" "POSIXt")
#' @param unit time unit ("secs", "mins", "hours", "days", or "weeks"): defualt is "secs"
#' @param output_dif Logical specifying whether or not the time difference for each row should be output (default is FALSE
#' @param break_ties if to times from time2 are equidistant should the "earlier" or "later" time be chosen (default is "earlier")
#' @export

nearest_time<- function(time1, time2, unit="secs", output_dif = FALSE, break_ties="earlier"){
  output<- data.frame(closest_time = rep(NA, length(time1)), time_dif = rep(NA_integer_, length(time1)))
  class(output$closest_time)<- class(time1)
  for (i in 1:length(time1)) {
    time_dif<- difftime(time2, time1[i], units = unit)
    idx<- which(abs(time_dif)==min(abs(time_dif)))
    closest_time_dif<- time_dif[idx]
    closest_time<- time2[idx]
    if(is.na(time1)[i]){
      closest_time<- NA
      closest_time_dif<- NA
      }else if (length(idx) > 1 & break_ties == "later"){
        closest_time<- max(closest_time)
        closest_time_dif<- max(closest_time_dif)
      }else if (length(idx) > 1 & break_ties == "earlier"){
        closest_time<- min(closest_time)
        closest_time_dif<- min(closest_time_dif)}
    output$closest_time[i]<- closest_time
    output$time_dif[i]<- closest_time_dif
  }
  if (output_dif==FALSE){output<- output$closest_time}
  return(output)
}
