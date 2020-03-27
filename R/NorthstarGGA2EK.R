#' Reformat postion from Northstar-941X--GGA.lab files to look like those from an EK cruise track csv
#' @param file_name path to Northstar-941X--GGA.lab file
#' @import dplyr
#' @importFrom readr read_delim
#' @importFrom lubridate ymd
#' @importFrom lubridate ddays
#' @importFrom lubridate dseconds

NorthstarGGA2EK<-function(file_name){
  if(basename(file_name)!="Northstar-941X---GGA.lab"){
    message("filename not supported by NorthstarGGA2EK function")
    stop()}
  col_types<- paste0(strrep("d", 6), "c", strrep("d", 6), "c")
  GGA<- readr::read_delim(file_name, delim = " ", col_names = FALSE, col_types = col_types)
  names(GGA)[c(1,3, 4, 9, 10)] <- c("Year", "Day", "Decimal_Day", "Latitude", "Longitude")
  GGA<- GGA[,!grepl(pattern = "X\\d", names(GGA))]
  GGA<- GGA %>% mutate(time_origin= lubridate::ymd(paste(as.character(Year), "-1-1")))
  GGA<- GGA %>% mutate(GPS_date=time_origin+lubridate::ddays(Day-1))
  GGA<- GGA %>% mutate(seconds_in_day=Decimal_Day*24*60*60)
  GGA<- GGA %>% mutate(GPS_milliseconds=(seconds_in_day-floor(seconds_in_day))*1000)
  GGA<- GGA %>% mutate(datetime=GPS_date+lubridate::dseconds(floor(seconds_in_day)))
  GGA<- GGA %>% mutate(GPS_time= strftime(datetime,format="%H:%M:%S"))
  output<- GGA %>% select(GPS_date, GPS_time, GPS_milliseconds, Latitude, Longitude)
  output<- output %>% mutate(GPS_fix= 0:(nrow(output)-1), GPS_status=0, GPS_filename=file_name, Distance=NaN)
  output<- output %>% select(GPS_fix, GPS_date, GPS_time, GPS_milliseconds, Latitude, Longitude, GPS_status, GPS_filename, Distance)
  return(output)
}
