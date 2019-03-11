#' Calculates layback of CBASS
#'
#' Calculates layback of CBASS in meters
#'
#' @param payout line out from boat (meters)
#' @param depth depth of CBASS (meters)
#' @param zeroed "water" or "block"
#' @param cat_fact catenary factor (from 0-1)
#' @param GPS_Source Adds offset forward/aft offset between trawl block and GPS source. Must be "None", "GNSS_Fwd" or "EK"
#' @export

calc_layback<- function(payout, depth, GPS_Source= "None", zeroed, cat_fact=1){
  if (GPS_Source != "GNSS_Fwd" & GPS_Source != "EK" & GPS_Source != "None"){stop('GPS_Source must be "None", "GNSS_Fwd" or "EK"' )}
  if (zeroed != "water" & zeroed != "block"){stop('zeroed must be "water" or "block"' )}
  if (cat_fact > 1 | cat_fact <=0){stop("catenary factor must be between 0-1" )}
  Block_height<- 6.065 #Height of trawl block above water
  if (zeroed == "water"){payout<- payout + Block_height} #Approximately this much line would be let out to reach water
  if (GPS_Source == "None") {y_offset<- 0} #Forward/Aft offset between EK and trawl block
  if (GPS_Source == "EK") {y_offset<- 21.296} #Forward/Aft offset between EK and trawl block
  if (GPS_Source == "GNSS_Fwd") {y_offset<- 8.617} #Forward/Aft offset between GNSS_Fwd antenna and trawl block
  layback<- y_offset + sqrt(((cat_fact * payout)^2) - ((depth + Block_height)^2))
  return(layback)
}

