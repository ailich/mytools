#' Get speed from Hypack log
#'
#' Get speed from Hypack log
#' @param Hypack output of read_hypack
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr separate
#' @export

get_HypackSpeed<- function(Hypack){
  hypack_speed<- Hypack %>% dplyr::filter(X1=="MSG") %>% dplyr::filter(stringr::str_detect(X4, "^\\$GPVTG"))
  hypack_speed<- hypack_speed %>% tidyr::separate(X4, sep = ",", into = paste0("X", 5:13))
  hypack_speed<- hypack_speed %>% dplyr::select(timestamp, X10, X12)
  names(hypack_speed)[2:3]<- c("Speed_knots", "Speed_kph")
  hypack_speed$Speed_knots<- as.numeric(hypack_speed$Speed_knots)
  hypack_speed$Speed_kph<- as.numeric(hypack_speed$Speed_kph)
  return(hypack_speed)
}
