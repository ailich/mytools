#' Get ship position from Hypack log
#'
#' Get ship position from Hypack log
#' @param Hypack output of read_hypack
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr separate
#' @export
get_HypackPos<- function(Hypack){
  hypack_pos<- Hypack %>% dplyr::filter(X1=="MSG") %>% dplyr::filter(stringr::str_detect(X4, "^\\$GPGGA"))
  hypack_pos<- hypack_pos %>% tidyr::separate(X4, sep = ",", into = paste0("X", 5:17))
  hypack_pos<- hypack_pos %>% dplyr::select(timestamp, X9, X7)
  names(hypack_pos)[2:3]<- c("Ship_Lon", "Ship_Lat")
  hypack_pos<- hypack_pos %>% dplyr::mutate(Ship_Lon= -1*(as.numeric(substr(Ship_Lon, 1,3)) + (as.numeric(substr(Ship_Lon, 4,9))/60)))
  hypack_pos<- hypack_pos %>% dplyr::mutate(Ship_Lat= as.numeric(substr(Ship_Lat, 1,2)) + (as.numeric(substr(Ship_Lat, 3,8))/60))
  return(hypack_pos)
}
