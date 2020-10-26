#' helper function for read_hypack
#'
#' helper function for read_hypack
#' @param filename filename of raw Hypack file
#' @import dplyr
#' @importFrom  readr read_delim
#' @importFrom  stringr str_extract
#' @importFrom  lubridate mdy_hms
#' @importFrom  lubridate dseconds

read_hypack_helper<- function(filename){
  Hypack<- suppressWarnings(suppressMessages(readr::read_delim(filename, delim=" ", col_names = FALSE, skip = 32, col_types = "")))
  File_Date<- read_delim <- read.csv(filename, skip = 8, header = FALSE, nrows = 1, colClasses = "character")
  File_Date<- stringr::str_extract(File_Date, pattern = "\\d{2}/\\d{2}/\\d{4}")
  File_Date<- lubridate::mdy_hms(paste0(File_Date,"_00:00:00"))
  Hypack<- Hypack %>% mutate(timestamp= File_Date+lubridate::dseconds(X3))
  Hypack<- Hypack %>% select(timestamp, everything())
  return(Hypack)
}


#' Reads RAW Hypack files
#'
#' Reads RAW Hypack log files into R
#' @param filename one or more RAW hypack files as a character vector
#' @importFrom dplyr bind_rows
#' @export

read_hypack<- function(filename){
  output<- dplyr::bind_rows(lapply(X = filename, FUN = read_hypack_helper))
  return(output)
}
