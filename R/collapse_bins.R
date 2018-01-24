#' Order Percent Cover Bins, gives their corresponding frequency, and lower and upper ranges
#'
#' Returns a tibbe of ordered percent cover bins, their corresponding frequency, as well as lower and higher ranges of bins. Percent Cover Bins are ordered in ascending order based on lower range. Ties are broken by upper range.
#' @param percent_cover a character vector of percent cover data in ranges formatted as "lower-upper"

order_bins2<- function(percent_cover){
  bins_count<- as.tibble(table(percent_cover))
  names(bins_count)<- c("bins","count")
  num_bins<- NROW(bins_count)
  low_range<- vector(mode = "numeric",length = num_bins) #Dimension variable
  low_range[1:num_bins]<- NA #Set initial values to NA
  high_range<- vector(mode = "numeric",length = num_bins) #Dimension variable
  high_range[1:num_bins]<- NA #Set initial values to NA
  for (i in 1:num_bins) {
    low_range[i]<- as.numeric(strsplit(x = bins_count$bins[i], split ="-")[[1]][1])
    high_range[i]<- as.numeric(strsplit(x = bins_count$bins[i], split ="-")[[1]][2])
  }
  bins_count<- bins_count %>% bind_cols(as.tibble(low_range)) %>% bind_cols(as.tibble(high_range))
  names(bins_count)[c(3,4)]<- c("low_range","high_range")
  output<- arrange(bins_count, by=low_range, by=high_range)
  output$high_range[is.na(output$high_range)]<- output$low_range[is.na(output$high_range)]
  return(output)
}

#' Collapse Percent Cover Bins to a new specified range
#'
#' Takes a vector of percent cover data and a vector of new bin ranges to collapse the data
#' @param percent_cover a character vector of percent cover data in ranges formatted as "lower-upper" or a single number a a character
#' @param new_bins vector of new bins formatted as a string with (eg c("0", "1-5", "6-100")). Ranges must be in the format "lower-upper" or a single number as a character.
#' @export

collapse_bins<- function(percent_cover, new_bins){
  sorted_bins<- order_bins2(percent_cover)
  sorted_new_bins<- order_bins2(new_bins)
  sorted_bins$new_bins<- NA_character_
  for (i in 1:NROW(sorted_new_bins)) {
    idx<- sorted_bins$low_range>=sorted_new_bins$low_range[i] & sorted_bins$high_range<=sorted_new_bins$high_range[i]
    sorted_bins$new_bins[idx]<- sorted_new_bins$bins[i]
  }
  new_percent_cover<-vector(mode = "character",length = length(percent_cover))
  for (i in 1:NROW(sorted_bins)) {
    idx<- percent_cover==sorted_bins$bins[i]
    new_percent_cover[idx]<- sorted_bins$new_bins[i]
  }
  if(sum(is.na(new_percent_cover)!=0)){warning("Answer Contains NA's")}
  return(new_percent_cover)
}
