#' Order Percent Cover Bins and gives their corresponding frequency
#'
#' Returns a tibbe of ordered percent cover bins and their corresponding frequency. Percent Cover Bins are ordered in ascending order based on lower range. Ties are broken by upper range.
#' @param percent_cover a character vector of percent cover data in ranges formatted as "lower-upper"
#' @export
#' @import dplyr

order_bins<- function(percent_cover){
  bins_count<- as_tibble(table(percent_cover))
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
  bins_count<- bins_count %>% bind_cols(as_tibble(low_range)) %>% bind_cols(as_tibble(high_range))
  names(bins_count)[c(3,4)]<- c("low_range","high_range")
  output<- arrange(bins_count, by=low_range, by=high_range) %>% select(bins, count)
  return(output)
}
