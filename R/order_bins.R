#' Order Percent Cover Bins
#'
#' Orders Percent Cover Bins in ascending order based on lower range
#' @param bins vector of bin ranges
#' @export
order_bins<- function(bins){
  num_bins<- length(bins)
  low_range<- vector(mode = "numeric",length = num_bins) #Dimension variable
  low_range[1:num_bins]<- NA #Set initial values to NA
  for (i in 1:num_bins) {
    if (bins[i]=="100") {low_range[i]=100}
    else{
      low_range[i]<- as.numeric(strsplit(x = bins[i], split ="-")[[1]][1])
    }}
  both<- cbind(as.data.frame(bins), as.data.frame(low_range))
  output<- arrange(both, by=low_range)[,1]
  return(output)
}
