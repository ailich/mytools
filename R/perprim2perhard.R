#' Percent Primary to Percent Hardbottom
#'
#' Takes a vector of Percent Primary Cover and outputs a vector of Percent Hardbottom
#' @param indur_col vector of induration
#' @param per_col vector of percent primary cover bins
#' @keywords percent_cover
#' @importFrom stats na.omit
#' @export
perprim2perhard<- function(indur_col, per_col){
  bins<-na.omit(unique(per_col))
  num_bins<- length(bins)
  inv_bins<- vector(mode = "character", length = num_bins) #Initialize vector
  for (i in 1:num_bins) {
    if (bins[i]=="100") {inv_bins[i]<-"0"}
    else{
      inv_range<- rev(100-as.numeric(strsplit(x = bins[i],split = "-")[[1]])) #Inverts percentage
      inv_bins[i]<- paste(as.character(inv_range[1]), as.character(inv_range[2]), sep="-")
    }}
  inv_per<- rep(NA_character_, length=length(per_col))
  for(i in 1:num_bins){
    inv_per[per_col==bins[i]]<- inv_bins[i]
  } #Invert all bins in new data
  idx<- grep(pattern = "Soft", x = indur_col) #Index of softbottom locations
  per_hard<- per_col
  per_hard[idx]<- inv_per[idx] #Relace with inv_per only for Soft Bottom
  return(per_hard)
}
