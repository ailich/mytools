#' Percent Primary to Percent Hardbottom
#'
#' Converts Percent Primary to Percent Hardbottom
#' @param my_data habitat spreadsheet
#' @param indur_col column number of induration
#' @param per_col column number of percent primary
#' @keywords percent_cover
#' @export
perprim2perhard<- function(my_data,indur_col, per_col){
  per<- my_data[,per_col][[1]]
  bins<-unique(per)
  num_bins<- length(bins)
  inv_bins<- vector(mode = "character", length = num_bins) #Initialize vector
  for (i in 1:num_bins) {
    if (bins[i]=="100") {inv_bins[i]<-"0"}
    else{
      inv_range<- rev(100-as.numeric(strsplit(x = bins[i],split = "-")[[1]])) #Inverts percentage
      inv_bins[i]<- paste(as.character(inv_range[1]), as.character(inv_range[2]), sep="-")
    }}
  inv_per<- vector(mode = "character", length = length(per))
  for(i in 1:num_bins){
    inv_per[per==bins[i]]<- inv_bins[i]
  } #Invert all bins in new data
  idx<- grep(pattern = "Soft", x = my_data[,indur_col][[1]]) #Index of softbottom locations
  output<- my_data
  output[,per_col][[1]][idx]<- inv_per[idx] #Relace with inv_per only for Soft Bottom
  names(output)[per_col]<- "Percent_Hardbottom"
  return(output)
}
