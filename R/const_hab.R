#' Extract More Constant Habitat Areas
#'
#' Returns only observations where the habitat classes were the same as their previous and subsequent observations. Useful for training algorithmns b/c reduces effect of positional uncertainty. Will return a dataframe if a dataframe is input. Will return a tibble if a tibble is input.
#' @param my_data a tibble or dataframe containing all data
#' @param hab_column number specifying the column to check
#' @export

const_hab<- function(my_data, hab_col){
  isatibble<- is.tibble(my_data)
  if (isatibble==TRUE) {my_data<-as.data.frame(my_data)} #Convert to dataframe for indexing
  idx=vector(mode = "logical", length =NROW(my_data)) #Initialize index vector as FALSE
  for (i in 2:(NROW(my_data)-1)) {
    if (my_data[,hab_col][i]==my_data[,hab_col][i-1] & my_data[,hab_col][i]==my_data[,hab_col][i+1]){idx[i]=TRUE}
  }#Include only observations that are the same as next and previous observations
  const_data<- my_data[idx,]
  if (isatibble==TRUE){const_data<- as.tibble(const_data)} #Convert to tibble if tibble was input
  return(const_data)
  }
