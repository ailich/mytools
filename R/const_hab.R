#' Extract More Constant Habitat Areas
#'
#' Returns only observations where the habitat classes were the same as their previous and subsequent observations. Useful for training algorithmns b/c reduces effect of positional uncertainty. Will return a dataframe if a dataframe is input. Will return a tibble if a tibble is input.
#' @param my_data a tibble, dataframe, or spatial dataframe containing all data
#' @param hab_col number or name specifying the column to check
#' @export
#' @import dplyr
#' @import tibble
#' @import sp

const_hab<- function(my_data, hab_col){
  isatibble<- is_tibble(my_data)
  isaspdf<- FALSE
  if (isatibble==TRUE) {my_data<-as.data.frame(my_data)} #Convert to dataframe for indexing
  if(class(my_data)=="SpatialPointsDataFrame"){
    isaspdf<- TRUE
    coords<- my_data@coords
    prj4<- my_data@proj4string
    my_data<- my_data@data
    my_data<- cbind(my_data,coords)
  }
  idx=vector(mode = "logical", length =NROW(my_data)) #Initialize index vector as FALSE
  for (i in 2:(NROW(my_data)-1)) {
    if (identical(my_data[,hab_col][i], my_data[,hab_col][i-1]) & identical(my_data[,hab_col][i],my_data[,hab_col][i+1])){idx[i]=TRUE}
  }#Include only observations that are the same as next and previous observations
  const_data<- my_data[idx,]
  if (isatibble==TRUE){const_data<- as_tibble(const_data)} #Convert to tibble if tibble was input
  if (isaspdf){
    num_var<- ncol(my_data)
    const_data<- SpatialPointsDataFrame(coords = cbind(const_data[,num_var-1], const_data[,num_var]), data = const_data[,1:(num_var-2)], proj4string =prj4)
    }
  return(const_data)
  }
