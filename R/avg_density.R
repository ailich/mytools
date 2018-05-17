#' Calculates Average Fish Density and bootstrap confidence intervals
#'
#' Calculates Fish Densities and optionally bootstrap confidence intervals.
#' Returns a dataframe or tibble of densities for each species (depending on class input as counts)
#' If iter is not NULL than row order will be lower bound, calculated average, upper bound
#' @param counts dataframe or tibble of counts
#' @param area vector of areas corresponding to observations (rows) in counts
#' @param conf optional confidence level (0-1)
#' @param iter optional number of iterations for creating confidence intervals
#' @export
#' @import dplyr
#' @import magrittr
avg_density<- function(counts, area, iter=NULL, conf=NULL){
#Check Inputs
if(!is.data.frame(counts)){
  message("Error: counts must be dataframe or tibble")
  stop()}
if(!is.vector(area)){
  message("Error: area must be a vector")
  stop()}
isatibble<- is_tibble(counts)
if(isatibble){counts<- as.data.frame(counts)}

#Calculate Average Density
densities<- calc_dens(counts = counts, area = area)

if(is.null(iter)){output=densities} else{
  boot_dens<- as.data.frame(matrix(data = NA_real_, nrow = iter, ncol = ncol(counts))) #Dimension variable
  names(boot_dens)<- names(counts)
  boot_dens[1,]<- densities #Add observed Density
  for (i in 2:iter) {
    rowidx<- sample(x = 1:nrow(counts), size = nrow(counts), replace = TRUE)
    counts_boot<- counts[rowidx,] #Sample with replacement to create bootstrap sample
    area_boot<- area[rowidx]
    boot_dens[i,]<- calc_dens(counts = counts_boot,area = area_boot) #Append calculated bootstrap density
  }
  boot_dens<- as.data.frame(sapply(boot_dens,sort)) #sort densities
  lower_idx<- round(iter*(1-conf)/2) #round to deal with floating point errors
  upper_idx<- iter-lower_idx
  lower<- boot_dens[lower_idx,]
  upper<- boot_dens[upper_idx,]
  output<- rbind(lower, densities, upper)
  rownames(output)<- NULL
}
if(isatibble){output<- as_tibble(output)}
return(output)
}

#' Helper function to calculate fish densities
#'
#' calculate average fish densities
#' @param counts dataframe of counts
#' @param area vector of areas corresponding to observations (rows) in counts
calc_dens<- function(counts, area){
  densities<- as.data.frame(lapply(counts,sum))/sum(area)
  names(densities)<- names(counts) #Fixes names (as.data.frame changes spaces to periods)
  return(densities)
}


