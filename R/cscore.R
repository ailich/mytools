#' Calculates normalized pairwise checkerboard C-scores
#'
#' Calculates normalized checkerboard C-scores based on Stone and Roberts 1990 and then standardizes them to a value between 0 and 1 by dividing by the maxiumum possible C-score for that pair of species (Groundfish Management Team 2013 http://www.pcouncil.org/wp-content/uploads/F8b_GMT_JUN2013BB.pdf)
#' Outputs a square symmetric species by species dataframe containing pairwise c-scores.
#' 0 indicates complete overlap. 1 indicates complete segregation.
#' > .7 indicates strong segregation and <.3 indicates strong overlap (Groundfish Management Team 2013)
#' @param pres_abs binary dataframe or tibble of presence/absence data where species are columns and observations are rows. Presence is indicated by 1 and absence is indicated by 0.
#' @export

cscore<- function(pres_abs){
  if(!is.data.frame(pres_abs)){
    message("Error: Input must be a dataframe or tibble")
    stop()}
  if(sum(pres_abs!=1 & pres_abs!=0)!=0){
    message("Error: Input must be a binary (0 or 1) presence absence data")
    stop()}
  output<- data.frame(matrix(data = NA_real_, nrow = ncol(pres_abs), ncol=ncol(pres_abs))) #Dimension Output
  rownames(output)<- colnames(pres_abs)
  colnames(output)<- colnames(pres_abs) #Create a Square symmentric species x species dataframe
  K<- colSums(pres_abs) #Number of species occurences
  for (i in 1:ncol(pres_abs)) {
    Ki<- K[i] #Number of occurences of Species i
    names(Ki)<- NULL #remove name
    for (j in 1:ncol(pres_abs)) {
      Kj<- K[j] #Number of occurences of Species i
      names(Kj)<- NULL #remove name
      pres_abs_ij<- pres_abs[,c(i,j)]
      Sij<- sum(rowSums(pres_abs_ij)==2) #Count Number of co-occurences
      Cij<- ((Ki-Sij)*(Kj-Sij))/(Ki*Kj) #Calculate C-Score
      output[i,j]<- Cij}
  }
  return(output)
}
