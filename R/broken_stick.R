#' Returns Eigenvalues expected by random data
#'
#' Returns Eigenvalues expected by random data for each component as determined by a broken-stick distribution. Useful for determining the number of interpretable Principal Components. When actual eigenvalues fall below this curve they are no longer considered interpretable.
#' @param n_variables number of variables
#' @export

broken_stick<- function(n_variables){
  b<-rep(NA_real_, n_variables)
  for (i in 1:n_variables) {
    b[i]<- sum(1/(i:n_variables)) #Eigenvector for each component under broken stick model
  }
  return(b)
}
