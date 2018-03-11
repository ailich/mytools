#' Computes p-value of accuracy of a classifer according to the Proportional Chance Criterion
#'
#' Computes p-value of accuracy of a classifer according to the Proportional Chance Criterion based on a specified number of iterations.
#' @param my_data a vector or one column dataframe/tibble
#' @param acc Accuracy of classifier
#' @param iter Number of iterations to run
#' @prop_val proportion of samples to be used in validation
#' @seed sampling is initiated by set.seed(seed) if input is not NULL. This will generate consistent results
#' @export

PCC<- function(my_data, acc, iter){
  my_data<- my_data[,1,drop=TRUE]
  n<- length(my_data)
  acc_chance<- vector(mode = "numeric", iter)
  acc_chance[1:iter]<- NA_real_
  acc_chance[1]<- acc
  for (i in 2:iter) {
    idx<- sample(1:n, size = n,replace = FALSE)
    shuffled_data<- my_data[idx]
    acc_chance[i]<- sum(my_data==shuffled_data)/n
  }
  p<- sum(acc_chance>=acc)/iter
  return(p)
}
