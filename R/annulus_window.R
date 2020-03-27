#' Creates annulus focal window
#'
#' Creates annulus focal window around central pixel.
#' https://desktop.arcgis.com/en/arcmap/10.3/analyze/arcpy-spatial-analyst/nbrannulus-class.htm

#' @param inner_rad radius of inner annulus in cell units (must be at least 1 and less than or equal to outer radius)
#' @param outer_rad radius of inner annulus in cell units
#' @param type Either 1 or 2. If type = "1" then weights will be 0 or 1. if type ="2", weights will sum to 1. Default is 1.
#' @export

annulus_window<- function(inner_rad, outer_rad, type=1){
  if (type != 1 & type != 2){
    message("type must be equal to 1 or 2")
    stop()}
  if(inner_rad > outer_rad){
    message("inner_rad must be less than or equal to outer_rad")
    stop()}
  if(inner_rad < 1){
    message("inner_rad must be at least 1")
    stop()}
  inner_w <- mytools::circle_window(radius = inner_rad-1, unit = "cell", type = 1)
  inner_w[ceiling(0.5 * length(inner_w))] <- 0
  outer_w <- mytools::circle_window(radius = outer_rad, unit = "cell", type = 1)
  outer_w[ceiling(0.5 * length(outer_w))] <- 0

  diff_rows <- nrow(outer_w) - nrow(inner_w)
  diff_cols <- ncol(outer_w) - ncol(inner_w)
  inner_w <- cbind(matrix(data = 0, nrow = nrow(inner_w), ncol = diff_cols/2),
                   inner_w, matrix(data = 0, nrow = nrow(inner_w), ncol = diff_cols/2))
  inner_w <- rbind(matrix(data = 0, nrow = diff_rows/2, ncol = ncol(inner_w)),
                   inner_w, matrix(data = 0, nrow = diff_rows/2, ncol = ncol(inner_w)))
  if (!identical(dim(inner_w), dim(outer_w))) {
    message("algorithm failed to create windows properly")
    stop()
  }
  w <- outer_w - inner_w
  if(type==2){
    w[w>0]<- 1/sum(w)}
  return(w)}
