#' Calculates Bathymetric Position Index
#'
#' Calculates Bathymetric Position Index (the difference between a central pixel and the mean of the values in the specified annulus). Positive values inidcate a crest, while negative values indicate a depression. 0 value is flat.
#' CURRENTLY DOES NOT MATCH VALUES FROM BENTHIC TERRAIN MODELLER
#'
#' @param bathy bathymetry raster
#' @param inner_rad Inner radius of annulus
#' @param outer_rad Outer radius of annulus
#' @param round_values logical specifying whether or not to round values to nearest integer (FALSE by default, but BTM algorithm in ArcGIS rounds by default)
#' @param standardize logical specifying whether or not to "standardize the BPI grid" (z-score). Default is FALSE
#' @param parallel logical specifying whether or not to run in parallel
#' @param ncores number of cores to use if running in parallel
#' @importFrom raster focal
#' @importFrom raster focalWeight
#' @importFrom raster res
#' @importFrom raster cellStats
#' @importFrom raster beginCluster
#' @importFrom raster endCluster
#' @importFrom raster clusterR
#' @importFrom parallel detectCores

BPI<- function(bathy, inner_rad, outer_rad, rad_units="cell", round_values= FALSE, standardize= FALSE, na.rm=FALSE, parallel=FALSE, ncores=parallel::detectCores()-1){
  out_name<- paste0("BPI_", as.character(inner_rad),"x", as.character(outer_rad), "_", rad_units)
  if(standardize){
    out_name<- paste(out_name, "std", sep="_")} else{
      out_name<- paste(out_name, "raw", sep="_")}
  if (rad_units!="cell" & rad_units!="map"){
    message("rad_units must be 'cell' or 'map'")
    stop()}
  if((raster::res(bathy)[1]!=raster::res(bathy)[2]) & rad_units== "cell"){
    message("x and y res must be equal if rad_units is cell")
    stop()}
  if(raster::res(bathy)[1]!=raster::res(bathy)[2]){ warning("x and y res are unequal")}
  if(rad_units=="cell"){
    inner_rad<- inner_rad*(raster::res(bathy)[1])
    outer_rad<- outer_rad*(raster::res(bathy)[1])} #convert to map units
  inner_w<- raster::focalWeight(x=bathy, d=inner_rad, type='circle')
  inner_w[inner_w>0]<- 1
  inner_w[ceiling(0.5 * length(inner_w))] <- 0
  outer_w<- raster::focalWeight(x=bathy, d=outer_rad, type='circle')
  outer_w[outer_w>0]<- 1
  outer_w[ceiling(0.5 * length(outer_w))] <- 0
  diff_rows<- nrow(outer_w) - nrow(inner_w)
  diff_cols<- ncol(outer_w) - ncol(inner_w)
  inner_w<- cbind(matrix(data = 0,nrow = nrow(inner_w),ncol = diff_cols/2), inner_w, matrix(data = 0,nrow = nrow(inner_w),ncol = diff_cols/2))
  inner_w<- rbind(matrix(data = 0,nrow = diff_rows/2,ncol = ncol(inner_w)), inner_w, matrix(data = 0,nrow = diff_rows/2,ncol = ncol(inner_w)))
  if(!identical(dim(inner_w), dim(outer_w))){
    message("algorithm failed to create windows properly")
    stop()}
  w<- outer_w - inner_w #Annulus window
  w[w>0]<- 1/sum(w>0) #Set weights to sum to 1
  if(parallel){
    raster::beginCluster(ncores)
    output<- bathy - raster::clusterR(x=bathy, fun = raster::focal, args= list(w=w, fun= sum, na.rm=FALSE)) #Calculate BPI
    raster::endCluster()} else{
        output<- bathy - raster::focal(x=bathy, w=w, fun=sum, na.rm=FALSE)} #Calculate BPI
  if(standardize){
    mean_BPI<- raster::cellStats(output, stat = "mean")
    sd_BPI<- raster::cellStats(output, stat = "sd")
    output<- (output- mean_BPI)/sd_BPI #Standardize BPI grid (z-score)
    }
  if (round_values) {output<- round(output, 0)} #round to interger values
  names(output)<- out_name
  return(output)
  }

