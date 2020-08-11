#' Combines multiple rasters into one
#'
#' Combines multiple rasters into one by using merge after first reprojecting or resampling and aligning rasters by matching them up with a a specified origin, resolution, and coordinate reference system, or that of a reference raster.
#' @param raster_list a list of rasters
#' @param ref_rast reference raster with desired properties (Alternatively can supply desired_origin, desired_res, and desired_crs)
#' @param desired_origin desired origin of output raster as a vector with length 2 (x,y)
#' @param desired_res  desired resolution of output raster. Either an integer or a vector of length 2 (x,y)
#' @param desired_crs desired coordinate reference system of output raster (CRS class)
#' @param method resampling method. Either "bilinear" for bilinear interpolation (the default), or "ngb" for using the nearest neighbor
#' @param display_progress Logical specifying whether or not to indicate progress
#' @param normalize Logical (False by Default) indicating whether to z-score normalize raster layer before merge (Useful for merging backscatter)
#' @param use_mean Logical (False by Default) indicating whether to take mean value of layers in overlapping area. If FALSE calls raster::merge. If TRUE calls raster::mosaic(fun=mean).
#' @importFrom  raster merge
#' @importFrom RStoolbox normImage
#' @importFrom  raster mosaic
#' @export

combine_rasters<- function(raster_list, ref_rast=NULL, desired_origin, desired_res, desired_crs, method= "bilinear", display_progress=TRUE, normalize=FALSE, use_mean=FALSE){
  if(class(raster_list)!="list"){
    message("Error: raster_list must be of type list")
    stop()}
  raster_list2<- vector("list", length = length(raster_list))
  for (i in 1:length(raster_list)) {
    if(display_progress){
      print(paste("Reprojecting", as.character(i), "of", as.character(length(raster_list))))}
    raster_list2[[i]]<- reproject_align_raster(raster_list[[i]], ref_rast=ref_rast, desired_origin=desired_origin, desired_res=desired_res, desired_crs=desired_crs, method= method)}
  if(normalize){
    for (k in 1:length(raster_list2)) {
      if(display_progress){
        print(paste("Normalizing raster", as.character(k), "of", as.character(length(raster_list2))))}
      raster_list2[[k]]<- RStoolbox::normImage(img= raster_list2[[k]], norm=TRUE)
    }}
  if(use_mean==FALSE){
    for (j in 1:length(raster_list2)) {
      if(display_progress){
        print(paste("Combining raster", as.character(j), "of", as.character(length(raster_list2))))}
      if(j==1){
        combined_raster<- raster_list2[[j]]} else{
          combined_raster<- raster::merge(combined_raster, raster_list2[[j]])}}}
  if(use_mean==TRUE){
    if(display_progress){
      print("Combining rasters using mean value")}
    names(raster_list2)[1:2] <- c('x', 'y')
    raster_list2$fun <- mean
    raster_list2$na.rm <- TRUE
    combined_raster<- do.call(raster::mosaic, raster_list2)
  }
  return(combined_raster)
}
