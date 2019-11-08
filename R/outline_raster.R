#' Outlines non-na values of a raster
#'
#' Outlines non-na values of a raster. May want to follow up with rgeos::gsimplify to reduce number of vertices.
#' @param r A Raster object
#' @param  fact aggregation factor for raster::aggregate. Useful if have very high resolution raster and tacing it causes memory issues. Also useful for filling "holes" in the middle of rasters where there may be some NA vals. By default fact=10
#' @importFrom raster aggregate
#' @importFrom raster rasterToPolygons
#' @export

outline_raster<- function(r, fact=10){
  if(fact>1){
    r<- raster::aggregate(x = r, fact=fact, fun=mean, na.rm=TRUE)}
  r1<- r >= -Inf
  outline <- raster::rasterToPolygons(r1, dissolve=TRUE)
  return(outline)
}
