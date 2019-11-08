#' Counts number of vertices in a spatial object
#'
#' Counts number of vertices in a spatial object. Supported classes include SpatialPolygons, SpatialPolygonsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPoints, and SpatialPointsDataFrame.
#' @param shp supported sp object
#' @export

n_vert<-function(shp){
  if(class(shp)=="SpatialPolygonsDataFrame" | class(shp)=="SpatialPolygons"){
    nverts<- nrow(shp@polygons[[1]]@Polygons[[1]]@coords-1) #Count rows of coordinates and subtract 1 b/c 1st and last vertex are identical (closed shape)
    } else if (class(shp)=="SpatialLinesDataFrame" | class(shp)=="SpatialLines"){
      nverts<- nrow(shp@lines[[1]]@Lines[[1]]@coords)
      } else if (class(shp)=="SpatialPointsDataFrame" | class(shp)=="SpatialPoints"){
        nverts<- nrow(shp@coords)}
  return(nverts)}
