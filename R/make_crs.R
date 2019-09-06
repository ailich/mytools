#' Makes a proj4 string of common coordinate systems/datums for my project
#'
#' Makes proj4 string for WGS84, UTM 16N, and UTM 17N.
#' "WGS84": datum = WGS84, projection = lat/long
#' "UTM 16N": datum WGS84, projection = UTM 16N
#' "UTM 17N": datum WGS84, projection = UTM 17N
#' @param x desired coordinate system/datum
#' @param exclude_epsg logical indicating whether to exclude +init=epsg: at beginning of proj4 string
#' @import sp
#' @importFrom stringr str_remove
#' @export

make_crs<- function(x, exclude_epsg=FALSE){
  if (x == "WGS84") {out<- CRS("+init=epsg:4326")}
  if (x == "UTM 16N") {out<- CRS("+init=epsg:32616")}
  if (x == "UTM 17N") {out<- CRS("+init=epsg:32617")}
  if(exclude_epsg){out<- CRS(str_remove(out, "^\\+init=epsg:\\d+"))}
  return(out)
}
