#' Makes a proj4 string of common coordinate systems/datums for my project
#'
#' Makes proj4 string for WGS84, UTM 16N, and UTM 17N.
#' "WGS84": datum = WGS84, projection = lat/long
#' "UTM 16N": datum WGS84, projection = UTM 16N
#' "UTM 17N": datum WGS84, projection = UTM 17N
#' @param x desired coordinate system/datum
#' @import sp
#' @export

make_crs<- function(x){
  if (x == "WGS84") {out<- CRS("+init=epsg:4326")}
  if (x == "UTM 16N") {out<- CRS("+init=epsg:32616")}
  if (x == "UTM 17N") {out<- CRS("+init=epsg:32617")}
  return(out)
}
