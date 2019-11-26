#' Makes a proj4 string based on EPSG code or name of common coordinate systems/datums for my project
#'
#' Makes proj4 string for WGS84, UTM 16N, and UTM 17N.
#' "WGS84": datum = WGS84, projection = lat/long (epsg:4326)
#' "UTM 16N": datum WGS84, projection = UTM 16N (epsg:32616)
#' "UTM 17N": datum WGS84, projection = UTM 17N (epsg:32617)
#' @param x desired coordinate system/datum or EPSG code
#' @param exclude_epsg logical indicating whether to exclude +init=epsg: at beginning of proj4 string
#' @importFrom sp CRS
#' @importFrom stringr str_remove
#' @export

make_crs<- function(x, exclude_epsg=FALSE){
   if (x == "WGS84"){EPSG<- 4326} else if (x == "UTM 16N") {
    EPSG<- 32616} else if (x == "UTM 17N"){
      EPSG<- 32617} else{
        EPSG<- x}
    out<- CRS(paste0("+init=epsg:", as.character(EPSG)))
    if(exclude_epsg){out<- CRS(str_remove(out, "^\\+init=epsg:\\d+"))}
    return(out)}
