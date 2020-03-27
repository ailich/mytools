#' Calculates Bathymetric Position Index
#'
#' Calculates Bathymetric Position Index (the difference between a central pixel and the mean of the values in the specified annulus). Positive values inidcate a crest, while negative values indicate a depression. 0 value is flat.
#'
#' @param bathy bathymetry raster
#' @param inner_rad Inner radius of annulus
#' @param outer_rad Outer radius of annulus
#' @param na.rm logical. If TRUE, NA will be removed from focal computations. The result will only be NA if all focal cells are NA. (Note: na.rm=TRUE will be much slower). Default is FALSE.
#' @param pad logical. If TRUE, additional 'virtual' rows and columns are padded to x such that there are no edge effects. Only applicable if na.rm=TRUE. Value of virtual cells is set to NA. Default is FALSE
#' #' @param na.rm_type default is 1. If set to 2, when na.rm=FALSE, it will remove all zero weights values from focal window before calculating so that NA's in zero weight cells do not cause calculation to evaluate to NA (Only applicable for na.rm=FALSE, and much slower than na.rm_type=1 with na.rm=FALSE)
#' @param software "R" "QGIS" or "Arc" (Note: If using QGIS you must first establish a connection using RQGIS3::set_env() and RQGIS3::qgis_session_info(). If using Arc you must have a Spatial Analyst license. Also for either the R Version architecture type (32bit or 64bit) must match architecture type of ArcGIS/QGIS)
#' @param Arc_Conn Connection to ARcGIS. object output by RPyGeo::rpygeo_build_env. Make sure to set extensions argument to "Spatial"
#' @export

BPI<- function(bathy, inner_rad, outer_rad, na.rm=FALSE, pad=FALSE, na.rm_type=1, software= "R", Arc_Conn){
  if(!all.equal(raster::res(bathy)[1],raster::res(bathy)[2])){ warning("x and y res are unequal")}

  if(software=="R"){
    return(mytools:::BPI_R(bathy=bathy, inner_rad=inner_rad, outer_rad=outer_rad, na.rm=na.rm, pad=pad, na.rm_type=na.rm_type))}

  if(software=="QGIS"){
    return(mytools:::BPI_QGIS(bathy=bathy, inner_rad=inner_rad, outer_rad=outer_rad))}

  if(software=="Arc"){
    return(mytools:::BPI_Arc(bathy=bathy, inner_rad=inner_rad, outer_rad=outer_rad, Arc_Conn = Arc_Conn))}
  }

#' @section BPI_R
#' Calcuates BPI in R
#' @import dplyr
#' @importFrom raster focal
#' @importFrom raster res
BPI_R<- function(bathy, inner_rad, outer_rad, na.rm=FALSE, pad=FALSE, na.rm_type=1){
  out_name<- paste0("BPI_", as.character(inner_rad),"x", as.character(outer_rad))
  w<- annulus_window(inner_rad=inner_rad, outer_rad=outer_rad, type=1)
  if(!na.rm){
    if(na.rm_type==1){
    w[w>0]<- 1/sum(w>0) #Set weights to sum to 1
    output<- bathy - raster::focal(x=bathy, w=w, fun=sum, na.rm=FALSE, pad=FALSE) #Calculate BPI
    } else{
      output<- bathy - raster::focal(x=bathy, w=w, na.rm=FALSE, pad=FALSE,
                                     fun=function(x,...){
                                       my_vals <- x
                                       my_weights<- as.vector(t(w))
                                       idx<- my_weights!=0
                                       my_vals<- my_vals[idx] #Remove values with zero weights
                                       mean(my_vals, na.rm = FALSE)})}}
  if(na.rm) {
        output<- bathy - raster::focal(x = bathy, w = w, na.rm = TRUE,
                                       pad = pad, fun = function(x, ...) {
                                         valXweights <- x
                                         my_weights <- as.vector(t(w))
                                         my_vals <- valXweights/my_weights
                                         idx<- !(is.na(my_vals) | (my_weights==0))
                                         my_vals<- my_vals[idx]
                                         my_weights<- my_weights[idx]
                                         mean(my_vals)}) #Calculate BPI
        }
  names(output)<- out_name
  return(output)
}

#' @section #' @section BPI_QGIS
#' Calculate BPI in QGIS
#' @import dplyr
#' @importFrom lubridate with_tz
#' @importFrom stringr str_remove_all
#' @importFrom RQGIS3 run_qgis

BPI_QGIS<- function (bathy, inner_rad, outer_rad) {
  def_state<- getOption("digits.secs")
  w <- mytools::annulus_window(inner_rad=inner_rad, outer_rad= outer_rad, type=1)
  options(digits.secs=6)
  f_name<- paste0(tempdir(), "/", stringr::str_remove_all(as.character(lubridate::with_tz(Sys.time(), tzone = "UTC")), pattern="\\D"), "_mean_", as.character(inner_rad), "_", as.character(outer_rad), "_temp.tif")
  options(digits.secs=def_state)
  focal_res<- run_qgis(alg = "grass7:r.neighbors", params = list(input=bathy, size=nrow(w), weight=w ,output= f_name), load_output=TRUE)
  output<- bathy - focal_res
  out_name<- paste0("BPI_", as.character(inner_rad),"x", as.character(outer_rad))
  names(output)<- out_name
  return(output)
}

#' @section Calculate BPI_Arc
#' Calculates BPI in ArcGIS
#' @import dplyr
#' @importFrom raster writeRaster
#' @importFrom lubridate with_tz
#' @importFrom stringr str_remove_all
#' @importFrom RPyGeo rpygeo_build_env
BPI_Arc<- function (bathy, inner_rad, outer_rad, Arc_Conn) {
  def_state<- getOption("digits.secs")
  options(digits.secs=6)
  in_file<- paste0(tempdir(), "/", stringr::str_remove_all(as.character(lubridate::with_tz(Sys.time(), tzone = "UTC")), pattern="\\D"), "_bathy_", "_temp.tif")
  out_file<- paste0(tempdir(),
                    "\\",
                    Sys.time() %>% with_tz("UTC") %>% as.character() %>% str_remove_all("\\D"),
                    ".tif")
  options(digits.secs=def_state)
  writeRaster(bathy, in_file)
  focal_res<- Arc_Conn$sa$FocalStatistics(in_raster = in_file, neighborhood = Arc_Conn$sa$NbrAnnulus(inner_rad,outer_rad, "CELL"), statistics_type = "MEAN")
  focal_res<- rpygeo_load(focal_res)
  out_name<- paste0("BPI_", as.character(inner_rad),"x", as.character(outer_rad))
  output<- bathy-focal_res
  names(output)<- out_name
  return(output)
}
