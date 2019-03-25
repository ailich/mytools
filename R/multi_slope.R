#' Calculates multiscale slope of bathymetry/DEM
#' DOES NOT GIVE SIMILAR VALUES TO QGIS. NEED TO CHECK!!!!!!!!!!!!
#' Calculates multiscale slope of bathymetry/DEM in degrees. Any window containing an NA will result in an NA for slope. This function uses the Wood (1996) method which fits a bivariate quadratic equation (Evans 1980) and to an n x n grid and then calculates slope using ordinary least squares. For more details see Multiscale Terrain Analysis of Multibeam Bathymetry Data for Habitat Mapping on the Continental Slope by Wilson ey al 2007, pages 6-9.
#' @param bathy bathymetry or DEM raster
#' @param window_size odd integer specifying dimensions of window (default is 3x3)
#' @importFrom raster focal

multi_slope<- function(bathy, window_size = 3){
  output<- focal(x = bathy, w= matrix(1, nc=window_size, nr=window_size), fun= wood_slp)
  return(output)
}
