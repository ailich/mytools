#' Modification of writeRaster that can delete auxillary files
#'
#' Modification of writeRaster that can delete auxillary files. If overwrite is set to TRUE (the default) it will overwrite raster file and delete all associated files (e.g. pyramid files). Might only work on Windows.

#' @param x Raster* object
#' @param filename Output filename
#' @param overwrite Logical. If TRUE, "filename" will be overwritten if it exists. Additionally, auxillary files related to this raster will be deleted
#' @param ... other arguments passed to writeRaster
#' @importFrom raster writeRaster
#' @importFrom stringr str_remove
#' @export

writeRaster2<- function(x, filename, overwrite=TRUE, ...){
  if(!grepl("Raster", x = class(x))){
  message("Error: x is not a Raster* Object")
  stop()}
  if(overwrite){
    if(!grepl(pattern = "^[[:alpha:]]:/", filename)){
      filename= paste(getwd(), filename, sep="/")} #convert from relative to full path
    base_name<- basename(filename)
    write_dir<- str_remove(filename, paste0(base_name,"$"))
    aux_files<- list.files(path = write_dir, pattern = paste0("^", base_name, "\\.[[:alpha:]]+.*$"), full.names = TRUE)
    file.remove(aux_files)
  }
  writeRaster(x=x,filename=filename,overwrite=overwrite, ...)
}
