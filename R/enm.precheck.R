#' Takes an emtools.species object and does tests preparatory to building an ENM
#'
#' @param species An enmtools.species object
#'
#' @export enm.precheck

enm.precheck <- function(species){

  ### Check to make sure the data we need is there
  if(!class(f) == "formula"){
    stop("Argument \'formula\' must contain an R formula object!")
  }

  if(!"enmtools.species" %in% class(species)){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  if(!any(c("data.frame") %in% class(species$presence.points))){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }

  if(!any(c("data.frame") %in% class(species$background.points))){
    stop("Species background.points do not appear to be an object of class data.frame")
  }

  if(ncol(species$presence.points) < 3 & !any(c("raster", "RasterLayer", "RasterStack") %in% class(env))){
    stop("Species presence points do not contain environmental data, and no environmental rasters were supplied!")
  }

  if(ncol(species$background.points) < 3 & !any(c("raster", "RasterLayer", "RasterStack") %in% class(env))){
    stop("Species background points do not contain environmental data, and no environmental rasters were supplied!")
  }

  if(ncol(species$presence.points) < 2){
    stop("Species presence points do not contain longitude and latitude data!")
  }

  if(ncol(species$background.points) < 2){
    stop("Species background points do not contain longitude and latitude data!")
  }
}
