#' raster.standardize, standardizes all values in a raster file
#'
#' This function is used by a lot of the metrics calculated by ENMTools, in order to
#' standardize suitability scores so they sum to 1 over a geographic space.
#'
#'
#' @param x A raster or RasterLayer object, or an ENMTools model object containing a suitability raster.
#' @param verbose Controls printing of diagnostic messages
#'
#' @return A new raster object, standardized so that values sum to 1
#'
#' @keywords keywords
#'
#' @examples
#' raster.standardize(euro.worldclim[[1]])


raster.standardize <- function(x, verbose=FALSE){

  if(inherits(x, "enmtools.model")){
    x <- x$suitability
  }

  if(verbose){
    print(paste("Starting standardize on", x, "at", Sys.time()))
  }

  minval <- terra::minmax(x)[1]

  if(minval < 0){
    x <- x - minval + 1e-20
  }


  return(x/as.numeric(terra::global(x, "sum", na.rm = TRUE)))
}
