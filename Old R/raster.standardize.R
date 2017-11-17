#' raster.standardize, standardizes all values in a raster file
#'
#' This function is used by a lot of the metrics calculated by ENMTools, in order to
#' standardize suitability scores so they sum to 1 over a geographic space.
#'
#'
#' @param x A raster object or path to a raster.
#' @param verbose Controls printing of diagnostic messages
#'
#'
#' @keywords keywords
#'
#' @export raster.standardize
#'
#' @examples
#' raster.standardize(env[[1]])


raster.standardize <- function(x, verbose=FALSE){
  # Test if the args are raster objects or paths to files
  if(class(x) == "character"){
    x <- raster(x)
  }

  if(verbose){
    print(paste("Starting standardize on", x, "at", Sys.time()))
  }

  return(x/cellStats(x, stat=sum))
}
