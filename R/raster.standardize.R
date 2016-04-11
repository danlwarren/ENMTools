#' raster.standardize, standardizes all values in a raster file
#'
#' This function is used by a lot of the metrics calculated by ENMTools, in order to
#' standardize suitability scores so they sum to 1 over a geographic space.
#'
#'
#' @param x A raster object
#' @param verbose Controls printing of diagnostic messages
#'
#'
#' @keywords keywords
#'
#' @export raster.standardize
#'
#' @examples
#' raster.standardize(x)


raster.standardize <- function(x, verbose=FALSE){
  if(verbose){print(paste("Starting standardize on", x, "at", Sys.time()))}
  x/cellStats(x, stat=sum)
}
