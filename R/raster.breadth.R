#' raster.breadth, applies measures of niche breadth to an ENM
#'
#' This function measures the spatial heterogeneity of the distribution of suitability
#' scores from an ENM.  It returns Levins' two metrics of niche breadth.
#'
#' @param x An object of class raster or RasterLayer, or an ENMTools model object containing a suitability raster.
#' @param verbose Controls printing of diagnostic messages
#'
#' @return results A vector containing the two metrics
#'
#' @keywords keywords
#'
#' @export raster.breadth
#'
#' @examples
#' raster.breadth("env[[1]]")

raster.breadth <- function(x, verbose=FALSE){

  if(grepl("enmtools", class(x))){
    x <- x$suitability
  }

  if(verbose){print(paste("Starting breadth on", x, "at", Sys.time()))}
  x <- raster.standardize(x)

  ncells <- sum(!is.na(getValues(x)))

  B1 <- (1/cellStats(x^2, sum) - 1)/(ncells - 1)
  B2 <- 0 - cellStats(x * log(x), sum)/log(ncells)

  results <- list(B1 = B1, B2 = B2)
  return(results)

}
