#' raster.overlap, measures overlap between two ENMs
#'
#' This function measures similarity in the geographic distribution of suitability scores
#' from two ENMs.  It returns two metrics, I and D.  These metrics are described in
#' Warren et al. 2008.
#'
#' @param x A raster file
#' @param x Another raster file
#' @param verbose Controls printing of diagnostic messages
#'
#' @return results A vector containing the two metrics
#'
#' @keywords keywords
#'
#' @export raster.overlap
#'
#' @examples
#' raster.overlap("~/species1.asc", "~/species2.asc")

raster.overlap <- function(x, y, verbose=FALSE){
  proceed <- TRUE
  if(proceed){  #All pre-analysis checks have been completed

    if(verbose){print(paste("Starting overlap at", Sys.time()))}

    x <- raster.standardize(x)
    y <- raster.standardize(y)

    D <- 1 - cellStats(abs(x - y), stat=sum)/2
    I <- 1 - cellStats((sqrt(x) - sqrt(y))^2, stat=sum)/2

    results <- list(D = D, I = I)
    return(results)
  }
}
