#' raster.breadth, applies measures of niche breadth to an ENM
#'
#' This function measures the spatial heterogeneity of the distribution of suitability
#' scores from an ENM.  It returns Levins' two metrics of niche breadth.
#'
#' @param x An object of class raster or RasterLayer, or an ENMTools model object containing a suitability raster.
#' @param verbose Controls printing of diagnostic messages
#'
#' @return results A vector containing the two metrics B1 and B2 for niche breadth.
#'
#' @keywords keywords
#'
#' @examples
#' data(iberolacerta)
#' data(euro.worldclim)
#' aurelioi.glm <- enmtools.glm(iberolacerta.clade$species$aurelioi, euro.worldclim,
#' f = pres ~ bio1 + bio12)
#' raster.breadth(aurelioi.glm)

raster.breadth <- function(x, verbose=FALSE){

  if(inherits(x, "enmtools.model")){
    x <- x$suitability
  }

  if(verbose){print(paste("Starting breadth on", x, "at", Sys.time()))}
  x <- raster.standardize(x)

  x[which(getValues(x) == 0)] <- 1e-40

  B1 <- calc.B1(getValues(x))
  B2 <- calc.B2(getValues(x))

  results <- list(B1 = B1, B2 = B2)
  return(results)

}
