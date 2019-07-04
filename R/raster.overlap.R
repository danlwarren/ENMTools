#' raster.overlap, measures overlap between two ENMs
#'
#' This function measures similarity in the geographic distribution of suitability scores
#' from two ENMs.  It returns two metrics, I and D.  These metrics are described in
#' Warren et al. 2008.
#'
#' @param x A raster or RasterLayer object, or ENMTools model object containing a suitability raster.
#' @param y Another raster or RasterLayer object, or ENMTools model object containing a suitability raster.
#' @param verbose Controls printing of diagnostic messages
#'
#' @return results A vector containing the three metrics (I, D, and Spearman rank correlation)
#'
#' @keywords keywords
#'
#' @examples
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' aurelioi.glm <- enmtools.glm(iberolacerta.clade$species$aurelioi,
#' euro.worldclim, f = pres ~ bio1 + bio12)
#' aranica.glm <- enmtools.glm(iberolacerta.clade$species$aranica,
#' euro.worldclim, f = pres ~ bio1 + bio12)
#' raster.overlap(aurelioi.glm, aranica.glm)

raster.overlap <- function(x, y, verbose=FALSE){

  if(any(grepl("enmtools", class(x)))){
    x <- x$suitability
  }

  if(any(grepl("enmtools", class(y)))){
    y <- y$suitability
  }

  if(verbose){
    print(paste("Starting overlap at", Sys.time()))
  }

  x <- raster.standardize(x)
  y <- raster.standardize(y)

  D <- 1 - cellStats(abs(x - y), stat=sum)/2
  I <- 1 - cellStats((sqrt(x) - sqrt(y))^2, stat=sum)/2
  rank.cor <- raster.cor(x, y)

  results <- list(D = D, I = I, rank.cor = rank.cor)
  return(results)

}
