#' Calculates the correlation coefficient between two rasters.
#'
#' @param x A raster representing the spatial projection of an SDM, or an ENMTools model object with a suitability raster.
#' @param y A raster representing the spatial projection of an SDM, or an ENMTools model object with a suitability raster.
#' @param method The method to be used for calculating correlations.  Defaults to spearman, but can take "kendall" or "pearson" as well.
#'
#' @export raster.cor


raster.cor <- function(x, y, method="spearman"){

  if(grepl("enmtools", class(x))){
    x <- x$suitability
  }

  if(grepl("enmtools", class(y))){
    y <- y$suitability
  }


  x <- getValues(x)

  y <- getValues(y)

  x <- x[!is.na(x)]

  y <- y[!is.na(y)]

  return(cor(x, y, method=method))
}
