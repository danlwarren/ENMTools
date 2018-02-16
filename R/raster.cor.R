#' Calculates the correlation coefficient between two rasters.
#'
#' @param x A raster representing the spatial projection of an SDM, or an ENMTools model object with a suitability raster.
#' @param y A raster representing the spatial projection of an SDM, or an ENMTools model object with a suitability raster.
#' @param method The method to be used for calculating correlations.  Defaults to spearman, but can take "kendall" or "pearson" as well.
#'
#' @export raster.cor
#'
#' @examples
#' data(euro.worldclim)
#' raster.cor(euro.worldclim[[1]], euro.worldclim[[2]])


raster.cor <- function(x, y, method="spearman"){

  if(inherits(x, "enmtools.model")){
    x <- x$suitability
  }

  if(inherits(y, "enmtools.model")){
    y <- y$suitability
  }


  df <- cbind(getValues(x), getValues(y))

  df <- df[complete.cases(df),]

  return(cor(df[,1], df[,2], method=method))
}
