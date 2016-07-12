#' raster.cor Measure correlation coefficients between two rasters
#'
#' This function measures similarity in the geographic distribution of values
#' from two rasters.  Various methods can be specified, but it defaults to
#' spearman rank correaltion
#'
#' @param x A raster file
#' @param y Another raster file
#' @param method Determines which method to be used for calculating correlations (from cor function)
#'
#' @return results A correlation coefficient
#'
#' @keywords correlation, raster
#'
#' @export raster.cor
#'
#' @examples
#' raster.cor(env[[1]], env[[2]])
#'

raster.cor <- function(x, y, method="spearman"){
   x <- getValues(x)
   y <- getValues(y)
   x <- x[!is.na(x)]
   y <- y[!is.na(y)]
   return(cor(x, y, method=method))
}
