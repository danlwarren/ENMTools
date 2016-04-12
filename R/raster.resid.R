#' raster.resid Measure correlation coefficients between two rasters
#'
#' This function measures similarity in the geographic distribution of values
#' from two rasters.  Various methods can be specified, but it defaults to
#' spearman rank correaltion
#'
#' @param x A raster file
#' @param y Another raster file
#'
#' @return results A raster of residuals from a linear regression between the two supplied rasters
#'
#' @keywords correlation, raster, residuals
#'
#' @export raster.resid
#'
#' @examples
#' raster.resid(ahli.raster, allogus.raster)
#'

raster.resid <- function(x, y, method="spearman"){

  resid.raster <- y

  x <- getValues(x)
  y <- getValues(y)
  this.lm <- lm(x ~ y)
  resids <- this.lm$residuals/this.lm$fitted.values

  values(resid.raster)[!is.na(values(resid.raster))] <- resids

  return(list(residuals = resid.raster, lm = this.lm))
}
