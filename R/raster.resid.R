#' raster.resid Measure standardized residuals from a linear regression between two rasters.
#'
#' This function builds a linear model for the relationship between two rasters, and returns the linear model
#' and a spatial raster of the residuals from that model.
#'
#' @param x A raster or RasterLayer object, or ENMTools model object containing a suitability raster.
#' @param y Another raster  or RasterLayer object, or ENMTools model object containing a suitability raster.
#'
#' @return results A list containing a raster of residuals from a linear regression between the two supplied rasters and the linear model itself.
#'
#' @keywords correlation, raster, residuals
#'
#' @export raster.resid
#'
#' @examples
#' raster.resid(ahli.raster, allogus.raster)
#'

raster.resid <- function(x, y){

  if(inherits(x, "enmtools.model")){
    x <- x$suitability
  }

  if(inherits(y, "enmtools.model")){
    y <- y$suitability
  }

  resid.raster <- y

  x <- getValues(x)
  y <- getValues(y)
  this.lm <- lm(x ~ y)
  resids <- this.lm$residuals/this.lm$fitted.values

  values(resid.raster)[!is.na(values(resid.raster))] <- resids

  return(list(residuals = resid.raster, lm = this.lm))
}
