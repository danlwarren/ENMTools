#' raster.resid Measure standardized residuals from a linear regression between two rasters.
#'
#' This function builds a linear model for the relationship between two rasters, and returns the linear model
#' and a spatial raster of the residuals from that model.
#'
#' @param x A raster file or path to a raster.
#' @param y Another raster file or path.
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

  # Test if the args are raster objects or paths to files
  if(class(x) == "character"){
    x <- raster(x)
  }

  if(class(y) == "character"){
    y <- raster(y)
  }

  resid.raster <- y

  x <- getValues(x)
  y <- getValues(y)
  this.lm <- lm(x ~ y)
  resids <- this.lm$residuals/this.lm$fitted.values

  values(resid.raster)[!is.na(values(resid.raster))] <- resids

  return(list(residuals = resid.raster, lm = this.lm))
}
