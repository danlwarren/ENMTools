#' Takes a set of points, a buffer radius, and a mask and returns
#' a raster based on that buffer radius.
#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' NOTE: This function has been replaced by background.buffer.
#'
#' @param points A two column data frame with X and Y coordinates
#' @param radius Radius for circular buffers to draw around points, in meters.
#' @param mask A raster to use as a mask
#'
#' @return A raster object with values of 1 in every grid cell falling within the buffer.
#'
#' @examples
#' library(ENMTools)
#' background.raster.buffer(iberolacerta.clade$species$cyreni$presence.points, 100000, euro.worldclim)
background.raster.buffer <- function(points, radius, mask){

  return(background.buffer(points = points, buffer.width = radius, mask = mask,
                           buffer.type = "circles", return.type = "raster"))
}
