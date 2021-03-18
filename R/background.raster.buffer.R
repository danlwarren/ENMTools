#' Takes a set of points, a buffer radius, and a mask and returns
#' a raster based on that buffer radius.
#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' @param points A two column data frame with X and Y coordinates
#' @param radius Radius for circular buffers to draw around points, in meters.
#' @param mask A raster to use as a mask
#'
#' @return A raster object with values of 1 in every grid cell falling within the buffer.
#'
#' @examples
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' background.raster.buffer(iberolacerta.clade$species$cyreni$presence.points, 100000, euro.worldclim)


background.raster.buffer <- function(points, radius, mask){

  x <- circles(points, d=radius, lonlat=TRUE)

  pol <-  gUnaryUnion(x@polygons)

  if(length(names(mask)) > 1){
    mask <- mask[[1]]
  }

  buffer.raster <- mask(mask, pol)

  buffer.raster[!is.na(buffer.raster)] <- 1

  return(buffer.raster)
}
