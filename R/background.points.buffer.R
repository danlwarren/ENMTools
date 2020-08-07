#' Takes a set of points, a buffer radius, a sample size, and a mask and returns
#' randomly sampled points from within that buffer radius.
#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' @param points A two column data frame with X and Y coordinates
#' @param radius Radius for circular buffers to draw around points, in meters.
#' @param n Sample size for number of background points to return
#' @param mask A raster to use as a mask for drawing points
#'
#' @return A data frame of points drawn at random from circular buffers around the occurrence points.


background.points.buffer <- function(points, radius, n, mask){

  x <- circles(points, d=radius, lonlat=TRUE)

  pol <-  gUnaryUnion(x@polygons)

  buffer.raster <- mask(mask, pol)

  xy <- sampleRandom(buffer.raster, size=n, xy=TRUE)

  colnames(xy)[1:2] <- c(colnames(points))

  return(as.data.frame(xy[,1:2]))
}
