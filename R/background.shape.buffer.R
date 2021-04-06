#' Takes a set of points and a buffer radius, and returns
#' a polygon.
#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' NOTE: This function has been replaced by background.buffer.
#'
#' @param points A two column data frame with X and Y coordinates
#' @param radius Radius for circular buffers to draw around points, in meters.
#'
#' @return A polygon shapefile.


background.shape.buffer <- function(points, radius){

  return(background.buffer(points = points, buffer.width = radius,
                           buffer.type = "circles", mask = NA, return.type = "polygon"))
}
