#' Takes two emtools.species objects with range rasters, calculates overlap between them as in Fitzpatrick and Turelli 2006.  This metric divides the area of overlap between two species ranges by the smaller of the two areas of the species' individual ranges.  It therefore ranges from 0 (no overlap) to 1 (ranges are the same or the smaller species' range is contained entirely within the larger).
#'
#' @param x An enmtools.species object containing a range raster
#' @param y An enmtools.species object containing a range raster
#'
#' @return A numeric value measuring range overlap.
#'
#' @examples
#' cyreni <- iberolacerta.clade$species$cyreni
#' monticola <- iberolacerta.clade$species$monticola
#' cyreni$range <- background.raster.buffer(cyreni$presence.points, 100000, euro.worldclim)
#' monticola$range <- background.raster.buffer(monticola$presence.points, 100000, euro.worldclim)
#' geog.range.overlap(cyreni, monticola)

geog.range.overlap <- function(x, y){

  if(!inherits(x$range, c("SpatRaster"))){
    stop(paste("Species", x$species.name, "does not have a range raster!"))
  }

  if(!inherits(y$range, c("SpatRaster"))){
    stop(paste("Species", y$species.name, "does not have a range raster!"))
  }

  overlap.cells <- sum(terra::values(x$range * y$range) == 1, na.rm = TRUE)
  min.cells <- min(sum(terra::values(x$range) == 1, na.rm = TRUE), sum(terra::values(y$range) == 1, na.rm = TRUE))
  return(overlap.cells/min.cells)

}
