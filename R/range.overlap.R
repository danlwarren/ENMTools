#' Takes two emtools.species objects with range rasters, calculates overlap between them as in Fitzpatrick and Turelli 2006
#'
#' @param x An enmtools.species object containing a range raster
#' @param y An enmtools.species object containing a range raster
#'
#' @export geog.range.overlap

geog.range.overlap <- function(x, y){

  if(!inherits(x$range, c("raster", "RasterLayer"))){
    stop(paste("Species", x$species.name, "does not have a range raster!"))
  }

  if(!inherits(y$range, c("raster", "RasterLayer"))){
    stop(paste("Species", y$species.name, "does not have a range raster!"))
  }

  overlap.cells <- sum(getValues(x$range * y$range) == 1, na.rm = TRUE)
  min.cells <- min(sum(getValues(x$range) == 1, na.rm = TRUE), sum(getValues(y$range) == 1, na.rm = TRUE))
  return(overlap.cells/min.cells)

}
