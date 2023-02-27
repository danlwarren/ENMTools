#' Takes a set of points and a raster mask and returns a data frame trimmed so that only one point is returned per grid cell in the mask raster.
#'
#' @param points A two column data frame with X and Y coordinates
#' @param mask A raster to use as a mask for drawing points
#'
#' @return A new data frame with one point per grid cell.
#'
#' @examples
#' pts <- iberolacerta.clade$species$monticola$presence.points
#' trimdupes.by.raster(pts, euro.worldclim)

trimdupes.by.raster <- function(points, mask){

  pa <- terra::rasterize(as.matrix(terra::crds(points)), mask[[1]])

  new.points <- terra::as.points(pa)

  return(new.points)
}
