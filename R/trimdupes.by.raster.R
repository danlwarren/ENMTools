#' Takes a set of points and a raster mask and returns a data frame trimmed so that only one point is returned per grid cell in the mask raster.
#'
#' @param points A two column data frame with X and Y coordinates
#' @param mask A raster to use as a mask for drawing points
#'
#' @return A new data frame with one point per grid cell.
#'
#' @examples
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' pts <- iberolacerta.clade$species$monticola$presence.points
#' trimdupes.by.raster(pts, euro.worldclim)

trimdupes.by.raster <- function(points, mask){

  pa <- terra::rasterize(points, mask[[1]])

  new.points <- terra::as.points(pa)

  new.points <- terra::crds(new.points)

  colnames(new.points) <- c("Longitude", "Latitude")

  new.points <- as.data.frame(new.points)

  return(new.points)
}
