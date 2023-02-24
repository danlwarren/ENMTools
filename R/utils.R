#' get terra to produce same output as raster::rasterToPoints
#' @param rast A SpatRaster object
rasterToPoints2 <- function(rast) {
  as.matrix(terra::as.data.frame(rast, xy = TRUE))
}


