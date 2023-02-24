#' get terra to produce same output as raster::rasterToPoints
rasterToPoints2 <- function(rast) {
  as.matrix(terra::as.data.frame(rast, xy = TRUE))
}


