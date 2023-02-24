#' get terra to produce same output as raster::rasterToPoints
rasterToPoints2 <- function(rast) {
  as.matrix(terra::as.data.frame(rast, xy = TRUE))
}

#' export
wrap_list <- function(l) {
  rapply(l, terra::wrap, classes = c("SpatVector", "SpatRaster"))
}

#' export
unwrap_list <- function(l) {
  rapply(l, terra::wrap, classes = c("SpatVector", SpatRaster))
}
