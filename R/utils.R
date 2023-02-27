#' get terra to produce same output as raster::rasterToPoints
#' @param rast A SpatRaster object
#' @param l A list object that includes SpatVector or SpatRaster objects to wrap or unwrap

rasterToPoints2 <- function(rast) {
  as.matrix(terra::as.data.frame(rast, xy = TRUE))
}

#' export
wrap_list <- function(l) {
  rapply(l, terra::wrap, classes = c("SpatVector", "SpatRaster"))
}

#' export
unwrap_list <- function(l) {
  rapply(l, terra::wrap, classes = c("SpatVector", "SpatRaster"))
}
