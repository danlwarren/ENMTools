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
  rapply(l, terra::wrap, classes = c("SpatVector", "SpatRaster"))
}

make_analysis.df <- function(species) {
  pres <- species$presence.points
  abs <- species$background.points
  pres$presence <- 1
  abs$presence <- 0
  analysis.df <- terra::as.data.frame(rbind(pres, abs), geom = "XY")
  analysis.df <- analysis.df[ , c("x", "y", colnames(analysis.df)[!colnames(analysis.df) %in% c("x", "y")])]
  analysis.df
}
