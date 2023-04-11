#' get terra to produce same output as raster::rasterToPoints
#' @param rast A raster to convert to points
rasterToPoints2 <- function(rast) {
  as.matrix(terra::as.data.frame(rast, xy = TRUE))
}

#' export
#' @param l A list containing species objects that need to be converted
wrap_list <- function(l) {
  rapply(l, terra::wrap, classes = c("SpatVector", "SpatRaster"))
}

#' export
#' @param l A list containing species objects that need to be converted
unwrap_list <- function(l) {
  rapply(l, terra::unwrap, classes = c("PackedSpatVector", "PackedSpatRaster"))
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


check.points <- function(pts, name = "input") {
  if(!inherits(pts, "SpatVector")) {
    cls <- class(pts)
    if(length(cls) > 1) {
      cls <- paste(cls, collapse = ", ")
    }
    warning(sprintf('%s are not the expected SpatVector class (instead, it is of class %s). ENMTools will attempt to coerce using terra::vect(as.matrix(...), crs = "EPSG:4326"), but we cannot guaranteed the correctness of the result. Please consider using SpatVector format directly in the future, to minimize unexpected results.',
            name,
            cls))
    pts <- terra::vect(as.matrix(pts), crs = "EPSG:4326")
  }
  pts
}

check.raster <- function(rst, name = "input") {
  if(!inherits(rst, "SpatRaster")) {
    cls <- class(rst)
    if(length(cls) > 1) {
      cls <- paste(cls, collapse = ", ")
    }
    crss <- try(terra::crs(rst))
    if(!inherits(crss, "try-error")) {
      warning(sprintf('%s is not the expected SpatRaster class (instead, it is of class %s). ENMTools will attempt to coerce using terra::rast(...), but we cannot guaranteed the correctness of the result. Please consider using SpatRaster format directly in the future, to minimize unexpected results.',
            name,
            cls))
      rst <- terra::rast(rst)
    } else {
      warning(sprintf('%s is not the expected SpatRaster class (instead, it is of class %s). ENMTools will attempt to coerce using terra::rast(..., crs = "EPSG:4326"), but we cannot guaranteed the correctness of the result. Please consider using SpatRaster format directly in the future, to minimize unexpected results.',
            name,
            cls))
      rst <- terra::rast(rst, crs = "EPSG:4326")
    }
  }
  rst
}
