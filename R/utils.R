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
  analysis.df <- analysis.df[complete.cases(analysis.df), ]
  analysis.df
}

## 'borrowed' nearly verbatim from parsnip::case_weights_allowed
case_weights_check <- function (spec) {
    mod_type <- class(spec)[1]
    mod_eng <- spec$engine
    mod_mode <- spec$mode
    model_info <- parsnip::get_from_env(paste0(mod_type, "_fit"))
    model_info <- model_info[model_info$engine == mod_eng & model_info$mode == mod_mode, ]
    data_args <- model_info$value[[1]]$protect
    any(data_args == "weights")
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
    pts <- reformat.latlon(pts)
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
