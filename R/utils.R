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

## 'borrowed' nearly verbatim from parsnip::case_weights_allowed
case_weights_check <- function (spec) {
    mod_type <- class(spec)[1]
    mod_eng <- spec$engine
    mod_mode <- spec$mode
    model_info <- parsnip::get_from_env(paste0(mod_type, "_fit")) %>%
        dplyr::filter(engine == mod_eng & mode == mod_mode)
    data_args <- model_info$value[[1]]$protect
    any(data_args == "weights")
}
