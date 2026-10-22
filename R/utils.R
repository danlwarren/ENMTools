#' get terra to produce same output as raster::rasterToPoints
#' @param rast A raster to convert to points
rasterToPoints2 <- function(rast) {
  as.matrix(terra::as.data.frame(rast, xy = TRUE))
}

#' export
#' @param l A list containing species objects that need to be converted
wrap.list <- function(l) {
  rapply(l, terra::wrap, classes = c("SpatVector", "SpatRaster"))
}

#' export
#' @param l A list containing species objects that need to be converted
unwrap.list <- function(l) {
  rapply(l, terra::unwrap, classes = c("PackedSpatVector", "PackedSpatRaster"))
}

make.analysis.df <- function(species) {
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
case.weights.check <- function (spec) {
    mod.type <- class(spec)[1]
    mod.eng <- spec$engine
    mod.mode <- spec$mode
    model.info <- parsnip::get_from_env(paste0(mod.type, "_fit"))
    model.info <- model.info[model.info$engine == mod.eng & model.info$mode == mod.mode, ]
    data.args <- model.info$value[[1]]$protect
    any(data.args == "weights")
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


# Suitability predicted by an ENMTools model for a table of environmental values
suitability.from.data <- function(model, newdata){
  newdata <- as.data.frame(newdata)
  if(inherits(model, "enmtools.tidy")){
    return(as.numeric(predict(model$model, new_data = newdata, type = "prob")$.pred_1))
  }
  if(inherits(model, "enmtools.maxent")){
    return(as.numeric(predict(model$model, newdata)))
  }
  stop("Unrecognized model type.")
}


# Model types accepted by the hypothesis tests
model.types <- c("glm", "gam", "bc", "dm", "rf", "rf.ranger", "maxnet", "mx", "tidy")

# Check the type, formula, and model specification given to a hypothesis test
check.model.type <- function(type, f, model){
  if(!type %in% model.types){
    stop(paste0("Model type ", type, " not understood! Select one of: ", paste(model.types, collapse = ", "), "."))
  }
  if(type == "tidy" && !inherits(model, "model_spec")){
    stop("type = \"tidy\" requires a parsnip model specification in the model argument, e.g. parsnip::boost_tree(\"classification\").")
  }
  if(!is.null(f) && !inherits(f, c("formula", "recipe"))){
    stop("f must be a formula or a recipe.")
  }
  if(type == "mx" && !is.null(f)){
    warning("f is not used for Maxent models (type = \"mx\").")
  }
}

# Fit a model of the given type, as used by the hypothesis tests
# clamp = NULL uses the default of the modeling function for that type.
enmtools.model.by.type <- function(type, species, env, f = NULL, model = NULL, model.args = list(), clamp = NULL, ...){
  fun <- switch(type,
                glm = enmtools.glm, gam = enmtools.gam, bc = enmtools.bc, dm = enmtools.dm,
                rf = enmtools.rf, rf.ranger = enmtools.rf.ranger, maxnet = enmtools.maxnet,
                mx = enmtools.maxent, tidy = enmtools.tidy,
                stop(paste("Model type", type, "not understood!")))
  args <- list(species = species, env = env)
  if(type != "mx"){
    args$f <- f
  }
  if(type == "tidy"){
    args$model <- model
    args$model.args <- model.args
  }
  if(!is.null(clamp)){
    args$clamp <- clamp
  }
  do.call(fun, c(args, list(...)))
}

# Suitability raster predicted by an ENMTools model for a set of environmental
# layers, clamped or not according to the setting used to fit the model
suitability.from.env <- function(model, env){
  clamp <- isTRUE(model[["clamp"]])
  if(inherits(model, "enmtools.tidy")){
    if(clamp){
      env <- clamp.env(clamp.data(model$analysis.df, model$model), env)
    }
    return(terra::predict(env, model$model, type = "prob", na.rm = TRUE)$.pred_1)
  }
  if(inherits(model, "enmtools.maxent")){
    return(terra::predict(env, model$model, na.rm = TRUE))
  }
  stop("Unrecognized model type.")
}

# Whether a hypothesis test's model type is a presence-only envelope model
# (Bioclim, DOMAIN, or a hypervolume), which is not clamped by default
is.envelope.type <- function(type, model = NULL){
  type %in% c("bc", "dm") || (type == "tidy" && inherits(model, "pres.only.sdm"))
}
