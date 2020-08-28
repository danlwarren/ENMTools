#' Takes an emtools.model object and a set of environment layers and clamps the environment layers so that no variable falls outside of the range available in the training data.
#'
#' @param model An enmtools.model object.  Alternatively the analysis.df component of an enmtools.model object.
#' @param env A raster or raster stack of environmental data.
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @examples
#' \donttest{
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' monticola.gam <- enmtools.gam(iberolacerta.clade$species$monticola, euro.worldclim[[c(1,5,9,13)]])
#' euro.clamped <- clamp.env(monticola.gam, euro.worldclim)
#' clamped.prediction <- predict(monticola.gam, euro.clamped)
#' raster::plot(clamped.prediction$suitability - monticola.gam$suitability)
#' }


clamp.env <- function(model, env){

  ### Check to make sure the data we need is there
  if(!inherits(model, "enmtools.model") & !inherits(model, "data.frame")){
    stop("Argument \'model\' must contain an enmtools.model object or analysis.df data frame!")
  }

  if(!inherits(env, c("RasterLayer", "RasterStack", "raster", "RasterBrick"))){
    stop("Argument env must be a raster, RasterLayer, or RasterStack object!")
  }

  if(inherits(model, "enmtools.model")){
    df <- model$analysis.df
  } else {
    df <- model
  }

  # Getting the names of the layers to keep and subsetting the env layers to those
  keep.env <- names(env)[names(env) %in% colnames(df)]
  clamped.env <- env[[keep.env]]

  # Going through env layers one at a time and clamping to min/max in analysis.df
  for(i in names(clamped.env)){
    thismin <- min(df[,i])
    thismax <- max(df[,i])

    # Replacing values under min or over max with min/max respectively
    clamped.env[[i]][clamped.env[[i]] > thismax] <- thismax
    clamped.env[[i]][clamped.env[[i]] < thismin] <- thismin
  }

  return(clamped.env)

}
