#' Takes an emtools.model object and a set of environment layers and clamps the environment layers so that no variable falls outside of the range available in the training data.
#'
#' @param model An enmtools.model object.
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

  keep.env <- names(env)[names(env) %in% colnames(model$analysis.df)]
  clamped.env <- env[[keep.env]]

  for(i in names(clamped.env)){
    thismin <- min(model$analysis.df[,i])
    thismax <- max(model$analysis.df[,i])

    clamped.env[[i]][clamped.env[[i]] > thismax] <- thismax
    clamped.env[[i]][clamped.env[[i]] < thismin] <- thismin
  }

  return(clamped.env)

}
