#' Calculates evaluation metrics (AUC, etc.) using latin hypercube sampling in environment space
#'
#' @param species An enmtools.species object
#' @param model An enmtools.model object or a model that can be projected using the predict() function of dismo
#' @param env A raster or raster stack of environmental data.
#' @param bg.source Determines whether minima and maxima of the environment space should be picked using the environment layers or the background points.
#' @param ... Arguments to be passed to othfer functions
#'
#' @export env.evaluate

env.evaluate <- function(species, model, env, bg.source = "background", ...){

  species <- check.bg(species, env, ...)

  if(!inherits(species, "enmtools.species")){
    stop("Argument species must supply an enmtools.species object!")
  }

  presence <- species$presence.points
  background <- species$background.points

  if(inherits(model, "enmtools.model")){
    model <- model$model
  }

  if(bg.source == "background"){
    allpoints <- rbind(presence, background)
    values <- extract(env, allpoints)
    maxes <- apply(values, 2, max)
    mins <- apply(values, 2, min)
  }

  if(bg.source == "env") {
    maxes <- maxValue(env)
    mins <- minValue(env)
  }

  this.lhs <- randomLHS(10000, length(names(env)))
  bg.table <- t(t(this.lhs) * (maxes  - mins) + mins)
  colnames(bg.table) <- names(env)
  p.table <- extract(env, presence)

  pred.p <- as.numeric(predict(model, data.frame(p.table), type = "response"))
  pred.bg <- as.numeric(predict(model, data.frame(bg.table), type = "response"))

  env.evaluation <- evaluate(pred.p, pred.bg)

  return(env.evaluation)
}
