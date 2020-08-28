#' Calculates evaluation metrics (AUC, etc.) using latin hypercube sampling in environment space
#'
#' @param species An enmtools.species object
#' @param model An enmtools.model object or a model that can be projected using the predict() function of dismo
#' @param env A raster or raster stack of environmental data.
#' @param bg.source Determines whether minima and maxima of the environment space should be picked using the environment layers or the background points.
#' @param n.background The number of background points to sample from the environment space.
#' @param test.eval When set to "true", env.evaluate evaluates the test data stored in the model object instead of the training data.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param ... Arguments to be passed to othfer functions
#'
#' @return A dismo evaluate object measuring the performance of model predictions in environment space.
#'
#' @examples
#' \donttest{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni <- iberolacerta.clade$species$cyreni
#' cyreni.glm <- enmtools.glm(cyreni, euro.worldclim, test.prop = 0.2,
#' f = pres ~ bio1 + bio12, nback = 500)
#' env.evaluate(cyreni, cyreni.glm,  euro.worldclim)
#' }


env.evaluate <- function(species, model, env, bg.source = "background", n.background = 10000, test.eval = FALSE, verbose = FALSE, ...){

  # If we're evaluating the test data instead of the training data, we need
  # to make sure the data exists and then stuff it into the species object if so
  if(test.eval == TRUE){
    if(is.na(model$test.data)){
      stop("Test.eval set to TRUE but no test data present in model object!")
    }
    species$presence.points <- model$test.data
  }

  species <- check.bg(species, env, verbose = verbose)

  if(!inherits(species, "enmtools.species")){
    stop("Argument species must supply an enmtools.species object!")
  }

  presence <- species$presence.points[,1:2]
  background <- species$background.points[,1:2]

  if(inherits(model, "enmtools.model")){
    model <- model$model
  }

  if(bg.source == "background"){
    allpoints <- rbind(presence, background)
    values <- extract(env, allpoints)
    maxes <- apply(values, 2, function(x) max(x, na.rm = TRUE))
    mins <- apply(values, 2, function(x) min(x, na.rm = TRUE))
  }

  if(bg.source == "env") {
    maxes <- maxValue(env)
    mins <- minValue(env)
  }



  this.lhs <- randomLHS(n.background, length(names(env)))
  bg.table <- t(t(this.lhs) * (maxes  - mins) + mins)
  colnames(bg.table) <- names(env)

  p.table <- extract(env, presence)

  # Having to do this for now because the dismo models don't like "newdata"
  # Unfortunately I think we finally have to use an if statement because ranger predict is really different
  if(inherits(model, "ranger")) {
    pred.p <- as.numeric(predict(model, data = data.frame(p.table), type = "response")$predictions[ , 2, drop = TRUE])
    pred.bg <- as.numeric(predict(model, data = data.frame(bg.table), type = "response")$predictions[ , 2, drop = TRUE])
  } else {
    pred.p <- as.numeric(predict(model, newdata = data.frame(p.table), x = data.frame(p.table), type = "response"))
    pred.bg <- as.numeric(predict(model, newdata = data.frame(bg.table), x = data.frame(bg.table), type = "response"))
  }




  env.evaluation <-dismo::evaluate(pred.p, pred.bg)

  return(env.evaluation)
}
