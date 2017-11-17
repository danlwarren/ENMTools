#' Takes an enmtools model object and an emtools.species object with presence and background points, formats it, and calls dismo evaluate function
#'
#' @param species An enmtools.species object
#' @param species An enmtools model object (enmtools.glm, enmtools.dm, etc.)
#' @param env A RasterStack object containing the environmental predictors
#' @param ... Arguments to be passed to evaluate()
#'
#' @export enmtools.maxent

enmtools.evaluate <- function(species, model, env, ...){

  enmtools.evaluate.precheck(species, model, env)

  return(evaluate(species$presence.points, species$background.points,
                  model$model, env))

}

enmtools.evaluate.precheck <- function(species, model, env){

  ### Check to make sure the data we need is there
  if(!"enmtools.species" %in% class(species)){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  # Check to make sure the model is really a model
  if(!any(c("enmtools.dm", "enmtools.mx", "enmtools.bc", "enmtools.glm") %in% class(model))){
    stop("Argument \'model\' must contain an enmtools model object!")
  }

  check.species(species)

  if(!any(c("data.frame") %in% class(species$presence.points))){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }

  if(!any(c("data.frame") %in% class(species$background.points))){
    stop("Species background.points do not appear to be an object of class data.frame")
  }

  if(!any(c("raster", "RasterLayer", "RasterStack") %in% class(env))){
    stop("No environmental rasters were supplied!")
  }

  if(ncol(species$presence.points) != 2){
    stop("Species presence points do not contain longitude and latitude data!")
  }

  if(ncol(species$background.points) != 2){
    stop("Species background points do not contain longitude and latitude data!")
  }
}
