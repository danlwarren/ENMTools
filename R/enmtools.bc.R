#' Takes an emtools.species object with presence and background points, and builds a Bioclim model
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param ... Arguments to be passed to bioclim()
#'
#' @export enmtools.bc
#' @export print.enmtools.bc
#' @export summary.enmtools.bc
#' @export plot.enmtools.bc

enmtools.bc <- function(species, env = NA, ...){

  species <- check.bg(species, env, ...)

  bc.precheck(species, env)

  this.bc <- bioclim(env, species$presence.points[,1:2])

  suitability <- suitability <- predict(env, this.bc, type = "response")

  model.evaluation <- evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                               this.bc, env)

  output <- list(analysis.df = species$presence.points[,1:2],
                 model = this.bc,
                 model.evaluation = model.evaluation,
                 suitability = suitability)

  class(output) <- "enmtools.bc"

  return(output)

}


# Summary for objects of class enmtools.bc
summary.enmtools.bc <- function(this.bc){

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.bc$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(this.bc$model)

  cat("\n\nModel fit:  ")
  print(this.bc$model.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.bc$suitability)
  plot(this.bc)
}

#Print method for objects of class enmtools.bc
print.enmtools.bc <- function(this.bc){

  summary(this.bc)

}

# Plot method for objects of class enmtools.bc
plot.enmtools.bc <- function(this.bc){

  plot(this.bc$suitability, col = plasma(64))
  points(this.bc$analysis.df, pch = 21, bg = "white")

}

# Checking data for analysis using enmtools.bc
bc.precheck <- function(species, env, f){

  ### Check to make sure the data we need is there
  if(!"enmtools.species" %in% class(species)){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  check.species(species)

  if(!any(c("data.frame") %in% class(species$presence.points))){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }

  if(ncol(species$presence.points) < 2){
    stop("Species presence points do not contain longitude and latitude data!")
  }

  if(ncol(species$background.points) < 2){
    stop("Species background points do not contain longitude and latitude data!")
  }

  if(!any(c("raster", "RasterLayer", "RasterStack") %in% class(env))){
    stop("Bioclim models require a RasterLayer or RasterStack object for argument env")
  }
}


