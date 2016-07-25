#' Takes an emtools.species object with presence and background points, and builds a maxent model
#'
#' @param formula Standard R formula
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param ... Arguments to be passed to maxent()
#'
#' @export enmtools.maxent
#' @export print.enmtools.maxent
#' @export summary.enmtools.maxent
#' @export plot.enmtools.maxent

enmtools.maxent <- function(species, env, test.prop = 0, ...){

  species <- check.bg(species, env, ...)

  maxent.precheck(f, species, env)

  test.data <- NA
  test.evaluation <- NA

  if(test.prop > 0 & test.prop < 1){
    test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
    test.data <- species$presence.points[test.inds,]
    species$presence.points <- species$presence.points[-test.inds,]
  }

  analysis.df <- rbind(species$presence.points, species$background.points)
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))

  this.mx <- maxent(env, p = analysis.df[analysis.df$presence == 1,1:2], a = analysis.df[analysis.df$presence == 0,1:2], ...)

  model.evaluation <- evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                               this.mx, env)

  if(test.prop > 0 & test.prop < 1){
    test.evaluation <- evaluate(test.data, species$background.points[,1:2],
                                this.mx, env)
  }

  suitability <- predict(env, this.mx, type = "response")


  output <- list(analysis.df = analysis.df,
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.mx,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 suitability = suitability)

  class(output) <- c("enmtools.maxent", "enmtools.model")

  return(output)

}

# Summary for objects of class enmtools.maxent
summary.enmtools.maxent <- function(this.maxent){

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.maxent$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(summary(this.maxent$model))

  cat("\n\nModel fit (training data):  ")
  print(this.maxent$training.evaluation)

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(this.maxent$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(this.maxent$test.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.maxent$suitability)
  plot(this.maxent)

}

# Print method for objects of class enmtools.maxent
print.enmtools.maxent <- function(this.maxent){

  summary(this.maxent)

}

# Plot method for objects of class enmtools.maxent
plot.enmtools.maxent <- function(this.maxent){

  plot(this.maxent$suitability, col = plasma(64))
  points(this.maxent$analysis.df[this.maxent$analysis.df$presence == 1,1:2], pch = 21, bg = "white")
  if(!is.na(this.maxent$test.prop)){
    points(this.maxent$test.data, pch = 21, bg = "green")
  }

}

# Function for checking data prior to running enmtools.maxent
maxent.precheck <- function(f, species, env){

  ### Check to make sure the data we need is there
  if(!inherits(species, "enmtools.species")){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  check.species(species)

  if(!inherits(species$presence.points, "data.frame")){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }

  if(!inherits(species$background.points, "data.frame")){
    stop("Species background.points do not appear to be an object of class data.frame")
  }

  if(!inherits(env, c("raster", "RasterLayer", "RasterStack"))){
    stop("No environmental rasters were supplied!")
  }

  if(ncol(species$presence.points) != 2){
    stop("Species presence points do not contain longitude and latitude data!")
  }

  if(ncol(species$background.points) != 2){
    stop("Species background points do not contain longitude and latitude data!")
  }
}

