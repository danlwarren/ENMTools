#' Takes an emtools.species object with presence and background points, and builds a GLM
#'
#' @param formula Standard GLM formula
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param ... Arguments to be passed to glm()
#'
#' @export enmtools.glm
#' @export print.enmtools.glm
#' @export summary.enmtools.glm
#' @export plot.enmtools.glm


enmtools.glm <- function(f, species, env, test.prop = 0, ...){

  species <- check.bg(species, env, ...)

  glm.precheck(f, species, env)

  test.data <- NA
  test.evaluation <- NA

  if(test.prop > 0 & test.prop < 1){
    test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
    test.data <- species$presence.points[test.inds,]
    species$presence.points <- species$presence.points[-test.inds,]
  }

  ### Add env data
  species <- add.env(species, env)

  # Recast this formula so that the response variable is named "presence"
  # regardless of what was passed.
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"), response = "presence")

  analysis.df <- rbind(species$presence.points, species$background.points)
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))

  this.glm <- glm(f, analysis.df[,-c(1,2)], family="binomial", ...)

  suitability <- predict(env, this.glm, type = "response")

  model.evaluation <- evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                               this.glm, env)

  if(test.prop > 0 & test.prop < 1){
    test.evaluation <- evaluate(test.data, species$background.points[,1:2],
                                this.glm, env)
  }

  output <- list(formula = f,
                 analysis.df = analysis.df,
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.glm,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 suitability = suitability)

  class(output) <- c("enmtools.glm", "enmtools.model")

  return(output)

}

# Summary for objects of class enmtools.glm
summary.enmtools.glm <- function(this.glm){

  cat("\n\nFormula:  ")
  print(this.glm$formula)

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.glm$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(summary(this.glm$model))

  cat("\n\nModel fit (training data):  ")
  print(this.glm$training.evaluation)

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(this.glm$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(this.glm$test.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.glm$suitability)
  plot(this.glm)
}

# Print method for objects of class enmtools.glm
print.enmtools.glm <- function(this.glm){

  summary(this.glm)

}

# Plot method for objects of class enmtools.glm
plot.enmtools.glm <- function(this.glm){

  plot(this.glm$suitability, col = plasma(64))
  points(this.glm$analysis.df[this.glm$analysis.df$presence == 1,1:2], pch = 21, bg = "white")
  if(!is.na(this.glm$test.prop)){
    points(this.glm$test.data, pch = 21, bg = "green")
  }

}

# Function for checking data prior to running enmtools.glm
glm.precheck <- function(f, species, env){

  # Check to see if the function is the right class
  if(!inherits(f, "formula")){
    stop("Argument \'formula\' must contain an R formula object!")
  }

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

