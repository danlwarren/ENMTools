#' Takes an emtools.species object with presence and background points, and builds a GLM
#'
#' @param formula Standard GLM formula
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param ... Arguments to be passed to glm()
#'
#' @export enmtools.glm
#' @export print.enmtools.glm
#' @export summary.enmtools.glm


enmtools.glm <- function(f, species, env, ...){

  glm.precheck(f, species, env)

  ### Add env data
  species <- add.env(species, env)

  # Recast this formula so that the response variable is named "presence"
  # regardless of what was passed.
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"), response = "presence")

  analysis.df <- rbind(species$presence.points, species$background.points)
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))

  this.glm <- glm(f, analysis.df[,-c(1,2)], family="binomial", ...)

  suitability <- predict(env, this.glm, type = "response")


  output <- list(formula = f,
                 analysis.df = analysis.df,
                 model = this.glm,
                 suitability = suitability)

  class(output) <- "enmtools.glm"

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

  cat("\n\nSuitability:  \n")
  print(this.glm$suitability)
  plot(this.glm$suitability)
  points(this.glm$analysis.df[this.glm$analysis.df$presence == 1,1:2], pch = 16)

}

# Print method for objects of class enmtools.glm
print.enmtools.glm <- function(this.glm){

  summary(this.glm)

}

# Function for checking data prior to running enmtools.glm
glm.precheck <- function(f, species, env){

  # Check to see if the function is the right class
  if(!class(f) == "formula"){
    stop("Argument \'formula\' must contain an R formula object!")
  }

  ### Check to make sure the data we need is there
  if(!"enmtools.species" %in% class(species)){
    stop("Argument \'species\' must contain an enmtools.species object!")
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

