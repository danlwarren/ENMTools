#' Takes an emtools.species object with presence and background points, and builds a maxent model
#'
#' @param formula Standard R formula
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param ... Arguments to be passed to maxent()
#'
#' @export enmtools.maxent
#' @export print.enmtools.maxent
#' @export summary.enmtools.maxent

enmtools.maxent <- function(species, env, ...){

  maxent.precheck(f, species, env)

  analysis.df <- rbind(species$presence.points, species$background.points)
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))

  this.mx <- maxent(env, p = analysis.df[analysis.df$presence == 1,1:2], a = analysis.df[analysis.df$presence == 0,1:2], ...)

  suitability <- predict(env, this.mx, type = "response")


  output <- list(analysis.df = analysis.df,
                 model = this.mx,
                 suitability = suitability)

  class(output) <- "enmtools.maxent"

  return(output)

}

# Summary for objects of class enmtools.maxent
summary.enmtools.maxent <- function(this.maxent){

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.maxent$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(summary(this.maxent$model))

  cat("\n\nSuitability:  \n")
  print(this.maxent$suitability)
  plot(this.maxent$suitability)

}

# Print method for objects of class enmtools.maxent
print.enmtools.maxent <- function(this.maxent){

  summary(this.maxent)

}

# Function for checking data prior to running enmtools.maxent
maxent.precheck <- function(f, species, env){

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

