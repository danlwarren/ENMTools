#' Takes an emtools.species object with presence and background points, and builds a Bioclim model
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param ... Arguments to be passed to bioclim()
#'
#' @export enmtools.bc
#' @export print.enmtools.bc
#' @export summary.enmtools.bc

enmtools.bc <- function(species, env = NA, ...){

  bc.precheck(species, env)

  this.bc <- bioclim(env, species$presence.points[,1:2])

  suitability <- suitability <- predict(env, this.bc, type = "response")

  output <- list(analysis.df = species$presence.points[,1:2],
                 model = this.bc,
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

  cat("\n\nSuitability:  \n")
  print(this.bc$suitability)
  if("RasterLayer" %in% class(this.bc$suitability)){
    plot(this.bc$suitability)
  }

}


#Print method for objects of class enmtools.bc
print.enmtools.bc <- function(this.bc){

  summary(this.bc)

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


