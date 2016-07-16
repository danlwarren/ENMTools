#' Takes an emtools.species object with presence and background points, and builds a Domain model
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param ... Arguments to be passed to bioclim()
#'
#' @export enmtools.dm
#' @export print.enmtools.dm
#' @export summary.enmtools.dm

enmtools.dm <- function(species, env = NA, ...){

  dm.precheck(species, env)

  this.dm <- bioclim(env, species$presence.points[,1:2])

  suitability <- suitability <- predict(env, this.dm, type = "response")

  output <- list(analysis.df = species$presence.points[,1:2],
                 model = this.dm,
                 suitability = suitability)

  class(output) <- "enmtools.dm"

  return(output)

}


# Summary for objects of class enmtools.dm
summary.enmtools.dm <- function(this.dm){

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.dm$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(this.dm$model)

  cat("\n\nSuitability:  \n")
  print(this.dm$suitability)
  if("RasterLayer" %in% class(this.dm$suitability)){
    plot(this.dm$suitability)
  }

}


#Print method for objects of class enmtools.dm
print.enmtools.dm <- function(this.dm){

  summary(this.dm)

}


# Checking data for analysis using enmtools.dm
dm.precheck <- function(species, env, f){

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


