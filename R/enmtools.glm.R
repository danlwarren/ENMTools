#' Takes an emtools.species object with presence and background points, and builds a GLM
#'
#' @param formula Standard GLM formula
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.  Can be skipped if enmtools.species
#'            object already has environmental data associated with presence and background points.
#' @param ... Arguments to be passed to glm()
#'
#' @export enmtools.glm

enmtools.glm <- function(f, species, env = NA, ...){

  enm.precheck(species)

  ### Add env data if only lat/lon and a raster stack were supplied
  if(ncol(species$presence.points) == 2 | ncol(species$background.points) == 2){
    species <- add.env(species, env)
  }


  # Recast this formula so that the response variable is named "presence"
  # regardless of what was passed.
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"), response = "presence")

  analysis.df <- rbind(species$presence.points[,-c(1,2)], species$background.points[,-c(1,2)])
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))

  this.glm <- glm(f, analysis.df, family="binomial", ...)

  suitability <- "Suitability map not available, no environmental data supplied"

  if(any(c("raster", "RasterLayer", "RasterStack") %in% class(env))){
    suitability <- predict(env, this.glm)
  }

  output <- list(formula = f,
                 analysis.df = analysis.df,
                 model = this.glm,
                 suitability = suitability)

  class(output) <- "enmtools.glm"

  return(output)

}

summary.enmtools.glm <- function(this.glm){

  cat("\n\nFormula:  ")
  print(this.glm$formula)

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.glm$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(this.glm$model)

  cat("\n\nSuitability:  \n")
  print(this.glm$suitability)
  if("RasterLayer" %in% class(this.glm$suitability)){
    plot(this.glm$suitability)
  }

}


print.enmtools.glm <- function(this.glm){

  summary(this.glm)

}

