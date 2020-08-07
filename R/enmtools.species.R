#' Defining a class for enmtools.species.
#'
#' Each species gets:
#' @param range A raster or SpatialPolygon with the actual range they occur in
#' @param presence.points A data frame with sampled localities
#' @param background.points A data frame with absence/pseudoabsence/background localities
#' @param species.name A character vector with the species name
#' @param models A list of models that are made for the species, which will be stuffed in there as we go along
#' to pass the check.  This is used by internal enmtools functions to make sure the necessary data is present
#' before processing anything.
#'
#' @return Returns an enmtools.species object, either empty or populated with the parameter values that were passed into the function.


enmtools.species <- function(range = NA, presence.points = NA, background.points = NA,
                             species.name = NA, models=NA){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  if(!isTRUE(is.na(range))){
    if(!any(c("raster", "RasterLayer", "SpatialPolygons") %in% class(range))){
      stop("Argument range requires an object of class raster or SpatialPolygons")
    }
  }

  if(!isTRUE(is.na(presence.points))){
    if(!any(c("data.frame") %in% class(presence.points))){
      stop("Argument presence.points requires an object of class data.frame")
    }
  }

  if(!isTRUE(is.na(background.points))){
    if(!any("data.frame" %in% class(background.points))){
      stop("Argument background.points requires an object of class data.frame")
    }
  }

  if(!isTRUE(is.na(species.name))){
    if(!any("character" %in% class(species.name))){
      stop("Argument species.name requires an object of class character")
    }
  }

  output <- list(
    range = range,
    presence.points = presence.points,
    background.points = background.points,
    models = models,
    species.name = species.name)

  class(output) <- c("list", "enmtools.species")

  return(output)
}


summary.enmtools.species <- function(object, ...){
  stopifnot(inherits(object, "enmtools.species"))

  if(class(object$range) == "RasterLayer"){
    cat("\n\nRange raster: \n")
    print(object$range)
  } else {
    cat("\n\nRange raster not defined.")
  }

  if(class(object$presence.points) %in% c("data.frame", "matrix")){
    cat("\n\nPresence points (first ten only): ")
    print(knitr::kable(head(object$presence.points, 10)))
  } else{
    cat("\n\nPresence points not defined.")
  }

  if(class(object$background.points)  %in% c("data.frame", "matrix")){
    cat("\n\nBackground points (first ten only): ")
    print(knitr::kable(head(object$background.points, 10)))
  } else{
    cat("\n\nBackground points not defined.")
  }

  if(!is.na(object$models)){
    for(i in 1:length(object$models)){
      print(summary(object$models[[i]]))
    }
  }

  if(class(object$species.name) == "character"){
    cat(paste("\n\nSpecies name: ", object$species.name))
  } else {
    cat("\n\nSpecies name not defined.")
  }

  cat("\n\n")

}


plot.enmtools.species <- function(x, ...){
  stopifnot(inherits(x, "enmtools.species"))

  if(class(x$range) == "RasterLayer"){
    raster::plot(x$range)
  }

  if(class(x$background.points)  %in% c("data.frame", "matrix")){
    points(x$background.points[,1:2], pch = 4, col = "red")
  }

  if(class(x$presence.points) %in% c("data.frame", "matrix")){
    points(x$presence.points[,1:2], pch = 16, col = "black")
  }

  if(class(x$species.name) == "character"){
    title(x$species.name)
  }

}

print.enmtools.species <- function(x, ...){

  summary(x)

}

