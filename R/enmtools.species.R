#' Defining a class for enmtools.species.
#'
#' Each species gets:
#' @param range A raster or SpatialPolygon with the actual range they occur in
#' @param presence.points A data frame with sampled localities
#' @param background.points A data frame with absence/pseudoabsence/background localities
#' @param species.name A character vector with the species name
#' @param models A list of models that are made for the species, which will
#'    be stuffed in there as we go along
#'
#' @export enmtools.species


enmtools.species <- function(range = NA, presence.points = NA, background.points = NA,
                             species.name = NA, models=NA){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  if(!isTRUE(is.na(range))){
    if(!any(c("raster", "SpatialPolygons") %in% class(range))){
      print("Argument range requires an object of class raster or SpatialPolygons")
    }
  }

  if(!isTRUE(is.na(presence.points))){
    if(!any(c("data.frame") %in% class(presence.points))){
      print("Argument presence.points requires an object of class data.frame")
    }
  }

  if(!isTRUE(is.na(background.points))){
    if(!any("data.frame" %in% class(background.points))){
      print("Argument background.points requires an object of class data.frame")
    }
  }

  if(!isTRUE(is.na(species.name))){
    if(!any("character" %in% class(species.name))){
      print("Argument species.name requires an object of class character")
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

