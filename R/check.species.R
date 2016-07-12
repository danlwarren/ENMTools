#' Checking compliance for an object of class enmtools.species.
#'
#' Checks for existence and proper class of:
#' @param range A raster or SpatialPolygon with the actual range they occur in
#' @param presence.points A data frame with sampled localities
#' @param background.points A data frame with absence/pseudoabsence/background localities
#' @param species.name A character vector with the species name
#' @param models A list of models that are made for the species, which will be stuffed in there as we go along
#' to pass the check.  This is used by internal enmtools functions to make sure the necessary data is present
#' before processing anything.
#'
#' @export check.species


check.species <- function(this.species){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  if(!isTRUE(is.na(this.species$range))){
    if(!any(c("raster", "RasterLayer", "SpatialPolygons") %in% class(this.species$range))){
      stop("Argument range requires an object of class raster, RasterLayer, or SpatialPolygons")
    }
  }

  if(!isTRUE(is.na(this.species$presence.points))){
    if(!any(c("data.frame") %in% class(this.species$presence.points))){
      stop("Argument presence.points requires an object of class data.frame")
    }
  }

  if(!isTRUE(is.na(this.species$background.points))){
    if(!any("data.frame" %in% class(this.species$background.points))){
      stop("Argument background.points requires an object of class data.frame")
    }
  }

  if(!isTRUE(is.na(this.species$species.name))){
    if(!any("character" %in% class(this.species$species.name))){
      stop("Argument species.name requires an object of class character")
    }
  }

  # Might actually turn off returning the species eventually
  return(this.species)
}

