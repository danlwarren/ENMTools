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
#' @method summary enmtools.species
#' @method print enmtools.species
#' @method plot enmtools.species


enmtools.species <- function(range = NA, presence.points = NA, background.points = NA,
                             species.name = NA, models=NA){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  if(!isTRUE(is.na(range))){
    if(!any(c("raster", "RasterLayer", "SpatialPolygons") %in% class(range))){
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


summary.enmtools.species <- function(this.species){
  stopifnot(inherits(this.species, "enmtools.species"))

  if(class(this.species$range) == "RasterLayer"){
    cat("\n\nRange raster: \n")
    print(this.species$range)
  } else {
    cat("\n\nRange raster not defined.")
  }

  if(class(this.species$presence.points) %in% c("data.frame", "matrix")){
    cat("\n\nPresence points (first ten only): ")
    print(kable(head(this.species$presence.points, 10)))
  } else{
    cat("\n\nPresence points not defined.")
  }

  if(class(this.species$background.points)  %in% c("data.frame", "matrix")){
    cat("\n\nBackground points (first ten only): ")
    print(kable(head(this.species$background.points, 10)))
  } else{
    cat("\n\nBackground points not defined.")
  }

  if(!is.na(this.species$models)){
    for(i in 1:length(this.species$models)){
      print(summary(this.species$models[[i]]))
    }
  }

  if(class(this.species$species.name) == "character"){
    cat(paste("\n\nSpecies name: ", this.species$species.name))
  } else {
    cat("\n\nSpecies name not defined.")
  }

  cat("\n\n")

}


plot.enmtools.species <- function(this.species){
  stopifnot(inherits(this.species, "enmtools.species"))

  if(class(this.species$range) == "RasterLayer"){
    plot(this.species$range)
  }

  if(class(this.species$background.points)  %in% c("data.frame", "matrix")){
    points(this.species$background.points[,1:2], pch = 4, col = "red")
  }

  if(class(this.species$presence.points) %in% c("data.frame", "matrix")){
    points(this.species$presence.points[,1:2], pch = 16, col = "black")
  }

  if(class(this.species$species.name) == "character"){
    title(this.species$species.name)
  }

}

print.enmtools.species <- function(this.species){

  summary(this.species)

}

