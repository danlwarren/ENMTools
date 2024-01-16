#' Takes a list of enmtools.species objects and combines them into a single enmtools.species object
#'
#' @param species.list List of enmtools.species objects that you want to combine together
#'
#' @return An enmtools.species object with the occurrence data, names, and range rasters for the species list combined into one.
#'
#' @examples
#' combine.species(iberolacerta.clade$species)


combine.species <- function(species.list){
  combined <- species.list[[1]]

  # Check to see that all species have SpatVector presence points, combine if so
  if(all(unlist(lapply(species.list, function(x) inherits(x$presence.points, "SpatVector"))))){
    combined$presence.points <- terra::vect(lapply(species.list, function(x) rbind(x$presence.points)))
  } else {
    combined$presence.points <- NA
    warning("Not all species presence points were of class SpatVector, setting presence points to NA")
  }

  if(all(unlist(lapply(species.list, function(x) inherits(x$background, "SpatVector"))))){
    combined$background.points = terra::vect(lapply(species.list, function(x) rbind(x$background.points)))
  } else {
    combined$background.points <- NA
    warning("Not all species background points were of class SpatVector, setting background points to NA")
  }

  if(all(unlist(lapply(species.list, function(x) inherits(x$range, "SpatRaster"))))){
    # Add data from other species - first species should already be in "combined"
    for(i in 2:length(species.list)){
      combined$range <-  terra::merge(combined$range, species.list[[i]][["range"]])
    }
  } else {
    combined$range <- NA
    warning("Not all species ranges were of class SpatRaster, setting range raster to NA")
  }

  combined$species.name <- paste(combined$species.name, species.list[[i]][["species.name"]], sep = " + ")

  return(combined)
}
