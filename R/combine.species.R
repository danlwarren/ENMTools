#' Takes a list of enmtools.species objects and combines them into a single enmtools.species object
#'
#' @param species.list List of enmtools.species objects that you want to combine together
#'
#' @return An enmtools.species object with the occurrence data, names, and range rasters for the species list combined into one.
#'
#' @examples
#' data(iberolacerta.clade)
#' combine.species(iberolacerta.clade$species)


combine.species <- function(species.list){
  combined <- species.list[[1]]

  # Add data from other species
  for(i in 2:length(species.list)){

    if(is.data.frame(combined$presence.points) & is.data.frame(species.list[[i]]$presence.points)){
      combined$presence.points <- rbind(combined$presence.points, species.list[[i]][["presence.points"]])
    }

    if(is.data.frame(combined$background.points) & is.data.frame(species.list[[i]]$background.points)){
      combined$background.points <- rbind(combined$background.points, species.list[[i]][["background.points"]])
    }

    if(inherits(combined$range, "RasterLayer") & inherits(species.list[[i]]$range, "RasterLayer")){
      combined$range <-  raster::merge(combined$range, species.list[[i]][["range"]])
    } else if(is.data.frame(combined$range) & is.data.frame(species.list[[i]]$range)){
      combined$range <- species.list[[i]]$range
    } else {
      stop(paste("Inconsistent data types for species ranges:\n",
                 class(combined$range), "\n",
                 class(species.list[[i]]$range)))
    }
    combined$species.name <- paste(combined$species.name, species.list[[i]][["species.name"]], sep = " + ")
  }

  return(combined)
}
